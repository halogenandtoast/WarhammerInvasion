<script setup lang="ts">
// SVG-based player table. The static board (zones, capital image,
// tokens, piles' empty rects, labels, player chip, resources) is drawn
// inside one inline <svg> per player. Cards themselves are HTML
// elements in a sibling overlay, positioned in % of the container so
// they line up with the SVG-coordinate slots.
//
// Card MOVEMENT is animated with a manual GSAP FLIP pass: before each
// re-render we snapshot every card's on-screen rect keyed by its
// stable engine key; after the patch, any card whose rect changed (or
// that re-appeared under a new element, e.g. hand → zone) animates
// from its old position. Transient effects (damage floats, destroy
// ghosts, burn flashes) ride the boardFx bus.
//
// Each side is three rows stacked top → bottom:
//   self:     [battlefield] [capital row] [combined zone]
//   opponent: [combined zone] [capital row] [battlefield]

import {
  computed,
  nextTick,
  onBeforeUnmount,
  onBeforeUpdate,
  onMounted,
  onUpdated,
  ref,
} from 'vue'
import { useI18n } from 'vue-i18n'
import gsap from 'gsap'
import type {
  EngineCard,
  EngineCardDef,
  EngineLegend,
  EnginePlayer,
  EngineQuest,
  EngineSupport,
  EngineUnit,
  EngineZone,
  HandCard,
  PlayerKey,
  ZoneKind,
} from '../api/protocol'
import { isVisibleCard, zoneBurning, zoneHitPoints } from '../api/protocol'
import { onBoardFx, type FxEvent } from '../stores/boardFx'
import { capitalImageFor, raceLabel } from '../lib/race'
import { CARD_SM } from '../lib/cardSize'
import { cardHover } from '../stores/cardHover'
import { getCachedSize } from '../lib/cardImage'
import CardArt from './CardArt.vue'

const props = defineProps<{
  player: EnginePlayer
  units: EngineUnit[]
  // Free-standing (non-attached) supports controlled by this player.
  supports: EngineSupport[]
  quests: EngineQuest[]
  legend: EngineLegend | null
  perspective: 'self' | 'opponent'
  seatName: string
  seatNames: Record<PlayerKey, string>
  isActive: boolean
  isFirstPlayer: boolean
  canPlayCard?: (card: EngineCard) => boolean
  cardIsUnplayable?: (card: EngineCard) => boolean
  // The zone of THIS side currently under attack (null outside
  // combat / when the other side is the target). Renders the
  // crossed-swords marker on the zone plate.
  combatTargetZone?: ZoneKind | null
}>()

const emit = defineEmits<{
  (e: 'hand-card-click', payload: { card: EngineCard | null; rect: DOMRect }): void
  (e: 'play-to-zone', payload: { card: EngineCard; zone: ZoneKind }): void
}>()

const { t } = useI18n({ useScope: 'global' })

const reducedMotion =
  typeof window !== 'undefined' &&
  window.matchMedia?.('(prefers-reduced-motion: reduce)').matches

// ---- viewBox sizing ----
const VB_H = 570
const VB_W_FALLBACK = 1600

const PAD = 12
const ROW_GAP = 8
const BATTLE_H = 130
const CAP_H = 240
const COMBINED_H = 170

const CAP_BOARD_W = 175

const PILE_W = CARD_SM.h // 100 — rotated card width
const PILE_H = CARD_SM.w // 72 — rotated card height
const PILE_VERT_GAP = 18

const PLAYER_COL_W = 130

// ---- reactive width ----
const containerEl = ref<HTMLDivElement | null>(null)
const vbW = ref(VB_W_FALLBACK)

let ro: ResizeObserver | null = null
let roFrame = 0

onMounted(() => {
  const el = containerEl.value
  if (!el) return
  // rAF-debounced: resize streams dozens of entries per second while
  // dragging a window edge; one layout pass per frame is plenty.
  ro = new ResizeObserver((entries) => {
    if (roFrame) return
    roFrame = requestAnimationFrame(() => {
      roFrame = 0
      for (const entry of entries) {
        const cw = entry.contentRect.width
        const ch = entry.contentRect.height
        if (cw > 0 && ch > 0) {
          const w = Math.max(minVbW(), Math.round((cw / ch) * VB_H))
          if (w !== vbW.value) vbW.value = w
        }
      }
    })
  })
  ro.observe(el)
})

onBeforeUnmount(() => {
  ro?.disconnect()
  ro = null
  if (roFrame) cancelAnimationFrame(roFrame)
  fxUnsub?.()
  clearLongPress()
})

// Coarse-pointer devices get a tighter minimum viewBox width so cards
// render physically larger (≈ 44px tap targets at 390px wide).
function minVbW(): number {
  const coarse =
    typeof window !== 'undefined' &&
    window.matchMedia?.('(pointer: coarse)').matches
  return coarse ? 700 : 900
}

// ---- per-perspective y offsets ----
const layout = computed(() => {
  if (props.perspective === 'self') {
    const battleY = PAD
    const capY = battleY + BATTLE_H + ROW_GAP
    const combinedY = capY + CAP_H + ROW_GAP
    return { battleY, capY, combinedY }
  }
  const combinedY = PAD
  const capY = combinedY + COMBINED_H + ROW_GAP
  const battleY = capY + CAP_H + ROW_GAP
  return { battleY, capY, combinedY }
})

const xs = computed(() => {
  const W = vbW.value
  const capBoardX = Math.round((W - CAP_BOARD_W) / 2)
  const sideZoneW = capBoardX - PAD - ROW_GAP
  const questX = capBoardX + CAP_BOARD_W + ROW_GAP
  const battleW = W - PAD * 2

  const isOpp = props.perspective === 'opponent'
  const handGap = 16

  let pilesX: number
  let playerColX: number
  let handX: number
  let handW: number

  if (isOpp) {
    pilesX = PAD
    playerColX = W - PAD - PLAYER_COL_W
    handX = pilesX + PILE_W + handGap
    handW = playerColX - handGap - handX
  } else {
    playerColX = PAD
    pilesX = W - PAD - PILE_W
    handX = playerColX + PLAYER_COL_W + handGap
    handW = pilesX - handGap - handX
  }

  return {
    W,
    kingdomX: PAD,
    capBoardX,
    sideZoneW,
    questX,
    handX,
    handW,
    pilesX,
    playerColX,
    battleW,
    barX: PAD,
  }
})

// ---- piles (deck + discard stacked vertically) ----
const PILES_STACK_H = PILE_H * 2 + PILE_VERT_GAP
const pilesLayout = computed(() => {
  const cy = layout.value.combinedY
  const stackStart = cy + (COMBINED_H - PILES_STACK_H) / 2
  const topY = stackStart
  const bottomY = topY + PILE_H + PILE_VERT_GAP
  const isOpp = props.perspective === 'opponent'
  return {
    topY,
    bottomY,
    topKind: isOpp ? 'discard' : 'deck',
    bottomKind: isOpp ? 'deck' : 'discard',
  }
})

const deckY = computed(() =>
  pilesLayout.value.topKind === 'deck'
    ? pilesLayout.value.topY
    : pilesLayout.value.bottomY,
)
const discardY = computed(() =>
  pilesLayout.value.topKind === 'discard'
    ? pilesLayout.value.topY
    : pilesLayout.value.bottomY,
)

const topDiscard = computed<EngineCard | null>(() => {
  const d = props.player.discard
  return d.length ? d[d.length - 1] : null
})

// ---- player chip + resources ----
const chip = computed(() => {
  const isOpp = props.perspective === 'opponent'
  return {
    anchor: isOpp ? ('end' as const) : ('start' as const),
    nameX: isOpp ? PLAYER_COL_W : 0,
    raceX: isOpp ? PLAYER_COL_W : 0,
    activeCx: isOpp ? 8 : PLAYER_COL_W - 8,
    firstPipX: isOpp ? 0 : PLAYER_COL_W - 50,
  }
})
const chipLocalY = 12
const raceLocalY = chipLocalY + 16
const activeLocalY = chipLocalY - 4
const firstPipLocalY = chipLocalY + 22

const resourcesLocalY = chipLocalY + 50
const RESOURCE_ICON_W = 40

// ---- units in this zone ----
const ATTACHMENT_OFFSET = 16
const zoneUnits = (z: EngineZone): EngineUnit[] =>
  props.units.filter((u) => u.zone === z.kind)

const zoneSupports = (z: EngineZone): EngineSupport[] =>
  props.supports.filter((s) => s.attachedTo === null && s.zone === z.kind)

function evenSpread(n: number, areaX: number, areaW: number, cardW: number): number[] {
  if (n === 0) return []
  if (n === 1) return [areaX + (areaW - cardW) / 2]
  const preferredGap = 12
  const preferredStep = cardW + preferredGap
  const fitStep = (areaW - cardW) / (n - 1)
  const step = Math.min(preferredStep, fitStep)
  const span = (n - 1) * step + cardW
  const startX = areaX + (areaW - span) / 2
  return Array.from({ length: n }, (_, i) => startX + i * step)
}

function tallStackX(slotX: number, slotW: number): number {
  return slotX + (slotW - CARD_SM.w) / 2
}
function tallStackYs(slotY: number, slotH: number, n: number): number[] {
  if (n === 0) return []
  const step = Math.min(CARD_SM.h * 0.35, (slotH - CARD_SM.h - 24) / Math.max(n - 1, 1))
  const used = (n - 1) * step + CARD_SM.h
  const startY = slotY + (slotH - used) / 2
  return Array.from({ length: n }, (_, i) => startY + i * step)
}

type TokenKind = 'development' | 'damage' | 'burn'
function zoneTokens(z: EngineZone): { kind: TokenKind; count?: number }[] {
  const out: { kind: TokenKind; count?: number }[] = []
  if (z.developments > 0) out.push({ kind: 'development', count: z.developments })
  if (z.damage > 0) out.push({ kind: 'damage', count: z.damage })
  if (z.burned) out.push({ kind: 'burn' })
  return out
}
const tokenSrc: Record<TokenKind, string> = {
  development: '/tokens/dominance.png',
  damage: '/tokens/damage.png',
  burn: '/tokens/burn.png',
}
function tokenStripXs(slotX: number, slotW: number, n: number): number[] {
  const tokenW = 22
  const gap = 6
  const totalW = n * tokenW + (n - 1) * gap
  const startX = slotX + (slotW - totalW) / 2
  return Array.from({ length: n }, (_, i) => startX + i * (tokenW + gap))
}

interface ZoneRender {
  z: EngineZone
  label: string
  x: number
  y: number
  w: number
  h: number
  hp: number
  burning: boolean
  burned: boolean
}
function zoneRender(
  z: EngineZone,
  label: string,
  x: number,
  y: number,
  w: number,
  h: number,
): ZoneRender {
  return { z, label, x, y, w, h, hp: zoneHitPoints(z), burning: zoneBurning(z), burned: z.burned }
}

const zones = computed<ZoneRender[]>(() => {
  const { capY, battleY } = layout.value
  const x = xs.value
  return [
    zoneRender(props.player.capital.kingdom, t('game.play.capital.kingdom'), x.kingdomX, capY, x.sideZoneW, CAP_H),
    zoneRender(props.player.capital.quest, t('game.play.capital.quest'), x.questX, capY, x.sideZoneW, CAP_H),
    zoneRender(props.player.capital.battlefield, t('game.play.capital.battlefield'), x.barX, battleY, x.battleW, BATTLE_H),
  ]
})

const capY = computed(() => layout.value.capY)
const combinedY = computed(() => layout.value.combinedY)

// ---- hand ----
const handCardY = computed(
  () => combinedY.value + (COMBINED_H - CARD_SM.h) / 2,
)

// Hand entries: the viewer's own hand is fully visible; the
// opponent's arrives as key-only stubs. Keeping the stub keys lets
// the FLIP pass animate an opponent's play from their hand position
// to the board (the reveal happens at the destination).
interface HandEntry {
  key: number
  card: EngineCard | null
}
const handCards = computed<HandEntry[]>(() =>
  props.player.hand.map((c: HandCard, i) => ({
    key: typeof c.key === 'number' ? c.key : -(i + 1),
    card:
      props.perspective === 'self' && isVisibleCard(c) ? c : null,
  })),
)

function isCardClickable(card: EngineCard | null): boolean {
  if (!card || props.perspective !== 'self') return false
  return true
}

function isCardUnplayable(card: EngineCard | null): boolean {
  if (!card || props.perspective !== 'self') return false
  if (!props.cardIsUnplayable) return false
  return props.cardIsUnplayable(card)
}

const handXs = computed(() =>
  evenSpread(handCards.value.length, xs.value.handX, xs.value.handW, CARD_SM.w),
)

// Fan parameters: cards rotate around their bottom edge and arc
// slightly, like a hand held at the table edge. The opponent's backs
// stay flat (they're compressed and unreadable anyway).
function fanFor(i: number, n: number): { rot: number; lift: number } {
  if (props.perspective !== 'self' || n <= 1) return { rot: 0, lift: 0 }
  const mid = (n - 1) / 2
  const off = (i - mid) / Math.max(mid, 1) // -1 .. 1
  const maxRot = Math.min(8, 2 + n * 0.8)
  return { rot: off * maxRot, lift: Math.abs(off) * 6 }
}

const capitalSrc = computed(() => capitalImageFor(props.player.race))
const raceText = computed(() => raceLabel(props.player.race))

const legendSlot = computed(() => {
  const w = CARD_SM.w
  const h = CARD_SM.h
  const x = xs.value.capBoardX + (CAP_BOARD_W - w) / 2
  const y = layout.value.capY + (CAP_H - h) / 2
  return {
    w,
    h,
    x,
    y,
    labelX: xs.value.capBoardX + CAP_BOARD_W / 2,
    labelY: y - 4,
  }
})

// ---- flat card-slot list for the HTML overlay ----
interface CardSlot {
  key: string
  cardKey: number | null
  card: { code: string; title: string } | null
  faceDown: boolean
  rotated: boolean
  clickable: boolean
  unplayable?: boolean
  x: number
  y: number
  w: number
  h: number
  zIndex: number
  handCard?: EngineCard | null
  controllerLabel?: string
  tokens?: number
  // Fan rotation (deg) — hand cards only.
  rot?: number
  // Unit state for on-card badges and styling.
  damage?: number
  power?: number
  corrupted?: boolean
  attacking?: boolean
  defending?: boolean
  // Self hand cards that may be drag-played.
  draggable?: boolean
}

const cardSlots = computed<CardSlot[]>(() => {
  const slots: CardSlot[] = []

  // Hand
  const hxs = handXs.value
  const hy = handCardY.value
  const n = handCards.value.length
  handCards.value.forEach((entry, i) => {
    const c = entry.card
    const fan = fanFor(i, n)
    slots.push({
      key: `hand-${entry.key}`,
      cardKey: entry.key,
      card: c ? { code: c.code, title: c.title } : null,
      // Redacted cards (opponent's hand; both hands for spectators)
      // render as backs.
      faceDown: props.perspective === 'opponent' || !c,
      rotated: false,
      clickable: isCardClickable(c),
      unplayable: isCardUnplayable(c),
      x: hxs[i],
      y: hy + fan.lift,
      w: CARD_SM.w,
      h: CARD_SM.h,
      zIndex: 10 + i,
      handCard: c,
      rot: fan.rot,
      draggable: !!c && props.perspective === 'self',
    })
  })

  // Legend
  if (props.legend) {
    const ls = legendSlot.value
    slots.push({
      key: `legend-${props.legend.key}`,
      cardKey: props.legend.key,
      card: { code: props.legend.cardDef.code, title: props.legend.cardDef.title },
      faceDown: false,
      rotated: false,
      clickable: false,
      x: ls.x,
      y: ls.y,
      w: ls.w,
      h: ls.h,
      zIndex: 20,
      damage: props.legend.damage > 0 ? props.legend.damage : undefined,
    })
  }

  const unitSlot = (u: EngineUnit, x: number, y: number, zIndex: number): CardSlot => ({
    key: `unit-${u.key}`,
    cardKey: u.key,
    card: { code: u.cardDef.code, title: u.cardDef.title },
    faceDown: false,
    rotated: false,
    clickable: false,
    x,
    y,
    w: CARD_SM.w,
    h: CARD_SM.h,
    zIndex,
    damage: u.damage > 0 ? u.damage : undefined,
    power: u.effectivePower,
    corrupted: u.corrupted,
    attacking: u.attacking,
    defending: u.defending,
  })

  for (const zr of zones.value) {
    const units = zoneUnits(zr.z)
    const supports = zoneSupports(zr.z)
    if (zr.w > zr.h) {
      // Landscape zone — battlefield.
      const xsLane = evenSpread(
        units.length + supports.length,
        zr.x + 12,
        zr.w - 24,
        CARD_SM.w,
      )
      const ySlot = zr.y + (zr.h - CARD_SM.h) / 2
      units.forEach((u, i) => {
        u.attachments.forEach((att, ai) => {
          slots.push({
            key: `att-${att.key}`,
            cardKey: att.key,
            card: { code: att.cardDef.code, title: att.cardDef.title },
            faceDown: false,
            rotated: false,
            clickable: false,
            x: xsLane[i] + (u.attachments.length - ai) * ATTACHMENT_OFFSET,
            y: ySlot + (u.attachments.length - ai) * (ATTACHMENT_OFFSET / 2),
            w: CARD_SM.w,
            h: CARD_SM.h,
            zIndex: 30 + ai,
          })
        })
        slots.push(unitSlot(u, xsLane[i], ySlot, 30 + u.attachments.length + 1))
      })
      supports.forEach((s, j) => {
        slots.push({
          key: `support-${s.key}`,
          cardKey: s.key,
          card: { code: s.cardDef.code, title: s.cardDef.title },
          faceDown: false,
          rotated: false,
          clickable: false,
          x: xsLane[units.length + j],
          y: ySlot,
          w: CARD_SM.w,
          h: CARD_SM.h,
          zIndex: 30,
          tokens: s.tokens > 0 ? s.tokens : undefined,
        })
      })
    } else {
      // Portrait zone — kingdom / quest.
      const xSlot = tallStackX(zr.x, zr.w)
      const zoneQuests =
        zr.z.kind === 'QuestZone'
          ? props.quests.filter((q) => q.zoneOwner === props.player.key)
          : []
      const ysLane = tallStackYs(
        zr.y + 26,
        zr.h - 26 - 30,
        zoneQuests.length + units.length + supports.length,
      )

      zoneQuests.forEach((q, i) => {
        slots.push({
          key: `quest-${q.key}`,
          cardKey: q.key,
          card: { code: q.cardDef.code, title: q.cardDef.title },
          faceDown: false,
          rotated: false,
          clickable: false,
          x: xSlot,
          y: ysLane[i],
          w: CARD_SM.w,
          h: CARD_SM.h,
          zIndex: 25 + i,
          controllerLabel:
            q.controller !== q.zoneOwner
              ? props.seatNames[q.controller]
              : undefined,
          tokens: q.tokens > 0 ? q.tokens : undefined,
        })
      })

      units.forEach((u, i) => {
        const yBase = ysLane[zoneQuests.length + i]
        u.attachments.forEach((att, ai) => {
          slots.push({
            key: `att-${att.key}`,
            cardKey: att.key,
            card: { code: att.cardDef.code, title: att.cardDef.title },
            faceDown: false,
            rotated: false,
            clickable: false,
            x: xSlot,
            y: yBase + (u.attachments.length - ai) * ATTACHMENT_OFFSET,
            w: CARD_SM.w,
            h: CARD_SM.h,
            zIndex: 30 + ai,
          })
        })
        slots.push(unitSlot(u, xSlot, yBase, 30 + u.attachments.length + 1))
      })

      supports.forEach((s, j) => {
        const yBase = ysLane[zoneQuests.length + units.length + j]
        slots.push({
          key: `support-${s.key}`,
          cardKey: s.key,
          card: { code: s.cardDef.code, title: s.cardDef.title },
          faceDown: false,
          rotated: false,
          clickable: false,
          x: xSlot,
          y: yBase,
          w: CARD_SM.w,
          h: CARD_SM.h,
          zIndex: 30,
          tokens: s.tokens > 0 ? s.tokens : undefined,
        })
      })
    }
  }

  // Deck pile top.
  if (props.player.deck.length > 0) {
    slots.push({
      key: 'deck-top',
      cardKey: null,
      card: null,
      faceDown: true,
      rotated: true,
      clickable: false,
      x: xs.value.pilesX,
      y: deckY.value,
      w: PILE_W,
      h: PILE_H,
      zIndex: 5,
    })
  }

  // Discard top card.
  if (topDiscard.value) {
    slots.push({
      key: `discard-top-${topDiscard.value.key}`,
      cardKey: topDiscard.value.key,
      card: { code: topDiscard.value.code, title: topDiscard.value.title },
      faceDown: false,
      rotated: true,
      clickable: false,
      x: xs.value.pilesX,
      y: discardY.value,
      w: PILE_W,
      h: PILE_H,
      zIndex: 5,
    })
  }

  return slots
})

function slotStyle(slot: CardSlot): Record<string, string | number | undefined> {
  const W = vbW.value
  const H = VB_H
  return {
    left: `${(slot.x / W) * 100}%`,
    top: `${(slot.y / H) * 100}%`,
    width: `${(slot.w / W) * 100}%`,
    height: `${(slot.h / H) * 100}%`,
    zIndex: slot.zIndex,
  }
}

// Inner-wrapper rotation: fan rotation for hand cards, 90° for
// corrupted units (the physical game turns the card sideways). Kept
// on a child element so GSAP's x/y transforms on the slot itself
// never fight it.
function rotStyle(slot: CardSlot): Record<string, string> | undefined {
  const parts: string[] = []
  if (slot.rot) parts.push(`rotate(${slot.rot}deg)`)
  if (slot.corrupted) parts.push('rotate(90deg) scale(0.82)')
  if (parts.length === 0) return undefined
  return { transform: parts.join(' '), transformOrigin: '50% 80%' }
}

// ---------------------------------------------------------------
// GSAP FLIP: animate card movement across re-renders.
// ---------------------------------------------------------------
const overlayEl = ref<HTMLDivElement | null>(null)
let prevRects = new Map<string, DOMRect>()

function captureRects(): Map<string, DOMRect> {
  const m = new Map<string, DOMRect>()
  const root = overlayEl.value
  if (!root) return m
  for (const el of root.querySelectorAll<HTMLElement>('[data-flip-id]')) {
    const id = el.dataset.flipId
    if (id) m.set(id, el.getBoundingClientRect())
  }
  return m
}

onBeforeUpdate(() => {
  prevRects = captureRects()
})

onUpdated(() => {
  if (reducedMotion) return
  const root = overlayEl.value
  if (!root) return
  const deckEl = root.querySelector<HTMLElement>('[data-pile="deck"]')
  const deckRect = deckEl?.getBoundingClientRect() ?? null
  for (const el of root.querySelectorAll<HTMLElement>('[data-flip-id]')) {
    const id = el.dataset.flipId
    if (!id) continue
    const now = el.getBoundingClientRect()
    const before = prevRects.get(id)
    if (before) {
      const dx = before.left - now.left
      const dy = before.top - now.top
      if (Math.abs(dx) > 1 || Math.abs(dy) > 1) {
        const scale = before.width > 0 && now.width > 0 ? before.width / now.width : 1
        gsap.set(el, { zIndex: 60 })
        gsap.fromTo(
          el,
          { x: dx, y: dy, scale },
          {
            x: 0,
            y: 0,
            scale: 1,
            duration: 0.45,
            ease: 'power3.out',
            clearProps: 'zIndex',
            overwrite: 'auto',
          },
        )
      }
    } else if (id.startsWith('hand-') && deckRect) {
      // Fresh hand card: deal it from the deck pile.
      const dx = deckRect.left - now.left
      const dy = deckRect.top - now.top
      gsap.fromTo(
        el,
        { x: dx, y: dy, rotation: -8, opacity: 0.4 },
        { x: 0, y: 0, rotation: 0, opacity: 1, duration: 0.45, ease: 'power3.out', overwrite: 'auto' },
      )
    } else {
      // Anything else new on the board: gentle pop-in.
      gsap.fromTo(
        el,
        { scale: 0.7, opacity: 0 },
        { scale: 1, opacity: 1, duration: 0.3, ease: 'back.out(1.6)', overwrite: 'auto' },
      )
    }
  }
})

// ---------------------------------------------------------------
// Board FX: damage floats, destroy ghosts, burn flashes.
// ---------------------------------------------------------------
interface FloatFx {
  id: number
  text: string
  cls: string
  left: number
  top: number
}
interface GhostFx {
  id: number
  code: string
  title: string
  left: number
  top: number
  width: number
  height: number
}
const floats = ref<FloatFx[]>([])
const ghosts = ref<GhostFx[]>([])
let fxSeq = 1

function containerPoint(rect: DOMRect): { left: number; top: number } | null {
  const host = containerEl.value
  if (!host) return null
  const hr = host.getBoundingClientRect()
  return { left: rect.left - hr.left + rect.width / 2, top: rect.top - hr.top }
}

function spawnFloat(rect: DOMRect, text: string, cls: string) {
  const pt = containerPoint(rect)
  if (!pt) return
  const fx: FloatFx = { id: fxSeq++, text, cls, left: pt.left, top: pt.top }
  floats.value = [...floats.value, fx]
  nextTick(() => {
    const el = containerEl.value?.querySelector<HTMLElement>(`[data-fx-float="${fx.id}"]`)
    if (!el) {
      floats.value = floats.value.filter((f) => f.id !== fx.id)
      return
    }
    gsap.fromTo(
      el,
      { y: 6, opacity: 0, scale: 0.8 },
      {
        y: -34,
        opacity: 1,
        scale: 1.05,
        duration: 0.55,
        ease: 'power2.out',
        onComplete: () => {
          gsap.to(el, {
            opacity: 0,
            y: -52,
            duration: 0.4,
            ease: 'power1.in',
            onComplete: () => {
              floats.value = floats.value.filter((f) => f.id !== fx.id)
            },
          })
        },
      },
    )
  })
}

function flashSlot(el: HTMLElement, color: string) {
  gsap.fromTo(
    el,
    { boxShadow: `0 0 0 3px ${color}`, filter: 'brightness(1.6)' },
    { boxShadow: '0 0 0 0px rgba(0,0,0,0)', filter: 'brightness(1)', duration: 0.55, ease: 'power2.out', clearProps: 'boxShadow,filter' },
  )
  gsap.fromTo(el, { x: -3 }, { x: 0, duration: 0.3, ease: 'elastic.out(1, 0.3)' })
}

function slotElFor(unitKey: number): HTMLElement | null {
  return (
    overlayEl.value?.querySelector<HTMLElement>(`[data-card-key="${unitKey}"]`) ?? null
  )
}

function handleFx(events: FxEvent[]) {
  if (reducedMotion) return
  const mine = new Set(props.units.map((u) => u.key))
  for (const ev of events) {
    switch (ev.kind) {
      case 'unit-damaged': {
        if (!mine.has(ev.unitKey)) break
        const el = slotElFor(ev.unitKey)
        if (!el) break
        spawnFloat(el.getBoundingClientRect(), `-${ev.amount}`, 'dmg')
        flashSlot(el, 'rgba(220, 70, 50, 0.85)')
        break
      }
      case 'unit-healed': {
        if (!mine.has(ev.unitKey)) break
        const el = slotElFor(ev.unitKey)
        if (!el) break
        spawnFloat(el.getBoundingClientRect(), `+${ev.amount}`, 'heal')
        break
      }
      case 'unit-corrupted': {
        if (!mine.has(ev.unitKey)) break
        const el = slotElFor(ev.unitKey)
        if (el) flashSlot(el, 'rgba(150, 80, 200, 0.85)')
        break
      }
      case 'unit-destroyed': {
        if (ev.controller !== props.player.key) break
        // The event fires BEFORE the snapshot swap, so the unit's
        // element is still on screen — capture its rect now and fly a
        // ghost to the discard pile after the DOM patches.
        const el = slotElFor(ev.unitKey)
        const host = containerEl.value
        if (!el || !host) break
        const rect = el.getBoundingClientRect()
        const hr = host.getBoundingClientRect()
        const ghost: GhostFx = {
          id: fxSeq++,
          code: ev.code,
          title: ev.title,
          left: rect.left - hr.left,
          top: rect.top - hr.top,
          width: rect.width,
          height: rect.height,
        }
        ghosts.value = [...ghosts.value, ghost]
        nextTick(() => {
          const gEl = host.querySelector<HTMLElement>(`[data-fx-ghost="${ghost.id}"]`)
          const pileEl = host.querySelector<HTMLElement>('[data-pile="discard"]')
          if (!gEl) {
            ghosts.value = ghosts.value.filter((g) => g.id !== ghost.id)
            return
          }
          const target = pileEl?.getBoundingClientRect()
          const dx = target ? target.left - rect.left : 0
          const dy = target ? target.top - rect.top : 40
          gsap
            .timeline({
              onComplete: () => {
                ghosts.value = ghosts.value.filter((g) => g.id !== ghost.id)
              },
            })
            .to(gEl, { rotation: 6, x: -4, duration: 0.1, ease: 'power1.in' })
            .to(gEl, { rotation: -6, x: 4, duration: 0.1 })
            .to(gEl, {
              x: dx,
              y: dy,
              rotation: 90,
              scale: target ? target.width / Math.max(rect.width, 1) : 0.6,
              opacity: 0.15,
              duration: 0.5,
              ease: 'power2.in',
            })
        })
        break
      }
      case 'zone-damaged': {
        if (ev.player !== props.player.key) break
        const plate = containerEl.value?.querySelector<SVGGraphicsElement>(
          `[data-zone-plate="${ev.zone}"]`,
        )
        if (!plate) break
        spawnFloat(plate.getBoundingClientRect(), `-${ev.amount}`, 'dmg zone')
        break
      }
      case 'zone-burned': {
        if (ev.player !== props.player.key) break
        const plate = containerEl.value?.querySelector<SVGGraphicsElement>(
          `[data-zone-plate="${ev.zone}"]`,
        )
        if (!plate) break
        spawnFloat(plate.getBoundingClientRect(), t('game.play.fx.burned'), 'burn')
        const host = containerEl.value
        if (host) {
          gsap.fromTo(host, { x: -5 }, { x: 0, duration: 0.5, ease: 'elastic.out(1, 0.25)' })
        }
        break
      }
      case 'legend-damaged': {
        if (!props.legend || props.legend.key !== ev.legendKey) break
        const el = slotElFor(ev.legendKey)
        if (!el) break
        spawnFloat(el.getBoundingClientRect(), `-${ev.amount}`, 'dmg')
        flashSlot(el, 'rgba(220, 70, 50, 0.85)')
        break
      }
      case 'resources-changed': {
        if (ev.player !== props.player.key || ev.delta === 0) break
        const el = containerEl.value?.querySelector<SVGGraphicsElement>('[data-resources]')
        if (!el) break
        spawnFloat(
          el.getBoundingClientRect(),
          ev.delta > 0 ? `+${ev.delta}` : `${ev.delta}`,
          ev.delta > 0 ? 'gain' : 'dmg',
        )
        break
      }
      default:
        break
    }
  }
}

const fxUnsub = onBoardFx(handleFx)

// ---------------------------------------------------------------
// Pointer interaction: tap, long-press inspect, drag-to-play.
// ---------------------------------------------------------------
interface DragState {
  card: EngineCard
  startX: number
  startY: number
  pointerId: number
  sourceEl: HTMLElement
  dragging: boolean
  ghostX: number
  ghostY: number
  legal: ZoneKind[]
  over: ZoneKind | null
  widthPx: number
  heightPx: number
}
const drag = ref<DragState | null>(null)

function keywordTags(card: EngineCardDef): string[] {
  return card.keywords
    .map((k) =>
      k && typeof k === 'object' ? ((k as { tag?: string }).tag ?? '') : String(k),
    )
    .filter(Boolean)
}

// Zones this card may be drag-played into. Empty array = not
// drag-playable (attachments, tactics, legends keep the tap flow).
function dragZonesFor(card: EngineCard): ZoneKind[] {
  if (props.cardIsUnplayable?.(card)) return []
  const tags = keywordTags(card)
  if (card.kind === 'Unit' || (card.kind === 'Support' && !card.traits.includes('Attachment'))) {
    if (tags.includes('BattlefieldOnly')) return ['BattlefieldZone']
    if (tags.includes('KingdomOnly')) return ['KingdomZone']
    if (tags.includes('QuestOnly')) return ['QuestZone']
    return ['KingdomZone', 'QuestZone', 'BattlefieldZone']
  }
  if (card.kind === 'Quest') return ['QuestZone']
  return []
}

const DRAG_THRESHOLD = 9

function onSlotPointerDown(slot: CardSlot, ev: PointerEvent) {
  if (!slot.clickable || !slot.handCard) return
  const el = ev.currentTarget as HTMLElement
  startLongPress(slot, ev)
  if (!slot.draggable) return
  const card = slot.handCard
  const rect = el.getBoundingClientRect()
  drag.value = {
    card,
    startX: ev.clientX,
    startY: ev.clientY,
    pointerId: ev.pointerId,
    sourceEl: el,
    dragging: false,
    ghostX: ev.clientX,
    ghostY: ev.clientY,
    legal: dragZonesFor(card),
    over: null,
    widthPx: rect.width,
    heightPx: rect.height,
  }
  el.setPointerCapture(ev.pointerId)
}

function onSlotPointerMove(ev: PointerEvent) {
  const d = drag.value
  if (!d || ev.pointerId !== d.pointerId) {
    if (longPress && distanceFrom(ev, longPress.x, longPress.y) > 10) clearLongPress()
    return
  }
  const dist = distanceFrom(ev, d.startX, d.startY)
  if (!d.dragging) {
    if (dist <= DRAG_THRESHOLD) return
    if (d.legal.length === 0) return
    d.dragging = true
    clearLongPress()
    cardHover.clear()
  }
  d.ghostX = ev.clientX
  d.ghostY = ev.clientY
  d.over = zoneAtPoint(ev.clientX, ev.clientY, d.legal)
  drag.value = { ...d }
}

function onSlotPointerUp(slot: CardSlot, ev: PointerEvent) {
  clearLongPress()
  const d = drag.value
  if (d && ev.pointerId === d.pointerId) {
    drag.value = null
    if (d.dragging) {
      if (d.over) emit('play-to-zone', { card: d.card, zone: d.over })
      return
    }
  }
  // Plain tap — open the action popover.
  if (!slot.clickable || !slot.handCard) return
  ev.stopPropagation()
  const el = ev.currentTarget as HTMLElement
  emit('hand-card-click', {
    card: slot.handCard,
    rect: el.getBoundingClientRect(),
  })
}

function onSlotPointerCancel() {
  clearLongPress()
  drag.value = null
}

function distanceFrom(ev: PointerEvent, x: number, y: number): number {
  return Math.hypot(ev.clientX - x, ev.clientY - y)
}

// Convert a viewport point to viewBox coords and hit-test the legal
// drop zones.
function zoneAtPoint(cx: number, cy: number, legal: ZoneKind[]): ZoneKind | null {
  const host = containerEl.value
  if (!host) return null
  const hr = host.getBoundingClientRect()
  if (cx < hr.left || cx > hr.right || cy < hr.top || cy > hr.bottom) return null
  const vx = ((cx - hr.left) / hr.width) * vbW.value
  const vy = ((cy - hr.top) / hr.height) * VB_H
  for (const zr of zones.value) {
    if (!legal.includes(zr.z.kind)) continue
    if (vx >= zr.x && vx <= zr.x + zr.w && vy >= zr.y && vy <= zr.y + zr.h) {
      return zr.z.kind
    }
  }
  return null
}

const dragGhostStyle = computed<Record<string, string>>(() => {
  const d = drag.value
  if (!d || !d.dragging) return { display: 'none' } as Record<string, string>
  return {
    left: `${d.ghostX - d.widthPx / 2}px`,
    top: `${d.ghostY - d.heightPx * 0.85}px`,
    width: `${d.widthPx}px`,
    height: `${d.heightPx}px`,
  } as Record<string, string>
})

// Long-press inspect (touch): hold a face-up card to open the zoom
// overlay without triggering the tap action.
let longPress: { timer: number; x: number; y: number } | null = null

function startLongPress(slot: CardSlot, ev: PointerEvent) {
  if (ev.pointerType === 'mouse') return
  if (!slot.card || slot.faceDown) return
  clearLongPress()
  const el = ev.currentTarget as HTMLElement
  const code = slot.card.code
  const title = slot.card.title
  longPress = {
    x: ev.clientX,
    y: ev.clientY,
    timer: window.setTimeout(() => {
      longPress = null
      const r = el.getBoundingClientRect()
      const src = `/cards/${code}.jpg`
      cardHover.show({
        src,
        alt: title,
        anchor: { x: r.left, y: r.top, width: r.width, height: r.height },
        natural: getCachedSize(src),
      })
    }, 420),
  }
}

function clearLongPress() {
  if (longPress) {
    clearTimeout(longPress.timer)
    longPress = null
  }
}
</script>

<template>
  <div
    ref="containerEl"
    class="play-side"
    :class="[perspective, { active: isActive }]"
    :data-side-player="player.key"
  >
    <svg
      class="play-side-svg"
      :viewBox="`0 0 ${vbW} ${VB_H}`"
      preserveAspectRatio="xMidYMid meet"
      role="img"
      :aria-label="`${seatName} play area`"
    >
      <!-- Capital board (center of capital row) -->
      <g class="capital-area">
        <rect
          :x="xs.capBoardX"
          :y="capY"
          :width="CAP_BOARD_W"
          :height="CAP_H"
          rx="8"
          class="capital-frame"
        />
        <image
          :x="xs.capBoardX"
          :y="capY"
          :width="CAP_BOARD_W"
          :height="CAP_H"
          :href="capitalSrc"
          preserveAspectRatio="xMidYMid slice"
          class="capital-img"
        />
      </g>

      <!-- Legend slot -->
      <g class="legend-area">
        <text
          :x="legendSlot.labelX"
          :y="legendSlot.labelY"
          text-anchor="middle"
          class="legend-label"
        >
          {{ t('game.play.capital.legend').toUpperCase() }}
        </text>
        <rect
          v-if="!legend"
          :x="legendSlot.x"
          :y="legendSlot.y"
          :width="legendSlot.w"
          :height="legendSlot.h"
          rx="6"
          class="legend-empty"
        />
      </g>

      <!-- Zone slots -->
      <g
        v-for="zr in zones"
        :key="zr.label"
        class="zone-slot"
        :class="{
          burning: zr.burning,
          burned: zr.burned,
          'drop-legal': drag?.dragging && drag.legal.includes(zr.z.kind),
          'drop-over': drag?.dragging && drag.over === zr.z.kind,
          'under-attack': combatTargetZone === zr.z.kind,
        }"
      >
        <rect
          :x="zr.x"
          :y="zr.y"
          :width="zr.w"
          :height="zr.h"
          rx="6"
          class="zone-bg"
          :data-zone-plate="zr.z.kind"
        />
        <text :x="zr.x + 10" :y="zr.y + 16" class="zone-label">{{ zr.label.toUpperCase() }}</text>
        <text
          :x="zr.x + zr.w - 10"
          :y="zr.y + 16"
          text-anchor="end"
          class="zone-hp"
          :class="{ critical: zr.burning }"
        >
          {{ zr.z.damage }}/{{ zr.hp }}
        </text>
        <!-- Damage meter: thin bar under the header line. -->
        <rect
          :x="zr.x + 10"
          :y="zr.y + 21"
          :width="zr.w - 20"
          height="3"
          rx="1.5"
          class="zone-meter-track"
        />
        <rect
          v-if="!zr.burned"
          :x="zr.x + 10"
          :y="zr.y + 21"
          :width="Math.max(0, (zr.w - 20) * Math.min(1, zr.z.damage / Math.max(zr.hp, 1)))"
          height="3"
          rx="1.5"
          class="zone-meter-fill"
        />
        <text
          v-if="combatTargetZone === zr.z.kind"
          :x="zr.x + zr.w / 2"
          :y="zr.y + 16"
          text-anchor="middle"
          class="zone-attack-marker"
        >
          ⚔ {{ t('game.play.under_attack').toUpperCase() }} ⚔
        </text>

        <g
          v-for="(tok, i) in zoneTokens(zr.z)"
          :key="`${zr.label}-tok-${i}`"
          :transform="`translate(${tokenStripXs(zr.x, zr.w, zoneTokens(zr.z).length)[i]}, ${zr.y + zr.h - 26})`"
        >
          <image :href="tokenSrc[tok.kind]" x="0" y="0" width="22" height="22" />
          <text v-if="typeof tok.count === 'number'" x="26" y="16" class="token-count">
            {{ tok.count }}
          </text>
        </g>
      </g>

      <!-- Combined zone: player chip + resources + pile labels. -->
      <g class="combined-row">
        <g :transform="`translate(${xs.playerColX}, ${combinedY})`" class="player-col">
          <text
            :x="chip.nameX"
            :y="chipLocalY"
            :text-anchor="chip.anchor"
            class="seat-name"
          >
            {{ seatName }}
          </text>
          <text
            :x="chip.raceX"
            :y="raceLocalY"
            :text-anchor="chip.anchor"
            class="race-tag"
          >
            {{ raceText }}
          </text>
          <circle
            v-if="isActive"
            :cx="chip.activeCx"
            :cy="activeLocalY"
            r="5"
            class="active-dot"
          />
          <g
            v-if="isFirstPlayer"
            :transform="`translate(${chip.firstPipX}, ${firstPipLocalY})`"
            class="first-pip"
          >
            <rect x="0" y="0" width="50" height="16" rx="8" />
            <text x="25" y="12" text-anchor="middle">
              {{ t('game.play.first_player_tag') }}
            </text>
          </g>

          <g
            :transform="`translate(${chip.anchor === 'start' ? 0 : PLAYER_COL_W - RESOURCE_ICON_W - 50}, ${resourcesLocalY})`"
            class="resources"
            data-resources
          >
            <image
              href="/tokens/resource.png"
              x="0"
              y="0"
              :width="RESOURCE_ICON_W"
              :height="RESOURCE_ICON_W"
            />
            <text :x="RESOURCE_ICON_W + 8" y="28" class="counter big">
              {{ player.resources }}
            </text>
          </g>
        </g>

        <rect
          v-if="player.deck.length === 0"
          :x="xs.pilesX"
          :y="deckY"
          :width="PILE_W"
          :height="PILE_H"
          rx="6"
          class="empty-pile"
        />
        <rect
          v-if="!topDiscard"
          :x="xs.pilesX"
          :y="discardY"
          :width="PILE_W"
          :height="PILE_H"
          rx="6"
          class="empty-pile"
        />
        <text
          :x="xs.pilesX + PILE_W / 2"
          :y="pilesLayout.topY - 4"
          text-anchor="middle"
          class="pile-label"
        >
          {{ pilesLayout.topKind === 'deck'
            ? `${t('game.play.seat.deck').toUpperCase()} · ${player.deck.length}`
            : `${t('game.play.seat.discard').toUpperCase()} · ${player.discard.length}` }}
        </text>
        <text
          :x="xs.pilesX + PILE_W / 2"
          :y="pilesLayout.bottomY - 4"
          text-anchor="middle"
          class="pile-label"
        >
          {{ pilesLayout.bottomKind === 'deck'
            ? `${t('game.play.seat.deck').toUpperCase()} · ${player.deck.length}`
            : `${t('game.play.seat.discard').toUpperCase()} · ${player.discard.length}` }}
        </text>

        <text
          v-if="handCards.length === 0"
          :x="xs.handX + xs.handW / 2"
          :y="handCardY + CARD_SM.h / 2 + 4"
          text-anchor="middle"
          class="empty"
        >
          {{ t('game.play.seat.hand') }}: 0
        </text>
      </g>
    </svg>

    <!-- HTML card-slot overlay. -->
    <div ref="overlayEl" class="cards-overlay" aria-hidden="false">
      <div
        v-for="slot in cardSlots"
        :key="slot.key"
        class="card-slot"
        :class="{
          clickable: slot.clickable,
          unplayable: slot.unplayable,
          attacking: slot.attacking,
          defending: slot.defending,
          corrupted: slot.corrupted,
          'drag-source': drag?.dragging && slot.handCard === drag.card,
          'hand-card': slot.key.startsWith('hand-') && perspective === 'self',
        }"
        :style="slotStyle(slot)"
        :data-flip-id="slot.cardKey != null ? `card-${slot.cardKey}` : slot.key"
        :data-card-key="slot.cardKey ?? undefined"
        :data-pile="slot.key === 'deck-top' ? 'deck' : slot.key.startsWith('discard-top') ? 'discard' : undefined"
        @pointerdown="onSlotPointerDown(slot, $event)"
        @pointermove="onSlotPointerMove($event)"
        @pointerup="onSlotPointerUp(slot, $event)"
        @pointercancel="onSlotPointerCancel()"
      >
        <div class="card-rot" :style="rotStyle(slot)">
          <CardArt
            :card="slot.card"
            :face-down="slot.faceDown"
            :rotated="slot.rotated"
          />
          <div
            v-if="slot.controllerLabel"
            class="card-controller-banner"
            :title="t('game.play.quest.controlled_by', { name: slot.controllerLabel })"
          >
            {{ t('game.play.quest.controlled_by', { name: slot.controllerLabel }) }}
          </div>
          <div v-if="slot.tokens" class="card-token-badge">
            {{ slot.tokens }}
          </div>
          <!-- Unit stat chips: power bottom-left, damage top-right. -->
          <div v-if="typeof slot.power === 'number'" class="card-stat power">
            {{ slot.power }}
          </div>
          <div v-if="slot.damage" class="card-stat damage">
            {{ slot.damage }}
          </div>
          <div v-if="slot.corrupted" class="card-corrupt-tag">
            {{ t('game.play.corrupted_tag') }}
          </div>
        </div>
      </div>

      <!-- Destroy ghosts: cards mid-flight to the discard pile. -->
      <div
        v-for="g in ghosts"
        :key="`ghost-${g.id}`"
        class="fx-ghost"
        :data-fx-ghost="g.id"
        :style="{
          left: `${g.left}px`,
          top: `${g.top}px`,
          width: `${g.width}px`,
          height: `${g.height}px`,
        }"
      >
        <CardArt :card="{ code: g.code, title: g.title }" :face-down="false" :rotated="false" />
      </div>

      <!-- Floating damage / heal / resource numbers. -->
      <div
        v-for="f in floats"
        :key="`float-${f.id}`"
        class="fx-float"
        :class="f.cls"
        :data-fx-float="f.id"
        :style="{ left: `${f.left}px`, top: `${f.top}px` }"
      >
        {{ f.text }}
      </div>
    </div>

    <!-- Drag ghost: follows the pointer while drag-playing. -->
    <Teleport to="body">
      <div v-if="drag?.dragging" class="drag-ghost" :style="dragGhostStyle">
        <CardArt
          :card="{ code: drag.card.code, title: drag.card.title }"
          :face-down="false"
          :rotated="false"
        />
        <p class="drag-hint" :class="{ ok: drag.over }">
          {{ drag.over
            ? t('game.play.drag.release_to_play')
            : t('game.play.drag.aim_at_zone') }}
        </p>
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
.play-side {
  position: relative;
  width: 100%;
  height: 100%;
  display: block;
}

.play-side-svg {
  width: 100%;
  height: 100%;
  display: block;
}

.cards-overlay {
  position: absolute;
  inset: 0;
  pointer-events: none;
}

.card-slot {
  position: absolute;
  pointer-events: auto;
  border-radius: var(--card-radius);
  /* GSAP animates transform on this element; keep transitions off so
     they never fight. */
  touch-action: none;
}

.card-rot {
  width: 100%;
  height: 100%;
  position: relative;
  transition: transform 0.18s ease;
}

.card-slot.clickable {
  cursor: grab;
}
.card-slot.clickable:active {
  cursor: grabbing;
}
.card-slot.hand-card.clickable:hover .card-rot {
  transform: translateY(-10px) scale(1.06) !important;
}
.card-slot.clickable:hover {
  filter: drop-shadow(0 0 6px var(--accent, #c4634a));
  z-index: 50 !important;
}
.card-slot.unplayable {
  opacity: 0.55;
  filter: grayscale(0.4);
}
.card-slot.unplayable.clickable:hover {
  filter: grayscale(0.4) drop-shadow(0 0 6px rgba(180, 60, 60, 0.55));
  opacity: 0.75;
}

.card-slot.drag-source {
  opacity: 0.25;
}

/* Combat roles: attackers glow red and lean toward the enemy,
   defenders glow blue. */
.card-slot.attacking {
  filter: drop-shadow(0 0 8px rgba(224, 90, 60, 0.9));
}
.card-slot.attacking .card-rot {
  outline: 2px solid rgba(224, 90, 60, 0.85);
  outline-offset: 1px;
  border-radius: var(--card-radius);
}
.card-slot.defending {
  filter: drop-shadow(0 0 8px rgba(95, 160, 214, 0.9));
}
.card-slot.defending .card-rot {
  outline: 2px solid rgba(95, 160, 214, 0.85);
  outline-offset: 1px;
  border-radius: var(--card-radius);
}

.card-corrupt-tag {
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%) rotate(-90deg);
  padding: 1px 6px;
  background: rgba(90, 40, 130, 0.85);
  color: #fff;
  font-size: 9px;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  border-radius: 3px;
  pointer-events: none;
}

.card-controller-banner {
  position: absolute;
  left: 0;
  right: 0;
  top: 0;
  padding: 2px 4px;
  background: var(--accent-strong, #c4634a);
  color: var(--on-accent, #fff);
  font-size: 9px;
  font-weight: 700;
  letter-spacing: 0.4px;
  text-transform: uppercase;
  text-align: center;
  border-top-left-radius: var(--card-radius);
  border-top-right-radius: var(--card-radius);
  pointer-events: none;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.card-token-badge {
  position: absolute;
  right: 4px;
  bottom: 4px;
  min-width: 18px;
  height: 18px;
  padding: 0 4px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  background: rgba(0, 0, 0, 0.75);
  color: var(--fg, #fff);
  font-size: 11px;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  border: 1px solid var(--accent-strong, #c4634a);
  border-radius: 9px;
  pointer-events: none;
}

/* Unit stat chips. */
.card-stat {
  position: absolute;
  min-width: 17px;
  height: 17px;
  padding: 0 3px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 11px;
  font-weight: 800;
  font-variant-numeric: tabular-nums;
  border-radius: 50%;
  pointer-events: none;
  color: #fff;
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.8);
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.6);
}
.card-stat.power {
  left: 3px;
  bottom: 3px;
  background: radial-gradient(circle at 30% 30%, #e9c963, #9a7a1f);
  border: 1px solid rgba(255, 255, 255, 0.35);
}
.card-stat.damage {
  right: 3px;
  top: 3px;
  background: radial-gradient(circle at 30% 30%, #e0604a, #8a2418);
  border: 1px solid rgba(255, 255, 255, 0.35);
}

/* ── transient FX ── */
.fx-float {
  position: absolute;
  transform: translateX(-50%);
  font-size: 18px;
  font-weight: 800;
  pointer-events: none;
  z-index: 80;
  text-shadow:
    0 1px 2px rgba(0, 0, 0, 0.9),
    0 0 8px rgba(0, 0, 0, 0.6);
  font-variant-numeric: tabular-nums;
}
.fx-float.dmg { color: #ff6a4d; }
.fx-float.heal { color: #6fd086; }
.fx-float.gain { color: #e9c963; }
.fx-float.burn {
  color: #ffae42;
  font-size: 15px;
  letter-spacing: 0.12em;
  text-transform: uppercase;
}
.fx-float.zone { font-size: 22px; }

.fx-ghost {
  position: absolute;
  pointer-events: none;
  z-index: 70;
}

/* ── SVG board styling ── */

.seat-name {
  fill: var(--fg);
  font-size: 14px;
  font-weight: 600;
  font-family: var(--font-sans);
}

.race-tag {
  fill: var(--fg-faint);
  font-size: 10px;
  letter-spacing: 1px;
  text-transform: uppercase;
  font-family: var(--font-sans);
}

.active-dot {
  fill: #5da46a;
  animation: active-pulse 2s ease-in-out infinite;
}
@keyframes active-pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.45; }
}
@media (prefers-reduced-motion: reduce) {
  .active-dot { animation: none; }
}

.first-pip rect {
  fill: rgba(212, 179, 87, 0.18);
  stroke: rgba(212, 179, 87, 0.4);
  stroke-width: 1;
}
.first-pip text {
  fill: var(--race-empire, #d4b357);
  font-size: 8px;
  letter-spacing: 1px;
  font-family: var(--font-sans);
  font-weight: 700;
}

.capital-frame {
  fill: rgba(0, 0, 0, 0.55);
  stroke: rgba(255, 255, 255, 0.08);
  stroke-width: 1;
}

.zone-bg {
  fill: rgba(0, 0, 0, 0.3);
  stroke: rgba(255, 255, 255, 0.1);
  stroke-width: 1;
  transition: fill 0.2s ease, stroke 0.2s ease;
}
.zone-slot.burning .zone-bg {
  stroke: var(--accent-strong);
  stroke-width: 1.5;
}
.zone-slot.burned .zone-bg {
  fill: rgba(80, 30, 20, 0.55);
  stroke: var(--accent-strong);
}

/* Drag-play affordances. */
.zone-slot.drop-legal .zone-bg {
  stroke: rgba(110, 190, 130, 0.85);
  stroke-width: 2;
  stroke-dasharray: 6 4;
  fill: rgba(60, 120, 75, 0.18);
}
.zone-slot.drop-over .zone-bg {
  stroke: #7fdc9a;
  stroke-width: 2.5;
  stroke-dasharray: none;
  fill: rgba(75, 160, 95, 0.3);
}

.zone-slot.under-attack .zone-bg {
  stroke: rgba(224, 90, 60, 0.9);
  stroke-width: 2;
  animation: under-attack-pulse 1.2s ease-in-out infinite;
}
@keyframes under-attack-pulse {
  0%, 100% { fill: rgba(0, 0, 0, 0.3); }
  50% { fill: rgba(120, 40, 25, 0.4); }
}
@media (prefers-reduced-motion: reduce) {
  .zone-slot.under-attack .zone-bg { animation: none; }
}
.zone-attack-marker {
  fill: #ff8a6a;
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 1.4px;
  font-family: var(--font-sans);
}

.zone-label {
  fill: var(--fg-faint);
  font-size: 10px;
  letter-spacing: 1.2px;
  font-family: var(--font-sans);
  font-weight: 600;
}
.zone-hp {
  fill: var(--fg);
  font-size: 12px;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  font-family: var(--font-sans);
}
.zone-hp.critical { fill: var(--accent-strong); }

.zone-meter-track {
  fill: rgba(255, 255, 255, 0.08);
}
.zone-meter-fill {
  fill: var(--accent-strong, #e0775e);
  transition: width 0.4s ease;
}

.token-count {
  fill: var(--fg);
  font-size: 13px;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  font-family: var(--font-sans);
}

.counter {
  fill: var(--fg);
  font-size: 14px;
  font-variant-numeric: tabular-nums;
  font-family: var(--font-sans);
  font-weight: 700;
}
.counter.big { font-size: 24px; }

.pile-label {
  fill: var(--fg-faint);
  font-size: 9px;
  letter-spacing: 1.2px;
  font-family: var(--font-sans);
  font-weight: 600;
}

.empty-pile {
  fill: rgba(255, 255, 255, 0.04);
  stroke: rgba(255, 255, 255, 0.18);
  stroke-width: 1;
  stroke-dasharray: 4 4;
}

.empty {
  fill: var(--fg-faint);
  font-size: 13px;
  font-family: var(--font-sans);
}

.legend-label {
  fill: var(--fg-faint);
  font-size: 9px;
  letter-spacing: 1.2px;
  font-family: var(--font-sans);
  font-weight: 600;
}
.legend-empty {
  fill: rgba(255, 255, 255, 0.04);
  stroke: rgba(212, 179, 87, 0.35);
  stroke-width: 1;
  stroke-dasharray: 4 4;
}
</style>

<style>
/* Unscoped — the drag ghost is teleported to <body>. */
.drag-ghost {
  position: fixed;
  z-index: 200;
  pointer-events: none;
  filter: drop-shadow(0 12px 22px rgba(0, 0, 0, 0.65));
  transform: rotate(3deg) scale(1.08);
}
.drag-ghost .drag-hint {
  position: absolute;
  left: 50%;
  bottom: -26px;
  transform: translateX(-50%);
  margin: 0;
  padding: 2px 8px;
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.8);
  color: var(--fg-dim, #ccc);
  font-size: 11px;
  white-space: nowrap;
}
.drag-ghost .drag-hint.ok {
  background: rgba(58, 122, 76, 0.95);
  color: #fff;
}
</style>
