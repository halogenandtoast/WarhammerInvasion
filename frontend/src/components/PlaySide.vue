<script setup lang="ts">
// SVG-based player table. The static board (zones, capital image,
// tokens, piles' empty rects, labels, player chip, resources) is drawn
// inside one inline <svg> per player. Cards themselves are HTML
// elements in a sibling overlay, positioned in % of the container so
// they line up with the SVG-coordinate slots.
//
// Why the HTML overlay? `view-transition-name` is only reliably
// applied to elements with a CSS layout box. SVG <g> elements don't
// have one (they're laid out in SVG user space), so wrapping each card
// in a plain <div> is what makes the View Transitions API morph a card
// between hand, zones, and the discard pile.
//
// Each side is three rows stacked top → bottom:
//   self:     [battlefield] [capital row] [combined zone]
//   opponent: [combined zone] [capital row] [battlefield]
//
// The combined zone fills the full "below-capital" area and contains
// the player-info chip + resources, the hand, and the deck-on-discard
// pile (stacked vertically). The pile column lives on the outer side
// and the player chip + resources on the inner side; for the opponent
// these are mirrored.

import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type {
  EngineCard,
  EngineCardDef,
  EngineLegend,
  EnginePlayer,
  EngineQuest,
  EngineUnit,
  EngineZone,
  PlayerKey,
  ZoneKind,
} from '../api/protocol'
import { zoneBurning, zoneHitPoints } from '../api/protocol'
import { capitalImageFor, raceLabel } from '../lib/race'
import { CARD_SM } from '../lib/cardSize'
import CardArt from './CardArt.vue'

const props = defineProps<{
  player: EnginePlayer
  units: EngineUnit[]
  quests: EngineQuest[]
  legend: EngineLegend | null
  perspective: 'self' | 'opponent'
  seatName: string
  // Lookup of seat display names for both players. Used to label
  // controller-overlay badges on quests whose controller differs from
  // their zoneOwner (e.g. Dominion of Chaos played into the opponent's
  // play area).
  seatNames: Record<PlayerKey, string>
  isActive: boolean
  isFirstPlayer: boolean
  canPlayCard?: (card: EngineCardDef) => boolean
}>()

const emit = defineEmits<{
  (e: 'hand-card-click', payload: { card: EngineCard | null; rect: DOMRect }): void
}>()

const { t } = useI18n({ useScope: 'global' })

// ---- viewBox sizing ----
// Height is fixed (so card sizes etc. stay constant across resizes).
// Width is reactive: a ResizeObserver measures the container's actual
// aspect after mount and we set viewBox width to match so the SVG
// content fills horizontally without letterbox.
const VB_H = 570
const VB_W_FALLBACK = 1600

const PAD = 12
const ROW_GAP = 8
// Battlefield is wide and short, but tall enough to seat a portrait
// card. Battlefield cards must NOT be rotated unless the engine
// reports them rotated for a rules reason (exhaust / kneel) — the
// row's height is what gives the renderer room to keep them upright.
const BATTLE_H = 130
const CAP_H = 240
const COMBINED_H = 170

// Capital board: width fixed to preserve the printed ~0.72 portrait
// aspect at 240 tall.
const CAP_BOARD_W = 175

// Combined-row pile dimensions.
const PILE_W = CARD_SM.h // 100 — rotated card width
const PILE_H = CARD_SM.w // 72 — rotated card height
// Enough room between piles to seat the lower label without colliding
// with either card. Top label sits above the top pile; bottom label
// sits in this gap above the bottom pile (rather than below it, which
// would push past the SVG's viewBox on the self perspective).
const PILE_VERT_GAP = 18

// Width of the player-chip / resources column (the inner side of the
// combined zone for each player).
const PLAYER_COL_W = 130

// ---- reactive width ----
const containerEl = ref<HTMLDivElement | null>(null)
const vbW = ref(VB_W_FALLBACK)

let ro: ResizeObserver | null = null

onMounted(() => {
  const el = containerEl.value
  if (!el) return
  ro = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const cw = entry.contentRect.width
      const ch = entry.contentRect.height
      if (cw > 0 && ch > 0) {
        const w = Math.max(900, Math.round((cw / ch) * VB_H))
        if (w !== vbW.value) vbW.value = w
      }
    }
  })
  ro.observe(el)
})

onBeforeUnmount(() => {
  ro?.disconnect()
  ro = null
})

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

// ---- positions derived from vbW (perspective-aware) ----
//
// Combined-row x-layout:
//   self     (L→R): [ player col | hand | pile col ]
//   opponent (L→R, mirrored): [ pile col | hand | player col ]
//
// Pile column holds the deck stacked on the discard for self, and the
// opposite for the opponent.
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
// Centered vertically inside the combined row.
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
    // Self: deck on top, discard below. Opponent: inverted.
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

// ---- player chip + resources layout (inside the player column) ----
//
// The chip is a single horizontal pill on the top edge of the player
// column with the player's name, race, optional first-player tag, and
// optional active dot. The big resources counter sits below the chip.
// Both elements are anchored to the inner side of the column (start
// for self, end for opponent).
const chip = computed(() => {
  const isOpp = props.perspective === 'opponent'
  return {
    anchor: isOpp ? ('end' as const) : ('start' as const),
    // Local X origin inside the player-column group.
    nameX: isOpp ? PLAYER_COL_W : 0,
    raceX: isOpp ? PLAYER_COL_W : 0,
    activeCx: isOpp ? 8 : PLAYER_COL_W - 8,
    firstPipX: isOpp ? 0 : PLAYER_COL_W - 50,
  }
})
const chipLocalY = 12 // baseline of the seat-name text inside the column
const raceLocalY = chipLocalY + 16 // race tag baseline
const activeLocalY = chipLocalY - 4
const firstPipLocalY = chipLocalY + 22

const resourcesLocalY = chipLocalY + 50
const RESOURCE_ICON_W = 40

// ---- units in this zone ----
// Attachments cascade behind their host unit with a small per-attachment
// offset so each one shows a thin strip of card art beneath / beside the
// unit. The unit is drawn last so it stays fully visible on top.
const ATTACHMENT_OFFSET = 16
const zoneUnits = (z: EngineZone): EngineUnit[] =>
  props.units.filter((u) => u.zone === zoneToZoneKind(z))

function zoneToZoneKind(z: EngineZone): ZoneKind {
  return z.kind
}

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

// Centered Y for hand cards inside the combined row.
const handCardY = computed(
  () => combinedY.value + (COMBINED_H - CARD_SM.h) / 2,
)

const handCards = computed<Array<EngineCard | null>>(() => {
  if (props.perspective === 'opponent') {
    return new Array(props.player.hand.length).fill(null)
  }
  return props.player.hand
})

function isCardClickable(card: EngineCard | null): boolean {
  if (!card || props.perspective !== 'self') return false
  if (!props.canPlayCard) return false
  return props.canPlayCard(card)
}

const handXs = computed(() =>
  evenSpread(handCards.value.length, xs.value.handX, xs.value.handW, CARD_SM.w),
)

const capitalSrc = computed(() => capitalImageFor(props.player.race))
const raceText = computed(() => raceLabel(props.player.race))

// Legend slot: a portrait card-sized region centered on the capital
// board. Legend cards are printed portrait like every other unit card,
// so we use the standard CARD_SM dims (72 × 100) and don't rotate.
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
//
// Every visible card (hand, in-play unit, attached support, legend,
// deck top, discard top) becomes one entry here. Each slot has a
// position in SVG-viewBox units that we convert to a percentage of
// the container; the card-slot <div> uses that as `left`/`top`/etc.
// so it lines up exactly with where the SVG would have drawn the
// card. The `cardKey` field is what gets fed into
// `view-transition-name`.
interface CardSlot {
  key: string
  cardKey: number | null
  card: { code: string; title: string } | null
  faceDown: boolean
  rotated: boolean
  clickable: boolean
  // Position in SVG viewBox units.
  x: number
  y: number
  w: number
  h: number
  zIndex: number
  // Original EngineCard (only present for hand cards so the click
  // handler can pass it back up).
  handCard?: EngineCard | null
  // Quest-only: when this slot belongs to a quest whose controller
  // differs from the zoneOwner (i.e. Dominion of Chaos sitting in the
  // opponent's area), render an overlay tagging the real controller.
  controllerLabel?: string
  // Quest-only: damage / counter tokens currently sitting on the quest.
  tokens?: number
}

const cardSlots = computed<CardSlot[]>(() => {
  const slots: CardSlot[] = []

  // Hand
  const hxs = handXs.value
  const hy = handCardY.value
  handCards.value.forEach((c, i) => {
    slots.push({
      key: c ? `hand-${c.key}` : `hand-opp-${i}`,
      cardKey: c?.key ?? null,
      card: c ? { code: c.code, title: c.title } : null,
      faceDown: props.perspective === 'opponent',
      rotated: false,
      clickable: isCardClickable(c),
      x: hxs[i],
      y: hy,
      w: CARD_SM.w,
      h: CARD_SM.h,
      zIndex: 10,
      handCard: c,
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
    })
  }

  // Zone units + their attachments. Attachments first (so they render
  // BEHIND the unit they're attached to), unit on top.
  for (const zr of zones.value) {
    const units = zoneUnits(zr.z)
    if (zr.w > zr.h) {
      // Landscape zone — battlefield. Cards spread horizontally.
      const xsLane = evenSpread(units.length, zr.x + 12, zr.w - 24, CARD_SM.w)
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
        slots.push({
          key: `unit-${u.key}`,
          cardKey: u.key,
          card: { code: u.cardDef.code, title: u.cardDef.title },
          faceDown: false,
          rotated: false,
          clickable: false,
          x: xsLane[i],
          y: ySlot,
          w: CARD_SM.w,
          h: CARD_SM.h,
          zIndex: 30 + u.attachments.length + 1,
        })
      })
    } else {
      // Portrait zone — kingdom / quest. Cards stack vertically. The
      // quest zone also hosts any in-play quest cards whose zoneOwner
      // is this player; we stack the quests above the units.
      const xSlot = tallStackX(zr.x, zr.w)
      const zoneQuests =
        zr.z.kind === 'QuestZone'
          ? props.quests.filter((q) => q.zoneOwner === props.player.key)
          : []
      const ysLane = tallStackYs(
        zr.y + 26,
        zr.h - 26 - 30,
        zoneQuests.length + units.length,
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
        slots.push({
          key: `unit-${u.key}`,
          cardKey: u.key,
          card: { code: u.cardDef.code, title: u.cardDef.title },
          faceDown: false,
          rotated: false,
          clickable: false,
          x: xSlot,
          y: yBase,
          w: CARD_SM.w,
          h: CARD_SM.h,
          zIndex: 30 + u.attachments.length + 1,
        })
      })
    }
  }

  // Deck pile top: face-down card if the deck has anything in it.
  if (props.player.deck.length > 0) {
    slots.push({
      key: 'deck-top',
      // Face-down deck cards don't morph — picking one specific
      // back-image to morph to/from the next state would be wrong (any
      // card could come off the deck) and the visual is identical.
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
    // `view-transition-name` lives on the HTML slot wrapper (not the
    // inner SVG) so the browser captures a proper layout box.
    viewTransitionName:
      slot.cardKey != null ? `card-${slot.cardKey}` : undefined,
  }
}

function onSlotClick(slot: CardSlot, ev: MouseEvent) {
  if (!slot.clickable) return
  if (!slot.handCard) return
  ev.stopPropagation()
  const el = ev.currentTarget as HTMLElement
  emit('hand-card-click', {
    card: slot.handCard,
    rect: el.getBoundingClientRect(),
  })
}
</script>

<template>
  <div
    ref="containerEl"
    class="play-side"
    :class="[perspective, { active: isActive }]"
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

      <!-- Legend slot: label + empty-state outline. The legend CARD
           itself lives in the HTML overlay so it can carry a
           view-transition-name. -->
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
        <text
          v-if="legend && legend.damage > 0"
          :x="legendSlot.x + legendSlot.w - 6"
          :y="legendSlot.y + legendSlot.h - 6"
          text-anchor="end"
          class="legend-damage"
        >
          {{ legend.damage }}
        </text>
      </g>

      <!-- Zone slots (kingdom, quest, battlefield): backings, labels,
           HP, and tokens. Cards inside each zone live in the HTML
           overlay. -->
      <g
        v-for="zr in zones"
        :key="zr.label"
        class="zone-slot"
        :class="{ burning: zr.burning, burned: zr.burned }"
      >
        <rect :x="zr.x" :y="zr.y" :width="zr.w" :height="zr.h" rx="6" class="zone-bg" />
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

      <!-- Combined zone: player chip + resources + pile labels.
           Cards (hand, deck top, discard top) live in the overlay. -->
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

          <g :transform="`translate(${chip.anchor === 'start' ? 0 : PLAYER_COL_W - RESOURCE_ICON_W - 50}, ${resourcesLocalY})`" class="resources">
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

        <!-- Empty-pile rects sit underneath the overlay cards so the
             dashed outline shows through when a pile is exhausted. -->
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

        <!-- "Hand: 0" placeholder when the player's hand is empty.
             Skipping the overlay for this case keeps the no-cards
             state inside the SVG so it's centred predictably. -->
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

    <!-- HTML card-slot overlay. Lives over the SVG so each card has a
         real CSS layout box for `view-transition-name` to anchor to.
         Cards are positioned in % relative to the SVG viewBox, so they
         line up regardless of the actual rendered size. -->
    <div class="cards-overlay" aria-hidden="false">
      <div
        v-for="slot in cardSlots"
        :key="slot.key"
        class="card-slot"
        :class="{ clickable: slot.clickable }"
        :style="slotStyle(slot)"
        @click="onSlotClick(slot, $event)"
      >
        <CardArt
          :card="slot.card"
          :face-down="slot.faceDown"
          :rotated="slot.rotated"
        />
        <!-- Controller-overlay banner: shown on quests whose controller
             differs from the player whose zone visually houses them. -->
        <div
          v-if="slot.controllerLabel"
          class="card-controller-banner"
          :title="t('game.play.quest.controlled_by', { name: slot.controllerLabel })"
        >
          {{ t('game.play.quest.controlled_by', { name: slot.controllerLabel }) }}
        </div>
        <!-- Quest token counter (corruption / resource accumulator). -->
        <div v-if="slot.tokens" class="card-token-badge">
          {{ slot.tokens }}
        </div>
      </div>
    </div>
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
}

.card-slot.clickable {
  cursor: pointer;
}
.card-slot.clickable:hover {
  filter: drop-shadow(0 0 6px var(--accent, #c4634a));
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

.active-dot { fill: #5da46a; }

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
}
.zone-slot.burning .zone-bg {
  stroke: var(--accent-strong);
  stroke-width: 1.5;
}
.zone-slot.burned .zone-bg {
  fill: rgba(80, 30, 20, 0.55);
  stroke: var(--accent-strong);
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
.legend-damage {
  fill: var(--accent-strong);
  font-size: 11px;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  font-family: var(--font-sans);
}
</style>
