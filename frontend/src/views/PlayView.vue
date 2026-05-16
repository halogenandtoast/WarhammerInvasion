<script setup lang="ts">
// In-game table. Pure renderer of the engine snapshot held by the
// game store.
//
// Layout: the available vertical space is split exactly 50/50 between
// the opponent (top) and self (bottom). The action window pill lives
// in the page's bottom phase bar (see Game.vue), not inside this view.

import { computed, onBeforeUnmount, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { game } from '../stores/game'
import type {
  EngineCardDef,
  EngineGame,
  EngineLegend,
  EnginePlayer,
  EngineUnit,
  PlayerKey,
  SeatView,
  ZoneKind,
} from '../api/protocol'
import { priorityHolder } from '../api/protocol'
import PlaySide from '../components/PlaySide.vue'
import CardOverlay from '../components/CardOverlay.vue'
import { cardHover } from '../stores/cardHover'

onBeforeUnmount(() => cardHover.clear())

const props = defineProps<{
  engine: EngineGame
  seats: SeatView[]
}>()

const { t } = useI18n({ useScope: 'global' })

const mySeatKey = computed<PlayerKey | null>(() => {
  const me = game.you.value
  if (!me) return null
  const row = props.seats.find((s) => s.user.userId === me.userId)
  if (!row) return null
  return row.seat === 'Player1' ? 'Player1' : 'Player2'
})

const opponentSeatKey = computed<PlayerKey | null>(() =>
  mySeatKey.value === 'Player1' ? 'Player2' : mySeatKey.value === 'Player2' ? 'Player1' : null,
)

function playerFor(k: PlayerKey | null): EnginePlayer | null {
  if (!k) return null
  return k === 'Player1' ? props.engine.player1 : props.engine.player2
}

const me = computed(() => playerFor(mySeatKey.value))
const opponent = computed(() => playerFor(opponentSeatKey.value))

function seatName(k: PlayerKey): string {
  return props.seats.find((s) => s.seat === k)?.user.displayName ?? k
}

// Priority lookup is still needed for hand-card "playable" checks.
// The visible action-window pill itself now lives in the bottom phase
// bar (Game.vue), so this view doesn't render trigger/pass UI.
const priorityIsMe = computed(() => {
  const aw = props.engine.actionWindow
  return aw != null && mySeatKey.value != null && priorityHolder(aw.awaiting) === mySeatKey.value
})

const finished = computed(() => {
  const lc = props.engine.lifecycle
  if (lc.tag !== 'GameFinished') return null
  const youWon = mySeatKey.value === lc.contents.winner
  const reasonKey =
    lc.contents.reason === 'OpponentDeckedOut' ? 'reason_decked' : 'reason_burned'
  return {
    youWon,
    headline: youWon
      ? t('game.play.finished.you_win')
      : t('game.play.finished.you_lose', { name: seatName(lc.contents.winner) }),
    reason: t(`game.play.finished.${reasonKey}`),
  }
})

// ---- in-play units (split per player so each PlaySide gets only its own) ----

const myUnits = computed<EngineUnit[]>(() => {
  const k = mySeatKey.value
  if (!k) return []
  return props.engine.units.filter((u) => u.controller === k)
})

const opponentUnits = computed<EngineUnit[]>(() => {
  const k = opponentSeatKey.value
  if (!k) return []
  return props.engine.units.filter((u) => u.controller === k)
})

// At most one legend per player at a time (engine-enforced).
function legendFor(k: PlayerKey | null): EngineLegend | null {
  if (!k) return null
  return props.engine.legends.find((l) => l.controller === k) ?? null
}
const myLegend = computed(() => legendFor(mySeatKey.value))
const opponentLegend = computed(() => legendFor(opponentSeatKey.value))

// ---- hand-card play popover ----
//
// When the player clicks a card in their own hand, we open a small DOM
// popover anchored to the card's screen rect. The popover offers the
// legal "play" actions for that card; selecting one fires a
// 'GamePlayCard' frame and clears the open state. Server reply (the
// next 'GameUpdate') is what actually moves the card.

interface OpenPlay {
  card: EngineCardDef
  anchor: DOMRect
}
const openPlay = ref<OpenPlay | null>(null)

function isBattlefieldOnly(card: EngineCardDef): boolean {
  return card.keywords.some(
    (k) => k && typeof k === 'object' && (k as { tag?: string }).tag === 'BattlefieldOnly',
  )
}

function isAttachment(card: EngineCardDef): boolean {
  return card.traits.includes('Attachment')
}

// Is this card playable at all right now? Tactics any time we hold
// priority; everything else needs the active player to be us AND the
// open window to be the capital action window. Legends additionally
// require that the controller doesn't already have one in play
// (engine-enforced, but mirroring the rule client-side avoids opening
// a popover that won't do anything).
const canPlayCard = (card: EngineCardDef): boolean => {
  if (!priorityIsMe.value) return false
  if (card.kind === 'Tactic') return true
  if (props.engine.actionWindow?.trigger !== 'CapitalActionWindow') return false
  if (props.engine.currentPlayer !== mySeatKey.value) return false
  if (card.kind === 'Legend' && myLegend.value) return false
  return true
}

function onHandCardClick(payload: { card: EngineCardDef | null; rect: DOMRect }) {
  if (!payload.card) return
  if (!canPlayCard(payload.card)) return
  openPlay.value = { card: payload.card, anchor: payload.rect }
}

function closePopover() {
  openPlay.value = null
}

// Zones a unit/support card may legally enter. BattlefieldOnly cards
// can only go into the battlefield; everything else can enter any zone.
function legalZones(card: EngineCardDef): ZoneKind[] {
  if (isBattlefieldOnly(card)) return ['BattlefieldZone']
  return ['KingdomZone', 'QuestZone', 'BattlefieldZone']
}

function zoneLabel(z: ZoneKind): string {
  switch (z) {
    case 'KingdomZone': return t('game.play.action.kingdom')
    case 'QuestZone': return t('game.play.action.quest')
    case 'BattlefieldZone': return t('game.play.action.battlefield')
  }
}

function confirmLabel(kind: EngineCardDef['kind']): string {
  switch (kind) {
    case 'Tactic': return t('game.play.action.play_tactic')
    case 'Legend': return t('game.play.action.play_legend')
    case 'Quest': return t('game.play.action.play_quest')
    default: return t('game.play.action.play_quest')
  }
}

function playToZone(z: ZoneKind) {
  const card = openPlay.value?.card
  if (!card) return
  game.playCard(card.code, z, null)
  closePopover()
}

function playAsAttachment(targetKey: number) {
  const card = openPlay.value?.card
  if (!card) return
  game.playCard(card.code, null, targetKey)
  closePopover()
}

function playWithoutTarget() {
  const card = openPlay.value?.card
  if (!card) return
  game.playCard(card.code, null, null)
  closePopover()
}

// For the attachment picker: all in-play units in the game (Branded by
// Khorne can attach to the opponent), grouped by side.
const attachmentTargets = computed(() => {
  if (!openPlay.value || !isAttachment(openPlay.value.card)) return []
  return props.engine.units.map((u) => ({
    key: u.key,
    title: u.cardDef.title,
    code: u.cardDef.code,
    mine: u.controller === mySeatKey.value,
  }))
})

// Popover position: above the card if there's room, else below. We
// cap the popover width and re-center on the card's horizontal middle.
const popoverStyle = computed<Record<string, string>>(() => {
  const op = openPlay.value
  if (!op) return {}
  const W = 240
  const cx = op.anchor.left + op.anchor.width / 2
  const left = Math.max(8, Math.min(window.innerWidth - W - 8, cx - W / 2))
  const aboveTop = op.anchor.top - 8
  const belowTop = op.anchor.bottom + 8
  const useBelow = aboveTop < 80
  const style: Record<string, string> = {
    left: `${left}px`,
    top: useBelow ? `${belowTop}px` : `${aboveTop}px`,
    width: `${W}px`,
  }
  if (!useBelow) style.transform = 'translateY(-100%)'
  return style
})
</script>

<template>
  <div class="play-table">
    <div class="half top">
      <PlaySide
        v-if="opponent && opponentSeatKey"
        :player="opponent"
        :units="opponentUnits"
        :legend="opponentLegend"
        perspective="opponent"
        :seat-name="seatName(opponentSeatKey)"
        :is-active="engine.currentPlayer === opponentSeatKey"
        :is-first-player="engine.firstPlayer === opponentSeatKey"
      />
    </div>

    <div class="half bottom">
      <PlaySide
        v-if="me && mySeatKey"
        :player="me"
        :units="myUnits"
        :legend="myLegend"
        perspective="self"
        :seat-name="seatName(mySeatKey)"
        :is-active="engine.currentPlayer === mySeatKey"
        :is-first-player="engine.firstPlayer === mySeatKey"
        :can-play-card="canPlayCard"
        @hand-card-click="onHandCardClick"
      />
    </div>

    <!-- Hover-zoom preview for face-up cards. Teleports to <body> so
         the enlarged image isn't clipped by the play-table container. -->
    <CardOverlay />

    <!-- Game-over banner (overlays the table) -->
    <div v-if="finished" class="finished-overlay">
      <div class="finished-card" :class="{ win: finished.youWon }">
        <p class="finished-heading">{{ t('game.play.finished.heading') }}</p>
        <p class="finished-headline">{{ finished.headline }}</p>
        <p class="finished-reason">{{ finished.reason }}</p>
      </div>
    </div>

    <!-- Hand-card play popover. Teleported so it can render above the
         SVG board and the page chrome. -->
    <Teleport to="body">
      <div v-if="openPlay" class="play-popover-backdrop" @click="closePopover">
        <div
          class="play-popover"
          role="dialog"
          :aria-label="t('game.play.action.heading')"
          :style="popoverStyle"
          @click.stop
        >
          <header class="play-popover-head">
            <p class="play-popover-eyebrow">{{ t('game.play.action.heading') }}</p>
            <p class="play-popover-title">{{ openPlay.card.title }}</p>
          </header>

          <!-- Unit / non-attachment Support: pick a zone -->
          <template v-if="openPlay.card.kind === 'Unit' || (openPlay.card.kind === 'Support' && !isAttachment(openPlay.card))">
            <div class="play-popover-actions">
              <button
                v-for="z in legalZones(openPlay.card)"
                :key="z"
                class="play-popover-btn"
                type="button"
                @click="playToZone(z)"
              >
                {{ zoneLabel(z) }}
              </button>
            </div>
          </template>

          <!-- Attachment Support: pick a host unit -->
          <template v-else-if="openPlay.card.kind === 'Support' && isAttachment(openPlay.card)">
            <p class="play-popover-hint">{{ t('game.play.action.select_target') }}</p>
            <div v-if="attachmentTargets.length === 0" class="play-popover-empty">
              {{ t('game.play.action.no_targets') }}
            </div>
            <ul v-else class="play-popover-list">
              <li v-for="u in attachmentTargets" :key="u.key">
                <button
                  class="play-popover-row"
                  :class="{ mine: u.mine }"
                  type="button"
                  @click="playAsAttachment(u.key)"
                >
                  {{ u.title }}
                  <span v-if="u.mine" class="play-popover-row-tag">{{ t('game.seat.you_tag') }}</span>
                </button>
              </li>
            </ul>
          </template>

          <!-- Quest / Tactic / Legend: simple confirm -->
          <template v-else>
            <div class="play-popover-actions">
              <button class="play-popover-btn primary" type="button" @click="playWithoutTarget">
                {{ confirmLabel(openPlay.card.kind) }}
              </button>
            </div>
          </template>

          <button class="play-popover-cancel" type="button" @click="closePopover">
            {{ t('game.play.action.cancel') }}
          </button>
        </div>
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
.play-table {
  position: relative;
  flex: 1;
  min-height: 0;
  display: grid;
  /* Exact 50/50 split. */
  grid-template-rows: 1fr 1fr;
  gap: 0;
  padding: 0.4rem 0.5rem;
}

.half {
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 0;
  /* Allow the SVG to fill the cell vertically. */
  overflow: hidden;
}

.half.top { align-items: flex-start; padding-bottom: 0.3rem; }
.half.bottom { align-items: flex-end; padding-top: 0.3rem; }

/* ───────── game-over overlay ───────── */

.finished-overlay {
  position: absolute;
  inset: 0;
  background: rgba(0, 0, 0, 0.65);
  display: grid;
  place-items: center;
  z-index: 10;
}
.finished-card {
  padding: 1.3rem 1.6rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  border-left: 4px solid var(--accent-strong);
  text-align: center;
  min-width: min(360px, 80%);
}
.finished-card.win { border-left-color: #5da46a; }
.finished-heading { margin: 0 0 0.25rem; font-size: 0.72rem; letter-spacing: 0.18em; text-transform: uppercase; color: var(--fg-faint); }
.finished-headline { margin: 0 0 0.25rem; font-size: 1.4rem; font-weight: 600; }
.finished-reason { margin: 0; color: var(--fg-dim); font-size: 0.88rem; }
</style>

<style>
/* Unscoped — the popover is teleported to <body>, so scoped styles
   wouldn't apply. */
.play-popover-backdrop {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.25);
  z-index: 100;
}
.play-popover {
  position: fixed;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding: 0.7rem 0.8rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  box-shadow: 0 10px 28px rgba(0, 0, 0, 0.55);
  color: var(--fg);
}
.play-popover-head {
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
}
.play-popover-eyebrow {
  margin: 0;
  font-size: 0.66rem;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--fg-faint);
}
.play-popover-title {
  margin: 0;
  font-size: 0.95rem;
  font-weight: 600;
}
.play-popover-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}
.play-popover-btn {
  flex: 1 1 auto;
  min-height: var(--tap-target, 44px);
  padding: 0 0.7rem;
  background: var(--bg);
  border: 1px solid var(--border);
  color: var(--fg);
  border-radius: var(--radius-md);
  font-size: 0.85rem;
  cursor: pointer;
}
.play-popover-btn:hover {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
}
.play-popover-btn.primary {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
}
.play-popover-hint {
  margin: 0;
  font-size: 0.78rem;
  color: var(--fg-faint);
}
.play-popover-empty {
  padding: 0.5rem 0;
  color: var(--fg-faint);
  font-style: italic;
  font-size: 0.85rem;
}
.play-popover-list {
  margin: 0;
  padding: 0;
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  max-height: 260px;
  overflow-y: auto;
}
.play-popover-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
  min-height: 36px;
  padding: 0.4rem 0.6rem;
  background: var(--bg);
  border: 1px solid var(--border);
  color: var(--fg);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.85rem;
  text-align: left;
}
.play-popover-row:hover {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
}
.play-popover-row.mine {
  border-left: 3px solid var(--accent);
}
.play-popover-row-tag {
  font-size: 0.66rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--fg-faint);
}
.play-popover-row:hover .play-popover-row-tag {
  color: var(--on-accent);
}
.play-popover-cancel {
  margin-top: 0.1rem;
  align-self: flex-end;
  background: transparent;
  border: none;
  color: var(--fg-faint);
  font-size: 0.78rem;
  padding: 0.25rem 0.4rem;
  cursor: pointer;
}
.play-popover-cancel:hover {
  color: var(--fg);
}
</style>
