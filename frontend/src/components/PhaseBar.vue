<script setup lang="ts">
// Bottom strip during an active game. Shows:
//   - the turn counter (animated flip on change)
//   - whose turn it is ("Your turn" / "<name>'s turn")
//   - the 4-phase tracker
//   - when an action window is open: the trigger label inline on its
//     phase pip, with a Pass button for whoever holds priority
//   - a leave / "leave spectator seat" button on the right

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { EngineGame, Phase, PlayerKey } from '../api/protocol'
import { priorityHolder } from '../api/protocol'
import { PHASES, TRIGGER_TO_PHASE } from '../lib/phase'

const props = defineProps<{
  engine: EngineGame
  mySeat: PlayerKey | null
  // Display name of the active player (used in the "X's turn" copy
  // when the active player isn't you). Pulled from the seat list by
  // the parent.
  activePlayerName: string
  isSpectator: boolean
}>()

const emit = defineEmits<{ (e: 'pass'): void; (e: 'leave'): void }>()

const { t } = useI18n({ useScope: 'global' })

const aw = computed(() => props.engine.actionWindow)
const awPhase = computed<Phase | null>(() =>
  aw.value ? TRIGGER_TO_PHASE[aw.value.trigger] : null,
)
const priorityIsMe = computed(
  () =>
    aw.value != null &&
    props.mySeat != null &&
    priorityHolder(aw.value.awaiting) === props.mySeat,
)
const windowTriggerLabel = computed(() =>
  aw.value ? t(`game.play.window.trigger.${aw.value.trigger}`) : null,
)

const turnLabel = computed(() =>
  t('game.play.turn_label', { n: props.engine.turn }),
)

const activeLabel = computed(() =>
  props.mySeat === props.engine.currentPlayer
    ? t('game.play.your_turn')
    : t('game.play.opponent_turn', { name: props.activePlayerName }),
)

// Phase-pip label: when the open window belongs to this pip, swap the
// phase name for the more specific trigger label so combat sub-steps
// surface in-place rather than crowding the bar with extras.
function pipLabel(p: Phase): string {
  if (awPhase.value === p && windowTriggerLabel.value) return windowTriggerLabel.value
  return t(`game.play.phase.${p.replace('Phase', '')}`)
}

function pass() {
  if (priorityIsMe.value) emit('pass')
}
</script>

<template>
  <footer class="phase-bar">
    <div class="phase-bar-left">
      <span class="turn-label" :title="turnLabel">
        {{ t('game.play.turn_prefix') }}
        <span class="turn-number-slot">
          <Transition name="turn-flip">
            <span class="turn-number" :key="engine.turn">{{ engine.turn }}</span>
          </Transition>
        </span>
      </span>
      <span class="active-label">{{ activeLabel }}</span>
    </div>
    <ol class="phase-track">
      <li
        v-for="p in PHASES"
        :key="p"
        class="phase-pip"
        :class="{
          active: engine.phase === p,
          'window-open': awPhase === p,
          mine: awPhase === p && priorityIsMe,
        }"
      >
        <span class="pip-label">{{ pipLabel(p) }}</span>
        <button
          v-if="awPhase === p"
          class="pip-pass"
          type="button"
          :disabled="!priorityIsMe"
          :title="!priorityIsMe ? t('game.play.window.pass_disabled') : undefined"
          @click.stop="pass"
        >
          {{ t('game.play.window.pass') }}
        </button>
      </li>
    </ol>
    <button class="leave-btn" type="button" @click="emit('leave')">
      {{ isSpectator ? t('game.spectators.leave') : t('game.controls.leave') }}
    </button>
  </footer>
</template>

<style scoped>
.phase-bar {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  gap: 0.6rem;
  padding: 0.2rem 0.9rem;
  background: var(--bg-elev);
  border-top: 1px solid var(--border);
  min-height: 34px;
  flex-wrap: wrap;
}
.phase-bar-left {
  display: flex;
  align-items: baseline;
  gap: 0.55rem;
}
.turn-label {
  font-size: 0.68rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--fg-faint);
  display: inline-flex;
  align-items: baseline;
  gap: 0.35em;
}
.turn-number-slot {
  position: relative;
  display: inline-block;
  min-width: 1ch;
  text-align: left;
  line-height: 1;
}
.turn-number {
  display: inline-block;
  will-change: transform, opacity;
}
.turn-flip-enter-active {
  transition: transform 320ms cubic-bezier(0.4, 0, 0.2, 1), opacity 320ms ease;
}
.turn-flip-enter-from {
  opacity: 0;
  transform: translateY(60%);
}
.turn-flip-leave-active {
  position: absolute;
  top: 0;
  left: 0;
  transition: transform 420ms cubic-bezier(0.4, 0, 0.2, 1), opacity 420ms ease;
}
.turn-flip-leave-to {
  transform: translateY(-120%);
  opacity: 0;
}
@media (prefers-reduced-motion: reduce) {
  .turn-flip-enter-active,
  .turn-flip-leave-active { transition: opacity 120ms ease; }
  .turn-flip-enter-from { transform: none; }
  .turn-flip-leave-to { transform: none; }
}
.active-label {
  font-size: 0.86rem;
  font-weight: 600;
  color: var(--fg);
}
.phase-track {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  gap: 0.3rem;
  flex-wrap: wrap;
}
.phase-pip {
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.68rem;
  letter-spacing: 0.06em;
  padding: 0.18rem 0.6rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-faint);
  transition:
    padding 180ms ease,
    background 180ms ease,
    color 180ms ease,
    border-color 180ms ease,
    box-shadow 180ms ease;
}
.phase-pip.active {
  color: var(--on-accent);
  background: var(--accent);
  border-color: var(--accent);
}
.pip-label { white-space: nowrap; }

/* When an action window is open on this pip, it expands and recolors
   to read like the old standalone action-window pill. The "mine"
   variant pulses to signal that the player holds priority. */
.phase-pip.window-open {
  padding: 0.18rem 0.35rem 0.18rem 0.75rem;
  background: rgba(20, 14, 8, 0.92);
  border-color: rgba(255, 255, 255, 0.25);
  color: rgba(255, 255, 255, 0.92);
  text-transform: uppercase;
  font-size: 0.66rem;
  letter-spacing: 0.1em;
}
.phase-pip.window-open.mine {
  border-color: var(--accent);
  animation: pip-pulse 1.6s ease-in-out infinite;
}
@keyframes pip-pulse {
  0%, 100% { box-shadow: 0 0 0 2px rgba(196, 99, 74, 0.35); }
  50%      { box-shadow: 0 0 0 6px rgba(196, 99, 74, 0.10); }
}

.pip-pass {
  min-height: 22px;
  padding: 0 0.6rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.68rem;
  letter-spacing: 0.04em;
  cursor: pointer;
}
.pip-pass:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}
.pip-pass:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.leave-btn {
  margin-left: auto;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  padding: 0.18rem 0.6rem;
  border-radius: var(--radius-md);
  font-size: 0.74rem;
  cursor: pointer;
}
.leave-btn:hover { color: var(--accent-strong); border-color: var(--accent-strong); }

/* Narrow screens: idle phase pips collapse to dots so the bar stays a
   single row; only the pip with an open window keeps its label. The
   pass button grows to a full tap target. */
@media (max-width: 880px) {
  .phase-bar {
    gap: 0.4rem;
    padding: 0.25rem 0.6rem;
    flex-wrap: nowrap;
    overflow-x: auto;
  }
  .phase-pip:not(.window-open):not(.active) .pip-label {
    display: none;
  }
  .phase-pip:not(.window-open):not(.active) {
    width: 10px;
    height: 10px;
    padding: 0;
    border-radius: 50%;
  }
  .pip-pass {
    min-height: 34px;
    padding: 0 0.8rem;
  }
  .active-label {
    font-size: 0.78rem;
    white-space: nowrap;
  }
  .turn-label { white-space: nowrap; }
}
@media (pointer: coarse) {
  .pip-pass { min-height: 44px; }
  .leave-btn { min-height: 44px; }
}
</style>
