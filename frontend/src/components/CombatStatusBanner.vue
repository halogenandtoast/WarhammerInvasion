<script setup lang="ts">
// Renders the in-flight combat state at the top of the side rail so
// the player can see what they're being prompted about (who's
// attacking what, how far through the 5-step ladder we are). Mounted
// whenever 'engine.combat' is set.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type {
  ActionWindowTrigger,
  EngineGame,
  PlayerKey,
  SeatView,
  ZoneKind,
} from '../api/protocol'

const props = defineProps<{
  engine: EngineGame
  seat: PlayerKey | null
  seats: SeatView[]
}>()

const { t } = useI18n({ useScope: 'global' })

const combat = computed(() => props.engine.combat)
const aw = computed(() => props.engine.actionWindow)

function seatName(k: PlayerKey): string {
  return props.seats.find((s) => s.seat === k)?.user.displayName ?? k
}

function sideLabel(k: PlayerKey): string {
  if (props.seat && k === props.seat) return t('game.seat.you_tag')
  return seatName(k)
}

function zoneLabel(z: ZoneKind): string {
  switch (z) {
    case 'KingdomZone':
      return t('game.play.capital.kingdom')
    case 'QuestZone':
      return t('game.play.capital.quest')
    case 'BattlefieldZone':
      return t('game.play.capital.battlefield')
  }
}

// Map an action-window trigger to one of the five combat sub-steps so
// the banner can render a "Step 3/5: Defenders" indicator. Returns
// null for non-combat windows (we shouldn't render the banner then
// anyway, but be defensive).
function stepFor(trigger: ActionWindowTrigger | undefined): { n: number; label: string } | null {
  switch (trigger) {
    case 'AfterDeclareCombatTarget':
      return { n: 1, label: t('game.play.combat.step.target') }
    case 'AfterDeclareAttackers':
      return { n: 2, label: t('game.play.combat.step.attackers') }
    case 'AfterDeclareDefenders':
      return { n: 3, label: t('game.play.combat.step.defenders') }
    case 'AfterAssignCombatDamage':
      return { n: 4, label: t('game.play.combat.step.assign') }
    case 'AfterApplyCombatDamage':
      return { n: 5, label: t('game.play.combat.step.apply') }
    default:
      return null
  }
}

const step = computed(() => stepFor(aw.value?.trigger))

const attackerUnits = computed(() => {
  if (!combat.value) return []
  const keys = new Set(combat.value.attackers)
  return props.engine.units.filter((u) => keys.has(u.key))
})

const defenderUnits = computed(() => {
  if (!combat.value) return []
  const keys = new Set(combat.value.defenders)
  return props.engine.units.filter((u) => keys.has(u.key))
})

const legendTarget = computed(() => {
  const c = combat.value
  if (!c || c.targetLegend == null) return null
  return props.engine.legends.find((l) => l.key === c.targetLegend) ?? null
})
</script>

<template>
  <aside v-if="combat" class="combat-status">
    <header>
      <strong>{{ t('game.play.combat.status_heading') }}</strong>
      <span v-if="step" class="step-tag">
        {{ t('game.play.combat.step_label', { n: step.n, total: 5, label: step.label }) }}
      </span>
    </header>

    <p class="line">
      <strong>{{ sideLabel(combat.attackingPlayer) }}</strong>
      <span class="arrow">→</span>
      <span v-if="legendTarget">
        {{ t('game.play.combat.attacks_legend', {
          defender: sideLabel(combat.defendingPlayer),
          legend: legendTarget.cardDef.title,
          zone: zoneLabel(combat.targetZone),
        }) }}
      </span>
      <span v-else>
        {{ t('game.play.combat.attacks_zone', {
          defender: sideLabel(combat.defendingPlayer),
          zone: zoneLabel(combat.targetZone),
        }) }}
      </span>
    </p>

    <div class="combatants">
      <div>
        <span class="label">{{ t('game.play.combat.attackers_label') }}</span>
        <ul v-if="attackerUnits.length">
          <li v-for="u in attackerUnits" :key="u.key">
            {{ u.cardDef.title }}
            <span v-if="u.damage > 0" class="dmg">({{ u.damage }})</span>
          </li>
        </ul>
        <p v-else class="empty">—</p>
      </div>
      <div>
        <span class="label">{{ t('game.play.combat.defenders_label') }}</span>
        <ul v-if="defenderUnits.length">
          <li v-for="u in defenderUnits" :key="u.key">
            {{ u.cardDef.title }}
            <span v-if="u.damage > 0" class="dmg">({{ u.damage }})</span>
          </li>
        </ul>
        <p v-else class="empty">{{ t('game.play.combat.undefended') }}</p>
      </div>
    </div>
  </aside>
</template>

<style scoped>
.combat-status {
  background: var(--bg-elev, #2a2a32);
  border: 1px solid var(--accent-strong, #c4634a);
  border-radius: var(--radius-md, 6px);
  padding: 0.55rem 0.75rem;
  color: var(--fg, #f5f5f5);
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
}
.combat-status header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 0.5rem;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint, #aaa);
}
.combat-status .step-tag {
  font-size: 0.66rem;
  letter-spacing: 0.08em;
  color: var(--accent-strong, #c4634a);
  font-weight: 700;
}
.combat-status .line {
  margin: 0;
  font-size: 0.85rem;
  line-height: 1.35;
}
.combat-status .arrow {
  color: var(--accent-strong, #c4634a);
  padding: 0 0.25rem;
}
.combatants {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.5rem;
  font-size: 0.78rem;
}
.combatants .label {
  display: block;
  font-size: 0.66rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint, #aaa);
  margin-bottom: 0.2rem;
}
.combatants ul {
  margin: 0;
  padding: 0;
  list-style: none;
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
}
.combatants .empty {
  margin: 0;
  color: var(--fg-faint, #888);
  font-style: italic;
}
.dmg {
  color: var(--accent-strong, #c4634a);
  margin-left: 0.25em;
  font-variant-numeric: tabular-nums;
}
</style>
