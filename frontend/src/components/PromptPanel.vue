<script setup lang="ts">
// Renders the engine's currently-pending prompt for the seated
// player. The engine pauses while a prompt is set; this panel is the
// only way to resume it. For the non-prompted seat the panel shows a
// "waiting for opponent" notice so spectators / the other player
// know what's happening.

import { computed } from 'vue'
import type {
  EngineCard,
  EngineGame,
  EnginePrompt,
  EngineUnit,
  PlayerKey,
  Race,
  TargetOption,
  ZoneKind,
} from '../api/protocol'
import { game } from '../stores/game'

const props = defineProps<{
  engine: EngineGame
  seat: PlayerKey | null
}>()

const prompt = computed<EnginePrompt | null>(() => props.engine.pendingPrompt)

const itsMine = computed(
  () => !!prompt.value && !!props.seat && prompt.value.player === props.seat,
)

// Selected unit keys for ChooseUnits-style prompts. We keep them
// local; the parent submits via `submitUnits` when the user confirms.
import { ref, watch } from 'vue'
const picks = ref<number[]>([])
watch(prompt, () => {
  picks.value = []
})

const me = computed(() => {
  if (!props.seat) return null
  return props.seat === 'Player1' ? props.engine.player1 : props.engine.player2
})

// Convert a PromptFilter into the list of selectable cards (hand /
// discard / in-play units). Each entry has its UnitKey + a short
// label for display.
interface PickOption {
  key: number
  label: string
}

const options = computed<PickOption[]>(() => {
  if (!prompt.value) return []
  if (prompt.value.kind.tag !== 'ChooseUnits') return []
  const filter = prompt.value.kind.filterSpec
  const m = me.value
  if (!m) return []

  const fromCardList = (cards: EngineCard[], race?: Race): PickOption[] =>
    cards
      .filter((c) => {
        if (c.kind !== 'Unit') return false
        if (race == null) return true
        return c.races.includes(race)
      })
      .map((c) => ({ key: c.key, label: c.title }))

  const fromUnits = (units: EngineUnit[]): PickOption[] =>
    units
      .filter((u) => u.controller === props.seat)
      .map((u) => ({ key: u.key, label: u.cardDef.title }))

  switch (filter.tag) {
    case 'AnyOwnUnit':
      return fromUnits(props.engine.units)
    case 'AnyUnitInPlay':
      return props.engine.units.map((u) => ({ key: u.key, label: u.cardDef.title }))
    case 'UnitsFromList': {
      const allowed = new Set(filter.contents)
      return props.engine.units
        .filter((u) => allowed.has(u.key))
        .map((u) => ({ key: u.key, label: u.cardDef.title }))
    }
    case 'OwnUnitsFromHandByRace':
      return fromCardList(m.hand, filter.contents)
    case 'OwnUnitsFromDiscardByRace':
      return fromCardList(m.discard, filter.contents)
    case 'OwnUnitsFromHandOrDiscardByRace':
      return [...fromCardList(m.hand, filter.contents), ...fromCardList(m.discard, filter.contents)]
  }
})

function togglePick(key: number) {
  const i = picks.value.indexOf(key)
  if (i >= 0) {
    picks.value.splice(i, 1)
  } else if (
    prompt.value?.kind.tag === 'ChooseUnits' &&
    picks.value.length >= prompt.value.kind.maxPick
  ) {
    // Already at max — silently ignore additional clicks rather than
    // booting an existing pick. Player can deselect first.
  } else {
    picks.value.push(key)
  }
}

function submitUnits() {
  game.resolvePromptUnits(picks.value)
}

function submitNone() {
  game.resolvePromptNone()
}

function submitYes() {
  game.resolvePromptBool(true)
}

function submitNo() {
  game.resolvePromptBool(false)
}

// Sacrifice prompt: list the player's own units in the named zone.
const sacrificeOptions = computed<PickOption[]>(() => {
  if (!prompt.value || prompt.value.kind.tag !== 'ChooseSacrifice') return []
  const zone = prompt.value.kind.zone
  return props.engine.units
    .filter((u) => u.controller === props.seat && u.zone === zone)
    .map((u) => ({ key: u.key, label: u.cardDef.title }))
})

const minOk = computed(() => {
  if (!prompt.value || prompt.value.kind.tag !== 'ChooseUnits') return true
  return picks.value.length >= prompt.value.kind.minPick
})

// ChooseAmount: integer slider/input bounded by [minAmount, maxAmount].
const amountPick = ref<number>(0)
watch(prompt, () => {
  if (prompt.value?.kind.tag === 'ChooseAmount') {
    amountPick.value = prompt.value.kind.minAmount
  }
})

function submitAmount() {
  game.resolvePromptAmount(amountPick.value)
}

// ChooseTargetOption: the engine has already pre-filtered the legal
// picks into `options`. Render one button per option, resolving unit
// keys to titles and zone tags to a "{player}'s {zone}" label.
const unitTitleByKey = computed<Map<number, string>>(() => {
  const m = new Map<number, string>()
  for (const u of props.engine.units) m.set(u.key, u.cardDef.title)
  return m
})

function zoneLabel(z: ZoneKind): string {
  switch (z) {
    case 'KingdomZone': return 'Kingdom'
    case 'QuestZone': return 'Quest zone'
    case 'BattlefieldZone': return 'Battlefield'
  }
}

function playerLabel(p: PlayerKey): string {
  if (props.seat && p === props.seat) return 'Your'
  return "Opponent's"
}

function targetOptionLabel(o: TargetOption): string {
  if (o.tag === 'TargetUnitOption') {
    return unitTitleByKey.value.get(o.contents) ?? `Unit #${o.contents}`
  }
  const [owner, zone] = o.contents
  return `${playerLabel(owner)} ${zoneLabel(zone).toLowerCase()}`
}

function targetOptionKey(o: TargetOption): string {
  if (o.tag === 'TargetUnitOption') return `u:${o.contents}`
  const [owner, zone] = o.contents
  return `z:${owner}:${zone}`
}

function submitTargetOption(o: TargetOption) {
  game.resolvePromptTargetOption(o)
}
</script>

<template>
  <aside v-if="prompt" class="prompt-panel" :class="{ 'is-mine': itsMine }">
    <header>
      <strong>{{ itsMine ? 'Your choice' : 'Waiting on opponent' }}</strong>
    </header>

    <p class="prompt-desc">{{ prompt.kind.description }}</p>

    <!-- ChooseYesNo -->
    <div v-if="prompt.kind.tag === 'ChooseYesNo'" class="actions">
      <template v-if="itsMine">
        <button type="button" @click="submitYes">Yes</button>
        <button type="button" @click="submitNo">No</button>
      </template>
      <template v-else>
        <em>Waiting…</em>
      </template>
    </div>

    <!-- ChooseUnits -->
    <div v-else-if="prompt.kind.tag === 'ChooseUnits'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in options"
            :key="o.key"
            type="button"
            class="pick"
            :class="{ selected: picks.includes(o.key) }"
            @click="togglePick(o.key)"
          >
            {{ o.label }}
          </button>
          <p v-if="options.length === 0" class="empty">No eligible cards.</p>
        </div>
        <div class="confirm">
          <button type="button" :disabled="!minOk" @click="submitUnits">
            Confirm ({{ picks.length }}/{{ prompt.kind.maxPick }})
          </button>
          <button v-if="prompt.kind.minPick === 0" type="button" @click="submitNone">Skip</button>
        </div>
      </template>
      <template v-else>
        <em>Waiting…</em>
      </template>
    </div>

    <!-- ChooseAmount -->
    <div v-else-if="prompt.kind.tag === 'ChooseAmount'" class="actions">
      <template v-if="itsMine">
        <div class="amount-row">
          <input
            type="number"
            :min="prompt.kind.minAmount"
            :max="prompt.kind.maxAmount"
            v-model.number="amountPick"
            class="amount-input"
          />
          <span class="amount-bounds">
            ({{ prompt.kind.minAmount }}–{{ prompt.kind.maxAmount }})
          </span>
        </div>
        <div class="confirm">
          <button
            type="button"
            :disabled="
              amountPick < prompt.kind.minAmount ||
              amountPick > prompt.kind.maxAmount
            "
            @click="submitAmount"
          >
            Confirm
          </button>
        </div>
      </template>
      <template v-else>
        <em>Waiting…</em>
      </template>
    </div>

    <!-- ChooseTargetOption -->
    <div v-else-if="prompt.kind.tag === 'ChooseTargetOption'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in prompt.kind.options"
            :key="targetOptionKey(o)"
            type="button"
            class="pick"
            @click="submitTargetOption(o)"
          >
            {{ targetOptionLabel(o) }}
          </button>
          <p v-if="prompt.kind.options.length === 0" class="empty">No valid targets.</p>
        </div>
      </template>
      <template v-else>
        <em>Waiting…</em>
      </template>
    </div>

    <!-- ChooseSacrifice -->
    <div v-else-if="prompt.kind.tag === 'ChooseSacrifice'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in sacrificeOptions"
            :key="o.key"
            type="button"
            class="pick"
            :class="{ selected: picks.includes(o.key) }"
            @click="picks = [o.key]"
          >
            {{ o.label }}
          </button>
          <p v-if="sacrificeOptions.length === 0" class="empty">No eligible units.</p>
        </div>
        <div class="confirm">
          <button
            type="button"
            :disabled="picks.length === 0 && !prompt.kind.optional && sacrificeOptions.length > 0"
            @click="picks.length > 0 ? submitUnits() : submitNone()"
          >
            {{ picks.length > 0 ? 'Sacrifice' : 'Skip' }}
          </button>
        </div>
      </template>
      <template v-else>
        <em>Waiting…</em>
      </template>
    </div>
  </aside>
</template>

<style scoped>
.prompt-panel {
  background: #2a2a32;
  border: 2px solid #555;
  border-radius: 6px;
  padding: 0.75rem 1rem;
  color: #f5f5f5;
}
.prompt-panel.is-mine {
  border-color: #f6b04c;
  box-shadow: 0 0 8px rgba(246, 176, 76, 0.45);
}
.prompt-desc {
  margin: 0.25rem 0 0.5rem;
}
.actions .picks {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  margin-bottom: 0.5rem;
}
.pick {
  background: #3a3a44;
  color: #f5f5f5;
  border: 1px solid #555;
  padding: 0.25rem 0.55rem;
  border-radius: 4px;
  cursor: pointer;
  min-height: 44px;
}
.pick.selected {
  background: #f6b04c;
  color: #1c1c20;
  border-color: #f6b04c;
}
.confirm {
  display: flex;
  gap: 0.5rem;
}
.amount-row {
  display: flex;
  gap: 0.5rem;
  align-items: center;
  margin-bottom: 0.5rem;
}
.amount-input {
  width: 5rem;
  min-height: 44px;
  background: #3a3a44;
  color: #f5f5f5;
  border: 1px solid #555;
  border-radius: 4px;
  padding: 0 0.6rem;
  font-size: 1rem;
}
.amount-bounds {
  color: #aaa;
  font-size: 0.85rem;
}
.confirm button {
  min-height: 44px;
  padding: 0 1rem;
  background: #f6b04c;
  color: #1c1c20;
  border: 0;
  border-radius: 4px;
  cursor: pointer;
}
.confirm button:disabled {
  background: #555;
  color: #999;
  cursor: not-allowed;
}
.empty {
  color: #888;
  font-style: italic;
  margin: 0;
}
</style>
