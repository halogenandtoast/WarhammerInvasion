<script setup lang="ts">
// A coloured chip showing a capital / race label. Used in the deck
// list, deck view, deck editor, and the import preview. Keeps the
// per-race background palette in one place.
//
// Pass `capital = null` to render the "no capital chosen" variant.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { slugOfCapital, type Capital } from '../lib/race'

const props = defineProps<{
  capital: Capital | null
  label?: string
}>()

const { t } = useI18n({ useScope: 'global' })

const classes = computed(() => ({
  'capital-chip': true,
  'capital-unset': props.capital === null,
  [`race-${props.capital ? slugOfCapital(props.capital) : 'unset'}`]: true,
}))

const text = computed(() => {
  if (props.label != null) return props.label
  if (props.capital === null) return t('decks.capital.unset')
  return t(`decks.capital.${props.capital}`)
})
</script>

<template>
  <span :class="classes">{{ text }}</span>
</template>

<style scoped>
.capital-chip {
  display: inline-block;
  font-size: 0.7rem;
  letter-spacing: 0.05em;
  padding: 0.18rem 0.65rem;
  border-radius: var(--radius-pill);
  text-transform: uppercase;
  color: var(--bg);
  border: 1px solid transparent;
  font-weight: 600;
}
.capital-chip.race-empire { background: var(--race-empire); }
.capital-chip.race-dwarf { background: var(--race-dwarf); }
.capital-chip.race-high-elf { background: var(--race-high-elf); }
.capital-chip.race-chaos { background: var(--race-chaos); color: var(--on-accent); }
.capital-chip.race-orc { background: var(--race-orc); }
.capital-chip.race-dark-elf { background: var(--race-dark-elf); color: var(--on-accent); }
.capital-chip.capital-unset {
  background: var(--bg);
  color: var(--fg-dim);
  border-color: var(--border);
  font-weight: 500;
}
</style>
