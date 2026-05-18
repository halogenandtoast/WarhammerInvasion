<script setup lang="ts">
// Full-card detail dialog. Opens when a user taps a tile in the cards
// browser. Renders the printed card layout: image on the left, stat
// badges + traits + rules text on the right.
//
// Game-text icons (`[Power]`, `[Hammer]`, etc.) get inlined as small
// rounded glyph spans — see `renderText` below.

import { onMounted, onUnmounted } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card } from '../types/card'
import { raceClass } from '../lib/race'
import { cardImageUrl } from '../lib/assets'

const props = defineProps<{ card: Card | null }>()
const emit = defineEmits<{ (e: 'close'): void }>()

const { t } = useI18n({ useScope: 'global' })

function onKey(e: KeyboardEvent) {
  if (e.key === 'Escape' && props.card) emit('close')
}

onMounted(() => window.addEventListener('keydown', onKey))
onUnmounted(() => {
  window.removeEventListener('keydown', onKey)
  document.body.style.overflow = ''
})

function onBackdropClick(e: MouseEvent) {
  if (e.target === e.currentTarget) emit('close')
}

// Inline `[Icon]` markers in rules text. We wrap each match in a small
// rounded glyph span; the CSS in this component paints them in the
// accent palette.
function renderText(text: string | null): string {
  if (!text) return ''
  return text.replace(/\[([^\]]+)\]/g, (_, sym: string) => {
    const cls = `icon icon-${sym.toLowerCase().replace(/\s+/g, '-')}`
    return `<span class="${cls}" aria-label="${sym}">${sym[0]}</span>`
  })
}
</script>

<template>
  <div
    v-if="card"
    class="modal-backdrop"
    role="dialog"
    aria-modal="true"
    :aria-label="card.name"
    @click="onBackdropClick"
  >
    <div class="modal" :class="raceClass(card.race)">
      <button
        class="close"
        type="button"
        :aria-label="t('cards.modal.aria_close')"
        @click="emit('close')"
      >×</button>
      <div class="modal-image">
        <img
          v-if="cardImageUrl(card)"
          :src="cardImageUrl(card)!"
          :alt="card.name"
        />
        <div v-else class="no-img big">{{ t('cards.modal.no_image') }}</div>
      </div>
      <div class="modal-detail">
        <header>
          <p class="modal-eyebrow">
            {{ card.set }}
            <span v-if="card.number" class="muted">
              · #{{ String(card.number).padStart(3, '0') }}
            </span>
          </p>
          <h2>{{ card.name }}</h2>
        </header>

        <div class="badges">
          <span v-if="card.race" class="badge">{{ card.race }}</span>
          <span v-if="card.type" class="badge">{{ card.type }}</span>
          <span v-if="card.cost !== null" class="badge cost">
            {{ card.cost }}{{ t('cards.modal.cost_suffix') }}
          </span>
          <span v-if="card.loyalty !== null" class="badge">
            {{ t('cards.modal.loyalty_prefix') }}{{ card.loyalty }}
          </span>
          <span v-if="card.power !== null" class="badge">
            {{ t('cards.modal.power_prefix') }}{{ card.power }}
          </span>
          <span v-if="card.health !== null" class="badge">
            {{ t('cards.modal.health_prefix') }}{{ card.health }}
          </span>
          <span v-if="card.quantity" class="badge">
            {{ t('cards.modal.quantity_prefix') }}{{ card.quantity }}
          </span>
        </div>

        <p v-if="card.traits" class="traits"><em>{{ card.traits }}</em></p>

        <p v-if="card.text" class="card-text" v-html="renderText(card.text)" />

        <p v-if="card.stub" class="stub-note">
          {{ t('cards.modal.stub_note') }}
        </p>

        <footer class="modal-footer">
          <span v-if="card.illustrator" class="muted">
            {{ t('cards.modal.illustrator', { name: card.illustrator }) }}
          </span>
          <span class="muted">{{ card.cycle }}{{ t('cards.modal.cycle_suffix') }}</span>
        </footer>
      </div>
    </div>
  </div>
</template>

<style scoped>
.modal-backdrop {
  position: fixed;
  inset: 0;
  background: var(--overlay-strong);
  display: grid;
  place-items: center;
  padding: 1rem;
  z-index: var(--z-modal);
  overflow-y: auto;
}

.modal {
  position: relative;
  width: 100%;
  max-width: 880px;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-xl);
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
  gap: 0;
  overflow: hidden;
  max-height: calc(100dvh - 2rem);
}

.modal-image {
  background: var(--bg);
  display: grid;
  place-items: center;
  padding: 1rem;
  overflow: hidden;
}

.modal-image img {
  width: 100%;
  max-height: 70vh;
  object-fit: contain;
  border-radius: var(--card-radius);
  box-shadow: var(--shadow-card);
}

.no-img.big {
  height: 60vh;
  display: grid;
  place-items: center;
  width: 100%;
}

.modal-detail {
  padding: 1.5rem 1.6rem 1.5rem;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 0.85rem;
}

.modal-detail header h2 {
  margin: 0.2rem 0 0;
  font-size: 1.5rem;
  color: var(--fg);
}

.modal-eyebrow {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

.muted {
  color: var(--fg-faint);
}

.badges {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
}

.badge {
  display: inline-flex;
  align-items: center;
  padding: 0.18rem 0.55rem;
  background: var(--bg-elev-2);
  border: 1px solid var(--border);
  color: var(--fg);
  border-radius: var(--radius-pill);
  font-size: 0.78rem;
  letter-spacing: 0.02em;
}

.badge.cost {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
}

.traits {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.95rem;
}

.card-text {
  margin: 0;
  color: var(--fg);
  white-space: pre-line;
  line-height: 1.5;
}

.card-text :deep(.icon) {
  display: inline-grid;
  place-items: center;
  width: 1.3em;
  height: 1.3em;
  margin: 0 0.06em;
  font-size: 0.7em;
  font-weight: 700;
  color: var(--on-accent);
  background: var(--accent);
  border-radius: var(--radius-sm);
  vertical-align: -0.18em;
}

.stub-note {
  margin: 0;
  padding: 0.55rem 0.7rem;
  background: var(--bg);
  border-left: 3px solid var(--accent);
  border-radius: var(--radius-sm);
  color: var(--fg-dim);
  font-size: 0.88rem;
}

.modal-footer {
  margin-top: auto;
  padding-top: 0.6rem;
  border-top: 1px solid var(--border);
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  justify-content: space-between;
  font-size: 0.84rem;
}

.close {
  position: absolute;
  top: 0.4rem;
  right: 0.5rem;
  width: 36px;
  height: 36px;
  display: grid;
  place-items: center;
  background: var(--overlay-soft);
  color: var(--on-accent);
  border: none;
  border-radius: var(--radius-circle);
  cursor: pointer;
  font-size: 1.4rem;
  line-height: 1;
  z-index: 1;
}

.close:hover {
  background: var(--overlay-strong);
}

@media (max-width: 720px) {
  .modal {
    grid-template-columns: 1fr;
    max-height: calc(100dvh - 2rem);
  }

  .modal-image {
    padding: 0.8rem;
  }

  .modal-image img {
    max-height: 45vh;
  }
}
</style>
