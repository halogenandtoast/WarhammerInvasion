<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'

const { t } = useI18n({ useScope: 'global' })

const sectionIds = [
  'overview',
  'structure',
  'setup',
  'deckbuilding',
  'cards',
  'turn',
  'combat',
  'effects',
  'keywords',
  'corruption',
  'control',
  'draft',
  'glossary',
] as const
type SectionId = (typeof sectionIds)[number]

const orderRaces = ['empire', 'dwarf', 'high_elf'] as const
const destructionRaces = ['chaos', 'orc', 'dark_elf'] as const
const flavourRaces = ['dwarf', 'empire', 'high_elf', 'orc', 'chaos', 'dark_elf'] as const

const anatomyParts = [
  'title',
  'cost',
  'race',
  'loyalty',
  'power',
  'card_type',
  'hp',
  'traits',
  'text',
  'collector',
] as const

const cardTypeRows = ['unit', 'support', 'tactic', 'quest', 'draft'] as const
const cardTypePersistence: Record<(typeof cardTypeRows)[number], 'yes' | 'no' | 'na'> = {
  unit: 'yes',
  support: 'yes',
  tactic: 'no',
  quest: 'yes',
  draft: 'na',
}

const keywordEntries = [
  'counterstrike',
  'toughness',
  'scout',
  'zone_only',
  'zone_prefixed',
  'limited',
  'order_destruction',
] as const

const glossaryEntries = [
  'action',
  'action_window',
  'active_player',
  'applied_damage',
  'assigned_damage',
  'attachment',
  'burn_token',
  'capital',
  'constant_effect',
  'controller',
  'corrupt',
  'development',
  'forced_effect',
  'loyalty',
  'non_combat_damage',
  'owner',
  'power_icon',
  'quest',
  'sacrifice',
  'trait',
  'uncancellable',
  'unique_card',
  'zone',
] as const

const draftCards = ['cut_supply', 'reinforcements', 'sabotage', 'shifting_tides'] as const

const sections = computed(() =>
  sectionIds.map((id) => ({ id, title: t(`rules.sections.${id}`) })),
)

const activeId = ref<SectionId>(sectionIds[0])
let observer: IntersectionObserver | null = null

onMounted(() => {
  observer = new IntersectionObserver(
    (entries) => {
      const visible = entries
        .filter((e) => e.isIntersecting)
        .sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top)
      if (visible[0]) activeId.value = visible[0].target.id as SectionId
    },
    { rootMargin: '-20% 0px -65% 0px', threshold: 0 },
  )
  for (const id of sectionIds) {
    const el = document.getElementById(id)
    if (el) observer.observe(el)
  }
})

onUnmounted(() => observer?.disconnect())

function jumpTo(event: Event, id: string) {
  const el = document.getElementById(id)
  if (!el) return
  event.preventDefault()
  el.scrollIntoView({ behavior: 'smooth', block: 'start' })
  history.replaceState(null, '', `#${id}`)
  activeId.value = id as SectionId
  const details = document.getElementById('toc-mobile') as HTMLDetailsElement | null
  if (details) details.open = false
}

// Delegate clicks inside v-html-rendered content for anchors that carry a
// data-jump attribute (Vue event bindings inside v-html don't apply).
function onContentClick(event: MouseEvent) {
  const anchor = (event.target as HTMLElement).closest<HTMLAnchorElement>('a[data-jump]')
  if (!anchor) return
  jumpTo(event, anchor.dataset.jump!)
}
</script>

<template>
  <div class="rules-page">
    <aside class="toc toc-desktop" :aria-label="t('rules.toc_label')">
      <p class="toc-eyebrow">{{ t('app.brand') }}</p>
      <h2 class="toc-title">{{ t('rules.page_title') }}</h2>
      <nav>
        <ol>
          <li v-for="(s, i) in sections" :key="s.id">
            <a
              :href="`#${s.id}`"
              :class="{ active: activeId === s.id }"
              :aria-current="activeId === s.id ? 'true' : undefined"
              @click="jumpTo($event, s.id)"
            >
              <span class="toc-num">{{ String(i + 1).padStart(2, '0') }}</span>
              <span class="toc-text">{{ s.title }}</span>
            </a>
          </li>
        </ol>
      </nav>
    </aside>

    <details id="toc-mobile" class="toc toc-mobile">
      <summary>
        <span>{{ t('rules.toc_mobile_summary') }}</span>
        <span class="toc-current">{{ sections.find((s) => s.id === activeId)?.title }}</span>
      </summary>
      <nav>
        <ol>
          <li v-for="(s, i) in sections" :key="s.id">
            <a
              :href="`#${s.id}`"
              :class="{ active: activeId === s.id }"
              @click="jumpTo($event, s.id)"
            >
              <span class="toc-num">{{ String(i + 1).padStart(2, '0') }}</span>
              <span class="toc-text">{{ s.title }}</span>
            </a>
          </li>
        </ol>
      </nav>
    </details>

    <div class="rules-content-wrap">
      <article class="rules-content" @click="onContentClick">
        <header class="rules-header">
          <h1>{{ t('rules.page_title_full') }}</h1>
          <p class="lead">{{ t('rules.lead') }}</p>
        </header>

        <section id="overview">
          <h2>{{ t('rules.overview.heading') }}</h2>
          <p v-html="t('rules.overview.intro')" />
          <h3>{{ t('rules.overview.winning_heading') }}</h3>
          <ul>
            <li v-html="t('rules.overview.winning_burn')" />
            <li>{{ t('rules.overview.winning_decking') }}</li>
          </ul>
          <h3 class="golden">{{ t('rules.overview.golden_heading') }}</h3>
          <blockquote>{{ t('rules.overview.golden_text') }}</blockquote>
        </section>

        <section id="structure">
          <h2>{{ t('rules.structure.heading') }}</h2>
          <h3>{{ t('rules.structure.zones_heading') }}</h3>
          <p v-html="t('rules.structure.zones_intro')" />
          <div class="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>{{ t('rules.structure.zone_table.col_zone') }}</th>
                  <th>{{ t('rules.structure.zone_table.col_base_power') }}</th>
                  <th>{{ t('rules.structure.zone_table.col_role') }}</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>{{ t('rules.structure.zone_table.kingdom') }}</td>
                  <td>{{ t('rules.structure.zone_table.kingdom_power') }}</td>
                  <td>{{ t('rules.structure.zone_table.kingdom_role') }}</td>
                </tr>
                <tr>
                  <td>{{ t('rules.structure.zone_table.quest') }}</td>
                  <td>{{ t('rules.structure.zone_table.quest_power') }}</td>
                  <td>{{ t('rules.structure.zone_table.quest_role') }}</td>
                </tr>
                <tr>
                  <td>{{ t('rules.structure.zone_table.battlefield') }}</td>
                  <td>{{ t('rules.structure.zone_table.battlefield_power') }}</td>
                  <td>{{ t('rules.structure.zone_table.battlefield_role') }}</td>
                </tr>
              </tbody>
            </table>
          </div>
          <p v-html="t('rules.structure.power_intro')" />

          <h3>{{ t('rules.structure.races_heading') }}</h3>
          <p>{{ t('rules.structure.races_intro') }}</p>
          <div class="faction-grid">
            <div class="faction order">
              <h4>{{ t('rules.structure.order') }}</h4>
              <ul>
                <li v-for="r in orderRaces" :key="r">{{ t(`rules.structure.race_${r}`) }}</li>
              </ul>
            </div>
            <div class="faction destruction">
              <h4>{{ t('rules.structure.destruction') }}</h4>
              <ul>
                <li v-for="r in destructionRaces" :key="r">{{ t(`rules.structure.race_${r}`) }}</li>
              </ul>
            </div>
          </div>
          <p v-html="t('rules.structure.neutral')" />
        </section>

        <section id="setup">
          <h2>{{ t('rules.setup.heading') }}</h2>
          <ol>
            <li v-html="t('rules.setup.shuffle')" />
            <li v-html="t('rules.setup.capital_boards')" />
            <li v-html="t('rules.setup.pool_tokens')" />
            <li v-html="t('rules.setup.first_player')" />
            <li v-html="t('rules.setup.draw_seven')" />
          </ol>
        </section>

        <section id="deckbuilding">
          <h2>{{ t('rules.deckbuilding.heading') }}</h2>
          <ul>
            <li v-html="t('rules.deckbuilding.min_max')" />
            <li v-html="t('rules.deckbuilding.max_copies')" />
            <li>{{ t('rules.deckbuilding.no_mixing') }}</li>
            <li>{{ t('rules.deckbuilding.races_within') }}</li>
          </ul>

          <h3>{{ t('rules.deckbuilding.race_flavours_heading') }}</h3>
          <dl class="keywords">
            <template v-for="r in flavourRaces" :key="r">
              <dt>{{ t(`rules.deckbuilding.race_${r}`) }}</dt>
              <dd>{{ t(`rules.deckbuilding.race_${r}_desc`) }}</dd>
            </template>
          </dl>

          <h3>{{ t('rules.deckbuilding.loyalty_heading') }}</h3>
          <p v-html="t('rules.deckbuilding.loyalty_text')" />
          <pre><code>{{ t('rules.deckbuilding.loyalty_formula') }}</code></pre>
          <div class="callout" v-html="t('rules.deckbuilding.loyalty_example')" />
        </section>

        <section id="cards">
          <h2>{{ t('rules.cards.heading') }}</h2>
          <h3>{{ t('rules.cards.types_heading') }}</h3>
          <div class="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>{{ t('rules.cards.type_table.col_type') }}</th>
                  <th>{{ t('rules.cards.type_table.col_persistent') }}</th>
                  <th>{{ t('rules.cards.type_table.col_notes') }}</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="row in cardTypeRows" :key="row">
                  <td>{{ t(`rules.cards.type_table.${row}`) }}</td>
                  <td>{{ t(`rules.cards.type_table.${cardTypePersistence[row]}`) }}</td>
                  <td>{{ t(`rules.cards.type_table.${row}_notes`) }}</td>
                </tr>
              </tbody>
            </table>
          </div>
          <p v-html="t('rules.cards.attachments_note')" />

          <h3>{{ t('rules.cards.anatomy_heading') }}</h3>
          <ol class="anatomy">
            <li v-for="key in anatomyParts" :key="key" v-html="t(`rules.cards.anatomy.${key}`)" />
          </ol>

          <h3>{{ t('rules.cards.unique_heading') }}</h3>
          <p v-html="t('rules.cards.unique_text')" />

          <h3>{{ t('rules.cards.developments_heading') }}</h3>
          <p v-html="t('rules.cards.developments_text')" />

          <h3>{{ t('rules.cards.quest_heading') }}</h3>
          <p v-html="t('rules.cards.quest_text_1')" />
          <p v-html="t('rules.cards.quest_text_2')" />
          <p v-html="t('rules.cards.quest_text_3')" />
        </section>

        <section id="turn">
          <h2>{{ t('rules.turn.heading') }}</h2>
          <p v-html="t('rules.turn.intro')" />
          <ol class="phases">
            <li>
              <h3>{{ t('rules.turn.kingdom_heading') }}</h3>
              <p v-html="t('rules.turn.kingdom_text')" />
            </li>
            <li>
              <h3>{{ t('rules.turn.quest_heading') }}</h3>
              <p v-html="t('rules.turn.quest_text')" />
            </li>
            <li>
              <h3>{{ t('rules.turn.capital_heading') }}</h3>
              <p v-html="t('rules.turn.capital_text')" />
            </li>
            <li>
              <h3>{{ t('rules.turn.battlefield_heading') }}</h3>
              <p v-html="t('rules.turn.battlefield_text')" />
            </li>
          </ol>
          <div class="callout" v-html="t('rules.turn.first_player_penalty')" />
        </section>

        <section id="combat">
          <h2>{{ t('rules.combat.heading') }}</h2>
          <p v-html="t('rules.combat.intro')" />
          <ol class="combat-steps">
            <li>
              <h3>{{ t('rules.combat.declare_target_heading') }}</h3>
              <p>{{ t('rules.combat.declare_target_text') }}</p>
            </li>
            <li>
              <h3>{{ t('rules.combat.declare_attackers_heading') }}</h3>
              <p>{{ t('rules.combat.declare_attackers_text') }}</p>
            </li>
            <li>
              <h3>{{ t('rules.combat.declare_defenders_heading') }}</h3>
              <p>{{ t('rules.combat.declare_defenders_text') }}</p>
            </li>
            <li>
              <h3>{{ t('rules.combat.assign_damage_heading') }}</h3>
              <p v-html="t('rules.combat.assign_damage_text')" />
              <p class="hint" v-html="t('rules.combat.assign_damage_hint')" />
            </li>
            <li>
              <h3>{{ t('rules.combat.apply_damage_heading') }}</h3>
              <p v-html="t('rules.combat.apply_damage_text')" />
            </li>
          </ol>
          <div class="callout" v-html="t('rules.combat.counterstrike_callout')" />

          <h3>{{ t('rules.combat.non_combat_heading') }}</h3>
          <p v-html="t('rules.combat.non_combat_text')" />
        </section>

        <section id="effects">
          <h2>{{ t('rules.effects.heading') }}</h2>
          <p>{{ t('rules.effects.intro') }}</p>

          <h3>{{ t('rules.effects.action_heading') }}</h3>
          <p v-html="t('rules.effects.action_text')" />

          <h3>{{ t('rules.effects.forced_heading') }}</h3>
          <p v-html="t('rules.effects.forced_text')" />

          <h3>{{ t('rules.effects.constant_heading') }}</h3>
          <p>{{ t('rules.effects.constant_text') }}</p>

          <h3>{{ t('rules.effects.response_heading') }}</h3>
          <p v-html="t('rules.effects.response_text')" />
          <div class="callout" v-html="t('rules.effects.response_example')" />
        </section>

        <section id="keywords">
          <h2>{{ t('rules.keywords.heading') }}</h2>
          <dl class="keywords">
            <template v-for="k in keywordEntries" :key="k">
              <dt>{{ t(`rules.keywords.${k}_term`) }}</dt>
              <dd v-html="t(`rules.keywords.${k}_def`)" />
            </template>
          </dl>
        </section>

        <section id="corruption">
          <h2>{{ t('rules.corruption.heading') }}</h2>
          <h3>{{ t('rules.corruption.corruption_heading') }}</h3>
          <p v-html="t('rules.corruption.corruption_text')" />

          <h3>{{ t('rules.corruption.sacrifice_heading') }}</h3>
          <p v-html="t('rules.corruption.sacrifice_text')" />

          <h3>{{ t('rules.corruption.cannot_heading') }}</h3>
          <p v-html="t('rules.corruption.cannot_text')" />
        </section>

        <section id="control">
          <h2>{{ t('rules.control.heading') }}</h2>
          <p v-html="t('rules.control.text')" />
          <h3>{{ t('rules.control.corresponding_heading') }}</h3>
          <p>{{ t('rules.control.corresponding_text') }}</p>
          <h3>{{ t('rules.control.decking_heading') }}</h3>
          <p v-html="t('rules.control.decking_text')" />
        </section>

        <section id="draft">
          <h2>{{ t('rules.draft.heading') }}</h2>
          <p>{{ t('rules.draft.intro') }}</p>

          <h3>{{ t('rules.draft.setup_heading') }}</h3>
          <ol>
            <li v-html="t('rules.draft.setup_1')" />
            <li>{{ t('rules.draft.setup_2') }}</li>
            <li v-html="t('rules.draft.setup_3')" />
            <li>{{ t('rules.draft.setup_4') }}</li>
          </ol>

          <h3>{{ t('rules.draft.pack_heading') }}</h3>
          <p v-html="t('rules.draft.pack_intro')" />
          <ol>
            <li v-html="t('rules.draft.pack_1')" />
            <li v-html="t('rules.draft.pack_2')" />
            <li>{{ t('rules.draft.pack_3') }}</li>
            <li v-html="t('rules.draft.pack_4')" />
          </ol>
          <p v-html="t('rules.draft.pack_outro')" />
          <div class="callout" v-html="t('rules.draft.short_pack')" />

          <h3>{{ t('rules.draft.format_heading') }}</h3>
          <p>{{ t('rules.draft.format_intro') }}</p>
          <dl class="keywords">
            <template v-for="c in draftCards" :key="c">
              <dt>{{ t(`rules.draft.${c}_term`) }}</dt>
              <dd v-html="t(`rules.draft.${c}_def`)" />
            </template>
          </dl>
        </section>

        <section id="glossary">
          <h2>{{ t('rules.glossary.heading') }}</h2>
          <dl class="glossary">
            <template v-for="g in glossaryEntries" :key="g">
              <dt>{{ t(`rules.glossary.${g}_term`) }}</dt>
              <dd v-html="t(`rules.glossary.${g}_def`)" />
            </template>
          </dl>
        </section>
      </article>
    </div>
  </div>
</template>

<style scoped>
.rules-page {
  display: grid;
  grid-template-columns: minmax(260px, 300px) minmax(0, 1fr);
  gap: 0;
  min-height: 100dvh;
  background: var(--bg);
}

.rules-content-wrap {
  container-type: inline-size;
  min-width: 0;
}

/* ---------- Desktop TOC ---------- */
.toc-desktop {
  position: sticky;
  top: 0;
  align-self: start;
  max-height: 100dvh;
  overflow-y: auto;
  padding: 2.5rem 1.75rem 2rem;
  background: var(--bg-elev);
  border-right: 1px solid var(--border);
}

.toc-eyebrow {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.toc-title {
  margin: 0.4rem 0 1.5rem;
  font-size: 1.6rem;
  color: var(--fg);
}

.toc nav ol {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
}

.toc nav a {
  display: flex;
  align-items: center;
  gap: 0.8rem;
  min-height: var(--tap-target);
  padding: 0.55rem 0.7rem;
  border-radius: var(--radius-md);
  color: var(--fg-dim);
  font-size: 0.95rem;
  border-left: 2px solid transparent;
}

.toc nav a:hover {
  color: var(--fg);
  background: var(--bg-elev-2);
  text-decoration: none;
}

.toc nav a.active {
  color: var(--fg);
  background: var(--bg-elev-2);
  border-left-color: var(--accent);
}

.toc-num {
  font-family: var(--font-mono);
  font-size: 0.78rem;
  color: var(--fg-faint);
  min-width: 1.6em;
}

.toc nav a.active .toc-num {
  color: var(--accent-strong);
}

/* ---------- Mobile TOC ---------- */
.toc-mobile {
  display: none;
}

/* ---------- Article ---------- */
.rules-content {
  max-width: 95cqi;
  width: 100%;
  margin: 0 auto;
  padding: 3rem 2rem 5rem;
}

.rules-header {
  margin-bottom: 3rem;
  padding-bottom: 2rem;
  border-bottom: 1px solid var(--border);
}

.eyebrow {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

.rules-header h1 {
  margin: 0.5rem 0 1rem;
  font-size: clamp(1.85rem, 4vw, 2.7rem);
  color: var(--fg);
}

.lead {
  margin: 0;
  font-size: 1.08rem;
  color: var(--fg-dim);
  max-width: 60ch;
}

.rules-content section {
  margin: 0 0 3rem;
  scroll-margin-top: 1rem;
}

.rules-content h2 {
  font-size: 1.55rem;
  margin-bottom: 1.1rem;
  color: var(--fg);
}

.rules-content h3 {
  font-size: 1.1rem;
  margin: 1.6rem 0 0.6rem;
  color: var(--fg);
}

.rules-content h3.golden {
  margin-top: 2rem;
  color: var(--accent-strong);
}

.rules-content h4 {
  font-size: 1rem;
  margin: 1.2rem 0 0.4rem;
}

.rules-content p {
  margin: 0 0 0.9rem;
  color: var(--fg);
}

.rules-content ul,
.rules-content ol {
  margin: 0 0 1rem;
  padding-left: 1.4rem;
  color: var(--fg);
}

.rules-content li {
  margin-bottom: 0.35rem;
}

.rules-content blockquote {
  margin: 0 0 1rem;
  padding: 0.9rem 1.1rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent);
  border-radius: var(--radius-sm);
  font-style: italic;
  color: var(--fg);
}

.rules-content pre {
  margin: 0 0 1rem;
  padding: 0.9rem 1.1rem;
  background: var(--bg-elev);
  border-radius: var(--radius-md);
  overflow-x: auto;
  font-size: 0.92rem;
}

.rules-content pre code {
  background: transparent;
  padding: 0;
}

.callout {
  margin: 1.4rem 0;
  padding: 1rem 1.2rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent);
  border-radius: var(--radius-sm);
}

.hint {
  color: var(--fg-dim);
  font-size: 0.95rem;
}

/* ---------- Tables ---------- */
.table-wrap {
  overflow-x: auto;
  margin: 0 0 1rem;
  border-radius: var(--radius-md);
  border: 1px solid var(--border);
}

.rules-content table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.95rem;
}

.rules-content th,
.rules-content td {
  text-align: left;
  padding: 0.7rem 0.9rem;
  border-bottom: 1px solid var(--border);
  vertical-align: top;
}

.rules-content th {
  background: var(--bg-elev);
  font-weight: 600;
  color: var(--fg);
}

.rules-content tr:last-child td {
  border-bottom: none;
}

/* ---------- Faction blocks ---------- */
.faction-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 1rem;
  margin: 0 0 1rem;
}

.faction {
  padding: 1rem 1.2rem;
  background: var(--bg-elev);
  border-radius: var(--radius-md);
  border-top: 3px solid;
}

.faction.order {
  border-top-color: var(--faction-order);
}

.faction.destruction {
  border-top-color: var(--faction-destruction);
}

.faction h4 {
  margin: 0 0 0.5rem;
}

.faction ul {
  margin: 0;
  padding-left: 1.2rem;
}

/* ---------- Phases / combat steps ---------- */
.phases,
.combat-steps {
  counter-reset: step;
  list-style: none;
  padding: 0;
}

.phases > li,
.combat-steps > li {
  position: relative;
  padding: 0 0 0 3.2rem;
  margin-bottom: 1.4rem;
  counter-increment: step;
}

.phases > li::before,
.combat-steps > li::before {
  content: counter(step);
  position: absolute;
  left: 0;
  top: 0.05em;
  width: 2.2rem;
  height: 2.2rem;
  display: grid;
  place-items: center;
  border-radius: var(--radius-circle);
  background: var(--bg-elev);
  border: 1px solid var(--border);
  color: var(--accent-strong);
  font-weight: 600;
  font-family: var(--font-mono);
}

.phases > li h3,
.combat-steps > li h3 {
  margin: 0 0 0.4rem;
  font-size: 1.05rem;
}

.phases > li p,
.combat-steps > li p {
  margin: 0 0 0.6rem;
}

/* ---------- Anatomy list ---------- */
.anatomy {
  padding-left: 1.4rem;
}

/* ---------- Definition lists ---------- */
.keywords,
.glossary {
  margin: 0;
}

.keywords dt,
.glossary dt {
  font-weight: 600;
  color: var(--fg);
  margin-top: 1rem;
}

.keywords dd,
.glossary dd {
  margin: 0.2rem 0 0.4rem;
  color: var(--fg-dim);
}

.rules-footer {
  margin-top: 4rem;
  padding-top: 1.5rem;
  border-top: 1px solid var(--border);
  color: var(--fg-faint);
  font-size: 0.88rem;
}

/* ---------- Responsive ---------- */
@media (max-width: 900px) {
  .rules-page {
    grid-template-columns: 1fr;
  }

  .toc-desktop {
    display: none;
  }

  .toc-mobile {
    display: block;
    position: sticky;
    top: 0;
    z-index: var(--z-toc-mobile);
    background: var(--bg-elev);
    border-bottom: 1px solid var(--border);
  }

  .toc-mobile > summary {
    list-style: none;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
    padding: 1rem 1.2rem;
    min-height: var(--tap-target);
    font-weight: 600;
    color: var(--fg);
  }

  .toc-mobile > summary::-webkit-details-marker {
    display: none;
  }

  .toc-mobile > summary::after {
    content: "▾";
    color: var(--fg-faint);
    transition: transform var(--transition-base);
  }

  .toc-mobile[open] > summary::after {
    transform: rotate(180deg);
  }

  .toc-mobile .toc-current {
    color: var(--fg-dim);
    font-weight: 400;
    font-size: 0.92rem;
    text-align: right;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .toc-mobile nav {
    padding: 0.4rem 1.2rem 1rem;
    max-height: 60vh;
    overflow-y: auto;
  }

  .toc-mobile nav a {
    min-height: var(--tap-target);
  }

  .rules-content {
    padding: 2rem 1.2rem 4rem;
  }

  .rules-header {
    margin-bottom: 2rem;
  }
}

@media (max-width: 480px) {
  .rules-content {
    padding: 1.5rem 1rem 3rem;
  }

  .rules-content h2 {
    font-size: 1.3rem;
  }

  .phases > li,
  .combat-steps > li {
    padding-left: 2.6rem;
  }

  .phases > li::before,
  .combat-steps > li::before {
    width: 1.9rem;
    height: 1.9rem;
    font-size: 0.85rem;
  }
}
</style>
