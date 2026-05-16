<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'

const sections = [
  { id: 'overview', title: 'Overview' },
  { id: 'structure', title: 'Game structure' },
  { id: 'setup', title: 'Setup' },
  { id: 'deckbuilding', title: 'Deckbuilding' },
  { id: 'cards', title: 'Cards' },
  { id: 'turn', title: 'Turn sequence' },
  { id: 'combat', title: 'Combat' },
  { id: 'effects', title: 'Effects' },
  { id: 'keywords', title: 'Keywords' },
  { id: 'corruption', title: 'Corruption & sacrifice' },
  { id: 'control', title: 'Control & ownership' },
  { id: 'glossary', title: 'Glossary' },
] as const

const activeId = ref<string>(sections[0].id)
let observer: IntersectionObserver | null = null

onMounted(() => {
  observer = new IntersectionObserver(
    (entries) => {
      const visible = entries
        .filter((e) => e.isIntersecting)
        .sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top)
      if (visible[0]) activeId.value = visible[0].target.id
    },
    { rootMargin: '-20% 0px -65% 0px', threshold: 0 },
  )
  for (const s of sections) {
    const el = document.getElementById(s.id)
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
  activeId.value = id
  // close the mobile TOC after navigating
  const details = document.getElementById('toc-mobile') as HTMLDetailsElement | null
  if (details) details.open = false
}
</script>

<template>
  <div class="rules-page">
    <aside class="toc toc-desktop" aria-label="Table of contents">
      <p class="toc-eyebrow">Warhammer: Invasion</p>
      <h2 class="toc-title">Rules</h2>
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
        <span>Table of contents</span>
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

    <article class="rules-content">
      <header class="rules-header">
        <p class="eyebrow">Living Card Game · Core Set</p>
        <h1>Warhammer: Invasion — Rules</h1>
        <p class="lead">
          A two-player duelling game where you raise armies, send heroes on quests,
          and burn the sections of your opponent's capital. First player to set fire
          to two of the three sections of their opponent's capital wins.
        </p>
      </header>

      <section id="overview">
        <h2>1. Overview</h2>
        <p>
          Each player commands a <strong>capital</strong> divided into three zones —
          the <em>kingdom</em>, the <em>quest zone</em>, and the <em>battlefield</em>.
          Cards from the Old World are played into these zones to gather resources,
          draw more cards, and march on the enemy.
        </p>
        <h3>Winning</h3>
        <ul>
          <li>Burn <strong>two of the three</strong> sections of your opponent's capital, or</li>
          <li>force your opponent to run out of cards in their deck (immediate elimination).</li>
        </ul>
        <h3 class="golden">The Golden Rule</h3>
        <blockquote>
          If the rules text of a card contradicts the text of this rulebook, the rules on
          the card take precedence.
        </blockquote>
      </section>

      <section id="structure">
        <h2>2. Game structure</h2>
        <h3>Zones</h3>
        <p>
          Each player has three zones. Each zone starts at <strong>8 hit points</strong>
          (raised by <em>developments</em>, see Cards).
        </p>
        <div class="table-wrap">
          <table>
            <thead>
              <tr><th>Zone</th><th>Base power</th><th>Role</th></tr>
            </thead>
            <tbody>
              <tr><td>Kingdom</td><td>3</td><td>Produces resources during the Kingdom phase.</td></tr>
              <tr><td>Quest zone</td><td>1</td><td>Draws cards during the Quest phase. Quests live here.</td></tr>
              <tr><td>Battlefield</td><td>0</td><td>Attacks the opponent during the Battlefield phase.</td></tr>
            </tbody>
          </table>
        </div>
        <p>
          A zone's <strong>power</strong> = base power + power icons on cards in that zone +
          any temporary modifiers. The power of a zone determines its output in the
          phase it corresponds to.
        </p>

        <h3>Races and factions</h3>
        <p>Six races, split into two factions. Cards from different factions cannot share a deck.</p>
        <div class="faction-grid">
          <div class="faction order">
            <h4>Order</h4>
            <ul><li>Empire</li><li>Dwarf</li><li>High Elf</li></ul>
          </div>
          <div class="faction destruction">
            <h4>Destruction</h4>
            <ul><li>Chaos</li><li>Orc</li><li>Dark Elf</li></ul>
          </div>
        </div>
        <p>
          <strong>Neutral</strong> cards (grey border, no race symbol) can be used in either
          faction unless their text says otherwise.
        </p>
      </section>

      <section id="setup">
        <h2>3. Setup</h2>
        <ol>
          <li><strong>Shuffle decks.</strong> Either player may cut or shuffle the opponent's deck.</li>
          <li><strong>Place capital boards</strong> in front of each player.</li>
          <li><strong>Pool tokens</strong> (resource, damage, burn) in the centre of the play area.</li>
          <li><strong>Determine first player</strong> randomly (coin flip or similar).</li>
          <li>
            <strong>Draw 7 cards</strong> as your starting hand. Either player may take one
            <em>mulligan</em>: shuffle those 7 back in and redraw a new hand of 7. The
            second hand is final.
          </li>
        </ol>
      </section>

      <section id="deckbuilding">
        <h2>4. Deckbuilding</h2>
        <ul>
          <li>Minimum <strong>50 cards</strong>, maximum <strong>100 cards</strong>.</li>
          <li>No more than <strong>3 copies</strong> of any card by title.</li>
          <li>Order and Destruction cards cannot share a deck.</li>
          <li>You may mix races within a faction; doing so makes loyalty management harder.</li>
        </ul>
        <h3>Loyalty</h3>
        <p>
          A card's <em>loyalty cost</em> is the number of loyalty icons under its printed
          cost. It is reduced (to a floor of zero) by the number of matching race
          symbols you control in play. Each capital board provides one race symbol
          for its faction.
        </p>
        <pre><code>total_cost = printed_cost + max(0, loyalty_icons - matching_race_symbols)</code></pre>
      </section>

      <section id="cards">
        <h2>5. Cards</h2>
        <h3>The five card types</h3>
        <div class="table-wrap">
          <table>
            <thead>
              <tr><th>Type</th><th>Persistent?</th><th>Notes</th></tr>
            </thead>
            <tbody>
              <tr><td>Unit</td><td>Yes</td><td>Attack, defend, quest, or produce resources.</td></tr>
              <tr><td>Support</td><td>Yes</td><td>Buildings, weapons, attachments. Stay in play until destroyed.</td></tr>
              <tr><td>Tactic</td><td>No</td><td>Single-shot. Played from hand, resolves, discarded.</td></tr>
              <tr><td>Quest</td><td>Yes</td><td>Lives in the quest zone; a unit may be sent on it.</td></tr>
              <tr><td>Draft format</td><td>n/a</td><td>Used only during the draft variant; never enters a deck.</td></tr>
            </tbody>
          </table>
        </div>
        <p><strong>Attachments:</strong> a support card attached to another card is sacrificed when its host leaves play.</p>

        <h3>Anatomy of a card</h3>
        <ol class="anatomy">
          <li><strong>Title.</strong> A banner before the title marks the card as <em>unique</em>.</li>
          <li><strong>Cost.</strong> Printed resource cost (upper left).</li>
          <li><strong>Race symbol.</strong></li>
          <li><strong>Loyalty icons.</strong> Additional variable cost, reduced by matching race symbols you control.</li>
          <li><strong>Power icons.</strong> Resources in kingdom, cards drawn in quest, damage in battlefield.</li>
          <li><strong>Card type.</strong></li>
          <li><strong>Hit points</strong> (units only).</li>
          <li><strong>Traits.</strong> Flavor tags with no inherent rules (e.g. Warrior, Spell, Building).</li>
          <li><strong>Card text.</strong> The card's unique effects.</li>
          <li><strong>Collector info.</strong></li>
        </ol>

        <h3>Unique cards</h3>
        <p>
          A player cannot have two copies of the same unique card in play at once.
          Both players may, however, each have their own copy. A unique card in the
          discard pile does not prevent playing another copy.
        </p>

        <h3>Developments</h3>
        <p>
          During the Capital phase, the active player may play <strong>one</strong> card
          from hand face-down as a <em>development</em> into any of their three zones.
          Each development adds <strong>+1 HP</strong> to that zone. You may inspect
          your own developments at any time, but not your opponent's.
        </p>

        <h3>Sending a unit on a quest</h3>
        <p>
          When you play a unit during the Capital phase, you may play it on top of a
          quest card already in your quest zone. The unit stays in that zone (and can
          defend it), but is also <em>questing</em>. Only one unit can quest on a given
          quest card at a time. If the questing unit leaves play, the quest stays but
          its accumulated resource tokens are lost.
        </p>
      </section>

      <section id="turn">
        <h2>6. Turn sequence</h2>
        <p>
          The active player completes all four phases in order before passing the
          turn. Every phase has one or more <em>action windows</em> where both
          players may take actions; the engine advances only after both players pass
          consecutively.
        </p>
        <ol class="phases">
          <li>
            <h3>Kingdom</h3>
            <p>
              Active player returns all unspent resources to the pool, may
              <strong>restore one</strong> corrupt card they control, then collects
              resources equal to the total power in their kingdom (base 3 + power
              icons in that zone).
            </p>
          </li>
          <li>
            <h3>Quest</h3>
            <p>
              Active player draws cards equal to the total power in their quest zone
              (base 1 + power icons there).
            </p>
          </li>
          <li>
            <h3>Capital</h3>
            <p>
              The only phase in which the active player may play unit, support, and
              quest cards from their hand. They may also play one card face-down as a
              development. The non-active player may still play tactics and trigger
              abilities. The phase ends when both players consecutively pass.
            </p>
          </li>
          <li>
            <h3>Battlefield</h3>
            <p>
              The active player may attack one of the opponent's three zones. See
              <a href="#combat" @click="jumpTo($event, 'combat')">Combat</a> for the
              five-step procedure.
            </p>
          </li>
        </ol>
        <div class="callout">
          <strong>First player penalty.</strong> On the <em>very first</em> turn of
          the game, the starting player skips Quest and Battlefield entirely. This
          applies only to turn 1, only to player 1.
        </div>
      </section>

      <section id="combat">
        <h2>7. Combat</h2>
        <p>
          The Battlefield phase is a five-step procedure. <strong>Each step is
          followed by an action window</strong>; the engine advances only after both
          players pass.
        </p>
        <ol class="combat-steps">
          <li>
            <h3>Declare target of attack</h3>
            <p>The active player picks one of the opponent's three zones to attack.</p>
          </li>
          <li>
            <h3>Declare attackers</h3>
            <p>
              The active player chooses any subset of units in their own battlefield to
              attack. Only battlefield units may attack. Corrupt units cannot attack.
            </p>
          </li>
          <li>
            <h3>Declare defenders</h3>
            <p>
              The defending player chooses any subset of their units in the attacked
              zone to defend. Combat is not per-unit pairing — all attackers attack
              together, all defenders defend together. Corrupt units cannot defend.
            </p>
          </li>
          <li>
            <h3>Assign damage</h3>
            <p>
              Each side's inflicted damage equals the total power of its participating
              units. The attacker assigns first and must cover defending units'
              remaining HP before damage can spill over onto the capital. The defender
              assigns next, only to attacking units — defenders <strong>cannot damage
              the attacker's capital</strong>. Either side may over-assign damage to a
              single unit in anticipation of Toughness or cancellation.
            </p>
            <p class="hint">
              Damage at this step is <em>assigned</em>, not yet committed. Tokens are
              placed near their targets and can still be cancelled.
            </p>
          </li>
          <li>
            <h3>Apply damage</h3>
            <p>
              Both sides commit assigned damage simultaneously. Toughness fires here,
              cancelling damage before it lands. Units at or over their HP are
              destroyed. A capital section reaching zero HP <strong>burns</strong>:
              all damage tokens are removed from that section, a burn token is placed,
              and that section is overrun. Two burns ends the game.
            </p>
          </li>
        </ol>
        <div class="callout">
          <strong>Counterstrike is the exception.</strong> A unit with
          <code>Counterstrike N</code> deals N <em>uncancellable</em> damage to a
          single attacker the instant it is declared as a defender (in step 3),
          before regular damage is assigned. It still also fights normally in
          steps 4 and 5.
        </div>
      </section>

      <section id="effects">
        <h2>8. Effects</h2>
        <p>Card effects come in four shapes. The first three are described here; keywords are in the next section.</p>

        <h3>Action</h3>
        <p>
          Marked <code>Action:</code> on a card. Optional. Triggerable by either
          player during any action window. Tactic cards are themselves actions —
          played from hand into the discard pile.
        </p>

        <h3>Forced</h3>
        <p>
          Marked <code>Forced:</code>. Triggers automatically when its condition is
          met. Cannot be cancelled or interrupted, and resolves immediately.
        </p>

        <h3>Constant</h3>
        <p>
          Card text without a bold trigger. Continuously affects the game state
          while the card is in play and any stated condition holds.
        </p>

        <h3>Playing an action "in response"</h3>
        <p>
          Actions can be triggered in response to other actions. They resolve
          <strong>last-in, first-out</strong>: the response resolves before the
          action it interrupted. All costs are paid and targets chosen at the time
          the action is triggered, regardless of when it actually resolves. An
          action already in the chain resolves even if its source leaves play.
        </p>
      </section>

      <section id="keywords">
        <h2>9. Keywords</h2>
        <dl class="keywords">
          <dt>Counterstrike N</dt>
          <dd>
            On being declared a defender, deal N uncancellable damage to one
            attacker of the defender's choice. Applied immediately, before regular
            damage. Does not consume the unit's normal combat damage.
          </dd>

          <dt>Toughness N</dt>
          <dd>
            Cancel N damage assigned to this unit before it is applied. Stacks
            additively across sources. Does not affect damage tagged
            <em>uncancellable</em>.
          </dd>

          <dt>Scout</dt>
          <dd>
            After combat damage is applied, each surviving participating Scout unit
            forces the opponent to discard one random card from hand.
          </dd>

          <dt>Kingdom / Quest / Battlefield only</dt>
          <dd>
            Restricts the zone the card may enter play in. Once in play, card
            effects can still move it to another zone.
          </dd>

          <dt>Zone-prefixed effects</dt>
          <dd>
            An effect prefixed with an italicised zone name
            (<em>Kingdom.</em>, <em>Quest.</em>, <em>Battlefield.</em>) is active
            only while the card is in that zone.
          </dd>

          <dt>Limited</dt>
          <dd>You may play at most one card with the Limited keyword per turn.</dd>

          <dt>Order Only / Destruction Only</dt>
          <dd>Restricts an otherwise-neutral card to a single faction.</dd>
        </dl>
      </section>

      <section id="corruption">
        <h2>10. Corruption & sacrifice</h2>
        <h3>Corruption</h3>
        <p>
          Some effects <em>corrupt</em> a card — physically, you turn it 90
          degrees. A corrupt card cannot be declared as an attacker or defender.
          At the start of their Kingdom phase, before any actions, the active
          player may <strong>restore one</strong> of their corrupt cards by
          turning it back upright. One restoration per turn.
        </p>

        <h3>Sacrifice</h3>
        <p>
          When a player is instructed to sacrifice a card, it goes to its
          <em>owner's</em> discard pile. Sacrifice is <strong>absolute</strong>;
          it cannot be cancelled by any other effect. Attachments are
          automatically sacrificed when their host card leaves play.
        </p>

        <h3>"Cannot" is absolute</h3>
        <p>
          If an effect says <code>cannot</code>, it overrides anything that says
          otherwise. <em>Attached unit cannot be corrupted</em> beats
          <em>Action: Corrupt one target unit</em>.
        </p>
      </section>

      <section id="control">
        <h2>11. Control & ownership</h2>
        <p>
          The <strong>owner</strong> of a card is the player whose deck it began
          in. The <strong>controller</strong> is whoever currently controls it —
          usually the owner, unless an effect has stolen control. When a card
          leaves play, it returns to its <em>owner's</em> hand, deck, or discard
          pile as the effect dictates.
        </p>
        <h3>Corresponding zones</h3>
        <p>
          When a card refers to an opponent's "corresponding zone," it means the
          opponent's zone of the same name as the one the card currently occupies.
          A Battlefield unit referring to "the opponent's corresponding zone"
          means the opponent's battlefield.
        </p>
        <h3>Decking out</h3>
        <p>
          If a player ever has zero cards in their deck, they are
          <strong>immediately eliminated</strong>. The check is at the moment
          of the empty deck, not at the end of a phase.
        </p>
      </section>

      <section id="glossary">
        <h2>12. Glossary</h2>
        <dl class="glossary">
          <dt>Action</dt>
          <dd>Optional <code>Action:</code> effect; triggerable in any action window.</dd>

          <dt>Action window</dt>
          <dd>
            A pause between phase steps where either player may take actions.
            The engine advances only after both pass consecutively.
          </dd>

          <dt>Active player</dt>
          <dd>The player whose turn it currently is.</dd>

          <dt>Applied damage</dt>
          <dd>Damage committed to a target. Toughness fires here.</dd>

          <dt>Assigned damage</dt>
          <dd>Damage placed near a target but not yet committed; still cancellable.</dd>

          <dt>Attachment</dt>
          <dd>A support card attached to another card. Sacrificed when its host leaves play.</dd>

          <dt>Burn token</dt>
          <dd>Placed on an overrun capital section. Two burns on one player ends the game.</dd>

          <dt>Capital</dt>
          <dd>A player's three-zone area: kingdom, quest zone, battlefield.</dd>

          <dt>Constant effect</dt>
          <dd>Card text without a bold trigger; active while the card is in play.</dd>

          <dt>Controller</dt>
          <dd>The player currently in control of a card. Defaults to the owner.</dd>

          <dt>Corrupt</dt>
          <dd>Flagged so as to be unable to attack or defend. Restored at most once per Kingdom phase.</dd>

          <dt>Development</dt>
          <dd>Face-down card in a zone, adding +1 HP. Max one per turn.</dd>

          <dt>Forced effect</dt>
          <dd><code>Forced:</code> effect; fires automatically, cannot be cancelled.</dd>

          <dt>Loyalty</dt>
          <dd>
            Variable cost portion paid in resources; reduced by matching race
            symbols, floor of zero.
          </dd>

          <dt>Owner</dt>
          <dd>The player whose deck the card began in; cards revert here on leaving play.</dd>

          <dt>Power icon</dt>
          <dd>Symbol on a card. In Kingdom → resources, Quest → draw, Battlefield → damage.</dd>

          <dt>Quest</dt>
          <dd>Persistent card in the quest zone; accumulates resources via a questing unit.</dd>

          <dt>Sacrifice</dt>
          <dd>Send a card to its owner's discard pile. Absolute; cannot be cancelled.</dd>

          <dt>Trait</dt>
          <dd>Flavor tag (Warrior, Hero, Spell, Building, …). No inherent rules.</dd>

          <dt>Uncancellable damage</dt>
          <dd>Damage that bypasses Toughness and cancellation effects.</dd>

          <dt>Unique card</dt>
          <dd>
            Marked with a banner before its title. A player may not control more
            than one copy at a time.
          </dd>

          <dt>Zone</dt>
          <dd>One of Kingdom, Quest, Battlefield. Each player has three, each at 8 base HP.</dd>
        </dl>
      </section>

      <footer class="rules-footer">
        <p>Condensed from the Warhammer: Invasion LCG Core Set rulebook.</p>
      </footer>
    </article>
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
  min-height: 44px;
  padding: 0.55rem 0.7rem;
  border-radius: 6px;
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
  font-family: "SF Mono", "JetBrains Mono", Menlo, Consolas, monospace;
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
  max-width: 760px;
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
  border-radius: 4px;
  font-style: italic;
  color: var(--fg);
}

.rules-content pre {
  margin: 0 0 1rem;
  padding: 0.9rem 1.1rem;
  background: var(--bg-elev);
  border-radius: 6px;
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
  border-radius: 4px;
}

.hint {
  color: var(--fg-dim);
  font-size: 0.95rem;
}

/* ---------- Tables ---------- */
.table-wrap {
  overflow-x: auto;
  margin: 0 0 1rem;
  border-radius: 6px;
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
  border-radius: 6px;
  border-top: 3px solid;
}

.faction.order {
  border-top-color: #6e9bd1;
}

.faction.destruction {
  border-top-color: var(--accent);
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
  border-radius: 50%;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  color: var(--accent-strong);
  font-weight: 600;
  font-family: "SF Mono", "JetBrains Mono", Menlo, Consolas, monospace;
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
    z-index: 10;
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
    min-height: 44px;
    font-weight: 600;
    color: var(--fg);
  }

  .toc-mobile > summary::-webkit-details-marker {
    display: none;
  }

  .toc-mobile > summary::after {
    content: "▾";
    color: var(--fg-faint);
    transition: transform 0.2s;
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
    min-height: 44px;
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
