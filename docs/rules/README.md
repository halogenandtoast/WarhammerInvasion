# Warhammer: Invasion LCG — rules reference

Compiled from the Core Set rulebook for the use of future agents (and humans)
working on this codebase. This is a *condensed* and *implementation-oriented*
reference — not a substitute for the rulebook, but tight enough to grep, and
written with the engine in mind.

## Files

- [game-structure.md](./game-structure.md) — components, races, zones, win
  condition, deckbuilding constraints.
- [cards.md](./cards.md) — card types, anatomy, costs (printed + loyalty),
  uniqueness, developments.
- [turn-sequence.md](./turn-sequence.md) — the four phases, action windows,
  first-player penalty.
- [combat.md](./combat.md) — battlefield phase in detail: the 5 combat steps.
- [effects.md](./effects.md) — actions, forced effects, constant effects,
  keywords, corruption, sacrifice, control vs. ownership.
- [glossary.md](./glossary.md) — alphabetical lookup of terms.

## The Golden Rule

> If the rules text of a card contradicts the text of the rulebook, the
> rules on the card take precedence.

Engines must therefore allow per-card behavior to override the general
sequence and effects. Build the base rules as the *default* path, with cards
able to interpose.

## The minimum you need to know

- Two players. Each builds a 50–100 card deck (no more than 3 copies of any
  card by title). Three zones each: **kingdom**, **quest zone**,
  **battlefield**. Each zone starts at 8 HP.
- A turn has four phases in order: **Kingdom → Quest → Capital → Battlefield**.
- The first player skips Quest and Battlefield on their first turn.
- Win condition: burn **two of the three** sections of the opponent's capital
  (sections burn when total damage applied to them ≥ remaining HP).
- Decking out (running out of cards in deck) eliminates the player
  immediately.
- All meaningful timing happens in **action windows** between phase steps:
  both players consecutively pass before the engine advances.

## When implementing

- Engine state changes always happen in response to typed events
  (`Message`). When the rulebook says "X then Y," your engine wants
  *enqueue Y after X*, not *inline both in one handler*. This keeps cards
  able to interpose via Forced and Action triggers.
- Action windows are *not* free-form — they are explicit points in the
  sequence where both players may take actions. Model them as states the
  engine pauses in.
- Damage has two stages: **assigned** (placed near a target, cancellable)
  vs **applied** (committed to the target, where Toughness etc. trigger).
  Do not collapse these.
- Cancelled damage tokens return to the pool, they do not disappear into
  the void — preserve the token-pool model if you care about token economy.
