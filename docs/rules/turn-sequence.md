# Turn sequence

Each turn, one player is **active**. They complete all five phases in
order before the opponent's turn starts. Phase 0 was added explicitly by
FAQ 2.2 — it is the home of every "at the beginning of your turn"
effect.

```
Beginning of Turn  →  Kingdom  →  Quest  →  Capital  →  Battlefield  →  End of Turn
        (Phase 0)        (Phase 1)   (Phase 2)  (Phase 3)    (Phase 4)        (Phase 5)
```

> **First player penalty.** On the **very first** turn of the game, the
> starting player skips Quest and Battlefield entirely. This applies only
> to turn 1, only to player 1. (Phase 0 and the Kingdom and Capital
> phases still happen.)

Every phase has one or more **action windows** between steps. In each
window both players may take actions; the engine only advances after both
players have **passed consecutively** without taking an action.

For combat detail, see [combat.md](./combat.md). For what counts as an
action / forced effect / keyword, see [effects.md](./effects.md). For the
full per-trigger / per-window timing algorithm, see [timing.md](./timing.md).

## 0. Beginning of the turn (Phase 0)

1. Resolve every "at the beginning of the turn" triggered Constant /
   Forced effect (in active-player-first order — see
   [timing.md](./timing.md) §A).
2. **Action window.**

This is a new explicit phase introduced by FAQ 2.2. Older card text that
reads "After your turn begins…" should be treated as "At the beginning of
your turn…" and fire here.

## 1. Kingdom phase

1. Active player returns **all unspent resources** to the central pool.
2. Active player **may restore one** corrupt card they control (see
   [effects.md](./effects.md) — Corruption).
3. Active player counts total power in their kingdom zone (base 3 + power
   icons on cards there) and takes that many resources from the pool.
4. **Action window.**

## 2. Quest phase

1. Active player counts total power in their quest zone (base 1 + power
   icons on cards there) and draws that many cards from the top of their
   deck.
2. **Action window.**

> Drawing from an empty deck does not auto-fail mid-phase — but the
> standing "running out of cards" rule eliminates a player who has zero
> cards in deck. Implementation-wise: check elimination after each draw.

## 3. Capital phase

The **only** phase in which the active player may play unit, support, and
quest cards from their hand into their zones.

In any order, the active player may:

- Play a unit/support/quest card from hand by paying its total cost (see
  [cards.md](./cards.md)). On play, the controller chooses which of their
  three zones it enters (subject to any zone-restriction keyword on the
  card).
- Play a **legend** from hand directly onto their capital board, paying
  its total cost. Restricted: cannot be played in response to another
  action, cannot be played outside the Capital phase, and the controller
  may have only one legend in play at a time. See
  [cards.md](./cards.md) — Legends.
- Play **one** card face-down as a **development** (per turn).
- Trigger any abilities they have available.

The non-active player may take actions during this phase too — they just
cannot play units / supports / quests / developments from hand. They can
still play tactic cards and trigger abilities.

The phase ends when both players **consecutively** pass.

> Playing a unit/support/quest or playing a development counts as the
> active player **taking an action** for action-window purposes.

## 4. Battlefield phase

Active player may attack **one** of the opponent's zones with any subset of
units in their battlefield. The defending player may then defend with any
subset of units in the attacked zone.

The phase is a 5-step procedure, each step followed by an action window:

1. **Declare target of attack** (choose opponent's kingdom / quest /
   battlefield).
2. **Declare attackers** (subset of attacker's battlefield).
3. **Declare defenders** (subset of defender's units in the attacked zone).
4. **Assign damage** (placed near targets, not yet committed).
5. **Apply damage** (committed; Toughness etc. trigger here; burn tokens
   placed if HP crosses zero).

After step 5's action window, the battlefield phase ends. See
[combat.md](./combat.md) for the gory detail.

## 5. End of turn

1. **Action window.**
2. Resolve every "at the end of the turn" triggered Constant / Forced
   effect (in active-player-first order — see [timing.md](./timing.md) §E).
3. Constant effects that last "until the end of the turn" expire.

After end-of-turn resolution finishes, the opponent becomes active and
begins their Phase 0 (Beginning of the Turn).

Note: some Forced effects say "after your turn ends" — these fire at the
turn boundary and can interact with the now-incoming player's Phase 0.
Implementations should expose a clean `EndOfTurn` / `BeginTurn` boundary
so cards can hook it.

## Action-window cheat sheet

| After… | Action window? |
|---|---|
| Phase 0: beginning-of-turn triggers resolved | ✓ |
| Kingdom: resources collected | ✓ |
| Quest: cards drawn | ✓ |
| Capital: any single play | (implicit; phase ends only after both pass) |
| Combat: target declared | ✓ |
| Combat: attackers declared | ✓ |
| Combat: defenders declared | ✓ |
| Combat: damage assigned | ✓ |
| Combat: damage applied | ✓ |
| Phase 5: end-of-turn (before end triggers) | ✓ |

Each ✓ means: pause, allow either player to take actions, advance only when
both pass consecutively. Combat sub-step windows are only emitted if an
attack is actually declared — passing the initial Battlefield window
ends the phase outright.

## The word "skip"

When a card instructs you to **skip** a phase, that phase is bypassed
based on the timing the card specifies. A single phase can be skipped
more than once — repeated "skip" effects from different cards in the
same phase are not redundant; each one shuts the phase down once.

(Example: a card that says "skip your battlefield phase to destroy
this card" still shuts down the phase even if another card has already
made the same player skip it.)
