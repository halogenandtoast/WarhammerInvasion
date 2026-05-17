# Detailed timing structure

The rulebook gives a high-level turn diagram; the FAQ (v2.2) introduces a
formal **timing structure** that the engine must follow when resolving
triggers, action chains, and constant/forced effects. This document
mirrors that structure so engine code has a single canonical reference.

The four top-level routines are:

```
A. After a Trigger Condition
B. Action Window
C. Resolving a Constant/Forced Effect
D. Beginning of a Phase/Turn
E. End of a Phase/Turn
```

Each routine may recursively re-enter another routine (e.g. paying for a
forced effect may itself trigger constant effects, which kicks back into
A). The engine must therefore model these as a stack, not a flat
sequence.

## A. After a trigger condition

A "trigger condition" is **anything that must happen before a card
ability can fire** — playing a card, using an ability, even a constant
effect changing state. Whenever one resolves, immediately:

1. **Resolve triggered Constant Effects.** All triggered constant effects
   exist independently of their source.
   - The active player resolves *all* of his triggered constant effects
     in any order he chooses (each one drops into routine **C**).
   - The next player then resolves *all* of his.
2. **Resolve Forced Effects.** Same active-player-first ordering, each
   one dropping into **C**.

Only after both substeps are clear does the engine proceed to whatever
window or step comes next.

## B. Action window

An action window is the explicit pause where either player may take
optional actions. It runs as:

1. **Place triggered actions on a chain.** Each player, starting with
   the first player (the active player in normal play), adds any
   triggered actions whose trigger condition has been met since the
   *last* action window. Continue alternating until both players have no
   more triggered actions to add.
   - After this initial pass, players may only add a triggered action
     if its trigger condition met *since the window opened* — and they
     must add it at the first opportunity once the window opens, or
     they lose the chance entirely.
2. **Optional actions.** Players take turns playing actions (starting
   with the player who holds priority — by default the active player).
   Each new action either starts a new chain or adds to the existing
   chain if there are queued triggered actions. After each play,
   priority passes to the other player. The window remains open while
   either player still wants to act. When both players pass
   consecutively, the chain (or empty chain) is resolved.
3. **Resolve the action chain.**
   - Resolves in reverse order — last in, first out.
   - Each effect exists independently of its source (see §"In play /
     out of play" in [effects.md](./effects.md)).
   - Targets and conditional requirements are re-checked at resolution
     time. If targets are now illegal or a conditional fails, the
     effect is cancelled. (See "Illegal target" in
     [effects.md](./effects.md).)
   - If an ability triggers a Constant or Forced effect during
     resolution, pause the chain and drop into routine **A**.

When the chain finishes and both players have passed again, the window
**closes** and the engine advances to the next phase step.

## C. Resolving a Constant/Forced effect

Used whenever a forced effect fires or a constant effect's trigger
condition becomes true. The effect resolves through this three-step
procedure:

1. **Choose target(s).** If no legal target exists, *cancel* the effect.
2. **Pay costs.** Some constant/forced effects have built-in costs. If
   the cost itself triggers more Constant/Forced effects, drop into
   routine **A**. If the cost cannot be paid, cancel the effect.
3. **Apply the effect.** Same recursion rule — if applying the effect
   triggers more Constant/Forced effects, drop into **A**.

## D. Beginning of a phase / turn

Whenever a new phase or turn begins:

1. Resolve every "at the beginning of the turn/phase" triggered
   Constant/Forced effect (routine **A**).
2. Open an **action window** (routine **B**) before the phase's printed
   steps run.

## E. End of a phase / turn

Mirror image of D:

1. Open an **action window** (routine **B**).
2. Resolve every "at the end of the turn/phase" triggered
   Constant/Forced effect (routine **A**).
3. Constant effects that last "until the end of the phase/turn" expire.

## Updated turn-sequence diagram (FAQ 2.2)

The FAQ adds an explicit **Phase 0 — Beginning of the Turn** before the
Kingdom phase. The full diagram is:

```
Phase 0   Beginning of the Turn   →  action window
Phase 1   Kingdom phase
  - return resources, restore one corrupt card, collect resources
  - action window
Phase 2   Quest phase
  - draw cards
  - action window
Phase 3   Capital phase
  - play units/supports/quests, one development, take actions
Phase 4   Battlefield phase   (see combat.md for the 5 sub-steps)
Phase 5   End of the Turn   →  action window, "end of turn" triggers
```

All "at the beginning of the turn" effects now fire in **Phase 0** —
before Kingdom-phase resources are reset or collected. This is a change
from older versions of the rulebook that put them inside the Kingdom
phase itself.

## Simultaneous effects (v1.6)

When two non-Action card effects trigger at the same time, the player
whose turn it currently is applies his effects in any order he chooses,
then the opponent applies his.

- Forced and Constant effects always resolve in a fixed order, so two
  effects are only "simultaneous" if they are the same type.
- Keywords and conditional actions count as Constant effects for the
  purposes of timing.
- Two or more Constant Effects *without* a trigger condition are not
  considered simultaneous — players apply their net effect.

## Self-referential effects (v2.0)

A self-referential effect uses the language "this unit" / "this card."
A card whose printed text triggers when it leaves play (e.g. Blue
Horrors, Crypt Ghouls) can still fire its ability *after* it has left
play, because the effect references itself.

Cards that **don't** use self-referential text (e.g. Thief of Essence,
Dwarf Ranger) cannot trigger their abilities once they have left play —
they need a triggering condition on the board.

A self-referential ability must be triggered at the *first opportunity*
after the card moves to the discard pile / hand / deck, or the chance
is lost.

## "Just played" (v1.8)

A card is "just played" while its play resolution is still on the
unresolved active action chain. Cards that target *just-played* cards
(e.g. High Elf's Disdain) can be played at any time during that chain
and may target any card on the chain. Cards that *cancel* a just-played
card can only respond to the matching card type.

## Card effects that result in playing cards (v1.8)

When an effect plays a card during the resolution of an action chain,
no new chain is started — the played card resolves as if it were part
of the original chain. You cannot respond to it.

## Triggered actions (formerly "response actions", v1.4)

A triggered action is an Action with a trigger condition. Each *copy*
of a triggered action may fire **at most once per trigger condition
this turn**. If the trigger condition is met during the resolution of
another effect (or outside an action window), the triggered action
**must** be added to the chain at the first opportunity once an action
window opens, or it cannot fire at all.

## Conditional actions (v1.1)

Some actions create a constant effect that waits for a specific
condition to be met before resolving. These are *conditional actions*
and last until the end of the turn or until their condition is met,
whichever is first.

Example: Blessing of Valaya — `Action: The next 2 damage dealt to one
target unit are redirected to another target unit.` Played → creates a
waiting constant effect → fires on the next 2 damage assigned.

## Trigger condition (v1.4)

A trigger condition is *anything* that must happen before a card ability
can fire. Common ones include:

- Playing a card.
- Using another ability.
- A constant effect changing state.
- A card leaving / entering play.
- The beginning / end of a turn or phase.

## Triggered Constant Effects vs. Conditional Actions

When the engine has to order multiple things at the same trigger:

| Type | Order |
|---|---|
| Triggered Constant Effect | Active player first, then opponent |
| Forced Effect | Active player first, then opponent |
| Constant Effect *without* a trigger | Net effect, no ordering |
| Conditional Action waiting on a trigger | Counts as a Constant Effect |
| Keyword | Counts as a Constant Effect |

## Implementation checklist

- Model routines A/B/C/D/E as stack frames the engine can re-enter.
- Each action window pushes a frame onto a stack; CloseActionWindow
  pops it.
- "Beginning of the turn" effects must fire in a Phase-0 routine A pass
  before any Kingdom-phase work runs.
- A "trigger" should be modelled as a queue of pending fires, drained
  in active-player-first order each time it's processed.
- Triggered actions need a per-copy / per-trigger-condition counter for
  the "once per copy per trigger this turn" gate.
- Targets must be validated *both* on play (queueing time) and on
  resolution (when the effect actually fires). If invalid at
  resolution, cancel — see `Illegal target` in
  [effects.md](./effects.md).
