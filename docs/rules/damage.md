# Damage

Damage in *Warhammer: Invasion* is **two-stage**: it is *assigned* to a
target, then later *applied* to it. Most cancellation effects (Toughness,
Master Rune of Valaya, etc.) sit between these two stages. A handful of
sources bypass the split entirely (Counterstrike, non-combat damage).

```
1. Inflict   (compute total power / effect value)
2. Assign    (place tokens near targets; can be redirected or cancelled)
3. Apply     (move tokens onto targets; Toughness fires; HP check; burn check)
4. Post      ("after damage dealt" / Scout / "after damage applied" hooks)
```

This file covers the rules that touch each stage. For the surrounding
combat ladder, see [combat.md](./combat.md). For action-chain semantics,
see [timing.md](./timing.md).

## Dealing damage in combat

A unit is considered to have **dealt damage in combat** as long as at
least one damage from the combat-damage pool it contributed to is
actually *applied* to an opponent's unit or capital during the Apply
Damage step. The unit does not need to have personally inflicted that
specific token — the pool is shared.

> Example: Two units with the "Forced: After this unit deals damage in
> combat" trigger are in a single combat. If even one combined
> damage reaches the enemy, both units qualify.

## Dealt damage (v1.1)

A unit has been **dealt damage** if at least one damage is *applied*
to it after damage cancellation effects occur during the Apply Damage
step. Effects that "cancel" damage prevent the unit from ever having
been dealt damage, even if tokens were assigned to it.

## Non-combat damage

Card effects that deal damage outside of combat use the same
assign-then-apply pattern *except*: non-combat damage is **always
applied as soon as it is assigned**, before any other actions can be
taken. This means there is no action window to cancel non-combat
damage after the assign step — interrupt cards (e.g. Steel's Bane)
must be played in response to the *source* effect.

> Example: Nurgle's Pestilence reads `Action: Each unit in play takes
> 1 damage…`. To cancel that damage, you must respond to the play of
> Nurgle's Pestilence itself; you cannot wait for the damage tokens
> to be placed and then cancel them.

## Indirect damage (v1.5)

Indirect damage is **non-combat damage dealt to a target player**, who
then assigns it to their own units, legends, and/or capital.

- Indirect damage **cannot** be assigned to a burning zone.
- A player cannot assign **more** indirect damage to a unit or legend
  than would destroy it (Toughness and other cancellation effects
  count toward this maximum).
- A player cannot assign more indirect damage to a capital section
  than would burn it.
- If multiple players must assign indirect damage at the same time,
  the active player chooses where to assign their damage first, then
  the next player clockwise. The damage is still applied at the same
  time.

## Cancelling damage (v2.0)

Some effects (Toughness, Steel's Bane, …) *cancel* damage. Cancellation
happens **after** the damage is assigned but **right before** it is
applied. Cancelled tokens return to the central pool — they do not
"vanish."

Uncancellable damage (Counterstrike, anything tagged "uncancellable")
ignores cancellation entirely.

## Redirecting damage (v1.2)

Some effects redirect damage from one target to another. Redirection
happens **after** the damage is assigned but **right before** it is
applied — same window as cancellation, but with a different mechanic.

- Redirection is **not** cancellation; effects that look at "next 2
  damage that would be dealt" are not stacking-cumulative, since they
  redirect the *next* damage and then expire.
- A player **does not** have to take redirection effects into account
  when assigning damage to defenders in combat — they assign as if
  the redirection weren't there. (Redirecting damage is not
  cancellation.)

## Moving damage

Effects that move damage from one unit to another (Stubborn Refusal,
Orc Shaman, …) are **not** considered to deal or assign damage. Moved
damage **bypasses cancellation** — Toughness does not apply.

- You **cannot** move more damage to a unit than it takes to destroy
  that unit. (Orc Shaman can move 5 damage, but not onto a unit with
  only 1 remaining HP — only 1 damage moves.)

## Counterstrike

Counterstrike is the **only** combat damage that bypasses the
assigned-vs-applied split. When a unit with Counterstrike N is
**declared as a defender** (step 3), it immediately deals N
uncancellable damage to a single attacking unit of the defender's
choice — *before* any other actions can be taken.

- Cannot be split across multiple attackers.
- Cannot be cancelled by Toughness or anything else.
- Does **not** prevent the counterstriking unit from dealing its
  normal combat damage at step 4/5.
- Multiple sources of Counterstrike on one unit stack numerically.

## Toughness

Toughness N is a defender-and-attacker keyword that cancels N points
of *cancellable* damage assigned to the unit before it is applied.

- Multiple Toughness sources stack additively.
- Damage tagged "uncancellable" is unaffected.
- Counterstrike damage is applied *before* the cancel step, so
  Toughness never applies to Counterstrike.
- Cancelled tokens return to the pool.

## Healing (v1.1)

Healing is the game term for *removing damage tokens from a unit*. To
heal a unit, that unit must have damage on it.

## Hit points vs. remaining hit points (v1.9)

- **Hit points** = the unit's printed HP value, modified by any
  current effects on the unit.
- **Remaining hit points** = the unit's hit points minus the damage
  currently on the unit.

A capital section's hit points are `8 + (developments in that zone)`;
its remaining hit points are that figure minus the damage on the
section.

## Reduction

Any effect that would reduce a number (cost, hit points, damage, …)
**cannot reduce it below zero**.

## Cost (v1.4)

Any reference to a card's "cost" always means the printed number in
the top-left corner. A card with a cost of 0 cannot be reduced.

- Loyalty is a *variable additive* to total play cost, but it is not
  part of the card's "cost" for the purposes of card effects.
- A card played "for no cost" has its cost considered to be 0; printed
  costs in the card text (e.g. action costs) still apply.

## Card-text references to damage

- "Damage dealt": damage that has been *applied* to a target after
  cancellation.
- "Damage assigned": damage that has been placed near a target but
  not yet applied.
- "Damage that would be dealt": damage in flight, before assignment
  is final. Used by redirect / cancel cards (FAQ-normalised wording).

## Implementation notes

- Model damage as a pair of stages — `DealDamageToX` and a separate
  pending-assignment buffer. The engine should never collapse them
  into a single mutation.
- Counterstrike runs synchronously in step 3 — don't queue it through
  the same path as combat damage.
- Non-combat damage and Counterstrike are the two paths that **skip
  the action window** between assign and apply. Everything else gets
  the window.
- Moved damage bypasses Toughness — wire it as a direct token shift,
  not a fresh DealDamage call.
