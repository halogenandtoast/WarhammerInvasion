# Effects, keywords, and special rules

Four kinds of card effects exist: **actions**, **forced effects**,
**constant effects**, and **keywords**. Plus several global rules
(corruption, sacrifice, "cannot," control vs. ownership, corresponding
zones).

## Actions

Marked `Action:` (bold) on a card. Always optional. Triggerable by either
player during any **action window** in the turn sequence (see
[turn-sequence.md](./turn-sequence.md)).

- For an action on a unit/support/quest, the card must be **in play**
  unless the action's text explicitly allows triggering from out of play
  (e.g. from hand or discard).
- **Tactic cards** are themselves actions: played from hand into the
  discard pile.

### Playing an action "in response"

Actions can be triggered **in response** to another action. The response
resolves *first* — last-in, first-out, like a stack.

- All costs are paid and all targets are chosen at the time the action is
  triggered, regardless of when it resolves.
- An action that has entered the chain still resolves even if the source
  card leaves play.
- To avoid an effect, either remove its target before resolution or cancel
  the effect with another action.

## Forced effects

Marked `Forced:` (bold) on a card. Triggered by specific game events.

- **Automatic** — they fire when their trigger is met, whether the
  controller wants them to or not.
- **Cannot be cancelled** or interrupted by other actions.
- Resolve immediately at the trigger point.

Example: `Forced: After your turn ends, this unit takes 1 damage.` — the
controller has no choice; it fires at end of their turn.

## Constant effects

Card text **without** a bold trigger. Continuously affects game state
while the card is in play and any stated conditions hold.

Example: `This unit gains ** while you control at least 1 damaged unit.`
While the host card is in play and any damaged unit is controlled, +2
power applies. The moment the condition fails, the bonus disappears.

## Keywords

Shorthand for common rule effects.

### Counterstrike N

Defender-only. When the unit is **declared as a defender** it immediately
deals N **uncancellable** damage to a single attacking unit of the
defender's choice. Cannot be split. The unit still also fights normally in
step 4/5. Multiple Counterstrike sources stack numerically. See
[combat.md](./combat.md) for placement in the combat sequence.

### Toughness N

When the unit is **assigned** damage, N of that damage is cancelled
**before** application. Cancelled tokens return to the pool.

- Multiple Toughness sources stack additively.
- Damage tagged "uncancellable" bypasses Toughness.
- Counterstrike's pre-step-5 timing means Toughness does not apply to
  Counterstrike damage.

### Scout

After combat damage is **applied**, the controller of any surviving
participating Scout unit forces the opponent to discard one random card
from hand per surviving Scout.

### Kingdom / Quest / Battlefield only

A zone-restriction on entering play. The card can only be played (or put
into play) into the named zone. Once in play, card effects may move it to
another zone — the restriction is on entry only.

### Zone-specific effects (italicized zone prefix)

`Kingdom.` / `Quest.` / `Battlefield.` prefix on an effect = the effect is
**active only while the card is in the named zone**. If the card is
elsewhere, the effect is dormant.

Example: `Quest. Action: Spend 2 resources to have a target unit gain *
until the end of the turn.` Works only when the host is in the quest zone.

### Limited

A player may play at most **one** card with the Limited keyword each
turn.

### Order Only / Destruction Only

Restricts a (typically neutral) card to one faction. `Order Only` cards
cannot go into Destruction decks; `Destruction Only` cannot go into Order
decks.

## Corruption

Some effects **corrupt** a card. Mechanically: turn the card 90 degrees.

- **Corrupt cards cannot be declared as attackers or defenders.**
- Most other interactions still work normally — they're stuck on garrison
  duty in their zone.
- At the start of their **kingdom phase**, before any actions, the active
  player **may restore one** of their own corrupt cards. To restore: turn
  it back upright. Only one restoration per kingdom phase.

Effect-design note: some cards corrupt *themselves* as a cost; others
corrupt opponent's units as harassment. Implementations should treat
corruption as a flag on the entity, not a stateful animation.

## Sacrifice

When a player is instructed to sacrifice a card, that card goes to its
**owner's** discard pile. Sacrifice is **absolute**: it cannot be
cancelled by other effects.

Attachments (typically supports) attached to another card are sacrificed
automatically when the host leaves play.

## "Cannot" is absolute

If an effect says `cannot`, it overrides any conflicting effect. The
rulebook gives this example: `Attached unit cannot be corrupted` beats
`Action: Corrupt one target unit`.

Implementation: when resolving an effect, evaluate `cannot`-style
prohibitions last (or first, before changes apply) and short-circuit the
effect if any apply.

## Control vs. ownership

- **Owner**: who put the card in their deck.
- **Controller**: whoever currently controls the card. Defaults to owner;
  can change if another card steals control.
- When a card **leaves play**, it reverts to its **owner**'s hand, deck,
  or discard pile (as the effect dictates).

## Corresponding zones

If a card text refers to "the corresponding zone" of another player, it
means **the same-named zone** as the card's current location.

Example: a Battlefield unit's effect that targets "the opponent's
corresponding zone" targets the opponent's **battlefield**.

If the text says "each player's corresponding zone," that includes the
card's controller's zone *and* each opponent's zone of the same name.

## Damage cancellation pipeline (assembled)

Putting the above together, when damage is dealt:

```
1. Damage inflicted (sum of power icons, or value of effect)
2. Damage assigned    ←  attacker/defender choose distribution
   (action window — cancellable here)
3. Damage applied     ←  commit to targets
   - Toughness cancels (unless damage is uncancellable)
   - Surviving units stay; units at/over HP are destroyed
   - Capital sections at/over remaining HP burn
4. Post-damage effects (Scout, "after damage applied" triggers)
```

Counterstrike damage skips steps 2's action window and is applied at the
moment of defender declaration, uncancellable.

## Running out of cards

If at any time a player has **zero cards left in their deck**, they are
**immediately eliminated**. Not "at the end of the phase" — at the
moment. Check on every effect that draws or mills cards.
