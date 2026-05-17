# Effects, keywords, and special rules

Four kinds of card effects exist: **actions**, **forced effects**,
**constant effects**, and **keywords**. Plus several global rules
(corruption, sacrifice, "cannot," control vs. ownership, corresponding
zones).

For the algorithmic timing structure that surrounds all of the
following, see [timing.md](./timing.md).

## In play vs. out of play

| Zone | In play? |
|---|---|
| Kingdom | in play |
| Quest zone | in play |
| Battlefield | in play |
| Deck | out of play |
| Hand | out of play |
| Discard pile | out of play |

Unless a card effect specifies otherwise (e.g. destroy, sacrifice),
cards can only **move between in-play zones**, never out into hand /
deck / discard via a "move" effect.

A card that **leaves play** has just moved from an in-play zone to an
out-of-play zone. Cards generally cannot trigger their own abilities
once they have left play — *except* for self-referential effects (see
"Self-referential effects" in [timing.md](./timing.md)).

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
deals N **uncancellable** damage to a single attacking unit or legend of
the defender's choice. Cannot be split. The unit still also fights normally in
step 4/5. Multiple Counterstrike sources stack numerically. See
[combat.md](./combat.md) for placement in the combat sequence.

Counterstrike's trigger condition is *being declared a defender* — it
exists independently of the source. If the counterstriking unit is
later destroyed in the same combat, its Counterstrike damage has
already been dealt.

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
from hand per surviving Scout. Scout triggers **even if no damage was
dealt during the combat**, provided the unit participated and survived.

### Ambush

A keyword found only on developments. Ambush may **only be triggered
on a facedown development**. If an effect has flipped the development
faceup, its Ambush ability cannot fire.

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

### Limit one Hero per zone

A keyword-style restriction that appears on most Hero units. While a
player controls a Hero in a given zone:

- That player cannot play, take control of, move, or put into play
  (via a card effect) another Hero into that same zone.
- The opponent also cannot play, give control of, move, or put into
  play (via a card effect) another Hero into that same zone.

The restriction is *per zone*, not per capital — a player may control
one Hero in kingdom and a different Hero in battlefield.

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

## "Then"

When a card effect uses the word **then**, the *preceding* effect must
have resolved successfully before the effect following "then" can
resolve.

> Example: `Forced: When this unit enters play, search the top five
> cards of your deck for a support card with cost 2 or lower. You may
> put that card into this zone. Then, shuffle your deck.` — if the
> search effect is somehow prevented from completing (cancellation,
> etc.), the shuffle does not happen.

## "Or"

Some effects offer two outcomes joined by **or** — the player choosing
must pick an option they can *fully* resolve. If only one of the two
options is resolvable, the player must choose that one.

> Example: Warpstone Meteor — `each player must corrupt one of his
> units in this corresponding zone or deal 1 damage to his capital.`
> If a player has no unit to corrupt, they must take the damage; if
> they have no capital to take the damage (all sections burned), they
> must corrupt.

## The letter "X"

Unless a card, card effect, or granted player choice defines it, **X is
always 0**. This applies both to cost values and to effect values.

> Example: copying a tactic with `cost X` via Twin Tailed Comet
> resolves with `X = 0`.

## Trigger condition (v1.4)

A "trigger condition" is anything that must happen before a card
ability can fire. This includes playing a card, using an ability, or
even a constant effect's condition becoming true. The full algorithm
for resolving triggers (active-player-first ordering, the constant /
forced split, etc.) is in [timing.md](./timing.md).

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
   (action window — cancellable / redirectable here)
3. Damage applied     ←  commit to targets
   - Toughness cancels (unless damage is uncancellable)
   - Surviving units stay; units at/over HP are destroyed
   - Capital sections at/over remaining HP burn
4. Post-damage effects (Scout, "after damage applied" triggers)
```

Counterstrike damage skips step 2's action window and is applied at the
moment of defender declaration, uncancellable.

Non-combat damage also skips the action window — it is applied as soon
as it is assigned.

See [damage.md](./damage.md) for the full damage rules: assigned vs.
dealt, indirect damage, redirection, moved damage, healing.

## Running out of cards

If at any time a player has **zero cards left in their deck**, they are
**immediately eliminated**. Not "at the end of the phase" — at the
moment. Check on every effect that draws or mills cards.
