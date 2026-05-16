# Cards

## Card types

| Type | Stays in play? | Notes |
|---|---|---|
| **Unit** | Yes | Heroes, soldiers, monsters. Attack, defend, quest, produce resources. |
| **Support** | Yes | Buildings, weapons, attachments. Once played, persists until destroyed. |
| **Tactic** | No | Single-shot. Played from hand, effect resolves, goes to discard. |
| **Quest** | Yes | Long-term pursuits. Played into quest zone; units may be sent on it. |
| **Legend** | Yes | Hero-class card. Played directly onto the capital board (not into a zone). Contributes power to all three zones. See [Legends](#legends). |
| **Draft format** | n/a | Only used during the [Draft variant](#draft-variant); never goes in a normal deck. |

**Attachments**: a support card attached to another card is **sacrificed** if
the host card leaves play.

## Anatomy

Card slots referenced in the rulebook:

1. **Title** — name. A `*` (banner) before the name marks the card as **unique** (see below).
2. **Cost** — printed resource cost in upper left.
3. **Race symbol** — which race the card belongs to.
4. **Loyalty icons** — additional resource cost, reducible by matching race symbols you control (see Loyalty below).
5. **Power icons** — what the card produces in its zone (resources / draw / damage, depending on zone).
6. **Card type** — banner indicating unit/support/tactic/quest.
7. **Hit points** — units only; damage ≥ HP destroys it.
8. **Traits** — flavor tags (Warrior, Hero, Spell, Building, …). No inherent rules; other cards may reference them.
9. **Card text** — effects unique to this card.
10. **Collector info** — set symbol and number.

## Cost = printed cost + loyalty cost

```
total_cost = printed_cost + max(0, loyalty_icons_on_card - matching_race_symbols_in_play)
```

Each capital board provides **one** matching race symbol for its faction.
Additional matching race symbols come from cards in play. The clamp at zero
matters: loyalty cost cannot be negative — you cannot make a card cheaper
than its printed cost via loyalty.

Example (from the rulebook): Kris plays **Thyrus Gorman** (printed cost 3,
3 Empire loyalty icons). Kris has 2 Empire race symbols in play. Loyalty
cost = 3 − 2 = 1. Total cost = 3 + 1 = **4 resources**.

## Power icons

A unit's / support's power icons mean different things depending on the
zone it sits in:

- **Kingdom** → resources collected during Kingdom phase.
- **Quest zone** → cards drawn during Quest phase.
- **Battlefield** → damage inflicted in combat.

(Some card effects override or augment this. See [effects.md](./effects.md).)

## Unique cards

Marked with a banner (`*`) before the title. A player **cannot** play, take
control of, or put into play a unique card while they already control a copy
of it. Both players *can* simultaneously have the same unique card. A unique
card in your discard pile does not block you from playing another copy — the
restriction applies only when you already have one in play.

## Developments

During the Capital phase, the active player may play **one** card from hand
**face-down** as a development in any one of their three zones.

- Each development adds **+1 HP** to its zone.
- Developments are face-down; the player may inspect their own developments
  at any time, but not the opponent's.
- One development per turn maximum.
- Some card effects scale with the number of developments in a zone.

## Sending a unit on a quest

When a player plays a unit during the Capital phase, they may play it
**on top of** a quest card already in their quest zone. The unit:

- Is still in the quest zone (contributes power and can defend).
- Is additionally "questing on" that quest.
- Lets resource tokens accumulate on the quest while it's there.

Only **one unit** can quest on a given quest card at a time. If the
questing unit leaves play, the quest stays but accumulated tokens are lost
to the pool. Tokens on a quest can only be spent on that quest's effect.

## Legends

Legends are their own card type. They are shuffled into a player's deck
like any other card, but when played they go **directly onto the capital
board**, not into a zone.

### Playing a legend

- Playing a legend counts as the controller **taking an action**, with
  two restrictions:
  - It must be played during the **owner's Capital phase**.
  - It **cannot be played in response** to another action.
- The legend's **loyalty icons must match** a loyalty icon on the
  controller's capital board. A capital without the matching loyalty
  cannot play that legend at all.
- Each player may control **at most one legend in play at a time**.
  While a legend is in play, its controller cannot play another legend
  until the one in play leaves play.

### What legends are (and aren't)

Legends are **not** units, supports, or tactics. Card effects that target
those types **do not** affect a legend (e.g. a unit-targeting removal
tactic can never remove a legend). Legends can still be **attacked in
combat** — see [combat.md](./combat.md).

### Anatomy

A legend has a cost, loyalty icons, hit points, and power. Its power is
**split across the three zones** (kingdom / quest / battlefield) —
the per-zone breakdown is printed on the card.

- **Kingdom phase** — the legend contributes its kingdom-zone power as
  resources, as if it were located in the kingdom zone.
- **Quest phase** — the legend contributes its quest-zone power as card
  draws, as if it were located in the quest zone.
- **Battlefield phase** — the legend may use its battlefield-zone power
  to **attack and defend as though it were a unit in the battlefield**.
  It can deal and receive combat damage like a unit (but still cannot be
  targeted by unit-targeting card effects).

### Damage and destruction

If a legend has applied damage **equal to or greater than its hit
points**, it is **destroyed** — same threshold as a unit.

## Draft variant

A separate play mode where players draft 40-ish-card decks from random
piles. Order vs. Destruction is fixed at draft time. Four special draft
format cards exist (Cut Supply Lines, Reinforcements, Sabotage, Shifting
Tides) that affect the drafting process itself and never enter a deck.

Not on the implementation roadmap; see `ARCHITECTURE.md` §9.
