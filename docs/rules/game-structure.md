# Game structure

## Players and goal

Two players. The first to **burn two of the three sections of the opponent's
capital** wins. A player is also eliminated if they ever have **no cards
left in their deck**.

## Zones

Each player controls a **capital** with three zones, each starting at 8 hit
points:

| Zone | Role |
|---|---|
| **Kingdom** | Generates resources. Base power 3. |
| **Quest zone** | Draws cards. Base power 1. Quests are played here; units can be sent on a quest. |
| **Battlefield** | Attacks the opponent. Base power 0. Only battlefield units can declare attacks. |

A zone's **power** is the sum of:
- The base power printed on the capital board for that zone
- The power icons on units and supports in that zone
- Any temporary modifiers from card effects

A zone's **hit points** are 8 base + 1 per development in that zone.

When applied damage to a zone ≥ its remaining HP, the zone **burns**: all
damage tokens on it are removed, a burn token is placed, and that section is
overrun. Two burns on the same opponent ends the game.

## Races

Six races, split into two factions:

- **Order**: Empire, Dwarf, High Elf
- **Destruction**: Chaos, Orc, Dark Elf

Order and Destruction **cannot be combined** in the same deck. Races within
the same faction *can* be combined, at the cost of harder loyalty
management (see [cards.md](./cards.md)).

**Neutral** cards (grey border, no race symbol) can be used in either side,
unless the card text says otherwise (see `Order Only` / `Destruction Only`
keywords in [effects.md](./effects.md)).

## Components (physical game)

- 220 cards (4 prebuilt 40-card starters: Dwarf, Empire, Orc, Chaos; plus
  neutrals, alliances, draft format cards, and a small set of High Elf /
  Dark Elf cards).
- 4 capital boards.
- 35 resource tokens, 60 damage tokens, 4 burn tokens.

In a digital implementation, only the token *counts* and *placement* matter
— the engine state is sufficient.

## Setup

1. Each player shuffles their deck (opponent may cut/shuffle for fairness).
2. Each player places their capital board.
3. Pool damage/resource/burn tokens centrally.
4. Randomly determine the first player (coin flip or similar).
5. Each player draws **7 cards** as their starting hand. Either may take
   one mulligan: shuffle the 7 back in, redraw 7, keep the second hand.

## Deckbuilding constraints

- Minimum **50 cards**, maximum **100 cards**.
- No more than **3 copies of any card by title** in a deck.
- Order ↔ Destruction cards never share a deck.
- Loyalty: see [cards.md](./cards.md) for how mixing races within a faction
  costs more resources.

## Starter-deck quick-start

The rulebook ships pre-built 40-card starters per race (Dwarf, Empire, Orc,
Chaos). For first games these are padded out to 50 cards by dealing 10
shuffled neutrals to each player.

The codebase's `dwarfStarterDeck` in `backend/src/Invasion/Engine.hs` is the
40-card version — it does not yet include the neutral fill.
