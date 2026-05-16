# Combat (the Battlefield phase)

The Battlefield phase is a five-step procedure with an action window
**after every step**. Both players must consecutively pass before the
engine advances. The defending player chooses *which zone is being
attacked* implicitly by what they defend; **the attacker picks the target
zone in step 1**.

```
1. Declare target of attack
   ⤷ action window
2. Declare attackers
   ⤷ action window
3. Declare defenders
   ⤷ action window
4. Assign damage   (placed, not committed)
   ⤷ action window
5. Apply damage    (committed; Toughness etc. fire here)
   ⤷ action window
```

## 1. Declare target of attack

Active player picks **one** of the opponent's three zones (kingdom, quest,
battlefield) to attack. This decision happens *before* attackers are
chosen.

## 2. Declare attackers

Active player picks any subset of units in their own **battlefield** to
attack. Only battlefield units can be declared as attackers — units in
kingdom or quest zone never attack.

Corrupt units cannot attack (see [effects.md](./effects.md) — Corruption).

## 3. Declare defenders

Defender picks any subset of their units in the **attacked zone** to
defend. So if the kingdom is being attacked, only kingdom units can
defend; if the quest zone, only quest units; etc.

Combat is **not** per-unit pairing. All attackers attack together; all
defenders defend together. Damage is pooled and then assigned (step 4).

Corrupt units cannot defend.

> A defender does **not** have to defend. They may let the zone take the
> hit if that's the better play. (Capital damage is real — but so is
> losing units.)

## 4. Assign damage

Each side counts power icons on its participating units; that total is its
**inflicted damage** for this combat.

- **Attacker assigns first.** Damage must cover defending units' remaining
  HP **before** any damage can spill into the attacked capital section.
  The attacker can over-assign onto a unit (e.g., to defeat Toughness or
  cancellation), but must satisfy the minimum-to-each-defender rule before
  reaching the capital.
- **Defender assigns next.** Defender's damage must be assigned to
  attacking units. **Defenders cannot damage the attacker's capital.** The
  defender may also over-assign to a single unit.

Damage at this step is **assigned**, not applied. Tokens are placed *near*
their targets and can still be cancelled. Action window follows.

## 5. Apply damage

Both sides apply all assigned damage simultaneously. Now and only now:

- **Toughness N** cancels N damage on its unit *before* it is applied.
  Cancelled tokens return to the pool.
- Units with applied damage ≥ HP are destroyed and discarded.
- Capital sections with applied damage ≥ remaining HP (= 8 + developments)
  **burn**: all damage tokens on the section are removed, a burn token is
  placed, and that section is overrun.
- If this brings the defender to **two burn tokens**, the game ends
  immediately — attacker wins.

After this step there is one final action window, then the battlefield
phase ends.

## Counterstrike — the exception

**Counterstrike N** on a defending unit deals N **uncancellable** damage
to a single attacking unit (defender's choice) **immediately when the unit
is declared as a defender** — i.e., in step 3, *before* normal damage is
assigned/applied. Counterstrike damage:

- Is applied right away, not buffered for step 5.
- Cannot be split across multiple attackers.
- Cannot be cancelled by Toughness or anything else.
- Does **not** prevent the counterstriking unit from also dealing its
  normal combat damage in step 4 / 5.

This is the **only** combat damage that bypasses the assigned-vs-applied
split. Implementations should special-case it.

## Worked example (from the rulebook, lightly compressed)

- Kris attacks Tom's quest zone. Kris's battlefield: Defender of the Hold,
  Hammerer of Karak Azul (2 HP, Toughness 1), King Kazador (3 power).
- Kris attacks with Hammerer + Kazador (skipping Defender).
- Tom defends with Doom Divers (2 HP, 2 power) from his quest zone.
- Power: Kris inflicts 4 (Kazador 3 + Hammerer 1). Tom inflicts 2 (Divers 2).
- **Assign**: Kris puts 2 damage on Doom Divers (covers their HP), then 2
  on Tom's quest section. Tom puts 2 on Hammerer.
- **Apply**: Hammerer's Toughness 1 cancels 1 → 1 remains. Hammerer
  survives (1 < 2 HP). Doom Divers receive 2 → destroyed. Quest section
  receives 2 damage tokens.

## Non-combat damage

Card effects can deal damage outside of combat. It uses the same
assign-then-apply pattern *except* for Counterstrike, which is always
applied immediately.

Whenever an effect deals damage, model it as a sequence:

```
DamageAssigned target n  →  (action window if rules call for one)  →  DamageApplied target n
```

Some non-combat damage is also tagged "uncancellable" — same semantics as
Counterstrike: Toughness and cancellation effects don't reduce it.
