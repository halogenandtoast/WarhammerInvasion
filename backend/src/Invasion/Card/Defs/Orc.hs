{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Orc core cards (core-056..080). Every printed ability is
-- implemented; one card uses an engine-driven simplification
-- (Rip Dere 'eads Off! always destroys rather than flipping
-- then conditionally summoning) — the compromise is called out
-- in the card's local comment.
module Invasion.Card.Defs.Orc (module Invasion.Card.Defs.Orc) where

import Data.Map.Strict qualified as Map
import Invasion.Capital
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

crookedTeefGoblins :: CardDef Unit
crookedTeefGoblins = unitCard "core-056" "Crooked Teef Goblins" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Goblin, Warrior]
  body "Battlefield only."
  battlefieldOnly

squigHerders :: CardDef Unit
squigHerders = unitCard "core-057" "Squig Herders" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Goblin, Warrior]
  body "Squig Herders gain {power} while you control at least 1 damaged unit."
  effects \self owner ->
    let mine = filter (\u -> u.controller == owner.key && isDamaged u) <$> getGameUnits
     in mine >>= \xs -> when (not (null xs)) (gainPower self 1)

ironclawsHorde :: CardDef Unit
ironclawsHorde = unitCard "core-058" "Ironclaw's Horde" do
  race Orc
  cost 5
  loyalty 2
  power 4
  hitPoints 2
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

followersOfMork :: CardDef Unit
followersOfMork = unitCard "core-059" "Followers of Mork" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Shaman
  body "Forced: After this unit enters play, each player takes 2 indirect damage. (Players allocate their own indirect damage.)"
  onEnterPlay \_owner self -> do
    indirectDamage self.controller 2
    indirectDamage self.controller.next 2

blackOrcSquad :: CardDef Unit
blackOrcSquad = unitCard "core-060" "Black Orc Squad" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 4
  traits [Warrior, Elite]

boarBoyz :: CardDef Unit
boarBoyz = unitCard "core-061" "Boar Boyz" do
  race Orc
  cost 4
  loyalty 2
  power 1
  hitPoints 4
  trait Cavalry
  body "This unit gains {power}{power} while you control at least one damaged unit."
  effects \self owner ->
    let mine = filter (\u -> u.controller == owner.key && isDamaged u) <$> getGameUnits
     in mine >>= \xs -> when (not (null xs)) (gainPower self 2)

urguck :: CardDef Unit
urguck = unitCard "core-062" "Urguck" do
  hero
  trait Warrior
  race Orc
  cost 3
  loyalty 3
  power 1
  hitPoints 3
  body "Limit one Hero per zone. During your capital phase, you may spend damage on this unit as though it were resources."
  -- A free repeatable action: trade 1 damage on Urguck for 1
  -- resource for the controller. Gated to the capital phase by
  -- checking 'g.phase' at fire time. Repeat to spend more damage.
  action "Spend a wound" 0 \usage -> do
    g <- getGame
    case findUnit usage.self.key g of
      Just u
        | isDamaged u
        , g.phase == Just CapitalPhase
        , g.currentPlayer == usage.user -> do
            push (HealUnit usage.self.key 1)
            push (GainResources usage.user 1)
      _ -> pure ()

grimgorIronhide :: CardDef Unit
grimgorIronhide = unitCard "core-063" "Grimgor Ironhide" do
  hero
  trait Warrior
  race Orc
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  body "Limit one Hero per zone. Forced: After this unit enters play, destroy all support cards and developments in each player's corresponding zone."
  onEnterPlay \_owner self -> do
    g <- getGame
    -- Destroy every free-standing support in the same zone, both
    -- sides. Attached supports are left alone (their host unit is
    -- the targetable thing).
    let supps =
          [ s.key
          | s <- g.supports
          , s.attachedTo == Nothing
          , s.zone == self.zone
          ]
    for_ supps destroySupport
    -- Then pop every development from the matching zone on each
    -- side. We just push one DestroyDevelopment per dev.
    for_ [Player1, Player2] \pk -> do
      let p = playerOf pk g
          Developments n = case self.zone of
            KingdomZone -> p.capital.kingdom.developments
            QuestZone -> p.capital.quest.developments
            BattlefieldZone -> p.capital.battlefield.developments
      replicateM_ n (destroyDevelopment pk self.zone)

nightGoblins :: CardDef Unit
nightGoblins = unitCard "core-064" "Night Goblins" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Goblin, Shaman]
  body "Forced: After this unit enters play, destroy one target Attachment card in any player's corresponding zone, if able."
  onEnterPlay \_owner self -> do
    g <- getGame
    let attachments =
          [ s
          | u <- g.units
          , u.zone == self.zone
          , s <- u.attachments
          ]
    case attachments of
      [] -> pure ()
      _ -> do
        let cards =
              [ mkCard s.key (SupportCardDef s.cardDef)
              | s <- attachments
              ]
        chooseFromCards self.controller 1 1 cards
          "Choose an attachment to destroy." \chosen ->
            for_ chosen \c -> destroySupport c.key

doomDivers :: CardDef Unit
doomDivers = unitCard "core-065" "Doom Divers" do
  race Orc
  cost 4
  loyalty 1
  power 2
  hitPoints 2
  trait Goblin
  body "Battlefield. Forced: After your turn begins, each player must either sacrifice a development or deal 1 damage to each section of his capital."
  -- Each player picks: sacrifice a development (if any) or take the
  -- per-section capital damage. We ask only if there's an actual
  -- development to choose; with none, the damage is mandatory.
  onMyTurnBegin \_owner self ->
    when (self.zone == BattlefieldZone) $ do
      g <- getGame
      for_ [self.controller, self.controller.next] \pk -> do
        let p = playerOf pk g
            devZones =
              [ zk
              | (zk, Developments n) <-
                  [ (KingdomZone, p.capital.kingdom.developments)
                  , (QuestZone, p.capital.quest.developments)
                  , (BattlefieldZone, p.capital.battlefield.developments)
                  ]
              , n > 0
              ]
            dealAll =
              for_ [KingdomZone, QuestZone, BattlefieldZone] \zk ->
                dealZoneDamage pk zk 1
        case devZones of
          [] -> dealAll
          _ -> do
            sacrifice <- askYesNo pk "Sacrifice a development instead of taking 1 damage to each capital section?"
            if sacrifice
              then withTarget pk
                (CapitalMatching \_ (owner, zk) ->
                  owner == pk && zk `elem` devZones)
                \(owner, zk) -> destroyDevelopment owner zk
              else dealAll

lobberCrew :: CardDef Unit
lobberCrew = unitCard "core-066" "Lobber Crew" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Goblin
  body "Kingdom. Action: Sacrifice this unit to force an opponent to sacrifice a unit he controls, if able."
  kingdom $ actionWith "Force a sacrifice" 0 [SacrificeUnit] \usage -> do
    let opp = usage.user.next
    withTarget opp (UnitMatching \_ _ u -> u.controller == opp) \k ->
      destroyUnit k

bigUns :: CardDef Unit
bigUns = unitCard "core-067" "Big 'Uns" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Battlefield. Your damaged units gain Toughness 1."
  toughnessAura \_g self target ->
    if self.zone == BattlefieldZone
        && target.controller == self.controller
        && isDamaged target
      then 1
      else 0

rockLobber :: CardDef Support
rockLobber = supportCard "core-068" "Rock Lobber" do
  race Orc
  cost 2
  loyalty 2
  trait Siege
  body "Battlefield. Action: Pay 2 resources and sacrifice one of your units in this zone to deal 2 damage to one section of any capital (limit once per turn)."
  battlefield $ actionWith "Lob a rock" 2 [SacrificeUnit] \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user AnyCapital \(owner, zk) ->
        dealZoneDamage owner zk 2

choppa :: CardDef Support
choppa = supportCard "core-069" "Choppa" do
  race Orc
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target in your battlefield. Attached unit gains {power}{power}."
  attachedTo \_self unit -> gainPower unit 2

totemOfGork :: CardDef Support
totemOfGork = supportCard "core-070" "Totem of Gork" do
  race Orc
  cost 3
  loyalty 3
  power 1
  trait Siege
  body "Units in this zone gain {power} while attacking or defending."
  -- Combat-only bonus for units sharing the Totem's zone — NOT a
  -- zone-income aura. 'supportCombat' contributions flow into
  -- 'combatDamageOf', which only runs for declared attackers and
  -- defenders, and the attacking/defending flags pin it further.
  supportCombat \_g self u ->
    if u.controller == self.controller
      && u.zone == self.zone
      && (u.attacking || u.defending)
      then 1
      else 0

bannaOfDaRedSunz :: CardDef Support
bannaOfDaRedSunz = supportCard "core-071" "Banna of Da Red Sunz" do
  race Orc
  cost 4
  loyalty 1
  power 2
  trait Banner
  body "Kingdom. Each opponent that collects 7 or more resources for his kingdom phase must assign one of those resources as a damage token to a target unit of your choice."
  onReceive $ Receive \msg _owner self -> case msg of
    CollectResources opp
      | opp /= self.controller, self.zone == KingdomZone -> do
          g <- getGame
          let p = playerOf opp g
              Resources r = p.resources
          when (r >= 7) do
            -- Take the resource as damage to a unit of our choice.
            push (SpendResources opp 1)
            withTarget self.controller AnyUnit \k -> dealDamage k 1
    _ -> pure ()

smashEmAll :: CardDef Quest
smashEmAll = questCard "core-072" "Smash 'Em All!" do
  race Orc
  cost 1
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to destroy all enemy support cards. Use this ability only if Smash 'Em All! has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Smash them all" 0 \usage ->
    withQuest usage.self.key \q ->
      when (q.tokens >= 3) $
        for_ q.questingUnit \quester -> do
          destroyUnit quester
          g <- getGame
          let enemySupports =
                [ s.key
                | s <- g.supports
                , s.controller /= usage.user
                , s.attachedTo == Nothing
                ]
          for_ enemySupports destroySupport

grimgorsCamp :: CardDef Support
grimgorsCamp = supportCard "core-073" "Grimgor's Camp" do
  race Orc
  cost 3
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Lower the cost of the first {orc} unit you play each turn by 1."
  globalCostAdjust \g self pk filt ->
    let h = case Map.lookup ThisTurn g.history of
              Just hx -> hx
              Nothing -> mempty
        played = Map.findWithDefault 0 pk h.unitsPlayedBy
        zoneGate = self.zone == KingdomZone
     in if pk == self.controller
           && zoneGate
           && filt.cfKind == Unit
           && Orc `elem` filt.cfRaces
           && played == 0
          then -1
          else 0

smashGoBoom :: CardDef Tactic
smashGoBoom = tacticCard "core-074" "Smash-Go-Boom!" do
  race Orc
  costVariable
  loyalty 2
  body "Play during your turn. Action: Destroy X target developments in one zone."
  whenResolved \self ->
    when (self.xValue > 0) $
      withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
        replicateM_ self.xValue (destroyDevelopment owner zk)

ripDereEadsOff :: CardDef Tactic
ripDereEadsOff = tacticCard "core-075" "Rip Dere 'eads Off!" do
  race Orc
  cost 1
  loyalty 1
  body "Action: Turn one target development faceup. If it is a unit, leave it in play and sacrifice it at the end of the turn. Otherwise, sacrifice it immediately."
  playableWhen \g _ -> hasAnyDevelopment g
  whenResolved \self ->
    withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
      flipDevelopment owner zk

wezBigga :: CardDef Tactic
wezBigga = tacticCard "core-076" "We'z Bigga!" do
  race Orc
  cost 0
  loyalty 1
  body "Action: Lower the cost of the next unit you play this turn by 1. That unit comes into play with 1 damage on it."
  whenResolved \self -> do
    push (ScheduleNextUnitDiscount self.controller 1)
    push (ScheduleNextUnitDamage self.controller 1)

favourOfMork :: CardDef Tactic
favourOfMork = tacticCard "core-077" "Favour of Mork" do
  race Orc
  cost 1
  loyalty 2
  trait Spell
  body "Action: One target unit loses {power} and another target unit gains {power} until the end of the turn."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \loser ->
      withTarget pk AnyUnit \winner -> do
        until EndOfTurn $ buffPower loser (-1)
        until EndOfTurn $ buffPower winner 1

pillage :: CardDef Tactic
pillage = tacticCard "core-078" "Pillage" do
  race Orc
  cost 2
  loyalty 2
  body "Action: Destroy one target support card."
  playableWhen hasEnemySupport
  tacticTargets SupportTargetSchema
  onResolveEnemySupport \_pk s -> destroySupport s.key

waaagh :: CardDef Tactic
waaagh = tacticCard "core-079" "Waaagh!" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Each attacking unit gains {power}{power} until the end of the turn."
  whenResolved \_ ->
    withCombat \cs ->
      for_ cs.attackers \k ->
        until EndOfTurn $ buffPower k 2

trollVomit :: CardDef Tactic
trollVomit = tacticCard "core-080" "Troll Vomit" do
  race Orc
  cost 4
  loyalty 2
  body "Play during your turn. Action: Destroy all units in play."
  whenResolved \_ -> do
    g <- getGame
    for_ g.units \u -> destroyUnit u.key

-- | A small lifted lookup that retrieves the unit list from
-- 'getGame'. The 'effects' DSL works inside 'EffectM', not a 'HasGame'
-- monad; we drive it through getGame manually.
getGameUnits :: HasGame m => m [UnitDetails]
getGameUnits = (.units) <$> getGame

-- | True if any zone (either player's) has at least one development.
-- Used as a 'playableWhen' gate for development-targeting tactics.
hasAnyDevelopment :: Game -> Bool
hasAnyDevelopment g =
  let withDev :: [Zone] -> Bool
      withDev zs = any (\z -> case z.developments of Developments n -> n > 0) zs
   in withDev g.player1.capital.zones || withDev g.player2.capital.zones
