-- | Smoke test for the phase / turn machinery. Run with @stack test@.
--
-- This is intentionally a one-file, dependency-free test using base +
-- the library — when we adopt hspec we'll move/expand it.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Invasion.Capital (Capital (..), Damage (..), Zone (..))
import Invasion.Card (Card (..), SomeCardDef (..))
import Invasion.CardDef (CardDef (..), Keyword (..))
import Invasion.Engine
import Invasion.Entity (UnitDetails (..))
import Invasion.Game
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

import System.Exit (exitFailure)

main :: IO ()
main = do
  setupResult <- runSetup
  g0 <- case setupResult of
    Left err -> do
      putStrLn $ "FAIL runSetup: " <> err
      exitFailure
    Right g -> pure g

  -- After setup: hands dealt, first player chosen, no turn started yet.
  check "p1 hand = 7" (length g0.player1.hand == 7)
  check "p2 hand = 7" (length g0.player2.hand == 7)
  check "lifecycle = GameSetup" (isGameSetup g0.lifecycle)
  check "phase = Nothing" (isNothing g0.phase)
  check "actionWindow = Nothing" (isNothing g0.actionWindow)
  check "turn = 0" (g0.turn == Turn 0)

  -- Begin the game: turn 1 starts, kingdom phase runs to its window.
  g1 <- applyMessage g0 BeginGame
  let fp = g0.firstPlayer
  check "lifecycle = GamePlaying" (isGamePlaying g1.lifecycle)
  check "turn = 1" (g1.turn == Turn 1)
  check "phase = Just KingdomPhase" (g1.phase == Just KingdomPhase)
  check "currentPlayer = firstPlayer" (g1.currentPlayer == fp)
  check "action window open on kingdom"
    (windowTrigger g1.actionWindow == Just KingdomActionWindow)
  check "priority with active player"
    (case g1.actionWindow of
       Just aw -> priorityHolder aw.awaiting == fp
       Nothing -> False)
  check "active player has 3 resources"
    ((activePlayer g1).resources == Resources 3)
  check "inactive player has 0 resources"
    ((inactivePlayer g1).resources == Resources 0)

  -- Both players pass: kingdom window closes; quest phase skipped on
  -- turn 1 for the first player; capital phase opens its window.
  g2 <- applyMessages g1 [PassPriority fp, PassPriority fp.next]
  check "after kingdom: phase = Just CapitalPhase (Quest skipped)"
    (g2.phase == Just CapitalPhase)
  check "after kingdom: turn still 1" (g2.turn == Turn 1)
  check "after kingdom: window open on capital"
    (windowTrigger g2.actionWindow == Just CapitalActionWindow)
  check "no quest draw happened (first turn skips quest)"
    (length g2.player1.hand == 7 && length g2.player2.hand == 7)

  -- Pass capital window. Battlefield is also skipped on turn 1, so the
  -- turn ends and play hands off to the other player at the start of
  -- their kingdom phase.
  g3 <- applyMessages g2 [PassPriority fp, PassPriority fp.next]
  check "after capital: turn = 2" (g3.turn == Turn 2)
  check "after capital: currentPlayer flipped"
    (g3.currentPlayer == fp.next)
  check "after capital: phase = Just KingdomPhase"
    (g3.phase == Just KingdomPhase)
  check "new active has 3 resources"
    ((activePlayer g3).resources == Resources 3)
  check "previously-active player still has 3 resources (not yet reset)"
    ((inactivePlayer g3).resources == Resources 3)

  -- On the SECOND turn the active player no longer skips Quest or
  -- Battlefield. Pass the kingdom window; we should land on a quest
  -- action window with a card drawn.
  let active2 = g3.currentPlayer
  let handSizeBefore = length (activePlayer g3).hand
  g4 <- applyMessages g3 [PassPriority active2, PassPriority active2.next]
  check "after kingdom (T2): phase = Just QuestPhase (no skip)"
    (g4.phase == Just QuestPhase)
  check "active drew 1 quest card"
    (length (activePlayer g4).hand == handSizeBefore + 1)
  check "previously-active resources reset to 0 next time their turn ends"
    -- player 1's resources weren't reset because it's not their kingdom
    -- phase yet — sanity check we're not double-collecting.
    ((inactivePlayer g4).resources == Resources 3)

  -- PlayUnit: pick any affordable Unit from the active player's hand
  -- and play it into the kingdom zone. The active player has just been
  -- granted 3 resources for the new turn (see g3 above), so anything
  -- costing 3 or less is fair game.
  let preP = activePlayer g4
  case findPlayableUnit preP of
    Nothing -> do
      putStrLn "  FAIL active hand has no affordable Unit; can't exercise PlayUnit"
      exitFailure
    Just (cardKey, cardCode, cardCost) -> do
      let handBefore = length preP.hand
          Resources resBefore = preP.resources
      g5 <- applyMessage g4 (PlayUnit g4.currentPlayer cardKey KingdomZone)
      let postP = activePlayer g5
      check "PlayUnit: hand size decreased by 1"
        (length postP.hand == handBefore - 1)
      check "PlayUnit: resources reduced by cost"
        (postP.resources == Resources (resBefore - cardCost))
      check "PlayUnit: game has exactly one in-play unit"
        (length g5.units == 1)
      let unitOk match err =
            case g5.units of
              [UnitDetails {controller, zone, cardDef = CardDef {code}}] ->
                match controller zone code
              _ -> False
            where _ = err :: String
      check "PlayUnit: unit controller = active player"
        (unitOk (\c _ _ -> c == g4.currentPlayer) "controller")
      check "PlayUnit: unit zone = Kingdom"
        (unitOk (\_ z _ -> z == KingdomZone) "zone")
      check "PlayUnit: card code carried through"
        (unitOk (\_ _ c -> c == cardCode) "code")
      check "PlayUnit: in-play unit reuses the in-hand card key"
        ( case g5.units of
            [u] -> u.key == cardKey
            _ -> False
        )

      -- DealDamageToUnit: apply 1 damage and check the unit's damage
      -- counter advanced (or the unit was destroyed if HP=1).
      g6 <- applyMessage g5 (DealDamageToUnit cardKey 1)
      case [u | u <- g6.units, u.key == cardKey] of
        [UnitDetails {damage = dmg}] ->
          check "DealDamageToUnit: damage recorded"
            (dmg == Damage 1)
        [] ->
          check "DealDamageToUnit: 1-HP unit destroyed"
            (null g6.units)
        _ -> do
          putStrLn "  FAIL multiple units share the played card key"
          exitFailure

      -- DestroyUnit: nuke whatever's left in play and confirm it moves
      -- to the controller's discard pile.
      let discardBefore = length (activePlayer g6).discard
      g7 <- case g6.units of
        [] -> pure g6 -- already gone above
        _ -> applyMessage g6 (DestroyUnit cardKey)
      check "DestroyUnit: no units in play after"
        (null g7.units)
      check "DestroyUnit: card lands in controller's discard"
        (length (activePlayer g7).discard >= discardBefore + 1
          || null g6.units)

  -- Combat smoke: feed an artificial state with a unit on each side
  -- and run BeginCombat → assert damage landed.
  setupResult2 <- runSetup
  gA <- case setupResult2 of
    Right g -> applyMessage g BeginGame
    Left err -> do
      putStrLn $ "FAIL second runSetup: " <> err
      exitFailure
  -- Skip turn 1 entirely, get to turn 2 capital window.
  gB <- applyMessages gA
    [ PassPriority gA.currentPlayer
    , PassPriority gA.currentPlayer.next
    , PassPriority gA.currentPlayer
    , PassPriority gA.currentPlayer.next
    ]
  -- Now on opponent's turn 1 (still "turn 2" counter-wise).
  -- Drop a 1-HP unit into the active player's battlefield via PlayUnit
  -- if one is in hand. Otherwise skip the rest of this smoke.
  case findPlayableUnit (activePlayer gB) of
    Nothing -> putStrLn "  skip combat smoke (no playable unit)"
    Just (cardKey, _, _) -> do
      gC <- applyMessage gB (PlayUnit gB.currentPlayer cardKey BattlefieldZone)
      -- BeginCombat attacker against opponent's battlefield. Auto-pick
      -- the attacker we just placed.
      let attackerKeys =
            [ k
            | UnitDetails {key = k, controller = c} <- gC.units
            , c == gB.currentPlayer
            ]
      gD <- applyMessage gC (BeginCombat gB.currentPlayer BattlefieldZone attackerKeys)
      check "Combat: combat state cleared after resolve"
        (isNothing gD.combat)
      check "Combat: at least some damage left a mark"
        (let Player {capital = Capital {battlefield = Zone {damage = Damage zd}}} =
                case gB.currentPlayer of
                  Player1 -> gD.player2
                  Player2 -> gD.player1
             anyUnitHurt = any
               (\UnitDetails {damage = Damage d} -> d > 0)
               gD.units
          in zd > 0 || anyUnitHurt)

  putStrLn "Phase / turn smoke test: OK"

activePlayer :: Game -> Player
activePlayer g = case g.currentPlayer of
  Player1 -> g.player1
  Player2 -> g.player2

inactivePlayer :: Game -> Player
inactivePlayer g = case g.currentPlayer of
  Player1 -> g.player2
  Player2 -> g.player1

isGameSetup :: GameState -> Bool
isGameSetup = \case
  GameSetup -> True
  _ -> False

isGamePlaying :: GameState -> Bool
isGamePlaying = \case
  GamePlaying -> True
  _ -> False

-- | Trigger of the currently-open action window, if any. Lets tests
-- compare against an expected 'ActionWindowTrigger' value directly
-- instead of pattern-matching the wrapping 'Just'.
windowTrigger :: Maybe ActionWindow -> Maybe ActionWindowTrigger
windowTrigger = fmap (.trigger)

-- | Find the first Unit in a player's hand whose total cost (printed +
-- loyalty surcharge, accounting for the player's capital race symbol)
-- is within that player's current resources, and which doesn't carry
-- Toughness (which would let the smoke's 1-damage test get cancelled
-- entirely). Returns the card's in-hand key, printed code, and total
-- effective cost.
findPlayableUnit :: Player -> Maybe (UnitKey, CardCode, Int)
findPlayableUnit p =
  let Resources budget = p.resources
  in go p.hand budget
  where
    hasToughness cardDef =
      any isToughness cardDef.keywords
    isToughness = \case
      Toughness _ -> True
      _ -> False
    go [] _ = Nothing
    go (Card {key, def} : rest) budget = case def of
      UnitCardDef cardDef
        | hasToughness cardDef -> go rest budget
        | otherwise ->
            let printed = case cardDef.cost of
                  Fixed n -> n
                  Variable -> 1000
                symbolMatch = if p.race `elem` cardDef.races then 1 else 0
                loyaltySurcharge = max 0 (cardDef.loyalty - symbolMatch)
                total = printed + loyaltySurcharge
             in if total <= budget
                  then Just (key, cardDef.code, total)
                  else go rest budget
      _ -> go rest budget

check :: String -> Bool -> IO ()
check label ok =
  if ok
    then putStrLn $ "  ok   " <> label
    else do
      putStrLn $ "  FAIL " <> label
      exitFailure
