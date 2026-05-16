-- | Smoke test for the phase / turn machinery. Run with @stack test@.
--
-- This is intentionally a one-file, dependency-free test using base +
-- the library — when we adopt hspec we'll move/expand it.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Invasion.Engine
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
  check "lifecycle = GameSetup" (case g0.lifecycle of GameSetup -> True; _ -> False)
  check "phase = Nothing" (case g0.phase of Nothing -> True; _ -> False)
  check "actionWindow = Nothing" (case g0.actionWindow of Nothing -> True; _ -> False)
  check "turn = 0" (g0.turn == Turn 0)

  -- Begin the game: turn 1 starts, kingdom phase runs to its window.
  g1 <- applyMessage g0 BeginGame
  let fp = g0.firstPlayer
  check "lifecycle = GamePlaying" (case g1.lifecycle of GamePlaying -> True; _ -> False)
  check "turn = 1" (g1.turn == Turn 1)
  check "phase = Just KingdomPhase" (case g1.phase of Just KingdomPhase -> True; _ -> False)
  check "currentPlayer = firstPlayer" (g1.currentPlayer == fp)
  check "action window open on kingdom"
    (case g1.actionWindow of
       Just aw -> case aw.trigger of KingdomActionWindow -> True; _ -> False
       Nothing -> False)
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
    (case g2.phase of Just CapitalPhase -> True; _ -> False)
  check "after kingdom: turn still 1" (g2.turn == Turn 1)
  check "after kingdom: window open on capital"
    (case g2.actionWindow of
       Just aw -> case aw.trigger of CapitalActionWindow -> True; _ -> False
       Nothing -> False)
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
    (case g3.phase of Just KingdomPhase -> True; _ -> False)
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
    (case g4.phase of Just QuestPhase -> True; _ -> False)
  check "active drew 1 quest card"
    (length (activePlayer g4).hand == handSizeBefore + 1)
  check "previously-active resources reset to 0 next time their turn ends"
    -- player 1's resources weren't reset because it's not their kingdom
    -- phase yet — sanity check we're not double-collecting.
    ((inactivePlayer g4).resources == Resources 3)

  putStrLn "Phase / turn smoke test: OK"

activePlayer :: Game -> Player
activePlayer g = case g.currentPlayer of
  Player1 -> g.player1
  Player2 -> g.player2

inactivePlayer :: Game -> Player
inactivePlayer g = case g.currentPlayer of
  Player1 -> g.player2
  Player2 -> g.player1

check :: String -> Bool -> IO ()
check label ok =
  if ok
    then putStrLn $ "  ok   " <> label
    else do
      putStrLn $ "  FAIL " <> label
      exitFailure
