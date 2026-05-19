{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card (module Invasion.Card) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import {-# SOURCE #-} Invasion.Engine (HasPromptIO (..))
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Capital
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue (..), push)

data SomeCardDef where
  UnitCardDef :: CardDef 'Unit -> SomeCardDef
  SupportCardDef :: CardDef 'Support -> SomeCardDef
  QuestCardDef :: CardDef 'Quest -> SomeCardDef
  TacticCardDef :: CardDef 'Tactic -> SomeCardDef
  LegendCardDef :: CardDef 'Legend -> SomeCardDef

instance Show SomeCardDef where
  show (UnitCardDef card) = show card
  show (SupportCardDef card) = show card
  show (QuestCardDef card) = show card
  show (TacticCardDef card) = show card
  show (LegendCardDef card) = show card

instance ToJSON SomeCardDef where
  toJSON (UnitCardDef card) = toJSON card
  toJSON (SupportCardDef card) = toJSON card
  toJSON (QuestCardDef card) = toJSON card
  toJSON (TacticCardDef card) = toJSON card
  toJSON (LegendCardDef card) = toJSON card

-- | Per-kind extractors for 'SomeCardDef'. Used by engine helpers like
-- 'takeFromPile' so a single take-from-pile loop can be parameterised
-- by which card kind it expects to find.
asUnit :: SomeCardDef -> Maybe (CardDef Unit)
asUnit = \case
  UnitCardDef cd -> Just cd
  _ -> Nothing

asSupport :: SomeCardDef -> Maybe (CardDef Support)
asSupport = \case
  SupportCardDef cd -> Just cd
  _ -> Nothing

asQuest :: SomeCardDef -> Maybe (CardDef Quest)
asQuest = \case
  QuestCardDef cd -> Just cd
  _ -> Nothing

asTactic :: SomeCardDef -> Maybe (CardDef Tactic)
asTactic = \case
  TacticCardDef cd -> Just cd
  _ -> Nothing

asLegend :: SomeCardDef -> Maybe (CardDef Legend)
asLegend = \case
  LegendCardDef cd -> Just cd
  _ -> Nothing

-- | A card instance: a definition paired with the stable 'UnitKey' that
-- identifies this specific copy across the entire game. The same key is
-- minted when the card is shuffled into the deck, carried through the
-- hand, into play (as a 'UnitDetails' / 'SupportDetails' / …), and back
-- into discard if the card leaves play. Keys never recycle.
--
-- The frontend uses the key as a CSS view-transition name so a card
-- visually morphs from one location to another as state updates.
data Card = Card
  { key :: UnitKey
  , def :: SomeCardDef
  }
  deriving stock Show

-- | Flatten the card definition into a JSON object and splice in the
-- 'key' field, so the wire shape is the existing 'CardDef' surface
-- (code, title, traits, …) plus a stable integer key. Falls back to a
-- tagged object if the inner JSON is somehow not an object (defensive;
-- 'CardDef' always serializes as one today).
instance ToJSON Card where
  toJSON c = case toJSON c.def of
    Object o -> Object (KeyMap.insert (AesonKey.fromString "key") (toJSON c.key) o)
    other -> object ["key" .= c.key, "def" .= other]

-- | Convenience: lift a definition to a 'Card' with the given key.
mkCard :: UnitKey -> SomeCardDef -> Card
mkCard k d = Card {key = k, def = d}

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

emptyCardDef :: forall k. HasDefaultExtras k => CardCode -> String -> CardKind -> CardDef k
emptyCardDef code title kind =
  CardDef
    code
    title
    kind
    []
    (Fixed 0)
    0
    0
    Nothing
    []
    Nothing
    Nothing
    []
    False
    []
    noReceive
    (defaultExtras @k)
    (\_ _ -> 0)
    (\_ _ -> True)

unitCard :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unitCard code title = buildCard $ (emptyCardDef code title Unit) {CardDef.hitPoints = Just (Fixed 1)}

supportCard :: CardCode -> String -> CardBuilder Support a -> CardDef Support
supportCard code title = buildCard $ emptyCardDef code title Support

questCard :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
questCard code title = buildCard $ emptyCardDef code title Quest

tacticCard :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tacticCard code title = buildCard $ emptyCardDef code title Tactic

-- | Legends are persistent like units (they have hit points and live on
-- the capital board) but they're their own card type and are not
-- targeted by unit/support/tactic effects. The HP default of 1 mirrors
-- 'unitCard'; legend cards will normally override via 'hitPoints'.
legendCard :: CardCode -> String -> CardBuilder Legend () -> CardDef Legend
legendCard code title = buildCard $ (emptyCardDef code title Legend) {CardDef.hitPoints = Just (Fixed 1)}

buildCard :: CardDef k -> CardBuilder k a -> CardDef k
buildCard def (CardBuilder inner) = execState inner def

unique :: CardBuilder k ()
unique = modify \cardDef -> cardDef {unique = True}

race :: Race -> CardBuilder k ()
race r = modify \cardDef -> cardDef {races = r : cardDef.races}

cost :: Int -> CardBuilder k ()
cost c = modify \cardDef -> cardDef {cost = Fixed c}

loyalty :: Int -> CardBuilder k ()
loyalty l = modify \cardDef -> cardDef {loyalty = l}

power :: Int -> CardBuilder k ()
power p = modify \cardDef -> cardDef {power = p}

-- | Cards that carry hit points: units and legends. Other kinds reject
-- 'hitPoints' at the type level so a tactic builder can't silently set
-- an HP value that the engine would never read.
class HasHitPoints (k :: CardKind)
instance HasHitPoints 'Unit
instance HasHitPoints 'Legend

hitPoints :: HasHitPoints k => Int -> CardBuilder k ()
hitPoints hp = modify \cardDef -> cardDef {CardDef.hitPoints = Just (Fixed hp)}

trait :: Trait -> CardBuilder k ()
trait t = modify \cardDef -> cardDef {traits = t : cardDef.traits}

traits :: [Trait] -> CardBuilder k ()
traits = traverse_ trait

body :: String -> CardBuilder k ()
body f = modify \cardDef -> cardDef {CardDef.text = Just f}

flavor :: String -> CardBuilder k ()
flavor f = modify \cardDef -> cardDef {flavor = Just f}

keyword :: Keyword -> CardBuilder k ()
keyword k = modify \cardDef -> cardDef {keywords = k : cardDef.keywords}

toughness :: Int -> CardBuilder Unit ()
toughness n = keyword (Toughness $ Fixed n)

toughnessX :: CardBuilder Unit ()
toughnessX = keyword (Toughness Variable)

scout :: CardBuilder Unit ()
scout = keyword Scout

-- | Counterstrike N keyword: while declared as a defender, this unit
-- immediately deals N uncancellable damage to one attacker of its
-- choice before regular combat damage assigns.
counterstrike :: Int -> CardBuilder Unit ()
counterstrike n = keyword (Counterstrike n)

-- | Ambush keyword (FAQ 2.2 v2.0): only triggerable while the host
-- development is face-down. Used by Cataclysm-era developments.
ambush :: CardBuilder k ()
ambush = keyword Ambush

-- | Order Only keyword: cannot be included in a Destruction deck.
orderOnly :: CardBuilder k ()
orderOnly = keyword OrderOnly

-- | Destruction Only keyword: cannot be included in an Order deck.
destructionOnly :: CardBuilder k ()
destructionOnly = keyword DestructionOnly

-- | Limit one Hero per zone: while a player controls this Hero in a
-- zone, neither player may put another Hero into that same zone.
limitOneHeroPerZone :: CardBuilder Unit ()
limitOneHeroPerZone = keyword LimitOneHeroPerZone

-- | Hero meta-builder. Sets 'unique', adds the 'Hero' trait, and
-- installs the "Limit one Hero per zone" keyword in one step. Combine
-- with additional traits ('trait Warrior', 'trait Sorcerer', …).
--
-- > durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
-- >   hero
-- >   trait Warrior
-- >   race Dwarf
-- >   ...
hero :: CardBuilder Unit ()
hero = do
  unique
  trait Hero
  limitOneHeroPerZone

-- | Zone-restriction keywords for cards that may only enter play in a
-- specific zone.
kingdomOnly :: CardBuilder k ()
kingdomOnly = keyword KingdomOnly

questOnly :: CardBuilder k ()
questOnly = keyword QuestOnly

battlefieldOnly :: CardBuilder k ()
battlefieldOnly = keyword BattlefieldOnly

-- | Append a 'Receive' handler. The existing receiver runs first, then
-- the new one; this lets a card stack several event-specific hooks
-- without one stomping the previous one. 'emptyCardDef' starts with
-- 'noReceive', so the first append is effectively a set.
onReceive :: Receive k -> CardBuilder k ()
onReceive (Receive new) = modify \cardDef ->
  let Receive prev = cardDef.receive
   in cardDef
        { receive = Receive \msg owner self -> do
            prev msg owner self
            new msg owner self
        }

-- ---------------------------------------------------------------------
-- Trigger DSL
--
-- These combinators wrap 'onReceive' with message-pattern matchers, so
-- card definitions don't have to spell out @case msg of UnitEnteredPlay
-- pk uk | pk == self.controller …@ boilerplate. Each combinator is
-- thin: it inspects the message, applies a kind-appropriate self-check,
-- and forwards to the supplied handler with the same capability set as
-- 'Receive'. Card bodies can mix and match them; 'onReceive' composes.
-- ---------------------------------------------------------------------

-- | Per-kind matcher for "this in-play card just entered play". Each
-- kind has its own 'Entered' message constructor; the class hides the
-- ceremony from card-side code.
class HasEnteredPlay (k :: CardKind) where
  matchEnteredPlay :: Message -> Maybe (PlayerKey, UnitKey)

instance HasEnteredPlay Unit where
  matchEnteredPlay = \case
    UnitEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Support where
  matchEnteredPlay = \case
    SupportEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Quest where
  matchEnteredPlay = \case
    QuestEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Legend where
  matchEnteredPlay = \case
    LegendEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

-- | Per-kind matcher for "this in-play card is being destroyed".
class HasDestroyMatch (k :: CardKind) where
  matchDestroy :: Message -> Maybe UnitKey

instance HasDestroyMatch Unit where
  matchDestroy = \case
    DestroyUnit uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Support where
  matchDestroy = \case
    DestroySupport uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Quest where
  matchDestroy = \case
    DestroyQuest uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Legend where
  matchDestroy = \case
    DestroyLegend uk -> Just uk
    _ -> Nothing

-- | Common capability set every trigger body has access to. Mirrors the
-- constraints on 'Receive' so handlers may be lifted in and out.
type TriggerM m =
  (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)

-- | "When this card enters play." Fires only for the entry message of
-- the matching kind and only when the controller and key identify this
-- specific in-play instance.
onEnterPlay
  :: forall k
   . ( HasEnteredPlay k
     , HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onEnterPlay handler = onReceive $ Receive \msg owner self ->
  case matchEnteredPlay @k msg of
    Just (pk, uk)
      | pk == self.controller && uk == self.key ->
          handler owner self
    _ -> pure ()

-- | "When this card is destroyed." Useful for sacrifice-on-destruction
-- reactions (Festering Nurglings, Doombull, …). Pairs with the
-- 'UnitLeftPlay' / 'SupportLeftPlay' /… narration messages, but fires
-- one step earlier — at the actual destroy event — which is what most
-- "when this unit leaves play, …" cards want.
onSelfDestroyed
  :: forall k
   . ( HasDestroyMatch k
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onSelfDestroyed handler = onReceive $ Receive \msg owner self ->
  case matchDestroy @k msg of
    Just uk | uk == self.key -> handler owner self
    _ -> pure ()

-- | "At the beginning of my controller's turn." Cards that further
-- gate on @self.zone == KingdomZone@ etc. should add that check inside
-- the handler body.
onMyTurnBegin
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk | pk == self.controller -> handler owner self
  _ -> pure ()

-- | "At the end of my controller's turn." (e.g. Chaos Spawn.)
onMyTurnEnd
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyTurnEnd handler = onReceive $ Receive \msg owner self -> case msg of
  EndTurn pk | pk == self.controller -> handler owner self
  _ -> pure ()

-- | "At the end of phase P on my controller's turn." Used by
-- Valkia the Bloody (end of her quest phase).
onMyPhaseEnd
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => Phase
  -> (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyPhaseEnd phase handler = onReceive $ Receive \msg owner self -> case msg of
  EndPhase p
    | p == phase, owner.key == self.controller ->
        handler owner self
  _ -> pure ()

-- | "When ANY 'BeginTurn' fires." The handler receives the turn owner
-- as its third argument. Use this for cards whose trigger depends on
-- some other player's turn (e.g. Mark of Chaos checks the host's
-- controller).
onAnyTurnBegin
  :: forall k
   . (forall m. TriggerM m => Player -> InPlay k -> PlayerKey -> m ())
  -> CardBuilder k ()
onAnyTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk -> handler owner self pk
  _ -> pure ()

-- | "When an opponent's unit enters play." Skips self-triggering so a
-- unit's own entry doesn't fire its opponent-watch handler.
onOpponentUnitEnterPlay
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onOpponentUnitEnterPlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitEnteredPlay pk uk
    | pk /= self.controller && uk /= self.key ->
        handler owner self uk
  _ -> pure ()

-- | "When an opponent's unit leaves play." Carries the departing unit's
-- key, zone, and card code so handlers can react to specifics.
onOpponentUnitLeavePlay
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> UnitKey -> ZoneKind -> CardCode -> m ()
     )
  -> CardBuilder k ()
onOpponentUnitLeavePlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitLeftPlay du
    | du.controller /= self.controller ->
        handler owner self du.key du.zone du.cardDef.code
  _ -> pure ()

-- | "When one of my OTHER friendly units leaves play." Excludes the
-- card itself, which matters for unit cards that watch their teammates
-- (Dwarf Ranger).
onFriendlyUnitLeavePlay
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> UnitKey -> ZoneKind -> CardCode -> m ()
     )
  -> CardBuilder k ()
onFriendlyUnitLeavePlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitLeftPlay du
    | du.controller == self.controller, du.key /= self.key ->
        handler owner self du.key du.zone du.cardDef.code
  _ -> pure ()

-- | "When this tactic resolves." The handler receives the chosen
-- target carried by 'PlayTactic'; for tactics that ignore the target
-- the third argument can be ignored.
onResolve
  :: ( forall m
      . TriggerM m
     => Player -> TacticContext -> ActionTarget -> m ()
     )
  -> CardBuilder Tactic ()
onResolve handler = onReceive $ Receive \msg owner self -> case msg of
  TacticResolved pk _code target
    | pk == self.controller ->
        handler owner self target
  _ -> pure ()

-- | "When this tactic resolves." Slim version that only exposes
-- 'self' — preferred for tactics that don't pre-declare a target via
-- 'tacticTargets' and don't need the 'owner' record. Use 'withTarget'
-- mid-effect for player picks, and 'playerOf self.controller' if you
-- need the full 'Player'.
whenResolved
  :: (forall m. TriggerM m => TacticContext -> m ())
  -> CardBuilder Tactic ()
whenResolved handler = onReceive $ Receive \msg _owner self -> case msg of
  TacticResolved pk _code _target | pk == self.controller -> handler self
  _ -> pure ()

-- | "When this tactic resolves, with the chosen enemy support
-- resolved off the engine-supplied 'ActionTarget'." Pairs with
-- 'tacticTargets SupportTargetSchema' (the engine prompts for the
-- support at play time). The handler is skipped silently if no
-- enemy support is in play. Used by Demolition!, Da Big Stomp.
onResolveEnemySupport
  :: (forall m. TriggerM m => PlayerKey -> SupportDetails -> m ())
  -> CardBuilder Tactic ()
onResolveEnemySupport handler = onResolve \_owner self target -> do
  g <- getGame
  let pk = self.controller
  whenJust (resolveEnemySupport pk target g) $ handler pk
  where
    resolveEnemySupport pk t g = case t of
      TargetSupport k | Just s <- findSupport k g, s.controller /= pk -> Just s
      _ -> find ((/= pk) . (.controller)) g.supports

-- | "On combat resolve while this in-play card is one of the
-- attackers." Used by approximate "when this unit damages a zone /
-- enemy" cards (Lokhir, Corsairs, Malekith).
onCombatResolveAsAttacker
  :: forall k
   . HasField "key" (InPlay k) UnitKey
  => (forall m. TriggerM m => Player -> InPlay k -> CombatState -> m ())
  -> CardBuilder k ()
onCombatResolveAsAttacker handler = onReceive $ Receive \msg owner self -> case msg of
  ResolveCombat -> withCombat \cs ->
    when (self.key `elem` cs.attackers) $ handler owner self cs
  _ -> pure ()

-- | "When this in-play card is declared as part of an attack."
onMyAttackDeclared
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> ZoneKind -> [UnitKey] -> m ()
     )
  -> CardBuilder k ()
onMyAttackDeclared handler = onReceive $ Receive \msg owner self -> case msg of
  BeginCombat attacker zone attackers
    | attacker == self.controller, self.key `elem` attackers ->
        handler owner self zone attackers
  _ -> pure ()

-- | "When the given action window opens." Used by cards (Vicious
-- Marauder) that force their controller's action choice at a specific
-- phase boundary.
onActionWindow
  :: forall k
   . ActionWindowTrigger
  -> (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onActionWindow which handler = onReceive $ Receive \msg owner self -> case msg of
  OpenActionWindow t | t == which -> handler owner self
  _ -> pure ()

-- | "When this quest's token count changes." Receives the delta. Used
-- by Dominion of Chaos to fire on the third token landing.
onMyQuestTokensAdjusted
  :: ( forall m
      . TriggerM m
     => Player -> QuestDetails -> Int -> m ()
     )
  -> CardBuilder Quest ()
onMyQuestTokensAdjusted handler = onReceive $ Receive \msg owner self -> case msg of
  AdjustQuestTokens qk delta
    | qk == self.key ->
        handler owner self delta
  _ -> pure ()

-- | "When the unit this support is attached to is dealt damage."
-- Skips zero-damage hits and unattached supports.
onHostDamaged
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitKey -> Int -> m ()
     )
  -> CardBuilder Support ()
onHostDamaged handler = onReceive $ Receive \msg owner self -> case msg of
  DealDamageToUnit uk n
    | Just hostKey <- self.attachedTo, uk == hostKey, n > 0 ->
        handler owner self hostKey n
  _ -> pure ()

-- | "When the host of this attachment's controller's turn begins."
-- The host record is resolved from the current game and passed to the
-- handler.
onAttachedHostTurnBegin
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitDetails -> m ()
     )
  -> CardBuilder Support ()
onAttachedHostTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk
    | Just hostKey <- self.attachedTo -> do
        g <- getGame
        case findUnit hostKey g of
          Just host | host.controller == pk -> handler owner self host
          _ -> pure ()
  _ -> pure ()

-- | "Run on every message dispatch this card sees." Used by
-- Northern Wastes' continuous self-check. Avoid unless you really
-- need it; it costs one closure call per message.
onAnyMessage
  :: forall k
   . (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onAnyMessage handler = onReceive $ Receive \_msg owner self -> handler owner self

-- | "Draw a card." Hides the @Draw (Drawing StandardDraw pk)@ ceremony
-- so card bodies read like the printed text.
drawCard :: HasQueue Message m => PlayerKey -> m ()
drawCard pk = push (Draw (Drawing StandardDraw pk))

-- | "Shuffle your deck." Hides the @push (ShuffleDeck pk)@ ceremony.
shuffleDeck :: HasQueue Message m => PlayerKey -> m ()
shuffleDeck pk = push (ShuffleDeck pk)

-- | "Put a facedown development in [zone]." Bumps the development
-- counter on the named zone of the named player's capital.
addDevelopment :: HasQueue Message m => PlayerKey -> ZoneKind -> m ()
addDevelopment pk zone = push (AddDevelopment pk zone)

-- | "Deal N damage to a target unit."
dealDamage :: HasQueue Message m => UnitKey -> Int -> m ()
dealDamage k n = push (DealDamageToUnit k n)

-- | "Deal N damage to a target capital zone."
dealZoneDamage :: HasQueue Message m => PlayerKey -> ZoneKind -> Int -> m ()
dealZoneDamage pk zone n = push (DealDamageToZone pk zone n)

-- | "Heal N damage from your capital." (Volkmar the Grim, Keystone
-- Forge.)
healCapital :: HasQueue Message m => PlayerKey -> Int -> m ()
healCapital pk n = push (HealCapital pk n)

-- | "Heal N damage from a unit." (Trolls regenerate, Bloodsworn.)
healUnit :: HasQueue Message m => UnitKey -> Int -> m ()
healUnit k n = push (HealUnit k n)

-- | "Deal N uncancellable damage to a unit." (Mark of Chaos's turn-
-- start tick, Orc Shaman's self-damage.)
dealUncancellableDamage :: HasQueue Message m => UnitKey -> Int -> m ()
dealUncancellableDamage k n = push (DealDamageToUnitUncancellable k n)

-- | "Deal N damage to this unit at end of turn." Used by tactics
-- that hand out a temporary buff with a delayed bill (Berserk Fury,
-- Crush 'Em).
queueEoTDamage :: HasQueue Message m => UnitKey -> Int -> m ()
queueEoTDamage k n = push (DeferDamageToUnitUntilEoT k n)

-- | "Destroy a quest."
destroyQuest :: HasQueue Message m => UnitKey -> m ()
destroyQuest k = push (DestroyQuest k)

-- | "Discard a random card from this player's hand." Modeling the
-- engine's discard-at-random reaction (Horror of Tzeentch).
discardRandom :: HasQueue Message m => PlayerKey -> m ()
discardRandom pk = push (DiscardRandomFromHand pk)

-- | "Cancel up to N damage assigned to a unit." (Defenders of the
-- Faith.)
cancelDamageOnUnit :: HasQueue Message m => UnitKey -> Int -> m ()
cancelDamageOnUnit k n = push (CancelAssignedDamageOnUnit k n)

-- | "Put a unit into play from hand or discard in the named zone.
-- Origin tells the engine which pile to remove from."
data PlayUnitOrigin = FromHand | FromDiscard

putUnitIntoPlay
  :: HasQueue Message m
  => PlayerKey -> PlayUnitOrigin -> UnitKey -> ZoneKind -> m ()
putUnitIntoPlay pk origin uk z = push $ case origin of
  FromHand -> PutUnitIntoPlay pk uk z
  FromDiscard -> PutUnitIntoPlayFromDiscard pk uk z

-- | "Place or remove N resource tokens on this support card."
adjustSupportTokens :: HasQueue Message m => UnitKey -> Int -> m ()
adjustSupportTokens k n = push (AdjustSupportTokens k n)

-- | "Gain N resources." (Burying the Grudge.)
gainResources :: HasQueue Message m => PlayerKey -> Int -> m ()
gainResources pk n = push (GainResources pk n)

-- | "Move all damage from src to dst." (Stubborn Refusal, Valkia.)
moveAllDamage :: HasQueue Message m => UnitKey -> UnitKey -> m ()
moveAllDamage src dst = push (MoveAllDamage src dst)

-- | "Place / remove N resource tokens on this quest."
addQuestToken :: HasQueue Message m => UnitKey -> Int -> m ()
addQuestToken k n = push (AdjustQuestTokens k n)

-- | "Destroy a unit."
destroyUnit :: HasQueue Message m => UnitKey -> m ()
destroyUnit k = push (DestroyUnit k)

-- | "Destroy a support card or development."
destroySupport :: HasQueue Message m => UnitKey -> m ()
destroySupport k = push (DestroySupport k)

-- | "Corrupt a target unit."
corrupt :: HasQueue Message m => UnitKey -> m ()
corrupt k = push (CorruptUnit k)

-- | "Spend N resources."
payResources :: HasQueue Message m => PlayerKey -> Int -> m ()
payResources pk n = push (SpendResources pk n)

-- | "Draw N cards."
drawCards :: HasQueue Message m => PlayerKey -> Int -> m ()
drawCards pk n = replicateM_ n (drawCard pk)

-- | "Deal N damage to each enemy unit (of the given controller) in
-- this zone."
damageEachEnemyInZone
  :: HasQueue Message m => PlayerKey -> ZoneKind -> Int -> m ()
damageEachEnemyInZone pk z n =
  push (DealDamageToEachEnemyUnitInZone pk z n)

-- | "Deal N damage to each attacking and each defending unit."
damageEachUnitInCombat :: HasQueue Message m => Int -> m ()
damageEachUnitInCombat n = push (DealDamageToEachUnitInCombat n)

-- | "Attach <departing card> facedown to this unit as an experience."
-- Mirror of 'destroyUnit' for Skulltaker-style trigger bodies.
attachExperience :: HasQueue Message m => UnitKey -> CardCode -> m ()
attachExperience hostKey code =
  push (AttachExperience hostKey code)

-- ---------------------------------------------------------------------
-- Composite prompt helpers
--
-- These wrap multi-step prompt + effect patterns that recur across
-- many cards (pay-to-do-X, reveal-from-hand-then-do-X, "each player
-- must …", forced sacrifice). Each is a thin shell over 'askPrompt'
-- so card bodies can state intent in one line.
-- ---------------------------------------------------------------------

-- | "You may spend N resources to do X." Gates on actually having N
-- resources, asks a yes/no, debits on confirm, then runs the body.
--
-- > mayPay self.controller 1 "Spend 1 resource to attach this unit?" do
-- >   attachExperience self.key code
mayPay
  :: (HasGame m, HasPromptIO m, HasQueue Message m)
  => PlayerKey -> Int -> Text -> m () -> m ()
mayPay pk n prompt body = do
  me <- playerOf pk <$> getGame
  let Resources r = me.resources
  when (r >= n) $
    may pk prompt do
      payResources pk n
      body

-- | "Reveal a card matching this predicate from your hand to do X."
-- Skips silently if the player has no matching card. The reveal is
-- modeled today as a 1-card pick from the matching subset; the body
-- receives the revealed 'Card' so cards that care about its identity
-- (e.g. its name in a log message) can access it.
--
-- > revealFromHand self.controller chaosCard
-- >   "Reveal a Chaos legend or unit to deal 2 damage." \_revealed ->
-- >     withTarget self.controller AnyUnit \k -> dealDamage k 2
revealFromHand
  :: (HasGame m, HasPromptIO m)
  => PlayerKey -> (Card -> Bool) -> Text -> (Card -> m ()) -> m ()
revealFromHand pk pred desc body = do
  me <- playerOf pk <$> getGame
  let candidates = filter pred me.hand
  unless (null candidates) $
    chooseFromCards pk 0 1 candidates desc \chosen ->
      for_ chosen body

-- | "Each player does X." Runs the body once for each 'PlayerKey' in
-- turn-priority order (Player1 first), so prompts emitted by the body
-- arrive deterministically.
eachPlayer :: Applicative f => (PlayerKey -> f ()) -> f ()
eachPlayer body = body Player1 *> body Player2

-- | "Sacrifice one of your units in this zone." Forced (non-optional)
-- prompt; if the player has no unit there the prompt resolves with no
-- pick and the effect is silently a no-op (matching the rulebook:
-- "if able"). Used by Bloodthirster, Zhufbar Engineers.
mustSacrificeInZone
  :: (HasGame m, HasPromptIO m, HasQueue Message m)
  => PlayerKey -> ZoneKind -> Text -> m ()
mustSacrificeInZone pk zone desc = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseSacrifice { zone, optional = False, description = desc }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits (chosen : _) ->
      withUnit chosen \u ->
        when (u.controller == pk) $ destroyUnit chosen
    _ -> pure ()

-- | True iff the named card carries the given race. Looks the card up
-- in 'allCards'; returns 'False' if the code is unknown (defensive —
-- should never happen for in-play card codes).
hasRace :: Race -> CardCode -> Bool
hasRace r code = case Map.lookup code allCards of
  Just sd -> isRace sd r
  Nothing -> False

-- | Anything that carries a race list. Used by 'isRace' so card
-- bodies can write @unit \`isRace\` Dwarf@ uniformly across card
-- definitions and in-play / departed records.
class HasRaces a where
  racesOf :: a -> [Race]

instance HasRaces SomeCardDef where
  racesOf = \case
    UnitCardDef cd -> cd.races
    SupportCardDef cd -> cd.races
    QuestCardDef cd -> cd.races
    TacticCardDef cd -> cd.races
    LegendCardDef cd -> cd.races

instance HasRaces (CardDef k) where
  racesOf cd = cd.races

instance HasRaces UnitDetails where
  racesOf u = u.cardDef.races

instance HasRaces SupportDetails where
  racesOf s = s.cardDef.races

instance HasRaces QuestDetails where
  racesOf q = q.cardDef.races

instance HasRaces LegendDetails where
  racesOf l = l.cardDef.races

instance HasRaces DepartedUnit where
  racesOf du = du.cardDef.races

-- | @x \`isRace\` r@ — does the value carry the given race?
isRace :: HasRaces a => a -> Race -> Bool
isRace x r = r `elem` racesOf x

-- | "May …" — yes/no prompt that gates an effect. Mirrors card text
-- "You may put that card into this zone."
--
-- > may pk "Put that card into this zone?" $
-- >   playSupportFromDeck pk card.key zone
may
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Text -> m () -> m ()
may pk prompt action = do
  yes <- askYesNo pk prompt
  when yes action

-- | "Sacrifice one of your units." Prompts the firing player for a
-- unit they control, destroys it, and runs the continuation with
-- the sacrificed unit's key. The continuation is skipped if no
-- valid sacrifice is chosen.
--
-- > sacrificeOwnUnit pk "Sacrifice a unit." \_sacrificed -> ...
sacrificeOwnUnit
  :: (HasPromptIO m, HasGame m, HasQueue Message m)
  => PlayerKey -> Text -> (UnitKey -> m ()) -> m ()
sacrificeOwnUnit pk desc cont = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseUnits
        { filterSpec = AnyOwnUnit
        , minPick = 1
        , maxPick = 1
        , description = desc
        }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits (chosen : _) -> do
      destroyUnit chosen
      cont chosen
    _ -> pure ()

-- | "Pick up to N from these specific units." Fires a prompt
-- restricted to the supplied candidate list (typically computed by
-- the card — e.g. "the attacking units" / "the units in this
-- zone"). Player may pick 0..N; the continuation runs with whatever
-- they chose (possibly empty).
--
-- > chooseUpTo pk 2 cs.attackers \chosen ->
-- >   traverse_ destroyUnit chosen
chooseUpTo
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Int -> [UnitKey] -> ([UnitKey] -> m ()) -> m ()
chooseUpTo pk maxN candidates k = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseUnits
        { filterSpec = UnitsFromList candidates
        , minPick = 0
        , maxPick = maxN
        , description =
            "Choose up to " <> tshow maxN <> " unit"
              <> (if maxN == 1 then "" else "s") <> "."
        }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits chosen -> k chosen
    _ -> k []

-- | "Pick between min and max cards from this list." Used when the
-- candidate set is a list of cards (not in-play unit keys) — e.g.
-- "search the top five cards of your deck for a support card with
-- cost 2 or lower". The engine embeds the actual card data in the
-- prompt so the prompted player's client can render the choices.
--
-- > chooseFromCards pk 0 1 matches "Pick a support to put into play."
-- >   \chosen -> for_ chosen \c -> playSupportFromDeck pk c.key zone
chooseFromCards
  :: (HasPromptIO m, HasGame m)
  => PlayerKey
  -> Int
  -> Int
  -> [Card]
  -> Text
  -> ([Card] -> m ())
  -> m ()
chooseFromCards pk minN maxN cards desc k = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseFromCards
        { cards
        , minPick = minN
        , maxPick = maxN
        , description = desc
        }
    , callback = CallbackInlinePrompt
    }
  let chosen = case answer of
        PickUnits keys -> [c | c <- cards, c.key `elem` keys]
        _ -> []
  k chosen

-- ---------------------------------------------------------------------
-- "Then" chains
--
-- Card text often uses "Then" to chain effects: "Effect A. Then, effect
-- B." Per the rules, B fires only if A resolved successfully. For
-- simple cases the preceding step returns 'Bool' and 'then_' gates
-- the chain. Step verbs that own their own block (like
-- 'searchTopOfDeck') gate the body on resolution internally — the
-- body itself is the "Then" chain.
-- ---------------------------------------------------------------------

-- | "Then, …" chain. Fires the chained body only if the preceding
-- step reports success ('True').
then_ :: Applicative f => Bool -> f () -> f ()
then_ = when

-- ---------------------------------------------------------------------
-- Searching the deck
--
-- 'searchTopOfDeck' runs a callback over the top N cards of a
-- player's deck. The callback receives a 'SearchResult' handle
-- exposing the cards. The body runs only if the search itself
-- resolved (i.e. no interrupt prevented it), so anything written
-- after the find verbs IS the card text's "Then" chain.
-- ---------------------------------------------------------------------

-- | Handle threaded through a 'searchTopOfDeck' block. Today carries
-- just the cards looked at; future fields can carry depth searched,
-- which matched, etc.
data SearchResult = SearchResult
  { cards :: [Card]
  }

-- | "On finding a support card matching the predicate, run the
-- callback." Looks at the supplied card list (typically
-- @result.cards@) and fires 'action' with the first match. Does
-- nothing if no match exists.
onFindSupport
  :: Monad m
  => (CardDef Support -> Bool)
  -> [Card]
  -> (Card -> m ())
  -> m ()
onFindSupport pred cards action =
  whenJust (pickSupportFrom cards pred) action

-- | "Search the top N cards of pk's deck." Runs the body with a
-- 'SearchResult' handle. The body fires only if the search resolved
-- (today: always; future: gated on interrupt state once we model
-- "cannot search" effects), so any statement after the find verbs
-- inside the body is the card text's "Then …" chain.
searchTopOfDeck
  :: HasGame m
  => PlayerKey -> Int -> (SearchResult -> m ()) -> m ()
searchTopOfDeck pk n body = do
  let resolved = True  -- TODO: gate on interrupt state once we have it
  when resolved $ do
    player <- getPlayer pk
    body SearchResult {cards = take n player.deck}

-- | "Put that support card into play [in the given zone]." Plays the
-- named support directly from the player's deck.
playSupportFromDeck
  :: HasQueue Message m
  => PlayerKey -> UnitKey -> ZoneKind -> m ()
playSupportFromDeck pk key zone = push (PlaySupportFromDeck pk key zone)

-- | "Ask the named player a yes/no question and return their answer."
-- Defaults to 'False' if no client is attached (test / debug paths).
askYesNo
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Text -> m Bool
askYesNo pk description = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseYesNo {description}
    , callback = CallbackInlinePrompt
    }
  pure $ case answer of
    PickBool True -> True
    _ -> False

-- | Predicate over a card definition: does its printed cost meet the
-- given cap? 'Variable' costs are treated as failing the predicate
-- (no card in the core set uses Variable on support, but be safe).
costAtMost :: Int -> CardDef k -> Bool
costAtMost n cd = case cd.cost of
  Fixed v -> v <= n
  _ -> False

-- | "Pick a support card from this list matching a predicate." Used
-- when searching the top of a deck (or any card list) for a support.
-- Returns the first match, or 'Nothing'.
pickSupportFrom :: [Card] -> (CardDef Support -> Bool) -> Maybe Card
pickSupportFrom cs p = listToMaybe (filterSupportsIn cs p)

-- | All supports in the list matching a predicate over their card
-- definition. Used when the player should pick from every matching
-- candidate (e.g. "search ... for a support card with cost 2 or
-- lower").
filterSupportsIn :: [Card] -> (CardDef Support -> Bool) -> [Card]
filterSupportsIn cs p =
  [ c
  | c <- cs
  , Just cd <- [asSupport c.def]
  , p cd
  ]

-- | A modifier declaration that's not yet been installed. Produced by
-- verbs like 'buffPower' and consumed by 'until', so card bodies read
-- like the printed text:
--
-- > until EndOfTurn $ buffPower target 1
data PendingBuff = PendingBuff UnitKey ModifierDetails

-- | "Target unit gains +N power." Produces a 'PendingBuff' that's
-- installed by wrapping with 'until' to pick a scope.
buffPower :: UnitKey -> Int -> PendingBuff
buffPower target n = PendingBuff target (GainPower n)

-- | Install a 'PendingBuff' for the named 'ModifierScope'. Mirrors the
-- card text "until end of turn" / "while X is in play".
--
-- > until EndOfTurn $ buffPower target 1
until :: HasQueue Message m => ModifierScope -> PendingBuff -> m ()
until scope (PendingBuff target details) =
  push (InstallModifier (UnitRef target) (Modifier details scope))

-- ---------------------------------------------------------------------
-- Extras builders
--
-- The engine reads kind-specific 'extras' fields instead of casing on a
-- card's 'code'. Each helper below sets a single slice; absence keeps
-- the corresponding default from 'defaultExtras' (no-op for that
-- behavior).
-- ---------------------------------------------------------------------

modifyUnitExtras :: (UnitExtras -> UnitExtras) -> CardBuilder Unit ()
modifyUnitExtras f =
  modify \cd -> cd {extras = f cd.extras}

modifySupportExtras :: (SupportExtras -> SupportExtras) -> CardBuilder Support ()
modifySupportExtras f =
  modify \cd -> cd {extras = f cd.extras}

-- | Game-state-derived self power bonus (Troll Slayers, Korhil, …).
selfPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
selfPower f = modifyUnitExtras \e -> e {selfPowerBonus = f}

-- | Extra combat damage this unit deals (Lord of Khorne, Gorbad).
combatPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
combatPower f = modifyUnitExtras \e -> e {combatPowerBonus = f}

-- | Power this unit grants to other in-play units (Karl Franz, Templar
-- of Sigmar). Args to @f@: game, this unit, target unit.
unitAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
unitAura f = modifyUnitExtras \e -> e {unitAuraPower = f}

-- | Predicate gating attacker eligibility (Sworn of Khorne).
canAttack :: (Game -> PlayerKey -> ZoneKind -> UnitDetails -> Bool) -> CardBuilder Unit ()
canAttack f = modifyUnitExtras \e -> e {canAttackZone = f}

-- | Per-turn damage cap (Daemonettes of Slaanesh).
perTurnDamageCap :: Int -> CardBuilder Unit ()
perTurnDamageCap n = modifyUnitExtras \e -> e {damageCap = Just n}

-- | Mark the unit as corrupting any enemy it deals non-zero combat
-- damage to (Plaguebearers of Nurgle, Beasts of Nurgle).
corruptsOnDamage :: CardBuilder Unit ()
corruptsOnDamage = modifyUnitExtras \e -> e {corruptsOnCombatDamage = True}

-- | Extra resources a non-controller must spend to target this unit
-- (King Kazador).
targetTax :: (Game -> PlayerKey -> UnitDetails -> Int) -> CardBuilder Unit ()
targetTax f = modifyUnitExtras \e -> e {extraTargetTax = f}

-- | Multiplier on ALL applied damage while this unit is in play
-- (Bloodletter).
damageMultiplier :: Int -> CardBuilder Unit ()
damageMultiplier n = modifyUnitExtras \e -> e {damageMultiplierWhileInPlay = n}

-- | Self-cost adjustment when playing this card (Bloodcrusher: -1
-- per burning zone). May be negative.
selfCostAdjust :: (Game -> PlayerKey -> Int) -> CardBuilder k ()
selfCostAdjust f = modify \cd -> cd {selfCostAdjustment = f}

-- | Per-card playability predicate. The engine refuses to play this
-- card if the predicate returns 'False'. Stacks with the engine's
-- baseline checks (resources, unique, Limited).
--
-- > playableWhen \g _pk ->
-- >   any (\u -> ... source candidate ...) g.units
playableWhen :: (Game -> PlayerKey -> Bool) -> CardBuilder k ()
playableWhen pred = modify \cd -> cd {canPlay = pred}

-- | Static power contribution while attached (Daemonsword, Banner of
-- Sigmar, …).
attachmentPower :: Int -> CardBuilder Support ()
attachmentPower n = modifySupportExtras \e -> e {attachmentPowerBonus = n}

-- | Static HP contribution while attached (Daemonsword).
attachmentHp :: Int -> CardBuilder Support ()
attachmentHp n = modifySupportExtras \e -> e {attachmentHPBonus = n}

-- | While attached, the host unit's combat damage is uncancellable
-- (Hammer of Sigmar).
grantsUncancellable :: CardBuilder Support ()
grantsUncancellable =
  modifySupportExtras \e -> e {grantsUncancellableDamage = True}

-- | Power this support grants to a unit (Iron Tower, Cauldron of
-- Blood, Da Bad Moon static slice).
supportAura :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportAura f = modifySupportExtras \e -> e {supportAuraPower = f}

-- | Extra combat damage this support adds to a unit (Rift of Battle,
-- Organ Gun while defending, Da Bad Moon and Big Boss's Banner for Orc
-- attackers).
supportCombat :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportCombat f = modifySupportExtras \e -> e {supportCombatBonus = f}

-- | "Attach to a target unit. Attached unit gains …" Runs the body
-- each engine recompute with the support and the unit it's attached
-- to; 'gainPower unit n' (and future buff verbs) emit contributions
-- that flow into the attached unit's effective stats. Body fires
-- only while attached.
--
-- > organGun = supportCard "core-015" "Organ Gun" do
-- >   ...
-- >   attachedTo \_self unit ->
-- >     when unit.defending $ gainPower unit 2
attachedTo
  :: (SupportDetails -> UnitDetails -> EffectM ())
  -> CardBuilder Support ()
attachedTo body = modifySupportExtras \e -> e
  { supportAuraPower = \g s u ->
      let prev = e.supportAuraPower g s u
          extra = case s.attachedTo of
            Just k | k == u.key ->
              activeBonusPower (execEffectM g (body s u))
            _ -> 0
       in prev + extra
  }

-- | Bonus power this support contributes to a zone of its controller
-- (Lighthouse of Lothern, Rift of Chaos).
zonePowerAura :: (Game -> SupportDetails -> ZoneKind -> Int) -> CardBuilder Support ()
zonePowerAura f = modifySupportExtras \e -> e {zonePowerBonus = f}

-- | Cost-of-play adjustment this support imposes on other cards being
-- played (Imperial Crown, Master Rune of Dismay).
globalCostAdjust
  :: (Game -> SupportDetails -> PlayerKey -> CardCodeFilter -> Int)
  -> CardBuilder Support ()
globalCostAdjust f = modifySupportExtras \e -> e {globalCostAdjustment = f}

-- | Mark the printed Rune of Fortitude effect on this support.
imposesRuneOfFortitudeTax :: CardBuilder Support ()
imposesRuneOfFortitudeTax =
  modifySupportExtras \e -> e {runeOfFortitudeTax = True}

-- | Append an 'ActionDef' to the card's static action list. Multiple
-- actions can be declared; the engine surfaces them to the client by
-- index. Used directly by cards that build a record-style action; the
-- slim positional 'action' verb (declared in an 'EffectM' block) is
-- the preferred entry point.
actionDef :: ActionDef k -> CardBuilder k ()
actionDef a = modify \cardDef -> cardDef {actions = cardDef.actions ++ [a]}

-- | Convenience builder for declaring a tactic's target schema. The
-- effect closure isn't used for tactics — the engine fires the
-- card's 'receive' with 'TacticResolved'; the action metadata is
-- there purely for client display and target validation.
tacticTargets :: TargetSchema -> CardBuilder Tactic ()
tacticTargets schema = actionDef ActionDef
  { actionName = "Play"
  , actionCost = 0  -- the actual cost lives on CardDef.cost
  , actionExtraCosts = []
  , actionTarget = schema
  , availableInZone = Nothing
  , actionEffect = ActionEffect \_usage -> pure ()
  }

-- | First in-play unit controlled by an opponent of 'pk'. Used as an
-- | Look up an in-play unit by its 'UnitKey'. Mirror of the Engine-side
-- helper so card receive bodies can resolve their attachment hosts
-- without importing 'Invasion.Engine'.
findUnit :: UnitKey -> Game -> Maybe UnitDetails
findUnit k g = find ((== k) . (.key)) g.units

-- | Continuation-style unit lookup. Mirror of 'withQuest'.
withUnit
  :: HasGame m => UnitKey -> (UnitDetails -> m ()) -> m ()
withUnit k k' = do
  g <- getGame
  whenJust (findUnit k g) k'

-- | Look up an in-play free-standing support by key.
findSupport :: UnitKey -> Game -> Maybe SupportDetails
findSupport k g = find ((== k) . (.key)) g.supports

-- | Continuation-style support lookup. Mirror of 'withQuest'.
withSupport
  :: HasGame m => UnitKey -> (SupportDetails -> m ()) -> m ()
withSupport k k' = do
  g <- getGame
  whenJust (findSupport k g) k'

-- | Look up an in-play quest by key. Pure form for use inside list
-- comprehensions, guards, and other pure contexts.
findQuest :: UnitKey -> Game -> Maybe QuestDetails
findQuest k g = find ((== k) . (.key)) g.quests

-- | Continuation-style quest lookup. Runs the body with the quest
-- record if it exists; skips silently otherwise. Reads game state
-- from the surrounding monad so card bodies don't need an explicit
-- @g <- getGame@.
--
-- > withQuest self.key \q ->
-- >   when (isJust q.questingUnit) $ addQuestToken self.key 1
withQuest
  :: HasGame m => UnitKey -> (QuestDetails -> m ()) -> m ()
withQuest k k' = do
  g <- getGame
  whenJust (findQuest k g) k'

-- | Look up an in-play legend by key.
findLegend :: UnitKey -> Game -> Maybe LegendDetails
findLegend k g = find ((== k) . (.key)) g.legends

-- | Continuation-style legend lookup. Mirror of 'withQuest'.
withLegend
  :: HasGame m => UnitKey -> (LegendDetails -> m ()) -> m ()
withLegend k k' = do
  g <- getGame
  whenJust (findLegend k g) k'

-- | All in-play units sitting in the given zone. Reads live game
-- state via 'HasGame'.
unitsInZone :: HasGame m => ZoneKind -> m [UnitDetails]
unitsInZone z = do
  g <- getGame
  pure [u | u <- g.units, u.zone == z]

-- ---------------------------------------------------------------------
-- Playability helpers
--
-- Boolean predicates over 'Game' + 'PlayerKey' that mirror the
-- "Action: …" preconditions printed on tactic cards. Used with
-- 'playableWhen' so the engine refuses to play a tactic that can't
-- meaningfully resolve (no valid target, wrong phase, …).
-- ---------------------------------------------------------------------

-- | Has this unit taken any damage?
isDamaged :: UnitDetails -> Bool
isDamaged u = let Damage d = u.damage in d > 0

-- | Other units sitting in the same zone as the given unit (the unit
-- itself is excluded).
peersInZoneOf :: Game -> UnitDetails -> [UnitDetails]
peersInZoneOf g u = [v | v <- g.units, v.zone == u.zone, v.key /= u.key]

-- | Has the given unit at least one peer in its zone?
hasPeerInZone :: Game -> UnitDetails -> Bool
hasPeerInZone g u = not (null (peersInZoneOf g u))

-- | Is there a combat in progress?
inCombat :: Game -> PlayerKey -> Bool
inCombat g _pk = isJust g.combat

-- | Is there a combat in progress with at least one combatant on this
-- player's side? Attackers if they're attacking, defenders if
-- they're defending.
hasFriendlyCombatant :: Game -> PlayerKey -> Bool
hasFriendlyCombatant g pk = case g.combat of
  Just cs
    | cs.attackingPlayer == pk -> not (null cs.attackers)
    | cs.defendingPlayer == pk -> not (null cs.defenders)
  _ -> False

-- | Is there a combat in progress where the opponent is attacking and
-- has at least one attacker on the board?
hasEnemyAttacker :: Game -> PlayerKey -> Bool
hasEnemyAttacker g pk = case g.combat of
  Just cs | cs.attackingPlayer /= pk -> not (null cs.attackers)
  _ -> False

-- | Does the opponent control any support card in play?
hasEnemySupport :: Game -> PlayerKey -> Bool
hasEnemySupport g pk = any (\s -> s.controller /= pk) g.supports

-- | Does the player's deck have at least N cards?
hasDeckSize :: Int -> Game -> PlayerKey -> Bool
hasDeckSize n g pk = length (playerOf pk g).deck >= n

-- | Does the player have at least one non-burned development zone
-- (kingdom or battlefield)?
canDevelop :: Game -> PlayerKey -> Bool
canDevelop g pk =
  let me = playerOf pk g
   in not me.capital.kingdom.burned || not me.capital.battlefield.burned

-- | "If a combat is in progress, run this body with the combat
-- state." Convenience wrapper that hides the @g <- getGame; for_
-- g.combat@ boilerplate.
--
-- > withCombat \cs ->
-- >   when (cs.attackingPlayer /= pk) $
-- >     traverse_ destroyUnit (take 2 cs.attackers)
withCombat :: HasGame m => (CombatState -> m ()) -> m ()
withCombat k = do
  g <- getGame
  for_ g.combat k

-- | The legend currently in play for the given player, if any. Each
-- player may control at most one legend at a time.
legendOf :: PlayerKey -> Game -> Maybe LegendDetails
legendOf pk g = find ((== pk) . (.controller)) g.legends

-- | Total number of currently-burning zones across both capitals. Used
-- by Chaos cards that scale with burning (Bloodcrusher, Lord of Khorne,
-- Rift of Chaos, Durgnar).
burningZoneCount :: Game -> Int
burningZoneCount g =
  length
    [ ()
    | p <- [g.player1, g.player2]
    , z <- p.capital.zones
    , z.burning
    ]

-- | Number of facedown developments in the named unit's zone. Read by
-- Toughness X and a couple of dev-scaling cards (Troll Slayers,
-- Ironbreakers of Ankhor).
devsInZone :: Game -> UnitDetails -> Int
devsInZone g u =
  let player = case u.controller of
        Player1 -> g.player1
        Player2 -> g.player2
      Developments d = case u.zone of
        KingdomZone -> player.capital.kingdom.developments
        QuestZone -> player.capital.quest.developments
        BattlefieldZone -> player.capital.battlefield.developments
   in d

-- | True iff any section of @pk@'s capital is currently burning.
controllerBurning :: Game -> PlayerKey -> Bool
controllerBurning g pk =
  let p = case pk of
        Player1 -> g.player1
        Player2 -> g.player2
   in any (.burning) p.capital.zones

-- | True iff the unit is currently declared as an attacker in the
-- in-flight combat. Returns False outside combat.
unitIsAttacking :: Game -> UnitDetails -> Bool
unitIsAttacking g u = maybe False (elem u.key . (.attackers)) g.combat

-- | True iff the unit is currently declared as a defender. Returns False
-- outside combat.
unitIsDefending :: Game -> UnitDetails -> Bool
unitIsDefending g u = maybe False (elem u.key . (.defenders)) g.combat

-- | Every in-play support, whether free-standing in 'Game.supports' or
-- attached to a unit. Used when an effect needs to consult every
-- support regardless of attachment status.
allInPlaySupports :: Game -> [SupportDetails]
allInPlaySupports g = g.supports ++ concatMap (.attachments) g.units

-- ---------------------------------------------------------------------
-- Zone-gated static effects
--
-- Cards with a "Battlefield." / "Kingdom." / "Quest." prefix scope a
-- static (always-on) effect to the named zone. The builders below give
-- those prefixes their own keyword in the DSL: each takes a body that
-- runs in 'EffectM', a tiny builder monad that accumulates static
-- contributions (today: just bonus power) and only fires while the
-- host card sits in the matching zone.
-- ---------------------------------------------------------------------

-- | Builder monad for static-effect declarations. Wraps a Writer over
-- 'ActiveEffect' with read access to the current 'Game'; verbs like
-- 'gainPower' emit contributions, and lookup helpers like 'zoneOf'
-- read fresh game state on demand.
newtype EffectM a = EffectM (ReaderT Game (Writer ActiveEffect) a)
  deriving newtype (Functor, Applicative, Monad)

instance HasGame EffectM where
  getGame = EffectM ask

-- | Drain an 'EffectM' block against the current game to the
-- 'ActiveEffect' it produced.
execEffectM :: Game -> EffectM () -> ActiveEffect
execEffectM g (EffectM r) = execWriter (runReaderT r g)

-- | "This unit gains +N power." Emits a power-bonus contribution
-- inside an 'EffectM' block. The 'self' argument mirrors the printed
-- card text ("This unit ...") and is not consulted internally — the
-- surrounding host already knows which unit it's evaluating.
gainPower :: UnitDetails -> Int -> EffectM ()
gainPower _self n = EffectM (tell (ActiveEffect (Sum n)))

-- | The owner of an in-play card, as a full 'Player' record.
playerOf :: PlayerKey -> Game -> Player
playerOf Player1 g = g.player1
playerOf Player2 g = g.player2

-- | Non-gated constant block. The body fires every engine tick
-- regardless of which zone this unit is in. Use for "this unit gains
-- X while CONDITION" effects whose condition isn't zone-specific
-- (Durgnar the Bold, …).
--
-- > effects \self owner ->
-- >   when (capitalBurning owner) $ gainPower self 2
effects
  :: (UnitDetails -> Player -> EffectM ())
  -> CardBuilder Unit ()
effects body = modifyUnitExtras \e -> e
  { runtimeEffects = \g self ->
      let prev = e.runtimeEffects g self
          owner = playerOf self.controller g
       in prev <> execEffectM g (body self owner)
  }

-- ---------------------------------------------------------------------
-- Card-body verbs
--
-- 'forced', 'triggered', 'action', and 'constant' are top-level
-- CardBuilder verbs that mirror the printed card-text categories.
-- Outside a zone wrapper they register their effects unconditionally;
-- inside 'battlefield' / 'quest' / 'kingdom' the wrapper save/restores
-- the registrations and applies a zone gate.
-- ---------------------------------------------------------------------

-- | Read access to the current 'Message' from inside a 'forced'
-- body. Event-dispatch verbs (`onTurnBegin`, `onFriendlyUnitLeave`,
-- …) consult this to decide whether to fire.
class Monad m => HasMessage m where
  getMessage :: m Message

-- | Carrier monad for a 'forced' body. Adds read access to the
-- current 'Message' over the underlying trigger monad while
-- forwarding every other engine capability.
newtype ForcedT m a = ForcedT (ReaderT Message m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (ForcedT m) where
  liftIO = ForcedT . liftIO

instance HasGame m => HasGame (ForcedT m) where
  getGame = ForcedT (lift getGame)

instance HasQueue msg m => HasQueue msg (ForcedT m) where
  getQueue = ForcedT (lift getQueue)

instance HasPromptIO m => HasPromptIO (ForcedT m) where
  askPrompt = ForcedT . lift . askPrompt

instance Monad m => HasMessage (ForcedT m) where
  getMessage = ForcedT ask

runForcedT :: ForcedT m a -> Message -> m a
runForcedT (ForcedT r) = runReaderT r

-- | "Forced: …" registration. The body runs every time the host
-- card's 'Receive' is dispatched; inside, event-dispatchers like
-- 'onTurnBegin' / 'onFriendlyUnitLeave' decide whether to fire based
-- on the current 'Message'. This is the only place 'getMessage' is
-- in scope.
--
-- > kingdom $ forced \self ->
-- >   onTurnBegin self.controller $
-- >     healCapital self.controller 1
forced
  :: forall k.
     ( forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m, HasMessage m)
      => InPlay k -> m ()
     )
  -> CardBuilder k ()
forced body = onReceive $ Receive \msg _owner self ->
  runForcedT (body self) msg

-- | "Action: When …" marker for action-triggered abilities. Currently
-- a documentation alias.
triggered :: CardBuilder k () -> CardBuilder k ()
triggered = id

-- ---------------------------------------------------------------------
-- Event dispatchers (used inside 'forced' bodies)
-- ---------------------------------------------------------------------

-- | "At the beginning of [pk]'s turn, …" Runs the action only when
-- the current message is 'BeginTurn pk'.
onTurnBegin :: HasMessage m => PlayerKey -> m () -> m ()
onTurnBegin pk action = do
  msg <- getMessage
  case msg of
    BeginTurn k | k == pk -> action
    _ -> pure ()

-- | "At the end of [pk]'s turn, …"
onTurnEnd :: HasMessage m => PlayerKey -> m () -> m ()
onTurnEnd pk action = do
  msg <- getMessage
  case msg of
    EndTurn k | k == pk -> action
    _ -> pure ()

-- | "When one of [pk]'s units leaves play, …" Body receives a
-- 'DepartedUnit' snapshot. Card text that says "one of your OTHER
-- units" should additionally guard on @unit.key /= self.key@ inside
-- the body.
onUnitOfLeavesPlay
  :: HasMessage m
  => PlayerKey -> (DepartedUnit -> m ()) -> m ()
onUnitOfLeavesPlay pk body = do
  msg <- getMessage
  case msg of
    UnitLeftPlay du | du.controller == pk -> body du
    _ -> pure ()

-- | "Action: …" top-level verb. Registers a card action; by default
-- 'availableInZone' is 'Nothing' (always available). Inside a zone
-- wrapper the wrapper patches it to 'Just z'. The effect body
-- receives an 'ActionUsage' record with the firing player, this
-- card, the resolved target, and (empty for plain 'action') the
-- payment receipts.
action
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> m ()
     )
  -> CardBuilder k ()
action name cost effect = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionExtraCosts = []
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionEffect = ActionEffect effect
  }

-- | Action with non-resource extra costs (sacrifice, discard, …) in
-- addition to a resource cost. The engine validates every extra cost
-- before firing and prompts for any choices needed to pay them; if
-- any extra cost can't be paid the action is rejected before
-- resources are spent. The effect body receives an 'ActionUsage'
-- carrying receipts for the paid extras.
--
-- > battlefield $ actionWith "Volley" 1 [SacrificeUnit] \usage -> ...
actionWith
  :: Text
  -> Int
  -> [ExtraCost]
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> m ()
     )
  -> CardBuilder k ()
actionWith name cost extras effect = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionExtraCosts = extras
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionEffect = ActionEffect effect
  }

-- | "Action: ... target enemy unit." Engine-side target prompt:
-- the player is asked for an enemy unit at activation time, BEFORE
-- the resource cost is paid (so the action can't be activated when
-- no valid target exists). The body receives the chosen 'UnitKey'.
--
-- > actionEnemyUnit "Shoot" 1 \_u k -> dealDamage k 1
actionEnemyUnit, actionFriendlyUnit, actionAnyUnit
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> UnitKey -> m ()
     )
  -> CardBuilder k ()
actionEnemyUnit name cost body =
  actionUnitSchema name cost EnemyUnitTargetSchema body
-- | "Action: ... target unit you control." See 'actionEnemyUnit'.
actionFriendlyUnit name cost body =
  actionUnitSchema name cost FriendlyUnitTargetSchema body
-- | "Action: ... target unit (any player)." See 'actionEnemyUnit'.
actionAnyUnit name cost body =
  actionUnitSchema name cost AnyUnitTargetSchema body

-- | Shared implementation for 'actionEnemyUnit' / 'actionFriendlyUnit'
-- / 'actionAnyUnit'. Pins the schema, leaves zone gate and extra
-- costs at defaults.
actionUnitSchema
  :: Text -> Int -> TargetSchema
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> UnitKey -> m ()
     )
  -> CardBuilder k ()
actionUnitSchema name cost schema body = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionTarget = schema
  , availableInZone = Nothing
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u -> case u.target of
      TargetUnit k -> body u k
      _ -> pure ()
  }

-- | "Action: ... target enemy zone." Engine prompts for an opposing
-- capital zone before activation; body receives the @(owner, zone)@
-- pair so card bodies can route messages either at the owner or at
-- the activating player.
actionEnemyZone
  :: Text -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> (PlayerKey, ZoneKind) -> m ()
     )
  -> CardBuilder k ()
actionEnemyZone name cost body = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionTarget = EnemyZoneTargetSchema
  , availableInZone = Nothing
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u -> case u.target of
      TargetZone owner z -> body u (owner, z)
      _ -> pure ()
  }

-- | "Forced: At the beginning of your turn, place 1 resource token
-- on this card if a unit is questing here." The whole-line idiom
-- shared by every token-payoff quest (Defending the Empire, The
-- White Tower, Slaughter at Lustria, Greenskin Rush).
--
-- > forced \self -> accrueTokenWhileQuesting self
accrueTokenWhileQuesting
  :: ( HasMessage m, HasGame m, HasQueue Message m )
  => InPlay Quest -> m ()
accrueTokenWhileQuesting self =
  onTurnBegin self.controller $
    withQuest self.key \q ->
      when (isJust q.questingUnit) $
        addQuestToken self.key 1

-- | "Action: Spend N tokens to do X." Models the second half of a
-- token-payoff quest. The wrapper enforces "has ≥ N tokens" before
-- firing the body and debits the tokens on success, so the card body
-- only states the payoff.
--
-- > spendTokens "Draw 3 cards" 3 \u -> drawCards u.user 3
spendTokens
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage Quest -> m ()
     )
  -> CardBuilder Quest ()
spendTokens name n body = actionDef ActionDef
  { actionName = name
  , actionCost = 0
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u ->
      withQuest u.self.key \q -> when (q.tokens >= n) do
        addQuestToken u.self.key (-n)
        body u
  }

-- | "Constant" static-effect block. Body fires every engine tick and
-- accumulates 'EffectM' contributions into the unit's per-tick
-- 'runtimeEffects'. The lambda receives just this unit; live game
-- state (the unit's current zone record, its owner, etc.) is
-- available via lookup helpers like 'zoneOf' so the read is always
-- fresh.
--
-- > battlefield $ constant \self -> do
-- >   z <- zoneOf self
-- >   when (z.developments >= 2) $ gainPower self 2
constant
  :: (UnitDetails -> EffectM ())
  -> CardBuilder Unit ()
constant body = modifyUnitExtras \e -> e
  { runtimeEffects = \g self ->
      let prev = e.runtimeEffects g self
       in prev <> execEffectM g (body self)
  }

-- | Look up the current 'Zone' record for a unit (off its
-- controller's capital). Reads game state via 'HasGame' so the
-- result is always fresh.
zoneOf :: HasGame m => UnitDetails -> m Zone
zoneOf u = do
  g <- getGame
  let owner = playerOf u.controller g
  pure $ case u.zone of
    KingdomZone -> owner.capital.kingdom
    QuestZone -> owner.capital.quest
    BattlefieldZone -> owner.capital.battlefield

-- | Continuation-style 'zoneOf'. Runs the body with the unit's
-- current zone record. Mirrors 'withQuest' / 'withUnit' /
-- 'withSupport' / 'withLegend' / 'withCombat'.
--
-- > withZoneOf self \z ->
-- >   when (z.developments >= 2) $ gainPower self 2
withZoneOf
  :: HasGame m => UnitDetails -> (Zone -> m ()) -> m ()
withZoneOf u k = zoneOf u >>= k

-- | Look up the 'History' bucket for a scope (counts of units
-- discarded, attackers declared, damage taken, etc. since the
-- scope's last reset). Mirrors 'zoneOf' for zone records.
historyOf :: HasGame m => Scope -> m History
historyOf s = do
  g <- getGame
  pure (Map.findWithDefault mempty s g.history)

-- | Continuation-style 'historyOf'.
--
-- > withHistory ThisTurn \h ->
-- >   when (h.unitsDiscarded > 0) $
-- >     gainResources pk h.unitsDiscarded
withHistory
  :: HasGame m => Scope -> (History -> m ()) -> m ()
withHistory s k = historyOf s >>= k

-- | Compose two 'Receive' handlers sequentially. The first runs, then
-- the second.
composeReceive :: Receive k -> Receive k -> Receive k
composeReceive (Receive a) (Receive b) = Receive \msg owner self -> do
  a msg owner self
  b msg owner self

-- | Wrap a 'Receive' handler so it only fires while the host is in
-- the given zone.
gateReceive
  :: HasField "zone" (InPlay k) ZoneKind
  => ZoneKind -> Receive k -> Receive k
gateReceive z (Receive r) = Receive \msg owner self ->
  when (self.zone == z) (r msg owner self)

-- ---------------------------------------------------------------------
-- Zone wrappers
--
-- 'battlefield', 'quest', 'kingdom' wrap a card-builder block and
-- zone-gate every registration added inside: triggers ('forced' /
-- 'triggered' / direct trigger combinators) get a 'self.zone == z'
-- check at fire time; actions get 'availableInZone = Just z'; for
-- Unit cards 'constant' callbacks get the same self-zone gate.
-- ---------------------------------------------------------------------

-- | Per-kind handling of the zone gate. Receive + actions are
-- handled uniformly across kinds; 'constant'-style 'runtimeEffects'
-- gating only applies to Unit cards (no other kind has that slot).
class CanZoneGate (k :: CardKind) where
  withZoneGate :: ZoneKind -> CardBuilder k () -> CardBuilder k ()

instance CanZoneGate Unit where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
        prevRT = cd0.extras.runtimeEffects
    modify \cd -> cd
      { receive = noReceive
      , actions = []
      , extras = cd.extras {runtimeEffects = \_ _ -> mempty}
      }
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
        addedRT = cd1.extras.runtimeEffects
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      , extras = cd.extras
          { runtimeEffects = \g self ->
              prevRT g self
                <> if self.zone == z then addedRT g self else mempty
          }
      }

instance CanZoneGate Support where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
    modify \cd -> cd {receive = noReceive, actions = []}
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      }

instance CanZoneGate Legend where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
    modify \cd -> cd {receive = noReceive, actions = []}
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      }

-- | "Battlefield. …" gate. Wraps a card-builder block; every
-- registration inside is gated to the battlefield zone.
--
-- > trollSlayers = unitCard "core-004" "Troll Slayers" do
-- >   ...
-- >   battlefield $ constant \zone self _owner ->
-- >     when (zone.developments >= 2) $ gainPower self 2
battlefield :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
battlefield = withZoneGate BattlefieldZone

-- | "Quest. …" gate. Mirrors 'battlefield' for the quest zone.
quest :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
quest = withZoneGate QuestZone

-- | "Kingdom. …" gate. Mirrors 'battlefield' for the kingdom zone.
kingdom :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
kingdom = withZoneGate KingdomZone

-- | True iff any section of this player's capital is currently
-- burning. The 'Player'-shaped counterpart to 'controllerBurning'.
capitalBurning :: Player -> Bool
capitalBurning p = any (.burning) p.capital.zones

-- ---------------------------------------------------------------------
-- Targets
--
-- 'Target a' describes what kind of pick an action needs and is the
-- typed counterpart to the wire-side 'TargetSchema'. The phantom 'a'
-- is the type returned by 'withTarget', so card bodies don't have to
-- pattern-match on a heterogeneous 'ActionTarget'.
-- ---------------------------------------------------------------------

-- | Typed action-target descriptor. Each single-variant constructor
-- pins the return type that 'withTarget' produces; 'Or' merges
-- branches into a flat 'TargetOption' so card bodies pattern-match
-- on the wire-side variant directly rather than nested 'Either'.
data Target a where
  AnyUnit :: Target UnitKey
  AnyCapital :: Target (PlayerKey, ZoneKind)
    -- ^ A capital zone. Standalone 'withTarget' enumerates every
    -- unburned zone across both players and prompts.
  MyDevZone :: Target ZoneKind
    -- ^ One of the controller's own non-burned development zones
    -- (kingdom or battlefield). Used by cards that place developments
    -- in a chosen zone (Wake the Mountain).
  UnitMatching :: (PlayerKey -> Game -> UnitDetails -> Bool) -> Target UnitKey
    -- ^ A unit satisfying the supplied predicate. The first argument
    -- is the player making the pick (so predicates can reference
    -- "your" / "an opponent's" via @u.controller@). Use the smart
    -- constructors below (@ownUnit@, @enemyUnit@, @defendingUnit@…)
    -- for common cases.
  CapitalMatching
    :: (PlayerKey -> (PlayerKey, ZoneKind) -> Bool)
    -> Target (PlayerKey, ZoneKind)
    -- ^ A capital zone satisfying the supplied predicate. Used to
    -- restrict 'AnyCapital'-style picks to e.g. opposing zones.
  Or :: Target a -> Target b -> Target TargetOption
    -- ^ "X or Y." Enumerates both branches' candidates into a
    -- single unified prompt and returns the chosen 'TargetOption'.
    -- The card body pattern-matches on the wire-side variant
    -- (e.g. @TargetUnitOption u@); the loss of compile-time
    -- exhaustiveness vs. nested 'Either' is the price of staying
    -- flat under deeper combinations.

-- | "A unit you control."
ownUnit :: Target UnitKey
ownUnit = UnitMatching \pk _ u -> u.controller == pk

-- | "A unit an opponent controls."
enemyUnit :: Target UnitKey
enemyUnit = UnitMatching \pk _ u -> u.controller /= pk

-- | "A defending unit in the current combat."
defendingUnit :: Target UnitKey
defendingUnit = UnitMatching \_pk g u -> case g.combat of
  Just cs -> u.key `elem` cs.defenders
  Nothing -> False

-- | "An attacking unit in the current combat."
attackingUnit :: Target UnitKey
attackingUnit = UnitMatching \_pk g u -> case g.combat of
  Just cs -> u.key `elem` cs.attackers
  Nothing -> False

-- | "A unit (any controller) matching this predicate."
unitWhere :: (UnitDetails -> Bool) -> Target UnitKey
unitWhere p = UnitMatching \_ _ u -> p u

-- | "An enemy unit matching this predicate."
enemyUnitWhere :: (UnitDetails -> Bool) -> Target UnitKey
enemyUnitWhere p = UnitMatching \pk _ u -> u.controller /= pk && p u

-- | "A capital zone an opponent controls (not burned)."
enemyCapital :: Target (PlayerKey, ZoneKind)
enemyCapital = CapitalMatching \pk (owner, _) -> owner /= pk

-- | Fire a target-selection prompt (or auto-resolve, depending on the
-- target shape) and run a continuation with the chosen pick. If no
-- target can be acquired, the continuation is skipped silently.
--
-- > withTarget pk AnyUnit \t -> until EndOfTurn $ buffPower t 1
withTarget
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Target a -> (a -> m ()) -> m ()
withTarget pk t k = pickTarget pk t >>= flip whenJust k

-- | Resolve a 'Target' to its concrete pick, or 'Nothing' if the
-- player declined / no valid target exists. 'Or' enumerates every
-- branch's candidates into a single unified prompt and returns the
-- chosen 'TargetOption'; single-variant Targets unwrap the option
-- to their typed value (e.g. 'AnyUnit' → 'UnitKey').
pickTarget
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Target a -> m (Maybe a)
pickTarget pk = \case
  AnyUnit -> do
    opts <- enumerateOptions pk AnyUnit
    promptForOption pk opts >>= \case
      Just (TargetUnitOption k) -> pure (Just k)
      _ -> pure Nothing
  AnyCapital -> do
    opts <- enumerateOptions pk AnyCapital
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  MyDevZone -> do
    opts <- enumerateOptions pk MyDevZone
    promptForOption pk opts >>= \case
      Just (TargetZoneOption _ z) -> pure (Just z)
      _ -> pure Nothing
  UnitMatching p -> do
    opts <- enumerateOptions pk (UnitMatching p)
    promptForOption pk opts >>= \case
      Just (TargetUnitOption k) -> pure (Just k)
      _ -> pure Nothing
  CapitalMatching p -> do
    opts <- enumerateOptions pk (CapitalMatching p)
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  Or a b -> do
    aOpts <- enumerateOptions pk a
    bOpts <- enumerateOptions pk b
    promptForOption pk (aOpts <> bOpts)

-- | Fire a 'ChooseTargetOption' prompt with the supplied options.
-- Returns 'Nothing' if the option list is empty or the player
-- declines.
promptForOption
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> [TargetOption] -> m (Maybe TargetOption)
promptForOption _ [] = pure Nothing
promptForOption pk opts = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseTargetOption
        { options = opts
        , description = "Choose a target."
        }
    , callback = CallbackInlinePrompt
    }
  pure $ case answer of
    PickTargetOption chosen | chosen `elem` opts -> Just chosen
    _ -> Nothing

-- | Enumerate every candidate 'TargetOption' a 'Target' offers.
-- 'Or' concatenates its branches; single-variant constructors emit
-- their natural options.
enumerateOptions
  :: HasGame m => PlayerKey -> Target a -> m [TargetOption]
enumerateOptions pk t = do
  g <- getGame
  pure (enumerateOptionsPure pk g t)

-- | Pure version of 'enumerateOptions'. Used by 'hasTarget' (so that
-- 'playableWhen \\g pk -> hasTarget t g pk' avoids running a prompt
-- just to check existence).
enumerateOptionsPure :: PlayerKey -> Game -> Target a -> [TargetOption]
enumerateOptionsPure pk g = \case
  AnyUnit -> [TargetUnitOption u.key | u <- g.units]
  AnyCapital ->
    let zonesOf p =
          [ (p.key, z.kind)
          | z <- p.capital.zones
          , not z.burned
          ]
     in [ TargetZoneOption owner z
        | (owner, z) <- zonesOf g.player1 <> zonesOf g.player2
        ]
  MyDevZone ->
    let me = playerOf pk g
     in [ TargetZoneOption pk z.kind
        | z <- [me.capital.kingdom, me.capital.battlefield]
        , not z.burned
        ]
  UnitMatching p ->
    [TargetUnitOption u.key | u <- g.units, p pk g u]
  CapitalMatching p ->
    let zonesOf player =
          [ (player.key, z.kind)
          | z <- player.capital.zones
          , not z.burned
          ]
     in [ TargetZoneOption owner z
        | (owner, z) <- zonesOf g.player1 <> zonesOf g.player2
        , p pk (owner, z)
        ]
  Or a b ->
    enumerateOptionsPure pk g a <> enumerateOptionsPure pk g b

-- | "Does this target offer at least one candidate?" Use inside
-- 'playableWhen' so a tactic can verify there's something to pick
-- before resolving:
--
-- > playableWhen $ hasTarget enemyUnit
hasTarget :: Target a -> Game -> PlayerKey -> Bool
hasTarget t g pk = not (null (enumerateOptionsPure pk g t))

-- | Every unit matching a 'Target UnitKey' as 'UnitDetails'. Useful for
-- "each of your Xs" effects that need to fan out across multiple
-- units without a prompt.
--
-- > mine <- unitsMatching pk ownUnit
-- > for_ mine \u -> until EndOfTurn $ buffPower u.key 1
unitsMatching :: HasGame m => PlayerKey -> Target UnitKey -> m [UnitDetails]
unitsMatching pk t = do
  g <- getGame
  pure
    [ u
    | TargetUnitOption k <- enumerateOptionsPure pk g t
    , Just u <- [findUnit k g]
    ]

-- | "Pick up to N units matching this 'Target' and do something."
-- Combines target enumeration with 'chooseUpTo' so cards can express
-- "corrupt up to 3 target units" / "destroy up to 2 target enemy
-- attackers" in one line.
--
-- > withUpTo pk 3 (unitWhere (not . (.corrupted))) (traverse_ corrupt)
withUpTo
  :: (HasGame m, HasPromptIO m)
  => PlayerKey -> Int -> Target UnitKey -> ([UnitKey] -> m ()) -> m ()
withUpTo pk n t body = do
  candidates <- unitsMatching pk t
  chooseUpTo pk n (map (.key) candidates) body

-- | "Each of your Xs gains N power until end of turn." Buffs every
-- unit matching 'Target UnitKey' with a 'GainPower' modifier scoped
-- to end of turn.
--
-- > whenResolved \self -> buffEachUntilEoT self.controller ownOrcs 2
buffEachUntilEoT
  :: (HasGame m, HasQueue Message m)
  => PlayerKey -> Target UnitKey -> Int -> m ()
buffEachUntilEoT pk t n = do
  mine <- unitsMatching pk t
  for_ mine \u -> until EndOfTurn $ buffPower u.key n

allCards :: Map CardCode SomeCardDef
allCards =
  Map.fromList
    [ ("core-001", UnitCardDef defenderOfTheHold)
    , ("core-002", UnitCardDef zhufbarEngineers)
    , ("core-003", UnitCardDef hammererOfKarakAzul)
    , ("core-004", UnitCardDef trollSlayers)
    , ("core-005", UnitCardDef runesmith)
    , ("core-006", UnitCardDef durgnarTheBold)
    , ("core-007", UnitCardDef kingKazador)
    , ("core-008", UnitCardDef dwarfCannonCrew)
    , ("core-009", UnitCardDef dwarfMasons)
    , ("core-010", UnitCardDef dwarfRanger)
    , ("core-011", UnitCardDef mountainBrigade)
    , ("core-012", UnitCardDef ironbreakersOfAnkhor)
    , ("core-013", SupportCardDef runeOfFortitude)
    , ("core-014", SupportCardDef keystoneForge)
    , ("core-015", SupportCardDef organGun)
    , ("core-016", SupportCardDef masterRuneOfDismay)
    , ("core-017", QuestCardDef aGloriousDeath)
    , ("core-018", SupportCardDef grudgeThrower)
    , ("core-019", TacticCardDef buryingTheGrudge)
    , ("core-020", TacticCardDef stubbornRefusal)
    , ("core-021", TacticCardDef strikingTheGrudge)
    , ("core-022", TacticCardDef grudgeThrowerAssault)
    , ("core-023", TacticCardDef demolition)
    , ("core-024", TacticCardDef wakeTheMountain)
    , ("core-025", TacticCardDef masterRuneOfValaya)
    , ("core-081", UnitCardDef servantsOfKhorne)
    , ("core-082", UnitCardDef savageMarauders)
    , ("core-087", UnitCardDef valkiaTheBloody)
    , ("core-092", UnitCardDef bloodthirster)
    , ("core-103", TacticCardDef bloodForTheBloodGod)
    , ("legends-031", UnitCardDef bloodletter)
    , ("path-of-the-zealot-031", UnitCardDef bloodsworn)
    , ("cataclysm-034", UnitCardDef bloodcrusher)
    , ("fragments-of-power-031", UnitCardDef swornOfKhorne)
    , ("the-fourth-waystone-091", UnitCardDef viciousMarauder)
    , ("legends-032", UnitCardDef warhounds)
    , ("path-of-the-zealot-032", QuestCardDef wolvesOfTheNorth)
    , ("the-chaos-moon-032", UnitCardDef doombull)
    , ("faith-and-steel-113", UnitCardDef skulltaker)
    , ("cataclysm-033", UnitCardDef lordOfKhorne)
    , ("the-warpstone-chronicles-094", TacticCardDef berserkFury)
    , ("the-warpstone-chronicles-095", SupportCardDef daemonsword)
    , ("the-eclipse-of-hope-093", SupportCardDef brandedByKhorne)
    , ("omens-of-ruin-013", SupportCardDef markOfChaos)
    , ("the-ruinous-hordes-083", SupportCardDef northernWastes)
    , ("the-inevitable-city-013", SupportCardDef ironThroneroom)
    , ("the-inevitable-city-020", QuestCardDef raidingCamps)
    , ("the-accursed-dead-052", SupportCardDef riftOfBattle)
    , ("cataclysm-037", SupportCardDef riftOfChaos)
    , ("days-of-blood-018", TacticCardDef recklessAttack)
    , ("the-ruinous-hordes-082", QuestCardDef dominionOfChaos)
    -- ----------------------------------------------------------------------
    -- Empire (core-026 to core-050)
    , ("core-026", UnitCardDef spearmenOfWissenland)
    , ("core-027", UnitCardDef stateTroops)
    , ("core-028", UnitCardDef templarOfSigmar)
    , ("core-029", UnitCardDef witchHunter)
    , ("core-030", UnitCardDef greatswordsOfNuln)
    , ("core-031", UnitCardDef knightsPanther)
    , ("core-032", UnitCardDef reiksguard)
    , ("core-033", UnitCardDef rieklandMarksmen)
    , ("core-034", UnitCardDef thyrusGorman)
    , ("core-035", UnitCardDef karlFranz)
    , ("core-036", UnitCardDef volkmarTheGrim)
    , ("core-037", UnitCardDef mariusLeitdorf)
    , ("core-038", UnitCardDef lectorOfSigmar)
    , ("core-039", UnitCardDef imperialEngineers)
    , ("core-040", UnitCardDef pegasusKnights)
    , ("core-041", SupportCardDef theImperialCrown)
    , ("core-042", SupportCardDef hammerOfSigmar)
    , ("core-043", SupportCardDef bannerOfSigmar)
    , ("core-044", SupportCardDef altdorf)
    , ("core-045", QuestCardDef defendingTheEmpire)
    , ("core-046", TacticCardDef forSigmar)
    , ("core-047", TacticCardDef sigmarsWrath)
    , ("core-048", TacticCardDef counterCharge)
    , ("core-049", TacticCardDef battleOfTheReik)
    , ("core-050", TacticCardDef defendersOfTheFaith)
    -- ----------------------------------------------------------------------
    -- High Elf (core-051 to core-075)
    , ("core-051", UnitCardDef phoenixGuard)
    , ("core-052", UnitCardDef whiteLionsOfChrace)
    , ("core-053", UnitCardDef swordmastersOfHoeth)
    , ("core-054", UnitCardDef highElfArchers)
    , ("core-055", UnitCardDef seaGuardOfLothern)
    , ("core-056", UnitCardDef silverHelms)
    , ("core-057", UnitCardDef dragonPrincesOfCaledor)
    , ("core-058", UnitCardDef princeTyrion)
    , ("core-059", UnitCardDef teclis)
    , ("core-060", UnitCardDef eltharionTheGrim)
    , ("core-061", UnitCardDef korhil)
    , ("core-062", UnitCardDef loremasterOfHoeth)
    , ("core-063", UnitCardDef mageOfTheWhiteTower)
    , ("core-064", UnitCardDef spearmenOfLothern)
    , ("core-065", UnitCardDef reaverKnights)
    , ("core-066", SupportCardDef lighthouseOfLothern)
    , ("core-067", SupportCardDef bannerOfAvelorn)
    , ("core-068", SupportCardDef bowOfAvelorn)
    , ("core-069", SupportCardDef hoethsWisdom)
    , ("core-070", QuestCardDef theWhiteTower)
    , ("core-071", TacticCardDef voiceOfCommand)
    , ("core-072", TacticCardDef dragonBreath)
    , ("core-073", TacticCardDef magicOfTheOldOnes)
    , ("core-074", TacticCardDef battleMagic)
    , ("core-075", TacticCardDef sacredIncantations)
    -- ----------------------------------------------------------------------
    -- Chaos (fill the core-083..core-102 gaps)
    , ("core-083", UnitCardDef tzeentchSorcerer)
    , ("core-084", UnitCardDef slaaneshiMarauders)
    , ("core-085", UnitCardDef plaguebearersOfNurgle)
    , ("core-086", UnitCardDef festeringNurglings)
    , ("core-088", UnitCardDef archaonTheEverchosen)
    , ("core-089", UnitCardDef chaosKnights)
    , ("core-090", UnitCardDef chaosWarriors)
    , ("core-091", UnitCardDef maraudersOfTheNorth)
    , ("core-093", UnitCardDef chaosSorcerer)
    , ("core-094", UnitCardDef horrorOfTzeentch)
    , ("core-095", UnitCardDef daemonettesOfSlaanesh)
    , ("core-096", UnitCardDef beastsOfNurgle)
    , ("core-097", UnitCardDef chaosSpawn)
    , ("core-098", SupportCardDef eyeOfTzeentch)
    , ("core-099", SupportCardDef theIronTower)
    , ("core-100", SupportCardDef pyreOfTcharzanek)
    , ("core-101", TacticCardDef tidesOfChaos)
    , ("core-102", TacticCardDef doomOfTheEmpire)
    -- ----------------------------------------------------------------------
    -- Orc (core-106 to core-130)
    , ("core-106", UnitCardDef grimgorIronhide)
    , ("core-107", UnitCardDef skarsnik)
    , ("core-108", UnitCardDef gorbadIronclaw)
    , ("core-109", UnitCardDef orcBigUns)
    , ("core-110", UnitCardDef blackOrcs)
    , ("core-111", UnitCardDef savageOrcs)
    , ("core-112", UnitCardDef orcBoyz)
    , ("core-113", UnitCardDef boarBoyz)
    , ("core-114", UnitCardDef nightGoblins)
    , ("core-115", UnitCardDef goblinWolfRiders)
    , ("core-116", UnitCardDef squigHoppers)
    , ("core-117", UnitCardDef orcShaman)
    , ("core-118", UnitCardDef trolls)
    , ("core-119", UnitCardDef forestGoblinSpiderRiders)
    , ("core-120", UnitCardDef snotlings)
    , ("core-121", SupportCardDef daBadMoon)
    , ("core-122", SupportCardDef choppa)
    , ("core-123", SupportCardDef bigBossesBanner)
    , ("core-124", SupportCardDef daMorksEye)
    , ("core-125", SupportCardDef orcWarmachine)
    , ("core-126", QuestCardDef greenskinRush)
    , ("core-127", TacticCardDef waaagh)
    , ("core-128", TacticCardDef crushEm)
    , ("core-129", TacticCardDef runEmDown)
    , ("core-130", TacticCardDef daBigStomp)
    -- ----------------------------------------------------------------------
    -- Dark Elf (core-131 to core-155)
    , ("core-131", UnitCardDef malekith)
    , ("core-132", UnitCardDef morathi)
    , ("core-133", UnitCardDef croneHellebron)
    , ("core-134", UnitCardDef lokhirFellheart)
    , ("core-135", UnitCardDef witchElves)
    , ("core-136", UnitCardDef blackGuardOfNaggarond)
    , ("core-137", UnitCardDef executioners)
    , ("core-138", UnitCardDef corsairs)
    , ("core-139", UnitCardDef coldOneKnights)
    , ("core-140", UnitCardDef darkRiders)
    , ("core-141", UnitCardDef darkSorceress)
    , ("core-142", UnitCardDef assassinsOfKhaine)
    , ("core-143", UnitCardDef repeaterCrossbowmen)
    , ("core-144", UnitCardDef bloodwrackMedusa)
    , ("core-145", UnitCardDef blackDragon)
    , ("core-146", UnitCardDef manticore)
    , ("core-147", SupportCardDef cauldronOfBlood)
    , ("core-148", SupportCardDef theBlackArk)
    , ("core-149", SupportCardDef whipOfAgony)
    , ("core-150", SupportCardDef druchiiBanner)
    , ("core-151", SupportCardDef witchbrew)
    , ("core-152", QuestCardDef slaughterAtLustria)
    , ("core-153", TacticCardDef khainesEmbrace)
    , ("core-154", TacticCardDef murderousProwess)
    , ("core-155", TacticCardDef coldBloodedSlaughter)
    ]

trollSlayers :: CardDef Unit
trollSlayers = unitCard "core-004" "Troll Slayers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Slayer
  body
    "Battlefield. This unit gains {power}{power} while you have at least two developments in this zone."
  battlefield $ constant \self ->
    withZoneOf self \z ->
      when (z.developments >= 2) $ gainPower self 2

runesmith :: CardDef Unit
runesmith = unitCard "core-005" "Runesmith" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Priest
  body
    "Quest. Action: Spend 2 resources to have a target unit gain {power} until the end of the turn."
  quest $ action "Buff a unit" 2 \usage ->
    withTarget usage.user AnyUnit \t -> until EndOfTurn $ buffPower t 1

durgnarTheBold :: CardDef Unit
durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
  hero
  trait Warrior
  race Dwarf
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  body
    "Limit one Hero per zone.\nThis unit gains {power}{power} while one section of your capital is burning."
  effects \self owner ->
    when (capitalBurning owner) $ gainPower self 2

kingKazador :: CardDef Unit
kingKazador = unitCard "core-007" "King Kazador" do
  hero
  trait Warrior
  race Dwarf
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  body
    "Limit one Hero per zone.\nToughness 2.\nOpponents cannot target this unit with card effects unless they pay an additional 3 resources per effect."
  toughness 2
  targetTax \_g caster self ->
    if caster /= self.controller then 3 else 0

dwarfCannonCrew :: CardDef Unit
dwarfCannonCrew = unitCard "core-008" "Dwarf Cannon Crew" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Engineer
  body
    "Forced: When this unit enters play, search the top five cards of your deck for a support card with cost 2 or lower. You may put that card into this zone. Then, shuffle your deck."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let matches = filterSupportsIn result.cards (costAtMost 2)
      chooseFromCards pk 0 1 matches
        "Choose a support to put into play (or skip)." \chosen ->
          for_ chosen \c -> playSupportFromDeck pk c.key self.zone
      shuffleDeck pk

dwarfMasons :: CardDef Unit
dwarfMasons = unitCard "core-009" "Dwarf Masons" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Engineer
  -- FAQ 2.2: "After" → "When" on card text.
  body
    "Forced: When this unit enters play, put the top card of your deck facedown into this zone as a development."
  flavor "Put your faith in the rock, lad."
  onEnterPlay \_owner self ->
    addDevelopment self.controller self.zone

dwarfRanger :: CardDef Unit
dwarfRanger = unitCard "core-010" "Dwarf Ranger" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Ranger
  scout
  body
    "Scout.\nQuest. Forced: When one of your other Dwarf units leaves play, deal 1 damage to one target unit or capital."
  quest $ forced \self ->
    onUnitOfLeavesPlay self.controller \unit ->
      when (unit.key /= self.key && unit `isRace` Dwarf) $
        withTarget self.controller (AnyUnit `Or` AnyCapital) \case
          TargetUnitOption u -> dealDamage u 1
          TargetZoneOption owner z -> dealZoneDamage owner z 1

mountainBrigade :: CardDef Unit
mountainBrigade = unitCard "core-011" "Mountain Brigade" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 6
  trait Warrior
  flavor "These stout dwarfs are the first line of defence."

ironbreakersOfAnkhor :: CardDef Unit
ironbreakersOfAnkhor = unitCard "core-012" "Ironbreakers of Ankhor" do
  race Dwarf
  cost 5
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  toughnessX
  body
    "Toughness X (whenever this unit is assigned damage, cancel X of that damage).\nX is the number of developments in this zone."

runeOfFortitude :: CardDef Support
runeOfFortitude = supportCard "core-013" "Rune of Fortitude" do
  race Dwarf
  cost 2
  loyalty 1
  trait Rune
  body "Each unit attacking this zone loses {power} unless its controller pays 1 resource per unit."
  imposesRuneOfFortitudeTax

keystoneForge :: CardDef Support
keystoneForge = supportCard "core-014" "Keystone Forge" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, heal 1 damage to your capital."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      healCapital self.controller 1

organGun :: CardDef Support
organGun = supportCard "core-015" "Organ Gun" do
  race Dwarf
  cost 0
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target unit.\n Attached unit gains {power}{power} while defending."
  flavor "A dwarf device deadly and reliable forever."
  attachedTo \_self unit ->
    when unit.defending $ gainPower unit 2

masterRuneOfDismay :: CardDef Support
masterRuneOfDismay = supportCard "core-016" "Master Rune of Dismay" do
  race Dwarf
  cost 4
  loyalty 3
  trait Rune
  body "Kingdom. Opponent's units cost 1 additional resource to play."
  flavor "Enemies of the dwarfs beware, your fears are returned to you a hundredfold!"
  globalCostAdjust \_g s playing _filter ->
    if playing /= s.controller && s.zone == KingdomZone then 1 else 0

aGloriousDeath :: CardDef Quest
aGloriousDeath = questCard "core-017" "A Glorious Death" do
  race Dwarf
  cost 0
  loyalty 2
  body
    "Quest. Action: Sacrifice the unit on this quest to destroy up to two target attacking units. Use this ability only if A Glorious Death has 3 or more resource tokens on it.\nQuest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Glorious sacrifice" 0 \usage ->
    withQuest usage.self.key \q -> when (q.tokens >= 3) $
      for_ q.questingUnit \quester -> do
        destroyUnit quester
        withCombat \cs ->
          when (cs.attackingPlayer /= usage.user) $
            chooseUpTo usage.user 2 cs.attackers (traverse_ destroyUnit)

grudgeThrower :: CardDef Support
grudgeThrower = supportCard "core-018" "Grudge Thrower" do
  race Dwarf
  cost 1
  loyalty 2
  trait Siege
  body
    "Battlefield. Action: Spend 1 resource and sacrifice a unit to have each attacking or defending unit gain {power} until the end of the turn."
  battlefield $ actionWith "Volley" 1 [SacrificeUnit] \_usage ->
    withCombat \cs ->
      for_ (cs.attackers <> cs.defenders) \k ->
        until EndOfTurn $ buffPower k 1

buryingTheGrudge :: CardDef Tactic
buryingTheGrudge = tacticCard "core-019" "Burying the Grudge" do
  race Dwarf
  cost 0
  loyalty 2
  body "Action: Gain 1 resource for each unit that entered a discard pile this turn."
  flavor "Grudges are best buried with the corpse of the wrongdoer."
  playableWhen \g _ ->
    maybe 0 (.unitsDiscarded) (Map.lookup ThisTurn g.history) > 0
  whenResolved \self ->
    withHistory ThisTurn \h ->
      gainResources self.controller h.unitsDiscarded

stubbornRefusal :: CardDef Tactic
stubbornRefusal = tacticCard "core-020" "Stubborn Refusal" do
  race Dwarf
  cost 2
  loyalty 1
  body
    "Action: Move all damage from one target unit to another target unit in any player's corresponding zone."
  playableWhen \g _pk ->
    any (\u -> isDamaged u && hasPeerInZone g u) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \src ->
      withUnit src \srcUnit -> do
        peers <- filter (\u -> u.key /= src) <$> unitsInZone srcUnit.zone
        chooseUpTo pk 1 (map (.key) peers) \chosen ->
          for_ chosen \dst -> moveAllDamage src dst

strikingTheGrudge :: CardDef Tactic
strikingTheGrudge = tacticCard "core-021" "Striking the Grudge" do
  race Dwarf
  cost 1
  loyalty 3
  body
    "Action: One target attacking or defending unit gains {power}{power} until the end of the turn."
  flavor "Honour redeemed, oaths fulfilled."
  playableWhen $ hasTarget (Or attackingUnit defendingUnit)
  whenResolved \self ->
    withTarget self.controller (Or attackingUnit defendingUnit) \case
      TargetUnitOption k -> until EndOfTurn $ buffPower k 2
      _ -> pure ()

grudgeThrowerAssault :: CardDef Tactic
grudgeThrowerAssault = tacticCard "core-022" "Grudge Thrower Assault" do
  race Dwarf
  cost 2
  loyalty 3
  body
    "Play during combat, after damage has been assigned.\nAction: Destroy one target attacking unit."
  playableWhen hasEnemyAttacker
  whenResolved \self ->
    withTarget self.controller attackingUnit \k -> destroyUnit k

demolition :: CardDef Tactic
demolition = tacticCard "core-023" "Demolition!" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Destroy one target support card or development."
  flavor "KABOOM!"
  playableWhen hasEnemySupport
  tacticTargets SupportTargetSchema
  onResolveEnemySupport \_pk s -> destroySupport s.key

wakeTheMountain :: CardDef Tactic
wakeTheMountain = tacticCard "core-024" "Wake the Mountain" do
  race Dwarf
  cost 3
  loyalty 2
  body
    "Action: Put the top three cards of your deck into your battlefield or kingdom facedown as developments. (All three developments must go in the same zone.)"
  playableWhen \g pk -> hasDeckSize 3 g pk && canDevelop g pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyDevZone \zone ->
      replicateM_ 3 (addDevelopment pk zone)

masterRuneOfValaya :: CardDef Tactic
masterRuneOfValaya = tacticCard "core-025" "Master Rune of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: Cancel all damage assigned during the battlefield phase this turn."
  flavor "Valaya preseve and protect us in our hour of need!"
  whenResolved \_ -> do
    push CancelAllAssignedDamage
    push CancelAllBattlefieldDamageThisTurn

defenderOfTheHold :: CardDef Unit
defenderOfTheHold = unitCard "core-001" "Defender of the Hold" do
  race Dwarf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  flavor "My blood for the hold, 'tis a fair trade."
  keyword BattlefieldOnly

zhufbarEngineers :: CardDef Unit
zhufbarEngineers = unitCard "core-002" "Zhufbar Engineers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  -- FAQ 2.2: "After" → "When" on card text.
  body "Forced: When this unit leaves play, each opponent must sacrifice a unit in this corresponding zone."

hammererOfKarakAzul :: CardDef Unit
hammererOfKarakAzul = unitCard "core-003" "Hammerer of Karak Azul" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."
  flavor "\"The hammer blow rings out doom to our foe.\"\n-Ancient Hammerer saying"

-- ----------------------------------------------------------------------------
-- Chaos cards

servantsOfKhorne :: CardDef Unit
servantsOfKhorne = unitCard "core-081" "Servants of Khorne" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

savageMarauders :: CardDef Unit
savageMarauders = unitCard "core-082" "Savage Marauders" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

valkiaTheBloody :: CardDef Unit
valkiaTheBloody = unitCard "core-087" "Valkia the Bloody" do
  unique
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Daemon]
  body
    "Limit one Hero per zone.\n\
    \Quest. Action: Spend 2 resources to move any number of damage on this unit to a target corrupted unit."
  quest $ actionWith "Sluice damage" 2 [] \u -> do
    let self = u.self
    when (isDamaged self) $
      withTarget self.controller corruptedEnemy \k ->
        moveAllDamage self.key k
  where
    corruptedEnemy =
      UnitMatching \pk _ u -> u.controller /= pk && u.corrupted

bloodthirster :: CardDef Unit
bloodthirster = unitCard "core-092" "Bloodthirster" do
  unique
  race Chaos
  cost 8
  loyalty 5
  power 5
  hitPoints 8
  trait Daemon
  keyword DamageCannotBeCancelled
  -- FAQ 2.2 General: "After your turn begins" → "At the beginning of
  -- your turn". The trigger fires in Phase 0 (Beginning of the Turn).
  body
    "Damage cannot be cancelled.\n\
    \Forced: At the beginning of your turn, each player must sacrifice a unit in this corresponding zone."
  onMyTurnBegin \_owner self ->
    eachPlayer \pk ->
      mustSacrificeInZone pk self.zone
        "Sacrifice one of your units in this zone."

bloodForTheBloodGod :: CardDef Tactic
bloodForTheBloodGod = tacticCard "core-103" "Blood for the Blood God" do
  race Chaos
  cost 2
  loyalty 2
  body
    "Action: Choose a target unit in any battlefield. Deal damage to that unit equal to its power."
  playableWhen $ hasTarget unitInBattlefield
  whenResolved \self ->
    withTarget self.controller unitInBattlefield \k ->
      withUnit k \u -> dealDamage u.key u.cardDef.power
  where
    unitInBattlefield = unitWhere \u -> u.zone == BattlefieldZone

bloodletter :: CardDef Unit
bloodletter = unitCard "legends-031" "Bloodletter" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  trait Daemon
  body "Double all damage assigned to units as it is being assigned."
  damageMultiplier 2

bloodsworn :: CardDef Unit
bloodsworn = unitCard "path-of-the-zealot-031" "Bloodsworn" do
  race Chaos
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  trait Warrior
  body "Forced: When an opponent's unit enters a discard pile from play, heal all damage on Bloodsworn."
  -- Heal all damage. A large constant beats threading the current HP
  -- into the message; 'HealUnit' clamps to 0.
  onOpponentUnitLeavePlay \_owner self _uk _zone _code ->
    healUnit self.key 999

bloodcrusher :: CardDef Unit
bloodcrusher = unitCard "cataclysm-034" "Bloodcrusher" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 5
  trait Daemon
  body "Lower the cost to play this unit by 1 for each burning zone."
  selfCostAdjust \g _pk -> negate (burningZoneCount g)

swornOfKhorne :: CardDef Unit
swornOfKhorne = unitCard "fragments-of-power-031" "Sworn of Khorne" do
  race Chaos
  cost 2
  loyalty 1
  power 3
  hitPoints 1
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit cannot attack unless the defending zone has at least 1 corrupted unit."
  canAttack \g defender zone _u ->
    any
      ( \v ->
          v.controller == defender
            && v.zone == zone
            && v.corrupted
      )
      g.units

viciousMarauder :: CardDef Unit
viciousMarauder = unitCard "the-fourth-waystone-091" "Vicious Marauder" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit must attack during your battlefield phase, if able."
  -- Approximation: when the battlefield action window opens for the
  -- controller's turn, auto-initiate an attack on the opposing
  -- battlefield with every friendly unit in that zone (the marauder
  -- itself plus any others) — strictly, the rules force only the
  -- marauder, but auto-batching with others keeps the line of play
  -- coherent until action prompts exist.
  onActionWindow BattlefieldActionWindow \_owner self -> do
    g <- getGame
    when (g.currentPlayer == self.controller && self.zone == BattlefieldZone) do
      let attackers =
            [ u.key
            | u <- g.units
            , u.controller == self.controller
            , u.zone == BattlefieldZone
            , not u.corrupted
            ]
      unless (null attackers) $
        push $ BeginCombat self.controller BattlefieldZone attackers

warhounds :: CardDef Unit
warhounds = unitCard "legends-032" "Warhounds" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Creature
  body
    "Action: When this unit enters play, reveal a {chaos} legend or unit from your hand. \
    \If you do, deal 2 damage to target unit in any corresponding zone."
  onEnterPlay \_owner self ->
    revealFromHand self.controller chaosLegendOrUnit
      "Reveal a Chaos legend or unit to deal 2 damage." \_revealed ->
        withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
          dealDamage k 2
  where
    chaosLegendOrUnit c = case c.def of
      UnitCardDef cd -> Chaos `elem` cd.races
      LegendCardDef cd -> Chaos `elem` cd.races
      _ -> False

wolvesOfTheNorth :: CardDef Quest
wolvesOfTheNorth = questCard "path-of-the-zealot-032" "Wolves of the North" do
  race Chaos
  cost 0
  loyalty 2
  trait QuestTrait
  body
    "Action: During your quest phase, the unit questing on this card can initiate a single attack against a single zone controlled by an opponent."
  actionEnemyZone "Out-of-phase attack" 0 \u (_, z) ->
    withQuest u.self.key \q ->
      for_ q.questingUnit \attackerKey ->
        push $ BeginCombat u.user z [attackerKey]

doombull :: CardDef Unit
doombull = unitCard "the-chaos-moon-032" "Doombull" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, deal 4 damage to target unit in any corresponding zone."
  onSelfDestroyed \_owner self ->
    withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
      dealDamage k 4

skulltaker :: CardDef Unit
skulltaker = unitCard "faith-and-steel-113" "Skulltaker" do
  unique
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Daemon
  body
    "This unit gains {power} for each experience attached to it. \
    \Action: When an opponent's unit leaves play, spend 1 resource to attach it facedown to this unit as an experience."
  selfPower \_g u -> length u.experiences
  onOpponentUnitLeavePlay \_owner self _uk _zone code ->
    mayPay self.controller 1
      "Spend 1 resource to attach the departing unit as an experience on Skulltaker?" $
        attachExperience self.key code

lordOfKhorne :: CardDef Unit
lordOfKhorne = unitCard "cataclysm-033" "Lord of Khorne" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "This unit deals +1 damage in combat for each burning zone."
  combatPower \g _u -> burningZoneCount g

berserkFury :: CardDef Tactic
berserkFury = tacticCard "the-warpstone-chronicles-094" "Berserk Fury" do
  race Chaos
  cost 2
  loyalty 3
  body
    "Action: One target Unit gains 3 Power until the end of the turn. At the end of the turn, that unit takes 2 damage."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ buffPower k 3
      queueEoTDamage k 2

daemonsword :: CardDef Support
daemonsword = supportCard "the-warpstone-chronicles-095" "Daemonsword" do
  race Chaos
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {chaos} unit. Corrupt that unit. \
    \Attached unit gains 3 Power and gets +2 Hit Points."
  attachmentPower 3
  attachmentHp 2
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt

brandedByKhorne :: CardDef Support
brandedByKhorne = supportCard "the-eclipse-of-hope-093" "Branded by Khorne" do
  race Chaos
  cost 0
  loyalty 2
  trait Attachment
  body "Attach to a target unit. If attached unit is damaged, destroy that unit."
  onHostDamaged \_owner _self hostKey _n -> destroyUnit hostKey

markOfChaos :: CardDef Support
markOfChaos = supportCard "omens-of-ruin-013" "Mark of Chaos" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Spell]
  body
    "Attach to a target unit. Attached unit gains {power}{power}. \
    \Forced: At the beginning of your turn, attached unit takes 1 uncancellable damage."
  attachmentPower 2
  -- The +2 power half waits on dynamic modifiers; for now wire the
  -- turn-start damage tick on the host's controller's turn.
  onAttachedHostTurnBegin \_owner _self host ->
    dealUncancellableDamage host.key 1

northernWastes :: CardDef Support
northernWastes = supportCard "the-ruinous-hordes-083" "Northern Wastes" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  trait Wasteland
  body "If you control a faceup non-{chaos} unit or support card, sacrifice this card."
  -- Self-check on every message tick: if controller has any faceup
  -- non-Chaos unit or support, sacrifice. (Attachments inherit their
  -- host's faceup status; we don't track facedown explicitly so every
  -- in-play card is treated as faceup for this check.)
  onAnyMessage \_owner self -> do
    g <- getGame
    let nonChaosUnit u =
          u.controller == self.controller
            && Chaos `notElem` u.cardDef.races
        nonChaosSupport s =
          s.key /= self.key
            && s.controller == self.controller
            && Chaos `notElem` s.cardDef.races
    when
      ( any nonChaosUnit g.units
          || any nonChaosSupport g.supports
      )
      (destroySupport self.key)

ironThroneroom :: CardDef Support
ironThroneroom = supportCard "the-inevitable-city-013" "Iron Throneroom" do
  unique
  race Chaos
  cost 3
  loyalty 5
  power 2
  trait CapitalCenter
  body
    "This card enters play with 4 resource tokens on it. \
    \Action: At the beginning of your turn, remove a resource token from this card. \
    \Then, if there are no resource tokens on this card, put up to 3 {chaos} units into play from your hand or discard pile."
  onEnterPlay \_owner self -> adjustSupportTokens self.key 4
  onMyTurnBegin \_owner self -> when (self.tokens > 0) do
    adjustSupportTokens self.key (-1)
    when (self.tokens == 1) do
      let pk = self.controller
      me <- playerOf pk <$> getGame
      let isChaosUnit c = case c.def of
            UnitCardDef cd -> Chaos `elem` cd.races
            _ -> False
          handChaos = filter isChaosUnit me.hand
          discardChaos = filter isChaosUnit me.discard
          candidates = handChaos <> discardChaos
          inHandKeys = map (.key) handChaos
      chooseFromCards pk 0 3 candidates
        "Choose up to 3 Chaos units from your hand or discard pile to put into play." \chosen ->
          for_ chosen \c ->
            putUnitIntoPlay pk
              (if c.key `elem` inHandKeys then FromHand else FromDiscard)
              c.key KingdomZone

raidingCamps :: CardDef Quest
raidingCamps = questCard "the-inevitable-city-020" "Raiding Camps" do
  race Chaos
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. \
    \Quest. Action: When you play a {chaos} non-Attachment support card from your hand, \
    \destroy target support card in a zone with no units if a unit is questing here."
  -- "When this card enters play, draw a card." The second ability
  -- needs unit-questing-here tracking and is parked.
  onEnterPlay \_owner self ->
    drawCard self.controller

riftOfBattle :: CardDef Support
riftOfBattle = supportCard "the-accursed-dead-052" "Rift of Battle" do
  race Chaos
  cost 1
  loyalty 2
  trait Rift
  body "Units in all corresponding zones deal +1 damage in combat."
  supportCombat \_g _s _u -> 1

riftOfChaos :: CardDef Support
riftOfChaos = supportCard "cataclysm-037" "Rift of Chaos" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  trait Rift
  body "This card gains {power} for each burning zone."
  zonePowerAura \g s zone ->
    if s.zone == zone then burningZoneCount g else 0

recklessAttack :: CardDef Tactic
recklessAttack = tacticCard "days-of-blood-018" "Reckless Attack" do
  race Chaos
  cost 1
  loyalty 2
  keyword Limited
  body
    "Limited. Action: When your opponent declares at least 1 defender against your attack, \
    \put target unit in your discard pile into play in your battlefield declared as an attacker. \
    \At the end of the phase, sacrifice all units that attacked this phase."
  playableWhen \g pk ->
    isMyAttackWithDefenders g pk
      && any isUnitCard (playerOf pk g).discard
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    chooseFromCards pk 1 1 (filter isUnitCard me.discard)
      "Choose a unit in your discard pile to put into your battlefield." \chosen ->
        for_ chosen \c -> do
          putUnitIntoPlay pk FromDiscard c.key BattlefieldZone
          push ScheduleAttackerSacrifice
  where
    isUnitCard c = case c.def of
      UnitCardDef _ -> True
      _ -> False
    isMyAttackWithDefenders g pk = case g.combat of
      Just cs -> cs.attackingPlayer == pk && not (null cs.defenders)
      _ -> False

dominionOfChaos :: CardDef Quest
dominionOfChaos = questCard "the-ruinous-hordes-082" "Dominion of Chaos" do
  race Chaos
  cost 0
  loyalty 3
  trait Mission
  keyword PlayInOpponentArea
  body
    "Play in any opponent's zone under your control. \
    \When you assign combat damage to this zone, you may place any number of that combat damage on this quest instead. \
    \Forced: When the 3rd damage token is placed here, sacrifice this quest to corrupt up to 3 target units."
  onMyQuestTokensAdjusted \_owner self _delta ->
    withQuest self.key \q -> when (q.tokens >= 3) $
      withUpTo self.controller 3 (unitWhere (not . (.corrupted))) \chosen -> do
        traverse_ corrupt chosen
        destroyQuest self.key

-- ============================================================================
-- Empire (core-026 to core-050)
-- ============================================================================

spearmenOfWissenland :: CardDef Unit
spearmenOfWissenland = unitCard "core-026" "Spearmen of Wissenland" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Battlefield. This unit gains {power} while defending."

stateTroops :: CardDef Unit
stateTroops = unitCard "core-027" "State Troops" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  flavor "The backbone of the Empire's vast standing army."

templarOfSigmar :: CardDef Unit
templarOfSigmar = unitCard "core-028" "Templar of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Priest]
  body "Battlefield. Your other Warrior units gain {power} while in this zone."
  unitAura \_g self target ->
    if self.zone == BattlefieldZone
       && target.zone == BattlefieldZone
       && self.controller == target.controller
       && target.key /= self.key
       && Warrior `elem` target.cardDef.traits
       then 1
       else 0

witchHunter :: CardDef Unit
witchHunter = unitCard "core-029" "Witch Hunter" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Forced: When this unit enters play, destroy one target corrupted unit."
  onEnterPlay \_owner self ->
    withTarget self.controller (unitWhere (.corrupted)) \k ->
      destroyUnit k

greatswordsOfNuln :: CardDef Unit
greatswordsOfNuln = unitCard "core-030" "Greatswords of Nuln" do
  race Empire
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

knightsPanther :: CardDef Unit
knightsPanther = unitCard "core-031" "Knights Panther" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. Action: When this unit attacks, it gains {power}{power} for this attack."

reiksguard :: CardDef Unit
reiksguard = unitCard "core-032" "Reiksguard" do
  race Empire
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

rieklandMarksmen :: CardDef Unit
rieklandMarksmen = unitCard "core-033" "Riekland Marksmen" do
  race Empire
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  actionEnemyUnit "Shoot" 1 \_ k -> dealDamage k 1

thyrusGorman :: CardDef Unit
thyrusGorman = unitCard "core-034" "Thyrus Gorman" do
  unique
  race Empire
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to deal 2 damage to target unit."
  actionEnemyUnit "Cast" 2 \_ k -> dealDamage k 2

karlFranz :: CardDef Unit
karlFranz = unitCard "core-035" "Karl Franz" do
  unique
  race Empire
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Your other Empire units gain {power}."
  unitAura \_g self target ->
    if self.controller == target.controller
       && target.key /= self.key
       && Empire `elem` target.cardDef.races
       then 1
       else 0

volkmarTheGrim :: CardDef Unit
volkmarTheGrim = unitCard "core-036" "Volkmar the Grim" do
  unique
  race Empire
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Priest]
  body
    "Limit one Hero per zone.\n\
    \Forced: At the beginning of your turn, heal 2 damage from your capital."
  onMyTurnBegin \_owner self -> healCapital self.controller 2

mariusLeitdorf :: CardDef Unit
mariusLeitdorf = unitCard "core-037" "Marius Leitdorf" do
  unique
  race Empire
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self ->
    drawCard self.controller

lectorOfSigmar :: CardDef Unit
lectorOfSigmar = unitCard "core-038" "Lector of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Priest
  body "Kingdom. While in your kingdom, your capital gains +1 hit points in each zone."

imperialEngineers :: CardDef Unit
imperialEngineers = unitCard "core-039" "Imperial Engineers" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self -> drawCard self.controller

pegasusKnights :: CardDef Unit
pegasusKnights = unitCard "core-040" "Pegasus Knights" do
  race Empire
  cost 5
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. This unit can attack the turn it enters play."

theImperialCrown :: CardDef Support
theImperialCrown = supportCard "core-041" "The Imperial Crown" do
  unique
  race Empire
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Kingdom. Your Empire heroes cost 1 less to play."
  globalCostAdjust \_g s playing tgt ->
    if playing == s.controller
       && s.zone == KingdomZone
       && Empire `elem` tgt.cfRaces
       && Hero `elem` tgt.cfTraits
       then -1
       else 0

hammerOfSigmar :: CardDef Support
hammerOfSigmar = supportCard "core-042" "The Hammer of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Empire unit. Attached unit gains {power}{power}; its damage cannot be cancelled."
  attachmentPower 2
  grantsUncancellable

bannerOfSigmar :: CardDef Support
bannerOfSigmar = supportCard "core-043" "Banner of Sigmar" do
  race Empire
  cost 1
  loyalty 2
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}."
  attachmentPower 1

altdorf :: CardDef Support
altdorf = supportCard "core-044" "Altdorf" do
  unique
  race Empire
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Kingdom. While in play, non-combat damage to your capital is reduced by 1 (minimum 0)."

defendingTheEmpire :: CardDef Quest
defendingTheEmpire = questCard "core-045" "Defending the Empire" do
  race Empire
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token on this card if a unit is questing here.\n\
    \Action: Spend 3 resource tokens from this card to heal all damage on your capital."
  forced accrueTokenWhileQuesting
  spendTokens "Heal your capital" 3 \u ->
    healCapital u.user 99

forSigmar :: CardDef Tactic
forSigmar = tacticCard "core-046" "For Sigmar!" do
  race Empire
  cost 2
  loyalty 1
  body "Action: Each of your units gains {power} until the end of the turn."
  playableWhen $ hasTarget ownUnit
  whenResolved \self -> buffEachUntilEoT self.controller ownUnit 1

sigmarsWrath :: CardDef Tactic
sigmarsWrath = tacticCard "core-047" "Sigmar's Wrath" do
  race Empire
  cost 3
  loyalty 2
  body "Action: Deal 3 damage to target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> dealDamage k 3

counterCharge :: CardDef Tactic
counterCharge = tacticCard "core-048" "Counter-charge" do
  race Empire
  cost 1
  loyalty 2
  body "Play during combat. Action: Target defending unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget defendingUnit
  whenResolved \self ->
    withTarget self.controller defendingUnit \k ->
      until EndOfTurn $ buffPower k 2

battleOfTheReik :: CardDef Tactic
battleOfTheReik = tacticCard "core-049" "Battle of the Reik" do
  race Empire
  cost 2
  loyalty 2
  body "Action: Deal 1 damage to each attacking and each defending unit."
  playableWhen inCombat
  whenResolved \_ -> damageEachUnitInCombat 1

defendersOfTheFaith :: CardDef Tactic
defendersOfTheFaith = tacticCard "core-050" "Defenders of the Faith" do
  race Empire
  cost 1
  loyalty 1
  body "Action: Cancel up to 2 damage assigned to a unit you control."
  playableWhen $ hasTarget ownUnit
  whenResolved \self ->
    withTarget self.controller ownUnit \k ->
      cancelDamageOnUnit k 2

-- ============================================================================
-- High Elf (core-051 to core-075)
-- ============================================================================

phoenixGuard :: CardDef Unit
phoenixGuard = unitCard "core-051" "Phoenix Guard" do
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

whiteLionsOfChrace :: CardDef Unit
whiteLionsOfChrace = unitCard "core-052" "White Lions of Chrace" do
  race HighElf
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

swordmastersOfHoeth :: CardDef Unit
swordmastersOfHoeth = unitCard "core-053" "Swordmasters of Hoeth" do
  race HighElf
  cost 5
  loyalty 3
  power 4
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

highElfArchers :: CardDef Unit
highElfArchers = unitCard "core-054" "High Elf Archers" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  actionEnemyUnit "Loose arrow" 1 \_ k -> dealDamage k 1

seaGuardOfLothern :: CardDef Unit
seaGuardOfLothern = unitCard "core-055" "Sea Guard of Lothern" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior

silverHelms :: CardDef Unit
silverHelms = unitCard "core-056" "Silver Helms" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]

dragonPrincesOfCaledor :: CardDef Unit
dragonPrincesOfCaledor = unitCard "core-057" "Dragon Princes of Caledor" do
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

princeTyrion :: CardDef Unit
princeTyrion = unitCard "core-058" "Prince Tyrion" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 4
  hitPoints 5
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

teclis :: CardDef Unit
teclis = unitCard "core-059" "Teclis" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, draw 2 cards."
  onEnterPlay \_owner self -> drawCards self.controller 2

eltharionTheGrim :: CardDef Unit
eltharionTheGrim = unitCard "core-060" "Eltharion the Grim" do
  unique
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When an opponent's unit enters play, deal 1 damage to that unit."
  onOpponentUnitEnterPlay \_owner _self ukey ->
    dealDamage ukey 1

korhil :: CardDef Unit
korhil = unitCard "core-061" "Korhil" do
  unique
  race HighElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each other White Lion unit you control."
  selfPower \g u ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.key /= u.key
      , v.cardDef.code == CardCode "core-052"
      ]

loremasterOfHoeth :: CardDef Unit
loremasterOfHoeth = unitCard "core-062" "Loremaster of Hoeth" do
  race HighElf
  cost 4
  loyalty 2
  power 1
  hitPoints 3
  traits [Sorcerer]
  body "Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self ->
    drawCard self.controller

mageOfTheWhiteTower :: CardDef Unit
mageOfTheWhiteTower = unitCard "core-063" "Mage of the White Tower" do
  race HighElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to look at the top 3 cards of your deck."

spearmenOfLothern :: CardDef Unit
spearmenOfLothern = unitCard "core-064" "Spearmen of Lothern" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior

reaverKnights :: CardDef Unit
reaverKnights = unitCard "core-065" "Reaver Knights" do
  race HighElf
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

lighthouseOfLothern :: CardDef Support
lighthouseOfLothern = supportCard "core-066" "The Lighthouse of Lothern" do
  unique
  race HighElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Quest. Your quest zone gains +1 power."
  zonePowerAura \_g s zone ->
    if s.zone == QuestZone && zone == QuestZone then 1 else 0

bannerOfAvelorn :: CardDef Support
bannerOfAvelorn = supportCard "core-067" "Banner of Avelorn" do
  race HighElf
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target High Elf unit. Attached unit gains {power}{power}."
  attachmentPower 2

bowOfAvelorn :: CardDef Support
bowOfAvelorn = supportCard "core-068" "Bow of Avelorn" do
  race HighElf
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target High Elf unit. Action: Sacrifice this card to deal 2 damage to target unit."
  actionEnemyUnit "Loose arrow (sacrifice)" 0 \u k -> do
    dealDamage k 2
    destroySupport u.self.key

hoethsWisdom :: CardDef Support
hoethsWisdom = supportCard "core-069" "Hoeth's Wisdom" do
  race HighElf
  cost 2
  loyalty 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onMyTurnBegin \_owner self ->
    when (self.zone == KingdomZone) $
      drawCard self.controller

theWhiteTower :: CardDef Quest
theWhiteTower = questCard "core-070" "The White Tower" do
  race HighElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to draw 3 cards."
  forced accrueTokenWhileQuesting
  spendTokens "Draw 3 cards" 3 \u ->
    drawCards u.user 3

voiceOfCommand :: CardDef Tactic
voiceOfCommand = tacticCard "core-071" "Voice of Command" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Target unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ buffPower k 2

dragonBreath :: CardDef Tactic
dragonBreath = tacticCard "core-072" "Dragon Breath" do
  race HighElf
  cost 3
  loyalty 3
  traits [Spell]
  body "Action: Deal 2 damage to each enemy unit in target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 2

magicOfTheOldOnes :: CardDef Tactic
magicOfTheOldOnes = tacticCard "core-073" "Magic of the Old Ones" do
  race HighElf
  cost 1
  loyalty 1
  traits [Spell]
  body "Action: Draw 2 cards."
  whenResolved \self -> drawCards self.controller 2

battleMagic :: CardDef Tactic
battleMagic = tacticCard "core-074" "Battle Magic" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Deal 2 damage to target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> dealDamage k 2

sacredIncantations :: CardDef Tactic
sacredIncantations = tacticCard "core-075" "Sacred Incantations" do
  race HighElf
  cost 1
  loyalty 2
  traits [Spell]
  body "Action: Cancel a target tactic that is being played."

-- ============================================================================
-- Chaos (fill core-083..core-102 gaps)
-- ============================================================================

tzeentchSorcerer :: CardDef Unit
tzeentchSorcerer = unitCard "core-083" "Tzeentch Sorcerer" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  traits [Sorcerer]
  body "Action: Spend 2 resources to deal 1 damage to a target unit and draw a card."

slaaneshiMarauders :: CardDef Unit
slaaneshiMarauders = unitCard "core-084" "Slaaneshi Marauders" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior]
  body "Battlefield only."
  keyword BattlefieldOnly

plaguebearersOfNurgle :: CardDef Unit
plaguebearersOfNurgle = unitCard "core-085" "Plaguebearers of Nurgle" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 4
  trait Daemon
  body "Forced: When this unit damages an enemy unit in combat, corrupt that unit."
  corruptsOnDamage

festeringNurglings :: CardDef Unit
festeringNurglings = unitCard "core-086" "Festering Nurglings" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Creature]
  body "Forced: When this unit leaves play, corrupt target enemy unit."
  onSelfDestroyed \_owner self ->
    withTarget self.controller enemyUnit corrupt

archaonTheEverchosen :: CardDef Unit
archaonTheEverchosen = unitCard "core-088" "Archaon the Everchosen" do
  unique
  race Chaos
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit attacks, corrupt one defending unit."
  onMyAttackDeclared \_owner self _zone _attackers ->
    withTarget self.controller defendingUnit corrupt

chaosKnights :: CardDef Unit
chaosKnights = unitCard "core-089" "Chaos Knights" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

chaosWarriors :: CardDef Unit
chaosWarriors = unitCard "core-090" "Chaos Warriors" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

maraudersOfTheNorth :: CardDef Unit
maraudersOfTheNorth = unitCard "core-091" "Marauders of the North" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

chaosSorcerer :: CardDef Unit
chaosSorcerer = unitCard "core-093" "Chaos Sorcerer" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to corrupt target enemy unit."
  actionEnemyUnit "Corrupt" 2 \_ -> corrupt

horrorOfTzeentch :: CardDef Unit
horrorOfTzeentch = unitCard "core-094" "Horror of Tzeentch" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Forced: When this unit enters play, you may discard a card to deal 2 damage to a target unit."
  onEnterPlay \owner self -> when (not (null owner.hand)) do
    let pk = self.controller
    may pk "Discard a card to deal 2 damage to a target unit?" $
      withTarget pk AnyUnit \k -> do
        discardRandom pk
        dealDamage k 2

daemonettesOfSlaanesh :: CardDef Unit
daemonettesOfSlaanesh = unitCard "core-095" "Daemonettes of Slaanesh" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Battlefield. This unit cannot be assigned more than 1 damage per turn."
  perTurnDamageCap 1

beastsOfNurgle :: CardDef Unit
beastsOfNurgle = unitCard "core-096" "Beasts of Nurgle" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 5
  traits [Creature, Daemon]
  body "Forced: When this unit damages an enemy unit, corrupt that unit."
  corruptsOnDamage

chaosSpawn :: CardDef Unit
chaosSpawn = unitCard "core-097" "Chaos Spawn" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 3
  trait Creature
  body "Forced: At the end of your turn, deal 1 damage to this unit."
  onMyTurnEnd \_owner self -> dealDamage self.key 1

eyeOfTzeentch :: CardDef Support
eyeOfTzeentch = supportCard "core-098" "Eye of Tzeentch" do
  race Chaos
  cost 2
  loyalty 2
  traits [Attachment, Spell]
  body "Attach to a target Chaos unit. Attached unit gains {power}; you may draw a card whenever it attacks."
  attachmentPower 1

theIronTower :: CardDef Support
theIronTower = supportCard "core-099" "The Iron Tower" do
  unique
  race Chaos
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Chaos units gain {power} while in this zone."
  supportAura \_g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && u.zone == BattlefieldZone
       && Chaos `elem` u.cardDef.races
       then 1
       else 0

pyreOfTcharzanek :: CardDef Support
pyreOfTcharzanek = supportCard "core-100" "Pyre of Tchar'zanek" do
  race Chaos
  cost 2
  loyalty 2
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to a target zone."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      withTarget self.controller AnyCapital \(owner, z) ->
        dealZoneDamage owner z 1

tidesOfChaos :: CardDef Tactic
tidesOfChaos = tacticCard "core-101" "Tides of Chaos" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Corrupt target unit."
  playableWhen $ hasTarget uncorruptedUnit
  whenResolved \self ->
    withTarget self.controller uncorruptedUnit corrupt
  where
    uncorruptedUnit = unitWhere (not . (.corrupted))

doomOfTheEmpire :: CardDef Tactic
doomOfTheEmpire = tacticCard "core-102" "Doom of the Empire" do
  race Chaos
  cost 3
  loyalty 3
  body "Action: Deal 2 damage to target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(owner, z) ->
      dealZoneDamage owner z 2

-- ============================================================================
-- Orc (core-106 to core-130)
-- ============================================================================

grimgorIronhide :: CardDef Unit
grimgorIronhide = unitCard "core-106" "Grimgor Ironhide" do
  unique
  race Orc
  cost 6
  loyalty 5
  power 4
  hitPoints 5
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. Your other Orc units gain {power} while attacking."

skarsnik :: CardDef Unit
skarsnik = unitCard "core-107" "Skarsnik" do
  unique
  race Orc
  cost 4
  loyalty 3
  power 2
  hitPoints 3
  traits [Hero, Warrior]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, search the top 3 cards of your deck for a Goblin unit and put it into play. Shuffle your deck."

gorbadIronclaw :: CardDef Unit
gorbadIronclaw = unitCard "core-108" "Gorbad Ironclaw" do
  unique
  race Orc
  cost 5
  loyalty 4
  power 3
  hitPoints 4
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages a zone, deal 1 additional damage to that zone."
  -- Approximated as a flat +1 to its own combat damage while
  -- attacking; spillover to the zone scales naturally with that.
  combatPower \g u -> if unitIsAttacking g u then 1 else 0

orcBigUns :: CardDef Unit
orcBigUns = unitCard "core-109" "Orc Big 'Uns" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]

blackOrcs :: CardDef Unit
blackOrcs = unitCard "core-110" "Black Orcs" do
  race Orc
  cost 5
  loyalty 3
  power 4
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

savageOrcs :: CardDef Unit
savageOrcs = unitCard "core-111" "Savage Orcs" do
  race Orc
  cost 3
  loyalty 1
  power 3
  hitPoints 2
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

orcBoyz :: CardDef Unit
orcBoyz = unitCard "core-112" "Orc Boyz" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

boarBoyz :: CardDef Unit
boarBoyz = unitCard "core-113" "Boar Boyz" do
  race Orc
  cost 3
  loyalty 2
  power 3
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Battlefield only."
  keyword BattlefieldOnly

nightGoblins :: CardDef Unit
nightGoblins = unitCard "core-114" "Night Goblins" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior

goblinWolfRiders :: CardDef Unit
goblinWolfRiders = unitCard "core-115" "Goblin Wolf Riders" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

squigHoppers :: CardDef Unit
squigHoppers = unitCard "core-116" "Squig Hoppers" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Creature

orcShaman :: CardDef Unit
orcShaman = unitCard "core-117" "Orc Shaman" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit; deal 1 damage to this unit."
  actionEnemyUnit "Sorcery" 2 \u k -> do
    dealDamage k 2
    dealUncancellableDamage u.self.key 1

trolls :: CardDef Unit
trolls = unitCard "core-118" "Trolls" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 4
  trait Creature
  body "Forced: At the beginning of your turn, heal 1 damage from this unit."
  onMyTurnBegin \_owner self -> healUnit self.key 1

forestGoblinSpiderRiders :: CardDef Unit
forestGoblinSpiderRiders = unitCard "core-119" "Forest Goblin Spider Riders" do
  race Orc
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

snotlings :: CardDef Unit
snotlings = unitCard "core-120" "Snotlings" do
  race Orc
  cost 0
  loyalty 1
  power 1
  hitPoints 1
  trait Creature

daBadMoon :: CardDef Support
daBadMoon = supportCard "core-121" "Da Bad Moon" do
  unique
  race Orc
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your other Orc units gain {power} while attacking."
  -- "Battlefield." prefix scopes the aura to the controller's
  -- battlefield. The aura fires on combat damage, not on resting
  -- power, so it's a 'supportCombat' bonus.
  supportCombat \g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && Orc `elem` u.cardDef.races
       && unitIsAttacking g u
       then 1
       else 0

choppa :: CardDef Support
choppa = supportCard "core-122" "Choppa" do
  race Orc
  cost 1
  loyalty 1
  traits [Attachment, Weapon]
  body "Attach to a target Orc unit. Attached unit gains {power}{power}."
  attachmentPower 2

bigBossesBanner :: CardDef Support
bigBossesBanner = supportCard "core-123" "Big Boss's Banner" do
  race Orc
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target Orc unit. While attached unit is attacking, your other Orc attackers gain {power}."
  -- The banner buffs the host's other Orc attackers — never its own
  -- host. Grant +1 to any friendly Orc attacker different from the
  -- host while the host is itself attacking.
  supportCombat \g s u ->
    case s.attachedTo of
      Just hostKey
        | hostKey /= u.key
        , s.controller == u.controller
        , Orc `elem` u.cardDef.races
        , unitIsAttacking g u
        , Just host <- findUnit hostKey g
        , unitIsAttacking g host ->
            1
      _ -> 0

daMorksEye :: CardDef Support
daMorksEye = supportCard "core-124" "Da Mork's Eye" do
  race Orc
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to one target enemy zone."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      withTarget self.controller enemyCapital \(owner, z) ->
        dealZoneDamage owner z 1

orcWarmachine :: CardDef Support
orcWarmachine = supportCard "core-125" "Orc Warmachine" do
  race Orc
  cost 3
  loyalty 2
  trait Siege
  body "Battlefield. Action: Sacrifice a unit to deal 2 damage to a target zone."

greenskinRush :: CardDef Quest
greenskinRush = questCard "core-126" "Greenskin Rush" do
  race Orc
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 2 tokens to put a Goblin unit from your hand into play."
  forced accrueTokenWhileQuesting
  spendTokens "Summon a Goblin unit" 2 \u -> do
    me <- playerOf u.user <$> getGame
    let goblinsInHand = filter (cardIs goblinCodes) me.hand
    chooseFromCards u.user 0 1 goblinsInHand
      "Choose a Goblin unit to put into play." \chosen ->
        for_ chosen \c ->
          putUnitIntoPlay u.user FromHand c.key BattlefieldZone
  where
    goblinCodes =
      [ CardCode "core-114"  -- Night Goblins
      , CardCode "core-115"  -- Goblin Wolf Riders
      , CardCode "core-119"  -- Forest Goblin Spider Riders
      , CardCode "core-120"  -- Snotlings
      ]
    cardIs codes c = case c.def of
      UnitCardDef cd -> cd.code `elem` codes
      _ -> False

waaagh :: CardDef Tactic
waaagh = tacticCard "core-127" "Waaagh!" do
  race Orc
  cost 2
  loyalty 2
  body "Action: Each of your Orc units gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget ownOrcs
  whenResolved \self -> buffEachUntilEoT self.controller ownOrcs 2
  where
    ownOrcs = UnitMatching \pk _ u -> u.controller == pk && u `isRace` Orc

crushEm :: CardDef Tactic
crushEm = tacticCard "core-128" "Crush 'Em" do
  race Orc
  cost 1
  loyalty 1
  body "Action: Target unit gains {power}{power}{power} until the end of the turn; sacrifice it at end of turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ buffPower k 3
      queueEoTDamage k 99

runEmDown :: CardDef Tactic
runEmDown = tacticCard "core-129" "Run 'Em Down" do
  race Orc
  cost 2
  loyalty 1
  body "Action: Deal 1 damage to each enemy unit in target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 1

daBigStomp :: CardDef Tactic
daBigStomp = tacticCard "core-130" "Da Big Stomp" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Destroy target support card or development."
  playableWhen hasEnemySupport
  tacticTargets SupportTargetSchema
  onResolveEnemySupport \_pk s -> destroySupport s.key

-- ============================================================================
-- Dark Elf (core-131 to core-155)
-- ============================================================================

malekith :: CardDef Unit
malekith = unitCard "core-131" "Malekith" do
  unique
  race DarkElf
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy unit, corrupt that unit."
  -- Approximation: when a combat we participate in resolves, let Malekith's
  -- controller pick one defender to corrupt. Real card requires
  -- per-damage-event source tracking.
  onCombatResolveAsAttacker \_owner self _cs ->
    withTarget self.controller defendingUnit corrupt

morathi :: CardDef Unit
morathi = unitCard "core-132" "Morathi" do
  unique
  race DarkElf
  cost 5
  loyalty 4
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to corrupt one target unit."
  actionEnemyUnit "Corrupt" 2 \_ -> corrupt

croneHellebron :: CardDef Unit
croneHellebron = unitCard "core-133" "Crone Hellebron" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each Witch Elf unit you control."
  selfPower \g u ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.cardDef.code == CardCode "core-135"
      ]

lokhirFellheart :: CardDef Unit
lokhirFellheart = unitCard "core-134" "Lokhir Fellheart" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy zone, draw a card."
  -- Approximation: any combat we participate in where the attacker
  -- side wins damage to a zone (spillover) → draw. The damage-source
  -- granularity isn't tracked, so just fire whenever this unit is one
  -- of the attackers and combat resolves.
  onCombatResolveAsAttacker \_owner self _cs ->
    drawCard self.controller

witchElves :: CardDef Unit
witchElves = unitCard "core-135" "Witch Elves" do
  race DarkElf
  cost 3
  loyalty 2
  power 3
  hitPoints 1
  trait Warrior
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

blackGuardOfNaggarond :: CardDef Unit
blackGuardOfNaggarond = unitCard "core-136" "Black Guard of Naggarond" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

executioners :: CardDef Unit
executioners = unitCard "core-137" "Executioners" do
  race DarkElf
  cost 4
  loyalty 2
  power 4
  hitPoints 2
  traits [Warrior, Elite]
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

corsairs :: CardDef Unit
corsairs = unitCard "core-138" "Corsairs" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior
  body "Forced: When this unit damages an enemy zone, draw a card."
  -- Same approximation as Lokhir: fire on ResolveCombat when attacking.
  onCombatResolveAsAttacker \_owner self _cs ->
    drawCard self.controller

coldOneKnights :: CardDef Unit
coldOneKnights = unitCard "core-139" "Cold One Knights" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

darkRiders :: CardDef Unit
darkRiders = unitCard "core-140" "Dark Riders" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

darkSorceress :: CardDef Unit
darkSorceress = unitCard "core-141" "Dark Sorceress" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit."
  actionEnemyUnit "Cast" 2 \_ k -> dealDamage k 2

assassinsOfKhaine :: CardDef Unit
assassinsOfKhaine = unitCard "core-142" "Assassins of Khaine" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 1
  trait Warrior
  body "Forced: When this unit enters play, destroy one target unit with 1 hit point."
  onEnterPlay \_owner self ->
    withTarget self.controller (unitWhere \u -> u.effectiveMaxHP <= 1) \k ->
      destroyUnit k

repeaterCrossbowmen :: CardDef Unit
repeaterCrossbowmen = unitCard "core-143" "Repeater Crossbowmen" do
  race DarkElf
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to a target unit."
  actionEnemyUnit "Loose bolts" 1 \_ k -> dealDamage k 1

bloodwrackMedusa :: CardDef Unit
bloodwrackMedusa = unitCard "core-144" "Bloodwrack Medusa" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Creature
  body "Forced: When this unit enters play, deal 1 damage to each enemy unit in target zone."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 1

blackDragon :: CardDef Unit
blackDragon = unitCard "core-145" "Black Dragon" do
  race DarkElf
  cost 6
  loyalty 3
  power 4
  hitPoints 5
  trait Creature
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

manticore :: CardDef Unit
manticore = unitCard "core-146" "Manticore" do
  race DarkElf
  cost 5
  loyalty 2
  power 4
  hitPoints 3
  trait Creature
  body "Battlefield only."
  keyword BattlefieldOnly

cauldronOfBlood :: CardDef Support
cauldronOfBlood = supportCard "core-147" "Cauldron of Blood" do
  unique
  race DarkElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Witch Elf units gain {power}."
  supportAura \_g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && u.cardDef.code == CardCode "core-135"
       then 1
       else 0

theBlackArk :: CardDef Support
theBlackArk = supportCard "core-148" "The Black Ark" do
  unique
  race DarkElf
  cost 3
  loyalty 3
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onMyTurnBegin \_owner self ->
    when (self.zone == KingdomZone) $
      drawCard self.controller

whipOfAgony :: CardDef Support
whipOfAgony = supportCard "core-149" "Whip of Agony" do
  race DarkElf
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Dark Elf unit. Attached unit gains {power}{power}."
  attachmentPower 2

druchiiBanner :: CardDef Support
druchiiBanner = supportCard "core-150" "Druchii Banner" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}; opponents pay 1 additional resource to target it."
  attachmentPower 1

witchbrew :: CardDef Support
witchbrew = supportCard "core-151" "Witchbrew" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target Dark Elf unit. Action: Sacrifice this card to give attached unit {power}{power}{power} until the end of the turn."
  action "Brew (sacrifice)" 0 \u ->
    for_ u.self.attachedTo \hostKey -> do
      until EndOfTurn $ buffPower hostKey 3
      destroySupport u.self.key

slaughterAtLustria :: CardDef Quest
slaughterAtLustria = questCard "core-152" "Slaughter at Lustria" do
  race DarkElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to corrupt up to 2 target units."
  forced accrueTokenWhileQuesting
  spendTokens "Corrupt up to 2 units" 3 \u ->
    withUpTo u.user 2 (unitWhere (not . (.corrupted))) (traverse_ corrupt)

khainesEmbrace :: CardDef Tactic
khainesEmbrace = tacticCard "core-153" "Khaine's Embrace" do
  race DarkElf
  cost 2
  loyalty 2
  body "Action: Destroy target unit with 2 or fewer hit points."
  playableWhen $ hasTarget weakUnit
  whenResolved \self ->
    withTarget self.controller weakUnit \k -> destroyUnit k
  where
    weakUnit = unitWhere \u -> u.effectiveMaxHP <= 2

murderousProwess :: CardDef Tactic
murderousProwess = tacticCard "core-154" "Murderous Prowess" do
  race DarkElf
  cost 1
  loyalty 2
  body "Action: Each of your Dark Elf units gains {power} until the end of the turn."
  playableWhen $ hasTarget ownDarkElves
  whenResolved \self -> buffEachUntilEoT self.controller ownDarkElves 1
  where
    ownDarkElves = UnitMatching \pk _ u -> u.controller == pk && u `isRace` DarkElf

coldBloodedSlaughter :: CardDef Tactic
coldBloodedSlaughter = tacticCard "core-155" "Cold Blooded Slaughter" do
  race DarkElf
  cost 3
  loyalty 3
  body "Action: Deal damage equal to your hand size to a target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    withTarget pk AnyUnit \k -> dealDamage k (length me.hand)

