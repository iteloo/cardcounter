module App (
      AppData
    , initData
    , Msg(..)
    , handleMsgStep
  ) where

import Cmd
import Deck hiding (addCard, removeCard)
import qualified Deck (addCard, removeCard)
import CardDist
import Helpers

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map


-- app data and operations

data AppData = AppData {
    deck :: Deck,
    hand :: Hand,
    addingCard :: Bool,  -- [problem] possibly changes when undoing
    undoStack :: [AppData],
    redoStack :: [AppData]
  }

initData = AppData {
    deck = fullDeck,
    hand = [],
    addingCard = False,
    undoStack = [],
    redoStack = []
  }

addCardToDeck c app = app { deck = Deck.addCard c $ deck app }

removeCardFromDeck c app =
  fmap (\d -> app { deck = d }) $ Deck.removeCard c $ deck app

restartDeck app = app { deck = fullDeck }

addCardToHand c app = app { hand = c:hand app }

flushHand app = app { hand = [] }

toggleAddingCard app = app { addingCard = not $ addingCard app }

undo app =
  let (h, _) = pop $ undoStack app
  in case h of
    Nothing -> Left "Nothing to undo."
    Just h -> Right $ h { redoStack = push app $ redoStack app }

-- [todo] add last msg
saveState app = app { undoStack = push app $ undoStack app }

pop xs = if null xs then (Nothing, xs) else (Just $ head xs, tail xs)

push = (:)

-- Msgs and handlers

data Msg =
  Undo
  | RestartDeck
  | RemoveCardFromDeck Card
  | FlushHand
  | AddCardToHand Card
  | ToggleAddingCard
  | NewCard Card
  | ShowDeck
  | ShowHand

handleMsgStep :: Msg -> AppData -> (AppData, Cmd Msg)
handleMsgStep msg app = (\(x,y)->(y,x)) $ runState (handleMsgStep' msg) app

handleMsgStep' :: Msg -> State AppData (Cmd Msg)
handleMsgStep' msg = case msg of
  Undo -> do
    result <- tryUpdate $ undo
    return $ case result of
      Left e -> logMsg $ show e
      Right _ -> logMsg $ "Undid last action"
  ShowDeck ->
    get >>= return . logMsg . (++) "Current deck: " . show . Map.toList . deck
  ShowHand ->
    get >>= return . logMsg . (++) "Current hand: " . show . hand
  ToggleAddingCard -> do
    modify $ toggleAddingCard
    app <- get
    return $ logMsg $ if addingCard app
          then "Started adding card"
          else "Stopped adding card"
  -- the following commands can be undone
  msg -> do
    modify $ saveState
    case msg of
      RestartDeck -> do
        modify $ restartDeck
        return $ logMsg $ "Deck restarted"
      FlushHand -> do
        modify $ flushHand
        return $ logMsg $ "Hand flushed"
      NewCard c -> do
        cmd <- handleMsgStep' $ RemoveCardFromDeck c
        app <- get
        cmd' <- if addingCard app
          then handleMsgStep' $ AddCardToHand c
          else return noCmd
        return $ batch [cmd, cmd']
      RemoveCardFromDeck c -> do
        result <- tryUpdate $ removeCardFromDeck c
        return $ logMsg $ case result of
          -- [problem] this null action is saved
          Left e -> show e
          Right _ -> "Removed " ++ show c ++ " from deck"
      AddCardToHand c -> do
        modify $ addCardToHand c
        let cmd = logMsg $ "Added " ++ show c ++ " to hand"
        cmd' <- handleMsgStep' ShowHand
        app <- get
        return $ batch [
            cmd
          , cmd'
          , logMsg
              $ "Should add card: "
              ++ (show $ shouldAdd1Full (deck app) (hand app))
          ]

-- helpers

tryUpdate :: MonadState s m => (s -> Either String s) -> m (Either String ())
tryUpdate f = do
    x <- get
    case f x of
      Right y -> do
        put y
        return $ Right ()
      Left e -> return $ Left $ "Error: Cannot update state due to: " ++ e

update :: MonadState s m => (s -> Maybe s) -> m (Maybe ())
update f = do
    x <- get
    case f x of
      Just y -> do
        put y
        return $ Just ()
      Nothing -> return Nothing
