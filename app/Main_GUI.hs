{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Deck hiding (addCard, removeCard)
import qualified Deck (addCard, removeCard)
import CardDist
import Helpers

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe as Maybe
import System.IO

import Graphics.UI.WX hiding (Event, get)
import Reactive.Banana
import Reactive.Banana.WX


-- IO

main = start $ do
    f       <- frame [text := "Counter"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    output  <- staticText f []

    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]

    let networkDescription :: MomentIO ()
        networkDescription = do

          eup   <- event0 bup   command
          edown <- event0 bdown command

          (counter :: Behavior Int)
              <- accumB 0 $ unions
                  [ (+1)       <$ eup
                  , subtract 1 <$ edown
                  ]

          sink output [text :== show <$> counter]

    network <- compile networkDescription
    actuate network

main' = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  runStateT mainLoop initData

mainLoop :: StateT AppData IO ()
mainLoop = do
  inputChar <- liftIO $ getChar
  case parseCmd inputChar of
    Left e -> liftIO $ putStrLn e
    Right cmd -> handleCmd cmd
  mainLoop

data Cmd =
  ToggleAddingCard
    | RestartDeck
    | FlushHand
    | NewCard Card
    | ShowDeck
    | ShowHand
    | Undo

data AppData = AppData {
    deck :: Deck,
    hand :: Hand,
    addingCard :: Bool,
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

addCardToHand c app = app { hand = c:hand app }

toggleAddingCard app = app { addingCard = not $ addingCard app }

restartDeck app = app { deck = fullDeck }

flushHand app = app { hand = [] }

isAddingCard = fmap addingCard get

undo app =
  let (h, _) = pop $ undoStack app
  in case h of
    Nothing -> Left "Nothing to undo."
    Just h -> Right $ h { redoStack = push app $ redoStack app }

-- [todo] add last cmd
saveState app = app { undoStack = push app $ undoStack app }

pop xs = if null xs then (Nothing, xs) else (Just $ head xs, tail xs)

push = (:)

parseCmd :: Char -> Either String Cmd
parseCmd char = case char of
  'a' -> Right ToggleAddingCard
  'r' -> Right RestartDeck
  'f' -> Right FlushHand
  'd' -> Right ShowDeck
  'h' -> Right ShowHand
  'u' -> Right Undo
  x -> maybeToEither "Error: Invalid command!" $ fmap NewCard $ parseChar x

handleCmd :: Cmd -> StateT AppData IO ()
handleCmd cmd = case cmd of
  Undo -> do
    result <- tryUpdate undo
    case result of
      Left e -> liftIO $ putStrLn $ show e
      Right _ -> liftIO $ putStrLn $ "Undid last action"
  ShowDeck ->
    get >>= liftIO . putStrLn
      . (++) "Current deck: " . show . Map.toList . deck
  ShowHand ->
    get >>= liftIO . putStrLn
      . (++) "Current hand: " . show . hand
  ToggleAddingCard -> do
      modify $ toggleAddingCard
      adding <- isAddingCard
      liftIO $ putStrLn
        $ if adding then "Started adding card" else "Stopped adding card"
  -- the following commands can be undone
  cmd -> do
    modify saveState
    case cmd of
      RestartDeck -> do
          modify $ restartDeck
          liftIO $ putStrLn $ "Deck restarted"
      FlushHand -> do
          modify $ flushHand
          liftIO $ putStrLn $ "Hand flushed"
      NewCard c -> do
        adding <- isAddingCard
        result <- tryUpdate $ removeCardFromDeck c
        case result of
          -- [problem] this null action is saved
          Left e -> liftIO $ putStrLn $ show e
          Right _ -> liftIO $ putStrLn $ "Removed " ++ show c ++ " from deck"
        if adding
          then do
            modify $ addCardToHand c
            liftIO $ putStrLn $ "Added " ++ show c ++ "to hand"
            handleCmd ShowHand
            app <- get
            liftIO $ putStrLn
              $ "Should add card: "
              ++ (show $ shouldAdd1Full (deck app) (hand app))
          else return ()

parseChar :: Char -> Maybe Card
parseChar char = case char of
  '1' -> Just N1
  '2' -> Just N2
  '3' -> Just N3
  '4' -> Just N4
  '5' -> Just N5
  '6' -> Just N6
  '7' -> Just N7
  '8' -> Just N8
  '9' -> Just N9
  '0' -> Just N10
  'j' -> Just J
  'q' -> Just Q
  'k' -> Just K
  'l' -> Just Joker
  _ -> Nothing

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
