{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import App
import Cmd
import Deck
import Helpers

import Control.Monad.State
import Data.Maybe as Maybe
import System.IO


-- IO

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  runStateT mainLoop initData

mainLoop :: StateT AppData IO ()
mainLoop = do
  inputChar <- liftIO $ getChar
  case parseMsg inputChar of
    Left e -> liftIO $ putStrLn e
    Right msg -> handleMsg msg
  mainLoop

handleMsg :: Msg -> StateT AppData IO ()
handleMsg msg = do
  app <- get
  let (app', cmd) = handleMsgStep msg app
  put app'
  foldl (>>) (return ()) $ do  -- in list Monad
      io <- runCmd cmd  -- `io` :: IO (Either () Msg)
      return $ do
        msg' <- liftIO io
        case msg' of
          Left () -> return ()
          Right msg -> handleMsg msg

parseMsg :: Char -> Either String Msg
parseMsg char = case char of
  'a' -> Right ToggleAddingCard
  'r' -> Right RestartDeck
  'f' -> Right FlushHand
  'd' -> Right ShowDeck
  'h' -> Right ShowHand
  'u' -> Right Undo
  x -> maybeToEither "Error: Invalid command!" $ fmap NewCard $ parseChar x

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
