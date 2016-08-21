module Cmd (
      Cmd  -- constructors private
    , runCmd
    , batch
    , noCmd
    , logMsg
  ) where

-- [problem] not extensible, dependent on executable environment

data Cmd msg =
    Single (IO (Either () msg))
  | Batch [Cmd msg]

runCmd :: Cmd msg -> [IO (Either () msg)]
runCmd (Single io) = [io]
runCmd (Batch cmds) = concatMap runCmd cmds

batch :: [Cmd msg] -> Cmd msg
batch = Batch

noCmd :: Cmd msg
noCmd = Single $ return $ Left ()

logMsg :: String -> Cmd msg
logMsg = Single . fmap Left . putStrLn
