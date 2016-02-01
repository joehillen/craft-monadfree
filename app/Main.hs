{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad.Trans.Free
import           Control.Monad.Reader
import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Control.Monad.Trans.Class as Trans

type StdOut = String
type StdErr = String
type Args = [String]
type Command = String

data CraftEnv
  = CraftEnv
    {
      --_craftPackageManager :: PackageManager
      craftSourcePaths    :: [String]
    }

-- type Craft m a = (MonadFree CraftDSL m, MonadReader CraftEnv m) => m a

data CraftDSL next
  = Exec  CraftEnv Command Args (Bool -> next)
  | FileRead CraftEnv FilePath (String -> next)
  | FileWrite CraftEnv FilePath String next
  deriving Functor


newtype Craft a = Craft { unCraft :: ReaderT CraftEnv (FreeT CraftDSL (LoggingT IO)) a }
  deriving ( Functor, Monad, MonadIO, Applicative
           , MonadReader CraftEnv, MonadFree CraftDSL, MonadThrow, MonadLogger)


instance (MonadLogger m, Functor f) => MonadLogger (FreeT f m) where
  monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d



interpretCraft :: CraftEnv -> (CraftDSL (LoggingT IO a) -> LoggingT IO a) -> Craft a -> LoggingT IO a
interpretCraft ce interpreter = iterT interpreter . flip runReaderT ce . unCraft

runCraftLocal :: CraftEnv -> Craft a -> LoggingT IO a
runCraftLocal ce' = interpretCraft ce' run
 where
  run (Exec ce cmd args next) = do
    logDebugN "logging in exec"
    Trans.lift $ putStrLn $ unwords ("Exec":cmd:args)
    next True
  run (FileWrite ce fp s next ) = do
    Trans.lift $ putStrLn $ unwords ["FileWrite", fp, s]
    next
  run (FileRead ce fp next) = do
    Trans.lift $ putStrLn $ unwords ["FileRead", fp]
    next "derp"


exec :: Command -> Args -> Craft Bool
exec cmd args = do
  ce <- ask
  liftF $ Exec ce cmd args id


fileRead :: FilePath -> Craft String
fileRead fp = do
  ce <- ask
  liftF $ FileRead ce fp id


fileWrite :: FilePath -> String -> Craft ()
fileWrite fp content = do
  ce <- ask
  liftF $ FileWrite ce fp content ()


myscript :: Craft Bool
myscript = do
  herpderp <- fileRead "/foobar"
  fileWrite "/foo/bar/baz" "foobar"
  exec "foo" ["bar", "baz"]
  logInfoN "logging in the script"
  throwM MyError
  return True


data MyError = MyError
  deriving (Show)
instance Exception MyError

main :: IO ()
main = do
  r <- runStdoutLoggingT $ runCraftLocal (CraftEnv []) myscript
  print r
  return ()
