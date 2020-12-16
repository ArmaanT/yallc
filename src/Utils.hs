{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
  )
import QuickCheck.GenT (GenT, MonadGen (..), getSize)

runSimulator :: (a -> Either String Int) -> a -> IO ()
runSimulator sim prog = do
  case sim prog of
    Left x -> putStrLn $ "Error: " ++ x
    Right x -> putStrLn $ "Result: " ++ show x

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen
  variant _ s = s
  resize _ s = s
  sized f = do
    a <- getSize
    f a
  choose = lift . choose

instance MonadState s m => MonadState s (GenT m) where
  get = lift get
  put = lift . put
