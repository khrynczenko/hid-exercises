{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
    fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader e) where
    pure ::  a -> Reader e a
    pure x = Reader (\_ -> x)

    (<*>) :: Reader e (b -> c) -> Reader e b -> Reader e c
    Reader f <*> Reader g = Reader (\e -> (f e (g e)))

instance Monad (Reader e) where
    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    Reader f >>= g = Reader (\e -> (runReader (g (f e)) e))

ask :: Reader a a
ask = Reader id

asks :: (e -> a) -> Reader e a
asks f = Reader f

newtype Writer w a = Writer { runWriter :: (w -> (w, a))}

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer g) = Writer (\w -> (fst (g w), f $ snd (g w)))

instance Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (\w -> (w, a))

    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (<*>) (Writer f) (Writer g) = 
        Writer (\w -> (fst $ f $ fst (g w), snd (f w) $ (snd $ g w)))

instance Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (>>=) (Writer f) g =
        Writer (\w -> (fst $ runWriter (g $ snd (f w)) (fst (f w)), snd $ runWriter (g $ snd (f w)) w))

tell :: Semigroup w => w  -> Writer w ()
tell writeLog = Writer (\w -> (w <> writeLog, ()))


data Config = Config {
    verbose :: Bool
}

getConfiguration :: IO Config
getConfiguration = pure $ Config { verbose = True }

-- Without Reader
--work :: Config -> String
--work config = doSomething config

--doSomething :: Config -> String
--doSomething config = if verbose config then "Verbose" else "Not verbose"

--main :: IO ()
--main = 
    --getConfiguration >>= (\config -> putStrLn $ work config)

-- With Reader
--work :: Reader Config String
--work = doSomething

--doSomething :: Reader Config String
--doSomething = 
    --asks verbose >>= (\ver -> pure $ if ver then "Verbose" else "Not verbose")

--main :: IO ()
--main = do
    --config <- getConfiguration
    --let result = runReader work config
    --putStrLn result
    --
    --

-- With Writer
type Log = String

work :: Writer Log String
work = do
    doSomething
    doSomething
    doSomething

doSomething :: Writer Log String
doSomething = 
    tell "Call 1" >>= (\_ -> pure "Verbose")

main :: IO ()
main = do
    let (log, result) = runWriter work ""
    putStrLn log
    putStrLn result



