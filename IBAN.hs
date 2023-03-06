м--
-- INFOB3CC Concurrency
-- Practical 1: IBAN calculator
--
-- http://www.cs.uu.nl/docs/vakken/b3cc/assessment.html
--
-- By: Bram Lankhorst (6885705)
-- Attemped all three questions (Count,List,Search)

module IBAN (

  Mode(..), Config(..),
  count, list, search

) where

import Control.Concurrent
import Crypto.Hash.SHA1
import Data.IORef
import Data.List                                          ( elemIndex )
import Data.Word
import Data.Maybe                                         ( fromJust )
import System.Environment
import System.IO
import Data.ByteString.Char8                              ( ByteString )
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import Control.Monad

-- -----------------------------------------------------------------------------
-- 0. m-test
-- -----------------------------------------------------------------------------
                      
mtest:: Int -> Int -> Bool
mtest m x = (mtestHelp x `mod` m) == 0

--makes list of each digit times its multiplication, and takes the sum 
mtestHelp :: Int -> Int
mtestHelp n = sum (zipWith (*) d w) 
  where d = toDigits n 
        w = [1..9]

--makes list of digits 
toDigits :: Int -> [Int]
toDigits 0 = []
toDigits x = 
  let (d,m) = divMod x 10
   in m : toDigits d
-- -----------------------------------------------------------------------------
-- 1. Counting mode (3pt)
-- -----------------------------------------------------------------------------

count :: Config -> IO Int
count config@(Config _ _ _ threads) =  do
  counter <- newIORef 0
  forkThreads threads (doWork counter config)
  readIORef counter

doWork:: IORef Int -> Config -> Int -> IO()
doWork counter (Config b e m threads) threadID = do
  -- divide in parts: e.g. 4 threads then thread 0 has numbers 0,4,8,... and thread 1 numbers 1,5,9,... etc. (with range from 0)
  forM_ [begin, (begin + threads) .. (e-1)] $ \n -> when (mtest m n) (changeCount counter) --if m-test is correct, change the counter
  where 
    begin = b + mod threadID threads --b + threads * threadID, vanaf b tot e. 

-- using the IORef counter as a lock with atomicCAS:                       
changeCount :: IORef Int -> IO ()
changeCount c = do 
  i <- readIORef c
  unlocked <- atomicCAS c i (i+1) --only change to (i+1) if old value is i (nothing changed in between)
  if unlocked
   then return () 
   else changeCount c --if we cannot unlock / change counter, then try again

-- -----------------------------------------------------------------------------
-- 2. List mode (3pt)
-- -----------------------------------------------------------------------------

list :: Config -> IO ()
list config@(Config _ _ _ threads) =  do
  counter <- newMVar 0 --need to print sequence number so need a counter as well
  forkThreads threads (doWork' counter config)

doWork':: MVar Int -> Config -> Int -> IO()
doWork' counter (Config b e m threads) threadID = do 
  forM_ [begin, (begin + threads) .. (e-1)] $ \n -> when (mtest m n) (printMVar counter n)
  where begin = b + mod threadID threads

--counter is a MVar so can use it as a lock as well (by using takeMVar and putMVar)
printMVar :: MVar Int -> Int -> IO ()
printMVar c n = do 
  i <- takeMVar c 
  --need to print immediately, before putMVar, so that other threads cannot do anything with the counter in the mean time
  putStrLn $ show (i + 1) ++ " " ++ show n
  putMVar c (i + 1)

-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------

--queue implementation
data Queue a =
  Queue (MVar (List a)) (MVar (List a))

type List a = MVar (Item a)
data Item a = Item a (List a)

newQueue :: IO (Queue a)
newQueue = do
 hole <- newEmptyMVar
 readEnd <- newMVar hole
 writeEnd <- newMVar hole
 return (Queue readEnd writeEnd)

enqueue :: Queue a -> a -> IO ()
enqueue (Queue _ writeEnd) val = do
 newHole <- newEmptyMVar
 oldHole <- takeMVar writeEnd
 putMVar oldHole (Item val newHole)
 putMVar writeEnd newHole

dequeue :: Queue a -> IO (Maybe a)
dequeue (Queue readEnd _) = do
  oldHole <- takeMVar readEnd
  empty <- isEmptyMVar oldHole
  if empty 
    then do 
      putMVar readEnd oldHole --put back old readEnd
      return Nothing --empty so can not return value
    else do
      (Item val newHole) <- takeMVar oldHole 
      putMVar readEnd newHole --change readEnd 
      return $ Just val 


search :: Config -> ByteString -> IO ()
search config@(Config b e _ threads) query = do
  q <- newQueue
  enqueue q (b,e)
  tasks <- newMVar 1 --counter for the ammount of pending tasks on queue
  found <- newMVar False --checks if the corresponding bank account is found
  forkThreads threads (doSearch config q tasks found query)
  f <- readMVar found
  --here all threads are done, if they have not found the account number it should print 'not found'
  unless f (putStrLn "not found") 
  
doSearch :: Config -> Queue (Int,Int) -> MVar Int -> MVar Bool -> ByteString -> Int -> IO()
doSearch (Config _ _ m threads) q tasks found query _ = do
  i <- takeMVar tasks
  if i == 0 
    then do 
      putMVar tasks 0
      return() 
    else do
      range <- dequeue q 
      putMVar tasks (i-1) --dequeue, so decrement the amount of tasks remaining
      let (from,till) = fromJust range
          part = div (till - from) threads --part that one thread should work on
          mypart = minimum [part, 50000] --if part > 50000, just take a part of 50000 (otherwise to large parts / slower)
          remainder = till - (from + mypart) 
      if (till - from) < 50000 
        then do --cut-off, work is sufficiently small that thread should do it alone
          searchHash (from,till) query m found
        else do --otherwise do a part (mypart) previously calculated
          searchHash (from,from + mypart) query m found
          t <- takeMVar tasks
          enqueue q (from + mypart, div remainder 2) --put remaining numbers in two parts back on queue
          enqueue q (div remainder 2, till)
          putMVar tasks (t+2) --enqueue so increment amount of tasks

--checks if m-test holds, if it does check if the value corresponds to the hash
searchHash :: (Int,Int) -> ByteString -> Int -> MVar Bool -> IO()
searchHash (first,last) query m found = do
  forM_ [first .. last] $ \n -> when (mtest m n) (when (checkHash query (show n)) (do
    putStrLn $ show n
    takeMVar found
    putMVar found True))

-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------

data Mode = Count | List | Search ByteString
  deriving Show

data Config = Config
  { cfgLower   :: !Int
  , cfgUpper   :: !Int
  , cfgModulus :: !Int
  , cfgThreads :: !Int
  }
  deriving Show

-- Evaluates a term, before continuing with the next IO operation.
--
evaluate :: a -> IO ()
evaluate x = x `seq` return ()

-- Atomic compare-and-swap
--
-- Takes a reference, the expected value and a desired value. Replaces the
-- value in the reference with the desired value, if it contained
-- 'expected'. Returns whether the operation was successful.
--
atomicCAS
    :: IORef Int    -- ^ reference
    -> Int          -- ^ expected value
    -> Int          -- ^ desired value
    -> IO Bool
atomicCAS ref expected desired =
  atomicModifyIORef' ref $ \old ->
    if old == expected
       then (desired, True)
       else (old, False)

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var

-- Checks whether 'value' has the expected hash.
--
checkHash :: ByteString -> String -> Bool
checkHash expected value = expected == hash (B8.pack value)

