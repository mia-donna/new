-- Your project should define an appropriate Haskell data type Customer,
-- which should include their name, account number, and account balance.

--Your project should also contain a main thread which creates ten “customers” (ten values of type Customer),
-- and spawns ten threads, one for each of these customers. 

--Each customer thread should behave as follows: at random time intervals,
-- the thread should select one of the other customers at random,
-- and transfer a random amount of money (between £10 and £50)  into their account. 
--The amount transferred should not be more than what that customer has available 
--in their account balance (account balances should not be negative).

module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Control.Monad
import System.IO

data Customer = Customer {
  name :: Name,
  balance :: Balance,
  account :: Account
} deriving (Eq, Show)

type Account = Int
type Balance =  Int
type Name = String
type Value = Int


main :: IO ()
main = do
    -- create customers
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}


   -- create an empty box for each customer
    one <- newEmptyMVar
    two <- newEmptyMVar
    three <- newEmptyMVar
    value <- newEmptyMVar -- for value
    -- fork the customer processes
    mapM_ forkIO [process c1 one value, process c2 two value, process c3 three value]
    c <- takeMVar one
    d <- takeMVar two
    e <- takeMVar three
    putStrLn $ "hi " ++ (show c)
    putStrLn $ "hi " ++ (show d)
    putStrLn $ "hi " ++ (show e)

process :: Customer -> MVar Customer -> MVar Value -> IO () 
process cust custbox value = do
    --v <- takeMVar value -- just a method to run it / blocks it if enabled
    putMVar custbox cust
    threadDelay 100
    process cust custbox value




-------------------------------------------------------

-- customers :: [MVar Bool]   -- this will collect all the customer MVars
-- value :: MVar Int          -- this will hold the value -- shared by all threads


-- ACTIONS IN MAIN THREAD
-- 1. Create 10 customers
-- 2. Create 10 customer processes/threads
-- 3. Pick two customer MVars (True, False)
-- So need to add the customer MVars into a list of MVars of type Bool, then use a random selector to pick them
--  

-- ACTIONS IN CUSTOMER THREAD
-- Flip a coin (all customers get a result - like in concurrency app)
-- If True then == recipient
-- If False == then payee
-- first one to both is the match
-- need to put their result in a box

randomN :: IO Int 
randomN = do
    r <- randomRIO (1, 6)
    return r
