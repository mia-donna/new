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

data Type = Payee | Recipient deriving (Show, Eq)

typeFlip :: IO Type
typeFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Payee else Recipient

main :: IO ()
main = do
    -- create customers
    putStrLn $ "4 customers created."
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}
    let c4 = Customer {name = "C4", balance = 100, account = 4}

   -- create an empty box for each customer
    one <- newEmptyMVar
    two <- newEmptyMVar
    three <- newEmptyMVar
    four <- newEmptyMVar
    value <- newEmptyMVar -- for value
    --typebox1 <- newEmptyMVar -- for random customer type
    --typebox2 <- newEmptyMVar -- for random customer type
    --typebox3 <- newEmptyMVar -- for random customer type
    --typebox4 <- newEmptyMVar -- for random customer type
    typebox <- newEmptyMVar -- for random customer type
    list <- newEmptyMVar -- for bool list
    -- fork the customer processes
    putStrLn $ "4 customer threads being created."
    mapM_ forkIO [process c1 one value typebox , process c2 two value typebox , process c3 three value typebox, process c4 four value typebox ] 
    -- create a box for each process
    c <- takeMVar one
    d <- takeMVar two
    e <- takeMVar three
    f <- takeMVar four
    putStrLn $ "hi " ++ (show c)
    putStrLn $ "hi " ++ (show d)
    putStrLn $ "hi " ++ (show e)
    putStrLn $ "hi " ++ (show f)
    --test <- takeMVar typebox1 -- only runs if c1 gets Payee
    test <- takeMVar typebox  

    putStrLn $ "type test " ++ (show test)
    putStrLn $ "hi " ++ (show c) 
    putStrLn $ "hi " ++ (show d) 
    putStrLn $ "hi " ++ (show e) 
    putStrLn $ "hi " ++ (show f) 
    
    putStrLn $ "done"

-- CUSTOMER THREAD PROCESS : picks random type, puts the type in a type box and customer in a customer box
process :: Customer -> MVar Customer -> MVar Value -> MVar Type -> IO () 
process cust custbox value typebox = do
    --v <- takeMVar value -- just a method to run it / blocks it if enabled
    t1 <- typeFlip
    putMVar custbox cust
    putStrLn $ (show cust) ++ " -- got " ++ (show t1)
    if t1 == Payee then do
        putStrLn $ (show cust) ++ " -- joining Payee group"
        putMVar typebox Payee
    else do 
       putStrLn $ (show cust) ++ " -- joining Recipient group"   
       putMVar typebox Recipient


randomN :: IO Int 
randomN = do
    r <- randomRIO (1, 4)
    return r






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


