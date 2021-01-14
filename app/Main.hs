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
type List = [MVar Type]

data Type = Payee | Recipient deriving (Show, Eq)

randomN :: IO Int 
randomN = do
    r <- randomRIO (1, 4)
    return r

-- CUSTOMER THREAD PROCESS : picks random type, puts the type in a type box and customer in a customer box
process :: Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Customer -> MVar Bool -> IO () 
process cust custbox value typebox_p typebox_r bool = do
    --v <- takeMVar value -- just a method to run it / blocks it if enabled
    t1 <- typeFlip
    
    putMVar custbox cust
    putStrLn $ (show cust) ++ " -- got " ++ (show t1) 
    if t1 == Payee then do
        putStrLn $ (show cust) ++ " -- joining Payee group"
        putMVar typebox_p cust
        putMVar bool True
    else do 
       putStrLn $ (show cust) ++ " -- joining Recipient group"   
       putMVar typebox_r cust
       putMVar bool False

typeFlip :: IO Type
typeFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Payee else Recipient


randomCustomer :: IO Int 
randomCustomer = do
    r <- randomRIO (0, 3)
    return r

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

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
    
    c1bool <- newEmptyMVar
    c2bool <- newEmptyMVar
    c3bool <- newEmptyMVar
    c4bool <- newEmptyMVar

-- customers that get pay

    typebox_p1 <- newEmptyMVar
    typebox_p2 <- newEmptyMVar
    typebox_p3 <- newEmptyMVar
    typebox_p4 <- newEmptyMVar

-- customers that get rec
    typebox_r1 <- newEmptyMVar
    typebox_r2 <- newEmptyMVar
    typebox_r3 <- newEmptyMVar
    typebox_r4 <- newEmptyMVar


    value <- newEmptyMVar -- for value
    -- list <- newEmptyMVar -- attempting list of customers ** so i think customer list is created in main
    --typebox1 <- newEmptyMVar -- for random customer type
    --typebox2 <- newEmptyMVar -- for random customer type
    --typebox3 <- newEmptyMVar -- for random customer type
    --typebox4 <- newEmptyMVar -- for random customer type
    -- typebox <- newEmptyMVar -- for random customer type
    typebox_p <- newEmptyMVar 
    typebox_r <- newEmptyMVar 


    -- fork the customer processes
    putStrLn $ "4 customer threads being created."
    mapM_ forkIO [process c1 one value typebox_p1 typebox_r1 c1bool, process c2 two value typebox_p2 typebox_r2 c2bool, process c3 three value typebox_p3 typebox_r3 c3bool, process c4 four value typebox_p4 typebox_r4 c4bool] 
    -- create a box for each process
    
{-
    boolist <- newEmptyMVar 
    putMVar boolist [c1bool, c2bool, c3bool, c4bool]

    b <- takeMVar boolist
    --putStrLn ++ (show (b!!0)) 
    let headblist = (b!!0)
    let taillist = (b!!3)
    b <- takeMVar headblist 
    b2 <- takeMVar taillist
    putStrLn $ "this is the head of the list " ++ (show b) ++ " and the tail " ++ (show b2)
 -}
   -- now create two boxes for payees and recipients and select one at random, then transfer

 {-   
    payee_list <- newEmptyMVar
    putMVar payee_list [typebox_p1, typebox_p2, typebox_p3, typebox_p4 ]
 
    {-
    recipient_list <- newEmptyMVar
    putMVar recipient_list [typebox_r1, typebox_r2, typebox_r3, typebox_r4 ]
    -}
    p <- takeMVar payee_list
    let headplist = (p!!0)
    p <- takeMVar headplist
    putStrLn $ "RANDOM PAYEE : this is the head of the customer payee list " ++ (show p)
 -}  
   {-
    r <- takeMVar recipient_list
    let headrlist = (r!!0)
    r <- takeMVar headplist
    putStrLn $ "RANDOM RECIPIENT : this is the head of the customer payee list " ++ (show r)
    -}

   -- now just use the original customer list to select two customers in main
    customer_list <- newEmptyMVar 
    putMVar customer_list [one, two, three, four]
    c <- takeMVar customer_list
    
    r1 <- randomCustomers
    r2 <- randomCustomers
    

    let first_random = (c!!r1)
    let second_random = (c!!r2)
    fr <- takeMVar first_random
    sr <- takeMVar second_random
     
    putStrLn $ "RANDOM RECIPIENT IS : " ++ (show fr) ++ "RANDOM PAYEE IS : " ++ (show sr)
     
    a1 <- randomAmount 
     
    putStrLn $ "RANDOM AMOUNT IS: " ++ (show a1) 
    (sr, fr) <- transfer sr fr a1
    putStrLn $ "****UPDATED****  RANDOM RECIPIENT HAS : " ++ (show fr) ++ "RANDOM PAYEE HAS : " ++ (show sr)
    

    


    -- below creates two boxes for rec and pay and transfers money
{-
    payees <- takeMVar typebox_p 
    recipients <- takeMVar typebox_r
    putStrLn (show payees)
    putStrLn (show recipients)

    (payees, recipients) <- transfer payees recipients 10

    putStrLn (show payees)
    putStrLn (show recipients)
-}


    --let list_try = c:d:e:f:[]
    
    -- put the list of customers into an MVar of type MVar [Customer]
     
    
    
    random <- randomCustomer
    
    putStrLn $ "hi"

-- PROCEED WITH TRANSFER  
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  -- amount <= 0 = return (from, to)
  | balance from < amount = return (from, to)
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))        

-- SELECT RANDOM AMOUNT TO TRANSFER  
randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r

-- SELECT RANDOM CUSTOMER   
randomCustomers :: IO Int 
randomCustomers = do
    r <- randomRIO (0, 3)
    return r


data Dice = One | Two | Three | Four deriving (Show, Eq)


mapIntToDice :: Int -> Dice
mapIntToDice n = case r of
      0 -> One
      1 -> Two
      2 -> Three
      3 -> Four
    where r = mod n 4 

diceThrow :: IO Dice
diceThrow = do
    n <- randomIO :: IO Int
    let dice = mapIntToDice n
    return dice

    -- below is playing with a customer list
    --putStrLn $ "HIhere!!!!" ++ (show list_try)
    --putStrLn $ "here!!!!" ++ (show (list_try!!0)) 
    --let z = pick list_try -- work out how to return this (cant as uses IO)
    
    --random <- randomCustomer
    --random2 <- randomCustomer
    --if random /= random2 then 
       --putStrLn $ "random choice a: " ++ (show (list_try!!random)) ++ "random choice b: " ++ (show (list_try!!random2))
       --else 
           --putStrLn $ "woops"  
    --let pickRandomCustomer = list_try!!
    

    --putStrLn $ "hi " ++ (show c)
    --putStrLn $ "hi " ++ (show d)
    --putStrLn $ "hi " ++ (show e)
    --putStrLn $ "hi " ++ (show f)
    --test <- takeMVar typebox1 -- only runs if c1 gets Payee

    -- a- uncomment + use this is check the TYPE of the thread: test <- takeMVar typebox  
    --  b- uncomment + use this is check the TYPE of the thread: putStrLn $ "type test " ++ (show test)
    
    

    -- Can work with a pair of values in MVar box
    --list <- newEmptyMVar
    --test1 <- takeMVar typebox1
    --test2 <- takeMVar typebox2
    
     --test mvars in samebox
    --list2 <- newEmptyMVar
    --putMVar list2 c
   -- putMVar list2 d 
   -- putMVar list2 e
   -- putMVar list2 f
    --putStrLn $ "customers added to a list"
   -- h <- takeMVar list2
     

    --putMVar list (test1,test2)
    --pair <- takeMVar list 
    --let testa = fst pair
    --let testb = snd pair
    --if testa == testb then
        --putStrLn $ "shit - they're the same!"
     --else
        --putStrLn $ "woop ti doop - we can start the transfer!"   
    --list_b <- newEmptyMVar  
    --putMVar h addtypetolist

 -- print all threads (test)
    {-putStrLn $ "hi " ++ (show c) 
    putStrLn $ "hi " ++ (show d) 
    putStrLn $ "hi " ++ (show e) 
    putStrLn $ "hi " ++ (show f) -}










-------------------------------------------------------

-- customers :: [MVar Bool]   -- this will collect all the customer MVars
-- value :: MVar Int          -- this will hold the value -- shared by all threads


-- ACTIONS IN MAIN THREAD
-- 1. Create 10 customers
-- 2. Create 10 customer processes/threads
-- 3. Create an Mvar List customers (one for Payee, one for Recipient) and pick two randomly
-- So need to add the customer MVars into a list of MVars of type Bool, then use a random selector to pick them
--  

-- ACTIONS IN CUSTOMER THREAD
-- Flip a coin (all customers get a result - like in concurrency app)
-- If True then == recipient
-- If False == then payee
-- first one to both is the match
-- need to put their result in a box



















