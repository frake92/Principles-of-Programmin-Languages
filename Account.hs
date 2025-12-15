{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Account where

import Currency (Money(..), USD, addMoney, subtractMoney)
import GHC.TypeLits (Symbol)

-- Type class mint "interface"
class Account a where
    type Currency a :: Symbol
    getBalance :: a -> Money (Currency a)
    setBalance :: Money (Currency a) -> a -> a
    getAccountNumber :: a -> String
    getOwner :: a -> String
    
    -- Default implementáció a depositra
    deposit :: Money (Currency a) -> a -> Either String a
    deposit amount acc 
        | amount <= Money 0 = Left "Deposit amount must be positive"
        | otherwise = Right $ setBalance (addMoney (getBalance acc) amount) acc

    -- Absztrakt metódus a withdraw-ra
    withdraw :: Money (Currency a) -> a -> Either String a

-- Konkrét account típusok
data CheckingAccount (c :: Symbol) = CheckingAccount
    { checkingNumber :: String
    , checkingOwner :: String
    , checkingBalance :: Money c
    , overdraftLimit :: Money c
    } deriving (Show, Eq)

data SavingsAccount (c :: Symbol) = SavingsAccount
    { savingsNumber :: String
    , savingsOwner :: String
    , savingsBalance :: Money c
    , interestRate :: Double
    } deriving (Show, Eq)

data InvestmentAccount (c :: Symbol) = InvestmentAccount
    { investmentNumber :: String
    , investmentOwner :: String
    , investmentBalance :: Money c
    , portfolio :: [String]  -- egyszerűsített
    } deriving (Show, Eq)

-- Smart constructors
createChecking :: String -> String -> Money c -> Money c -> Either String (CheckingAccount c)
createChecking num owner bal limit
    | null num = Left "Account number cannot be empty"
    | null owner = Left "Owner name cannot be empty"
    | otherwise = Right $ CheckingAccount num owner bal limit

createSavings :: String -> String -> Money c -> Double -> Either String (SavingsAccount c)
createSavings num owner bal rate
    | null num = Left "Account number cannot be empty"
    | null owner = Left "Owner name cannot be empty"
    | rate < 0 = Left "Interest rate cannot be negative"
    | otherwise = Right $ SavingsAccount num owner bal rate

createInvestment :: String -> String -> Money c -> [String] -> Either String (InvestmentAccount c)
createInvestment num owner bal portfolio
    | null num = Left "Account number cannot be empty"
    | null owner = Left "Owner name cannot be empty"
    | otherwise = Right $ InvestmentAccount num owner bal portfolio

-- Account instance-ok (inheritance-szerű)
instance Account (CheckingAccount c) where
    type Currency (CheckingAccount c) = c
    getBalance = checkingBalance
    setBalance newBal acc = acc { checkingBalance = newBal }
    getAccountNumber = checkingNumber
    getOwner = checkingOwner
    

    -- Checking account: Overdraft limit figyelembe vétele!
    withdraw amount acc@(CheckingAccount { checkingBalance = bal, overdraftLimit = limit })
        | amount <= Money 0 = Left "Withdrawal amount must be positive"
        | (bal `addMoney` limit) < amount = Left "Exceeds overdraft limit"
        | otherwise = Right $ acc { checkingBalance = bal `subtractMoney` amount }

instance Account (SavingsAccount c) where
    type Currency (SavingsAccount c) = c
    getBalance = savingsBalance
    setBalance newBal acc = acc { savingsBalance = newBal }
    getAccountNumber = savingsNumber
    getOwner = savingsOwner

    -- Savings account: Nincs overdraft, csak a pozitív egyenlegig mehet
    withdraw amount acc
        | amount <= Money 0 = Left "Withdrawal amount must be positive"
        | getBalance acc < amount = Left "Insufficient funds"
        | otherwise = Right $ setBalance (getBalance acc `subtractMoney` amount) acc

instance Account (InvestmentAccount c) where
    type Currency (InvestmentAccount c) = c
    getBalance = investmentBalance
    setBalance newBal acc = acc { investmentBalance = newBal }
    getAccountNumber = investmentNumber
    getOwner = investmentOwner

    -- Investment account: Szigorúbb szabályok (pl. nem mehet negatívba)
    withdraw amount acc
        | amount <= Money 0 = Left "Withdrawal amount must be positive"
        | getBalance acc < amount = Left "Insufficient funds"
        | otherwise = Right $ setBalance (getBalance acc `subtractMoney` amount) acc