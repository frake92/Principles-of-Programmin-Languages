{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Report where

import Account (Account(..), CheckingAccount, SavingsAccount, InvestmentAccount)
import Transaction (Transaction(..), TransactionType(..))
import Currency (Money(..), USD)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.TypeLits (KnownSymbol)
import GHC.TypeLits (KnownSymbol, Symbol)

data AnyAccount (c :: Symbol)
    = WrappedChecking (CheckingAccount c)
    | WrappedSavings (SavingsAccount c)
    | WrappedInvestment (InvestmentAccount c)
    deriving Show


instance Account (AnyAccount c) where
    type Currency (AnyAccount c) = c
    
    getBalance (WrappedChecking acc) = getBalance acc
    getBalance (WrappedSavings acc) = getBalance acc
    getBalance (WrappedInvestment acc) = getBalance acc
    
    setBalance m (WrappedChecking acc) = WrappedChecking (setBalance m acc)
    setBalance m (WrappedSavings acc) = WrappedSavings (setBalance m acc)
    setBalance m (WrappedInvestment acc) = WrappedInvestment (setBalance m acc)
    
    getAccountNumber (WrappedChecking acc) = getAccountNumber acc
    getAccountNumber (WrappedSavings acc) = getAccountNumber acc
    getAccountNumber (WrappedInvestment acc) = getAccountNumber acc
    
    getOwner (WrappedChecking acc) = getOwner acc
    getOwner (WrappedSavings acc) = getOwner acc
    getOwner (WrappedInvestment acc) = getOwner acc
    
    withdraw m (WrappedChecking acc) = fmap WrappedChecking (withdraw m acc)
    withdraw m (WrappedSavings acc) = fmap WrappedSavings (withdraw m acc)
    withdraw m (WrappedInvestment acc) = fmap WrappedInvestment (withdraw m acc)


-- List comprehension: statisztikák
accountSummaries :: KnownSymbol c => [CheckingAccount c] -> [String]
accountSummaries accounts = 
    [ getOwner acc ++ ": " ++ show (getBalance acc)
    | acc <- accounts
    ]

-- Map és filter kombinálva
highValueAccounts :: Double -> [CheckingAccount c] -> [CheckingAccount c]
highValueAccounts threshold = 
    filter (\acc -> let Money bal = getBalance acc in bal > threshold)

-- Fold: aggregálás
totalBalance :: Account a => [a] -> Money (Currency a)
totalBalance = foldr (\acc (Money total) -> 
    let Money bal = getBalance acc 
    in Money (total + bal)) (Money 0)

-- Rekurzív adatszerkezet: Pénzügyi Portfólió
-- Ez demonstrálja a Generics-et és a Rekurziót egy valós banki koncepcióval.
-- Egy portfólió állhat egyetlen eszközből (Asset), vagy al-portfóliók csoportjából (Composite).
data Portfolio a = Asset a | Composite String [Portfolio a]
    deriving Show

-- Covariance: Functor instance
-- Lehetővé teszi a portfólió tartalmának transzformálását (pl. Számla -> Egyenleg)
-- a struktúra (csoportosítások) megőrzése mellett.
instance Functor Portfolio where
    fmap f (Asset x) = Asset (f x)
    fmap f (Composite name items) = Composite name (map (fmap f) items)

-- Contravariance: Predicate example
-- Valós haszon: Logika újrahasznosítása kisebb típusról (Money) nagyobbra (Account)
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

-- Contravariant map
contramap :: (b -> a) -> Predicate a -> Predicate b
contramap f (Predicate p) = Predicate (p . f)

-- Üzleti logika: "Nagy értékű" tranzakció vagy egyenleg definíciója (Money-ra)
isHighValue :: Predicate (Money c)
isHighValue = Predicate (\(Money amount) -> amount > 1000)

-- Ezt a logikát kiterjesztjük Számlákra a contramap segítségével
-- Nem kell újraírni a > 1000 ellenőrzést, csak megmondjuk, hol van a pénz a számlában
isHighValueAccount :: Account a => Predicate a
isHighValueAccount = contramap getBalance isHighValue

-- Segédfüggvény a szűréshez
filterBy :: Predicate a -> [a] -> [a]
filterBy (Predicate p) = filter p

-- Rekurzió: A portfólió teljes értékének kiszámítása
-- Bejárja a fát és összegzi az egyenlegeket
portfolioValue :: Account a => Portfolio a -> Money (Currency a)
portfolioValue (Asset acc) = getBalance acc
portfolioValue (Composite _ items) = 
    foldr (\p (Money total) -> 
        let Money val = portfolioValue p 
        in Money (total + val)) (Money 0) items

-- Segédfüggvény: Portfólió struktúra megjelenítése (nevekkel)
showPortfolioStructure :: (Account a, KnownSymbol (Currency a)) => Portfolio a -> String
showPortfolioStructure p = go 0 p
  where
    indent n = replicate (n * 2) ' '
    go n (Asset acc) = indent n ++ "- " ++ getOwner acc ++ ": " ++ show (getBalance acc) ++ "\n"
    go n (Composite name items) = indent n ++ "+ " ++ name ++ "\n" ++ concatMap (go (n + 1)) items

-- Lambda: rendezés és formázás
formatTopAccounts :: (Account a, KnownSymbol (Currency a)) => Int -> [a] -> [String]
formatTopAccounts n accounts =
    let sorted = sortBy (comparing ((\(Money x) -> x) . getBalance)) accounts
        top = take n $ reverse sorted
    in map (\acc -> getOwner acc ++ ": $" ++ 
            show ((\(Money x) -> x) $ getBalance acc)) top

-- Transaction report
transactionReport :: KnownSymbol c => [Transaction c] -> String
transactionReport transactions =
    let deposits = filter (\t -> transactionType t == Deposit) transactions
        withdrawals = filter (\t -> transactionType t == Withdrawal) transactions
        totalDep = sum $ map ((\(Money x) -> x) . amount) deposits
        totalWith = sum $ map ((\(Money x) -> x) . amount) withdrawals
    in unlines
        [ "Transaction Report"
        , "=================="
        , "Total Deposits: $" ++ show totalDep
        , "Total Withdrawals: $" ++ show totalWith
        , "Net Change: $" ++ show (totalDep - totalWith)
        ]