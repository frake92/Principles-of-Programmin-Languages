{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Transaction where

import Currency (Money(..), USD, addMoney, subtractMoney)
import Data.Time (UTCTime, getCurrentTime)
import GHC.TypeLits (Symbol)
import Account (Account(..))

-- Transaction típusok
data TransactionType = Deposit | Withdrawal | Transfer
    deriving (Show, Eq)

-- Generikus Transaction típus (c :: Symbol)
data Transaction (c :: Symbol) = Transaction
    { transactionType :: TransactionType
    , amount :: Money c
    , timestamp :: UTCTime
    , description :: String
    , accountId :: String
    } deriving (Show, Eq)

-- Generikus Error típus
data TransactionError (c :: Symbol)
    = InsufficientFunds (Money c)
    | InvalidAmount (Money c)
    | AccountNotFound String
    | TransactionFailed String
    deriving (Show, Eq)

-- Rekurzív tranzakció feldolgozás (Account alapú)
processTransactions :: (Account a) => [Transaction (Currency a)] -> a -> Either (TransactionError (Currency a)) a
processTransactions [] acc = Right acc
processTransactions (t:ts) acc = 
    if accountId t /= getAccountNumber acc
    then Left $ AccountNotFound ("Transaction ID mismatch: " ++ accountId t ++ " vs " ++ getAccountNumber acc)
    else 
        let continueWith newAcc = processTransactions ts newAcc
            mapError str = TransactionFailed str
        in case transactionType t of
            Deposit -> case Account.deposit (amount t) acc of
                Left err -> Left (mapError err)
                Right newAcc -> continueWith newAcc
            Withdrawal -> case Account.withdraw (amount t) acc of
                Left err -> Left (mapError err)
                Right newAcc -> continueWith newAcc
            Transfer -> processTransactions ts acc

-- List processing: szűrés típus szerint
filterByType :: TransactionType -> [Transaction c] -> [Transaction c]
filterByType tType = filter (\t -> transactionType t == tType)

-- Map: összegek kinyerése
getAmounts :: [Transaction c] -> [Money c]
getAmounts = map amount

-- Fold: összes összeg
totalAmount :: [Transaction c] -> Money c
totalAmount = foldr addMoney (Money 0) . getAmounts