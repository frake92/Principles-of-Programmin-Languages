{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Interest where

import Currency (Money(..), USD, multiplyMoney)
import Account (SavingsAccount(..), Account(..))
import GHC.TypeLits (Symbol)

-- Higher-order function: kamatszámító stratégia
type InterestStrategy = Double -> Double -> Int -> Double

-- Egyszerű kamat (lambda)
-- Visszatérési érték: Tőke + Kamat
simpleInterest :: InterestStrategy
simpleInterest principal rate years = 
    principal * (1 + rate * fromIntegral years)

-- Kamatos kamat (rekurzió)
-- Visszatérési érték: Tőke + Kamat
compoundInterest :: InterestStrategy
compoundInterest principal rate years = compoundInterest' principal years
  where
    compoundInterest' p 0 = p
    compoundInterest' p n = compoundInterest' (p * (1 + rate)) (n - 1)

-- Method pointer: kamatszámítás stratégiával
calculateInterest :: InterestStrategy -> Money c -> Double -> Int -> Money c
calculateInterest strategy (Money principal) rate years =
    Money $ strategy principal rate years

-- Higher-order: custom stratégia készítés
makeCompoundStrategy :: Int -> InterestStrategy
makeCompoundStrategy periods principal rate years =
    principal * (1 + rate / fromIntegral periods) ** (fromIntegral periods * fromIntegral years)

-- Account-aware interest calculation
-- Ez a függvény kifejezetten SavingsAccount-ot vár, és annak a kamatlábát használja
calculateProjectedBalance :: SavingsAccount c -> InterestStrategy -> Int -> Money c
calculateProjectedBalance acc strategy years =
    let (Money principal) = getBalance acc
        rate = interestRate acc
        newAmount = strategy principal rate years
    in Money newAmount