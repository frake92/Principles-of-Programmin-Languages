{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Currency where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

-- Phantom type a pénznemhez
data Money (currency :: Symbol) = Money Double
    deriving (Eq, Ord)

instance (KnownSymbol c) => Show (Money c) where
    show (Money amount) = symbolVal (Proxy :: Proxy c) ++ " " ++ show amount

-- Currency típusok
type USD = "USD"
type EUR = "EUR"
type GBP = "GBP"

-- Konverziós type class
class Convertible (from :: Symbol) (to :: Symbol) where
    conversionRate :: Double
    convert :: Money from -> Money to
    convert (Money amount) = Money (amount * conversionRate @from @to)

-- Konkrét konverziók
instance Convertible USD EUR where
    conversionRate = 0.92

instance Convertible EUR USD where
    conversionRate = 1.09

instance Convertible USD GBP where
    conversionRate = 0.79

instance Convertible GBP USD where
    conversionRate = 1.27

-- Helper függvények
addMoney :: Money c -> Money c -> Money c
addMoney (Money a) (Money b) = Money (a + b)

subtractMoney :: Money c -> Money c -> Money c
subtractMoney (Money a) (Money b) = Money (a - b)

multiplyMoney :: Money c -> Double -> Money c
multiplyMoney (Money a) factor = Money (a * factor)