-- cli interface, összekötő, exceptikn hsndling. ez a demo app
module Main where

import Account
import Transaction
import Currency
import Interest
import Report
import Data.Time (getCurrentTime)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "=== Bank Account Management System ==="
    putStrLn ""
    
    -- Demo accounts
    let checking = case createChecking "CHK001" "Alice" (Money 1000 :: Money USD) (Money 500) of
            Right acc -> acc
            Left err -> error err

    let savings = case createSavings "SAV002" "Alice" (Money 5000 :: Money USD) 0.05 of
            Right acc -> acc
            Left err -> error err
    
    let investment = case createInvestment "INV003" "Alice" (Money 2300 :: Money USD) ["AMD", "NVIDIA"] of
            Right acc -> acc
            Left err -> error err

    let investment1 = case createInvestment "INV001" "Charlie" (Money 10000 :: Money GBP) ["AAPL", "GOOGL"] of
            Right acc -> acc
            Left err -> error err
    
    -- Account info
    putStrLn "1. Account Information:"
    putStrLn $ "  Checking: " ++ getOwner checking ++ " - " ++ show (getBalance checking)
    putStrLn $ "  Savings: " ++ getOwner savings ++ " - " ++ show (getBalance savings)
    putStrLn $ "  Investment: " ++ getOwner investment ++ " - " ++ show (getBalance investment)
    putStrLn ""
    
    -- Currency conversion
    putStrLn "2. Currency Conversion:"
    let usdAmount = Money 100 :: Money USD
    let eurAmount = convert usdAmount :: Money EUR
    putStrLn $ "  $100 USD = " ++ show eurAmount ++ " EUR"
    putStrLn ""
    
    -- Transactions
    putStrLn "3. Transaction Processing:"
    now <- getCurrentTime
    let trans = [ Transaction Deposit (Money 200 :: Money USD) now "Salary" "CHK001"
                , Transaction Withdrawal (Money 50) now "ATM" "CHK001"
                , Transaction Deposit (Money 100) now "Refund" "CHK001"
                ]
    
    case processTransactions trans checking of
        Left err -> putStrLn $ "  Error: " ++ show err
        Right newAcc -> putStrLn $ "  New balance on Alice's account after transactions: " ++ show (getBalance newAcc)
    
    putStrLn $ "  " ++ transactionReport trans
    
    putStrLn "4. Interest Calculation:"
    putStrLn $ getOwner savings ++ "'s savings account"
    putStrLn $ ""
    putStrLn $ getOwner savings ++ "'s savings: " ++ show (getBalance savings)
    putStrLn $ "Interest rate: " ++ show (interestRate savings)
    
    let simple = calculateProjectedBalance savings simpleInterest 5
    let compound = calculateProjectedBalance savings compoundInterest 5
    
    putStrLn $ "  Projected balance after 5 years (Simple): " ++ show simple
    putStrLn $ "  Projected balance after 5 years (Compound): " ++ show compound
    putStrLn ""
    
    -- Reports
    putStrLn "5. Account Reports:"
    let accounts = [checking]
    putStrLn $ "  Total balance: " ++ show (totalBalance accounts)
    putStrLn $ "  High value accounts (>$500): " ++ show (length $ highValueAccounts 500 accounts)
    
    -- Contravariance Demo
    putStrLn "\n  [Contravariance Demo] Filtering High Value Accounts:"
   
    let checkHighValue name acc = 
            if getPredicate isHighValueAccount acc 
            then name ++ " is a High Value customer."
            else name ++ " is a Standard customer."

    putStrLn $ "  " ++ checkHighValue "Alice (Checking)" checking
    putStrLn $ "  " ++ checkHighValue "Alice (Savings)" savings
    putStrLn $ "  " ++ checkHighValue "Charlie (Investment)" investment

    putStrLn "\n  [Functor & Recursion Demo] Client Portfolio Analysis:"
    let alicePortfolio = Composite "Alice's wealth"
            [ Composite "Private" 
                [ 
                        Asset (WrappedChecking checking)    -- Alice magánszámlái 
                ]
                
            , Composite "Savings"
                [ 
                        Asset (WrappedSavings savings)    -- Alice befektetései és megtakarítása
                ]
                ,
                Composite "Investments"
                [
                        Asset (WrappedInvestment investment)
                ]
            ]
    
    putStrLn "  Portfolio Structure:"
    putStrLn $ showPortfolioStructure alicePortfolio
    
    putStrLn $ "  Total Portfolio Value: " ++ show (portfolioValue alicePortfolio)
    
    putStrLn ""
    
    putStrLn "All demos completed!"

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
