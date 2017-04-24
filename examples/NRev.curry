-- A naive reverse benchmark.
-- The main function reads an integer n and executes
-- nrev on a list of length n.

import Read
import System

main :: IO ()
main = do
  args <- getArgs
  if null args then error "Integer argument missing!"
               else seq (id $## nrev [1.. (readNat (head args))]) done

-- Naive reverse implementation:
nrev :: [a] -> [a]
nrev []     = []
nrev (x:xs) = app (nrev xs) [x]

app :: [a] -> [a] -> [a]
app [] ys     = ys
app (x:xs) ys = x : app xs ys
