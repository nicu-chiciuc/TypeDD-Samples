module Main

import System
import Prelude.Nat

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readPair : IO (String, String)
readPair = do
  str1 <- getLine
  str2 <- getLine
  pure (str1, str2)

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1_ok <- readNumber | Nothing => pure Nothing
  Just num2_ok <- readNumber | Nothing => pure Nothing
  pure (Just (num1_ok, num2_ok))

countdown : (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do
  printLn (S secs)
  usleep 1000000
  countdown secs

guess: (target: Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn ("try number: " ++ (show guesses))
  num <- readNumber
  case num of
    Nothing => do
      putStrLn "wrong input"
      guess target (guesses + 1)
    Just n => if n == target
      then putStrLn "success"
      else if n < target
        then do
          putStrLn "guess is smaller"
          guess target (guesses + 1)
        else do
          putStrLn "guess is bigger"
          guess target (guesses + 1)

main : IO ()
main = do
  target <- time
  let ntarget = fromIntegerNat target
  -- printLn ntarget
  let between = modNat ntarget 100
  -- printLn between
  guess between 0
