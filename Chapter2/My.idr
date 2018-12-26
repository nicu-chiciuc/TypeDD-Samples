module Main



average : String -> Double
average str =
  let numWords = wordCount str
      totalLength = sum (allLengths (words str)) in

  cast totalLength / cast numWords

  where
    wordCount : String -> Nat
    wordCount str = length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map length strs


palindrome : Nat -> String -> Bool
palindrome minLen str = let low = toLower str in
  length str >= minLen &&
  low == reverse low

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten list = let sorted = reverse (sort list) in
  take 10 sorted

over_length : Nat -> List String -> Nat
over_length len list =
  let
    lengths = map length list
    bigLengths = filter (> len) lengths
  in

  length bigLengths

showPalindrome : String -> String
showPalindrome str = show (palindrome 2 str)

main : IO ()
main = repl "give string> " showPalindrome