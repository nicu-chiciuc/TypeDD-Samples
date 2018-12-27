import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

tenInts: Vect 10 Int
tenInts = fourInts ++ sixInts

total allVectLengths : Vect len String -> Vect len Nat
allVectLengths [] = []
allVectLengths (word :: words) = length word :: allVectLengths words


insert : Ord elem => (x : elem)  -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs



insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) =
  let
    xsSorted = insSort xs
    in (insert x xsSorted)

-- implicit arguments
myLen : Vect n elem -> Nat
myLen {n} xs = n

append : {elem : Type} -> {n : Nat} -> {m : Nat} ->
         Vect n elem -> Vect m elem -> Vect (n + m) elem
