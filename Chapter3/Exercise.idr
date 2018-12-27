module Main

import Data.Vect

myLen : List a -> Nat
myLen [] = 0
myLen (x :: xs) = S (myLen xs)

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

mapVect : (a -> b) -> Vect n a -> Vect n b
mapVect f [] = []
mapVect f (x :: xs) = f x :: mapVect f xs

-- Matrices

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeHelper2 : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper2 = zipWith (::)


transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let
    xsTrans = transposeMat xs
    in (transposeHelper2 x xsTrans)

addVect : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
addVect = zipWith (+)


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addVect x y :: addMatrix xs ys

-- matrix multiplication

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect l a) -> Vect n (Vect l a)
multMatrix [] [] = []
multMatrix [] (x :: xs) = []
multMatrix (x :: xs) [] = ?what
multMatrix (x :: xs) (y :: ys) = ?multMatrix_rhs_5
