import Data.Primitives.Views

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

Functor InfList where
  map func (value :: xs) = func value :: map func xs


countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x+1)

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

every_other : Stream Nat -> Stream Nat
every_other (value :: val2 :: xs) = val2 :: every_other xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
  (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1
