import Data.Vect

removeElem : (value : a) ->
  (xs : Vect (S n) a) ->
  (prf : Elem value xs) ->
  Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) =
  y :: removeElem value ys later

removeElemAuto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElemAuto value xs {prf} = removeElem value xs prf

maryInVect : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVect = There (There Here)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElemM : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElemM value [] = No notInNil
isElemM value (x :: xs) = case decEq value x of
                              (Yes Refl) => Yes Here
                              (No notHere) => (case isElemM value xs of
                                                    (Yes prf) => Yes (There prf)
                                                    (No notThere) => No (notInTail notHere notThere))

-- Exercise

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

listIsEmpty : Last [] value -> Void
listIsEmpty LastOne impossible
listIsEmpty (LastCons _) impossible

notLast : (contra : (x = value) -> Void) -> Last [x] value -> Void
notLast contra LastOne = contra Refl
notLast _ (LastCons LastOne) impossible
notLast _ (LastCons (LastCons _)) impossible

noLastCons : (contra : Last xs value -> Void) -> Last (x :: xs) value -> Void
noLastCons contra (LastCons prf) = contra prf

isLast : DecEq a => (list : List a) -> (value : a) -> Dec (Last list value)
isLast [] value = No listIsEmpty
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (notLast contra)
isLast (x :: xs) value = case isLast xs value of
                              (Yes prf) => Yes (LastCons prf)
                              (No contra) => No (noLastCons contra)
