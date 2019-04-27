import Data.Vect

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

data ThreeEq : (a : Nat) -> (b : Nat) -> (c : Nat) -> Type where
  Same3 : (num : Nat) -> ThreeEq num num num


sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS n n n (Same3 n) = Same3 (S n)


checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
  Nothing => Nothing
  (Just prf) => Just (cong prf)


exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just Refl => Just input

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

-- 8.2.1 Reversing a vector

revProof : Vect (len + 1) elem -> Vect (S len) elem
revProof {len} result = rewrite plusCommutative 1 len in result

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = revProof (myReverse xs ++ [x])

-- 8.2.5 Appending vectors, revisited

appendm : Vect n elem -> Vect m elem -> Vect (m + n) elem
appendm [] ys = ?appendm_rhs_1
appendm (x :: xs) ys = ?appendm_rhs_2 (x :: appendm xs ys)

-- voids

twoPlusFive : 2 + 2 = 5 -> Void
twoPlusFive Refl impossible

valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible

--

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNoZero : (S k = 0) -> Void
sucNoZero Refl impossible



noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkNat Z Z = Yes Refl
checkNat Z (S k) = No zeroNotSuc
checkNat (S k) Z = No sucNoZero
checkNat (S k) (S j) = case checkNat k j of
                            (Yes prf) => Yes (cong prf)
                            (No contra) => No (noRec contra)
