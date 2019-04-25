import Data.Vect


data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a


readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)

readVect : IO (len ** Vect len String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do
      (_ ** xs) <- readVect
      pure (_ ** (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) =
  putStrLn (show xs ++ " (length " ++ show len ++ ")")

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["a", "b", "c"])

-----

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "fuck"
getStringOrInt True = 34

valToString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString False x = trim x
valToString True x = cast x

--- Functions with variable arguments

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType =>
        (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)


--- strings

data Format
  = Number Format
  | Str Format
  | Cha Format
  | Lit String Format
  | End

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Cha (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             (Lit lit chars2) => Lit (strCons c lit) chars2
                             fmt => Lit (strCons c "") fmt


PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Cha fmt) = (c : Char) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

total printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \num => printfFmt fmt (acc ++ cast num)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Cha fmt) acc = \char => printfFmt fmt (acc ++ (strCons char ""))
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

--- Exercises 01

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

--- Exercise 03

TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 3 Nat
test = (0, 0, 0, ())
