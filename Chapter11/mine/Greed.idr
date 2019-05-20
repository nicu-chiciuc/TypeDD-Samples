%default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

data Fuel = Dry | More (Lazy Fuel)


(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

partial
forever : Fuel
forever = More forever


greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
             then do putStrLn "bye"
                     Quit ()
             else do putStrLn ("hello " ++ name)
                     greet

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure Nothing

partial
main : IO ()
main = do run forever greet
          pure ()
