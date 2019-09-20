module Test where
import ODE

square' = dsolve' (\t y -> 2*t) 0 0 :: DD
exp' = dsolve' (\t y -> y) 0 1 :: DD
log' = dsolve' (\t y -> 1/(1+t)) 0 0 `scomp` spure (subtract 1) :: DD
exp'' = dsolve (\t x y -> x) 0 exp' 1 :: DD

trig = dsolve' (\t (y,z) -> (z,-y)) 0 (0,1)
sin' = spure fst `scomp` trig :: DD
cos' = spure snd `scomp` trig :: DD
atan' = dsolve' (\t y -> 1/(1+t^2)) 0 0 :: DD
pi' = 4 * sget atan' 1

test :: IO ()
test =
  do putStrLn "Exponential"
     mp "exp" exp $ stake' exp'' ([0..9] ++ [10,20..100])
     putStrLn "Logarithm"
     mp "ln" log $ stake' log' ([0.01,0.1,0.5,0.8] ++ [1..10] ++ [1e90,1e92,1e94,1e96,1e98,1e100,1e200,1e300])
     putStrLn "Square"
     mp "^2" (^2) $ stake' square' [0..10]
     putStrLn "Sin"
     mp "sin" sin $ stake' sin' ([0,0.1..1] ++ [pi/4,pi/2])
     putStrLn "ArcTan"
     mp "atan" atan $ stake' atan' ([0,0.1..1])
     putStrLn "Pi"
     mp "pi" (const pi) [(0,pi')]
  where mp f g = mapM_ (putStrLn . fn f g)
        fn f g (t,y) = " > " ++ f ++ "(" ++ show t ++ ") = " ++ show y ++ " [" ++ show (g t) ++ ", " ++ show (err (g t) y) ++ "]"
        err y z = abs $ 2 * (y - z) / (y + z)
