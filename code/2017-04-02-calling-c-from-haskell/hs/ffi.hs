import GCD

main :: IO ()
main = mapM_ (print . uncurry f_gcd) [(8, 12), (30, 105), (24, 108)]
