module Primes where
import System.Random

pow_mod :: Integer -> Integer -> Integer -> Integer
pow_mod x y m = f (x `rem` m) y 1 `mod` m
    where
        f _ 0 acc = acc
        f b e acc =
            if odd e
                then f (b * b `rem` m) (e `div` 2) (b * acc `rem` m)
            else f (b * b `rem` m) (e `div` 2) acc


get_odd_exp :: Integer -> Integer
get_odd_exp exp
    | odd exp = exp
    | otherwise = get_odd_exp (exp `div` 2)


check_rest_exps :: Integer -> Integer -> Integer -> Bool
check_rest_exps n a exp
    | exp >= (n - 1) = False
    | otherwise = 
        if (pow_mod a exp n) == (n - 1)
            then True
        else check_rest_exps n a (exp * 2)
    
        
single_test :: Integer -> Integer -> Bool
single_test n a =
    if (pow_mod a exp n) == 1
        then True
    else check_rest_exps n a exp
    where exp = get_odd_exp (n - 1)


multi_test :: RandomGen g => g -> Integer -> Integer -> Bool
multi_test gen n 1 =
    single_test n a
    where (a, _) = randomR (2, n - 1) gen
multi_test gen n trials =
    if single_test n a == False
        then False
    else multi_test gen' n (trials - 1)
    where (a, gen') = randomR (2, n) gen


generate_prime :: Integer -> Integer -> IO Integer
generate_prime lo hi = do
    gen <- newStdGen
    let (n, gen') = randomR (lo, hi) gen
    if multi_test gen' n 50
        then return n
        else generate_prime lo hi

    
choose_e :: Integer -> IO Integer
choose_e phi = do
    gen <- newStdGen
    let (e, _) = randomR (2, phi) gen
    if gcd e phi == 1
        then return e
    else choose_e phi
