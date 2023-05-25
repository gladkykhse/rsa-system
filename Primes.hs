module Primes where
import System.Random
import Utils
    
        
single_test :: Integer -> Integer -> Bool
single_test n a =
    pow_mod a exp n == 1 || check_rest_exps n a exp
    where exp = get_odd_exp (n - 1)


multi_test :: RandomGen g => g -> Integer -> Int -> Bool
multi_test gen n trials =
    all (single_test n) (take trials (randomRs (2, n - 2) gen))


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
