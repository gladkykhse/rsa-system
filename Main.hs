import System.IO
import System.Random
import Primes

-- choose_e e phi
--     | e < phi = 
--         if (gcd e phi) == 1
--             then e
--         else choose_e (e + 1) phi
--     | otherwise = error "Unable to find a suitable value for e"

extended_gcd :: Integer -> Integer -> (Integer, Integer, Integer)
extended_gcd a b
    | a == 0 = (b, 0, 1)
    | otherwise = (gcd, y - (b `div` a) * x, x)
        where (gcd, x, y) = extended_gcd (b `mod` a) a

mod_inv :: Integer -> Integer -> Integer
mod_inv e phi =
    if gcd /= 1
        then error "Modular inverse does not exist"
    else x `mod` phi
    where
        (gcd, x, y) = extended_gcd e phi

create_keys :: IO()
create_keys = do
    p <- generate_prime (2^512) (2^513)
    q <- generate_prime (2^512) (2^513)
    let n = p * q
        phi = (p - 1) * (q - 1) 
    e <- choose_e phi
    let d = mod_inv e phi
    writeFile "public.key" $ show e
    writeFile "private.key" $ show d

main = do
    g <- getStdGen
    print $ show (multi_test g 117 10)
