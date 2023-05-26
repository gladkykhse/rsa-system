module Utils where
import Data.Char

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


pow_mod :: Integer -> Integer -> Integer -> Integer
pow_mod x y m = f (x `rem` m) y `mod` m
    where
        f :: Integer -> Integer -> Integer
        f _ 0 = 1
        f b e
            | odd e     = (b * f (b * b `rem` m) (e `div` 2)) `rem` m
            | otherwise = f (b * b `rem` m) (e `div` 2)


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


string_to_int :: String -> Integer
string_to_int s = foldr (\c a -> 256 * a + (fromIntegral (ord c))) 0 s


int_to_string :: Integer -> String
int_to_string 0 = ""
int_to_string n = chr (fromIntegral (n `mod` 256)) : int_to_string (n `div` 256)


string_blocks :: String -> [String]
string_blocks [] = []
string_blocks str = chunk : string_blocks rest
    where (chunk, rest) = splitAt 128 str
