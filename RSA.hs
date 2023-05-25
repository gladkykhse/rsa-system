module RSA where
import Utils
import Primes


create_keys :: IO()
create_keys = do
    p <- generate_prime (2^512) (2^513)
    q <- generate_prime (2^512) (2^513)
    let n = p * q
        phi = (p - 1) * (q - 1) 
    e <- choose_e phi
    let d = mod_inv e phi
    writeFile "public.key" $ (show e ++ " " ++ show n)
    writeFile "private.key" $ (show d ++ " " ++ show n)


encrypt_decrypt_file :: String -> String -> String -> IO()
encrypt_decrypt_file key_file file file_to_save =
    do
        key <- readFile key_file
        contents <- readFile file
        let [key_str, n_str] = words key
            key_int = read key_str :: Integer
            n = read n_str :: Integer
            int = read contents :: Integer
        writeFile file_to_save $ (show $ encrypt_decrypt_integer int key_int n)
        

encrypt_decrypt_integer :: Integer -> Integer -> Integer -> Integer
encrypt_decrypt_integer msg key n =
    pow_mod msg key n