module RSA where
import Utils
import Primes
import Data.List
import System.IO


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


encrypt_file :: String -> String -> String -> IO()
encrypt_file key_file file file_to_save =
    do
        key <- readFile key_file
        contents <- readFile file
        let [key_str, n_str] = words key
            key_int = read key_str :: Integer
            n = read n_str :: Integer
            int_block_content = process_contents contents
            encrypted = map show (encrypt_decrypt_integers int_block_content key_int n)
        writeStringsToFile file_to_save encrypted
        
        
decrypt_file :: String -> String -> String -> IO()
decrypt_file key_file file file_to_save =
    do
        key <- readFile key_file
        contents <- readFile file
        let [key_str, n_str] = words key
            key_int = read key_str :: Integer
            n = read n_str :: Integer
            int_block_content = map read (lines contents)
            decrypted = encrypt_decrypt_integers int_block_content key_int n
            decrypted_str = map int_to_string decrypted
        writeFile file_to_save (concat decrypted_str)


write_strings_to_file :: String -> [String] -> IO ()
writeStringsToFile file strings = do
    handle <- openFile file WriteMode
    mapM_ (hPutStrLn handle) strings
    hClose handle
        

process_contents :: String -> [Integer]
process_contents contents = map string_to_int $ string_blocks contents


encrypt_decrypt_integers :: [Integer] -> Integer -> Integer -> [Integer]
encrypt_decrypt_integers msgs key n = [pow_mod msg key n | msg <- msgs]