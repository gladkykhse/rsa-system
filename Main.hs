import System.IO
import System.Environment
import Primes
import Utils
import RSA


run_with_args :: [String] -> IO()
run_with_args l
    | head l == "-gen-keys" =
        create_keys
    | head l == "-encrypt" ||  head l == "-decrypt" =
        let [_, pk_file, file_to_encrypt, file_to_save] = l
        in
            encrypt_decrypt_file pk_file file_to_encrypt file_to_save
    | otherwise =
        error "Invalid argument. Possible options: -gen-keys, -encrypt, -decrypt"


main :: IO()
main =
    do 
        args <- getArgs
        run_with_args args