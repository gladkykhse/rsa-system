import System.IO
import System.Environment
import Primes
import Utils
import RSA


run_with_args :: [String] -> IO()
run_with_args ["-gen-keys"] = create_keys
run_with_args [l, pk_file, file_to_encrypt, file_to_save]
    | l == "-encrypt" = encrypt_file pk_file file_to_encrypt file_to_save
    | l == "-decrypt" = decrypt_file pk_file file_to_encrypt file_to_save
run_with_args _ =
    error "Invalid argument. Possible options: -gen-keys, -encrypt, -decrypt"


main :: IO()
main =
    do 
        args <- getArgs
        run_with_args args