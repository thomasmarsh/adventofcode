import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as L

mhash :: String -> Int -> String
mhash secret n = show . md5 $ L.pack (secret ++ show n)

search' :: String -> Int -> Int -> Int
search' secret len n
    | all (=='0') (take len h) = n
    | otherwise = search' secret len (n+1)
    where h = mhash secret n

search :: String -> Int -> Int
search secret len = search' secret len 0

main :: IO ()
main = do
    putStrLn $ "Part 1: " ++ show (search "ckczppom" 5)
    putStrLn $ "Part 2: " ++ show (search "ckczppom" 6)
