import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as L

mhash secret n = show . md5 $ L.pack (secret ++ show n)

search' secret len n
    | allZero (take len h) = n
    | otherwise = search' secret len (n+1)
    where h = mhash secret n
          allZero s = and $ map (== '0') s

search secret len = search' secret len 0

main = do
    putStrLn $ "Part 1: " ++ show (search "ckczppom" 5)
    putStrLn $ "Part 2: " ++ show (search "ckczppom" 6)
