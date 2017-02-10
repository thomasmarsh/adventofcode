import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8

mhash secret n = show . md5 $ pack (secret ++ show n)

search' secret len n
    | allZero (Prelude.take len h) = n
    | otherwise = search' secret len (n+1)
    where h = mhash secret n
          allZero s = and $ Prelude.map (== '0') s

search secret len = search' secret len 0

main = do
    Prelude.putStrLn $ "Part 1: " ++ show (search "ckczppom" 5)
    Prelude.putStrLn $ "Part 2: " ++ show (search "ckczppom" 6)
