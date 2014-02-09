import qualified Data.Map
import Data.Char
import System.IO
import Control.Monad
import Data.List


count::  Data.Map.Map String Int ->String-> Data.Map.Map String Int
count m e = case (Data.Map.lookup e m) of 
			Just v -> Data.Map.insert e (v+1) m 
			Nothing -> Data.Map.insert e 1 m

stripR :: Char -> String -> String
stripR x = reverse . dropWhile (==x) . reverse

evallines::Data.Map.Map String Int ->String-> Data.Map.Map String Int
evallines m e = foldl (count) m (map (stripR '.') (words(map toLower e)))


main :: IO ()
main = do 
        f   <- readFile ("2011_12_26.txt")
        let linesStr = lines f
        print(Data.Map.toList (foldl evallines Data.Map.empty linesStr))
     

