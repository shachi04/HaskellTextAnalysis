module Cosine_tfidf(query_documents, appendStr, removeDir, tf_idf, similarity) 
where

import qualified Data.Map
import Data.Char
import System.IO
import Control.Monad
import Data.List
import System.Directory
import Data.String.Utils
import Data.Monoid
import Data.Function (on)
import Data.List (sortBy)
import System.Environment
import System.IO.Unsafe (unsafePerformIO)



tf_idf ::[String] -> [String] ->[(String, [(String, Double)])]
tf_idf d f = do
		let termfreq = termfrequency d f 
		let noofdocs = length $ removeDir d
		let m = map idf f
		--let computeidf = idf noofdocs
		let k = unzip $ mconcat m 
		let idfc = Data.Map.toList(foldl (count) Data.Map.empty (fst k))
		let idfmap = Data.Map.fromList(map (calcidf noofdocs) idfc)
		--print(idfmap)
		let hashmaplist = map Data.Map.toList termfreq
		let tfidf =  map (findtfidf idfmap) hashmaplist
		(concat) tfidf

termfrequency :: [String] -> [String] ->  [Data.Map.Map String [(String, Int)]]
termfrequency d f = do 
				let termfreq = map tf (zip (removeDir d) f)
				termfreq

similarity :: [String] -> [String] -> Double
similarity d f =  do 
				
		let termfreq = termfrequency d f 
		let noofdocs = length $ removeDir d
		let mapall = map makemap $ mconcat $ map Data.Map.elems termfreq
		let listofstrings = map fst $Data.Map.toList $ Data.Map.fromListWith (+) ((mconcat.mconcat) (map Data.Map.elems termfreq))
		let firstdoc = buildfreqmap (mapall!!0) listofstrings
		let seconddoc = buildfreqmap (mapall!!1) listofstrings
		let concatmap = Data.Map.toList(firstdoc) ++ Data.Map.toList(seconddoc)
		cosinesim concatmap (map snd (Data.Map.toList(firstdoc))) (map snd (Data.Map.toList(seconddoc))) 

query_documents :: String -> [String] -> [String] -> [(String,Double)]
query_documents qs d f =  do 
				
		let noofdocs = length $ removeDir d
		let termfreq = termfrequency d f
		let tfidf = tf_idf d f		
		let s = map (query (words(qs))) (tfidf)
		let filtered = filter (\(_,y) -> y /= 0.0) ((concat) s)
		let sorted = reverse (sortBy (compare `on` snd) (filtered))
		sorted



count::  Data.Map.Map String Int ->String-> Data.Map.Map String Int
count m e = case (Data.Map.lookup e m) of 
			Just v -> Data.Map.insert e (v+1) m 
			Nothing -> Data.Map.insert e 1 m

count1::  Data.Map.Map String Int ->String-> Data.Map.Map String Int
count1 m e = Data.Map.insert e 1 m 
			
evallines1::Data.Map.Map String Int ->String-> Data.Map.Map String Int
evallines1 m e = foldl (count1) m (words $ filter (\x -> isAlpha x || isSpace x) $(map toLower e))

idf::  String -> [(String,Int)]
idf contents  = do
					let linesStr = lines contents 
					Data.Map.toList(foldl evallines1 Data.Map.empty linesStr)

evallines::Data.Map.Map String Int ->String-> Data.Map.Map String Int
evallines m e = foldl (count) m (words $ filter (\x -> isAlpha x || isSpace x) $(map toLower e))

notstartswith :: String -> Bool
notstartswith b = not (startswith "." b)

removeDir :: [FilePath] -> [FilePath]
removeDir x = filter notstartswith x

mapit :: String -> [(String,Int)] -> [(String,[(String , Int)])]
mapit s l = (Data.Map.toList(Data.Map.fromList [(s, l)]))

tf :: (FilePath , String) -> Data.Map.Map String [(String, Int)]
tf (filename,contents) = do
						let linesStr = lines contents 
						Data.Map.fromList $ mapit filename (Data.Map.toList(foldl evallines Data.Map.empty linesStr))

appendStr :: String -> String -> String
appendStr a b = a++b


calcidf :: Int -> (String,Int) -> (String, Double)
calcidf k b = (fst b , 1.0*log((fromIntegral k :: Double )/(fromIntegral (snd b) :: Double )  ))

lookandcompute :: Data.Map.Map String Double->(String,Int)->(String,Double)
lookandcompute m (s,i) = case (Data.Map.lookup s m) of 
			Just v -> (s,(fromIntegral i :: Double )* v)
			Nothing -> (s,0.0)

findtfidf :: Data.Map.Map String Double-> [(String,[(String,Int)])] -> [(String,[(String,Double)])]
findtfidf m [(a,b)] = [(a, map (lookandcompute m) b)]

makemap ::(Ord k, Num n) =>  [(k, n)] -> Data.Map.Map k n
makemap list = foldl' (\m (k, n) -> Data.Map.alter (Just . maybe n (+ n)) k m) Data.Map.empty list

buildfreqmap ::  Data.Map.Map String Int -> [String] -> Data.Map.Map String Int
buildfreqmap m (e:es) = buildfreqmap ( case (Data.Map.lookup e m) of 
					Just v -> Data.Map.insert e v m 
					Nothing -> Data.Map.insert e 0 m ) es 
buildfreqmap m _ = m 

cosinesim :: [(String,Int)] -> [Int] -> [Int] -> Double
cosinesim a b c = (fromIntegral $ sum (Data.Map.elems (Data.Map.fromListWith (*) a)) ::Double) / (magnitude b * magnitude c)

magnitude :: [Int] -> Double
magnitude a = sqrt $ sum (map square a)

square :: Int -> Double
square a = fromIntegral(a*a) :: Double

query :: [String] -> (String, [(String, Double)]) -> [(String,Double)]
query str tfidf = [((fst tfidf ), foldl (search tfidf) 0.0 str)]

search ::  (String, [(String, Double)])  -> Double-> String -> Double
search (a, b) d str = do
					let bmap = Data.Map.fromList(b)
					case (Data.Map.lookup str bmap) of 
						Just v -> d+v
						Nothing -> d+0.0	

combinetfidf :: [(String, [(String,Double)])] -> (String, Double)
combinetfidf [(a, b)] = (a, (foldl (+) 0.0 (map snd b)))
combinetfidf _ = ("nothing", 0.0)









		
		
