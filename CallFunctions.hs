import System.IO
import System.Directory
import Cosine_tfidf(query_documents, appendStr, removeDir, tf_idf, similarity)
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	d <- getDirectoryContents (args!!0)
	f <- mapM readFile(map (appendStr (args!!0)) (removeDir d))
	print("PRINTING THE TFIDF")
	mapM print (tf_idf d f)
	print("PRINTING THE COSINE SIMILARITY")
	print(similarity d f)
	enter <- putStrLn "ENTER YOUR SEARCH QUERY"
	querystring <-getLine
 	print(query_documents querystring d f)
