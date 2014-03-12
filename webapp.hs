{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Cosine_tfidf(query_documents, appendStr, removeDir)
import System.IO.Unsafe (unsafePerformIO)
import Yesod
import Control.Applicative
import Data.Text (Text, unpack,pack)
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

--addStylesheet $ StaticR css_bootstrap_css

data MFormExample = MFormExample

mkYesod "MFormExample" [parseRoutes|
/ RootR GET
|]

instance Yesod MFormExample

instance RenderMessage MFormExample FormMessage where
  renderMessage _ _ = defaultFormMessage

data Search = Search { querystring :: Text}
              deriving Show


documentForm :: Html -> MForm Handler (FormResult Search, Widget)
documentForm extra =  do
    (queryRes, queryView) <- mreq textField "this is not used" Nothing
    let docRes = Search <$> queryRes  
    let p = String <$> queryRes
    let widget = do
            [whamlet|
                <p>
                    <center> DOCUMENT SEARCH AND RELEVANCE RANKING ENGINE <br> <br> <br> <br> <br> <br> <br>
            |]
            [whamlet|
                #{extra}
                <p>
                    <center>Input your query string<br>
                    <center> ^{fvInput queryView} <br>
                    <center><input type=submit value="Search">
                     
            |]

            
            toWidget
                [lucius|
                    ##{fvId queryView} {
                        width: 50em;
                    }
                        
                |]

    return (docRes, widget)
t:: [String]
t = unsafePerformIO $ do 
        let directory = "./YourDirectory"
        d <- getDirectoryContents (directory)
        f <- mapM readFile(map (appendStr directory) (removeDir d)) 
        let a  = (map appendStr (removeDir d))
        return (removeDir d) 
       
t2:: [String]
t2 = unsafePerformIO $ do 
        let directory = "./YourDirectory"
        d <- getDirectoryContents (directory)
        f <- mapM readFile(map (appendStr directory) (removeDir d))       
        return f  

l :: Search -> String
l (Search a) = unpack a 
query ::FormResult Search -> String  
query (FormSuccess (Search qs)) = unpack qs
query _ = " "

getRootR :: Handler Html
getRootR =  do
        ((res,  widget), enctype) <- runFormGet documentForm
        let b = t
        let contents = t2

        let s = query_documents (query res) b contents

        defaultLayout
            [whamlet|
                <form enctype=#{enctype}>
                    ^{widget}
                    <table>
                        <tr>
                            <td>Document Rank  &nbsp <td> TFIDF
                     
                     $forall (d) <- s
                       <tr>
                            <td>#{(fst d)} <td> #{show (snd d)}<br>
            |]



main :: IO ()
main = do 
        warp 3000 MFormExample
