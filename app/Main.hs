module Main where

import Templates

import Control.Applicative (optional)
import Happstack.Server
import Control.Monad (msum)
import System.Environment
import Data.Char
import Text.Read
import Data.Maybe

main :: IO ()
main = 
  do
    args <- getArgs
    envPortStr <- lookupEnv "PORT"
    let argPort = readPort args
        envPort = fmap (\p -> read p :: Int) envPortStr
        portNumber = head $ maybeToList argPort ++ maybeToList envPort ++ [8000]
    putStrLn $ "app started on port: " ++ (show portNumber) ++ " use `-port number` to change it"
    simpleHTTP nullConf {port = portNumber} myApp

readPort :: [String] -> Maybe Int
readPort (x:[]) = Nothing
readPort [] = Nothing
readPort (x:y:zs) = 
  let
    lowX = map toLower x
    portNumber = readMaybe y :: Maybe Int
  in
    if lowX == "-port"
      then portNumber
      else readPort $ y:zs

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

myApp :: ServerPart Response
myApp = 
  do decodeBody myPolicy
     msum [ dir "echo" $ echo
          , dir "query" $ queryParams
          , dir "form" $ formPage
          , homePage]

homePage :: ServerPart Response
homePage = ok homePageTemplate

echo :: ServerPart Response
echo =
    path $ \msg -> 
      ok $ echoTemplate msg

queryParams :: ServerPart Response
queryParams =
    do mFoo <- optional $ lookText "foo"
       ok $ queryParamsTemplate $ mFoo

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
        do method GET
           ok $ formPageGetTemplate

    processForm :: ServerPart Response
    processForm =
        do method POST
           msg <- lookText "msg"
           ok $ formPagePostTemplate msg
