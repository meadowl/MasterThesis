{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.JavaScript
  
--import Paths_javascript_bridge

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  let dataDir = "/mnt/c/Users/drago/Desktop/javascript-bridge/.stack-work/install/x86_64-linux/d93dcbaeb910596a03f83ac2d9557baaa4d28cf16ab2f5e4a81ad9d8c6e1d254/8.6.3/share/x86_64-linux-ghc-8.6.3/javascript-bridge-0.2.0"
  -- dataDir <- return "." -- use for debugging
  scotty i $ do
    middleware $ start app  
    get "/" $ file $ dataDir ++ "/examples/Main.html"

app :: Engine -> IO ()
app eng = do
  send eng $ do
    command $ call "console.log" [string "starting..."]
    render "Hello!"

-- It is good practice to reflect the JavaScript utilties
-- you are using as typed Haskell functions.
render :: Command f => String -> f ()
render t = command $ call "jsb.render" [value t]
