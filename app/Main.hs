module Main where

import Actions
import Lib
import Types

main :: IO ()
main = do
  putStrLn "******************************"
  putStrLn "        MultiSig Wallet"
  putStrLn "******************************"
  runApp