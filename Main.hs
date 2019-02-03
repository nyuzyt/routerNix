-- servant example
-- ####################################
module Main where

import Network.Wai.Handler.Warp

import Handler

main :: IO ()
main = run 8081 app3
-- ####################################
