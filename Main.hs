-- servant example
-- ####################################
-- module Main where
--
-- import Network.Wai.Handler.Warp
--
-- import Handler
--
-- main :: IO ()
-- main = run 8081 app3
-- ####################################

-- wai example
module Main where

import Network.Wai.Handler.Warp (run)

import YWai

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app2