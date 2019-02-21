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
-- ####################################
-- module Main where
--
-- import Network.Wai.Handler.Warp (run)
--
-- import YWai
--
-- main :: IO ()
-- main = do
--     putStrLn $ "http://localhost:8080/"
--     run 8080 app
-- ####################################

-- miso sample

 -- | Haskell language pragma
 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE RecordWildCards #-}

 -- | Haskell module declaration
 module Main where

 -- | Miso framework import
 import Miso
 import Miso.String

 import Sample

 -- | Entry point for a miso application
 main :: IO ()
 main = startApp App {..}
   where
     initialAction = SayHelloWorld -- initial action to be executed on application load
     model  = 0                    -- initial model
     update = updateModel          -- update function
     view   = viewModel            -- view function
     events = defaultEvents        -- default delegated events
     subs   = []                   -- empty subscription list
     mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')