#!/usr/bin/env stack
-- stack --resolver lts-14.7 script
module Main where

import Lib 

main :: IO ()
main = pushpullMain
