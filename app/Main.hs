--- Basically gonna be super_hot
module Main where

import CodeWorld
import Controller
import Model
import View

main :: IO ()
main = interactionOf initialModel handleTime handleEvent modelToPicture
  where handleTime _ s = s