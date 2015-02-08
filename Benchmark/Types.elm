module Benchmark.Types where

import Time (..)
import Graphics.Element (..)

type alias Result = { name:String, times:List Time }

type Benchmark = Logic String (List (() -> () -> ()))
               | Render String (List (() -> () -> Element))