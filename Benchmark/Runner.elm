module Benchmark.Runner
    ( run
    ) where

import Benchmark.Types (..)
import Benchmark.Types
import Benchmark.LineGraph (..)
import Native.Runner
import Result as Core
import Window
import List (..)
import Signal
import Graphics.Element (..)

type alias Result = Benchmark.Types.Result

numRepeats = 10

duplicateEach : Int -> List a -> List a
duplicateEach n xs = foldr (++) [] <| map (repeat n) xs

{-| Condenses every N elements in a list with `f`
-}
condenseEach : Int -> (List a -> a) -> List a -> List a
condenseEach n f xs = case xs of
    [] -> []
    ys -> f (take n ys) :: condenseEach n f (drop n ys)

{-| Implicit assumption that we've got the same type of result.
They should all have the same name and the same number of elements in .times
-}
averageResults : List Result -> Result
averageResults results =
    let n = length results
        times = map .times results
        numberOfData = length <| head times
        summed = foldr (\t s -> map2 (+) t s) (repeat numberOfData 0) times
        avgs = map (\x -> toFloat (round ((x / toFloat n) * 10))/10 ) summed
    in { name=(head results).name, times=avgs }


run : List Benchmark -> Signal Element
run bms =
    let repeatedBms = duplicateEach numRepeats bms
    in  Signal.map2 display Window.width <| Native.Runner.runMany repeatedBms


display : Int -> Core.Result Element (List Result) -> Element
display w elementString = case elementString of
    Core.Err element  -> element
    Core.Ok results -> 
        let avgs = condenseEach numRepeats averageResults results
        in  showResults w avgs
