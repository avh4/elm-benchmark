module Benchmark 
    ( logic
    , render
    , renderStatic
    , run
    , Benchmark
    ) where
{-| 
Simple Benchmarking library

# Basic Benchmarks
@docs logic, render, renderStatic

# Run the benchmark
@docs run

-}

import Benchmark.Types as T
import Benchmark.Runner as R
import List (..)
import Graphics.Element (..)

-- Bindings from other files for a cleaner export
type alias Benchmark = T.Benchmark


{-| Create a logic benchmark, running a function on many different inputs.
You provide a name, a function, and a list of input values. After the benchmark
suite runs, you will see results for each input all labeled with the given name.
 
      logic "Date Parsing" parseDate [ "1/2/1990", "1 Feb 1990", "February 1, 1990" ]
-}
logic : String -> (input -> output) -> List input -> Benchmark
logic name function inputs = 
  let noSetup f input = \_ -> let muted a = always () (f a)
                              in \_ -> muted input
  in  T.Logic name <| map (noSetup function) inputs


{-| Create a rendering benchmark, rendering a sequence of states. You provide a
name, a rendering function, and a sequence of states. Running this benchmark
measures the whole rendering pipeline.
 
      render "Profile" userProfile [ { user=123, friends=0 }
                                   , { user=123, friends=1 }
                                   , { user=123, friends=2 }
                                   , { user=123, friends=1 }
                                   ]

The sequence of states really is a *sequence*. They are run in order, so you can
see how well Elm's diffing engine does given the particular sequence you give it.
It may help to record a sequence of states directly from your project. Better to
use real data instead of making it up!
-}
render : String -> (input -> Element) -> List input -> Benchmark
render name function inputs =
  let noSetup f input = \_ _-> f input
  in  T.Render name <| map (noSetup function) inputs


{-| Just get the cost of rendering from scratch. This does not get any of
the benefits of diffing to speed things up, so it is mainly useful for
assessing page load time.

      renderStatic "Markdown rendering" markdownBlock
-}
renderStatic : String -> Element -> Benchmark
renderStatic name element = render name (\_ -> element) [()]


{-| For each benchmark, run it 10 times in a row and average the times. If the
benchmark needs to render something, it goes to screen. Once the benchmarks are
completed, the screen will change to display them as a line graph
    
    benchmarks = [ render "Blur image" blurPonyPNG [1..50]
                 , logic  "Compute determinant" [m1, m2, m3, m4]
                 ]
    main = run benchmarks
-}
run : List Benchmark -> Signal Element
run = R.run
