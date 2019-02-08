-----------------------------------------------------------------------
--- A benchmark to test different Curry system with the
--- naive reverse benchmark and visualize the results with gnuplot.
-----------------------------------------------------------------------

import System

import Test.Benchmark
import Test.Benchmark.Goodies

-----------------------------------------------------------------------
-- Compile a Curry program (here: NRev) with different Curry systems
-- (e.g., pakcs, kics2, mcc) and execute some benchmarks with
-- a given list of list lengths and a time limit.

-- The compilers used in the benchmarks
data CurrySystem = PAKCS
                 | MCC
                 | KiCS2 String -- ghc options

compileCurry :: CurrySystem -> String -> IO ()
compileCurry cs prog = system cmd >> done
 where
  cmd = case cs of
          MCC        -> "/opt/mcc/bin/cyc -o "++prog++" "++prog++".curry"
          PAKCS      -> "pakcs :load "++prog++" :save :quit"
          KiCS2 opts -> "kics2 "++opts++" :load "++prog++" :save :quit"

cleanCurry :: CurrySystem -> String -> IO ()
cleanCurry cs prog = case cs of
  MCC   -> system ("/bin/rm -f "++prog++".icurry "++prog) >> done
  PAKCS -> do system ("/opt/pakcs/bin/cleancurry "++prog)
              system ("/bin/rm -f "++prog)
              done
  KiCS2 _ -> do system ("/opt/kics2/bin/cleancurry "++prog)
                system ("/bin/rm -f "++prog)
                done

nrevProgBenchsWithLimit :: CurrySystem -> Float -> [Int]
                        -> Benchmark [(Int,Float)]
nrevProgBenchsWithLimit currysystem tlimit listlens =
  runUntilNothingOn (\n -> 3 *>- nrevBenchWithLimit n) listlens
    `withPrepare` compileCurry currysystem "NRev"
    `withCleanup` cleanCurry   currysystem "NRev"
 where
  nrevBenchWithLimit n =
    mapBench (maybe Nothing (Just . cpuTime))
             (benchCommandWithLimit ("./NRev "++show n) tlimit)

benchCurrySystemsAndPlot :: Float -> [Int] -> String -> IO String
benchCurrySystemsAndPlot tlimit inputs outfile = do
  pdata  <- execBench (nrevProgBenchsWithLimit PAKCS      tlimit inputs)
  kdata  <- execBench (nrevProgBenchsWithLimit (KiCS2 "") tlimit inputs)
  kwodata <- execBench (nrevProgBenchsWithLimit (KiCS2 ":set -opt") tlimit inputs)
  kodata <- execBench (nrevProgBenchsWithLimit (KiCS2 ":set ghc -optc-O3") tlimit inputs)
  mdata  <- execBench (nrevProgBenchsWithLimit MCC        tlimit inputs)
  plotResults outfile
              [Lines, Title "nrev run times",
               XLabel "list length", YLabel "run time (seconds)"]
              [("pakcs",pdata)
              ,("kics2",kdata)
              ,("kics2 no opt",kwodata)
              ,("kics2 -optc-O3",kodata)
              ,("mcc",mdata)
              ]
  return ("\\includegraphics[width=\\linewidth]{"++outfile++"}")

-----------------------------------------------------------------------
