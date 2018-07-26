----------------------------------------------------------------
--- Executes Curry code snippets occurring in a latex file so that they
--- are replaced by their results computed by the Curry code.
---
--- The code snippets are marked by \runcurry{...} where
--- the argument must not contain any curly brackets or backslashes.
--- Furthermore, it must be a valid Curry expression of type `IO String`.
--- Moreover, the latex file can also contain program lines enclosed by
--- \begin{curryprog}
--- ...
--- \end{curryprog}
--- These program lines are included in the Curry program before the
--- code snippets are executed.
---
--- @author Michael Hanus
--- @version October 2014
----------------------------------------------------------------

import Data.Char          ( isDigit )
import Data.Time
import Data.List          ( intercalate )
import System.Directory
import System.FilePath    ( (</>), takeDirectory, takeExtension, dropExtension )
import System.IO
import System.Process
import System.Environment
import Distribution       ( installDir,  curryCompiler)
import ReadShowTerm       ( readQTerm )

import BenchmarkPackageConfig ( packagePath )

-- Curry system executable to be used to execute Curry code snippets.
currySystem :: String
currySystem = installDir++"/bin/"++curryCompiler

-- The cleancurry command of the Curry system
cleanCurry :: String
cleanCurry = installDir++"/bin/cleancurry"

-- The LaTeX macro file containing definitions for Curry macros:
currycodeFile :: String
currycodeFile = "currycode.sty"

main :: IO ()
main = getArgs >>= processArgs False

processArgs :: Bool -> [String] -> IO ()
processArgs runlatex args = case args of
  [] -> showError
  ["-h"]      -> showHelp
  ["-?"]      -> showHelp
  ["--help"]  -> showHelp
  "-f":rargs  -> processArgs True rargs
  [infile]    -> if head infile == '-'
                 then showError
                 else let texfile = if takeExtension infile == "tex"
                                    then infile
                                    else infile++".tex"
                       in mainExec texfile runlatex
  _ -> showError
 where
  showError =
    error ("Illegal arguments (use '--help' for description):\n"++unwords args)

showHelp :: IO ()
showHelp = putStrLn $
  "Usage: <prog> <options> <texfile>\n" ++
  "where <options> can contain:\n"++
  "-h     : show help info\n"++
  "-?     : show help info\n"++
  "--help : show help info\n"++
  "-f     : format generated LaTeX file with pdflatex and show it with evince\n"

mainExec :: String -> Bool -> IO ()
mainExec textfile runlatex = do
  hascode <- extractCode textfile
  when (hascode && runlatex) $ do
    st <- system $ "pdflatex "++textfile
    when (st==0) (system ("evince "++dropExtension textfile++".pdf") >> done)
    exitWith st

-- Extract Curry code snippets from a latex file.
-- The code snippets are stored in a temporary Curry program as
-- pairs of the code snippet text and the code snippet.
-- Then the temporary Curry program is executed to generate
-- a latex macro file (with suffix `.currycode.tex`) defining
-- for each code snippet its execution result.
extractCode :: String -> IO Bool
extractCode textfile = do
  pid <- getPID
  let tmpname   = "tmpxxx"++show pid
      curryfile = tmpname++".curry"
      macrofile = dropExtension textfile ++ ".currycode.tex"
  absmacrofile <- getAbsolutePath macrofile
  cnts <- readFile textfile
  hc <- openFile curryfile WriteMode
  hPutStrLn hc "import ExecuteBenchmarkPaper\n"
  codesnippets <- extractCurryCode hc cnts
  if null codesnippets
   then do
     hClose hc
     removeFile curryfile
     putStrLn "No code snippets found, nothing done"
     return False
   else do
     saveOldFile absmacrofile
     copyFile (packagePath </> "include" </> currycodeFile)
              (takeDirectory absmacrofile++'/':currycodeFile)
     hPutStrLn hc $
       concatMap genMacro (zip [1..] codesnippets) ++
       "\nmain :: IO ()" ++
       "\nmain = genMacroFile [" ++
                 intercalate "," (map (\i->macroOpName i)
                                      [1 .. length codesnippets]) ++
                 "] \""++absmacrofile++"\"\n"
     hClose hc
     putStrLn "Computing results for Curry code snippets..."
     ec <- system $ unwords [currySystem,":set path",packagePath </> "src",
                             ":load",curryfile,":eval main",":quit"]
     system $ cleanCurry ++ " " ++ tmpname
     if ec==0
      then removeFile curryfile >> return True
      else error $ "Something went wrong when executing Curry code snippets\n"++
                   "Inspect generated Curry program in \""++curryfile++"\"\n"
 where
   macroOpName i = "runCurryMacro" ++ show i

   genMacro (i,m) =
     '\n':macroOpName i++" :: (String, IO String)\n"++
          macroOpName i++" = ("++show m++","++m++")\n"

--- If a file with the given name `fname` exists, it is moved
--- to the file `fname.date`, where `date` is the modification date
--- of this file.
saveOldFile :: String -> IO ()
saveOldFile fname = do
  exfname <- doesFileExist fname
  when exfname $ do
    mdate <- getModificationTime fname
    ctime <- toCalendarTime mdate
    let savename = fname ++ "." ++
                   intercalate "_"
                     (map (\f -> show (f ctime))
                          [ctYear,ctMonth,ctDay,ctHour,ctMin,ctSec])
    renameFile fname savename

--- Extract \begin{curryprog}...\end{curryprog} and \runcurry{...} from a text.
--- The "{...}" must not contain any further curly brackets.
--- The extracted `curryprog` code is written to the handle.
extractCurryCode :: Handle -> String -> IO [String]
extractCurryCode _ [] = return []
extractCurryCode hc (c:cs) = case c of
  '\\' -> if take 9 cs == "runcurry{"
          then let (s,ds) = break (=='}') (drop 9 cs)
                in if null ds
                   then error ("UNCLOSED MACRO: "++c:take 50 cs++"...")
                   else do xs <- extractCurryCode hc (tail ds)
                           return (s:xs)
          else extractCurryCode hc cs
  '\n' -> if not (null cs) && head cs == '%'
          then skipLine cs
          else if take 17 cs == "\\begin{curryprog}"
               then copyCurryCode hc (drop 17 cs) >>=
                    extractCurryCode hc
               else extractCurryCode hc cs
  _    -> extractCurryCode hc cs
 where
  skipLine [] = return []
  skipLine (d:ds) = if d=='\n'
                    then extractCurryCode hc (d:ds)
                    else skipLine ds

copyCurryCode :: Handle -> String -> IO String
copyCurryCode _ [] = error "\\end{curryprog} missing!"
copyCurryCode hc (d:ds) = case d of
  '\n' -> if take 15 ds == "\\end{curryprog}"
          then hPutChar hc '\n' >> return (drop 15 ds)
          else hPutChar hc d >> copyCurryCode hc ds
  _    -> hPutChar hc d >> copyCurryCode hc ds


-- Generate a LaTeX file containing results of macro execution.
genMacroFile :: [(String,IO String)] -> String -> IO ()
genMacroFile macros outfile = do
  s <- mapIO showMacro macros
  writeFile outfile ("\\newcommand{\\curryresult}[1]\n"++genResultMacro s++"\n")
  putStrLn $ "Execution results written into file '"++outfile++"'"
 where
  showMacro (m,act) = do
    bmReport m
    s <- act
    return ("\\ifthenelse{\\equal{#1}{" ++m ++ "}}{" ++ s ++"}\n")

  genResultMacro [] = "{\\{\\texttt{#1}\\}}\n"
  genResultMacro (t:ts) = "{"++t++"{"++genResultMacro ts++"}}"

-- Report execution of a code snippet:
bmReport :: String -> IO ()
bmReport c = putStrLn $ "Running benchmark code: " ++ c

----------------------------------------------------------------
