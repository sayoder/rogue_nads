module Main where

import qualified System.Environment as SysEnv
import qualified System.Exit as SysExit

exit :: IO a
exit = SysExit.exitWith (SysExit.ExitSuccess)

helpStr :: IO ()
helpStr = putStrLn "usage: rogue_nads [-h] [--version]"

versionStr :: IO ()
versionStr = putStrLn "rogue_nads v.0.0.1"

runProgram :: IO ()
runProgram = putStrLn "Ideally this program will do something eventually."

--Convert lists of arguments like this: ["-abc","-d","-ef"] into this:
----["-a","-b","-c","-d","-e","-f"]
splitArgs :: [String] -> [String]
splitArgs argList = (argList >>= (reverse . splitArgs'))
    where splitFunc ls arg = ('-':[arg]):ls --given [] and "abc", returns ["-a"].
          splitArgs' (x:xs) = if x == '-' && (head xs /= '-') && length xs > 1 --Make sure it's not a double-dash arg
                              then foldl splitFunc [] xs
                              else [(x:xs)]

parse :: [String] -> IO a
parse [] = runProgram >> exit --with no arguments left, run the program.
parse (x:xs)
    | x == "-h" || x == "--help" = helpStr >> exit --Typically programs exit after -h or --help.
    | x == "--version" = versionStr >> exit
    | otherwise = helpStr >> SysExit.exitWith (SysExit.ExitFailure 1) -- given an unrecognized argument

main = SysEnv.getArgs >>= (return . splitArgs) >>= parse