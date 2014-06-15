module Main where

import qualified System.Environment as SysEnv
import qualified System.Exit as SysExit

exit :: IO a
exit = SysExit.exitWith (SysExit.ExitSuccess)

helpStr :: IO ()
helpStr = putStrLn "usage: rogue_nads [-h] [-v]"

versionStr :: IO ()
versionStr = putStrLn "rogue_nads v.0.0.1"

runProgram :: IO ()
runProgram = putStrLn "Ideally this program will do something eventually."

parse :: [String] -> IO a
parse [] = runProgram >> exit
parse (x:xs)
    | x == "-h" = helpStr >> parse xs
    | x == "-v" = versionStr >> parse xs
    | otherwise = SysExit.exitWith (SysExit.ExitFailure 1)

main = SysEnv.getArgs >>= parse
