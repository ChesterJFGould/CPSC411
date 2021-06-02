module Main where

import Values
import Unique
import Cmf
import Asm
import Para
import Paren

import System.Environment
import System.IO -- (hPutStrLn, hPutStr, stderr)
import System.IO.Temp
import System.Process

prelude :: IO String
prelude = readFile "Runtime/prelude.s"

postlude :: IO String
postlude = readFile "Runtime/postlude.s"

compileX86 :: String -> String -> IO ()
compileX86 outFile input = withTempDirectory "." "build"
                           $ (\buildDir ->
                              writeFile (concat [buildDir, "/", "tmp.s"]) input
                              -- >> hPutStr stderr input
                              >> (callCommand $ unwords [ "nasm"
                                                        , "-f"
                                                        , "elf64"
                                                        , "-o"
                                                        , concat [buildDir, "/", "tmp.o"]
                                                        , concat [buildDir, "/", "tmp.s"] ])
                              >> (callCommand $ unwords [ "ld"
                                                        , "-e"
                                                        , "start"
                                                        , "-o"
                                                        , outFile
                                                        , concat [buildDir, "/", "tmp.o"] ]))

runCompiler :: String -> String -> IO ()
runCompiler inFile outFile = do
                             prelude <- prelude
                             postlude <- postlude
                             readFile inFile >>= ( either (hPutStrLn stderr)
                                                          (compileX86 outFile)
                                                 . (>>= return . Paren.compile prelude postlude)
                                                 . (>>= return . Para.lower)
                                                 . (>>= return . Asm.lower)
                                                 . (>>= return . Cmf.lower)
                                                 . (>>= return . Unique.lower)
                                                 . (>>= return . Values.lower)
                                                 . parse "stdin" )

main :: IO ()
main = do
       args <- getArgs
       case args of
            (inputFile : outputFile : _) -> runCompiler inputFile outputFile
            _ -> putStrLn "please pass the input and output file as arguments"
