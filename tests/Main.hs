{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import qualified Control.Monad as Monad
import qualified Glenda as Go
import qualified System.Exit as Exit
import qualified Text.Printf as Printf

main :: IO ()
main = runTests
  [ Test "parseNewline"
    (Go.runParse Go.parseNewline "\n")
    (Just Go.Newline)
  , Test "renderNewline"
    (Go.runRender Go.renderNewline Go.Newline)
    "\n"
  , Test "parseUnicodeChar"
    (Go.runParse Go.parseUnicodeChar "!")
    (Just (Go.UnicodeChar '!'))
  , Test "renderUnicodeChar"
    (Go.runRender Go.renderUnicodeChar (Go.UnicodeChar '!'))
    "!"
  , Test "parseUnicodeLetter"
    (Go.runParse Go.parseUnicodeLetter "A")
    (Just (Go.UnicodeLetter 'A'))
  , Test "renderUnicodeLetter"
    (Go.runRender Go.renderUnicodeLetter (Go.UnicodeLetter 'A'))
    "A"
  , Test "parseUnicodeDigit"
    (Go.runParse Go.parseUnicodeDigit "0")
    (Just (Go.UnicodeDigit '0'))
  , Test "renderUnicodeDigit"
    (Go.runRender Go.renderUnicodeDigit (Go.UnicodeDigit '0'))
    "0"
  , Test "parseLetter"
    (Go.runParse Go.parseLetter "_")
    (Just Go.Letter_Underscore)
  , Test "renderLetter"
    (Go.runRender Go.renderLetter (Go.Letter_Underscore))
    "_"
  , Test "parseDecimalDigit"
    (Go.runParse Go.parseDecimalDigit "0")
    (Just Go.DecimalDigit_0)
  , Test "renderDecimalDigit"
    (Go.runRender Go.renderDecimalDigit (Go.DecimalDigit_0))
    "0"
  , Test "parseOctalDigit"
    (Go.runParse Go.parseOctalDigit "0")
    (Just Go.OctalDigit_0)
  , Test "renderOctalDigit"
    (Go.runRender Go.renderOctalDigit (Go.OctalDigit_0))
    "0"
  , Test "parseOctalDigit"
    (Go.runParse Go.parseHexDigit "0")
    (Just Go.HexDigit_0)
  , Test "renderHexDigit"
    (Go.runRender Go.renderHexDigit (Go.HexDigit_0))
    "0"
  , Test "parseComment"
    (Go.runParse Go.parseComment "//\n")
    (Just '\n')
  , Test "parseLineComment"
    (Go.runParse Go.parseLineComment "//\n")
    (Just '\n')
  , Test "parseGeneralComment"
    (Go.runParse Go.parseGeneralComment "/**/")
    (Just ' ')
  , Test "parseWhiteSpace"
    (Go.runParse Go.parseWhiteSpace "")
    (Just "")
  , Test "parseIdentifier"
    (Go.runParse Go.parseIdentifier "_")
    (Just (Go.Identifier Go.Letter_Underscore []))
  , Test "renderIdentifier"
    (Go.runRender Go.renderIdentifier (Go.Identifier Go.Letter_Underscore []))
    "_"
  ]

runTests :: [Test] -> IO ()
runTests tests = do
  printIntro tests
  let (passed, failed, messages) = foldr runTest (0, 0, []) tests
  mapM_ putStrLn messages
  printOutro passed failed
  Monad.when (failed > 0) Exit.exitFailure

printIntro :: [Test] -> IO ()
printIntro tests = Printf.printf
  "Running %d test%s ...\n"
  (length tests)
  (if length tests == 1 then "" else "s")

runTest :: Test -> (Word, Word, [String]) -> (Word, Word, [String])
runTest (Test label actual expected) (passed, failed, messages) =
  if actual == expected
    then (succ passed, failed, showPass label : messages)
    else (passed, succ failed, showFail label actual expected : messages)

showPass :: String -> String
showPass label = Printf.printf "- [pass] %s" label

showFail :: Show a => String -> a -> a -> String
showFail label actual expected = Printf.printf
  "- [fail] %s: expected %s but got %s"
  label
  (show expected)
  (show actual)

printOutro :: Word -> Word -> IO ()
printOutro passed failed = Printf.printf
  "Ran %d test%s. %d test%s passed. %d test%s failed.\n"
  (passed + failed)
  (if passed + failed == 1 then "" else "s")
  passed
  (if passed == 1 then "" else "s")
  failed
  (if failed == 1 then "" else "s")

data Test
  = forall actual expected
  . (actual ~ expected, Eq actual, Show actual)
  => Test String actual expected
