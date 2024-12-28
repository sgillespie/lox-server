module Development.Server.Parser.InternalSpec (spec) where

import Development.Lox.Server.Parser (readLoxFile)
import Development.Lox.Server.Parser.Internal (expr, runParseProgram)
import Development.Lox.Server.Types
import Paths_obloxious (getDataFileName)

import Test.Hspec hiding (Spec)
import Test.Hspec.Megaparsec (shouldParse, shouldSucceedOn)
import Test.Syd (Spec)
import Test.Syd.Hspec (fromHspec)
import Text.Megaparsec (runParser)

spec :: Spec
spec = fromHspec $ do
  describe "parseProgram" $ do
    it "expressions.lox" $ do
      loxFile <- getDataFileName "test/data/expressions.lox"
      loxText <- readLoxFile loxFile
      runParseProgram loxFile `shouldSucceedOn` loxText

    it "statements.lox" $ do
      loxFile <- getDataFileName "test/data/statements.lox"
      loxText <- readLoxFile loxFile
      runParseProgram loxFile `shouldSucceedOn` loxText

    it "declarations.lox" $ do
      loxFile <- getDataFileName "test/data/declarations.lox"
      loxText <- readLoxFile loxFile
      runParseProgram loxFile `shouldSucceedOn` loxText

  describe "expr" $ do
    let parseExpr = runParser expr "<inline>"
        parseExpr' = fmap void . parseExpr

    it "super expression" $ do
      parseExpr' "super.x" `shouldParse` LoxSuper () "x"
