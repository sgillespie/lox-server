module Development.Server.Parser.InternalSpec (spec) where

import Development.Lox.Server.Parser (readLoxFile)
import Development.Lox.Server.Parser.Internal (expr, runParseProgram)
import Development.Lox.Server.Span (Located, Position (..), UInt, mkLocated)
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

    it "binary operators" $ do
      let expr' = "a * b && (c || d) <= e"
          expectedResult =
            LoxBinary
              (mkRange (0, 0) (0, 22))
              LogicalAnd
              ( LoxBinary
                  (mkRange (0, 0) (0, 6))
                  Mul
                  (LoxVar (mkRange (0, 0) (0, 2)) "a")
                  (LoxVar (mkRange (0, 4) (0, 6)) "b")
              )
              ( LoxBinary
                  (mkRange (0, 10) (0, 22))
                  LessEq
                  ( LoxBinary
                      (mkRange (0, 10) (0, 16))
                      LogicalOr
                      (LoxVar (mkRange (0, 10) (0, 12)) "c")
                      (LoxVar (mkRange (0, 15) (0, 16)) "d")
                  )
                  (LoxVar (mkRange (0, 21) (0, 22)) "e")
              )

      parseExpr expr' `shouldParse` expectedResult

    it "unary expressions" $ do
      shouldParse (parseExpr "!a") $
        LoxUnary
          (mkRange (0, 0) (0, 2))
          Exclamation
          (LoxVar (mkRange (0, 1) (0, 2)) "a")

      shouldParse (parseExpr "-!a") $
        LoxUnary
          (mkRange (0, 0) (0, 3))
          Dash
          ( LoxUnary
              (mkRange (0, 1) (0, 3))
              Exclamation
              (LoxVar (mkRange (0, 2) (0, 3)) "a")
          )

mkRange :: (UInt, UInt) -> (UInt, UInt) -> Located
mkRange (l1, c1) (l2, c2) = mkLocated (Position l1 c1) (Position l2 c2)
