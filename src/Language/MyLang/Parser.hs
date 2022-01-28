{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Parser
  ( ParserError (..),
    parseFile,
  )
where

import Control.Exception (Exception (..), throwIO)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Language.MyLang.CST
import Text.Megaparsec hiding (State, match, try)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integerL :: Parser Int
integerL = lexeme L.decimal

identL :: Parser Ident
identL = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_") <?> "ident")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

termP :: Parser Expr
termP =
  choice
    [ parens exprP,
      Var <$> identL,
      Lit <$> integerL
    ]

exprP :: Parser Expr
exprP = makeExprParser termP operatorTable
  where
    operatorTable =
      [ [ binary "*" (BinOp (:*)),
          binary "/" (BinOp (:/)),
          binary "%" (BinOp (:%))
        ],
        [ binary "+" (BinOp (:+)),
          binary "-" (BinOp (:-))
        ],
        [ binary "<=" (BinOp (:<=)),
          binary ">=" (BinOp (:>=)),
          binary "<" (BinOp (:<)),
          binary ">" (BinOp (:>)),
          binary "==" (BinOp (:==)),
          binary "!=" (BinOp (:!=))
        ],
        [ binary "&&" (BinOp (:&&))
        ],
        [ binary "!!" (BinOp (:!!))
        ]
      ]
      where
        binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
        binary name f = InfixL (f <$ symbol name)

stmP :: Parser Stm
stmP = foldr1 Seq <$> stm `sepBy1` symbol ";"
  where
    stm =
      choice
        [ Read <$> (symbol "read" *> parens identL),
          Write <$> (symbol "write" *> parens exprP),
          Skip <$ symbol "skip",
          if_,
          do symbol "while"; cond <- exprP; symbol "do"; body <- stmP; symbol "od"; pure (While cond body),
          do symbol "repeat"; body <- stmP; symbol "until"; cond <- exprP; pure (Repeat body cond),
          do var <- identL; symbol ":="; expr <- exprP; pure (var := expr)
        ]

    if_ =
      do
        symbol "if"
        cond <- exprP
        symbol "then"
        then_ <- stmP
        elifs <- many do symbol "elif"; cond <- exprP; symbol "then"; body <- stmP; pure (cond, body)
        else_ <- optional (symbol "else" *> stmP)
        symbol "fi"
        pure (If cond then_ elifs else_)

data ParserError = ParserError String
  deriving stock (Show)

instance Exception ParserError where
  displayException (ParserError s) = s

parseSource :: FilePath -> String -> Either ParserError Stm
parseSource path src =
  case runParser (sc *> stmP <* eof) path src of
    Right stm -> Right stm
    Left ex -> Left (ParserError (errorBundlePretty ex))

parseFile :: FilePath -> IO Stm
parseFile path = do
  src <- readFile path
  case parseSource path src of
    Right stm -> pure stm
    Left ex -> throwIO ex
