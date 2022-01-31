{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Parser
  ( ParserError (..),
    parseFile,
  )
where

import Control.Monad.Combinators.Expr
import Language.MyLang.CST
import Language.MyLang.Prelude
import Text.Megaparsec hiding (State, match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integerL :: Parser Int
integerL = lexeme L.decimal

ident :: Parser Ident
ident = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_") <?> "ident")

termP :: Parser Expr
termP =
  choice
    [ parens exprP,
      try do name <- ident; args <- parens (exprP `sepBy` symbol ","); pure (Apply name args),
      Var <$> ident,
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
        [ Skip <$ symbol "skip",
          if_,
          for_,
          do symbol "while"; cond <- exprP; symbol "do"; body <- stmP; symbol "od"; pure (While cond body),
          do symbol "repeat"; body <- stmP; symbol "until"; cond <- exprP; pure (Repeat body cond),
          do symbol "return"; mexpr <- optional exprP; pure (Return mexpr),
          try do f <- ident; args <- parens (exprP `sepBy` symbol ","); pure (Call f args),
          do var <- ident; symbol ":="; expr <- exprP; pure (var := expr)
        ]

    if_ = do
      symbol "if"
      cond <- exprP
      symbol "then"
      then_ <- stmP
      elifs <- many do symbol "elif"; cond <- exprP; symbol "then"; body <- stmP; pure (cond, body)
      else_ <- optional (symbol "else" *> stmP)
      symbol "fi"
      pure (If cond then_ elifs else_)

    for_ = do
      symbol "for"
      init <- stmP
      symbol ","
      cond <- exprP
      symbol ","
      step <- stmP
      symbol "do"
      body <- stmP
      symbol "od"
      pure (For init cond step body)

defP :: Parser Definition
defP = do
  name <- symbol "fun" *> ident
  args <- parens (ident `sepBy` symbol ",")
  locals <- option [] do symbol "local" *> (ident `sepBy` symbol ",")
  body <- between (symbol "{") (symbol "}") stmP
  pure Definition {name, args, locals, body}

unitP :: Parser Unit
unitP = do
  sc
  defs <- many defP
  body <- stmP
  eof
  pure Unit {body, defs}

data ParserError = ParserError String
  deriving stock (Show)

instance Exception ParserError where
  displayException (ParserError s) = s

parseSource :: FilePath -> String -> Either ParserError Unit
parseSource path src =
  case runParser unitP path src of
    Right stm -> Right stm
    Left ex -> Left (ParserError (errorBundlePretty ex))

parseFile :: FilePath -> IO Unit
parseFile path = do
  src <- readFile path
  case parseSource path src of
    Right stm -> pure stm
    Left ex -> throwIO ex
