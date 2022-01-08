{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Functor
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text
import           Data.Text.IO            hiding ( putStr
                                                , putStrLn
                                                )
import           Data.Void
import           Prelude                 hiding ( getLine )
import           Text.Megaparsec                ( (<|>)
                                                , MonadParsec(eof, try)
                                                , ParseErrorBundle
                                                , Parsec
                                                , between
                                                , choice
                                                , many
                                                , parse
                                                , sepEndBy
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , lowerChar
                                                , space1
                                                )
import qualified Text.Megaparsec.Char.Lexer    as Lexer

{- 
- Números
- Booleanos
- Listas
- Definição de variáveis
- If/Not
- Lambdas
- Invocar funções
-}

-- > (+ 1 2)
-- 3

data Op
  = Add
  | Mul
  | Def
  deriving (Show, Enum, Bounded)

toConcreteSyntax :: Op -> Text
toConcreteSyntax = \case
  Add -> "+"
  Mul -> "*"
  Def -> "def"

opList :: [Op]
opList = [minBound ..]


data SExp
  = Number Int
  | Boolean Bool
  | Identifier Text
  | List [SExp]
  | Operator Op
  deriving Show

-- > (+ 1 2)
-- Parsing
myList = List [Identifier "+", Number 1, Number 2]
-- Evaluation
result = [Number 3]

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "[-" "-]"

whiteSpace :: Parser ()
whiteSpace = Lexer.space space1 lineComment blockComment

symbol :: Text -> Parser Text
symbol = Lexer.symbol whiteSpace

parseNumber :: Parser Int
parseNumber = Lexer.signed (pure ()) Lexer.decimal

-- #f; #t
-- (if #t 5 6)
parseBool :: Parser Bool
parseBool = char '#' >> ((True <$ char 't') <|> (False <$ char 'f'))

-- começa com letra minúscula
-- pode conter letras minúsculas, maiúsculas, _, - e números
parseIdentifier :: Parser Text
parseIdentifier = do
  firstChar <- lowerChar
  rest      <- many parseIdentifier'
  return . pack $ (firstChar : rest)

 where
  parseIdentifier' :: Parser Char
  parseIdentifier' = char '-' <|> char '_' <|> alphaNumChar

parseOp :: Parser Op
parseOp = choice (toChoice <$> opList)
  where toChoice op = op <$ symbol (toConcreteSyntax op)

-- (1 2 3 4 5)
parseList :: Parser [SExp]
parseList = between (symbol "(") (symbol ")") (parseSExp `sepEndBy` whiteSpace)

parseSExp :: Parser SExp
parseSExp =
  choice
    . (try <$>)
    $ [ Number <$> parseNumber
      , Operator <$> parseOp
      , Boolean <$> parseBool
      , List <$> parseList
      , Identifier <$> parseIdentifier
      ]

parseLine = parseSExp <* eof



parseEmpty :: Text -> Either (ParseErrorBundle Text Void) SExp
parseEmpty = parse parseSExp mempty

type VarTable = Map Text SExp

data Error
  = UndefinedVariable Text
  | NotDefinedYet
  | TypeMismatch
  deriving (Show, Eq)

type MinhaMonadinha = StateT VarTable (ExceptT Error IO) SExp

eval :: SExp -> MinhaMonadinha
eval = \case
  Number     n  -> return $ Number n
  Boolean    b  -> return $ Boolean b
  Operator   op -> return $ Operator op
  Identifier id -> gets (Map.lookup id) >>= \case
    Nothing -> throwError $ UndefinedVariable id
    Just x  -> return x

  List elements -> apply elements

apply :: [SExp] -> MinhaMonadinha
apply = \case
  (Operator Add : rest) ->
    traverse eval rest >>= lift . toNumbers <&> Number . sum
  (Operator Mul : rest) ->
    traverse eval rest >>= lift . toNumbers <&> Number . product
  [Operator Def, Identifier name, value] -> do
    evaluated <- eval value
    modify (Map.insert name evaluated)
    return $ Identifier name
  _ -> throwError NotDefinedYet

toNumbers :: [SExp] -> ExceptT Error IO [Int]
toNumbers []              = return []
toNumbers (Number x : xs) = toNumbers xs <&> (x :)
toNumbers _               = throwError TypeMismatch

evalEmpty sexp = runStateT (eval sexp) Map.empty


repl :: StateT VarTable IO ()
repl = do
  liftIO (putStr "> ")
  input <- liftIO getLine
  case parse parseLine mempty input of
    Right value -> do
      initialState <- get
      result       <- mapStateT
        (\myValue -> do
          runExceptT myValue >>= \case
            Left  error           -> return (Left error, initialState)
            Right (result, state) -> return (Right result, state)
        )
        (eval value)
      case result of
        Left er -> liftIO . putStrLn $ "Error: " <> show er
        Right se -> liftIO . putStrLn $ "Yayy: " <> show se
    Left error -> void (liftIO (print error))

-- (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b 
-- (ExceptT Error IO (SExp)) -> IO (Either a b, SExp)
-- VarTable (ExceptT Error IO) SExp
runRepl :: IO ()
runRepl = evalStateT (forever repl) Map.empty
