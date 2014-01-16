module FP.Parser (parseFP) where

import           FP.AST
import           FP.Value

import           Control.Applicative  hiding (many, (<|>)) -- conflicts with Parsec
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle
                               { P.identStart = letter <|> char '+' <|> char '-' <|> char '*' <|> char '/',
                                 P.reservedNames = ["T", "F", "bu", "while", "if", "then", "else"],
                                 P.reservedOpNames = ["@", ".", "/", "\\", "_"]
                               }
                          )

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
lexeme     = P.lexeme lexer
angles     = P.angles lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
colon      = P.colon lexer
semi       = P.semi lexer
commaSep1  = P.commaSep1 lexer

boolAtom :: Parser Atom
boolAtom = (reserved "T" *> return (BoolAtom True))
       <|> (reserved "F" *> return (BoolAtom False))

numberAtom :: Parser Atom
numberAtom = NumberAtom <$> natural

symbolAtom :: Parser Atom
symbolAtom = SymbolAtom <$> many1 upper

atomObject :: Parser Object
atomObject = AtomObject <$> (numberAtom <|> boolAtom <|> symbolAtom)

sequenceObject :: Parser Object
sequenceObject = SequenceObject <$> angles (commaSep1 (atomObject <|> sequenceObject))

object :: Parser Object
object = atomObject <|> sequenceObject

functionList :: Parser [Function]
functionList = brackets (commaSep1 function)

-- FIXME: Use "p->f;g" for Condition instead of "if p then f else g"
-- It is not easy to parse "p->f;g" because this grammar introduces left recursion.
-- FIXME: handle s/sr functions. e.g., 1:x, 2r:x
term =  parens function
    <|> Function <$> identifier
    <|> Construction <$> functionList
    <|> Condition <$> (reserved "if" *> function) <*> (reserved "then" *> function) <*> (reserved "else" *> function)
    <|> Constant <$> (lexeme (char '_') *> object)
    <|> BinaryToUnary <$> (reserved "bu" *> function) <*> object
    <|> While <$> (reserved "while" *> function) <*> function

table = [ [prefix "@" ApplyToAll, prefix "\\" Insert ],
          [binary "." Composition AssocRight]
        ]
        where prefix  name fun = Prefix  (do { reservedOp name; return fun })
              binary  name fun = Infix   (do { reservedOp name; return fun })

function :: Parser Function
function = buildExpressionParser table term

definition :: Parser Definition
definition = Definition <$> (symbol "Def" *> identifier) <*> (lexeme (char '=') *> function)

expression :: Parser Expression
expression = try (Object <$> object)
         <|> Application <$> function <*> (colon *> object)

program :: Parser Program
-- Explicit whiteSpace is needed to skip any leading white space.
program = Program <$> (whiteSpace *> defs) <*> expression
    where defs = many (try definition)

parseFP :: String -> Either ParseError Program
parseFP = parse program ""
