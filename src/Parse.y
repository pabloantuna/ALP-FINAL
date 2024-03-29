{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseGram Gram
%name parseOp Op

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '&'            { TInit }
    '->'           { TArrow }
    '='            { TDef }
    '|'            { TOr }
    '\\'           { TLambda }
    ';'            { TEnd }
    '?'            { TIn }
    '=='           { TEqual }
    '+'            { TUnion }
    '.'            { TIntersec }
    '-'            { TDiff }
    '++'           { TConcat }
    '~'            { TComplement }
    '~~'           { TReverse }
    '!'            { TSide }
    T              { TT $$ }
    NT             { TNT $$ }
    '('            { TOpen }
    ')'            { TClose }

%left '=='
%nonassoc '?'
%nonassoc '='
%left '+' '-'
%left '.'
%left '++'
%nonassoc '~' '~~' '!'
%nonassoc '->'
%nonassoc ';'
%left '|'
%nonassoc '&'

%%

-- lado izquierdo de una regla de produccion puede tener el simbolo inicial el cual distinguimos del resto por obvias razones, o un simbolo No Terminal (NT)
LeftSide : '&'                                 { Initial }
         | NT                                  { NT $1 }

-- lado derecho de una regla de produccion para una gramatica izquierda
RightGIzq : NT T                               { RTNT $2 (NT $1) }
          | '&' T                              { RTNT $2 Initial }
          | T                                  { RT $1 }
          | '\\'                               { RL }

RightSideGIzq : RightGIzq                      { [$1] }
              | RightGIzq '|' RightSideGIzq    { $1 : $3 }

-- lado derecho de una regla de produccion para una gramatica derecha
RightGDer : T NT                               { RTNT $1 (NT $2)}
          | T '&'                              { RTNT $1 Initial }
          | T                                  { RT $1 }
          | '\\'                               { RL }

RightSideGDer : RightGDer                      { [$1] }
              | RightGDer '|' RightSideGDer    { $1 : $3 }

-- la regla en si para una gramatica izquierda
LeftG : LeftSide '->' RightSideGIzq            { Rule $1 $3 }

-- la regla en si para una gramatica derecha
RightG : LeftSide '->' RightSideGDer           { Rule $1 $3 }

-- la gramatica izquierda completa (todas sus reglas)
LGrammar  : LeftG ';'                          { [$1] }
          | LeftG ';' LGrammar                 { $1 : $3 }

-- la gramatica derecha completa (todas sus reglas)
RGrammar  : RightG ';'                         { [$1] }
          | RightG ';' RGrammar                { $1 : $3 }

-- la gramatica puede ser izquierda o derecha
Gram  : LGrammar                               { Left (Gram $1) }
      | RGrammar                               { Right (Gram $1) }

Grammar : NT                                   { OpGram $1 }
        | Grammar '+' Grammar                  { OpUnion $1 $3 }
        | Grammar '.' Grammar                  { OpIntersec $1 $3 }
        | Grammar '-' Grammar                  { OpDiff $1 $3 }
        | Grammar '++' Grammar                 { OpConcat $1 $3 }
        | Grammar '~'                          { OpComplement $1 }
        | Grammar '~~'                         { OpReverse $1 }
        | Grammar '!'                          { OpSide $1 }
        | '(' Grammar ')'                      { $2 }

Op    : NT '=' Grammar                         { OpDef $1 $3 }
      | Grammar '==' Grammar                   { OpEqual $1 $3 }
      | T '?' Grammar                          { OpIn $1 $3 }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TInit
           | TArrow
           | TDef
           | TOr
           | TLambda
           | TEnd
           | TIn
           | TEqual
           | TUnion
           | TIntersec
           | TDiff
           | TConcat
           | TComplement
           | TReverse
           | TSide
           | TT String
           | TNT String
           | TOpen
           | TClose
           | TEOF
  deriving Show

lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c  -> lexer cont cs
                          | isAlphaNum c -> lexNT (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('=':('=':cs)) -> cont TEqual cs
                    ('+':('+':cs)) -> cont TConcat cs
                    ('~':('~':cs)) -> cont TReverse cs
                    ('&':cs) -> cont TInit cs
                    ('=':cs) -> cont TDef cs
                    ('|':cs) -> cont TOr cs
                    ('\\':cs)-> cont TLambda cs
                    (';':cs) -> cont TEnd cs
                    ('?':cs) -> cont TIn cs
                    ('+':cs) -> cont TUnion cs
                    ('.':cs) -> cont TIntersec cs
                    ('-':cs) -> cont TDiff cs
                    ('~':cs) -> cont TComplement cs
                    ('!':cs) -> cont TSide cs
                    ('"':cs) -> lexT cs
                    ('(':cs) -> cont TOpen cs
                    (')':cs) -> cont TClose cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexT cs = case span (/= '"') cs of
                              (t, '"':rest) -> cont (TT t) rest
                              ([], _) -> \line -> Failed $ "Línea "++(show line)++": El terminal es vacio eso ta raro no?"
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs
                          lexNT cs = case span isAlphaNum cs of
                              (nt, rest) -> cont (TNT nt) rest
                                           
gram_parse s = parseGram s 1
op_parse s = parseOp s 1
}
