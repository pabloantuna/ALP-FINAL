{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseGrm Grm
%name parseOp Op

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '&'            { TInit }                  -- el sigma de la def de gramatica, el simbolo inicial (siempre va a ser & el simbolo inicial en nuestro programa, no podemos elegirlo a mano)
    '->'           { TArrow }                 -- la flechita para la regla de produccion
    '='            { TDef }                   -- la asignacion de una gramatica a un nombre
    '|'            { TOr }                    -- el or de las reglas de gramatica (BNF)
    '\\'           { TLambda }                -- el lambda, o sea la cadena vacia
    ';'            { TEnd }                   -- simplemente un punto y coma para terminar la regla de produccion (estamos siguiendo la sintaxis del pdf de la catedra)
    '?'            { TIn }                    -- para consultar si una cadena pertenece a un lenguaje (cadena *in* lenguaje, por eso In)
    '=='           { TEqual }                 -- para consultar la equivalencia de dos gramaticas
    '+'            { TUnion }                 -- para la union de dos gramaticas
    '.'            { TIntersec }              -- para la interseccion de dos gramaticas (no hay motivo de por que un punto simplemente no soy bueno eligiendo simbolos)
    '-'            { TDiff }                  -- para la resta de dos gramaticas (algunos simbolos si tienen motivo de eleccion claramente je)
    '++'           { TConcat }                -- para la concatenacion de dos gramaticas (simplemente es este simbolo por la concatenacion de listas en haskell)
    '~'            { TComplement }            -- para el complemento de una gramatica (casi seguro que ~ es literalmente el simbolo de complemento de conjunto en algun lado)
    '~~'           { TReverse }               -- para hacer el reverso de una gramatica (aca de nuevo me quede sin ideas de simbolos)
    '!'            { TSide }                  -- para pasar de derecha (izquierda) a izquierda (derecha) (si estas leyendo todos los comentarios te daras una idea de que aca nuevamente me quede sin ideas de simbolos je)
    T              { TT $$}                   -- los simbolos terminales
    NT             { TNT $$}                  -- los simbolos no terminales


%%

LGrammar  :
          |

RGrammar  :
          |

Grammar   : LGrammar                           {  }
          | RGrammar                           {  }

Op    : NT '=' Grammar                         { OpDef $1 $3}
      | Grammar '==' Grammar                   { OpEqual $1 $3}
      | T '?' Grammar                          { OpIn $1 $3}
     
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
               | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c  -> lexer cont cs
                          | isAlphaNum c -> lexNT (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('&':cs) -> cont TInit cs
                    ('-':('>':cs)) -> cont TArrow cs
                    ('=':cs) -> cont TDef cs
                    ('|':cs) -> cont TOr cs
                    ('\\':cs)-> cont TLambda cs
                    (';':cs) -> cont TEnd cs
                    ('?':cs) -> cont TIn cs
                    ('==':cs) -> cont TEqual cs
                    ('+':cs) -> cont TUnion cs
                    ('.':cs) -> cont TIntersec cs
                    ('-':cs) -> cont TDiff cs
                    ('+':('+':cs)) -> cont TConcat cs
                    ('~':cs) -> cont TComplement cs
                    ('~':('~':cs)) -> cont TReverse cs
                    ('!':cs) -> cont TSide cs
                    ('"':cs) -> lexT cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexT cs = case span (/= '"') cs of -- anteriormente era con isAlphaNum pero no tuve en cuenta espacios xd
                              (t, '"':rest) -> cont (T t) rest -- tal vez sacar espacios depende como lo termine manejando pero no me hago problema porque seguro termina siendo agregar un filter o algo por el estilo
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
                                           
grm_parse s = parseGrm s 1
op_parse s = parseOp s 1
}
