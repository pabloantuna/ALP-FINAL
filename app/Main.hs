module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )

import           Common
import           PPrint
import           Parse
import           Eval
import           Grammar

---------------------
--- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True [])

iname, iprompt :: String
iname = "Gramaticas Regulares"
iprompt = "GR> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool
  ,       -- True, si estamos en modo interactivo.
    env    :: Env
  }

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter env) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state { inter = True }

data Command = Compile String String
              | LPrint String
              | RPrint String
              | Browse
              | Quit
              | Help
              | Noop
              | Interactive String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, rest') = break isSpace x
    let rest         = dropWhile isSpace rest'
    let (t, tt')     = break isSpace rest
    let tt           = dropWhile isSpace tt'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t tt)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop
  else return (Interactive x)

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter env) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    putStr (unlines [ s | s <- reverse (nub (map fst env)) ])
    return (Just state)
  Compile n c -> do
    state' <- compileFile state c n
    return (Just state')
  LPrint s ->
    let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
    in  return (Just state)
  RPrint s ->
    let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
    in  return (Just state)
  Interactive s -> do
    state' <- compilePhrase state s
    return $ Just state'

data InteractiveCommand = Cmd [String] String (String -> String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const (const Browse)) "Ver los nombres en scope"
  , Cmd [":load"] "<name> <file>" (Compile) "Cargar una gramática desde un archivo y ponerle de nombre <name>"
  , Cmd [":lprint"] "<grm>" (const LPrint) "Imprime una gramática como gramática izquierda"
  , Cmd [":rprint"] "<grm>" (const RPrint) "Imprime una gramática como gramática derecha"
  , Cmd [":quit"]       ""       (const (const Quit)) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const (const Help)) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFile :: State -> String -> String -> InputT IO State
compileFile state@(S inter v) f name = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  grm <- do g <- parseIO f' (grm_parse) x
            return (maybe Nothing (\x -> Just x) g)
  lift $ putStrLn $ show $ pipo grm
  maybe (return state) (addGrm state name) grm

-- pipo :: GrmR -> String
pipo gm = case gm of
            Nothing -> "no amigo mal ahi"
            Just x -> show x

addGrm :: State -> String -> GrmR -> InputT IO State
addGrm state@(S inter env) name grm = 
  do 
    let grm' = grmRToAEFD grm
     in return (S inter (replace name grm' env))

replace :: String -> AEFDG -> Env -> Env
replace name gram [] = [(name, gram)]
replace name gram ((n,g):xs) = if name==n then ((n,gram):xs)
                                          else ((n,g):(replace name gram xs))

compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" op_parse x
  maybe (return state) (handleStmt state) x'

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleStmt :: State -> Op -> InputT IO State
handleStmt state stmt = lift $ do
  case stmt of
    OpDef n g -> addDef n g
    OpIn s g -> putStrLn "tuturu" >> return state
    OpEqual g1 g2 -> putStrLn "no hay tuturu" >> return state
 where
  addDef name grm = return state

it :: String
it = "it"


