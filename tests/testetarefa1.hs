import qualified Data.Text as T
import Data.List
import System.Directory

-- * Funções do mooshak

-- | Parte uma @String@ numa lista de linhas
inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

-- | Junta uma lista de linhas numa @String@
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

-- * Funções de teste

-- | Corre múltiplos testes para as três tarefas
correTestes :: IO ()
correTestes = do
    files <- getDirectoryContents "../tests"
    let inputsT1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files
    let inputsT2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files
    let inputsT3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files
    mapM_ correTeste inputs

-- | Corre um teste para uma tarefa
correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    -- nome do ficheiro
    let nome = reverse $ drop 3 $ reverse input
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let o = outStr (tarefa (inStr inp))
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp)   -- compara resultados
    then putStrLn "OK"
    else do
        putStrLn "FALHOU"
        putStr esp
        putStrLn o

-- * Funções tarefas

tarefa1 :: [String] -> [String]
tarefa1 = 

    
--tarefa2 :: [String] -> [String]
--tarefa2 = undefined -- TODO: substituir pelo vosso código!

--tarefa3 :: [String] -> [String]
--tarefa3 = undefined -- TODO: substituir pelo vosso código!
