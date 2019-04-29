import System.IO
import System.Environment
import Parser

interpretLines :: [String] -> IO()
interpretLines [] = return ()
interpretLines (x:xs) =  do let a = epureEval(eval(parser(tokenize(myParser(epureString(x))))))
                            putStrLn (show (a))
                            interpretLines xs

getLinesFiles :: [String] -> IO ()
getLinesFiles [] = return ()
getLinesFiles (x:xs) =  do
                           content <- readFile x
                           let linesOfFiles = lines content
                           interpretLines linesOfFiles
                           getLinesFiles xs

checkInteractive :: [String] -> Bool
checkInteractive [] = False
checkInteractive (x:xs) =
                          if x == "-i"
                              then True
                              else checkInteractive xs

interactive :: IO ()
interactive = do
                putStr "> "
                hFlush stdout
                name <- getLine 
                let a = epureEval(eval(parser(tokenize(myParser(epureString(name))))))
                putStrLn (show (a))
                interactive

removeDashI :: [String] -> IO [String]
removeDashI (y) = return (filter (\x -> x /= "-i") y)

main :: IO ()
main = do
    args <- getArgs
    let check = checkInteractive args
    newArgs <- removeDashI args
    getLinesFiles newArgs
    if check
        then interactive
        else putStr ""