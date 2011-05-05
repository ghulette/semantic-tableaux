import Expr
import Parser
import Tableau

parse :: String -> IO Expr
parse s = 
  case parseFormula s of
    Left err -> error (show err)
    Right e -> return e

main :: IO ()
main = do
  input <- getContents
  p <- parse input
  let t = tableau p
  let msg = if satisfiable t then " is "
                             else " is not "
  putStrLn $ (show p) ++ msg ++ "satisfiable\n"
  putStrLn (draw t)
