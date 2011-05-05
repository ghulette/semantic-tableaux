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
  putStrLn (drawTableau t)
