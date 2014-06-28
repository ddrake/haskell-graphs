import Wgraph
import System.IO
import Control.Monad

main = do
  putStrLn "Give me some weighted edges like this: 1 2 3.4"
  lns <- myGetLines
  let g = fromLines lns
  putStrLn "Your graph is:"
  myPutLines . map show . edges $ g
  putStrLn "What is the start node?"
  ln <- getLine
  let start = read ln :: Node
  let wnodes = dijkstra g start
  putStrLn $ "I found " ++ (show $ length wnodes) ++ " weighted nodes!"
  myPutLines $ map show wnodes
  forever $ do
    putStrLn "Give me an end node, please."
    ln <- getLine
    let end = read ln :: Node
    putStrLn $ "Path: " ++ (show $ pathToNode wnodes end)
    putStrLn $ "Distance: " ++ (show $ distToNode wnodes end)

-- Get the lines up to the first blank line
myGetLines :: IO [String]
myGetLines = do
  l <- getLine
  if null l
    then return []
    else do
      ls <- myGetLines
      return (l:ls)

-- Write out a list of lines
myPutLines :: [String] -> IO ()
myPutLines [] = return ()
myPutLines (l:ls) = do
  putStrLn l
  myPutLines ls
