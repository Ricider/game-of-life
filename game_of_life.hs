import Data.Array
import Control.Monad
import System.Environment

dim=50

draw_board board = putStr board_str where
	board_str = concat [(board ! i) ++ (if i `mod` dim /= (dim-1) then "" else "\n") | i <- [0..dim*dim-1]]

step_board board = board//[(n, new_value n) | n <- [0..dim*dim-1]] where
  	new_value index =   if (board!index == "O") then
							if ((neighbors index == 3) || (neighbors index == 2)) then "O"
							else "."
						else
							if (neighbors index == 3) then "O"
							else "."
  	neighbors index = sum [1 | x <- (filter (>=0) $ filter (<(dim*dim)) $ map ($index) indexes), board!x == "O"]
	indexes = [(+(-dim-1)),(+(-dim)),(+(-dim+1)),(+(-1)),(+1),(+(dim-1)),(+dim),(+(1+dim))]

step_sim board ttl = do
	draw_board board
	putStrLn ""
	if ttl == 0 then
		putStrLn "Done !"
	else
		step_sim (step_board board) (ttl-1)

main :: IO ()
main = do
	duration:rest <- getArgs
	let empty = array (0,(dim*dim-1)) [(i,".") | i <- [0..dim*dim-1]]
	let start = empty//[(i,"O") | i <- [3,53,103,102,51]]
	step_sim start $ read duration
 