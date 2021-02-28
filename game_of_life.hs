import Data.Array
import System.Environment

dim_x=70
dim_y=40

draw_board board = putStr board_str where
	board_str = concat [(board ! i) ++ (if i `mod` dim_x /= (dim_x-1) then "" else "\n") | i <- [0..dim_x*dim_y-1]]

step_board board = board//[(n, new_value n) | n <- [0..dim_x*dim_y-1]] where
  	new_value index =   if (board!index == "O") then
							if ((neighbors index == 3) || (neighbors index == 2)) then "O"
							else " "
						else
							if (neighbors index == 3) then "O"
							else " "
  	neighbors index = sum [1 | x <- (filter (>=0) $ filter (<(dim_x*dim_y)) $ map ($index) indexes), board!x == "O"]
	indexes = map (+) [-dim_x-1,-dim_x,-dim_x+1,-1,1,dim_x-1,dim_x,1+dim_x]

step_sim board ttl = do
	draw_board board
	putStr "\x1b[H"
	if ttl == 0 then
		putStrLn "Done !"
	else
		step_sim (step_board board) (ttl-1)

glider pos = map (+pos) [3,3+dim_x,3+2*dim_x,2+2*dim_x,1+dim_x]
blinker pos = map (+pos) [1,1+dim_x,1+dim_x*2]

main :: IO ()
main = do
	duration:rest <- getArgs
	let empty = array (0,(dim_x*dim_y-1)) [(i," ") | i <- [0..dim_x*dim_y-1]]
	let start = empty//[(i,"O") | i <- glider 0 ++ blinker 30 ++ glider 80]
	step_sim start $ read duration
