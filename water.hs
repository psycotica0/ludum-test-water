module Main where
import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, SurfaceFlag(SWSurface), surfaceGetPixelFormat, surfaceGetHeight, surfaceGetWidth, PixelFormat)
import Graphics.UI.SDL.Video (setVideoMode, mapRGB, fillRect)
import Graphics.UI.SDL.Color (Pixel)
import qualified Graphics.UI.SDL.Video as V (flip)

import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit, KeyDown), waitEvent)
import Graphics.UI.SDL.Keysym (Keysym(Keysym), SDLKey(SDLK_UP, SDLK_DOWN, SDLK_LEFT,SDLK_RIGHT, SDLK_SPACE))

import Graphics.UI.SDL.Rect (Rect(Rect))

import Control.Monad.Trans.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)

import Prelude hiding ((.))
import Control.Category ((.))

import Data.Lens.Common (Lens, lens, getL, modL, setL)
import Data.Lens.Lazy ((~=), access, (%=))

import Data.Function (on)

import Data.Ord.HT (limit)
import Data.Bool.HT (select, if')

import GHC.Word (Word8)

import System.Random (randomR, mkStdGen, StdGen)

import Game.Water.Datatypes

tile_width = 16
tile_height = 16

x_tiles = 24
y_tiles = 16

-- If your thirst gets to this level, you die
thirst_max = 10
-- This is how much thirst it costs to move
thirst_cost_move = 1
-- This is how much thirst it costs to dig (if you fail to find a well)
thirst_cost_dig = 5
-- This is the thirst cost between nodes that's considered fair
-- I've made it thirst_max / 2, so you should be able to almost get there, then back, without dying
thirst_cost_fair = thirst_max `div` (2 * thirst_cost_move)

-- This returns a rectangle representing the playing area, ignoring the status_bar
playing_area = Rect 0 (tile_height) (x_tiles * tile_width) (y_tiles * y_tiles)

-- This function is for binding a value into a lens
(~<-) :: (Monad m) => Lens a b -> StateT a m b -> StateT a m b
lens ~<- func = (lens ~=) =<< func

-- This function returns a rect that represents the tile referenced by pos
get_rect :: Position -> Rect
get_rect pos = Rect (tile_width * ((getL x pos) - 1)) (tile_height * (getL y pos)) tile_width tile_height

-- This function insets a rectangle, given a denominator to use as a fraction
inset_rect :: Rect -> Int -> Rect
inset_rect (Rect cx cy cw ch) fraction = Rect (cx + border_width) (cy + border_height) (cw - (2 * border_width)) (ch - (2 * border_height))
	where
	border_width = cw `div` fraction
	border_height = ch `div` fraction

-- This function returns the manhattan distance between two points
distance :: Position -> Position -> Int
distance pos1 pos2 = (on (\x y -> abs (x-y)) (getL x) pos1 pos2) + (on (\x y -> abs (x-y)) (getL y) pos1 pos2)

-- This returns True if the player is on a found well, or False otherwise
standing_on_well :: StateT GameState IO Bool
standing_on_well = do
	pos <- access (position.player)
	wells <- access (wells)
	return $ or $ map (\well -> (getL found well) && ((getL well_position well) == pos)) wells

-- This function returns whether one can get from anywhere in the set to the end node
-- The rules for "reachability" is that there's some chain where no two nodes are over thirst_cost_fair apart.
-- This should hopefully yield a "fair" path.
-- This is a stable state function, so assume that every item in chain is reachable from start
pos_reachable :: Position -> ([Position] -> Bool)
pos_reachable end = or . map (\i -> (distance i end < thirst_cost_fair))

-- This function generates new random, unique, reachable positions until the end is reachable
build_well_positions :: StdGen -> Position -> [Position] -> [Position]
build_well_positions gen end cur_positions = if' (pos_reachable end cur_positions) cur_positions $ build_well_positions gen' end next_pos_set
	where
	next_pos_set = if' ((pos_reachable new_pos cur_positions) && (not $ elem new_pos cur_positions)) (new_pos:cur_positions) cur_positions
	(new_pos, gen') = randomR (Position 1 1, Position x_tiles y_tiles) gen

build_wells :: StdGen -> Well -> Well -> [Well]
build_wells gen start end = map (\pos -> Well (visible pos) pos) (end_pos:positions)
	where
	start_pos = getL well_position start
	end_pos = getL well_position end
	positions = build_well_positions gen end_pos [start_pos]
	visible pos = if' (pos == start_pos || pos == end_pos) True False

-- Important Wells
starting_well = Well True (Position 1 1)
ending_well = Well True (Position x_tiles y_tiles)

-- This sets up the initial stuff
setup :: StateT GameState IO ()
setup = do
	s <- lift $ setVideoMode (x_tiles * tile_width) ((y_tiles + 1) * tile_height) 32 [SWSurface]
	screen ~= s
	format ~= surfaceGetPixelFormat s
	f <- access format
	(sand.colors) ~<- (lift $ mapRGB f 255 169 95)
	(player_color.colors) ~<- (lift $ mapRGB f 0 0 0)
	(water.colors) ~<- (lift $ mapRGB f 0 0 255)
	(position.player) ~= getL (well_position) starting_well
	-- Set the starting and the ending well
	wells ~= build_wells (mkStdGen 1) starting_well ending_well
	return ()

consume_move_thirst = (thirst.player) %= (+thirst_cost_move)

-- These function move the player in the given axis, and consume the proper amount of thirst
move_y v = ((y.position.player) %= (limit (1, y_tiles)).(+v)) >> consume_move_thirst
move_x v = ((x.position.player) %= (limit (1, x_tiles)).(+v)) >> consume_move_thirst

-- This is the event handler...
handle_event :: Event -> StateT GameState IO Bool
handle_event Quit = return True
handle_event (KeyDown (Keysym SDLK_UP _ _)) = (move_y (-1)) >> (return False)
handle_event (KeyDown (Keysym SDLK_DOWN _ _)) = (move_y 1) >> (return False)
handle_event (KeyDown (Keysym SDLK_LEFT _ _)) = (move_x (-1)) >> (return False)
handle_event (KeyDown (Keysym SDLK_RIGHT _ _)) = (move_x 1) >> (return False)
handle_event (KeyDown (Keysym SDLK_SPACE _ _)) = do
	pos <- access (position.player)
	wells %= (map (\well -> if' ((getL well_position well) == pos) (setL found True well) well))
	on_well <- standing_on_well
	-- If we're currently on a well, do nothing, otherwise we just dug unsucessfully and should be docked thirst
	(thirst.player) %= (if' on_well id (+thirst_cost_dig))
	return False
handle_event _ = return False

-- This function takes a distance and makes a color from it
distance_color :: Int -> StateT GameState IO Pixel
distance_color v = access format >>= (\x -> lift $ mapRGB x 0 0 $ select 0 [
	(v == 0, 255),
	(v < 3, 180),
	(v < 5, 100)
	])

-- This function returns the color the Distance Swatch should be. 
-- It does this by finding the distance to the closest well, then turning that into a color
distance_swatch_color :: StateT GameState IO Pixel
distance_swatch_color = do
	p <- access (position.player)
	wells <- access (wells)
	distance_color $ minimum $ map ((distance p).(getL well_position)) wells

-- This function returns the intensity given a thirst
thirst_color :: Int -> Word8
thirst_color raw_input = 255 - (input * step)
	where
	step = 255 `div` (fromIntegral thirst_max)
	input = fromIntegral $ limit (0,thirst_max) raw_input

-- This function returns the color of the thirst swatch
thirst_swatch_color :: StateT GameState IO Pixel
thirst_swatch_color = do
	f <- access format
	trst <- access (thirst.player)
	lift $ (\c -> mapRGB f c c c) $ thirst_color trst

-- This draws a swatch in the given position (1 being first)
draw_swatch :: Int -> Pixel -> StateT GameState IO Bool
draw_swatch index color = do
	s <- access screen
	lift $ fillRect s (Just $ inset_rect (get_rect $ Position index 0) 4) color

-- This function takes care of the rendering
render :: StateT GameState IO ()
render = do
	s <- access screen
	bg_color <- access (sand.colors)
	p_color <- access (player_color.colors)
	w_color <- access (water.colors)
	lift $ fillRect s (Just playing_area) bg_color
	-- Now we draw all of the visible wells
	wells <- access (wells)
	lift $ mapM (\p -> fillRect s (Just (get_rect p)) w_color) $ map (getL (well_position)) $ filter (getL found) wells
	-- Now we draw the player
	pos <- access (position.player)
	lift $ fillRect s (Just (get_rect pos)) p_color
	-- Now do the distance swatch
	(draw_swatch 1) =<< distance_swatch_color
	-- And the thirst swatch
	(draw_swatch 2) =<< thirst_swatch_color
	lift $ V.flip s

-- This is the game_over loop
-- Right now it paints the screen red, then waits to quit
game_over = do
	s <- access screen
	f <- access format
	over_color <- lift $ mapRGB f 255 0 0
	lift $ fillRect s (Just playing_area) over_color
	lift $ V.flip s
	lift wait_to_quit

-- This is the You Win screen
-- Right now it paints the screen white, then waits to quit
win = do
	s <- access screen
	f <- access format
	over_color <- lift $ mapRGB f 255 255 255
	lift $ fillRect s (Just playing_area) over_color
	lift $ V.flip s
	lift wait_to_quit

wait_to_quit :: IO ()
wait_to_quit = waitEvent >>= is_quit
	where
	is_quit Quit = return ()
	is_quit _ = wait_to_quit

-- This is the main loop
main_loop :: StateT GameState IO ()
main_loop = do
	on_well <- standing_on_well
	-- If we're on a well, reset the thirst. Otherwise leave it
	(thirst.player) %= if' on_well (const 0) id
	render
	event <- lift waitEvent
	quit <- handle_event event
	trst <- access (thirst.player)
	p_pos <- access (position.player)
	-- If the user quit, return. If thirst is over max, game_over, otherwise loop
	select main_loop [(quit, return ()), (trst > thirst_max, game_over), ((getL well_position ending_well) == p_pos, win)]

main = withInit [InitVideo, InitEventthread] $ (>>) (setCaption "Water Game" "") $ (flip runStateT) initial_game_state (setup >> main_loop)
