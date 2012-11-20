import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, SurfaceFlag(SWSurface), surfaceGetPixelFormat, surfaceGetHeight, surfaceGetWidth, PixelFormat)
import Graphics.UI.SDL.Video (setVideoMode, mapRGB, fillRect)
import Graphics.UI.SDL.Color (Pixel)
import qualified Graphics.UI.SDL.Video as V (flip)

import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit, KeyDown), waitEvent)
import Graphics.UI.SDL.Keysym (Keysym(Keysym), SDLKey(SDLK_UP, SDLK_DOWN, SDLK_LEFT,SDLK_RIGHT))

import Graphics.UI.SDL.Rect (Rect(Rect))

import Control.Monad.Trans.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)

import Prelude hiding ((.))
import Control.Category ((.))

import Data.Lens.Common (Lens, lens, getL, modL)
import Data.Lens.Lazy ((~=), access, (%=))

import Data.Function (on)

import Data.Ord.HT (limit)

import Game.Water.Datatypes

tile_width = 16
tile_height = 16

x_tiles = 24
y_tiles = 16

-- This function is for binding a value into a lens
(~<-) :: (Monad m) => Lens a b -> StateT a m b -> StateT a m b
lens ~<- func = (lens ~=) =<< func

-- This function returns a rect that represents the tile referenced by pos
get_rect :: Position -> Rect
get_rect pos = Rect (tile_width * ((getL x pos) - 1)) (tile_height * ((getL y pos) - 1)) tile_width tile_height

-- This function returns the manhattan distance between two points
distance :: Position -> Position -> Int
distance pos1 pos2 = (on (\x y -> abs (x-y)) (getL x) pos1 pos2) + (on (\x y -> abs (x-y)) (getL y) pos1 pos2)

-- This sets up the initial stuff
setup :: StateT GameState IO ()
setup = do
	s <- lift $ setVideoMode (x_tiles * tile_width) (y_tiles * tile_height) 32 [SWSurface]
	screen ~= s
	format ~= surfaceGetPixelFormat s
	f <- access format
	(sand.colors) ~<- (lift $ mapRGB f 255 169 95)
	(player_color.colors) ~<- (lift $ mapRGB f 0 0 0)
	(water.colors) ~<- (lift $ mapRGB f 0 0 255)
	wells ~= [Well True (Position 5 5)]
	return ()

x_plus v = (limit (1, x_tiles)).(+v)
y_plus v = (limit (1, y_tiles)).(+v)

-- This is the event handler...
handle_event :: Event -> StateT GameState IO ()
handle_event Quit = return ()
handle_event (KeyDown (Keysym SDLK_UP _ _)) = ((y.position.player) %= (y_plus (-1))) >> main_loop
handle_event (KeyDown (Keysym SDLK_DOWN _ _)) = ((y.position.player) %= (y_plus 1)) >> main_loop
handle_event (KeyDown (Keysym SDLK_LEFT _ _)) = ((x.position.player) %= (x_plus (-1))) >> main_loop
handle_event (KeyDown (Keysym SDLK_RIGHT _ _)) = ((x.position.player) %= (x_plus 1)) >> main_loop
handle_event _ = main_loop

-- This function takes care of the rendering
render :: StateT GameState IO ()
render = do
	s <- access screen
	bg_color <- access (sand.colors)
	p_color <- access (player_color.colors)
	w_color <- access (water.colors)
	lift $ fillRect s Nothing bg_color
	-- Now we draw all of the visible wells
	wells <- access (wells)
	lift $ mapM (\p -> fillRect s (Just (get_rect p)) w_color) $ map (getL (well_position)) $ filter (getL found) wells
	-- Now we draw the player
	pos <- access (position.player)
	lift $ fillRect s (Just (get_rect pos)) p_color
	lift $ V.flip s

-- This is the main loop
main_loop :: StateT GameState IO ()
main_loop = do
	render
	event <- lift waitEvent
	handle_event event

main = withInit [InitVideo, InitEventthread] $ (>>) (setCaption "Water Game" "") $ (flip runStateT) initial_game_state (setup >> main_loop)
