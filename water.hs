import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, SurfaceFlag(SWSurface), surfaceGetPixelFormat, surfaceGetHeight, surfaceGetWidth, PixelFormat)
import Graphics.UI.SDL.Video (setVideoMode, mapRGB, fillRect)
import Graphics.UI.SDL.Color (Pixel)
import qualified Graphics.UI.SDL.Video as V (flip)

import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit), waitEvent)

import Graphics.UI.SDL.Rect (Rect(Rect))

import Control.Monad.Trans.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)

import Prelude hiding ((.))
import Control.Category ((.))

import Data.Lens.Common (lens)
import Data.Lens.Lazy ((~=), access, (%=))

tile_width = 16
tile_height = 16

x_tiles = 24
y_tiles = 16

data Position = Position Int Int

getX (Position x _) = x
setX x (Position _ y) = Position x y

x = lens getX setX

getY (Position _ y) = y
setY y (Position x _) = Position x y

y = lens getY setY

data Player = Player Position

getPosition (Player pos) = pos
setPosition pos (Player _) = Player pos

position = lens getPosition setPosition

data GameState = GameState Surface PixelFormat Player
initial_game_state = GameState undefined undefined (Player (Position 1 1))

getScreen (GameState screen _ _) = screen
setScreen screen (GameState _ format player) = GameState screen format player

screen = lens getScreen setScreen

getFormat (GameState _ format _) = format
setFormat format (GameState screen _ player) = GameState screen format player

format = lens getFormat setFormat

getPlayer (GameState _ _ player) = player
setPlayer player (GameState screen format _) = GameState screen format player

player = lens getPlayer setPlayer

sand_color :: StateT GameState IO Pixel
sand_color = do
	f <- access format
	lift $ mapRGB f 255 169 95

setup :: StateT GameState IO ()
setup = do
	s <- lift $ setVideoMode (x_tiles * tile_width) (y_tiles * tile_height) 32 [SWSurface]
	screen ~= s
	format ~= surfaceGetPixelFormat s
	color <- sand_color
	lift $ fillRect s (Just (Rect 0 0 (surfaceGetWidth s) (surfaceGetHeight s))) color
	lift $ V.flip s
	return ()

handle_event :: Event -> StateT GameState IO ()
handle_event Quit = return ()
handle_event _ = main_loop

main_loop :: StateT GameState IO ()
main_loop = do
	-- Normally we'd do stuff here...
	event <- lift waitEvent
	handle_event event

main = withInit [InitVideo, InitEventthread] $ (>>) (setCaption "Water Game" "") $ (flip runStateT) initial_game_state (setup >> main_loop)
