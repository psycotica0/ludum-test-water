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

data GameState = GameState Surface PixelFormat
initial_game_state = GameState undefined undefined

getScreen (GameState screen _) = screen
setScreen screen (GameState _ format) = GameState screen format

screen = lens getScreen setScreen

getFormat (GameState _ format) = format
setFormat format (GameState screen _) = GameState screen format

format = lens getFormat setFormat

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
