import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, SurfaceFlag(SWSurface), surfaceGetPixelFormat, surfaceGetHeight, surfaceGetWidth)
import Graphics.UI.SDL.Video (setVideoMode, mapRGB, fillRect)
import Graphics.UI.SDL.Color (Pixel)
import qualified Graphics.UI.SDL.Video as V (flip)

import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit), waitEvent)

import Graphics.UI.SDL.Rect (Rect(Rect))

import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Control.Monad.Trans.Class (lift)

tile_width = 16
tile_height = 16

x_tiles = 24
y_tiles = 16

sand_color :: StateT Surface IO Pixel
sand_color = do
	screen <- get
	lift $ mapRGB (surfaceGetPixelFormat screen) 255 169 95

setup :: StateT Surface IO ()
setup = do
	screen <- lift $ setVideoMode (x_tiles * tile_width) (y_tiles * tile_height) 32 [SWSurface]
	put screen
	color <- sand_color
	lift $ fillRect screen (Just (Rect 0 0 (surfaceGetWidth screen) (surfaceGetHeight screen))) color
	lift $ V.flip screen
	return ()

handle_event :: Event -> StateT Surface IO ()
handle_event Quit = return ()
handle_event _ = main_loop

main_loop :: StateT Surface IO ()
main_loop = do
	-- Normally we'd do stuff here...
	event <- lift waitEvent
	handle_event event

main = withInit [InitVideo, InitEventthread] $ (>>) (setCaption "Water Game" "") $ (flip runStateT) undefined (setup >> main_loop)
