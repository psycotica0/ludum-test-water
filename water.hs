import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, SurfaceFlag(SWSurface), surfaceGetPixelFormat, surfaceGetHeight, surfaceGetWidth)
import Graphics.UI.SDL.Video (setVideoMode, mapRGB, fillRect)
import qualified Graphics.UI.SDL.Video as V (flip)

import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit), waitEvent)

import Graphics.UI.SDL.Rect (Rect(Rect))

tile_width = 16
tile_height = 16

x_tiles = 24
y_tiles = 16

sand_color surface = mapRGB (surfaceGetPixelFormat surface) 255 169 95

setup _ = do
	screen <- setVideoMode (x_tiles * tile_width) (y_tiles * tile_height) 32 [SWSurface]
	color <- sand_color screen
	fillRect screen (Just (Rect 0 0 (surfaceGetWidth screen) (surfaceGetHeight screen))) color
	V.flip screen
	return ()

handle_event Quit = return ()
handle_event _ = main_loop ()

main_loop _ = do
	-- Normally we'd do stuff here...
	waitEvent >>= handle_event

main = withInit [InitVideo, InitEventthread] $ setCaption "Water Game" "" >>= setup >>= main_loop
