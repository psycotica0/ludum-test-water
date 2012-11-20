module Game.Water.Datatypes (Position(Position), x, y,
	Well(Well), well_position, found,
	Player(Player), position,
	Colors(Colors), sand, player_color, water,
	GameState(GameState), screen, format, player, colors, wells,
	initial_game_state,
	) where

import Data.Lens.Common (lens)
import Graphics.UI.SDL.Types (Surface, PixelFormat)
import Graphics.UI.SDL.Color (Pixel)

data Position = Position Int Int deriving Eq

getX (Position x _) = x
setX x (Position _ y) = Position x y

x = lens getX setX

getY (Position _ y) = y
setY y (Position x _) = Position x y

y = lens getY setY

data Well = Well Bool Position

getWellPosition (Well _ pos) = pos
setWellPosition pos (Well found _) = Well found pos

well_position = lens getWellPosition setWellPosition

getFound (Well found _) = found
setFound found (Well _ pos) = Well found pos

found = lens getFound setFound


data Player = Player Position

getPosition (Player pos) = pos
setPosition pos (Player _) = Player pos

position = lens getPosition setPosition

data Colors = Colors Pixel Pixel Pixel

getSand (Colors sand _ _) = sand
setSand sand (Colors _ player water) = Colors sand player water

sand = lens getSand setSand

getPlayerColor (Colors _ player _) = player
setPlayerColor player (Colors sand _ water) = Colors sand player water

player_color = lens getPlayerColor setPlayerColor

getWater (Colors _ _ water) = water
setWater water (Colors sand player _) = Colors sand player water

water = lens getWater setWater

data GameState = GameState Surface PixelFormat Player Colors [Well]
initial_game_state = GameState undefined undefined (Player (Position 1 1)) (Colors undefined undefined undefined) []

getScreen (GameState screen _ _ _ _) = screen
setScreen screen (GameState _ format player colors wells) = GameState screen format player colors wells

screen = lens getScreen setScreen

getFormat (GameState _ format _ _ _) = format
setFormat format (GameState screen _ player colors wells) = GameState screen format player colors wells

format = lens getFormat setFormat

getPlayer (GameState _ _ player _ _) = player
setPlayer player (GameState screen format _ colors wells) = GameState screen format player colors wells

player = lens getPlayer setPlayer

getColors (GameState _ _ _ colors _) = colors
setColors colors (GameState screen format player _ wells) = GameState screen format player colors wells

colors = lens getColors setColors

getWells (GameState _ _ _ _ wells) = wells
setWells wells (GameState screen format player colors _) = GameState screen format player colors wells

wells = lens getWells setWells
