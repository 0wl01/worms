{-|
Module      : Data
Description : Central data types and game engine states.
Copyright   : (c) 0wl01, 2026
Maintainer  : programming0wl01@gmail.com

This module contains all structural definitions of the game.
It contains no computational logic or state manipulation,
only the architectural structure: coordinates, entities, weapons, terrain types, etc.
-}

module Data
    ( module Data
    , module Grid
    ) where

-- | Basic matrix type for the game grid.
type Matrix a = [[a]]

-- | (Row, Column) coordinates.
type Position = (Int,Int)

-- | Dimensions (Rows, Columns) of the Game grid.
type Dimensions = (Int,Int)

-- | Time units in the game engine.
type Ticks = Int

-- | Identifier for a worm in the global list.
type WormID = Int

-- | Identifier for an object in the global list.
type ObjectID = Int

-- | The 8 possible movement and aiming directions.
data Direction
    = North | Northeast | East | Southeast | South | Southwest | West | Northwest
    deriving (Eq, Ord, Show, Read, Enum)

-- | Terrain types that make up the map.
data Terrain
    = Air   -- ^ Empty space, entities can fall or pass through.
    | Water -- ^ Lethal terrain.
    | Dirt  -- ^ Opaque and destructible terrain.
    | Stone -- ^ Opaque and indestructible terrain.
    deriving (Eq, Ord, Show, Read, Enum)

-- | The game's map, represented as a matrix of terrains.
type Map = Matrix Terrain

-- | Available weapons in the game.
data Weapon 
    = Jetpack | Driller | Bazooka | Mine | Dynamite
    deriving (Eq, Ord, Show, Read, Enum)

-- | A worm's vitality state.
data WormHP
    = Health Int -- ^ Alive with a specific amount of health points.
    | Dead       -- ^ The worm has been eliminated. RIP.
    deriving (Eq, Ord, Show, Read)

-- | A single worm entity in the game.
data Worm = Worm
    { wormPosition   :: Maybe Position -- ^ The worm's current position (Nothing if outside the map).
    , wormHP         :: WormHP         -- ^ Current health status.
    , jetpackFuel    :: Int            -- ^ Jetpack fuel left.
    , drillerFuel    :: Int            -- ^ Driller fuel left.
    , bazookaAmmo    :: Int            -- ^ Bazooka shots left.
    , mineAmmo       :: Int            -- ^ Mines left.
    , dynamiteSticks :: Int            -- ^ Dynamite sticks left.
    } deriving (Eq, Ord, Show, Read)


-- | Represents an interactive entity on the map.
data GameObject
    = Projectile
        { shotPosition  :: Position    -- ^ Current position of the projectile in the map.
        , shotDirection :: Direction   -- ^ The direction it is traveling.
        , shotWeapon    :: Weapon      -- ^ The weapon type that generated this shot.
        , shotTimer     :: Maybe Ticks -- ^ Optional countdown until detonation.
        , shotOwner     :: WormID      -- ^ The ID of the worm that fired it.
        }
    | Barrel
        { barrelPosition :: Position   -- ^ The barrel's location on the map.
        , barrelExplodes :: Bool       -- ^ Flag indicating if it is about to detonate in the current tick.
        }
    deriving (Eq, Ord, Show, Read)

-- | The master state of the game containing all necessary simulation data.
data GameState = GameState
    { gameGrid    :: Map          -- ^ The current destructible environment.
    , gameObjects :: [GameObject] -- ^ Active projectiles and explosive barrels.
    , gameWorms   :: [Worm]       -- ^ All worms (alive or dead).
    } deriving (Eq, Ord, Show, Read)

-- | Actions a player can execute during their turn.
data Action
    = Shoot Weapon Direction -- ^ Fire a specific weapon in a given direction.
    | Move Direction         -- ^ Walk or jump in a given direction.
    deriving (Eq, Ord, Show, Read)