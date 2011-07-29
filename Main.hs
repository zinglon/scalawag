{--
    Main.hs
    Main file for this terrible roguelike.

    License: WTFPL
--}
import Control.Applicative
import Control.Monad
import Data.Maybe
import Graphics.DrawingCombinators
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import qualified Graphics.Rendering.OpenGL.GL as GL
import Vec

main :: IO ()
main = do SDL.init [SDL.InitTimer, SDL.InitVideo]
          SDL.setVideoMode 800 800 32 [SDL.OpenGL]
          gameLoop newGame
          SDL.quit
          
-- | Reads all pending events from SDL's queue.
newEvents :: IO [SDL.Event]
newEvents = moreEvents =<< SDL.pollEvent
    where moreEvents SDL.NoEvent = return []
          moreEvents ev = (ev:) <$> newEvents

keypresses :: SDL.Event -> Maybe SDLKey
keypresses (SDL.KeyDown sym) = Just $ SDL.symKey sym
keypresses _ = Nothing

gameLoop :: Game -> IO ()
gameLoop gs = do evs <- newEvents
                 let gs' = updateGame (mapMaybe keypresses evs) gs
                 GL.clear [GL.ColorBuffer]
                 clearRender $ renderGame gs'
                 SDL.glSwapBuffers
                 unless (any (== SDL.Quit) evs) (SDL.delay 20 >> gameLoop gs')
                 
data Terrain = Dirt | Wall deriving (Eq, Ord, Show, Read)

data Level = Level { size    :: Int
                   , terrain :: [[Terrain]] 
                   } deriving (Eq, Ord, Show, Read)

data Game = Game { player :: Pt2 Int
                 , level  :: Level 
                 } deriving (Eq, Ord, Show, Read)

newGame :: Game
newGame = Game 0 (emptyLevel 9)

emptyLevel :: Int -> Level
emptyLevel sz = Level sz (replicate sz (replicate sz Dirt))

updateGame :: [SDLKey] -> Game -> Game
updateGame ks g = g { player = clampToLevel <$> player g + sum playerMoves }
  where clampToLevel = clamp 0 (subtract 1 . size $ level g)
        playerMoves = mapMaybe getMove ks

clamp lb ub = max lb . min ub

getMove :: SDLKey -> Maybe (Pt2 Int)
getMove SDLK_DOWN  = Just $ pt2 0 1
getMove SDLK_UP    = Just $ pt2 0 (-1)
getMove SDLK_LEFT  = Just $ pt2 (-1) 0
getMove SDLK_RIGHT = Just $ pt2 1 0
getMove _          = Nothing

renderGame :: Game -> Image Any
renderGame g = translate(-1,1) 
           %% uncurry scale (toPair $ pt2 2 (-2) / (fromIntegral <$> levelDim g))
           %% doCrap
  where doCrap = drawPlayer
        drawPlayer = (translate . toPair $ fromIntegral <$> player g) 
                  %% tint (Color 0 1 0 1) (convexPoly [(0,0), (0,1), (1,1), (1,0)])

levelSize = size . level
levelDim = pure . levelSize


