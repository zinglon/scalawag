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

main :: IO ()
main = do SDL.init [SDL.InitTimer, SDL.InitVideo]
          SDL.setVideoMode 800 800 32 [SDL.OpenGL]
          gameLoop =<< return newGame
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
                 let gs' = updateGame (catMaybes $ keypresses <$> evs) gs
                 GL.clear [GL.ColorBuffer]
                 clearRender $ renderGame gs'
                 SDL.glSwapBuffers
                 unless (any (== SDL.Quit) evs) (SDL.delay 20 >> gameLoop gs')
                 
data Terrain = Dirt | Wall
data Level = Level { size :: Int,
                     terrain :: [[Terrain]] }
data Game = Game { player :: (Int,Int),
                   level :: Level }

newGame :: Game
newGame = Game (0,0) (emptyLevel 9)

emptyLevel :: Int -> Level
emptyLevel sz = Level sz (replicate sz (replicate sz Dirt))

updateGame :: [SDLKey] -> Game -> Game
updateGame ks g = g { player = clamp  (size $ level g) $  foldr movePlayer (player g) (getMoves ks) }
                  where clamp n (x,y) = (max 0 $ min (n-1) x, max 0 $ min (n-1) y) --oh god
                        getMoves = catMaybes . map getMove
                        
getMove :: SDLKey -> Maybe (Int,Int)
getMove SDLK_DOWN  = Just ( 0, 1)
getMove SDLK_UP    = Just ( 0,-1)
getMove SDLK_LEFT  = Just (-1, 0)
getMove SDLK_RIGHT = Just ( 1, 0)
getMove _          = Nothing

movePlayer :: (Int,Int) -> (Int,Int) -> (Int,Int)
movePlayer (x1,y1) (x2,y2) = (x1+x2, y1+y2)

renderGame :: Game -> Image Any
renderGame g = translate(-1,1) %% scale (2/ (fromIntegral $ size $ level g)) (-2/ (fromIntegral $ size $ level g)) %% doCrap
  where doCrap = drawPlayer
        drawPlayer = (translate $ toDamnR2 $ player g) 
                     %% tint (Color 0 1 0 1) (convexPoly [(0,0), (0,1), (1,1), (1,0)])

toDamnR2 :: Integral a => (a,a) -> R2
toDamnR2 (x,y) = (fromIntegral x, fromIntegral y)

