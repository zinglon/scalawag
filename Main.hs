{--
    Main.hs
    Main file for this terrible roguelike.

    License: WTFPL
--}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import Core
import Game
import Vec
import UI
import Util


main :: IO () 
main = do initUI
          input <- newTChanIO 
          game <- newTVarIO newGame
          quit <- newTVarIO False
          forkIO . forever . atomically $ updateStuff input game quit
          let actions = getEvents input >> renderFrame game
          beginLoop 20 actions quit
          killUI

updateStuff :: TChan Event -> TVar Game -> TVar Bool -> STM ()
updateStuff i g q = do e <- readTChan i
                       when (e == Quit) $ writeTVar q True
                       gs <- readTVar g
                       let gs' = updateGame (mapMaybe keypresses [e]) gs
                       writeTVar g gs'


getEvents :: TChan Event -> IO ()
getEvents i = atomically . mapM_ (writeTChan i) =<< (map translateEvent) <$> newEvents

renderFrame :: TVar Game -> IO ()
renderFrame g = do gs <- atomically $ readTVar g
                   doRendering $ renderGame gs

allDone :: TVar Bool -> IO Bool
allDone q = atomically $ readTVar q

beginLoop :: Ticks -> IO () -> TVar Bool -> IO ()
beginLoop ft acts q = void $ iterWhileM (eventLoop ft acts) (allDone q) 0

eventLoop :: Ticks -> IO () -> Ticks -> IO Ticks
eventLoop ft acts dt = do when (ft > dt) $ delay (ft - dt)
                          timeAction acts


updateGame :: [SDLKey] -> Game -> Game
updateGame ks g = modL player (fmap clampToLevel . (+ sum playerMoves)) g 
  where clampToLevel = clamp 0 (subtract 1 $ getL levelSize g)
        playerMoves = mapMaybe getMove ks

getMove :: SDLKey -> Maybe (Pt2 Int)
getMove SDLK_DOWN  = Just $ pt2 0 1
getMove SDLK_UP    = Just $ pt2 0 (-1)
getMove SDLK_LEFT  = Just $ pt2 (-1) 0
getMove SDLK_RIGHT = Just $ pt2 1 0
getMove _          = Nothing

