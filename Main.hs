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
          let actions = getEvents input quit >> renderFrame game
          beginLoop 20 actions quit
          killUI

updateStuff :: TChan GameAction -> TVar Game -> TVar Bool -> STM ()
updateStuff i g q = do e <- readTChan i                       
                       gs <- readTVar g
                       let gs' = updateGame e gs
                       writeTVar g gs'

getEvents :: TChan GameAction -> TVar Bool -> IO ()
getEvents i q = do evts <- (mapMaybe translateEvent) <$> newEvents 
                   mapM_ process evts 
                where process = either processUIEvt processGameEvt
                      processUIEvt e = when (e == QuitGame) (atomically $ writeTVar q True)
                      processGameEvt = atomically . writeTChan i 

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

updateGame :: GameAction -> Game -> Game
updateGame (Move pt) g = modL player (fmap clampToLevel . (+ pt)) g 
  where clampToLevel = clamp 0 (subtract 1 $ getL levelSize g)
updateGame _ g = g
