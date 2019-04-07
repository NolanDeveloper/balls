{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified SDL
import SDL (($=), (^-^), (^*), (^+^), (*^), Point (P), V2 (V2), norm, normalize, dot)
import Control.Monad
import Foreign.C.Types
import qualified Data.Vector.Storable as Vector
import Data.List
import System.Random
import Control.Parallel
import Control.Parallel.Strategies hiding (dot)
import qualified Control.Parallel.Strategies (dot)
import Control.DeepSeq
import GHC.Conc
import GHC.Generics
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Array.IO as A
import Data.Function

data Ball = Ball
    { ballPosition :: V2 Float
    , ballVelocity :: V2 Float
    } deriving (Eq)

windowWidth :: Real a => a
windowWidth = 640

windowHeight :: Real a => a
windowHeight = 480

radius :: Real a => a
radius = 8

totalNumber :: Int
totalNumber = 700

speedScale :: Real a => a
speedScale = 50

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    let windowConfig = 
            SDL.defaultWindow {
                SDL.windowInitialSize = V2 windowWidth windowHeight
            }
    window <- SDL.createWindow "Balls" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    startBalls <- flip traverse [1..totalNumber] $ \i -> do
        x <- randomRIO (radius, windowWidth - radius)
        y <- randomRIO (radius, windowHeight - radius)
        vx <- randomRIO (-speedScale, speedScale)
        vy <- randomRIO (-speedScale, speedScale)
        pure $ Ball (V2 x y) (V2 vx vy)
    now <- SDL.time
    prevTime <- newIORef now
    balls <- A.newListArray (0, totalNumber - 1) startBalls
    fix $ \f -> do
        events <- SDL.pollEvents
        shouldStop <- processEvents events
        SDL.rendererDrawColor renderer $= SDL.V4 50 50 50 255
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
        now <- SDL.time
        time <- (now -) <$> readIORef prevTime
        writeIORef prevTime now
        updateBalls time balls
        forM_ [0..totalNumber - 1] $ \i -> do
            ball <- A.readArray balls i
            drawBall renderer ball
        SDL.present renderer
        unless shouldStop f

processEvents :: [SDL.Event] -> IO Bool
processEvents events = do
    pure (any eventIsQPress events)
  where
    eventIsQPress :: SDL.Event -> Bool
    eventIsQPress event =
        case SDL.eventPayload event of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed 
                && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
            SDL.WindowClosedEvent{} -> True
            _ -> False

drawBall :: SDL.Renderer -> Ball -> IO ()
drawBall renderer (Ball position _) = SDL.drawLines renderer points
  where
    r = radius
    n = max 8 (ceiling $ 2 * pi * r / 10)
    makePoint angle = P $ floor <$> (position ^+^ r *^ (V2 (cos angle) (sin angle)))
    points = Vector.fromList $ map (\i -> makePoint (fromIntegral i / fromIntegral n * 2 * pi)) [0..n]

intersecting :: Ball -> Ball -> Bool
intersecting (Ball ac _) (Ball bc _) = norm (bc ^-^ ac) <= 2 * radius

updateBalls :: Float -> A.IOArray Int Ball -> IO ()
updateBalls time balls = do
    -- collide
    forM_ [0..totalNumber - 1] $ \i -> do
        forM_ [0..totalNumber - 1] $ \j -> do
            a@(Ball ac av) <- A.readArray balls i
            b@(Ball bc bv) <- A.readArray balls j
            when (i < j && intersecting a b) $ do
                let n = normalize (bc ^-^ ac)
                let vn1 = n ^* (av `dot` n)
                let vn2 = n ^* (bv `dot` n)
                let vt1 = av ^-^ vn1
                let vt2 = bv ^-^ vn2
                A.writeArray balls i (Ball ac (vt1 ^+^ vn2))
                A.writeArray balls j (Ball bc (vt2 ^+^ vn1))
    -- bounce walls
    forM_ [0..totalNumber - 1] $ \i -> do
        ball@(Ball c@(V2 x y) (V2 vx vy)) <- A.readArray balls i
        when (y - radius < 0 && vy < 0) $ do
            A.writeArray balls i (Ball c (V2 vx (-vy)))
        ball@(Ball c@(V2 x y) (V2 vx vy)) <- A.readArray balls i
        when (y + radius > windowHeight && vy > 0) $ do
            A.writeArray balls i (Ball c (V2 vx (-vy)))
        ball@(Ball c@(V2 x y) (V2 vx vy)) <- A.readArray balls i
        when (x - radius < 0 && vx < 0) $ do
            A.writeArray balls i (Ball c (V2 (-vx) vy))
        ball@(Ball c@(V2 x y) (V2 vx vy)) <- A.readArray balls i
        when (x + radius > windowWidth && vx > 0) $ do
            A.writeArray balls i (Ball c (V2 (-vx) vy))
    -- uncouple
    forM_ [0..totalNumber - 1] $ \i ->  do
        forM_ [0..totalNumber - 1] $ \j -> do
            a@(Ball ac av) <- A.readArray balls i
            b@(Ball bc bv) <- A.readArray balls j
            when (i < j && intersecting a b) $ do
                let ab = bc ^-^ ac
                let n = normalize ab
                let factor = (2 * radius - norm ab) / 2
                A.writeArray balls i (Ball (ac ^+^ n ^* (-factor)) av)
                A.writeArray balls j (Ball (bc ^+^ n ^* factor) bv)
    -- move
    forM_ [0..totalNumber - 1] $ \i -> do
        ball@(Ball c v) <- A.readArray balls i
        A.writeArray balls i (Ball (c ^+^ (time *^ v)) v)
