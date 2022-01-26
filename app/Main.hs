{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}
module Main where


import Control.Lens                       hiding (inside, transform)
import Control.Monad.Extra
import Control.Monad.ST
import Data.Array.Repa
import Data.Complex
import Data.Matrix                        (Matrix)
import Data.STRef
import GHC.Base                           (until)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, shift)
import Graphics.Gloss.Raster.Array
import Relude

import qualified Data.ByteString as B
import qualified Data.Matrix     as Mtx

data World = World { _v   :: Matrix Float
                   , _mtx :: Matrix Float
                   } deriving (Eq,Show)
makeLenses ''World

initialWorld :: World
initialWorld = World { _v = Mtx.fromLists [[-2],[-1]] -- FIXME no hardcode
                     , _mtx = Mtx.fromLists [[1/250,0],[0,1/250]]
                     }

type Vector a = Matrix a

data Settings = Settings { _windowSize :: (Int, Int)
                         , _offset     :: (Int, Int)
                         , _pointSize  :: (Int,Int)
                         , _threshold  :: forall a. Num a => a
                         , _fps        :: Int
                         , _iteration  :: Int
                         }
makeLenses ''Settings

settings :: Settings
settings = Settings { _windowSize = (750,500)
                    , _offset     = (250,250)
                    , _pointSize  = (1,1)
                    , _threshold  = 2
                    , _fps        = 60
                    , _iteration  = 100
                    }

window :: Display
window = InWindow "fractol" (settings^.windowSize) (settings^.offset)

toTup :: DIM2 -> (Int, Int)
toTup (Z :. y :. x) = (x,y)

toVec :: (Int, Int) -> Vector Float
toVec (!x, !y) = seq y $ seq x $ on f fromIntegral x y
  where
    f !x1 !x2 = Mtx.fromLists [[x1],[x2]]
{-# INLINE toVec #-}

transform :: World -> Vector Float -> Vector Float
transform !w !p = w^.mtx * p + w^.v
{-# INLINE transform #-}

mandel :: World -> Array D DIM2 Color
mandel !w = fromFunction (ix2 500 750) render
  where
    render (iter . transform w . toVec . toTup -> (len, n)) =
      let diff = len - settings^.threshold
          m = 2 * n
      in if diff >= 0
           then rgb' (0.9 * diff) (0.6 * diff) (0.1 * diff)
           else black

toComplex :: Vector Float -> Complex Float
toComplex !v = let [!x,!y] = Mtx.toList v
          in x :+ y
{-# INLINE toComplex #-}

iter :: Vector Float -> (Float,Int)
iter (toComplex -> !c) = let (!zn, !len, !n) = until condition endo start
                        in  (len, n)
  where
    start = (0,0,0)
    endo (!zn, !len, !n) = let new = zn * zn + c
                          in (new, magnitude new, succ n)
    {-# INLINE endo #-}
    condition (_, !len, !n) = len >= settings^.threshold
                                || n > settings^.iteration
    {-# INLINE condition #-}

{-# INLINE iter #-}

cast :: (Float, Float) -> (Int, Int)
cast (!x,!y) = on (,) floor x y
{-# INLINE cast #-}

shift :: Vector Float -> Vector Float
shift = (+ Mtx.fromLists [[375],[250]])
{-# INLINE shift #-}

eventHandler :: Event -> World -> World
eventHandler (EventKey (Char 'p') Up _ (shift . toVec . cast -> p)) w =
  let truePoint = transform w p
  in w & v -~ Mtx.scaleMatrix 0.1 truePoint
       & mtx %~ (Mtx.scaleMatrix 1.1 (Mtx.identity 2) *)
eventHandler (EventKey (Char 'n') Up _ (shift . toVec . cast -> p)) w =
  let truePoint = transform w p
  in w & v +~ Mtx.scaleMatrix 0.1 truePoint
       & mtx %~ (Mtx.scaleMatrix 0.9 (Mtx.identity 2) *)
eventHandler _ w = w

main :: IO ()
main = do
  let fractol = playArray window (settings^.pointSize) (settings^.fps)
  fractol initialWorld mandel eventHandler (const id)
