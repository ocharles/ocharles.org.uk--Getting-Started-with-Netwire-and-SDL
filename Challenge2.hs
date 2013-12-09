{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  void $ go empty screen clockSession_ challenge2 0

 where

  go keysDown screen s w x = do
    keysDown' <- parseEvents keysDown
    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right keysDown')
    let x' = either (const 0) id ex

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go keysDown' screen s' w' x'

challenge2 :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) Double
challenge2 = integral 0 . challenge2_velocity

challenge2_velocity :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) Double
challenge2_velocity  =  pure (-20) . when (keyDown SDL.SDLK_LEFT)
                    <|> pure 20 . when (keyDown SDL.SDLK_RIGHT)
                    <|> pure 0

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym
