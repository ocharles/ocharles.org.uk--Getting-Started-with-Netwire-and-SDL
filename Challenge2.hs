{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  void $ go empty screen clockSession challenge2

 where

  go keysDown screen s w = do
    keysDown' <- parseEvents keysDown
    (x, w', s') <- stepSession_ w s keysDown'

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    go keysDown' screen s' w'

challenge2 :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) Double
challenge2 = integral_ 0 . challenge2_velocity

challenge2_velocity :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) Double
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
