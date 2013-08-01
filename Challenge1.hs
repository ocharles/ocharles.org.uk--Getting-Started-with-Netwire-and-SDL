import Prelude hiding ((.), id)

import Control.Wire
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go screen clockSession challenge1

 where

  go screen s w = do
    (x, w', s') <- stepSession_ w s ()

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    go screen s' w'

challenge1 :: Monad m => Wire e m a Double
challenge1 = integral_ 0 . pure 20
