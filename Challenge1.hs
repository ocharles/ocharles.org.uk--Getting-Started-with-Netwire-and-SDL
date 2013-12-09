import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go screen clockSession_ challenge1 0

 where

  go screen s w x = do
    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right x)
    let x' = either (const 0) id ex
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go screen s' w' x'

challenge1 :: (HasTime t s, Monad m) => Wire s e m Double Double
challenge1 = integral 0 + 20
