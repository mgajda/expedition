import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDL
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.Random

width = 640
height = 480

main = withInit [InitVideo] $ 
    do screen <- setVideoMode height width 16 [SWSurface]
       setCaption "Meet aliens" ""
       enableUnicode True
       image <- load "assets/eagle.png"
       --image <- loadBMP "../image.bmp"
       display image
       loop (display image)

display :: Surface -> IO ()
display image
    = do screen <- getVideoSurface
         let format = surfaceGetPixelFormat screen
         white <- mapRGB format 0xFF 0xFF 0xFF
         green <- mapRGB format 0 0xFF 0
         fillRect screen Nothing white
         --fillRect screen (Just (Rect 10 10 10 10)) red
         posX <- randomRIO (0,width-1-surfaceGetWidth image)
         posY <- randomRIO (0,height-1-surfaceGetHeight image)
         blitSurface image Nothing screen (Just (Rect posX posY 0 0))
         SDL.flip screen


loop :: IO () -> IO ()
loop display
    = do event <- waitEvent
         case event of
           Quit -> exitWith ExitSuccess
           KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
           KeyDown (Keysym _ _ ' ') -> display
           _ -> return ()
         loop display

