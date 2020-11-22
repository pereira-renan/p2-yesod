import Prelude     (IO)
import Application (appMain)
import System.Environment

main :: IO ()
main = do
  port <- getEnv "PORT"
  warp (fromIntegral $ read port) App
