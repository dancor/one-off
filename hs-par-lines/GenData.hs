import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment

main :: IO ()
main = do
    [nStr] <- getArgs
    replicateM_ (read nStr) . BS.putStrLn $ BS.pack "{\"a\":\"b\"}"
