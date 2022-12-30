{-# LANGUAGE DeriveGeneric #-}
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import GHC.Generics
import System.IO (BufferMode(LineBuffering), hGetLine, hPutStrLn,
  hSetBuffering)
import System.Process (createProcess, shell, std_in, std_out,
  StdStream(CreatePipe))
data Query = Query
  { qId :: String
  , boardXSize :: Int
  , boardYSize :: Int
  , initialStones :: [[String]]
  , moves :: [[String]]
  , rules :: String
  , komi :: Float
  , maxVisits :: Int
  , includePolicy :: Bool} deriving Generic
-- Parse will fail on includePolicy = False right now.
-- Make a second Res in a different namespace?
baseQ = Query "1" 19 19 [] [] "japanese" 6.5 1 True
data RInfo = RInfo
  { scoreLead :: Float
  } deriving Generic
data Res = Res
  { id :: String
  , policy :: [Float]
  , rootInfo :: RInfo
  } deriving Generic
instance FromJSON Query
instance FromJSON Res
instance FromJSON RInfo
instance ToJSON Query
instance ToJSON Res
instance ToJSON RInfo
main = do
  putStrLn "Starting engine. First analysis will take a few extra seconds."
  (Just ih, Just oh, Nothing, _) <- createProcess $ (shell
    "analGo -quit-without-waiting 2>/dev/null")
    {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering ih LineBuffering
  hSetBuffering oh LineBuffering
  -- hPutStrLn ih $ B.unpack $ encode $ baseQ {includePolicy = True}
  hPutStrLn ih $ B.unpack $ encode baseQ
  putStrLn "Waiting for analysis."
  l <- hGetLine oh
  case decode $ B.pack l of
    Just (Res _ _ (RInfo score)) -> print score
    _ -> print "parse error"
