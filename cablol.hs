import Data.Char
import HSH
import System.Environment
import System.Exit

doDeps :: [String] -> IO ()
doDeps pkgArgs = do
    lines <- run ("cabal", "install":"--dry-run":pkgArgs)
    let deps = map (init . takeWhile (not . isDigit)) $ drop 2 lines
        inst dep = do
             e <- run ("sudo",
                 [ "apt-get"
                 , "install"
                 , "--assume-yes"
                 , "libghc-" ++ dep ++ "-doc"
                 , "libghc-" ++ dep ++ "-prof"])
             print (e :: ExitCode)
        cab dep = runIO ("cabal", "install":"-j":pkgArgs)
    --out <- run ("apt-cache", ["search", dep]) -|- ("grep", ["ghc"])
    --mapM_ inst deps
    mapM_ cab deps

main :: IO ()
main = do
    args <- getArgs
    case args of
      [""] -> doDeps []
      [pkg] -> doDeps [pkg]
      _ -> error "usage"
