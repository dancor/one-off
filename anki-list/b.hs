#include <h>

main = do
    old_wds <- Set.fromList . BSC.lines <$> BS.readFile "anki_wds"
    new_wds <-
        filter ((`Set.notMember` old_wds) . head . BSC.words) . BSC.lines <$>
        BS.readFile "/home/danl/p/l/melang/data/cmn/gbRec/defs"
    BS.putStr . BSC.unlines $ new_wds
