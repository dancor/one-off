#include <h>

import Database.HDBC
import Database.HDBC.Sqlite3

fromSqlByteString :: SqlValue -> BS.ByteString
fromSqlByteString (SqlByteString x) = x
fromSqlByteString a = error $ "fromSqlByteString: " ++ show a

fstCol :: BS.ByteString -> BS.ByteString
fstCol = fst . BS.breakByte 31

badChar :: Char -> Bool
badChar c = isAscii c -- || c `elem` "Ĝŝĝĉĵŭãéíô"

onlyGood :: [BS.ByteString] -> [BS.ByteString]
onlyGood = filter (not . BS.null) . map (BSC.filter $ not . badChar)

main = do
    db <- connectSqlite3 "collection.anki2"
    -- res <- quickQuery' db "SELECT flds FROM notes LIMIT 6000,3" []
    res <- quickQuery' db "SELECT flds FROM notes" []
    let allWds = map (fstCol . fromSqlByteString . head) res
    BS.putStr . BSC.unlines $ onlyGood allWds
    -- print allWds
    disconnect db
