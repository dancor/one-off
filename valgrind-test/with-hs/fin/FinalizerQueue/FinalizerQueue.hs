import Control.Concurrent (threadDelay, newMVar, modifyMVar, modifyMVar_)
import Control.Monad (forM_, replicateM_)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Ptr (nullPtr, FunPtr, freeHaskellFunPtr)
import System.IO (fixIO)


type MyThunk = FunPtr (IO ())
foreign import ccall "wrapper" mkThunk :: IO () -> IO MyThunk

makeCallback :: IO () -> IO (MyThunk,MyThunk)
makeCallback fun = do
    callback <- mkThunk fun
    finalizer <- fixIO $ \dPtr -> mkThunk $ do
        freeHaskellFunPtr callback
        freeHaskellFunPtr dPtr
    return (callback, finalizer)

main :: IO ()
main = do
    fs <- newMVar []
    replicateM_ 2 $ do
        newForeignPtr nullPtr $ do
            cb <- makeCallback (return ())
            modifyMVar_ fs (return . (cb:))
        threadDelay 1000000
        cbs <- modifyMVar fs (\l -> return ([],l))
        forM_ cbs $ \(thunk, finalizer) -> my_call_2 thunk finalizer

foreign import ccall safe "my_call_2"
  my_call_2 :: MyThunk -> MyThunk -> IO ()
