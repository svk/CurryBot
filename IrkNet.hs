module IrkNet where

import qualified IrkParse

import qualified System.IO as SysIO
import qualified System.IO.Error as SysIOErr

data IrkTrigger = IrkTrigger [String] Bool
type IrkHandler = (String -> IO IrkTrigger)

writeLine :: SysIO.Handle -> String -> IO()
writeLine h s = do
    SysIO.hPutStr h s
    SysIO.hPutStr h "\r\n"
    SysIO.hFlush h

writeLines :: SysIO.Handle -> [String] -> IO()
writeLines h (s:ss) = do writeLine h s
                         writeLines h ss
writeLines _ [] = return ()

writeLinesEcho :: SysIO.Handle -> [String] -> IO()
writeLinesEcho h (s:ss) = do writeLine h s
                             putStrLn $ "Output: " ++ s
                             writeLinesEcho h ss
writeLinesEcho _ [] = return ()

handleIrcServer :: SysIO.Handle -> [IrkHandler] -> IO()
handleIrcServer h handlers = do
    v <- SysIOErr.try (SysIO.hGetLine h)
    case v of
        Left _ -> return ()
        Right s -> do
                        replies <- applyHandlers handlers s
                        writeLinesEcho h replies
                        handleIrcServer h handlers

applyHandlers :: [IrkHandler] -> String -> IO [String]
applyHandlers (f:fs) s = do
                            IrkTrigger ls c <- f s
                            if not c then
                                return ls
                                else do
                                    ls' <- applyHandlers fs s
                                    return $ ls ++ ls'
applyHandlers [] _ = return []

-- Handlers for testing
handlerEcho :: IrkHandler
handlerEcho s = return $ IrkTrigger [s] False

handlerShow :: IrkHandler
handlerShow s = do putStrLn $ "Input: " ++ (show s)
                   return $ IrkTrigger [] True

handlerIrcShow :: IrkHandler
handlerIrcShow s = do putStrLn $ show $ IrkParse.irkParse IrkParse.ircpMessage s
                      return $ IrkTrigger [] True
