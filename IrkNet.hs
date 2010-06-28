module IrkNet where

import qualified IrkParse

import IrkIRC

import qualified System.IO as SysIO
import qualified System.IO.Error as SysIOErr

data IrkTrigger = IrkTrigger [String] Bool
type IrkHandler = (IrcMessage -> IO IrkTrigger)

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
                        putStrLn $ "Input: " ++ s
                        case (IrkParse.irkParse IrkParse.ircpMessage s) of
                            Nothing -> do
                                    putStrLn $ "Warning: no parse -- \"" ++ s ++ "\""
                                    handleIrcServer h handlers
                            Just msg -> do
                                    replies <- applyHandlers handlers msg
                                    writeLinesEcho h replies
                                    handleIrcServer h handlers

applyHandlers :: [IrkHandler] -> IrcMessage -> IO [String]
applyHandlers (f:fs) s = do
                            IrkTrigger ls c <- f s
                            if not c then
                                return ls
                                else do
                                    ls' <- applyHandlers fs s
                                    return $ ls ++ ls'
applyHandlers [] _ = return []

-- Handlers (some are just for testing)
ignoreHandler :: IrkHandler
ignoreHandler _ = return $ IrkTrigger [] True

handlerIrcShow :: IrkHandler
handlerIrcShow msg@(IrcMessage _ _ _) =
    do putStrLn $ show $ msg
       return $ IrkTrigger [] True

pingHandler :: IrkHandler
pingHandler (IrcMessage _ (IrcCommand "PING") pms) =
    return $ IrkTrigger [ formatMessage $ (IrcMessage Nothing (IrcCommand "PONG") pms) ] False
pingHandler x = ignoreHandler x
