module IrkNet where

import qualified IrkParse

import IrkIRC
import IrkBot

import qualified System.IO as SysIO
import qualified System.IO.Error as SysIOErr

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

handleIrcServer :: SysIO.Handle -> BotState -> [IrkHandler] -> IO BotState
handleIrcServer h st handlers = do
    v <- SysIOErr.try (SysIO.hGetLine h)
    case v of
        Left _ -> return st
        Right s -> do
                        putStrLn $ "Input: " ++ s
                        case (IrkParse.irkParse IrkParse.ircpMessage s) of
                            Nothing -> do
                                    putStrLn $ "Warning: no parse -- \"" ++ s ++ "\""
                                    handleIrcServer h st handlers
                            Just msg -> do
                                    (replies, st') <- applyHandlers st handlers msg
                                    writeLinesEcho h replies
                                    handleIrcServer h st' handlers

applyHandlers :: BotState -> [IrkHandler] -> IrcMessage -> IO ([String], BotState)
applyHandlers st (f:fs) s = do
                            IrkTrigger ls st' c <- f st s
                            if not c then
                                return (ls,st')
                                else do
                                    (ls', st'') <- applyHandlers st' fs s
                                    return $ (ls ++ ls', st'')
applyHandlers st [] _ = return ([],st)

makeCommand :: String -> [String] -> String
makeCommand cmd pms = formatMessage $ (IrcMessage Nothing (IrcCommand cmd) pms)

sendCommand :: SysIO.Handle -> String -> [String] -> IO ()
sendCommand h cmd pms = writeLinesEcho h $ [ makeCommand cmd pms ]

-- Handlers (some are just for testing)
ignoreHandler :: IrkHandler
ignoreHandler st _ = return $ IrkTrigger [] st True

handlerIrcShow :: IrkHandler
handlerIrcShow st msg@(IrcMessage _ _ _) =
    do putStrLn $ show $ msg
       return $ IrkTrigger [] st True

pingHandler :: IrkHandler
pingHandler st (IrcMessage _ (IrcCommand "PING") pms) =
    return $ IrkTrigger [ makeCommand "PONG" pms ] st False
pingHandler st x = ignoreHandler st x
