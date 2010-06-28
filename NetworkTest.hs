import qualified Network as Net
import System.IO
import qualified System.IO.Error as IOErr
import System (getArgs)
import qualified Control.Concurrent as Conc

netWrite :: Handle -> String -> IO()
netWrite h s = do
    hPutStr h s
    hFlush h

handleConnection :: Handle -> String -> Net.PortNumber -> IO()
handleConnection h host portno = do
    putStrLn "Ready for connection."
    putStrLn ("Connection: " ++ host ++ " " ++ (show portno))
    handleLines
    putStrLn ("Closing connection: " ++ host ++ " " ++ (show portno))
    hClose h
    where
        handleLines = do
            v <- IOErr.try (hGetLine h)
            case v of
                Left _ -> putStrLn ("Error. ")
                Right s -> do
                                putStrLn ("Input: " ++ s)
                                handleLines

doServer :: IO()
doServer = do
    sock <- Net.listenOn (Net.PortNumber 6668)
    handleConnections sock
    where
        handleConnections sock = do
                                (h, host, portno) <- Net.accept sock
                                _ <- Conc.forkIO $ handleConnection h host portno
                                handleConnections sock

doClient :: IO()
doClient = do
    h <- Net.connectTo "localhost" (Net.PortNumber 6668)
    s <- getLine
    netWrite h s
    hClose h

printUsage :: IO()
printUsage = do
    putStrLn "Usage: NetworkTest [server|client]"

main :: IO()
main = Net.withSocketsDo $ do
    cmdargs <- getArgs
    case (length cmdargs) of
        1 -> case (head cmdargs) of
            "server" -> doServer
            "client" -> doClient
            _ -> printUsage
        _ -> printUsage 
