import qualified Network as Net
import qualified IrkNet
import qualified System.IO as SysIO

botRealname :: String
botRealname = "hsBot"

botNick :: String
botNick = "hsBot__42"

handlers :: [ IrkNet.IrkHandler ]
handlers = [ IrkNet.handlerShow ]

main :: IO()
main = Net.withSocketsDo $ do
    h <- Net.connectTo "irc.ablenet.org" (Net.PortNumber 6667)
    IrkNet.writeLinesEcho h [ ("USER " ++ botRealname ++ " * * :" ++ botRealname),
                              ("NICK " ++ botNick ) ]
    IrkNet.handleIrcServer h handlers
    SysIO.hClose h
