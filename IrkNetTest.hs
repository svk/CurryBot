import qualified Network as Net
import qualified IrkNet
import qualified System.IO as SysIO

import qualified IrkCommands as IrkCmd

import IrkBot

botRealname :: String
botRealname = "currybot"

botNick :: String
botNick = "CurryBot"

handlers :: [ IrkBot.IrkHandler ]
handlers = [ IrkNet.handlerIrcShow, IrkNet.pingHandler, IrkCmd.commandHandler ]

main :: IO()
main = Net.withSocketsDo $ do
    h <- Net.connectTo "irc.quakenet.org" (Net.PortNumber 6667)
    IrkNet.sendCommand h "USER" [ botRealname, "*", "*", botRealname ]
    IrkNet.sendCommand h "NICK" [ botNick ]
    _ <- IrkNet.handleIrcServer h (BotState (FibonacciState 1 1)
                                  (SecurityState [RegisteredUser "kaw" "achilles95" AdminPrivilege] [])
                                  botNick
                                ) handlers
    SysIO.hClose h
