module IrkBot where

import IrkIRC

data BotEvent = MessageEvent IrcMessage

data FibonacciState = FibonacciState Integer Integer

data PrivilegeLevel = AdminPrivilege | NoPrivilege | NotIdentified
    deriving (Eq, Show)

data RegisteredUser = RegisteredUser String String PrivilegeLevel -- cleartext passwords oh noes
data IdentifiedUser = IdentifiedUser String String String PrivilegeLevel

data SecurityState = SecurityState [RegisteredUser] [IdentifiedUser]

data BotState = BotState { stateFibonacci :: FibonacciState,
                           stateSecurity :: SecurityState,
                           stateBotnick :: String
}

data IrkTrigger = IrkTrigger [String] BotState Bool
type IrkHandler = ( BotState -> IrcMessage -> IO IrkTrigger)
