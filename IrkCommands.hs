module IrkCommands where

import IrkIRC
import IrkBot
import IrkNet

import Maybe
import List

commandHandler :: IrkHandler
commandHandler st (IrcMessage (Just origin) (IrcCommand "PRIVMSG") pms)
    | startsWith prefix messageText
        = let (st', replies) = handleCommand (words $ drop (length prefix) messageText) messageTarget origin st
          in return $ IrkTrigger replies st' False
    | messageTarget == stateBotnick st
        = let (st', replies) = handleCommand (words messageText) originNick origin st
          in return $ IrkTrigger replies st' False
    | otherwise = return $ IrkTrigger [] st True
    where
        startsWith [] _ = True
        startsWith _ [] = False
        startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
                                 | otherwise = False
        prefix = (stateBotnick st) ++ ": "
        messageTarget = pms !! 0 -- security hole obvs
        messageText = pms !! 1
        Just originNick = prefixName origin
commandHandler st _ = return $ IrkTrigger [] st True

makeReply :: String -> String -> [String]
makeReply ctx msg = [ makeCommand "NOTICE" [ctx, msg] ]

getPrivilege :: BotState -> IrcPrefix -> PrivilegeLevel
getPrivilege st (IrcPrefix (Just a) (Just b) (Just c))
    | isNothing user = NotIdentified
    | otherwise = priv
    where
        SecurityState _ idUsers = stateSecurity st
        user = find (\(IdentifiedUser a' b' c' _) -> (a == a') && (b == b') && (c == c')) idUsers
        Just (IdentifiedUser _ _ _ priv) = user
getPrivilege _ _ = NotIdentified

handleCommand :: [String] -> String -> IrcPrefix -> BotState -> (BotState, [String])
handleCommand ("identify":as) ctx org@(IrcPrefix (Just a) (Just b) (Just c)) st
    | isNothing user = (st, makeReply ctx "Access denied.")
    | (getPrivilege st org) /= NotIdentified = (st, makeReply ctx "Already identified." )
    | otherwise = ( st{stateSecurity = SecurityState regUsers (newid:idUsers)},
                    makeReply ctx ("Identified as " ++ newun ++ ".") )
    where
        pw = as !! 0
        SecurityState regUsers idUsers = stateSecurity st
        user = find (\(RegisteredUser _ password _) -> password == pw) regUsers
        Just (RegisteredUser newun _ newpriv) = user
        newid = IdentifiedUser a b c newpriv

handleCommand ("quit":_) ctx org st
    | getPrivilege st org == AdminPrivilege = (st, [makeCommand "QUIT" ["Leaving."]])
    | otherwise = (st, makeReply ctx "Access denied.")

handleCommand ("privileges":_) ctx org st
    | getPrivilege st org == AdminPrivilege = (st, makeReply ctx "You have admin privileges.")
    | otherwise = (st, makeReply ctx "You have no privileges.")

handleCommand ("join":channel:_) ctx org st
    | getPrivilege st org == AdminPrivilege = (st, [makeCommand "JOIN" [ channel ]])
    | otherwise = (st, makeReply ctx "Access denied.")

handleCommand ("leave":channel:_) ctx org st
    | getPrivilege st org == AdminPrivilege = (st, [makeCommand "PART" [ channel ]])
    | otherwise = (st, makeReply ctx "Access denied.")

handleCommand ("speak":channel:rest) ctx org st
    | getPrivilege st org == AdminPrivilege = (st, [makeCommand "PRIVMSG" [ channel, unwords rest ]])
    | otherwise = (st, makeReply ctx "Access denied.")

handleCommand ("fibonacci":_) ctx _ st
    = (st {stateFibonacci = nextFib fibst },
       makeReply ctx ("The next fibonacci number is " ++ show n ++ "."))
        where
            curFib (FibonacciState a _) = a
            nextFib (FibonacciState a b) = FibonacciState b (a+b)
            fibst = stateFibonacci st
            n = curFib fibst

handleCommand (_:_) ctx _ st = (st, makeReply ctx "Unknown command.")
handleCommand [] ctx _ st = (st, makeReply ctx "No command given.")
