module IrkParse where

import IrkIRC

-- This is basically reinventing Parsec to learn how it works

-- Note that this does backtrack by default unlike Parsec
-- This is convenient and efficiency should not be a concern
-- as we are parsing very small strings.

data IrcMsg = IrcMsgOther String

data IrkParser a = IrkParser (String -> Maybe (a,String))

instance Monad IrkParser where
    (IrkParser p) >>= f = IrkParser (\s -> case (p s) of
                                             Just (a, s') -> let (IrkParser p') = f a
                                                             in p' s'
                                             Nothing -> Nothing)
    (IrkParser p) >> (IrkParser p') = IrkParser (\s -> case (p s) of
                                             Just (_, s') -> p' s'
                                             Nothing -> Nothing)
    return x = IrkParser (\s -> Just (x,s))
    fail _ = IrkParser (\_ -> Nothing)


(<|>) :: IrkParser a -> IrkParser a -> IrkParser a
(IrkParser f) <|> (IrkParser g) = IrkParser (\x -> case (f x) of
                                                Just y -> Just y
                                                Nothing -> g x)

ipSymbol :: Char -> IrkParser ()
ipSymbol s = IrkParser (\st -> if null st
                                    then Nothing
                                    else if ((head st) == s) then
                                         Just ((),(tail st))
                                         else Nothing)

ipChar :: (Char -> Bool) -> IrkParser (Char)
ipChar pr = IrkParser (\st -> if null st
                                then Nothing
                                else if (pr (head st)) then
                                     Just ((head st),(tail st))
                                     else Nothing)

suffixAfter :: (Eq a) => [a] -> [a] -> Maybe [a]
suffixAfter [] r = Just r
suffixAfter _ [] = Nothing
suffixAfter (x:xs) (y:ys) | x == y = suffixAfter xs ys
                          | otherwise = Nothing

ipString :: String -> IrkParser ()
ipString s = IrkParser (\st -> case (suffixAfter s st) of
                                    Nothing -> Nothing
                                    Just st' -> Just ((),st'))

ipStringUntil :: (Char -> Bool) -> IrkParser String
ipStringUntil p = IrkParser f
    where
        f (c:cs) = if (p c) then
                        Just ([], cs)
                        else let Just (a,b) = f cs
                             in Just (c:a, b)
        f [] = Just ([],[])

ipEnd :: IrkParser ()
ipEnd = IrkParser (\st -> if null st then
                                Just ((),[])
                                else Nothing)

irkParse :: IrkParser a -> String -> Maybe a
irkParse (IrkParser f) s = fmap fst (f s)


ipMany :: (IrkParser a) -> IrkParser [a]
ipMany (IrkParser p) = IrkParser f
    where
        f st = case (p st) of
            Nothing -> Just ([],st)
            Just (x, st') -> let Just (xs, st'') = f st'
                             in Just ((x:xs), st'')

ipMany1 :: (IrkParser a) -> IrkParser [a]
ipMany1 (IrkParser p) = IrkParser g
    where
        g st = case (p st) of
            Nothing -> Nothing
            Just (x, st') -> let Just (xs, st'') = f st'
                             in Just ((x:xs), st'')
        f st = case (p st) of
            Nothing -> Just ([],st)
            Just (x, st') -> let Just (xs, st'') = f st'
                             in Just ((x:xs), st'')

ipSkipMany :: (IrkParser a) -> IrkParser ()
ipSkipMany x = (ipMany x) >> return ()

-- Now, parsers specifically for the IRC protocol (e.g. RFC 1459)

ircpMessage :: IrkParser IrcMessage
ircpMessage = do
                ipSymbol ':'
                px <- ircpPrefix
                ircpSpaces
                IrcMessage _ cmd pms <- ircpSimpleMessage
                return $ IrcMessage (Just px) cmd pms
             <|> ircpSimpleMessage

ircpSimpleMessage :: IrkParser IrcMessage
ircpSimpleMessage = do
                        cmd <- ircpCommand
                        pms <- ircpParams
                        return $ IrcMessage Nothing cmd pms

ircpLetterPred :: Char -> Bool
ircpLetterPred a | a >= 'a' && a <= 'z' = True
                 | a >= 'A' && a <= 'Z' = True
                 | otherwise = False

ircpNumberPred :: Char -> Bool
ircpNumberPred a | a >= '0' && a <= '9' = True
                 | otherwise = False

ircpAlphaNumericPred :: Char -> Bool
ircpAlphaNumericPred x = (ircpLetterPred x) || (ircpNumberPred x)

ircpCommand :: IrkParser IrcCommand
ircpCommand = do
                name <- ipMany1 (ipChar ircpLetterPred)
                return $ IrcCommand name
              <|> do
                a <- ipChar ircpNumberPred
                b <- ipChar ircpNumberPred
                c <- ipChar ircpNumberPred
                return $ IrcNumericReply $ read [a,b,c]

ircpNick :: IrkParser String
ircpNick = do
                fl <- ipChar ircpLetterPred
                ml <- ipMany (ipChar ircpAlphaNumericPred)
                return (fl:ml)

ircpPrefix :: IrkParser IrcPrefix
ircpPrefix = do
                name <- ircpPrefixName
                user <- do ipSymbol '!'
                           username <- ircpPrefixUser
                           return (Just username)
                           <|> return Nothing
                host <- do ipSymbol '@'
                           hostname <- ircpHostname
                           return (Just hostname)
                           <|> return Nothing
                return $ IrcPrefix (Just name) user host

ircpPrefixName :: IrkParser String
ircpPrefixName = ircpHostname
                 <|> ircpNick -- never reached at the moment

ircpParams :: IrkParser [String]
ircpParams = do
                ircpSpaces
                ipSymbol ':'
                trailing <- ipStringUntil (=='\r')
                return [trailing]
             <|> do
                ircpSpaces
                middle <- ircpMiddle
                more <- ircpParams
                return (middle:more)
             <|> do
                ipSymbol '\r' -- the LF is stripped off earlier, in getLine
                ipEnd
                return []

ircpMiddle :: IrkParser String
ircpMiddle = do
                first <- ipChar (not . (`elem`[' ','\0','\r', '\n',':']))
                rest <- ipMany (ipChar (not . (`elem`[' ','\0','\r', '\n'])))
                return (first:rest)

ircpSpaces :: IrkParser ()
ircpSpaces = do ipSkipMany (ipSymbol ' ')
                return ()

-- For hostnames: faking it. See RFC 952 for the full def.
-- (Actually, see a newer RFC, IPv6 and all that.)
ircpHostname :: IrkParser String
ircpHostname = do
                name <- ipMany1 (ipChar ircpAlphaNumericPred)
                ipSymbol '.'
                rest <- ircpHostname
                return (name ++ "." ++ rest)
               <|> do
                name <- ipMany1 (ipChar ircpAlphaNumericPred)
                return name

-- This is also "faking it"? Surely it can't just be "nonwhite";
-- @ is nonwhite. Not sure what is proper.
ircpPrefixUser :: IrkParser String
ircpPrefixUser = ircpNick
