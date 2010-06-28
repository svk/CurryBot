module IrkIRC where

import Maybe

data IrcMessage = IrcMessage (Maybe IrcPrefix) IrcCommand [String]
data IrcPrefix = IrcPrefix { prefixName :: Maybe String,
                             prefixUser :: Maybe String,
                             prefixHost :: Maybe String }
data IrcCommand = IrcCommand String
                  | IrcNumericReply Int

instance Show IrcMessage where
    show (IrcMessage Nothing cmd pms) = ("(IrcMessage: " ++ show cmd ++ " " ++ show pms ++ ")")
    show (IrcMessage (Just pfx) cmd pms) = ("(IrcMessage: " ++ show pfx ++ " " ++ show cmd ++ " " ++ show pms ++ ")")

instance Show IrcPrefix where
    show x = show $ map (\(Just y) -> y) (filter isJust [ prefixName x, prefixUser x, prefixHost x ])

instance Show IrcCommand where
    show (IrcCommand s) = s
    show (IrcNumericReply n) = show n

formatPrefix :: (Maybe IrcPrefix) -> String
formatPrefix Nothing = ""
formatPrefix (Just x) = case (prefixName x) of
                            Nothing -> ""
                            Just a -> a ++ case (prefixUser x) of
                                            Nothing -> ""
                                            Just b -> "!" ++ b
                                        ++ case (prefixHost x) of
                                            Nothing -> ""
                                            Just c -> "@" ++ c

formatCommand :: IrcCommand -> String
formatCommand (IrcCommand s) = s
formatCommand (IrcNumericReply n) = show n

formatParameters :: [String] -> String
formatParameters (a:[]) = " :" ++ a
formatParameters (a:b) = " " ++ a ++ (formatParameters b)
formatParameters [] = " "

formatMessage :: IrcMessage -> String
formatMessage (IrcMessage mb cmd pms) = (formatPrefix mb) ++ 
                                        (formatCommand cmd) ++
                                        (formatParameters pms)
