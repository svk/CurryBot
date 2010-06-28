import IrkParse

rawsyms :: IrkParser String
rawsyms = do
            ipSymbol ':'
            ipString "ALPHA"
            return "alpha"
          <|> do
            ipSymbol ':'
            ipString "BETA"
            return "beta"
          <|> do
            ipSymbol ':'
            ipString "GAMMA"
            return "gamma"
          <|> do
            ipSymbol '!'
            return "omega"

parsesy :: IrkParser [String]
parsesy = do
            a <- rawsyms
            b <- parsesy
            return (a:b)
          <|> do
            ipEnd
            return []

hostnames :: IrkParser [String]
hostnames = do
                a <- ircpHostname
                b <- hostnames
                return (a:b)
            <|> do
                ipSymbol ':'
                n <- ircpNick
                ipEnd
                return [n]

testExpression :: (Show a) => IrkParser a -> String -> IO ()
testExpression p s = do
    putStrLn (s ++ " // " ++ (show (irkParse p s)))

main :: IO ()
main = do
    testExpression parsesy ":ALPHA:BETA"
    testExpression parsesy "!hahah"
    testExpression parsesy "!:BETA"
    testExpression parsesy "?"
    testExpression ircpNick "hellothere"
    testExpression ircpNick "hello12"
    testExpression ircpNick "12hello"
    testExpression ircpHostname "alpha.beta.gamma"
    testExpression ircpHostname "alpha.beta.gamma!"
    testExpression hostnames "alpha.beta.gamma.delta:hel13"
    testExpression ircpMessage "NOTICE AUTH :*** Looking up your hostname\r"
