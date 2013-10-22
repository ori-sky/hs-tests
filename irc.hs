import Network
import System.IO
import Text.Printf
import Data.List

nop :: IO ()
nop = sequence_ []

server = "irc.freenode.net"
port = 6667
nick = "hsbot"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h ("NICK " ++ nick)
    write h "USER hsbot 0 * :Haskell Bot"
    listen h

write :: Handle -> String -> IO ()
write h s = do
    hPrintf h "%s\r\n" s
    printf "-> %s\n" s

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t

    putStrLn s

    if op_ping s then do
        pong s
    else nop
  where
    forever a = a >> forever a

    op_ping x = "PING :" `isPrefixOf` x
    pong x = write h ("PONG " ++ (':' : drop 6 x))
    join x = write h ("JOIN " ++ x)
