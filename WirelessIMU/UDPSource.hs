module WirelessIMU.UDPSource (
    prodUDP
    ) where

import Network.Socket
import Control.Monad 
import Pipes

prodUDP :: PortNumber -> Int -> Producer String IO ()
prodUDP port bufSize = do
    s        <- lift $ socket AF_INET Datagram defaultProtocol
    bindAddr <- lift $ inet_addr "0.0.0.0"
    lift $ bindSocket s (SockAddrInet port bindAddr)
    forever $ do
        (msg,len,from) <- lift $ recvFrom s bufSize
        yield msg

