{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Types (StdMethod (..))
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getAddrInfo,
    listen,
    socket,
  )
import Network.Socket.ByteString (recv, send)
import Parser (ByteStringWithChars (..), requestParser, headerParser)
import Request (Header (UserAgentH), Req (..), URI (..), emptyReq)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Megaparsec (parse, many)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    -- Handle the clientSocket as needed...
    req <- readRequestLine clientSocket
    hs <- readHeaders clientSocket
    if method req == GET
      then do
        case uri req of
          Home -> do
            _ <- send clientSocket "HTTP/1.1 200 OK\r\n\r\n"
            close clientSocket
          Unknown _ -> do
            _ <- send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"
            close clientSocket
          Echo s -> do
            _ <- send clientSocket $ "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> (BC.pack . show . BC.length) s <> "\r\n\r\n" <> s
            close clientSocket
          UserAgent -> do
            case findUserAgent hs of
              Nothing -> do
                _ <- send clientSocket "HTTP/1.1 400 OK\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n"
                close clientSocket
              Just ua -> do
                _ <- send clientSocket $ "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> (BC.pack . show . BC.length) ua <> "\r\n\r\n" <> ua
                close clientSocket
      else do
        _ <- send clientSocket "HTTP/1.1 405 Method Not Allowed\r\n\r\n"
        close clientSocket

findUserAgent :: [Header] -> Maybe BC.ByteString
findUserAgent [] = Nothing
findUserAgent (UserAgentH ua : _) = Just ua
findUserAgent (_ : xs) = findUserAgent xs

getNextData :: Socket -> IO BC.ByteString
getNextData s = go s ""
  where
    go sInput acc = do
      next <- recv sInput 1
      let end = if acc == "" then next else BC.cons (BC.last acc) next
      if end == "\r\n" then return (acc <> next) else go s (acc <> next)

readRequestLine :: Socket -> IO Req
readRequestLine sock = do
    reqInput <- getNextData sock 
    either (const $ return emptyReq) return . parse requestParser "" . BS $ reqInput

readHeaders :: Socket -> IO [Header]
readHeaders sock = do
  hs <- readHeadersBytes sock
  either (const $ return []) return . parse (many headerParser) "" . BS $ hs
  where
    readHeadersBytes s = do
      header <- getNextData s
      if header == "\r\n"
        then return ""
        else (header <>) <$> readHeadersBytes s