-- Copyright (c) 2013, Joe Jevnik
-- hscp version 1.0 8.2.2013
import System.Environment
import System.Directory
import System.Process
import System.FilePath
import System.Posix.Process
import System.Posix.Files
import System.Exit
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA
import Data.Maybe
import Data.List
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as B

-- Data to represent a file being polled.
data PollNode = PollNode { file_name :: FilePath
                         , file_hash :: MD5Digest
                         } deriving Show

-- Equality of PollNodes is based only on file_name.
instance Eq PollNode where
    (==) a b = file_name a == file_name b
    (/=) a b = not $ a == b

-- Creates a PollNode with the name and a hash.
mk_poll_node :: (FilePath,MD5Digest) -> PollNode
mk_poll_node (p,e) = PollNode p e

-- Parses args and starts the hscp-daemon
main :: IO ()
main = do
    args <- getArgs
    config <- (parse_config . lines) <$> readFile (head args)
    forkProcess $ hscp_start_polling config
    exitImmediately ExitSuccess

-- Starts the polling process. 
hscp_start_polling :: (String,String,String,String,String,Int,[String]) -> IO ()
hscp_start_polling (user_name,pass,host,dir,clone_dir,poll_int,ignored) = do
    setFileCreationMask 0
    createSession
    setCurrentDirectory dir
   -- hClose stdout >> hClose stdin >> hClose stderr
    cs <- recurs_dir_conts dir ignored
    ts <- mapM B.readFile cs
    let polls = map mk_poll_node $ zip cs (map md5 ts)
    system ("scp -r " ++ dir ++ " " ++ user_name ++ '@':host ++ 
            ':':clone_dir) >> return ()
    threadDelay poll_int
    hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,polls)

-- Checks the polls and attempts the scp push the edited files.
hscp_poll :: (String,String,String,String,String,Int,[String],[PollNode]) 
          -> IO ()
hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,polls) = do
    setCurrentDirectory dir
    cs <- recurs_dir_conts dir ignored
    polls' <- get_new_polls cs polls
    mapM_ (attempt_scp_push user_name host clone_dir) $ polls'
    threadDelay poll_int
    hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,map fst polls')

-- Parses the config files lines into their fields. t
parse_config :: [String] -> (String,String,String,String,String,Int,[String])
parse_config config = ( drop 5 (head config)                           -- user_name
                      , drop 9 (config!!1)                             -- pass
                      , drop 5 (config!!2)                             -- host
                      , drop 10 (config!!3)                            -- dir
                      , drop 15 (config!!4)                            -- clone_dir
                      , read $ drop 13 (config!!5)                     -- poll_int
                      , "^\\.$":"^\\.\\.$":(drop 7 config :: [String]))-- ignored

-- Gets a list of [New PollNode,Old PollNode) to be used to check the times.
get_new_polls :: [FilePath] -> [PollNode] -> IO [(PollNode,PollNode)]
get_new_polls cs polls = do
    ts <- mapM B.readFile cs
    let polls' = map mk_poll_node $ zip cs (map md5 ts)
    return [(p,fromMaybe (mk_poll_node (file_name p,md5 B.empty)) (find (==p) polls))
                | p <- polls']

-- Gets the contents of dir that do not match any of the regex in ignored.
get_filtered_contents :: [String] -> FilePath -> IO [FilePath]
get_filtered_contents ignored dir = 
    filter (\c -> not $ any (\i -> c =~ i :: Bool) ignored) 
               <$> getDirectoryContents dir

-- Gets the recursive contents of dir that do not match any of the 
-- regex in ignored.
recurs_dir_conts :: FilePath -> [String] -> IO [FilePath]
recurs_dir_conts dir ignored = do
    cs <- get_filtered_contents ignored dir
    ps <- mapM (\c -> do
                    let p = dir </> c
                    is_dir <- doesDirectoryExist p
                    if is_dir
                    then recurs_dir_conts p ignored
                    else return [p]) cs
    return $ concat ps

-- Attempts to call scp to push the given PollNode if the edit_times 
-- are different.
attempt_scp_push :: String -> String -> FilePath -> (PollNode,PollNode) -> IO ()
attempt_scp_push user_name host clone_dir (p,p') = 
    if file_hash p /= file_hash p' -- Need to fix.
    then let a = file_name p in
         putStrLn "test" >> 
         system ("scp " ++ a ++ " " ++ user_name ++ '@':host ++ 
                 ':':clone_dir </> a) >> return ()
    else return () 