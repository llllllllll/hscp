-- Copyright (c) 2013, Joe Jevnik
-- hscp version 1.0 8.2.2013
import System.Environment
import System.Directory
import System.Process
import System.FilePath
import System.Posix.Files
import System.Posix.Process
import System.Exit
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA
import Foreign.C.Types
import Data.Maybe
import Data.List

data PollNode = PollNode { file_name    :: FilePath
                         , edit_time    :: Foreign.C.Types.CTime
                         }

instance Eq PollNode where
    (==) a b = file_name a == file_name b
    (/=) a b = not $ a == b

-- Creates a PollNode with the name, isDirectory, and the modification time.
mk_poll_node :: (FilePath,Foreign.C.Types.CTime) -> PollNode
mk_poll_node (p,e) = PollNode p e

main :: IO ()
main = do
    args <- getArgs
    config <- (parse_config . lines) <$> readFile (head args)
    hscp_start_polling config

-- Starts the polling process.
hscp_start_polling :: (String,String,String,String,String,Int,[String]) -> IO ()
hscp_start_polling (user_name,pass,host,dir,clone_dir,poll_int,ignored) = do
    cs <- recurs_dir_conts dir ignored
    ts <- mapM getFileStatus cs
    setCurrentDirectory dir
    let polls = map mk_poll_node $ zip cs (map modificationTime ts)
    putStrLn "Innitial push!"
    system ("scp -r " ++ dir ++ " " ++ user_name ++ '@':host ++ 
                 ':':clone_dir) >> return ()
    putStrLn "polling..."
    threadDelay poll_int
    hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,polls)
    
hscp_poll :: (String,String,String,String,String,Int,[String],[PollNode]) 
          -> IO ()
hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,polls) = do
    setCurrentDirectory dir
    cs <- recurs_dir_conts dir ignored
    polls' <- get_new_polls cs polls
    mapM_ (attempt_scp_push user_name host clone_dir) $ polls'
    putStrLn "polling..."
    threadDelay poll_int
    hscp_poll (user_name,pass,host,dir,clone_dir,poll_int,ignored,map fst polls')

parse_config :: [String] -> (String,String,String,String,String,Int,[String])
parse_config config = ( drop 5 (head config)                        -- user_name
                      , drop 9 (config!!1)                          -- pass
                      , drop 5 (config!!2)                          -- host
                      , drop 10 (config!!3)                         -- dir
                      , drop 15 (config!!4)                         -- clone_dir
                      , read $ drop 13 (config!!5)                  -- poll_int
                      , "\\.":"\\.\\.":(drop 7 config :: [String])) -- ignored

get_new_polls :: [FilePath] -> [PollNode] -> IO [(PollNode,PollNode)]
get_new_polls cs polls = do
    ts <- mapM getFileStatus cs
    let polls' = map mk_poll_node 
               $ zip cs (map modificationTime ts)
    return [(p,fromMaybe (mk_poll_node (file_name p,0)) (find (==p) polls)) 
                | p <- polls']
get_filtered_contents :: [String] -> FilePath -> IO [FilePath]
get_filtered_contents ignored dir = 
    filter (\c -> not $ any (\i -> i =~ c :: Bool) ignored) 
               <$> getDirectoryContents dir

recurs_dir_conts :: FilePath -> [String] -> IO [FilePath]
recurs_dir_conts dir ignored = do
    setCurrentDirectory dir
    cs <- get_filtered_contents ignored dir
    ps <- mapM (\c -> do
                    let p = dir </> c

                    is_dir <- doesDirectoryExist p
                    if is_dir
                    then recurs_dir_conts p ignored
                    else return [p]) cs
    return $ concat ps

is_edited :: (PollNode,PollNode) -> Bool
is_edited (p,p') = not $ edit_time p == edit_time p'

attempt_scp_push :: String -> String -> FilePath -> (PollNode,PollNode) -> IO ()
attempt_scp_push user_name host clone_dir (p,p') = 
    if edit_time p /= edit_time p' 
    then let a = file_name p in
         putStrLn ("Change found on file " ++ a ++ ", pushing!") >>
         system ("scp " ++ a ++ " " ++ user_name ++ '@':host ++ 
                 ':':clone_dir </> a) >> return ()
    else return ()