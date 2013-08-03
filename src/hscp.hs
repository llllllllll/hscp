-- Copyright (c) 2013, Joe Jevnik
-- hscp version 1.0 8.2.2013
import System.Environment
import System.Directory
import System.Process
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

data PollNode = PollNode { file_name    :: FilePath
                         , is_directory :: Bool
                         , edit_time    :: Foreign.C.Types.CTime
                         }


mk_poll_node :: (FilePath,Bool,Foreign.C.Types.CTime) -> PollNode
mk_poll_node (p,b,e) = PollNode p b e

main :: IO ()
main = do
    args <- getArgs
    hscp_start_polling (head args)

hscp_start_polling :: String -> IO ()
hscp_start_polling config_path = do
    config <- lines <$> readFile config_path
    let user_name = drop 5 (head config)
        pass = drop 9 (config!!1)
        host = drop 5 (config!!2)
        dir = drop 10 (config!!3)
        clone_dir = drop 15 (config!!4)
        poll_int = read $ drop 13 (config!!5)
        ignored = drop 7 config :: [String]
    cs <- filter (\c -> not $ any (\i -> i =~ c :: Bool) ignored) 
          <$> getDirectoryContents dir
    setCurrentDirectory dir
    ts <- mapM getFileStatus cs
    let polls = map mk_poll_node $ 
                zip3 cs (map isDirectory ts) (map modificationTime ts)
    putStrLn "Innitial push!"
    mapM_ (\p -> if is_directory p 
                 then let a = file_name p in
                      system ("scp -r " ++ a ++ " " 
                              ++ user_name ++ "@" ++ host ++ ":" 
                              ++ clone_dir ++ a) >> return ()
                 else let a = file_name p in
                      system ("scp -r " ++ a ++ " " 
                              ++ user_name ++ "@" ++ host ++ ":" 
                              ++ clone_dir ++ a) >> return ()) polls
    putStrLn "polling..."
    threadDelay poll_int
    hscp_poll user_name pass host dir clone_dir poll_int ignored polls
                           
hscp_poll user_name pass host dir clone_dir poll_int ignored polls = do
    setCurrentDirectory dir
    cs <- filter (\c -> not $ any (\i -> i =~ c :: Bool) ignored) 
          <$> getDirectoryContents dir
    ts <- mapM getFileStatus cs
    let polls' = map mk_poll_node 
                 $ zip3 cs (map isDirectory ts) (map modificationTime ts)
    mapM_ (\(p,p') -> 
               if edit_time p /= edit_time p' then 
                   if is_directory p 
                   then let a = file_name p in
                        putStrLn ("Change found on dir " ++ a 
                                  ++ ", pushing!") >>
                        system ("scp -r " ++ a ++ " " 
                                ++ user_name ++ "@" ++ host ++ ":" 
                                ++ clone_dir ++ a) >> return ()
                   else let a = file_name p in
                        putStrLn ("Change found on file " ++ a
                                  ++ ", pushing!") >>
                        system ("scp -r " ++ a ++ " " 
                                ++ user_name ++ "@" ++ host ++ ":" 
                                ++ clone_dir ++ a) >> return ()
               else putStrLn $ "No edits found on " ++ file_name p 
                        ++ " this poll!") 
              $ zip polls polls'
    putStrLn "polling..."
    threadDelay poll_int
    hscp_poll user_name pass host dir clone_dir poll_int ignored polls'