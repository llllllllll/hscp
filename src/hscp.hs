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


main :: IO ()
main = do
    args <- getArgs
    hscp_poll (head args)


hscp_poll config_path = do
    config <- lines <$> readFile config_path
    let user_name = drop 5 (head config)
        pass = drop 9 (config!!1)
        host = drop 5 (config!!2)
        dir = drop 10 (config!!3)
        clone_dir = drop 15 (config!!4)
        poll_int = drop 13 (config!!5)
        ignored = (tail . drop 6) config
    cs <- filter (`notElem` ignored) <$> getDirectoryContents dir
    pid <- getProcessID
    setCurrentDirectory dir
    ts <- mapM getFileStatus cs
    let access_times = zip3 cs (map isDirectory ts) (map accessTime ts)
    hscp_poll' user_name pass host 
               dir clone_dir (read poll_int) ignored access_times
                       
        
hscp_poll' user_name pass host dir clone_dir poll_int ignored access_times = do
    setCurrentDirectory dir
    cs <- filter (`notElem` ignored) <$> getDirectoryContents dir
    ts <- mapM getFileStatus cs
    let access_times' = zip3 cs (map isDirectory ts) (map accessTime ts)
    mapM_ (\((a,b,c),(a',b',c')) -> 
               if c /= c' then if b 
                               then putStrLn ("Change found on dir " ++ a 
                                              ++ ", pushing!") >>
                               system ("scp -r " ++ a ++ " " 
                                       ++ user_name ++ "@" ++ host ++ ":" 
                                       ++ clone_dir ++ a) >> return ()
                               else putStrLn ("Change found on file " ++ a
                                              ++ ", pushing!") >>
                               system ("scp -r " ++ a ++ " " 
                                       ++ user_name ++ "@" ++ host ++ ":" 
                                       ++ clone_dir ++ a) >> return ()
               else putStrLn $ "No edits found on " ++ a ++ " this poll!") 
              $ zip access_times access_times'
    putStrLn "polling..."
    threadDelay poll_int
    hscp_poll' user_name pass host dir clone_dir poll_int ignored access_times'