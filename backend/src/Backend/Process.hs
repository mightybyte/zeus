{-# LANGUAGE OverloadedStrings #-}

module Backend.Process where

------------------------------------------------------------------------------
import           Control.Error
import qualified Control.Exception as C
import           Control.Monad.Except
import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           System.Exit
import           System.IO
import           System.Process
import qualified Turtle as Turtle
------------------------------------------------------------------------------
import           Common.Types.JobStatus
import           Common.Types.ProcMsg
------------------------------------------------------------------------------


exitCodeToStatus :: ExitCode -> JobStatus
exitCodeToStatus ExitSuccess = JobSucceeded
exitCodeToStatus (ExitFailure _) = JobFailed

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle fp action = withFile fp AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  action h

withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracket (createProcess_ fun c) cleanup
              (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
  where
    cleanup (_, _, _, ph) = terminateProcess ph

runCmd2
  :: String
  -> FilePath
  -> Maybe [(String, String)]
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ()
runCmd2 cmd dir envVars action = do
  let cp = (shell cmd)
        { cwd = Just dir
        , env = envVars
        }
  runCP cp action

runProc
  :: String
  -> [String]
  -> FilePath
  -> Maybe [(String, String)]
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ()
runProc cmd args dir envVars action = do
  let cp = (proc cmd args)
        { cwd = Just dir
        , env = envVars
        }
  runCP cp action

runCP
  :: CreateProcess
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ()
runCP cp action = do
  t <- liftIO getCurrentTime
  liftIO $ action $ ProcMsg t BuildCommandMsg (cmdSpecToText $ cmdspec cp)
  res <- liftIO $ C.try
    (Turtle.foldShell (Turtle.streamWithErr cp (return mempty)) (shellHandler action))
  case res of
    Left e -> ExceptT $ return $ Left e
    Right _ -> return ()

runCPStr
  :: CreateProcess
  -> (ProcMsg -> IO ())
  -> ExceptT String IO ()
runCPStr cp action = do
  res <- lift $ runExceptT $ runCP cp action
  case res of
    Left ec -> throwError $ "runCPStr failed with exit code " <> show ec
    Right _ -> return ()

cmdSpecToText :: CmdSpec -> Text
cmdSpecToText (ShellCommand s) = T.pack s
cmdSpecToText (RawCommand cmd args) = T.unwords $ T.pack cmd : map doArg args
  where
    doArg s = "\"" <> T.pack s <> "\""

shellHandler
  :: (ProcMsg -> IO ())
  -> Turtle.FoldShell (Either Turtle.Line Turtle.Line) ()
shellHandler action = Turtle.FoldShell step () return
  where
    step _ a = do
      t <- getCurrentTime
      let pm = case a of
                Left m -> ProcMsg t StderrMsg (Turtle.lineToText m)
                Right m -> ProcMsg t StdoutMsg (Turtle.lineToText m)
      action pm
