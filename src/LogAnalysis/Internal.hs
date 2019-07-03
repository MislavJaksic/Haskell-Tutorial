module LogAnalysis.Internal
  ( createLogMessage,
    toInt,
    getMsgType,
    getStamp,
    getDesc,
    getSeverity,
    errorDescOverFifty,
    isErrorMsg,
    isError,
    isFiftyOrMore
  )
  where

import LogAnalysis.Log



createLogMessage :: [String] -> LogMessage
createLogMessage ("I":stamp:rest) = LogMessage Info (toInt stamp) (unwords rest)
createLogMessage ("W":stamp:rest) = LogMessage Warning (toInt stamp) (unwords rest)
createLogMessage ("E":code:stamp:rest) = LogMessage (Error (toInt code)) (toInt stamp) (unwords rest)
createLogMessage rest = Unknown (unwords rest)

toInt :: String -> Int
toInt x = read x :: Int



getMsgType :: LogMessage -> MessageType
getMsgType (LogMessage msgType _ _) = msgType

getStamp :: LogMessage -> TimeStamp
getStamp (LogMessage _ stamp _) = stamp
getStamp _ = -1

getDesc :: LogMessage -> String
getDesc (LogMessage _ _ desc) = desc
getDesc (Unknown desc) = desc



getSeverity :: MessageType -> Int
getSeverity (Error sev) = sev
getSeverity _ = -1



errorDescOverFifty :: LogMessage -> String
errorDescOverFifty msg = if (isErrorMsg msg) && (isFiftyOrMore $ getSeverity $ getMsgType msg)
                     then getDesc msg
                     else ""

isErrorMsg :: LogMessage -> Bool
isErrorMsg (LogMessage msgType _ _)
  | isError msgType = True
  | otherwise = False
isErrorMsg _ = False
  
isError :: MessageType -> Bool
isError (Error _) = True
isError _ = False

isFiftyOrMore :: Int -> Bool
isFiftyOrMore x = x > 49