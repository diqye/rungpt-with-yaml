module OperatingEnvironment where

import Mydefault
import AI.Datas
import AI.ChatGPT
import qualified HTTP.Myrequest as H
import qualified Network.HTTP.Types.Header as H
import Data.Aeson(Value)
import System.Environment(getEnv)
import Data.Default.Class
import System.Directory
import qualified Data.ByteString.Lazy as L
import Data.String(fromString)
import Control.Monad.Trans.Except



mydef = def {
    _c_logHandle = \ (req,resp) -> do
        homeDir <- getHomeDirectory
        let logsDir = homeDir <> "/Library/Logs/rungpt/"
        createDirectoryIfMissing True logsDir
        let contentType = lookup H.hContentType $ H.responseHeaders resp
        let log = L.intercalate "\n" [
                    "=======Start=====",
                    -- "Request:",
                    fromString $ show $ H.getUri req,
                    -- "Reponse:",
                    fromString $ show contentType,
                    -- if contentType == Just "application/json" then 
                    --     H.responseBody resp
                    -- else "[Not a json]",
                    H.responseBody resp,
                    "=======End=======\n"
                ]
        L.appendFile (logsDir <> "request.log") log
}

run :: GPT a -> IO (Either [Value] a)
run gpt = do
    key <- getEnv "API_KEY"
    let config = mydef {
        _c_apiKey = key ,
        _c_baseUrl = "https://api.openai.com/v1/" ,
        _c_managerAction = H.newManagerWithSocksProxy ("127.0.0.1", 7890)
    }
    runGPT  gpt config


runD :: GPT a -> IO (Either [Value] a)
runD gpt = do
    key <- getEnv "API_KEY_DOUYU"
    groupid <- getEnv "M_GROUP_ID"
    let config = mydef {
        _c_apiKey = key ,
        _c_baseUrl = "https://oneapi.zmexing.com/v1/" ,
        _c_other = Just $ OtherConfig {
            _o_ali = Just (key,"https://dashscope.aliyuncs.com/api/v1/"),
            _o_minimax = Just (key,"https://api.minimax.chat/v1/",groupid)
        },
        _c_managerAction = H.newTlsManager
    }
    runGPT  gpt config
