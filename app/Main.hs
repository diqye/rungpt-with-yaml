import Mydefault
import AI.ChatGPT
import AI.Datas
import Control.Monad.Trans.Except
import OperatingEnvironment
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import Data.Aeson((.=),FromJSON,Value,ToJSON)
import System.IO
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Class(liftIO)
import Control.Monad(forM_,forM)
import Data.String(fromString)
import System.Console.Haskeline(runInputT,defaultSettings,getInputLine)
import System.Process
import Control.Concurrent
import BirthInfo 
import Data.String.Conversions(cs)
import Control.Applicative((<|>))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [filePath] <- getArgs
    options <- Y.decodeFileThrow filePath
    let model = options ^?! key "model" . _JSON'
    runJSON model options
getMyLine :: IO (Maybe String)
getMyLine = runInputT defaultSettings $ getInputLine "user> "

getMessages :: Value -> [Message]
getMessages config =  config  ^.. key "messages" . values . _JSON'

handleMessages :: [Message] -> IO [Message]
handleMessages msgs = do
    utcStr <- getCurrentUTCTime
    let handle x@(MsgSystem {}) = x & sys'content %~  modify where 
            modify = T.replace "$CURRENT_UTC" (cs utcStr)
        handle a = a
    pure $ map handle msgs

getOptions :: Value -> Value
getOptions config = config ^?! key "options"

handleError :: ToJSON a => Either a t -> (t -> IO ()) -> IO ()
handleError (Left error) _ = L.putStrLn $ A.encode error
handleError (Right v) action = action v

fromMaybeG :: Maybe a -> GPT a
fromMaybeG Nothing = liftExceptT $ throwE []
fromMaybeG (Just a) =pure a

functionCall :: String -> Value -> Text -> GPT Message
functionCall "get_astrology_info" args myid = do
    let argsBs = A.encode args
    liftIO $ do
        putStrLn "Calling get_astrology_info"
        L.putStrLn $ argsBs
    y <- fromMaybeG $ args ^? key "year" . _JSON'
    m <- fromMaybeG $ args ^? key "month" . _JSON'
    d <- fromMaybeG $ args ^? key "day" . _JSON'
    h <- fromMaybeG $ args ^? key "hour" . _JSON'
    minute <- fromMaybeG $ args ^? key "minute" . _JSON'
    p <- fromMaybeG $ args ^? key "province" . _JSON'
    c <- fromMaybeG $ args ^? key "city" . _JSON'
    content <- liftIO $ getAstrologyChart (y,m,d,h,minute) (p,c) BirthD
    liftIO $ do
        putStrLn "Response"
        print $ length content
    pure $ userWith "Function" (cs $ "Function get_astrology_info respond:\n" <> cs argsBs <> "\n" <> content)
    {-
    pure $ MsgTool {
        _tool'role = MessageRoleTool ,
        _tool'content = cs content ,
        _tool'name = "get_astrology_info" ,
        _tool'tool_call_id = myid
    }
    -}
functionCall "get_now_astrology_info" args myid = do
    let argsBs = A.encode args
    liftIO $ do
        putStrLn "Calling get_now_astrology_info"
        L.putStrLn $ argsBs
    (y,m,d,h,minute) <- liftIO getCurrentTimeZoneDigits
    p <- fromMaybeG $ args ^? key "province" . _JSON'
    c <- fromMaybeG $ args ^? key "city" . _JSON'
    content <- liftIO $ getAstrologyChart (y,m,d,h,minute) (p,c) NowD
    liftIO $ do
        putStrLn "Response"
        print $ length content
    pure $ userWith "Function" (cs $ "Function get_now_astrology_info respond:\n" <> cs argsBs <> "\n" <> content)
    {-
    pure $ MsgTool {
        _tool'role = MessageRoleTool ,
        _tool'content = cs content ,
        _tool'name = "get_now_astrology_info" ,
        _tool'tool_call_id = myid
    }
    -}
functionCall _ _ _ = do
    undefined 

customMsg (MsgCustom val) = pure val
customMsg _ = liftExceptT $ throwE ["custom msg error"]
runJSON :: Text -> Value -> IO ()
runJSON m@"qwen-turbo" config = do
    result <- runD $ aliChatWith m (getMessages config) $ getOptions config
    handleError result $ \ v -> do
        putStrLn $ v ^?!
            key "output" .
            key "choices" .
            nth 0 .
            key "message" .
            key "content" .
            _JSON'
        putStrLn "========Sumary qwen-turbo===="
        L.putStrLn $ A.encode $ v ^? key "usage"
runJSON m@"qwen-vl-plus" config = do
    result <- runD $ aliChatWith m (getMessages config) $ getOptions config
    handleError result $ \ v -> do
        putStrLn $ v ^?!
            key "output" .
            key "choices" .
            nth 0 .
            key "message" .
            key "content" .
            nth 0 .
            key "text" .
            _JSON'
        putStrLn "========Sumary qwen-vl-plus===="
        L.putStrLn $ A.encode $ v ^? key "usage"
runJSON "wanx-v1" config = printErr $ printAliImage config aliImageGenerate >> pure () where
    printErr gpt = do
        result <- runD gpt
        handleError result $ \ _ -> pure ()
runJSON "wanx-background-generation-v2" config = printErr $ printAliImage config aliBackgroundGenerate>> pure () where
    printErr gpt = do
        result <- runD gpt
        handleError result $ \ _ -> pure ()
runJSON model config | model `elem` ["tts-1","tts-1-hd"] = do
    let filePath = config ^?! key "outputPath" . _JSON'
    result <- run $ do
        mp3 <- speech config
        liftIO $ L.writeFile filePath mp3
    handleError result $ const (pure ())
runJSON model config | model `elem` ["dall-e-2","dall-e-3"] = do
    let body = config ^?! _JSON'
    result <- run $ do
        images <- imageGenerate body
        forM_ (images ^.. key "data" . values . key "url" . _JSON') (liftIO . putStrLn)
    handleError result (const $ pure ())
runJSON "wordart-semantic" config = do
    result <- runD $ printAliImage config aliSemantic
    handleError result (const $ pure ())
runJSON "wordart-texture" config = do
    result <- runD $ printAliImage config aliTexture
    handleError result (const $ pure ())
runJSON "speech-02" config = do
    result <- runD $ minimaxT2A config
    handleError result $ \ v -> do
        let audioFile = v ^?! key "audio_file" . _JSON'
        let subtitleFile = v ^?! key "subtitle_file" . _JSON'
        putStrLn audioFile
        putStrLn subtitleFile
runJSON model config = do
    result <- runD $ do
        let isREPL = config ^? key "repl" . _JSON'
        let msgs = getMessages config
        let fCall msgs chatObj loop = do
                respMsg <- fromMaybeG $ chatObj ^? chat_choices . ix 0 . choice_message
                valueMsg <- customMsg respMsg
                toolCalls <- fromMaybeG $  valueMsg ^. key "tool_calls" . _JSON'
                newMsgs <- forM (toolCalls::[Value]) $ \ tool -> do
                    name <- fromMaybeG $ tool ^? key "function" . key "name" . _JSON'
                    argStr <- fromMaybeG $ tool ^? key "function" .key "arguments" . _JSON'
                    id' <- fromMaybeG $ tool ^? key "id" . _JSON'
                    arg <- fromMaybeG $ A.decode $ cs (argStr :: String)
                    functionCall name arg id'
                -- loop $ msgs <> [MsgCustom $ valueMsg & key "content" .~ "called"] <> newMsgs
                loop $ msgs <> (newMsgs)
        let replActive messages chatObj loop = do
                let content = chatObj ^. chat_choices . ix 0 . choice_message . msg'content
                liftIO $ do
                    T.putStrLn content
                    putStrLn "========Sumary ===="
                    -- _ <- forkIO $ () <$ readProcess "say" [cs content] ""
                    L.putStrLn $ A.encode $ chatObj ^. chat_usage
                if isREPL == Just True then do
                    line <- liftIO $ do
                        putStr "\x1B[31m"
                        (Just line) <- getMyLine
                        putStr "\x1B[0m"
                        pure line
                    if line /= ":quit" then do
                        let userMsg = user $ fromString line
                        loop (messages <> [assistant content,userMsg])
                    else pure ()
                else pure ()
        let loop messages = do
                -- liftIO $ L.putStrLn $ A.encode messages
                msgs <- liftIO $ handleMessages messages
                chatObj <- chatWith model msgs (getOptions config)
                fCall msgs chatObj loop <|> replActive msgs chatObj loop
                    
        loop msgs
    handleError result (const $ pure ())

printAliImage :: Value -> (Value -> GPT Value) -> GPT ()
printAliImage config ali = do
    v <- ali config
    let taskId = v ^?! key "output" . key "task_id" . _JSON'
    let loop n = do
            liftIO $ threadDelay (1000000 * 3)
            liftIO $ putStr $ "\r第" ++ show n ++ "次尝试:"++ taskId 
            task <- aliTask taskId
            if task ^?! key "output" . key "task_status" . _JSON' `elem` ["SUCCEEDED","FAILED"] then do
                liftIO $ putStrLn ""
                forM_ (task ^.. key "output" . key "results" . values ) $ \v -> liftIO $ L.putStrLn $ A.encode v
            else loop (succ n)
    loop 1



