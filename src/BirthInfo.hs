module BirthInfo where

import Mydefault
import Control.Lens hiding ((.=),noneOf)
import Data.Aeson.Lens
import qualified HTTP.Myrequest as H
import qualified Data.Aeson as A
import Data.Aeson((.=),FromJSON,Value,ToJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as L
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.IO.Class(liftIO)
import Control.Monad(forM_)
import Data.String(fromString)
import Data.IORef
import System.IO.Unsafe
import Control.Applicative((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Data.List (intercalate)
import Data.String.Conversions(cs)
import qualified Data.Time as Time
import Control.Monad.Trans.Except



provinceRef :: IORef [(Text,Int)]
provinceRef = unsafePerformIO $ newIORef []

readProvinceData :: IO [(Text,Int)]
readProvinceData = do
    provinces <- readIORef provinceRef
    if null provinces then readData else pure provinces
    where readData :: IO [(Text,Int)]
          readData = do
            (Just xs) <- A.decodeFileStrict "data/province.json"
            let provinces = map trans xs
            writeIORef provinceRef provinces
            pure provinces
          trans :: Value -> (Text,Int)
          trans val = (val ^?! key "text" . _JSON', val ^?! key "val" . _JSON')

cityRef :: IORef [[(Text,Text)]]
cityRef = unsafePerformIO $ newIORef []

readCityData :: IO [[(Text,Text)]]
readCityData = do
    citys <- readIORef cityRef
    if null citys then readData else pure citys
    where readData :: IO [[(Text,Text)]]
          readData = do
            (Just citys) <- A.decodeFileStrict "data/city.json"
            writeIORef cityRef citys
            pure citys


newtype MyText = MyText Text deriving Show

instance Eq MyText where
    (MyText a) == (MyText b) = ab || ba where
        ab = T.isInfixOf a b
        ba = T.isInfixOf b a

queryCityCode :: Text -> Text -> ExceptT Text IO (Int,Text)
queryCityCode province city = do
    provinceList <- liftIO $ readProvinceData
    let foundProvince = lookup province provinceList
    let foundProvince' = lookup (MyText province) $ map (_1 %~ MyText) provinceList
    idx <- exceptFromMaybe (foundProvince <|> foundProvince') $ ptip provinceList
    cities <- liftIO $ readCityData
    foundCities <- exceptFromMaybe (cities ^? ix idx) $ "获取地址失败"
    cityItem <- exceptFromMaybe
        (lookup city foundCities <|> lookup (MyText city) (map (_1 %~ MyText) foundCities))
        (ctip foundCities)
    pure (idx,cityItem)
    where
        exceptFromMaybe a e = except $ maybe (Left e) Right a
        ptip xs = "省,取直只能是下列之一\n" <> format xs
        ctip xs = "市，取直只能是下列之一\n" <> format xs
        format xs = T.intercalate "\n" $ map fst xs
            
notString :: String -> Parser Char
notString str = do
    notFollowedBy $ string str
    anyChar
starParsec :: Parser String
starParsec = do
    skipMany $ notString "id=\"d1\""
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try trStarParsec
    spaces
    _ <- string "</tbody>"
    pure $ ("星位：\n" <>) $
        intercalate "\n" $
        map (\ (a,b,c) -> intercalate "|" [a,b,c]) $
        ("行星","星座","宫位"):stars 

starNowParsec :: Parser String
starNowParsec = do
    skipMany $ notString "id=\"d1\""
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try trStarParsec
    spaces
    _ <- string "</tbody>"
    pure $ 
        intercalate "\n" $
        map (\ (a,b,_) -> intercalate "" ["此刻",a,"在",b]) $ stars 
trStarParsec :: Parser (String,String,String)
trStarParsec = do
    _ <- string "<tr>"
    a <- tdaParsec
    b <- tdaParsec
    c <- tdaParsec
    skipMany $ notString "</tr>"
    _ <- string "</tr>"
    pure (a,b,c)
atagParsec = do
    _ <- string "<a"
    skipMany $ noneOf ">"
    _ <- anyChar
    str <- many1 $ notString "</a>"
    _ <- string "</a>"
    pure str
tdaParsec = do
    spaces
    _ <- string "<td"
    skipMany $ noneOf ">"
    _ <- anyChar
    strs <- many $ try atagParsec  <|> (many1 $ noneOf "<")
    _ <- string "</td>"
    spaces
    pure $ intercalate "" strs
trParsec :: Parser [String]
trParsec = do
    spaces
    _ <- string "<tr"
    skipMany $ noneOf ">"
    _ <- anyChar
    xs <- many $ try tdaParsec
    _ <- string "</tr>"
    spaces
    pure xs
houseParsec :: Parser String
houseParsec = do
    skipMany $ notString "id=\"d2\""
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try trParsec
    spaces
    _ <- string "</tbody>"
    pure $ ("\n宫位：\n" <>) $
        intercalate "\n" $
        map (intercalate "|") $
        ["宫位","宫始点","宫主星","宫神星"]:stars
phaseParsec :: Parser String
phaseParsec = do
    skipMany $ notString "id=\"d3\""
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try $ trParsec
    spaces
    _ <- string "</tbody>"
    pure $ ("\n相位：\n" <>) $
        intercalate "\n" $
        map (intercalate "|") $
        ["行星A","行星B","相位","容许度"]:stars

majorAxesParsec :: Parser String
majorAxesParsec = do
    skipMany $ notString "id=\"d4\""
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try $ trParsec
    spaces
    _ <- string "</tbody>"
    pure $ ("\n四大尖轴：\n" <>) $
        intercalate "\n" $
        map (intercalate "|") $
        ["四大尖轴","星座位置"]:stars

intersectionPointAxesParsec :: Parser String
intersectionPointAxesParsec = do
    skipMany $ notString "<tbody>"
    skipMany $ notString "<tr>"
    stars <- many1 $ try $ trParsec
    spaces
    _ <- string "</tbody>"
    pure $ ("\n交点：\n" <>) $
        intercalate "\n" $
        map (intercalate "|") $
        ["交点","所落星座","所落宫位"]:stars

allParsec :: Parser String
allParsec = do
    star <- starParsec
    house <- houseParsec
    phase <- phaseParsec
    major <- majorAxesParsec
    point <- intersectionPointAxesParsec
    pure $ intercalate "\n" $ [star,house,phase,major,point]

testMain :: IO ()
testMain = do
    html <- readFile "data/test.html"
    let a = parse allParsec "https://cn.astrodoor.cc/horoscope_result.jsp" html
    let content = either show id a
    putStrLn content
    putStrLn "hello"

type Birth = (Int,Int,Int,Int,Int)
type Addr = (Text,Text)
data Direction = BirthD | NowD deriving (Show,Eq)
getAstrologyChart :: Birth -> Addr -> Direction -> IO String
getAstrologyChart birth addr dir = do
    code <- runExceptT $ queryCityCode (addr ^. _1) (addr ^. _2)
    let reqFn :: (Int,Text) -> IO L.ByteString
        reqFn (p,ci) = H.request' $
            H.setRequestBodyForm [
                ("year",c $ show $ birth ^. _1),
                ("month",c $ show $ birth ^. _2),
                ("day",c $ show $ birth ^. _3),
                ("hour",c $ show $ birth ^. _4),
                ("minute",c $ show $ birth ^. _5),
                ("area",Just "0"),
                ("province", Just $ cs $ show p),
                ("city", Just $ cs ci),
                ("longitude",Just "E"),
                ("longitudeDegree",Just "114"),
                ("longitudeMinute",Just "20"),
                ("latitude",Just "N"),
                ("latitudeDegree",Just "34"),
                ("latitudeMinute",Just "49"),
                ("timezone",Just "-480"),
                ("showAsteroids",Just "Y"),
                ("minorAspect",Just "yes"),
                ("houseSystem",Just "P"),
                ("sun0",Just "10"),
                ("sun180",Just "10"),
                ("sun90",Just "10"),
                ("sun120",Just "10"),
                ("sun60",Just "6"),
                ("sunminor",Just "3"),
                ("moon0",Just "10"),
                ("moon180",Just "10"),
                ("moon90",Just "10"),
                ("moon120",Just "10"),
                ("moon60",Just "6"),
                ("moonminor",Just "3"),
                ("planet0",Just "7"),
                ("planet180",Just "7"),
                ("planet90",Just "7"),
                ("planet120",Just "7"),
                ("planet60",Just "6"),
                ("planetminor",Just "3")
            ] $
            H.setHeader ("User-Agent","Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36") $
            H.setHeader ("Content-Type", "application/x-www-form-urlencoded") $
            H.setHeader ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7") $
            H.setHeader ("Connection","keep-alive") $
            H.setHeader ("Origin", "https://cn.astrodoor.cc") $
            H.setHeader ("Referer","https://cn.astrodoor.cc/horoscope.jsp") $ 
            H.mpost $ "https://cn.astrodoor.cc/horoscope_result.jsp"
    let action :: Either Text (Int,Text) -> IO String
        action (Left text) = pure $ cs text
        action (Right pcity) = do
            bs <- reqFn pcity
            let parsec = if dir == NowD then starNowParsec else allParsec
            let a = parse parsec "https://cn.astrodoor.cc/horoscope_result.jsp" $ cs $ bs
            let content = either (const "解析天象失败") id a
            pure content
    action code
    where c = Just . cs 
    
getCurrentUTCTime :: IO String
getCurrentUTCTime = show <$> Time.getCurrentTime

getCurrentTimeZoneDigits :: IO (Int,Int,Int,Int,Int)
getCurrentTimeZoneDigits = do
    current <- Time.getCurrentTime
    let local = Time.utcToLocalTime (Time.hoursToTimeZone 8) current
    let (year, month, day) = (Time.toGregorian . Time.localDay) local
    let Time.TimeOfDay hour minute _ = Time.localTimeOfDay local
    pure (fromEnum year, fromEnum month, fromEnum day, fromEnum hour, fromEnum minute)