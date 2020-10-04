module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show,Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book 
  { author="Will Kurt"
  , title="Learn Haskell"
  , year=2017 }

myBookJSON :: BC.ByteString 
myBookJSON = encode myBook

data Name = Name
  { firstName :: T.Text 
  , lastName :: T.Text
  } deriving (Show)

instance FromJSON Name where
  parseJSON (Object v) = Name 
    <$> v .: "firstName"
    <*> v .: "error"

instance ToJSON Name where
  toJSON (Name firstName lastName) = object
    ["firstName" .= firstName, "lastName" .= lastName]

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage 
    <$> v .: "message" 
    <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object
    ["message" .= message, "error" .= errorCode]

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

data NOAAResult = NOAAResult 
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Float 
  , resultId :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) = NOAAResult
    <$> v .: "uid"
    <*> v .: "mindate"
    <*> v .: "maxdate"
    <*> v .: "name"
    <*> v .: "datacoverage"
    <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON res = object
    [ "uid" .= uid res
    , "mindate" .= mindate res
    , "maxdate" .= maxdate res
    , "name" .= name res
    , "datacoverage" .= datacoverage res
    , "id" .= resultId res
    ]

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int 
  } deriving (Show, Generic)

instance FromJSON Resultset
instance ToJSON Resultset

data Metadata = Metadata 
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left message) = print message
printResults (Right results) = do
  forM_ results (print . name)
  -- print name

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResutls = results <$> noaaResponse
  printResults noaaResutls

data IntList = Cons Int IntList | EmptyList 
  deriving (Show, Generic)

instance ToJSON IntList
instance FromJSON IntList

intListExample :: IntList
intListExample = Cons 1 (Cons 2 EmptyList)


   