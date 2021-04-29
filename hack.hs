{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures #-}

import Database.SQLite.Simple
--import Data.Text.Lazy (Text, pack, unpack)
import Data.Int
import Data.Text
import qualified Database.SQLite3 as SQLite3
import Control.Exception
import Control.Concurrent

import Control.Applicative
import Data.Maybe (isJust, catMaybes)
import Data.List (takeWhile)
import Control.Monad (forever, forM, forM_, unless)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.IO.Unsafe
import Data.Function(fix)

hello = do
  conn <- open "test.db"
  let x = True
  let (q,n) = genN 99
  [[x::Int]] <- query conn q (Only n)
  print x

genQ :: Bool -> Query
genQ b = "select 2" <> (if b then " + 2" else "")

genN :: Int -> (Query,Int)
genN n = ("select 2 + ?",n)


data Person = Person {
    personId   :: Int64
    , personName :: Text
    , personAge  :: Text
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

-- when inserting a new Person, ignore personId. SQLite will provide it for us.
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

runPersonExample :: IO ()
runPersonExample = do
  conn <- open "test4.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
  --execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Justina" "15")
  --execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Jordi" "11")
  people <- query_ conn "SELECT id, name, age from people" :: IO [Person]

  --let x = True
  --let (q,n) =insertLite "people" "name" 3
  --execute conn q (Only n)
 -- executeNamed conn "UPDATE people SET parameter = value WHERE parameter = :parameter AND value = :value" [":parameter" := ("age" :: String), ":value" := ("3" :: String)]

  --executeNamed conn "INSERT INTO people (name, parameter) VALUES (Joe, value) WHERE parameter = :parameter AND value = :value" [":parameter" := ("age" :: String), ":value" := ("3" :: String)]
  --executeNamed conn "INSERT INTO people (name, parameter) VALUES (Joe, value)" ["parameter" := ("age" :: String), "value" := ("3" :: String)]
  execute conn "INSERT INTO people (?) VALUES (3)" (Only "age":: Only String)
  --executeNamed conn "INSERT INTO people (param1, param2) VALUES (val1, val2) WHERE param1 = :param1 AND param2 = :param2 AND val1 = :val1 AND val2 = :val2" [":param1" := ("name" :: String),":param2" := ("age" :: String),":val1" := ("Joe" :: String), ":val2" := ("3" :: String)]
  close conn
  print people

viewSQLite conn unevaluated_remote_accessor objectName = query conn "SELECT ? from ??"



insertLite :: String-> String-> Int -> (Query,Int)
--insertLite mytable parameters item = ("INSERT INTO " ++ mytable ++ " (" ++ parameters ++ ") VALUES (?)", item)

--insertLite mytable parameters item = 
    --(("INSERT INTO mytable (parameters) VALUES (?) WHERE mytable = :mytable AND parameters = :parameters" [":mytable" := mytable, ":parameters" := parameters]), item)

insertLite mytable parameters item = ("INSERT INTO people (age) VALUES (?)", item)

--insertLite2 :: String-> String-> Int -> _
--insertLite2 mytable parameters item = ("INSERT INTO people (parameter) VALUES (?) WHERE parameter = :parameter " [":parameter" := parameters] , item)


{-
error:
    • Couldn't match expected type ‘Only Int -> IO [[Int]]’
                  with actual type ‘IO [r0]’
    • The function ‘query_’ is applied to three arguments,
      but its type ‘Connection -> Query -> IO [r0]’ has only two
      In a stmt of a 'do' block: [[x :: Int]] <- query_ conn q (Only n)
      In the expression:
        do conn <- open "test.db"
           let x = True
           let (q, n) = genN 99
           [[x :: Int]] <- query_ conn q (Only n)
           ....
   |
11 |   [[x::Int]] <- query_ conn q (Only n)
-}


--https://stackoverflow.com/questions/5892314/sqlite-syntax-error

-- https://www.sqlite.org/lang_insert.html

-- http://hackage.haskell.org/package/postgresql-simple-named-0.0.0.0/docs/PgNamed.html


-- Attempt to use lowest level bindings.

specialExample::IO()
specialExample = do
    conn <- SQLite3.open "test4.db"
    --SQLite3.exec conn $ sqlite3_set_prototype "people" "age" "3"
    querydata <- sqlite3_get_prototype conn "people" "age"
    SQLite3.close conn
    print querydata

sqlite3_set_prototype :: String -> String -> String -> Text
sqlite3_set_prototype mytable parameters item = pack $ "INSERT INTO "++ mytable ++" ("++ parameters ++") VALUES ("++ item ++")"

sqlite3_get_prototype :: SQLite3.Database -> String -> String -> IO [(SQLite3.ColumnCount, [Text], [Maybe Text])] -- _
sqlite3_get_prototype conn mytable parameters = do
    let myquery = pack $ "SELECT " ++ parameters ++ " from " ++ mytable
    --SQLite3.execPrint conn myquery

    --let myfunction =  bracket (SQLite3.prepare conn myquery) SQLite3.finalize
    --myfunction SQLite3.columns

    --chan <- newTChan
    --let exec' sql = SQLite3.execWithCallback conn sql $ \c n v -> atomically $ writeTChan chan (c, n, v)

    -- let chan = (3::SQLite3.ColumnCount, ["Base"]::[Text], [(Just "Base")]::[Maybe Text])
    -- tha <- newTVarIO chan 
    -- let exec' sql = SQLite3.execWithCallback conn sql $ \c n v -> atomically $ writeTVar tha (c, n, v)
    -- exec' myquery

    chan2 <- newTChanIO -- (TChan(SQLite3.ColumnCount,[Text],[Maybe Text]))
    let exec'' sql = SQLite3.execWithCallback conn sql $ \c n v -> atomically $ writeTChan chan2 (c, n, v)
    exec'' myquery
    --stmt <- SQLite3.prepare conn myquery
    --SQLite3.Row <- SQLite3.step stmt
   -- res <- SQLite3.column stmt 0
    --SQLite3.Done <- SQLite3.step stmt
    --SQLite3.finalize stmt
    --return res
    let mylist = []
    --let makelist object = object : mylist
    --untilEmpty chan2 makelist
    --let extractJust = Data.List.takeWhile unless . isEmptyTChan
    --ch' <- fmap extractJust $ atomically $ readTChan chan2
    --forM_ ch' $ \x -> putStr (show x ++ ",")
    --untilEmpty chan2 print
    myempty chan2 mylist
    --let links = catMaybes $ Data.List.takeWhile isJust results
    --return links

myempty :: TChan a -> [a] -> IO [a]
myempty chan mylist = do
    empty <- atomically (isEmptyTChan chan)
    case empty of
        (True) -> pure(mylist)
        (False) -> do
            value <- atomically (readTChan chan)
            myempty chan (value : mylist)


untilEmpty :: TChan t -> (t -> IO a) -> IO ()
untilEmpty chan action = fix $ \next -> do
    empty <- atomically (isEmptyTChan chan)
    unless empty $ do
        value <- atomically (readTChan chan)
        action value
        next