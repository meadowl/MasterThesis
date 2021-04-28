{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Database.SQLite.Simple
import Data.Text.Lazy (Text, pack, unpack)
import Data.Int

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
  executeNamed conn "INSERT INTO people (name, parameter) VALUES (Joe, value)" ["parameter" := ("age" :: String), "value" := ("3" :: String)]
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