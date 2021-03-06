-- stack
-- package QuickCheck

{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, FlexibleInstances, RankNTypes #-}

module Main where

import Web.Scotty
import Network.JavaScript
import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson (Value, FromJSON)

-- Sqlite-simple
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types
import qualified Data.Text as T
--import Paths_javascript_bridge

import Data.Char
import System.Directory
import Control.Exception
import System.Environment
import Control.Concurrent
import System.IO
import System.IO.Unsafe
import Data.List
import Data.Int

import Test.QuickCheck

--main :: IO ()
--main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  -- dataDir <- return "." -- use for debugging
  scotty i $ do
    --middleware $ start app  
    middleware $ start exercise1
    get "/" $ file $ "Main.html"

app :: Engine -> IO ()
app eng = do
  send eng $ do
    command $ call "console.log" [string "starting..."]
    render "Hello!"
    --n <- multiplication 3.0 2.0
    --createObject
    initializeObjectAbstraction person test
    --remoteName <- getName
    --remoteNum <- getObjectAbstraction "person" "nest.extra"
    --a :: String <- procedure $ var remoteName
    --b :: Double <- procedure $ var remoteNum;
    --initializeObjectAbstraction "person" test
    let some_composite_field2 = nest >>> nest2 >>> extra3 
    d :: Double <- view some_composite_field2 person
    set some_composite_field2 "6" person

    e :: Double <- person ^. nest >>> nest2 >>> extra3 
    over some_composite_field2 (\x -> (x + 1::Double)) person
    
    f :: Double <- person ^. nest >>> nest2 >>> extra3 
-- Javascript is causing the initialized object to fall out of scope or something?
-- recreating the object command multiple times works, but then that means the object isn't working?

    --c <- getName2
    --createObject
    render $ "a" ++ " " ++ show d ++ " " ++ show e ++ " " ++ show f -- Meadow 3.0 6.0 7.0
    --render $ "3.0 * 2.0 = " ++ show n
    newperson <- initializeObjectAbstraction2 person test
    t2 :: Double <- view2 some_composite_field2 newperson
    set2 some_composite_field2 "6" newperson
    t3 :: Double <- view2 some_composite_field2 newperson
    over2 some_composite_field2 (\x -> (x + 1::Double)) newperson
    t4 :: Double <- view2 some_composite_field2 newperson

    t5 :: [RemoteMonad String] <- trav_view [some_composite_field2] newperson
    trav_set [some_composite_field2] "6" newperson
    t6 :: Double <- view2 some_composite_field2 newperson

    singular_over some_composite_field2 testfunction newperson
    g :: Double <-view2 some_composite_field2 newperson
    --t5 :: String <- trav_view [(\x -> show $ some_composite_field2 x), (\x -> x ++ ".id")] newperson
    render $ "b" ++ " " ++ show t2 ++ " " ++ show t3 ++ " " ++ show t4 ++ " " ++ show t6 ++ " " ++ show g-- ++ " " ++ show t5

-- when just using procedure for the getName, it would eliminate or stall object from scope
-- by the time taht getNum was executed.
-- later by changing it to a constructor, and then working around reconversions, grabbing both fields became possible

-- It is good practice to reflect the JavaScript utilties
-- you are using as typed Haskell functions.
render :: Command f => String -> f ()
render t = command $ call "jsb.render" [value t]

addition :: Procedure f => Double -> Double -> f Double
addition a b = procedure $ value a <> "+" <> value b

multiplication :: Procedure f => Double -> Double -> f Double
multiplication a b = procedure $ value a <> "*" <> value b

-- So, a is coerced to be the ToJson type of Double - rather it is assumed if 2.0 is statically inlined at the function call.
-- a is then put into the "Javascript" type via "value" that formats it as a "Text" type. (Fancier String Type)
-- <> mappened abreviation. mappened is built via foldr, with mempty as base case?
-- "*" is Text then.
-- text is merged together via mappened.
-- procedure , defined in internal.hs - at this point I lost the trail as to the details of what this is doing.
-- somehow Interhal.hs impliments remotemonad and a bunch of needed utility functions for Javascript.hs.

createObject :: Command f => f ()
createObject = command "var person = {name: \"Meadow\", id: 321}"

-- Makes basic Javascript Object with two fields.


getName2 :: (Monad f, Command f, Procedure f) => f String
getName2 = do
    v <- constructor $ JavaScript $ pack $ "person.name"
    procedure $ var v

getName :: Command f => f (RemoteValue String) 
getName = constructor "person.name"

getNum :: Command f => f (RemoteValue Double)
getNum = constructor "person.id"

-- "Lens of the Javascript world", uses standard object oriented field accessors

-- Have created a remote object, and lensed into its values.
-- Not sure how many transactions this caused, with hard coded attempt.
-- Command, constructor, and procedure should cause one transaction per each time executed?


--  Attempt to automate the managing of a remote object, broke my code.

data ValidObject where
    Generate :: String -> ValidObject
    Holder :: forall b. Show b => String -> b -> ValidObject
    Nested :: String -> [ValidObject] -> ValidObject

instance Show ValidObject where
    show (Holder a b) = a ++ ": " ++ (show b)
    show (Generate a) = a
    show (Nested a b) = a ++ ": {" ++ sortValidObjects b ++ "}"

test :: [ValidObject]
test = [ (Holder "name" "Meadow"), (Holder "id" 321)] ++ [(Nested "nest" [(Holder "extra" 1), (Holder "extra2" 2), (Nested "nest2" [(Holder "extra3" 3)])]), (Holder "name2" "Lyndon") ]

sortValidObjects :: [ValidObject] -> String
sortValidObjects (validObject:[]) = show validObject
sortValidObjects (validObject:rest) = show validObject ++ ", " ++ sortValidObjects(rest)

createObjectAbstraction :: String -> [ValidObject] -> String
createObjectAbstraction objectName validObjects = "window." ++ objectName ++ " = {" ++ sortValidObjects(validObjects) ++ "}"  -- used to use Var instead of Window, this caused all errors

initializeObjectAbstraction :: forall f. Command f => String -> [ValidObject] -> f ()
initializeObjectAbstraction objectName validObjects = command $ JavaScript $ pack $ createObjectAbstraction objectName validObjects

--

getObjectAbstraction :: forall f. Command f => String -> String -> f (RemoteValue Value)
getObjectAbstraction objectName fieldAccessor = constructor $ JavaScript $ pack $ objectName ++ "." ++ fieldAccessor

person :: String
person = "person"
some_remote_field1 = "nest"
some_remote_field2 = "nest2"
some_remote_field3 = "extra3"

nest objectName = objectName ++ ".nest"
nest2 objectName = objectName ++ ".nest2"
extra3 objectName = objectName ++ ".extra3"
--fucntion view = view(objectName) ++ ".extra3"

--test2 = new_3.new_2.new_1 "person"
test3 = person ^. nest >>> nest2 >>> extra3 

infixr 8 >>>
(>>>) = compose
compose x y = y.x

infixl 5 ^.
(^.) = apps
apps arg function = view function arg

--compose x y = y.x

-- " >>> " from Control.Arrow
--compose wrapper1 (compose wrapper2 wrapper3)
--(@@) = compose
--infix 8 @@
--f = wrapper1<>wrapper2<>wrapper3
--new_f = wrapper3.wrapper2.wrapper1


--compose_remote_field orignal_accessor additional_accessor = orignal_accessor ++ "." ++ additional_accessor

--some_composite_field1 :: String -> String
--some_composite_field1 object_name = (compose_remote_field (compose_remote_field (compose_remote_field object_name some_remote_field1)some_remote_field2) some_remote_field3)

view :: (Monad m, Command m, Procedure m, FromJSON b) => (String -> String) -> String -> m b
view unevaluated_remote_accessor objectName = do
    g <- constructor $ JavaScript $ pack $ unevaluated_remote_accessor objectName
    procedure $ var g

-- getObjectAbstraction objectName fieldAccessor = constructor $ value $ objectName ++ "." ++ fieldAccessor

-- Alright. I have no idea why my modification worked. I legit just used the first function in Data.Text.Lazy for conversion...


-- setPateronTitle = set' item_name "Twice Married" semiramis

set :: Command f => (String -> String) -> String -> String -> f (RemoteValue a)
set unevaluated_remote_accessor new_item objectName = constructor $ JavaScript $ pack $ (unevaluated_remote_accessor objectName) ++ " = " ++ new_item

over:: (Monad m, Command m, Procedure m, FromJSON t, Show a) =>
    (String -> String) -> (t -> a) -> String -> m (RemoteValue a)
over unevaluated_remote_accessor my_function objectName = do
    item <- view unevaluated_remote_accessor objectName
    let new_item = show $ my_function item
    set unevaluated_remote_accessor new_item objectName


-- So your typical lens, operates and contains some method to act on a record object or such in Haskell
-- It relies on you having the actual physical structure passed around.
-- Everything is remote, so you can only pass around the saved meta inforamiton, not the ability to directly hold that meta structure?

-- var_text :: RemoteValue a -> String
-- var_text (RemoteValue n) = "jsb.rs[" <> (show n) <> "]"
-- defined in Internal

initializeObjectAbstraction2 :: forall f a. Command f => String -> [ValidObject] -> f (RemoteValue a)
initializeObjectAbstraction2 objectName validObjects = constructor $ JavaScript $ pack $ objectName ++ " = {" ++ sortValidObjects(validObjects) ++ "}"--"var " ++ objectName ++ " = {" ++ sortValidObjects(validObjects) ++ "}"

--view2 :: (Monad m, Command m, Procedure m, FromJSON b) => (String -> String) -> (RemoteValue a) -> m b
view2 :: (FromJSON b) => (String -> String) -> (RemoteValue a) -> (RemoteMonad b)
view2 unevaluated_remote_accessor objectName = do
    g <- constructor $ JavaScript $ pack $ unevaluated_remote_accessor (var_text objectName)
    procedure $ var g

set2 :: (String -> String) -> String -> (RemoteValue a) -> RemoteMonad (RemoteValue a)
set2 unevaluated_remote_accessor new_item objectName = constructor $ JavaScript $ pack $ (unevaluated_remote_accessor (var_text objectName)) ++ " = " ++ new_item

over2 :: (FromJSON t, Show a) =>
    (String -> String) -> (t -> a) -> (RemoteValue a) -> RemoteMonad (RemoteValue a)
over2 unevaluated_remote_accessor my_function objectName = do
    item <- view2 unevaluated_remote_accessor objectName
    let new_item = show $ my_function item
    set2 unevaluated_remote_accessor new_item objectName

testfunction :: String
testfunction = "function testfunction(a) { return a * 2; };"

load_function :: Command f => String -> f (RemoteValue a)
load_function my_function = constructor $ JavaScript $ pack $ my_function

static_load_function :: String -> String
static_load_function my_function = "jsb.rs[1] = " ++ my_function

unloaded_over :: (Monad m, Command m) =>
    (String -> String) -> String -> RemoteValue a1 -> m (RemoteValue a2)
unloaded_over unevaluated_remote_accessor my_function objectName = do
    function_location <- load_function my_function
    let textPointerToFunction = var_text function_location
    let myobject = unevaluated_remote_accessor (var_text objectName)
    let applyobject = myobject ++ " = " ++ textPointerToFunction ++ "(" ++ myobject ++ ")"
    constructor $ JavaScript $ pack $ applyobject

loaded_over :: (Monad m, Command m) =>
    (String -> String) -> RemoteValue a -> RemoteValue a1 -> m (RemoteValue a2)
loaded_over unevaluated_remote_accessor function_location objectName = do
    let textPointerToFunction = var_text function_location
    let myobject = unevaluated_remote_accessor (var_text objectName)
    let applyobject = myobject ++ " = " ++ textPointerToFunction ++ "(" ++ myobject ++ ")"
    constructor $ JavaScript $ pack $ applyobject

singular_over :: (Monad m, Command m) =>
    (String -> String) -> String -> RemoteValue a1 -> m (RemoteValue a2)
singular_over unevaluated_remote_accessor my_function objectName = do
    let applyfunction = static_load_function my_function
    let textPointerToFunction = "jsb.rs[1]"
    let myobject = unevaluated_remote_accessor (var_text objectName)
    let applyobject = myobject ++ " = " ++ textPointerToFunction ++ "(" ++ myobject ++ ")"
    constructor $ JavaScript $ pack $ applyfunction ++ ";" ++ applyobject

-- I need to initialize the function, and know where it is.
-- I can't just declare it, with the array system that is used.
-- So I put the function in some array spot
-- Then I call the function from that stored array spot.
-- But Haskell still has to act and determine where it had put the array 
-- It eliminates the Haskell side computation of the function
-- But now has Haskell compute where the function was placed

-- I could delcare a global variable or reserve a spot in the array just to hold a single function, so that haskell /knows/ where the object is
-- But then, that would make you have to reload a new function in that spot at every time you wanted to use a new one.
-- Making it impossible to store and reuse old functions, causing computational costs on remaking that old function every time you wanted to use it.
-- And it is easy to imagine scenarios where you might want to :
-- use function1 then function2 then function1 again on different parts of the objects.

-- Also this makes a dependency on that spot being able to exist/ not be modified away by any other proecess in the meantime.
-- Which arguably since this is Javascript - could happen anyway at anywhere since it never allows you to know if a monkey has gone and moved a piece away.
-- I.E. both the object at location x, and the function at location y - are not actually guarenteed to be there.
-- so then making the depedency at location 1, isn't anymore dangerous of being overidden.

-- Still this introduces a problem of having to always reserve space for location 1.
-- And doesn't provide away to manage multiple functions at once.
-- Could always expand the storage, but as with registers, you end up reaching a limit to what you can reasonably allocate.
-- Since you have no way then of automatically knowing on the javascript side what the user wants to invoke.
-- You'd still need the method to capture the locations of each function
-- And let the Haskell end user decide when and where each function was invocated - which means the transaction problem wouldn't be resolved.

-- So, limiting one spot in the array to holding a singular function, would solve the problem but limit what you can do on the haskell end substantially
-- Plus, every new function would have to keep being reloaded into that single spot.

over3_fulljavascriptaccess :: (Monad m, Command m, Procedure m, Show a) =>
    (String -> String) -> String -> (RemoteValue a) -> m (RemoteValue a)
over3_fulljavascriptaccess unevaluated_remote_accessor my_function objectName = do
    let myobject = unevaluated_remote_accessor (var_text objectName)
    let applyobject = myobject ++ " = " ++ "jsb.rs[33](" ++ myobject ++ ")"
    constructor $ JavaScript $ pack $ my_function
    constructor $ JavaScript $ pack $ applyobject

tester :: [(String -> String)] -> (RemoteValue a) -> [JavaScript]
tester listStringFunctions objectName = JavaScript <$> (pack <$> (listStringFunctions <*> pure (var_text objectName)))

tester2 :: forall a f . Command f => [JavaScript] -> [f (RemoteValue a)]
tester2 javalist =  constructor <$> javalist

new_view :: (FromJSON b) => [(String -> String)] -> (RemoteValue a) -> RemoteMonad [b]
new_view list obj = case list of
    (x:[]) -> do
        z <- view2 x obj
        pure $ [z]
    (x:xs) -> do
        y <- new_view xs obj
        z <- view2 x obj
        pure $ z : y

trav_view :: (FromJSON b) => [(String -> String)] -> RemoteValue a -> RemoteMonad [RemoteMonad b]
trav_view list obj = case list of
    x:[] -> do
        pure [view2 x obj]
    x:xs -> do
        y <- trav_view xs obj
        pure $ [view2 x obj] ++ y

trav_set :: [(String -> String)] -> String -> (RemoteValue a) -> RemoteMonad (RemoteValue a)
trav_set list item obj = case list of
    x:[] -> do
        set2 x item obj
    x:xs -> do
        trav_set xs item obj
        set2 x item obj

trav_over:: (FromJSON t, Show a) =>
    [(String -> String)] -> (t -> a) -> (RemoteValue a) -> RemoteMonad (RemoteValue a)
trav_over list my_function obj = case list of
    x:[] -> do
        item <- view2 x obj
        let new_item = show $ my_function item
        set2 x new_item obj
    x:xs -> do
        trav_over xs my_function obj
        item <- view2 x obj
        let new_item = show $ my_function item
        set2 x new_item obj


{-
traversalview :: (Monad m, Command m, Procedure m, FromJSON b) => [(String -> String)] -> (RemoteValue a) -> [m b] -- (String -> String) for _
traversalview listStringFunctions objectName = do
    g <- constructor <$> tester listStringFunctions objectName
    pure procedure <*> (pure var <*> g)

-}
{-
traversalview :: (Monad m, Command m, Procedure m, FromJSON b) => _ -> (RemoteValue a) -> m b -- (String -> String) for _
traversalview unevaluated_remote_accessor objectName = do
    g <- constructor $ JavaScript $ pack $ unevaluated_remote_accessor (var_text objectName)
    procedure $ var g
-}

-- so for javascript traversals, it requires looking up either each object directly, or itterating through each item in a loop.

{-

type MyTraversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

instance Applicative Identity where
    pure x = Identity x
    (Identity y) <*> (Identity z) = Identity (y z)

instance Monoid a => Applicative (Const a) where
    pure x = Const mempty
    (Const vf) <*> (Const va) = Const (vf `mappend` va)

set'' :: forall s a. MyTraversal s a -> (a -> s -> s)
set'' ln x s = runIdentity (ln (set_fld x) s)

view'' :: forall s a. Monoid a => MyTraversal s a -> (s -> a)
view'' ln s = runConst (ln Const s)

over'' :: MyTraversal s a -> (a -> a) -> s -> s
over'' ln f s = runIdentity (ln (Identity . f) s)

title_nation :: MyTraversal Creation String
title_nation some_function (Creation e t n r a) = pure (\t' n' -> Creation e t' n' r a) <*>
    (some_function t)<*>
    (some_function n)

-}

-- Found "Language.Sunroof" after a google search.
-- Ironicially or unexpectedly a projected headed by Dr. Gill
-- Solves the problems of generating /Javascript/ strings.
-- Will just import this libarary if allowed and it is functional.

-- Need help to import, on modifying src to include it.
-- Will ask him if/when this becomes relevant.

data Java where
  Num :: Int -> Java
  Plus :: Java -> Java -> Java
  Minus :: Java -> Java -> Java
  Mult :: Java -> Java -> Java
  Div :: Java -> Java -> Java
  Boolean :: Bool -> Java
  And :: Java -> Java -> Java
  Or :: Java -> Java -> Java
  If :: Java -> Java -> Java -> Java
  Return :: Java -> Java
  deriving (Show,Eq)

buildFunction :: Java -> (Maybe String)
buildFunction (Num x) = Just $ show x
buildFunction (Plus x y) = do
    l <- buildFunction x;
    r <- buildFunction y;
    return $ l ++ " + " ++ r
buildFunction (Minus x y) = do
    l <- buildFunction x;
    r <- buildFunction y;
    return $ l ++ " - " ++ r
buildFunction (Mult x y) = do
    l <- buildFunction x;
    r <- buildFunction y;
    return $ l ++ " * " ++ r
buildFunction (Div x y) = do
    l <- buildFunction x;
    r <- buildFunction y;
    return $ l ++ " / " ++ r
-- "function testfunction(a) { return a * 2; };"


-- SideNote initializeObjectAbstraction is obsolete vs initializeObjectAbstraction2's better usage of remote monad
-- Remove from code/comment out

-- PYTHON BRIDGE
-- Build Executable Python Code From Haskell for Remote Lensing
-- Tasks to mirror:
-- Creat a Python data structure of a sort - perhaps an array.
-- Then add code to modify the data structure.
data ValidPythonObject where
    PythonGenerate :: String -> ValidPythonObject
    PythonHolder :: forall b. Show b => String -> b -> ValidPythonObject
    PythonNested :: String -> [ValidPythonObject] -> ValidPythonObject

instance Show ValidPythonObject where
    show (PythonHolder a b) ="'"++ a ++"'" ++ ": " ++ (show b)
    show (PythonGenerate a) ="'"++ a ++"'"
    show (PythonNested a b) ="'"++ a ++"'" ++ ": {" ++ sortValidPythonObjects b ++ "}"

testPython :: [ValidPythonObject]
testPython = [ (PythonHolder "name" "Meadow"), (PythonHolder "id" 321)] ++ [(PythonNested "nest" [(PythonHolder "extra" 1), (PythonHolder "extra2" 2), (PythonNested "nest2" [(PythonHolder "extra3" 3)])]), (PythonHolder "name2" "Lyndon") ]

sortValidPythonObjects :: [ValidPythonObject] -> String
sortValidPythonObjects (validObject:[]) = show validObject
sortValidPythonObjects (validObject:rest) = show validObject ++ ", " ++ sortValidPythonObjects(rest)

initializeObjectAbstractionPython :: String -> [ValidPythonObject] -> String
initializeObjectAbstractionPython objectName validPythonObject = objectName ++ " = {" ++ sortValidPythonObjects(validPythonObject) ++ "}"


nestPyth objectName = objectName ++ "['nest']"
nest2Pyth objectName = objectName ++ "['nest2']"
extra3Pyth objectName = objectName ++ "['extra3']"

-- nestPyth >>> nest2Pyth >>> extra3Pyth 

my_python_function = "def doub(u): return (2 * u)\n"

-- D['emp1']['name']
viewPython :: (String->String) -> String -> String
viewPython unevaluated_remote_accessor objectName = unevaluated_remote_accessor objectName 

--D['emp3']['name'] = 'Max'
setPython :: (String->String) -> String -> String -> String 
setPython unevaluated_remote_accessor new_item objectName = (unevaluated_remote_accessor objectName) ++ " = " ++ new_item

overPython :: (String -> String) -> String -> String -> String 
overPython unevaluated_remote_accessor my_function objectName = do
    let item = viewPython unevaluated_remote_accessor objectName
    let new_item = my_function ++ "(" ++ item ++ ")"
    setPython unevaluated_remote_accessor new_item objectName


createPythonFile :: [String] -> String -> IO()
createPythonFile myData myFileName = (try (createDirectoryIfMissing False ("GeneratedPython")) :: IO (Either SomeException ())) >>= \result ->
                                     case result of
                                        Right _ ->  let myFile = myFileName ++ ".py" in 
                                                    (putStr (myFile ++ "\n")) >>= \t1 ->
                                                    getCurrentDirectory >>= \current ->
                                                    let myFilePath = current ++ "/" ++ "GeneratedPython" ++ "/" ++ myFile in
                                                    (putStr (myFilePath ++ "\n")) >>= \t2 ->
                                                    (try (writeFile myFilePath $ unlines myData) :: IO (Either SomeException ())) >>= \y -> pure ()
                                        Left _ -> pure ()
                                    
-- Rules of Lenses

-- Get-Put
-- Forall S. put S (get S) = S 
-- forall $ \whole -> set l (view l whole) whole == whole
-- over2 some_composite_field2 (\x -> (x + 1::Double)) newperson
prop_get_put x = x

-- Put-Get
-- Forall S V. get (put s v) v
-- forall $ \whole part -> view l (set l part whole) == part

-- Put-Put
-- Forall S V v. put (put s v) V = put s V
-- forall $ \whole part1 part2 -> set l part2 (set l part1 whole) = set l part2 whole

-- Code Taken from Websites for Learning Quick Check
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

instance Show (Int -> Char) where
  show _ = "Function: (Int -> Char)"

instance Show (Char -> Maybe Double) where
  show _ = "Function: (Char -> Maybe Double)"

prop_MapMap :: (Int -> Char) -> (Char -> Maybe Double) -> [Int] -> Bool
prop_MapMap f g xs = map g (map f xs) == map (g . f) xs


--view2 :: (Monad m, Command m, Procedure m, FromJSON b) => (String -> String) -> (RemoteValue a) -> m b
instance Show (String -> String) where
    show _ = "Function: (String -> String)"

instance Arbitrary (RemoteValue Int) where
    arbitrary = do
        x <- elements [1..1000]
        return $ create_var x

-- More Cracking Open Internal.HS
-- create_var :: Int -> RemoteValue Int
-- create_var a = (RemoteValue a)

-- ((M Primitive) Bool)
instance Testable (RemoteMonad Bool) where
    --property (RemoteMonad True) = property True
    --property (RemoteMonad False) = property False
    property (RemoteMonad a) = property True

-- I would like to write something along the lines of:
-- property RemoteMonad True = propery True
-- property RemoteMonad False = property True
-- Error Shown Below from Haskell

{-|
 error:
    • Couldn't match expected type ‘M Primitive Bool’
                  with actual type ‘Bool’
    • In the pattern: False
      In the pattern: RemoteMonad False
      In an equation for ‘property’:
          property (RemoteMonad False) = property False
    |
556 |     property (RemoteMonad False) = property False

-}

prop_test :: (String -> String) -> (RemoteValue Int) -> (RemoteMonad Bool)
prop_test fieldAccessor remoteObject = do
    x :: String <- view2 fieldAccessor remoteObject
    pure $ x == x

{-|

*Main> quickCheck prop_test

<interactive>:7:1: error:
    • No instance for (Arbitrary (RemoteValue a0))
        arising from a use of ‘quickCheck’
    • In the expression: quickCheck prop_test
      In an equation for ‘it’: it = quickCheck prop_test

-}

{-|

*Main> quickCheck prop_test

<interactive>:4:1: error:
    • No instance for (Testable (RemoteMonad Bool))
        arising from a use of ‘quickCheck’
    • In the expression: quickCheck prop_test
      In an equation for ‘it’: it = quickCheck prop_test

-}

----------------------------------------
-- FP Complete Lens Exercise 1
----------------------------------------

address_ :: String -> String -> [ValidObject]
address_ street city = [ (Holder "street" street), (Holder "city" city) ]

people_ :: String -> String -> String -> Int -> [ValidObject]
people_ street city name age = [ (Holder "name" name), (Nested "address" (address_ street city)), (Holder "age" age) ]

address objectName = objectName ++ ".address"
street objectName = objectName ++ ".street"
city objectName = objectName ++ ".city"
name objectName = objectName ++ ".name"
age objectName = objectName ++ ".age"

alice :: [ValidObject]
alice = people_ "Hollywood" "Los Angeles" "Alice" 30

wilshire :: String
wilshire = "\"Wilshire Blvd\"" -- Remember to use \" for javascript string

aliceWilshire :: RemoteValue a -> RemoteMonad (RemoteValue a)
aliceWilshire newperson = set2 (address >>> street) wilshire newperson

getStreet :: RemoteValue a -> (RemoteMonad String)
getStreet newperson = view2 (address >>> street) newperson

birthday :: RemoteValue Int -> RemoteMonad (RemoteValue Int)
birthday newperson = over2 age (\x -> (x + 1::Int)) newperson

getAge :: RemoteValue a -> (RemoteMonad Int)
getAge newperson = view2 age newperson

exercise1 :: Engine -> IO ()
exercise1 eng = do
  send eng $ do
    command $ call "console.log" [string "starting..."]
    render $ "Exercise1"

    newperson <- initializeObjectAbstraction2 "alice" alice
    aliceWilshire newperson

    mystreet_person <- getStreet newperson
    birthday newperson
    herNewAge <- getAge newperson

    render $ "mystreet_person: " ++ mystreet_person
    render $ "herNewAge: " ++ show herNewAge
  send eng $ render $ "Exercise1"

-- Illistrative Example

-- Need exmaple with work, and without my work
-- Compeling example, why it makes things less work
-- Query and Updates - SQL

-- notice not possible to create new object
-- remote lensing requires live objects to generate
-- and does not create a new object, javascript modifies the existing objects

-- Lenses are a solution to bijective transformations
-- And a method of dealing with dictionary problems

-- Write a document 10 to 12 pages
-- Short Version of Thesis
-- Writing summary
-- Write down things of importance

-- Tell story with what I've done


{-|

https://www.fpcomplete.com/haskell/tutorial/lens/

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person
aliceWilshire = set (address.street) wilshire alice

getStreet :: Person -> Text
getStreet = view (address.street)
--getStreet = (^. address.street)

-- | Increase age by 1
birthday :: Person -> Person
birthday = over age (+ 1)
--birthday = age %~ (+ 1)

getAge :: Person -> Int
getAge = view age

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1

-}

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn

--http://www.mchaver.com/posts/2017-06-28-sqlite-simple.html

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
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Justina" "15")
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Jordi" "11")
  people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
  close conn
  print people

--insertLite :: Connection -> String -> String -> Only String -> IO ()
--insertLite conn mytable parameters item = execute conn ("INSERT INTO mytable (parameters) VALUES (?) WHERE mytable = :mytable AND parameters = :parameters" [":mytable" := mytable, ":parameters" := parameters]) item

-- Write the theoretic method
-- Give two or three examples
-- And show how they're working

--viewSQLite :: (String->String) -> _ -> _
--viewSQLite unevaluated_remote_accessor objectName = do
--    query_ conn "SELECT id, name, age from people"

--setSQLite :: (String->String) -> String -> Connection -> IO()
--setSQLite unevaluated_remote_accessor new_item conn = insertLite conn "person" "name" (Only "Jackson")

    --execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Justina" "15")

--overSQLite :: (String -> String) -> _ -> _ -> _ 
--overSQLite unevaluated_remote_accessor my_function objectName = do
--    item <- viewSQLite unevaluated_remote_accessor objectName
--    let new_item = show $ my_function item
--    setSQLite unevaluated_remote_accessor new_item objectName

{-
view2 :: (Monad m, Command m, Procedure m, FromJSON b) => (String -> String) -> (RemoteValue a) -> m b
view2 unevaluated_remote_accessor objectName = do
    g <- constructor $ JavaScript $ pack $ unevaluated_remote_accessor (var_text objectName)
    procedure $ var g

set2 :: Command f => (String -> String) -> String -> (RemoteValue a) -> f (RemoteValue a)
set2 unevaluated_remote_accessor new_item objectName = constructor $ JavaScript $ pack $ (unevaluated_remote_accessor (var_text objectName)) ++ " = " ++ new_item

over2 :: (Monad m, Command m, Procedure m, FromJSON t, Show a) =>
    (String -> String) -> (t -> a) -> (RemoteValue a) -> m (RemoteValue a)
over2 unevaluated_remote_accessor my_function objectName = do
    item <- view2 unevaluated_remote_accessor objectName
    let new_item = show $ my_function item
    set2 unevaluated_remote_accessor new_item objectName
-}

-- SQlite doesn't support nested tables, so nesting dictionaries with in each other
-- isn't possible unless  "Parent/Child Tables are suppoted."
-- Does Haskell SQlite support this automatic generation?
-- Found higher level package like Groundhog, can't find information
-- specifically on Sqlite simple, I don't think that level of functionatlity was added.

-- query

data RemoteLens a b = RemoteLens {
    remoteview ::  (FromJSON b) => (String -> String) -> (RemoteValue a) -> (RemoteMonad b)
    , remoteset :: (String -> String) -> String -> (RemoteValue a) -> RemoteMonad (RemoteValue a)}

a_Lens :: RemoteLens a b
a_Lens = RemoteLens {
    remoteview = view2
    , remoteset = set2}

applyview :: FromJSON b => RemoteLens a b -> (String -> String) -> RemoteValue a -> RemoteMonad b
applyview (RemoteLens viewer setter) = viewer

applyset :: RemoteLens a b -> (String -> String) -> String -> RemoteValue a -> RemoteMonad (RemoteValue a)
applyset (RemoteLens viewer setter) = setter

recover_set :: (String -> String) -> String -> RemoteValue a -> RemoteMonad (RemoteValue a)
recover_set = applyset a_Lens

recover_view :: (FromJSON b) =>  (String -> String) -> RemoteValue a -> RemoteMonad b
recover_view = applyview a_Lens

-- Only one problem the (String -> String) still exists

{-
address objectName = objectName ++ ".address"
street objectName = objectName ++ ".street"
city objectName = objectName ++ ".city"
name objectName = objectName ++ ".name"
age objectName = objectName ++ ".age"

getStreet :: RemoteValue a -> (RemoteMonad String)
getStreet newperson = view2 (address >>> street) newperson
-}

data RL_ObjectA  = RL_ObjectA {
    rl_address :: String -> String
    , rl_street :: String -> String
    , rl_city :: String -> String
    , rl_name :: String -> String
    , rl_age :: String -> String}
    deriving (Show)

a_Object :: RL_ObjectA
a_Object = RL_ObjectA {
    rl_address = address
    , rl_street = street
    , rl_city = city
    , rl_name = name
    , rl_age = age}

getname3 :: FromJSON b => RemoteValue a -> RemoteMonad b
getname3 newperson = view2 (rl_name a_Object) newperson

remote_view' :: FromJSON b => RL_ObjectA -> RemoteValue a -> RemoteMonad b
remote_view' object newperson = view2 (rl_name object) newperson

applyname :: RL_ObjectA -> String -> String
applyname (RL_ObjectA a b c d e) = d

--

type RemoteLenss = String -> String

getname4 :: (FromJSON b) => RemoteLenss -> RemoteValue a -> RemoteMonad b
getname4 accessor newperson = view2 accessor newperson