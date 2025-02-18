module Set14b where

-- In this exercise set, we're going to implement an HTTP API for a
-- simple bank. The user should be able to deposit money, withdraw
-- money and check an accounts balance over HTTP. The balances
-- themselves will be stored in an SQLite database.
--
-- It's a good idea to study Examples/Phonebook.hs and
-- Examples/PathServer.hs before jumping into this exercise set.
--
-- Let's start with some imports:
import Mooc.Todo

-- Utilities
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Read as TR
import Text.Read (readMaybe)

-- HTTP server
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)

-- Database
import Database.SQLite.Simple
  ( Connection
  , Query(..)
  , execute
  , execute_
  , open
  , query
  , query_
  )

------------------------------------------------------------------------------
-- Ex 1: Let's start with implementing some database operations. The
-- database will contain one table, called events, with two columns:
-- account (a string) and amount (a number).
--
-- The database will not be storing the balances of the accounts, but
-- instead a _transaction log_: each withdrawal and deposit will be
-- its own row. The balance of the account can then be computed from
-- these.
--
-- Below, you'll find three queries:
-- * initQuery creates the database
-- * depositQuery adds an (account, amount) row into the database
-- * getAllQuery gets all (account, amount) pairs from the database.
--   getAllQuery isn't needed for the implementation, but you can use it
--   to test your answer.
--
-- Your task is to implement the IO operations openDatabase and deposit.
-- See below for their details.
--
-- Tip: creating a database with the filename "" will create a
-- temporary database that won't get saved to disk. Useful for
-- testing!
--
-- Example in GHCi:
--   Set14b> db <- openDatabase ""
--   Set14b> deposit db (T.pack "xxx") 13
--   Set14b> deposit db (T.pack "yyy") 5
--   Set14b> deposit db (T.pack "xxx") 7
--   Set14b> query_ db getAllQuery :: IO [(String,Int)]
--   [("xxx",13),("yyy",5),("xxx",7)]
initQuery :: Query
initQuery =
  Query
    (T.pack
       "CREATE TABLE IF NOT EXISTS events (account TEXT NOT NULL, amount NUMBER NOT NULL);")

depositQuery :: Query
depositQuery =
  Query (T.pack "INSERT INTO events (account, amount) VALUES (?, ?);")

getAllQuery :: Query
getAllQuery = Query (T.pack "SELECT account, amount FROM events;")

-- openDatabase should open an SQLite database using the given
-- filename, run initQuery on it, and produce a database Connection.
openDatabase :: String -> IO Connection
openDatabase file = do
  db <- open file
  execute_ db initQuery
  return db

-- given a db connection, an account name, and an amount, deposit
-- should add an (account, amount) row into the database
deposit :: Connection -> T.Text -> Int -> IO ()
deposit db txt n =
  execute db depositQuery (txt, n)

------------------------------------------------------------------------------
-- Ex 2: Fetching an account's balance. Below you'll find
-- balanceQuery, a query which gets all the amounts related to an
-- account from the database.
--
-- Implement the IO operation balance, which given an account, returns
-- the sum of all the amounts related to that account.
--
-- PS. if you know SQL you can do the summing in SQL by changing
-- balanceQuery, otherwise you can do it in the balance operation
-- itself. If you choose to edit the SQL query, remember that sum
-- can return null.
--
-- Example in GHCi:
--   Set14b> db <- openDatabase ""
--   Set14b> deposit db (T.pack "xxx") 13
--   Set14b> deposit db (T.pack "yyy") 5
--   Set14b> deposit db (T.pack "xxx") 7
--   Set14b> balance db (T.pack "xxx")
--   20
--   Set14b> balance db (T.pack "yyy")
--   5
--   Set14b> balance db (T.pack "zzz")
--   0
balanceQuery :: Query
balanceQuery = Query (T.pack "SELECT amount FROM events WHERE account = ?;")

balance :: Connection -> T.Text -> IO Int
balance db txt = do
  n <- query db balanceQuery [txt] :: IO [[Int]]
  if null n
    then return 0
    else return (sum $ concat n)

------------------------------------------------------------------------------
-- Ex 3: Now that we have the database part covered, let's think about
-- our API next. The datatype Command represents the various commands
-- users can issue: Deposit and Balance.
--
-- The HTTP API will use paths like the following:
-- * /deposit/smith/3 will deposit 3 into the account "smith"
-- * /balance/lopez will query the balance of the account "lopez"
--
-- Your task is to implement the function parseCommand that takes the
-- pathInfo (remember: a list of Texts) of a request, and returns the
-- Command it corresponds to.
--
-- The return type of this function is Maybe Command instead of
-- Command so that we can add error handling later. For now, you can
-- assume the input to parseCommand is always valid, and the return
-- value is always Just someCommand.
--
-- The function parseInt that reads an Int from a Text is provided for
-- you.
--
-- PS. the test outputs print Text values as if they were Strings,
-- just like GHCi prints Texts as Strings.
--
-- Examples:
--   parseCommand [T.pack "balance", T.pack "madoff"]
--     ==> Just (Balance "madoff")
--   parseCommand [T.pack "deposit", T.pack "madoff", T.pack "123456"]
--     ==> Just (Deposit "madoff" 123456)
data Command
  = Deposit T.Text Int
  | Withdraw T.Text Int
  | Balance T.Text
  deriving (Show, Eq)

parseInt :: T.Text -> Maybe Int
parseInt = readMaybe . T.unpack

parseCommand :: [T.Text] -> Maybe Command
parseCommand [] = Nothing
parseCommand [x] = Nothing
parseCommand (x:y:z)
  | length z > 1 = Nothing
  | x == T.pack "balance" && null z = Just (Balance y)
  | x == T.pack "deposit" && z /= [] && isJust (parseInt (head z)) =
    Just (Deposit y (fromJust $ parseInt $ head z))
  | x == T.pack "withdraw" && z /= [] && isJust (parseInt (head z)) =
    Just (Withdraw y (fromJust $ parseInt $ head z))
  | otherwise = Nothing

------------------------------------------------------------------------------
-- Ex 4: Running commands. Implement the IO operation perform that takes a
-- database Connection, the result of parseCommand (a Maybe Command),
-- and runs the command in the database. Remember to use the
-- operations you implemented in exercises 1 and 2.
--
-- The perform operation should produce a Text that describes the result
-- of the command. The result of a Deposit command should be "OK" and
-- the result of a Balance command should be the balance, as a Text.
--
-- You don't need to handle the case where the command is Nothing yet,
-- you'll get to deal with that in exercise 8.
--
-- Example in GHCi:
--   Set14b> perform db (Just (Deposit (T.pack "madoff") 123456))
--   "OK"
--   Set14b> perform db (Just (Deposit (T.pack "madoff") 654321))
--   "OK"
--   Set14b> perform db (Just (Balance (T.pack "madoff")))
--   "777777"
--   Set14b> perform db (Just (Balance (T.pack "unknown")))
--   "0"
perform :: Connection -> Maybe Command -> IO T.Text
perform db (Just cmd) =
  case cmd of
    (Balance acc) -> do
      s <- balance db acc
      return (T.pack (show s))
    (Deposit acc amount) -> do
      s <- deposit db acc amount
      return (T.pack "OK")
    (Withdraw acc amount) -> do
      s <- withdraw db acc amount
      return (T.pack "OK")

------------------------------------------------------------------------------
-- Ex 5: Next up, let's set up a simple HTTP server. Implement a WAI
-- Application simpleServer that always responds with a HTTP status
-- 200 and a text "BANK" to any request.
--
-- You can use the function encodeResponse to convert a Text into the
-- right kind of ByteString to give to responseLBS.
--
-- Example:
--   - In GHCi: run 8899 simpleServer
--   - Go to <http://localhost:8899> in your browser, you should see the text BANK
encodeResponse :: T.Text -> LB.ByteString
encodeResponse t = LB.fromStrict (encodeUtf8 t)

-- Remember:
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
simpleServer :: Application
simpleServer request respond =
  respond (responseLBS status200 [] (encodeResponse $ T.pack "BANK"))

------------------------------------------------------------------------------
-- Ex 6: Now we finally have all the pieces we need to actually
-- implement our API. Implement a WAI Application called server that
-- receives a request, parses the Command it refers to, and runs the
-- command. Use the parseCommand, perform and encodeResponse
-- functions.
--
-- After you've implemented server, you can run the bank API from the
-- command line with
--   stack runhaskell Set14b.hs
-- This uses the main function provided below.
--
-- Tip: it can make debugging easier if you print the command before
-- performing it.
--
-- Example:
--   - Run the server with "stack runhaskell Set14b.hs"
--   - Open <http://localhost:3421/deposit/lopez/17> in your browser.
--     You should see the text OK.
--   - Open <http://localhost:3421/deposit/lopez/8> in your browser.
--     You should see the text OK.
--   - Open <http://localhost:3421/balance/lopez> in your browser.
--     You should see the text 25.
-- Remember:
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
server :: Connection -> Application
server db request respond =
  case cmd of
    Just x -> do
      res <- perform db cmd
      respond (responseLBS status200 [] (encodeResponse res))
    Nothing ->
      respond (responseLBS status200 [] (encodeResponse (T.pack "ERROR")))
  where
    path = pathInfo request
    cmd = parseCommand path

port :: Int
port = 3421

main :: IO ()
main = do
  db <- openDatabase "bank.db"
  putStr "Running on port: "
  print port
  run port (server db)

------------------------------------------------------------------------------
-- Ex 7: Add the possibility to withdraw funds to the API. Withdrawing
-- should happen via a /withdraw/<account>/<amount> path, similarly to
-- deposit. The response to a withdraw should be "OK", just like for a
-- deposit. You'll need to edit the Command datatype, and the
-- parseCommand and run functions to support this new command.
--
-- Hint: you can just use deposit IO operation to implement the
-- withdraw. You don't need new SQL queries.
--
-- Example:
--   - Run the server with "stack runhaskell Set14b.hs"
--   - Open <http://localhost:3421/deposit/simon/17> in your browser.
--     You should see the text OK.
--   - Open <http://localhost:3421/withdraw/simon/6> in your browser.
--     You should see the text OK.
--   - Open <http://localhost:3421/balance/simon> in your browser.
--     You should see the text 11.
withdrawalQuery :: Query
withdrawalQuery =
  Query (T.pack "INSERT INTO events (account, amount) VALUES (?, ?);")

withdraw :: Connection -> T.Text -> Int -> IO ()
withdraw db txt n =
  execute db withdrawalQuery (txt, -n)
------------------------------------------------------------------------------
-- Ex 8: Error handling. Modify the parseCommand function so that it
-- returns Nothing when the input is not valid. Modify the perform
-- function so that it produces an "ERROR" response given a Nothing.
--
-- Hint: the Maybe monad can help you with parseCommand, but you can
-- also just write normal code instead.
--
-- Examples:
--  - Run the server with "stack runhaskell Set14b.hs"
--  - All of these URLs should produce the text ERROR:
--    - http://localhost:3421/unknown/path
--    - http://localhost:3421/deposit/pekka
--    - http://localhost:3421/deposit/pekka/x
--    - http://localhost:3421/deposit/pekka/1x
--    - http://localhost:3421/deposit/pekka/1/3
--    - http://localhost:3421/balance
--    - http://localhost:3421/balance/matti/pekka
