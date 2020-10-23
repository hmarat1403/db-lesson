module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

main :: IO ()
main = do
    print "Enter a command"
    command <- getLine
    performCommand command

--performCommand organizes all the commands the user can enter
performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "addtool" = promptAndAddTool >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry command not found" >> main

{-Q41.1 Create an IO action named addTool, like addUser, to add a tool to the database-}
addTool :: String -> String -> IO ()
addTool name description = withConn "tools.db" $
    \conn -> do
        currentDay <- utctDay <$> getCurrentTime
        execute conn (mconcat ["INSERT INTO tools"
                               , "(name, description,"
                               , " lastReturned, timesBorrowed)"
                               , " Values (?,?,?,?)"])
                      (name, description, currentDay, (0 :: Int))
        print "tool added"

{-Q41.2 Add an addtool command that prompts the user for information and then adds
the tool by using the addTool action from the preceding question.-}
promptAndAddTool :: IO ()
promptAndAddTool = do
    print "Enter new tool name"
    name <- getLine
    print "Enter new tool description"
    description <- getLine
    addTool name description


data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }
instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"]    

instance FromRow Tool where
    fromRow = Tool  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field                        
data User = User
    { userId :: Int
    , userName :: String
    }   
instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user]
instance FromRow User where
    fromRow = User  <$> field
                    <*> field    

-- Organizing database actions
promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser userName
promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- pure read <*> getLine
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine
    checkout userId toolId
promptAndCheckin :: IO ()
promptAndCheckin = do
    print "enter the id of tool"
    toolId <- pure read <*> getLine
    checkinAndUpdate toolId                                        

--withConn lets you abstract out connecting to the database
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn 

--addUser action connects to database and inserts a user
addUser :: String -> IO ()
addUser userName = withConn "tools.db" $ 
    \conn -> do
        execute conn "INSERT INTO users (username) VALUES (?)"
            (Only userName)
        print "user added"     

--Checking out by adding the toolId and userId to checkedout         
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
    \conn -> do
        execute conn
            "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
            (userId,toolId)   

printUsers :: IO ()
printUsers = withConn "tools.db" $
    \conn -> do
        resp <- query_ conn "SELECT * FROM users;" :: IO [User]
        mapM_ print resp 

-- run any queries of tools from your database
printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
    \conn -> do
        resp <- query_ conn q :: IO [Tool]
        mapM_ print resp
printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"
printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from checkedout);"
                                          ]
printCheckedout :: IO ()
printCheckedout = printToolQuery $
    mconcat [ "select * from tools "
            , "where id in "
            , "(select tool_id from checkedout);"
            ]                         

-- updating the tool table
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
    \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime
        let updatedTool = updateTool <$> tool <*> pure currentDay
        updateOrWarn updatedTool  

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
        "SELECT * FROM tools WHERE id = (?)"
        (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp          

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x  

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
    \conn -> do
        let q = mconcat ["UPDATE TOOLS SET "
                        ,"lastReturned = ?,"
                        ," timesBorrowed = ? "
                        ,"WHERE ID = ?;"]
        execute conn q (lastReturned tool
                        , timesBorrowed tool
                        , toolId tool)
        print "tool updated"    

--Making sure your tool is updated when it’s checked in
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
    \conn -> do
        execute conn
            "DELETE FROM checkedout WHERE tool_id = (?);"
            (Only toolId)       
