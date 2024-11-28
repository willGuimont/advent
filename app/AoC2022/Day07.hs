{-# LANGUAGE TemplateHaskell #-}

module AoC2022.Day07 (main) where

import Control.Lens hiding (noneOf)
import Control.Monad.Except (throwError)
import qualified Control.Monad.State as S
import Data.List (elemIndices, isPrefixOf, singleton, sort)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Debug.Trace (trace)

-- Types
type FileName = String

data FileOutput
  = DirectoryOut FileName
  | FileOut FileName Int
  deriving (Show)

data Command
  = ChangeDir FileName
  | ListFiles [FileOutput]
  deriving (Show)

-- Parsing
trySepBy :: Parser a -> Parser sep -> Parser [a]
trySepBy p sep = do
  r <- optionMaybe p
  case r of
    Nothing -> return []
    Just x -> (x :) <$> many (try $ sep >> p)

parseDirectoryOut :: Parser FileOutput
parseDirectoryOut = (string "dir " >> many1 (noneOf "\n")) <&> DirectoryOut

parseFileOut :: Parser FileOutput
parseFileOut = do
  size <- read <$> many1 digit
  _ <- space
  name <- many1 (noneOf "\n")
  return $ FileOut name size

parseFileObject :: Parser FileOutput
parseFileObject = try parseDirectoryOut <|> try parseFileOut

parseFileObjects :: Parser [FileOutput]
parseFileObjects = parseFileObject `trySepBy` newline

parseChangeDir :: Parser Command
parseChangeDir = (string "$ cd " >> many1 (noneOf "\n")) <&> ChangeDir

parseList :: Parser Command
parseList = (string "$ ls" >> newline >> parseFileObjects) <&> ListFiles

parseCommand :: Parser Command
parseCommand = try parseChangeDir <|> try parseList

parseCommands :: Parser [Command]
parseCommands = parseCommand `sepBy` newline

parseInput :: String -> Either String [Command]
parseInput input = case parse parseCommands "commands" input of
  Left err -> throwError $ show err
  Right x -> return x

-- Logic
type FileSystem = M.Map FileName Int

data World = World
  { _currentDir :: FileName,
    _fs :: FileSystem,
    _directories :: [FileName]
  }
  deriving (Show)

makeLenses ''World

moveBack :: String -> String
moveBack path = take (idx + 1) path
  where
    idx = reverse (elemIndices '/' path) !! 1

changeDir :: String -> S.State World ()
changeDir "/" = do
  w <- S.get
  S.put $ over currentDir (const "/") w
changeDir ".." = do
  w <- S.get
  let newPath = moveBack $ view currentDir w
  S.put $ over currentDir (const newPath) w
changeDir d = do
  w <- S.get
  S.put $ over currentDir (<> d <> "/") w

addFile :: FileOutput -> S.State World ()
addFile (FileOut name size) = do
  w <- S.get
  let currDir = view currentDir w
  let path = currDir <> name
  S.put $ over fs (M.insert path size) w
addFile (DirectoryOut name) = do
  w <- S.get
  let currDir = view currentDir w
  S.put $ over directories ((currDir <> name <> "/") :) w

applyCommand :: Command -> S.State World ()
applyCommand (ChangeDir name) = changeDir name
applyCommand (ListFiles files) = mapM_ addFile files

applyCommands :: [Command] -> World -> World
applyCommands cs = S.execState (mapM_ applyCommand cs)

getDirSize :: World -> String -> Int
getDirSize w path = sum $ M.filterWithKey (\k _ -> path `isPrefixOf` k) $ view fs w

totalSpace :: Int
totalSpace = 70000000

updateSpace :: Int
updateSpace = 30000000

getSpaceToDelete :: Int -> Int
getSpaceToDelete occupied = updateSpace - (totalSpace - occupied)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let (Right commands) = parseInput input
  let fileSystem = applyCommands commands $ World "/" M.empty (singleton "/")
  let dirs = view directories fileSystem
  let sizes = getDirSize fileSystem <$> dirs
  print $ sum . filter (<= 100000) $ sizes
  
  let totalOccupied = getDirSize fileSystem "/"
  let toDelete = getSpaceToDelete totalOccupied
  print $ minimum . filter (>= toDelete) $ sizes
