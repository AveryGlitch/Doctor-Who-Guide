{-# LANGUAGE LambdaCase #-}
module Change where

import qualified DrWho as DrWho


-- First some types to make signatures more readable
type Name = String
type Note = String
type Synopsis = String
type Review = String
type NumEpisodes = Int
type Number = Int
type DoctorNum = Int
type SeasonNum = String


-- Our Table is just a list of Doctors...
type Table  = [Doctor]
-- A Doctor is just a number and a list of seasons...
data Doctor = Doctor DoctorNum [Season]
  deriving (Show, Read)
-- A Season is just a season number* and a list of stories
data Season = Season SeasonNum [Story]
  deriving (Show, Read)
-- A story contains a bunch of info!
data Story  = Story Name Number NumEpisodes DrWho.Missing DrWho.Recommendation (Maybe Note) Synopsis Review
  deriving (Show, Read)
-- A story is either missing no episodes, all episodes, or some specific episodes
data Missing = None
             | All
             | Some [Int] deriving (Show, Read, Eq)
-- Our recommendations
data Recommendation = Highly | Yes | Maybe | Partial | No deriving (Show, Read)

grab :: IO Change.Table
grab = do fileContents <- readFile "backup_DrWhoDB"
          return (read fileContents)


convert :: Change.Table -> DrWho.Table -> DrWho.Table
convert [] acc = acc
convert (Doctor _ seasons : rest) acc = let newDocTable = DrWho.addDoctor acc
                                        in convert rest (addSeasons seasons newDocTable)


addSeasons :: [Change.Season] -> DrWho.Table -> DrWho.Table
addSeasons [] table                          = table
addSeasons (Season num stories : rest) table = let Just newTable = (DrWho.addSeason
                                                                    (DrWho.getLastDoctor table) num table)
                                               in addSeasons (rest) (addStories num stories newTable)


addStories :: SeasonNum -> [Story] -> DrWho.Table -> DrWho.Table
addStories n [] table = table
addStories n (Story name number numEps missing recc note synopsis review : rest) table
  = let story = DrWho.Story name number numEps missing recc note synopsis review
        Just newTable = DrWho.addStory story n table
    in addStories n rest newTable

