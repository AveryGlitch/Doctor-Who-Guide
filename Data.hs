{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Data where

import qualified Data.Yaml as Yaml
import GHC.Generics

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
data Doctor = Doctor
  {
    doctorNum :: DoctorNum
  , seasons   :: [Season]
  } deriving (Generic, Show, Read)
-- A Season is just a season number* and a list of stories
data Season = Season
  {
    seasonNum :: SeasonNum
  , stories   :: [Story]
  } deriving (Generic, Show, Read)
-- A story contains a bunch of info!
data Story  = Story
  {
    name           :: Name
  , number         :: Number
  , numEpisodes    :: NumEpisodes
  , missing        :: Missing
  , reccomendation :: Recommendation
  , note           :: (Maybe Note)
  , synopsis       :: Synopsis
  , review         :: Review
} deriving (Generic, Show, Read)
-- A story is either missing no episodes, all episodes, or some specific episodes
data Missing = None
             | All
             | Some [Int] deriving (Generic, Show, Read, Eq)
-- Our recommendations
data Recommendation = Highly | Yes | Maybe | Partial | No deriving (Generic, Show, Read)

instance Yaml.ToJSON Doctor
instance Yaml.ToJSON Season
instance Yaml.ToJSON Story
instance Yaml.ToJSON Missing
instance Yaml.ToJSON Recommendation

instance Yaml.FromJSON Doctor
instance Yaml.FromJSON Season
instance Yaml.FromJSON Story
instance Yaml.FromJSON Missing
instance Yaml.FromJSON Recommendation


-- an empty table.
emptyTable :: Table
emptyTable = []


-- | Checks if a certain doctor is in the table
hasDoctor :: DoctorNum -> Table -> Bool
hasDoctor _ [] = False
hasDoctor n (Doctor n' _ : rest) = n == n' || hasDoctor n rest


-- | Checks if a certain season is in the table
hasSeason :: SeasonNum -> Table -> Bool
hasSeason _ [] = False
hasSeason n (Doctor _ seasons : rest) = any (\(Season n' _) -> n == n') seasons || hasSeason n rest


-- | Tells you what the last doctor is (by number)
getLastDoctor :: Table -> DoctorNum
getLastDoctor [] = 0
getLastDoctor table = case last table of
                        Doctor n _ -> n


-- | Adds another Doctor to the table
addDoctor :: Table -> Table
addDoctor table = let nextDoctor = getLastDoctor table + 1
                  in table ++ [Doctor nextDoctor []]


-- | Adds a new season to the table. Requires both a doctor number and a season number
addSeason :: DoctorNum -> SeasonNum -> Table -> Maybe Table
addSeason doctorNum seasonNum table = if hasDoctor doctorNum table then Just (addSeason' doctorNum seasonNum table)
                                                                   else Nothing
  where
    addSeason' :: DoctorNum -> SeasonNum -> Table -> Table
    addSeason' _ _ [] = error "impossible"
    addSeason' doctorNum seasonNum (Doctor n seasons : rest)
      = if n == doctorNum
        then Doctor n (seasons ++ [Season seasonNum []]) : rest
        else Doctor n seasons : addSeason' doctorNum seasonNum rest


-- | Adds a story to a specific season in the table
addStory :: Story -> SeasonNum -> Table -> Maybe Table
addStory story season table = if hasSeason season table then Just (addStory' story season table)
                                                        else Nothing
  where
    addStory' story season [] = error "impossible"
    addStory' story season (Doctor n seasons : rest)
      = if any (\s -> case s of Season n' _ -> season == n') seasons
        then (Doctor n (addToSeason story season seasons)) : rest
        else Doctor n seasons : addStory' story season rest
    addToSeason story season [] = error "impossible"
    addToSeason story season (Season sn stories : rest)
      | sn == season = Season sn (stories ++ [story]) : rest
      | otherwise    = Season sn stories : (addToSeason story season rest)
