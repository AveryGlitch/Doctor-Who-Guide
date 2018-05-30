{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import System.Directory (renameFile)

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
data Doctor = Doctor DoctorNum [Season] deriving (Show, Read)
-- A Season is just a season number* and a list of stories
data Season = Season SeasonNum [Story] deriving (Show, Read)
-- A story contains a bunch of info!
data Story  = Story Name Number NumEpisodes Missing Recommendation (Maybe Note) Synopsis Review deriving (Show, Read)
-- A story is either missing no episodes, all episodes, or some specific episodes
data Missing = None
             | All
             | Some [Int] deriving (Show, Read, Eq)
-- Our recommendations
data Recommendation = Highly | Yes | Maybe | Partial | No deriving (Show, Read)



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



--------------------------------------------------------------------------------
-- Output functions
--------------------------------------------------------------------------------
preamble, postamble, tableHeading :: String
preamble = "<html lang=\"en\"><head><title>Avery's Doctor Who Guide</title><meta charset=\"utf-8\" /><link rel=\"stylesheet\" href=\"style.css\"></head><body>"
postamble = "</body></html>"
tableHeading = "<table class=\"maintable\"><tr><th>Story</th><th>Watch?</th><th>Details</th></tr>\n"

output :: Table -> String
output table = preamble ++ introduction ++ output' table ++ postamble
  where
    output' [] = ""
    output' (Doctor n seasons : rest)
      = "<h1>" ++ ordinal n ++ " Doctor</h1>\n"
      ++ tableHeading ++ concatMap outputSeason seasons ++ "</table>"
      ++ output' rest

outputSeason :: Season -> String
outputSeason (Season num stories)
  = "<tr class=\"season\"><td colspan=3>Season " ++ num ++ "</td></tr>\n"
    ++ concatMap outputStory stories

outputStory :: Story -> String
outputStory (Story name number numEps missing recc note synopsis review)
  = "<tr class=name><td>\n"
    ++ "<p class="
    ++ (if missing == None then "name" else "name-missing")
    ++ ">" ++ name ++ "</p>\n"
    ++ "<table><tr class=info><td>Story Number</td><td>" ++ show number ++ "</td></tr>\n"
    ++ "<tr class=info><td>Number of Episodes</td><td>" ++ show numEps  ++ "</td></tr>\n"
    ++ case missing of
         None -> ""
         All  -> "<tr class=info><td>Missing Episodes?</td><td>Yes: all</td></tr>\n"
         Some eps -> "<tr class=info><td>Missing Episodes?</td><td>Yes: "
                     ++ showEps eps
                     ++ "</td></tr>\n"
    ++ "</table></td>\n"
    ++ "<td><div class=\"" ++ show recc ++ "\">"
    ++ case recc of
         Highly -> "✨ Highly Recommended ✨"
         Yes    -> "Watch"
         Maybe  -> "Maybe"
         Partial-> "Partial watch"
         No     -> "Don't watch"
    ++ case note of
         Just note' -> ", " ++ note'
         Nothing -> ""
    ++ "</div></td>\n"
    ++ "<td><table class=details>"
    ++ "<tr class=details><td class=details-tag>Synopsis</td>"
    ++ "<td class=details-text>" ++ synopsis ++ "</td></tr>\n"
    ++ "<tr class=details><td class=details-tag>Review</td>"
    ++ "<td class=details-text>" ++ review ++ "</td></tr></table>\n"
    ++ "</td></tr>\n"

showEps :: [Int] -> String
showEps [] = "none"
showEps [e] = show e
showEps [e1, e2] = show e1 ++ " and " ++ show e2
showEps (e:rest) = show e ++ ", " ++ showEps rest

ordinal :: Int -> String
ordinal n = case n of
              1  -> "First"
              2  -> "Second"
              3  -> "Third"
              4  -> "Fourth"
              5  -> "Fifth"
              6  -> "Sixth"
              7  -> "Seventh"
              8  -> "Eighth"
              9  -> "Nineth"
              10 -> "Tenth"
              11 -> "Eleventh"
              12 -> "Twelfth"
              13 -> "Thirteenth"
              14 -> "Fourteenth"
              15 -> "Fifteenth"
              16 -> "Sixteenth"
              17 -> "Seventeenth"
              18 -> "Eighteenth"
              19 -> "Nineteenth"
              _  -> error "I haven't accounted for this many doctors"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- Commands that can be run
data Command = Output | AddDoctor | AddSeason | AddStory | Usage

-- | Run a command on a table
run :: Command -> Table -> IO ()
run Usage _         = do putStrLn "dw Output     : outputs the table as html"
                         putStrLn "dw add doctor : adds a new doctor to the table"
                         putStrLn "dw add season : adds a new season to the table (interactive)"
                         putStrLn "dw add story  : adds a new story to the table (interactive)"
                         putStrLn "dw edit story : edit a particular story"
                         putStrLn ""
                         putStrLn $ "The table is stored in " ++ file
run Output table    = putStrLn $ output table
run AddDoctor table = do writeOut $ addDoctor table
run AddSeason table = do doctor <- prompt "To doctor: "
                         season <- prompt "Season Number: "
                         let result = addSeason (read doctor) season table
                         case result of
                           Just newTable -> do writeOut newTable
                           Nothing -> print "couldn't add season!"
run AddStory table  = do season <- prompt "Season Number: "
                         name <- prompt "Name: "
                         number <- prompt "Number: "
                         numEpisodes <- prompt "Number of Episodes: "
                         missing <- prompt "Missing: "
                         recommendation <- prompt "Recommendation: "
                         note <- prompt "Note: "
                         synopsis <- prompt "Synopsis: "
                         review <- prompt "Review: "
                         let result = addStory (Story name (read number) (read numEpisodes) (read missing) (read recommendation) (read note) synopsis review) season table
                         case result of
                           Just newTable -> do writeOut newTable
                           Nothing       -> print "couldn't add story!"

main :: IO ()
main = do arg <- getArgs
          fileContent <- readFile file
          pure (read fileContent) >>= case arg of
            ["output"]        -> run Output
            ["add", "doctor"] -> run AddDoctor
            ["add", "season"] -> run AddSeason
            ["add", "story"]  -> run AddStory
            _                 -> run Usage



-- | Prompt the user for a response
prompt :: String -> IO String
prompt text = putStr text >> getLine >>= return

file, tmpfile, backup :: FilePath
file = "/home/avery/.DrWhoDB"
tmpfile = "/home/avery/.DrWhoDB_tmp"
backup = "/home/avery/.DrWhoDB.bak"


writeOut :: Table -> IO ()
writeOut table = do writeFile tmpfile (show table)
                    renameFile file backup
                    renameFile tmpfile file


introduction
  = h1 "Avery's Doctor Who Guide"
    ++ p "So, you want to watch Doctor Who, through the classic and modern era, but you're not so sure on how much to watch? You've come to the right place! This guide has several different tracks, depending on what you're interested in"
    ++ "<table>"
    ++ tr (td (Just "Highly") "Fast Track"
           ++ td Nothing "The Highly recommended episodes. If you only want a small sampling of episodes, look here!")
    ++ tr (td (Just "Yes") "Recommended Track"
           ++ td Nothing "For most people, you'll want to stick on the Recommended track - watch both the Fast track episodes and the recommended episodes (don't forget the partials, see the next section), and you'll get quite a lot of Doctor Who, without having to sit through the slower stuff.")
    ++ tr (td (Just "Maybe") "Maybe Track"
          ++ td Nothing "If you're interested in a more thorough watch through, you can also watch the episodes on the maybe track. These aren't bad episodes by any right - they're just not neccessary to watch")
    ++ tr (td (Just "No") "Avoid"
          ++ td Nothing "These episodes are only recommended if you're truly curious and dedicated.")
    ++ "</table>"
    ++ p "Additionally, some stories are marked as a <span class=Partial>partial watch</span> - this means you <strong>should</strong> watch it, but not all of it - just certain episodes."
    ++ p "Many of the early episodes are missing. You will be able to tell which ones these are because the name of the story will be in italics, and it will be mentioned several times. These stories aren't unwatchable, surprisingly - reconstructions of the episodes have been made, and they are (relatively) watchable. If you don't want to watch the reconstuctions, though (and I don't blame you), they are easy to skip."
    ++ p "\"Wait, but what if I want to watch <strong>everything</strong>?\" go ahead! There's nothing stopping you. But this guide is for people who want a more selective sampling of the series, or for those who will watch every episode, you can use this guide as a litmus test."
    ++ p "This guide is currently a work in progress, and only goes as far as I've watched so far. I started watching through the episodes for this guide in early May 2018, and I'm still going strong."
    ++ "<hr>"


h1,p, tr :: String -> String
h1 string = "<h1>" ++ string ++ "</h1>"
p string = "<p>" ++ string ++ "</p>"
tr string = "<tr class=intro>" ++ string ++ "</tr>"
td :: Maybe String -> String -> String
td Nothing string = "<td>" ++ string ++ "</td>"
td (Just style) string = "<td class=" ++ style ++ ">" ++ string ++ "</td>"
