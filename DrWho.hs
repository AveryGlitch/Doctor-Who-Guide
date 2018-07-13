{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module DrWho where

--test
import System.Environment (getArgs)
import System.Directory (renameFile)
import Prelude hiding (div)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as ByteString
import System.IO (hSetBuffering, stdin, stdout, BufferMode (..))

import Data
import HTML
import IntroOutro





--------------------------------------------------------------------------------
-- Output functions
--------------------------------------------------------------------------------
preamble, postamble, tableHeading :: String
preamble = "<html lang=\"en\"><head><title>Avery's Doctor Who Guide</title><meta charset=\"utf-8\" /><link rel=\"stylesheet\" href=\"style.css\"></head><body>"
postamble = "</body></html>"
tableHeading = "<table class=\"maintable\"><tr><th>Story</th><th>Watch?</th><th>Details</th></tr>\n"

output :: Table -> String
output table = preamble +. introduction +. toc table +. "<hr>" +. output' table +. outro +. postamble
  where
    output' [] = ""
    output' (Doctor n seasons : rest)
      = "<a name=\"doctor" ++ show n ++ "\"></a>"
      +. h1 (ordinal n ++ " Doctor") ++ img ("../images/doctor-who/doctor" ++ show n ++ ".png") ("The " ++ ordinal n ++ " Doctor")
      +. if length rest > 0 then "<span style=\"font-size: small; text-align: right\">" ++ a ("#doctor" ++ show (n + 1)) " ↩ next doctor" ++ "</span>" else ""
      +. tableHeading +. concatMap outputSeason seasons +. "</table>"
      +. output' rest

outputSeason :: Season -> String
outputSeason (Season num stories)
  = tr' "season" ("<td colspan=3>" ++ "<a name=\"season-" ++ num ++ "\"></a>Season " ++ num ++ "</td>")
    +. concatMap outputStory stories

outputStory :: Story -> String
outputStory (Story name number numEps missing recc note synopsis review majorPlotChanges)
  = tr' "name"
    (td' (if missing == None then "name" else "name-missing")
        ("<p class="
         ++ (if missing == None then "name" else "name-missing")
         ++ ">" ++ name ++ "</p>"
         ++ (if missing /= None then div "reconstruction" "Reconstruction" else "")
         +. "<table>"
         +. tr' "info" (td "Story Number" ++ td (show number))
         +. tr' "info" (td "Number of Episodes" ++ td (show numEps))
         +. case missing of
              None -> ""
              All  -> tr' "info" (td "Missing Episodes?" ++ td "Yes: all")
              Some eps -> tr' "info" (td "Missing Episodes?" ++ td ("Yes: " ++ showEps eps))
         +. "</table>\n")
    +. td (div (show recc)
            (
              (case recc of
                  Highly -> "✨ Highly Recommended ✨"
                  Yes    -> "Watch"
                  Maybe  -> "Maybe"
                  Partial-> "Partial watch"
                  No     -> "Don't watch"
              )
              ++ (case note of
                     Just text -> ", " ++ text
                     Nothing   -> ""
                 )
            )
          )
    +. td ("<table class=details>"
           +. tr' "details" (td' "details-tag" "Synopsis" ++ td' "details-text" synopsis)
           +. tr' "details" (td' "details-tag" "Review"   ++ td' "details-text" review)
           +. (case majorPlotChanges of
                 Just changes -> tr' "details" (td' "details-tag" "Major Plot Changes"
                                                ++ td' "details-text"
                                                (simplehtml "details" $
                                                 (simplehtml "summary" "Click to reveal") ++ changes))
                 Nothing -> ""
              )
           +. "</table>"))
    ++ "\n"

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

toc :: Table -> String
toc [] = ""
toc table = div "dimbox" $
                "<details>" +. simplehtml "summary" "Table of Contents"
                +. "<ol>" +. toc' table +. "</ol>"
                +. "</details>"
  where
    toc' [] = ""
    toc' (Doctor n seasons : rest) = li $ a ("#doctor" ++ show n) (ordinal n ++ " Doctor")
                                     +. simplehtml "ul" (tocSeasons seasons)
                                     +. toc' rest
    tocSeasons [] = ""
    tocSeasons (Season n _ : rest) = li $ a ("#season-" ++ n) ("Season " ++ n)
                                     +. tocSeasons rest

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- Commands that can be run
data Command = Output | AddDoctor | AddSeason | AddStory | Usage

-- | Run a command on a table
run :: Command -> Maybe FilePath -> Table -> IO ()
run Usage _ _       = do args <- getArgs
                         putStrLn $ "did not recognise args: " ++ concat args
                         putStrLn "dw Output     : outputs the table as html"
                         putStrLn "dw add doctor : adds a new doctor to the table"
                         putStrLn "dw add season : adds a new season to the table (interactive)"
                         putStrLn "dw add story  : adds a new story to the table (interactive)"
                         putStrLn "dw edit story : edit a particular story"
                         putStrLn ""
                         putStrLn $ "The table is stored in " ++ file
run Output Nothing table           = putStrLn $ output table
run Output (Just outputFile) table = writeFile outputFile (output table)
run AddDoctor _ table = do writeOut $ addDoctor table
run AddSeason _ table = do doctor <- prompt "To doctor: "
                           season <- prompt "Season Number: "
                           let result = addSeason (read doctor) season table
                           case result of
                             Just newTable -> do writeOut newTable
                             Nothing -> print "couldn't add season!"
run AddStory _ table  = do season <- prompt "Season Number: "
                           name <- prompt "Name: "
                           number <- prompt "Number: "
                           numEpisodes <- prompt "Number of Episodes: "
                           missing <- prompt "Missing: "
                           recommendation <- prompt "Recommendation: "
                           note <- prompt "Note: "
                           synopsis <- prompt "Synopsis: "
                           review <- prompt "Review: "
                           let result = addStory (Story name (read number) (read numEpisodes) (read missing) (read recommendation) (readNote note) synopsis review Nothing) season table
                           case result of
                             Just newTable -> do writeOut newTable
                             Nothing       -> print "couldn't add story!"


realMain :: IO ()
realMain = do arg <- getArgs
              hSetBuffering stdin NoBuffering
              hSetBuffering stdout NoBuffering
              fileContent <- ByteString.readFile file
              Just table <- Yaml.decodeThrow fileContent
              case arg of
                ["output"]             -> run Output Nothing table
                ["output", outputFile] -> run Output (Just outputFile) table
                ["add", "doctor"]      -> run AddDoctor Nothing table
                ["add", "season"]      -> run AddSeason Nothing table
                ["add", "story"]       -> run AddStory Nothing table
                _                      -> run Usage Nothing table



-- | Prompt the user for a response
prompt :: String -> IO String
prompt text = putStr text >> getLine >>= return

file, tmpfile, backup :: FilePath
file = "DrWhoDB.yaml"
tmpfile = "DrWhoDB_tmp"
backup = "DrWhoDB.bak"


readNote :: String -> Maybe Note
readNote "" = Nothing
readNote n  = Just n


writeOut :: Table -> IO ()
writeOut table = do ByteString.writeFile tmpfile (Yaml.encode table)
                    renameFile file backup
                    renameFile tmpfile file
