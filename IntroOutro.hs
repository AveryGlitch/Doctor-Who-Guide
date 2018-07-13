module IntroOutro where

import HTML
import Prelude hiding (div)

introduction :: String
introduction
  = h1 "Avery's Doctor Who Guide"
    +. p "So, you want to watch Classic Doctor Who but you're not so sure on how much to watch? You've come to the right place! This guide has several different tracks depending on what you're interested in:"
    +. "<table>"
    +. concatMap (tr' "intro")
    [
      (td' "Highly" "Fast Track"
           ++ td' "invisible" "The Highly recommended episodes. If you only want a small sampling of episodes, look here!")
    , (td' "Yes" "Recommended Track"
           +. td' "invisible" ("For most people: watch both the Fast Track episodes and the Recommended episodes. You'll get quite a lot of Doctor Who, without having to sit through the slower stuff."
                               +. "<br>" +. "Some stories are marked as a <span class=Partial>partial watch</span> - this means you <strong>should</strong> watch it, but not all of it - just certain episodes.")
      )
    , (td' "Maybe" "Maybe Track"
          +. td' "invisible" "If you're interested in a more thorough watch, you can also watch the episodes on the maybe track. These aren't neccesarily bad episodes, just ones that are hard to recommend.")
    , (td' "No" "Avoid"
          +. td' "invisible" "These episodes are only recommended if you're truly curious and dedicated.")
    ]
    +. "</table>"
    +. p "This guide is currently a work in progress, and only goes as far as I've watched so far. I started watching through the episodes for this guide in early May 2018, and I'm still going strong."
    +. div "dimbox" (
      h3 "Important note"
      +. p "A lot of the black and white episodes were destroyed by the BBC, so all that remains of them is fan reconstructions. The reconstructions are well made, however they can be hard to watch - don't make yourself sit through them if you're having trouble with them."
      +. p "The reconstructions are clearly marked, so you should be able to tell which ones they are."
      )

outro :: String
outro = "<hr>"
        +. div "dimbox" (
          h3 "Acknowledgements"
          +. p "Thanks to:"
          +. "<ul>"
          +. li (a "https://mastodon.social/@The_T" "@The_T@mastodon.social" ++ " for convincing me to upgrade the recommendations for The Aztecs, The Sensorites, and The Reign of Terror; as well as downgrading The Edge of Destruction")
          +. li (a "https://computerfairi.es/@nezumi" "@nezumi@computerfairi.es" ++ " for making the downgrade of The Edge of Destruction more solid, by pointing out how the plot contrivances make everyone act out of character")
          +. li (a "https://wandering.shop/@DialMForMara" "@DialMForMara@wandering.shop" ++ " for convincing me to review the reconstructions as well")
          +. li "And a bunch of others on the fediverse for helping me make the colourscheme in this document less garish."
          +. "</ul>"
          +. p ("This guide was not created manually, but was (somewhat) automated with a program I made one afternoon. You can find the source for it " ++  a "https://notabug.org/AveryLychee/Doctor-Who-Guide" "on NotABug")
        )
        +. div "return" (p $ a "../" "Return Home")
