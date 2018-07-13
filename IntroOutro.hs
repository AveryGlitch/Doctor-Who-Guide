module IntroOutro where

import HTML
import Prelude hiding (div)

introduction :: String
introduction
  = h1 "Avery's Doctor Who Guide"
    +. p "So, you want to watch Doctor Who, through the classic and modern era, but you're not so sure on how much to watch? You've come to the right place! This guide has several different tracks, depending on what you're interested in"
    +. "<table>"
    +. concatMap (tr' "intro")
    [
      (td' "Highly" "Fast Track"
           ++ td' "invisible" "The Highly recommended episodes. If you only want a small sampling of episodes, look here!")
    , (td' "Yes" "Recommended Track"
           +. td' "invisible" "For most people, you'll want to stick on the Recommended track - watch both the Fast track episodes and the recommended episodes (don't forget the partials, see the next section), and you'll get quite a lot of Doctor Who, without having to sit through the slower stuff.")
    , (td' "Maybe" "Maybe Track"
          +. td' "invisible" "If you're interested in a more thorough watch through, you can also watch the episodes on the maybe track. These aren't bad episodes by any right - they're just not neccessary to watch")
    , (td' "No" "Avoid"
          +. td' "invisible" "These episodes are only recommended if you're truly curious and dedicated.")
    ]
    +. "</table>"
    +. p "Additionally, some stories are marked as a <span class=Partial>partial watch</span> - this means you <strong>should</strong> watch it, but not all of it - just certain episodes."
    +. p "Many of the early episodes are missing. You will be able to tell which ones these are because the name of the story will be in italics, and it will be mentioned several times. These stories aren't unwatchable, surprisingly - reconstructions of the episodes have been made, and they are (relatively) watchable. If you don't want to watch the reconstuctions, though (and I don't blame you), they are easy to skip."
    +. p "\"Wait, but what if I want to watch <strong>everything</strong>?\" go ahead! There's nothing stopping you. But this guide is for people who want a more selective sampling of the series, or for those who will watch every episode, you can use this guide as a litmus test."
    +. p "This guide is currently a work in progress, and only goes as far as I've watched so far. I started watching through the episodes for this guide in early May 2018, and I'm still going strong."
    +. div "dimbox" (
      h3 "Important note"
      +. p "The early doctor who episodes are <i>excruciatingly</i> slow compared to what we see on modern TV, so I've judged them less harshly on pacing. <strong>I won't blame you for skipping the black and white episodes</strong>"
      +. p "I think my ratings for the black and white episodes are also the ones which have generated the most controversy - fans want me to rate more of them higher, average people think I should rate more of them lower."
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
