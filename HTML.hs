module HTML where


-- | Concats two strings, but puts a newline between them
(+.) :: String -> String -> String
x +. y = x ++ "\n" ++ y

h1, h2, h3, p, tr, td, li :: String -> String
h1  = simplehtml "h1"
h2  = simplehtml "h2"
h3  = simplehtml "h2"
p   = simplehtml "p"
tr  = simplehtml "tr"
td  = simplehtml "td"
li  = simplehtml "li"
tr', td', div :: String -> String -> String
tr' = styledhtml "tr"
td' = styledhtml "td"
div = styledhtml "div"

a :: String -> String -> String
a link name = "<a href=\"" ++ link ++ "\">" ++ name ++ "</a>"

img :: String -> String -> String
img url alt = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"


simplehtml :: String -> (String -> String)
simplehtml tag = \s -> "<" ++ tag ++ ">" ++ s ++ "</" ++ tag ++ ">"
styledhtml :: String -> String -> (String -> String)
styledhtml tag style = \s -> "<" ++ tag ++ " class=\"" ++ style ++ "\">"++ s ++ "</" ++ tag ++ ">"
