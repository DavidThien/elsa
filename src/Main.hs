
-- import Language.Elsa.Runner (topMain)
import           Language.Elsa.Synthesis
import           Language.Elsa.Encodings



main :: IO ()
-- main = print $ synthesize booleans notSpec 1000
-- main = print $ synthesize booleans andSpec 100
-- main = print $ synthesize booleans orSpec 100
-- main = print $ synthesize booleans iteSpec 1000

-- main = print $ synthesize numerals succSpec 10000
-- main = print $ synthesize numerals plusSpec 20000000
-- main = print $ synthesize numerals expSpec 100

-- main = print $ synthesize pairs firstSpec 100
-- main = print $ synthesize pairs secondSpec 100


main = print $ coSynthesize booleans boolSpec 10000
-- main = print $ coSynthesize pairs pairSpec 1000000
