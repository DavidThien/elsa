
-- import Language.Elsa.Runner (topMain)
import Language.Elsa.Synthesis
import Language.Elsa.Encodings


main :: IO ()
-- main = topMain
main = print $ synthesize numerals plusSpec 20000000
