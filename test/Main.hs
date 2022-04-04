import           Test.DocTest

main :: IO ()
main = doctest [ "-XDataKinds"
               , "-XTypeOperators"
               , "-isrc" , "test/Test.hs"
               ]
