import Test.Hspec
--import Lib
import Opsem
import Control.Monad.Trans.State.Strict

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do 

{- VERY Basic tests -}
  describe "Basic Tests" $ do 

    it "Checks for terminal state" $ do 
      (evalState isTerminal) ([],[],emptyStore) `shouldBe` True

    it "Dereferences a variable from the store" $ do
      let s0 = ([CtlPhrase (IP (DeRef "x"))],[],(extendStore "x" 5 emptyStore))
          (ce,re,se) =  execState stepm $ s0    
      re `shouldBe` [ResPhrase (IP (IntVal 5))]
