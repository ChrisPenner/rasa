module Rasa.Testing (
  -- testBufAction
  ) where

-- import Eve
-- import Rasa.Internal.Interpreters
-- import Rasa.Internal.Buffer

-- import Test.Hspec
-- import Control.Monad
-- import qualified Yi.Rope as Y

-- testBufAction :: (Show a, Eq a) => String -> Y.YiString -> a -> BufAction a -> Spec
-- testBufAction description txt expectation bufAction = join . runIO $
--   bootstrapAction $ do
--     ref <- addBuffer txt
--     mResult <- bufDo ref bufAction
--     case mResult of
--       Nothing -> error "(Err #75b72c) Buffer not found by BufRef"
--       Just res -> return $ it description (res `shouldBe` expectation)
