{-# LANGUAGE DeriveGeneric #-}
import Reflex.Dom
import Reflex.Forms
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import GHC.Generics

data Colour = Red | Blue | Green
  deriving (Show, Generic)

instance ToWidget Colour

data Foo = Foo
  { intF :: Int
  , locTime :: LocalTime
  , rgb :: Colour
  , days :: [Day]
  } deriving (Generic, Show)

instance ToWidget Foo

main :: IO ()
main = mainWidget $ do
  foo  <- toWidget (Nothing :: Maybe Foo)
  display foo
