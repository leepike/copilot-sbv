module TestSBV where


import qualified Prelude as P

import Copilot.Language.Prelude --hiding (even, odd)
import Copilot.Language
import Copilot.Compile.SBV
import Copilot.Language.Reify (reify)

nats :: Stream Word64
nats = [0] ++ nats + 1

alt :: Stream Bool
alt = [True] ++ not alt

alt2 :: Stream Word64
alt2 = [0,1,2] ++ alt2 + 1

alt3 :: Stream Bool
alt3 = [True,True,False] ++ alt3

spec :: Spec
spec = do
  trigger "trig1" (alt3) [arg (3::Stream Word64)]
--  trigger "trig2" true [arg (4::Stream Word64)]

main = do 
  reify spec >>= compile "sbv" 


