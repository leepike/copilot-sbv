module Test where

import qualified Prelude as P

import Copilot.Language.Prelude
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

fib :: Stream Word64
fib = [0, 1] ++ fib + drop 1 fib

spec :: Spec
spec = do
  trigger "trig1" alt3 [arg (3::Stream Word64)]
  trigger "trig_fib" (fib < 7) [arg (3::Stream Word64)]

main = do 
  reify spec >>= compile "test" 

{-

(0) Go into the generated XXX directory.

(1) You need to post in something like the following in the generated
    copilot_driver_XXX.c.  Basically, define the triggers and put in a main that
    calls the main driver function.

void trig1(SWord64 x) {
  printf("trig1: %llu", x);
}

int main (void) {
  int i=0;
  for(;i<10;i++) {
    driver_test();
    printf("tmp_0: %i\n", tmp_0);
  }
  return 0;
}

(2) Then open the generated Makefile and change driver_sbv.c to copilot_driver_XXX.c

(3) Type make.

-}

