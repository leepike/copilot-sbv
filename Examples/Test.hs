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

fib' :: Stream Word64
fib' = [0, 1] ++ fib' + drop 1 fib

spec :: Spec
spec = do
  trigger "trig1" true [arg fib]
--  trigger "trig2" true [arg fib, arg alt3]

--  trigger "trig1" true [arg fib, arg (77::Stream Word64)]
--  trigger "trig1" true [arg nats]

main = do 
  reify spec >>= compile "test1" 

{-

(0) Go into the generated XXX directory.

(1) You need to post in something like the following in the generated
    copilot_driver_XXX.c.  Basically, define the triggers and put in a main that
    calls the main driver function.

void trig1(SWord64 f, SWord64 x) {
  printf("trig1: f %llu, three %llu\n", f, x);
}

void trig2(SWord64 f0, SWord64 f1) {
  printf("trig2: f0 %llu, f1 %llu\n", f0, f1);
}

int main (void) {
  int i=0;
  for(;i<10;i++) {
    driver_test();
//    printf("tmp_0: %i\n", tmp_0);
  }
  return 0;
}

(2) Then open the generated Makefile and change test_driver.c to 
    copilot_driver_test.c

(3) Type make.

-}

