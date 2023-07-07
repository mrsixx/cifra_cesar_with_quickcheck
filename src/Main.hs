module Main(main) where

import Test.QuickCheck
import Cesar
import Test

main :: IO ()
main = do

  let code = encode 3 "adoro haskell"
      dec = encode (-3) code
      cracked = crack code
  print ("Codificado: " ++ code)
  print ("Decodificado: " ++ dec)
  print ("Crackeado: " ++ cracked)
  print "Testando prop_neg_shift"
  quickCheck prop_neg_shift
  print "Testando prop_enc_length"
  quickCheck prop_enc_length
  print "Testando prop_enc_dec"
  quickCheck prop_enc_dec