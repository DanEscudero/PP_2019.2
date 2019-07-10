module Test where

import Cesar

prop_neg_shift :: Int -> Char -> Bool
prop_neg_shift d c = shift d (shift (-d) c) == c

prop_enc_length :: Int -> String -> Bool
prop_enc_length  d xs = length xs == length (encode d xs)

prop_enc_dec :: Int -> String -> Bool
prop_enc_dec d xs = xs == encode (-d) (encode d xs)