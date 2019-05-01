module Multiple

import Recur

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix input1 input2 | with_pat = ?isSuffix_rhs_rhs
