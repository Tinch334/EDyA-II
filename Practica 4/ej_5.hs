import RBT

data OneTwoThree a = Leaf |
    TwoNode a (OneTwoThree a) (OneTwoThree a) |
    ThreeNode a a (OneTwoThree a) (OneTwoThree a) (OneTwoThree a) |
    FourNode a a a (OneTwoThree a) (OneTwoThree a) (OneTwoThree a) (OneTwoThree a)
    deriving Show

fromRBTtoOTT :: Ord a => RBT a -> OneTwoThree a
fromRBTtoOTT E = Leaf
fromRBTtoOTT (T _ (T _ ll vl rl) v (T _ lr vr rr)) = FourNode vl v vr (fromRBTtoOTT ll) (fromRBTtoOTT rl) (fromRBTtoOTT lr) (fromRBTtoOTT rr)
fromRBTtoOTT (T _ (T _ ll vl rl) v E) = ThreeNode vl v (fromRBTtoOTT ll) (fromRBTtoOTT rl) Leaf
fromRBTtoOTT (T _ E v (T _ lr vr rr)) = ThreeNode v vr Leaf (fromRBTtoOTT lr) (fromRBTtoOTT rr)
fromRBTtoOTT (T _ E v E) = TwoNode v Leaf Leaf

example =
  T B
    (T B
      (T R E 1 E)
      2
      (T R E 3 E)
    )
    4
    (T B
      (T R E 5 E)
      6
      (T R E 7 E)
    )

example2 =
  T B
    -- Left subtree (values 1–7)
    (T B
      (T B
        (T R E 1 E)
        2
        (T R E 3 E)
      )
      4
      (T B
        (T R E 5 E)
        6
        (T R E 7 E)
      )
    )
    8
    -- Right subtree (values 9–15)
    (T B
      (T B
        (T R E 9  E)
        10
        (T R E 11 E)
      )
      12
      (T B
        (T R E 13 E)
        14
        (T R E 15 E)
      )
    )