--Data structure that represents an automaton (nfa or dfa)
data Automaton q s = Automaton {
                       states   :: [q],
                       alphabet :: [s],
                       initial  :: q,
                       finals   :: [q],
                       delta    :: q -> Maybe s -> [q]
                     }

--An example of dfa that accepts set of all strings over {0, 1} where number of 1's are even
nfa1 :: Automaton Char Int
nfa1 = Automaton states alphabet initial finals delta
       where states             = ['A', 'B']
             alphabet           = [0, 1]
             initial            = 'A'
             finals             = ['A']
             delta 'A' (Just 0) = ['A']
             delta 'A' (Just 1) = ['B']
             delta 'B' (Just 0) = ['B']
             delta 'B' (Just 1) = ['A']
           --delta 'A' Nothing  = ['B']
             delta _   _        = [] 

--Function that checks if a given automaton is deterministic or not
isDeterministic     :: Automaton q s -> Bool
isDeterministic auto = all (\x -> length x == 0) epsillonTrans && all (\x -> length x == 1) alphaTrans
                       where epsillonTrans = [delta auto q Nothing | q <- states auto]
                             alphaTrans    = [delta auto q (Just s) | q <- states auto, s <- alphabet auto]

accept          :: (Eq q) => Automaton q s -> [s] -> Bool
accept auto word = or (acceptFrom auto word q)
                   where q = initial auto

flattern :: [[a]] -> [a]
flattern  = foldr (++) []

 
acceptFrom              :: (Eq q) => Automaton q s -> [s] -> q -> [Bool]
acceptFrom auto [] z     = [z `elem` (finals auto)]
acceptFrom auto (x:xs) z = flattern (map (acceptFrom auto xs) ys) ++ flattern (map (acceptFrom auto (x:xs)) zs)
                           where ys = delta auto z (Just x)
                                 zs = delta auto z Nothing

