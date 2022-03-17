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