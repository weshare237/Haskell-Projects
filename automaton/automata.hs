--Data structure that represents an automaton (nfa or dfa)
data Automaton q s = Automaton {
                       states   :: [q],
                       alphabet :: [s],
                       initial  :: q,
                       finals   :: [q],
                       delta    :: q -> Maybe s -> [q]
                     }