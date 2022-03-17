data Regex a = Epsillon 
             | Litteral a 
             | Or Regex Regex 
             | Concat Regex Regex 
             | Star Regex 
             | OneOrMore Regex 
             | Optional Regex 