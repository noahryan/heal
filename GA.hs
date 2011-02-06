module GA(
  gaea
)
where

import EAMonad
import EA
import Loci
import Recombine
import Selection

garecomb pm pc pop = mutation pm pop >>= crossover 1 pc
gaea ps is gens pm pc eval =
  ea (mkBitPop ps is)
     (evaluate Max eval)
     tournament
     (garecomb pm pc)
     gens
     Max
     False
     (maxGens gens)
