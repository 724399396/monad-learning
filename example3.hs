traceFamily :: Sheep -> [(Sheep -> Maybe Sheep)] -> Maybe Sheep
traceFamily s l = foldM getParent s l
  where getParent s f = f s
  
mothersPaternalGrandfather s = traceFamily s [mother, father, father]
paternalGrandmother s = traceFamily s [father, mother]
