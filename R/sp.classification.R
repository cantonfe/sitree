  sp.classification <-
  function (tree.sp, treslag.gran, treslag.furu, treslag.lauv) 
  {
    sp.clas <- factor(rep("other", length(tree.sp)),
                      levels = c("spruce", 
                                 "pine", "birch", "other"))
    sp.clas[tree.sp %in% treslag.gran] <- "spruce"
    sp.clas[tree.sp %in% treslag.furu] <- "pine"
    sp.clas[tree.sp %in% treslag.lauv] <- "birch"
    
    return(sp.clas)
  }
## reassignInPackage("sp.classification", "sitree", sp.classification)
