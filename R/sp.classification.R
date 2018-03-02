sp.classification <-
  function (tree.sp, species.spruce, species.pine, species.harw) 
    {
      if (is.null(species.spruce)) stop("species.spruce is NULL")
      if (is.null(species.pine)) stop("species.pine is NULL")
      if (is.null(species.harw)) stop("species.harw is NULL")
    sp.clas <- factor(rep("other", length(tree.sp)),
                      levels = c("spruce", 
                                 "pine", "birch", "other"))
    sp.clas[tree.sp %in% species.spruce] <- "spruce"
    sp.clas[tree.sp %in% species.pine] <- "pine"
    sp.clas[tree.sp %in% species.harw] <- "birch"
    
    return(sp.clas)
  }
## reassignInPackage("sp.classification", "sitree", sp.classification)
