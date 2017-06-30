
recover.state <- function(tr, dead.trees, removed.trees){

  ## add dead trees
  new.trees <- list(  plot.id = dead.trees$data$plot.id
                  , treeid   = dead.trees$data$treeid
                  , dbh.mm   = dead.trees$data$dbh.mm
                  , height.dm = dead.trees$data$height.dm
                  , yrs.sim   = dead.trees$data$yrs.sim
                  , tree.sp   = dead.trees$data$tree.sp
                    )
  tr$addTrees(new.trees)
  
  ## add removed trees if there are any
  if (!is.null(removed.trees)){
    new.trees <- list(  plot.id = removed.trees$data$plot.id
                    , treeid   = removed.trees$data$treeid
                    , dbh.mm   = removed.trees$data$dbh.mm
                    , height.dm = removed.trees$data$height.dm
                    , yrs.sim   = removed.trees$data$yrs.sim
                    , tree.sp   = removed.trees$data$tree.sp
                      )
    tr$addTrees(new.trees)
  }
  dead.trees$last.measurement$treeid <- dead.trees$data$treeid
  removed.trees$last.measurement$treeid <- removed.trees$data$treeid
  attr(tr, 'last.measurement.dead') <- dead.trees$last.measurement
  attr(tr, 'last.measurement.removed') <- removed.trees$last.measurement
  
  return (tr)
}
##  reassignInPackage("recover.state", "sitree", recover.state)
