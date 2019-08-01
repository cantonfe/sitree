
## It will return a list with 4data.frames
## (1) live trees
## (2) dead trees
## (3) harvested trees
sitree2dataframe <- function(tr.dt){ 
	## just to make cran happy
i.height.dm <- tree.sp <- treeid <- plot.id <- NULL


  if (!class(tr.dt) %in% c('trList', 'trListDead')) stop('the object is not of class trList')
  if (class(tr.dt) == 'trList'){
    period.names <-  names(tr.dt$data$dbh.mm)
    dbh.mm <- data.table(tr.dt$data$dbh.mm)
    dbh.mm[, treeid := tr.dt$data$treeid]
    dbh.mm[, plot.id := tr.dt$data$plot.id]
    dbh.mm[, tree.sp := tr.dt$data$tree.sp]
    height.dm <- data.table(tr.dt$data$height.dm)
    height.dm[, treeid := dbh.mm$treeid]
    dbh.mm <- data.table::melt(dbh.mm  ,
                               measure.vars = period.names,
                               variable.name = 'period',
                               value.name = "dbh.mm")
    height.dm <- data.table::melt(height.dm  ,
                                  measure.vars = period.names,
                                  variable.name = 'period',
                                  value.name = "height.dm")
    
    dbh.mm[height.dm, height.dm := i.height.dm, on =c('treeid', 'period')]
    return(as.data.frame(dbh.mm))
  }
  if (class(tr.dt) == 'trListDead'){ ## for dead and harvested
    dead <- tr.dt$last.measurement
    dead$tree.sp <- tr.dt$data$tree.sp
    return(dead)
  }
  
}

sitree2dataframe.all <- function(sitree.res){
  list(
    live = sitree2dataframe(sitree.res$live),
    dead = sitree2dataframe(sitree.res$dead),
    removed =sitree2dataframe(sitree.res$removed)
  ) 
}

## dcast
## melt
