
sitree.summary <- function(sitrees.res, plots, by.stand = TRUE, plot = FALSE,
                           plot.all.together = FALSE){

  nplots <- length(sitrees.res$fl$ustandID)
  res <- list()
  
  ## plot1 -- BA per ha
  res$i.stand <- match(sitrees.res$live$data$ustandID, sitrees.res$fl$ustandID)
  ## NA mean no trees in that plot
  res$i.tree <- match(sitrees.res$fl$ustandID, sitrees.res$liv$data$ustandID)
  res$tree2ha <- 10000 / sitrees.res$fl$plot.size.m2
  res$SBA.m2.ha <- res$stems.ha <- res$heights.10 <-
    data.frame(t0 = matrix(NA, nrow = nplots))
  ## initialize my.plots
  my.plots <- list(plot1 = NULL, plot2 = NULL, plot3 = NULL, plot4 = NULL,
                   plot5 = NULL)
  

  for (i in 0:sitrees.res$live$nperiods){
    this.period <- paste0("t", i)
    res$tree.BA.m2 <- pi * (sitrees.res$live$data$dbh.mm[, this.period]/1000/2)^2
  
    ## SBA.m2.ha
    if (1 %in% plots){
      sa <-
        aggregate(res$tree.BA.m2 * res$tree2ha[res$i.stand],
                  by = list(ustandID =  sitrees.res$live$data$ustandID),
                  FUN = sum)
      res$SBA.m2.ha[, this.period] <-  sa$x[ match(sitrees.res$fl$ustandID,
                                                   sa$ustandID)]
    }
    
    if (2 %in% plots){
      ## stems.ha
      living.trees <- (sitrees.res$live$data$dbh.mm[, this.period] > 0)
      sa <-
      aggregate(res$tree2ha[res$i.stand][living.trees],
                by = list(ustandID =
                            sitrees.res$live$data$ustandID[living.trees]),
                FUN = sum)
      res$stems.ha[, this.period] <-  sa$x[ match(sitrees.res$fl$ustandID,
                                                  sa$ustandID)]
    }
    
    if (3 %in% plots){ ## height of 10 tallest trees
      ## stems.ha
      heights <- data.frame(
        heights = sitrees.res$live$data$height.dm[, this.period],
        ustandID = sitrees.res$live$data$ustandID
      )

      heights <- heights[heights$heights != 0,]
      heights <- heights[order(heights$ustandID, heights$heights),]
      heights <- aggregate(heights ~ ustandID, data = heights,
                           FUN = function(x) ifelse (length(x) > 9,
                                                     mean(x[1:10]),
                                                     mean(x[1:length(x)])
                                                     )
                           )
      res$heights.10[, this.period] <-
        heights$heights [ match(sitrees.res$fl$ustandID,
                                heights$ustandID)]
    }
  }

  if (4 %in% plots){
    ## number of dead trees
    dead.trees <- data.frame(
      period   = sitrees.res$dead$last.measurement$found.dead,
      ustandID = sitrees.res$dead$data$ustandID)
    
    num.dead.trees.ha <-
      aggregate(dead.trees$period,
                by = list(ustandID = dead.trees$ustandID,
                          period = dead.trees$period),
                FUN = length)
    num.dead.trees.ha$x <- num.dead.trees.ha$x *
      res$tree2ha[match(num.dead.trees.ha$ustandID, sitrees.res$fl$ustandID)]
  }
  
  if (5 %in% plots){
    ## number of removed trees
    removed.trees <- data.frame(
      period   = sitrees.res$removed$last.measurement$found.removed,
      ustandID = sitrees.res$removed$data$ustandID)
    
    num.removed.trees.ha <-
      aggregate(removed.trees$period,
                by = list(ustandID = removed.trees$ustandID,
                          period = removed.trees$period),
                FUN = length)
    num.removed.trees.ha$x <- num.removed.trees.ha$x *
      res$tree2ha[match(num.removed.trees.ha$ustandID, sitrees.res$fl$ustandID)]
  }
  
  
 ## plot1 -- SBA.m2.ha
  ## we need to make the data long
  SBA.m2.ha <- reshape( res$SBA.m2.ha, 
                       varying = names(res$SBA.m2.ha), 
                       timevar = "period",
                       direction = "long", sep = "")
  SBA.m2.ha$period <- factor(paste0("t", SBA.m2.ha$period))
  if (by.stand){
    my.plots$plot1 <- xyplot(t ~ period, groups = SBA.m2.ha$id,
                             data = SBA.m2.ha, ylab = "SBA.m2.ha", type = "l")
  } else{
    SBA.m2.ha$plot.size.m2 <-
      sitrees.res$fl$plot.size.m2[match(SBA.m2.ha$id,
                                        sitrees.res$fl$ustandID)]
    SBA.m2.ha$SBA.m2 <- with(SBA.m2.ha, t / 10000 * plot.size.m2)
    SBA.m2.ha <- aggregate( SBA.m2 ~ period, data = SBA.m2.ha, FUN = sum)
    my.plots$plot1 <- xyplot(SBA.m2 ~ period, 
                             data = SBA.m2.ha, ylab = "SBA.m2", type = "l")
  }
  
  ## plot2 -- stems per ha
  ## we need to make the data long
  stems.ha <- reshape( res$stems.ha, 
                       varying = names(res$stems.ha), 
                       timevar = "period",
                      direction = "long", sep = "")
  stems.ha$period <- factor(paste0("t", stems.ha$period))
  if (by.stand){
    my.plots$plot2 <- xyplot(t ~ period, groups = stems.ha$id,
                             data = stems.ha, ylab = "stems/ha", type = "l")
  } else {
    stems.ha$plot.size.m2 <-
      sitrees.res$fl$plot.size.m2[match(stems.ha$id,
                                        sitrees.res$fl$ustandID)]
    stems.ha$stems <- with(stems.ha, t / 10000 * plot.size.m2)
    stems.ha <- aggregate( stems ~ period, data = stems.ha, FUN = sum)
    my.plots$plot2 <- xyplot(stems ~ period, groups = stems.ha$id,
                             data = stems.ha, ylab = "stems", type = "l")
  }
  
  ## plot3 -- height of 10 tallest trees
  heights.10 <- reshape( res$heights.10, 
                       varying = names(res$heights.10), 
                       timevar = "period",
                       direction = "long", sep = "")
  heights.10$period <- factor(paste0("t", heights.10$period))
  
  my.plots$plot3 <- xyplot(t ~ period, groups = heights.10$id, data = heights.10,
                           type = 'l',
                           ylab = "Average height of the 10 tallest trees")
  
  
  ## plot4 --number of dead
  if (by.stand){
    my.plots$plot4 <- xyplot(x ~ period, groups =  num.dead.trees.ha$ustandID,
           data = num.dead.trees.ha, main = "dead trees per ha", type = "l")
  } else {
    num.dead.trees.ha$plot.size.m2 <-
      sitrees.res$fl$plot.size.m2[match(num.dead.trees.ha$ustandID,
                                        sitrees.res$fl$ustandID)]
    num.dead.trees.ha$stems <- with(num.dead.trees.ha, x / 10000 * plot.size.m2)
    num.dead.trees.ha <- aggregate( stems ~ period,
                                   data = num.dead.trees.ha, FUN = sum)
    my.plots$plot4 <- xyplot(stems ~ period, 
           data = num.dead.trees.ha, main = "number of dead trees", type = "l")
  }
  
  ## plot5 --number of trees harvest
  if (by.stand){
    my.plots$plot5 <- xyplot(x ~ period , data = num.removed.trees.ha,
           groups =  num.removed.trees.ha$ustandID,
           main = "removed trees per ha", type = "b",
           ylab = "number of trees removed")
  } else {
    num.removed.trees.ha$plot.size.m2 <-
      sitrees.res$fl$plot.size.m2[match(num.removed.trees.ha$ustandID,
                                        sitrees.res$fl$ustandID)]
    num.removed.trees.ha$stems <-
      with(num.removed.trees.ha, x / 10000 * plot.size.m2)
    num.removed.trees.ha <- aggregate( stems ~ period,
                                      data = num.removed.trees.ha, FUN = sum)
    my.plots$plot5 <- xyplot(stems ~ period , data = num.removed.trees.ha,
           
           main = "removed trees", type = "b",
           ylab = "number of trees removed")
  }


  
  if (plot) {
    if (plot.all.together & length(my.plots) > 1){

     
      if (by.stand){
         xx <- list(SBA.m2.ha = SBA.m2.ha[,c("period", "t", "id")],
                   stems.ha = stems.ha[,c("period", "t", "id")],
                   heights.10 = heights.10[,c("period", "t", "id")],
                   num.dead.trees.ha =
                     num.dead.trees.ha[,c("period", "x", "ustandID")],
                   num.removed.trees.ha =
                     num.removed.trees.ha[,c("period", "x", "ustandID")]
                   )
        xx[!1:5 %in% plots] <- NULL
        
        xx <- lapply(xx, FUN = function(x) {
          names(x) <- c("period", "t", "id")
          return(x)
        })
        xx <- rbindlist(xx, idcol = 'id.plot')
        
        print(xyplot(t ~ period|id.plot, data = xx, type = "b", groups = xx$id,
               scales = list (y = list(relation = "free"))))
        
      } else {
        xx <- list(SBA.m2 = SBA.m2.ha[,c("period", "SBA.m2")],
                   stems = stems.ha[,c("period", "stems")],
                   heights.10.tallest = heights.10[,c("period", "t", "id")],
                   num.dead.trees =
                     num.dead.trees.ha[,c("period", "stems")],
                   num.removed.trees =
                     num.removed.trees.ha[,c("period", "stems")]
                   )
        xx[!1:5 %in% plots] <- NULL
        
        xx <- lapply(xx, FUN = function(x) {
          if (ncol(x) == 2) x$id <- 1
          names(x) <- c("period", "t", "id")
          return(x)
        })
        xx <- rbindlist(xx, idcol = 'id.plot')
        print(xyplot(t ~ period|id.plot, data = xx, type = "b", groups = xx$id,
               scales = list (y = list(relation = "free")), ylab = ""))
        
      }## end if by.stand
      
    } else { ## if plot.all.together
      for (i in 1:length(my.plots)){
        print(my.plots[[i]])
        cat ("Press [enter] to continue")
        line <- readline()
      }
    } ## end else (if plot.all.together)
  }## end if plot
  invisible(my.plots)
}
