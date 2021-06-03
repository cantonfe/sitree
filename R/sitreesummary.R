

sitree.summary <- function(sitrees.res, plots, by.stand = TRUE, plot = FALSE,
                           plot.all.together = FALSE){

  nplots <- length(sitrees.res$plot.data$plot.id)
  res <- list()
  
  ## plot1 -- BA per ha
  res$i.stand <- match(sitrees.res$live$data$plot.id,
                       sitrees.res$plot.data$plot.id)
  ## NA mean no trees in that plot
  res$i.tree <- match(sitrees.res$plot.data$plot.id,
                      sitrees.res$liv$data$plot.id)
  res$tree2ha <- 10000 / sitrees.res$plot.data$plot.size.m2
  res$SBA.m2.ha <- res$stems.ha <- res$heights.10 <-
    data.frame(t0 = matrix(NA, nrow = nplots))
  ## initialize my.plots
  my.plots <- list(plot1 = NULL, plot2 = NULL, plot3 = NULL, plot4 = NULL,
                   plot5 = NULL)
  data.summary <- list()
  

  for (i in 0:sitrees.res$live$nperiods){
    this.period <- paste0("t", i)
    res$tree.BA.m2 <- pi * (sitrees.res$live$data$dbh.mm[, this.period]/1000/2)^2
  
    ## SBA.m2.ha
    if (1 %in% plots){
      sa <-
        aggregate(res$tree.BA.m2 * res$tree2ha[res$i.stand],
                  by = list(plot.id =  sitrees.res$live$data$plot.id),
                  FUN = sum)
      res$SBA.m2.ha[, this.period] <-  sa$x[ match(sitrees.res$plot.data$plot.id,
                                                   sa$plot.id)]
    }
    
    if (2 %in% plots){
      ## stems.ha
      living.trees <- (sitrees.res$live$data$dbh.mm[, this.period] > 0)
      sa <-
      aggregate(res$tree2ha[res$i.stand][living.trees],
                by = list(plot.id =
                            sitrees.res$live$data$plot.id[living.trees]),
                FUN = sum)
      res$stems.ha[, this.period] <-  sa$x[ match(sitrees.res$plot.data$plot.id,
                                                  sa$plot.id)]
    }
    
    if (3 %in% plots){ ## height of 10 tallest trees
      ## stems.ha
      heights <- data.frame(
        heights = sitrees.res$live$data$height.dm[, this.period],
        plot.id = sitrees.res$live$data$plot.id
      )

      heights <- heights[heights$heights != 0,]
      heights <- heights[order(heights$plot.id, heights$heights),]
      heights <- aggregate(heights ~ plot.id, data = heights,
                           FUN = function(x) ifelse (length(x) > 9,
                                                     mean(x[1:10]),
                                                     mean(x[1:length(x)])
                                                     )
                           )
      res$heights.10[, this.period] <-
        heights$heights [ match(sitrees.res$plot.data$plot.id,
                                heights$plot.id)]
    }
  }

  if (4 %in% plots){
    ## number of dead trees
    dead.trees <- data.frame(
      period   = sitrees.res$dead$last.measurement$found.dead,
      plot.id = sitrees.res$dead$data$plot.id)
    
    num.dead.trees.ha <-
      aggregate(dead.trees$period,
                by = list(plot.id = dead.trees$plot.id,
                          period = dead.trees$period),
                FUN = length)
    num.dead.trees.ha$x <- num.dead.trees.ha$x *
      res$tree2ha[match(num.dead.trees.ha$plot.id, sitrees.res$plot.data$plot.id)]
  }
  
  if (5 %in% plots){
    ## number of removed trees
    removed.trees <- data.frame(
      period   = sitrees.res$removed$last.measurement$found.removed,
      plot.id = sitrees.res$removed$data$plot.id)
    
    num.removed.trees.ha <-
      aggregate(removed.trees$period,
                by = list(plot.id = removed.trees$plot.id,
                          period = removed.trees$period),
                FUN = length)
    num.removed.trees.ha$x <- num.removed.trees.ha$x *
      res$tree2ha[match(num.removed.trees.ha$plot.id, sitrees.res$plot.data$plot.id)]
  }
  
  my.period.levels <- paste0("t", 0:sitrees.res$live$nperiods)
  ## plot1 -- SBA.m2.ha
  if (1 %in% plots){
  ## we need to make the data long
    SBA.m2.ha <- reshape( res$SBA.m2.ha, 
                         varying = names(res$SBA.m2.ha), 
                         timevar = "period",
                         direction = "long", sep = "")
    SBA.m2.ha$period <- factor(paste0("t", SBA.m2.ha$period),
                               levels = my.period.levels)
    SBA.m2.ha$plot.id <- as.factor(SBA.m2.ha$id)

    if (by.stand){
      my.plots$plot1 <- ggplot(SBA.m2.ha, aes(x = period, y = t, color = plot.id,group=plot.id)) +
        geom_line() + ylab("SBA.m2.ha")

    } else{
      SBA.m2.ha$plot.size.m2 <-
        sitrees.res$plot.data$plot.size.m2[match(SBA.m2.ha$id,
                                                 sitrees.res$plot.data$plot.id)]
      SBA.m2.ha$SBA.m2 <- with(SBA.m2.ha, t / 10000 * plot.size.m2)
      SBA.m2.ha <- aggregate( SBA.m2 ~ period, data = SBA.m2.ha, FUN = sum)

      my.plots$plot1 <-
        ggplot(SBA.m2.ha, aes(period, SBA.m2, group = 1))   + geom_line() + ylab("SBA.m2.ha")
      
    }
    data.summary$SBA.m2.ha <- SBA.m2.ha
  }
  
  ## plot2 -- stems per ha
  ## we need to make the data long
  if (2 %in% plots){
    stems.ha <- reshape( res$stems.ha, 
                        varying = names(res$stems.ha), 
                        timevar = "period",
                        direction = "long", sep = "")
    stems.ha$period <- factor(paste0("t", stems.ha$period),
                              levels = my.period.levels)
   
    if (by.stand){
      my.plots$plot2 <- ggplot(stems.ha, aes(x = period, y = t, color = plot.id, group=plot.id)) +
        geom_line() + ylab("stems/ha")
    } else {
      stems.ha$plot.size.m2 <-
        sitrees.res$plot.data$plot.size.m2[match(stems.ha$id,
                                                 sitrees.res$plot.data$plot.id)]
      stems.ha$stems <- with(stems.ha, t / 10000 * plot.size.m2)
      stems.ha <- aggregate( stems ~ period, data = stems.ha, FUN = sum)
      my.plots$plot2 <-
        ggplot(stems.ha ,aes(period, stems, group = 1))   + geom_line() + ylab("stems/ha")
      
      
    }
    data.summary$stems.ha <- stems.ha
  }
  
  ## plot3 -- height of 10 tallest trees
  if (3 %in% plots){
    heights.10 <- reshape( res$heights.10, 
                          varying = names(res$heights.10), 
                          timevar = "period",
                          direction = "long", sep = "")
    heights.10$period <- factor(paste0("t", heights.10$period),
                               levels = my.period.levels)
    heights.10$plot.id <- as.factor(heights.10$id)
    
    my.plots$plot3 <- ggplot(heights.10, aes(x = period, y = t, color = plot.id, group=plot.id)) +
        geom_line() + ylab("Average height of the 10 tallest trees")
    data.summary$heights.10 <- heights.10
  }
  
  ## plot4 --number of dead
  if (4 %in% plots){
    if (by.stand){
      num.dead.trees.ha$plot.id <- as.factor(num.dead.trees.ha$plot.id)

      my.plots$plot4 <-
        ggplot(num.dead.trees.ha, aes(x = period, y = x, color = plot.id,group=plot.id)) +
        geom_line() + ylab( "dead trees per ha")
      
    } else {
      num.dead.trees.ha$plot.size.m2 <-
        sitrees.res$plot.data$plot.size.m2[match(num.dead.trees.ha$plot.id,
                                                 sitrees.res$plot.data$plot.id)]
      num.dead.trees.ha$stems <- with(num.dead.trees.ha, x / 10000 * plot.size.m2)
      num.dead.trees.ha <- aggregate( stems ~ period,
                                     data = num.dead.trees.ha, FUN = sum)
      my.plots$plot4 <-
        ggplot(num.dead.trees.ha, aes(period, stems, group = 1))   +
        geom_line() + ylab("dead trees per ha")
      
    }
    data.summary$num.dead.trees.ha <- num.dead.trees.ha
  }
  
  ## plot5 --number of trees harvest
  if (5 %in% plots){
    if (by.stand){
      num.removed.trees.ha$plot.id <- as.factor(num.removed.trees.ha$plot.id)
       
      my.plots$plot5 <-
        ggplot(num.removed.trees.ha, aes(x = period, y = x, color = plot.id,group=plot.id)) +
        geom_line() + ylab( "removed trees per ha")
    } else {
      num.removed.trees.ha$plot.size.m2 <-
        sitrees.res$plot.data$plot.size.m2[match(num.removed.trees.ha$plot.id,
                                                 sitrees.res$plot.data$plot.id)]
      num.removed.trees.ha$stems <-
        with(num.removed.trees.ha, x / 10000 * plot.size.m2)
      num.removed.trees.ha <- aggregate( stems ~ period,
                                        data = num.removed.trees.ha, FUN = sum)
      my.plots$plot5 <-
        ggplot(num.removed.trees.ha,
               aes(period, stems, group = 1))   + geom_line() + ylab("removed trees per ha")
      



    }
    data.summary$num.removed.trees.ha <- num.removed.trees.ha
  }
    
  
  if (plot) {
    if (plot.all.together & length(my.plots) > 1){
     
      if (by.stand){
        if(1 %in% plots) SBA.m2.ha <- SBA.m2.ha[,c("period", "t", "id")] else {
          SBA.m2.ha <- NA}
        if(2 %in% plots) stems.ha <- stems.ha[,c("period", "t", "id")] else {
          stems.ha <- NA}
        if(3 %in% plots) heights.10 <- heights.10[,c("period", "t", "id")] else {
          heights.10 <- NA}
        if(4 %in% plots) {
          num.dead.trees.ha <- num.dead.trees.ha[,c("period", "x", "plot.id")]
          } else num.dead.trees.ha <- NA
        if(5 %in% plots) {
          num.removed.trees.ha <- num.removed.trees.ha[,c("period", "x",
                                                          "plot.id")]
          } else num.removed.trees.ha <- NA
        
        xx <- list(
          SBA.m2.ha = SBA.m2.ha,
          stems.ha = stems.ha,
          heights.10 =  heights.10,
          num.dead.trees.ha = num.dead.trees.ha,
          num.removed.trees.ha = num.removed.trees.ha
        )
        xx[!1:5 %in% plots] <- NULL
        
        xx <- lapply(xx, FUN = function(x) {
          names(x) <- c("period", "t", "id")
          return(x)
        })
        xx <- rbindlist(xx, idcol = 'id.plot')
       
       

        my.plots[['all_plots_figure']]  <- ggplot(xx, aes(period, t, group = id, col = id))   + geom_line()+
          facet_wrap(~id.plot, scales = "free") + ylab("")
        
        
      } else {
        if(1 %in% plots) SBA.m2 <- SBA.m2.ha[,c("period", "SBA.m2")] else {
          SBA.m2.ha <- NA}
        if(2 %in% plots) stems <- stems.ha[,c("period", "stems")] else {
          stems.ha <- NA}
        if(3 %in% plots) heights.10 <- heights.10[,c("period", "t", "id")] else {
          heights.10 <- NA}
        if(4 %in% plots) {
          num.dead.trees.ha <- num.dead.trees.ha[,c("period", "stems")]
          } else num.dead.trees.ha <- NA
        if(5 %in% plots) {
          num.removed.trees.ha <- num.removed.trees.ha[,c("period", "stems")]
          } else num.removed.trees.ha <- NA
        
        xx <- list(
          SBA.m2.ha = SBA.m2,
          stems.ha = stems,
          heights.10 =  heights.10,
          num.dead.trees.ha = num.dead.trees.ha,
          num.removed.trees.ha = num.removed.trees.ha
          )
          
        xx[!1:5 %in% plots] <- NULL
        
        xx <- lapply(xx, FUN = function(x) {
          if (ncol(x) == 2) x$id <- 1
          names(x) <- c("period", "t", "id")
          return(x)
        })
        xx <- rbindlist(xx, idcol = 'id.plot')

        xx$id  <-  as.factor(xx$id)
        
        my.plots[['all_plots_figure']]  <-  ggplot(xx, aes(period, t, group = id, col = id))   + geom_line()+
            facet_wrap(~id.plot, scales = "free") + ylab("")
        
     
            
      }## end if by.stand
      
    } else { ## if plot.all.together
      for (i in 1:length(my.plots)){
        print(my.plots[[i]])
        cat ("Press [enter] to continue")
        line <- readline()
      }
    } ## end else (if plot.all.together)
  }## end if plot
  
  attr(my.plots,'data') <- data.summary
  invisible(my.plots)
}
## reassignInPackage("sitree.summary", "sitree", sitree.summary)
