
sitree.summary <- function(sitrees.res,  plots, by.stand = TRUE, plot = FALSE,
                           plot.all.together = FALSE, with.legend = FALSE){

  ## check if plot.size.m2
  if(! 'plot.size.m2' %in% names(sitrees.res$plot.data))
    stop('plot.data in sitrees.res$plot.data should have a field named plot.size.m2')

  plot.data <- data.frame(plot.id = sitrees.res$plot.data$plot.id,
                          plot.size.m2 = sitrees.res$plot.data$plot.size.m2)
  plot.data$tree2ha <- 10000/plot.data$plot.size.m2
  nplots <- nrow(plot.data)
  res <- list()

  ############################
  ## PREPARING THE DATA 
  ##############################
  
  ## plot1 -- BA per ha
  res$i.stand <- match(sitrees.res$live$data$plot.id,
                       plot.data$plot.id)
  ## NA mean no trees in that plot
  res$i.tree <- match(plot.data$plot.id,
                      sitrees.res$liv$data$plot.id)
  res$tree2ha <- 10000 / plot.data$plot.size.m2
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
    sa <-
      aggregate(res$tree.BA.m2 * res$tree2ha[res$i.stand],
                by = list(plot.id =  sitrees.res$live$data$plot.id),
                FUN = sum)
    res$SBA.m2.ha[, this.period] <-  sa$x[ match(plot.data$plot.id,
                                                 sa$plot.id)]
    
    
    ## stems.ha
    living.trees <- (sitrees.res$live$data$dbh.mm[, this.period] > 0)
    sa <-
      aggregate(res$tree2ha[res$i.stand][living.trees],
                by = list(plot.id =
                            sitrees.res$live$data$plot.id[living.trees]),
                FUN = sum)
    res$stems.ha[, this.period] <-  sa$x[ match(plot.data$plot.id,
                                                sa$plot.id)]
    
    ## height of 10 tallest trees
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
        heights$heights [ match(plot.data$plot.id,
                                heights$plot.id)]
  }


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
    res$tree2ha[match(num.dead.trees.ha$plot.id, plot.data$plot.id)]



  ## number of removed trees
  if (!is.null(sitrees.res$removed)){
    removed.trees <- data.frame(
      period   = sitrees.res$removed$last.measurement$found.removed,
      plot.id = sitrees.res$removed$data$plot.id)
    
    num.removed.trees.ha <-
      aggregate(removed.trees$period,
                by = list(plot.id = removed.trees$plot.id,
                          period = removed.trees$period),
                FUN = length)
    num.removed.trees.ha$x <- num.removed.trees.ha$x *
      res$tree2ha[match(num.removed.trees.ha$plot.id, plot.data$plot.id)]
  } else {
    num.removed.trees.ha <- expand.grid(plot.id = plot.data$plot.id,
                                period = colnames(sitrees.res$live$data$dbh.mm))
    num.removed.trees.ha$x <- 0
  }
  
  
  my.period.levels <- paste0("t", 0:sitrees.res$live$nperiods)

  
  ############################
  ## PREPARING THE DATA -- DIFFERENTIATE BETWEEN BY PLOT OR GLOBAL
  ##############################

  ## plot1 -- SBA.m2.ha
  if (1 %in% plots){

  ## we need to make the data long
    SBA.m2.ha <- reshape( res$SBA.m2.ha, 
                         varying = names(res$SBA.m2.ha), 
                         timevar = "period",
                         direction = "long", sep = "")
    SBA.m2.ha$period <- factor(paste0("t", SBA.m2.ha$period),
                               levels = my.period.levels)
    SBA.m2.ha <- data.table(SBA.m2.ha)
    SBA.m2.ha$plot.id <- SBA.m2.ha$id
    SBA.m2.ha[plot.data, plot.size.m2 := i.plot.size.m2, on = 'plot.id']
    SBA.m2.ha$plot.id <- as.factor(SBA.m2.ha$id)

    if (by.stand){
      plot1 <-
        ggplot(SBA.m2.ha,
               aes(x = period, y = t, color = plot.id, group=plot.id)) +
        geom_line() + ylab("SBA (m2/ha)")+ ylim(0,NA)+ xlab('Simulation period') +
        guides(color = guide_legend(title = "Plot ID"))

    } else{

      SBA.m2.ha  <- SBA.m2.ha[, list(t = weighted.mean(t, w = plot.size.m2)),
                              by = period]
     
      plot1 <-
        ggplot(SBA.m2.ha, aes(period, t, group = 1))   +
        ylim(0,NA)+ xlab('Simulation period') +
        geom_line() + ylab("Average SBA (m2/ha)")
      
    }
    if (with.legend == FALSE){
      plot1 <- plot1 + theme(legend.position="none")

    }

    my.plots$plot1 <- plot1
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
    stems.ha <- stems.ha[!is.na(stems.ha$t),]
    stems.ha <- data.table(stems.ha)
    stems.ha$plot.id <- stems.ha$id
    stems.ha[plot.data, plot.size.m2 := i.plot.size.m2, on = 'plot.id']
    stems.ha$plot.id <- as.factor(stems.ha$id)

    if (by.stand){
      
      my.plots$plot2 <-
        ggplot(stems.ha, aes(x = period, y = t,
                             color = plot.id, group = plot.id)) + xlab('Simulation period') +
        geom_line() + ylab("stems/ha")+ ylim(0,NA)+ 
        guides(color = guide_legend(title = "Plot ID"))
    } else {

      stems.ha  <- stems.ha[, list(t = weighted.mean(t, w = plot.size.m2)),
                              by = period]
     
      my.plots$plot2 <-
        ggplot(stems.ha , aes(period, t, group = 1))   +
        geom_line() +ylim(0,NA)+  xlab('Simulation period') +
        ylab("Average stems/ha")
      
      
    }
    if (with.legend == FALSE){
      my.plots$plot2 <- my.plots$plot2 + theme(legend.position="none")
      
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
    heights.10 <- data.table(heights.10)
    heights.10$plot.id <- heights.10$id
    heights.10[plot.data, plot.size.m2 := i.plot.size.m2, on = 'plot.id']
    heights.10$plot.id <- as.factor(heights.10$id)

    ## if there are no trees, we don't plot anything
    heights.10 <- heights.10[!is.na(heights.10$t),]
    
    if (by.stand){
        my.plots$plot3 <-
        ggplot(heights.10, aes(x = period, y = t, color = plot.id, group = plot.id)) +
          geom_line() + xlab('Simulation period') +
          ylab("Average height of the 10 tallest trees (dm)")+ylim(0,NA)+ 
        guides(color = guide_legend(title = "Plot ID"))
    } else {
      heights.10 <- heights.10[, list(t = weighted.mean(t, w = plot.size.m2)),
                               by = period]
    
     
      my.plots$plot3 <-
        ggplot(heights.10, aes(x = period, y = t, group = 1)) +
        geom_line() +ylim(0,NA)+  xlab('Simulation period') +
        ylab("Average height of the 10 tallest trees per plot (dm)")

    }
    
    if (with.legend == FALSE){
      my.plots$plot3 <- my.plots$plot3 + theme(legend.position="none")
      
    }
    data.summary$heights.10 <- heights.10
  }
  
  ## plot4 --number of dead
  if (4 %in% plots){
     dead.trees <- expand.grid(plot.id = plot.data$plot.id,
                               period = colnames(sitrees.res$live$data$dbh.mm))
     dead.trees$num.dead.trees.ha  <- 0
     dead.trees <- data.table(dead.trees)
     dead.trees[num.dead.trees.ha, num.dead.trees.ha := i.x, on = list(plot.id, period)]
     
     dead.trees[plot.data, tree2ha := i.tree2ha, on = 'plot.id']

     dead.trees[plot.data, plot.size.m2 := i.plot.size.m2, on = 'plot.id']
     dead.trees$plot.id <- as.factor(dead.trees$plot.id)
     
    if (by.stand){
      my.plots$plot4 <-
        ggplot(dead.trees, aes(x = period, y = num.dead.trees.ha ,
                               color = plot.id, group = plot.id)) +ylim(0,NA)+
        xlab('Simulation period') +
        geom_line() + ylab( "dead trees per ha")+
        guides(color = guide_legend(title = "Plot ID"))
      
    } else {
      num.dead.trees.ha <-
        dead.trees[,
                   list(num.dead.trees.ha =
                          weighted.mean(num.dead.trees.ha, w = plot.size.m2)),
                   by = period]
    
      my.plots$plot4 <-
        ggplot(num.dead.trees.ha, aes(period, num.dead.trees.ha, group = 1))  +
        xlab('Simulation period') +
        ylim(0,NA)+ 
        geom_line() + ylab("Average number of dead trees per ha")
      
    }
     if (with.legend == FALSE){
      my.plots$plot4 <- my.plots$plot4 + theme(legend.position="none")
      
    }
    data.summary$num.dead.trees.ha <- num.dead.trees.ha
  }
  
  ## plot5 --number of trees harvest
  if (5 %in% plots){
    ## add zeros
    rem.trees <- expand.grid(plot.id = plot.data$plot.id,
                             period = colnames(sitrees.res$live$data$dbh.mm))
    rem.trees$num.removed.trees.ha  <- 0
    rem.trees <- data.table(rem.trees)
    rem.trees[num.removed.trees.ha, num.removed.trees.ha := i.x,
              on = list(plot.id, period)]
    rem.trees[plot.data, plot.size.m2 := i.plot.size.m2, on = 'plot.id']
    rem.trees$plot.id <- as.factor(rem.trees$plot.id)
    
    if (by.stand){
      my.plots$plot5 <-
        ggplot(rem.trees, aes(x = period, y = num.removed.trees.ha, color = plot.id,
                                         group = plot.id)) + xlab('Simulation period') +
        geom_line() + ylab( "removed trees per ha") +ylim(0,NA)+ 
        guides(color = guide_legend(title = "Plot ID"))
      
    } else {
      num.removed.trees.ha  <-
        rem.trees[, list(num.removed.trees.ha = weighted.mean(num.removed.trees.ha,
                                                              w = plot.size.m2)),
                            by = period]
      my.plots$plot5 <-
        ggplot(num.removed.trees.ha,
               aes(period, num.removed.trees.ha, group = 1))   + geom_line() +ylim(0,NA)+
        xlab('Simulation period') +
        ylab("Average number of removed trees per ha")
      
    }
    if (with.legend == FALSE){
      my.plots$plot5 <- my.plots$plot5 + theme(legend.position="none")
      
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

        xx$plot.id <- as.factor(xx$id)
        xx[id.plot == 'SBA.m2.ha', id.plot := 'SBA~(m^2/ha)']
        xx[id.plot == 'stems.ha', id.plot := 'Stems/ha']
        xx[id.plot == 'heights.10', id.plot := 'Height~of~the~10~tallest~trees~(dm)']
        xx[id.plot == 'num.dead.trees.ha', id.plot := 'Dead~trees~(stems/ha)']


        xx[, id.plot := factor(id.plot, levels = c('SBA~(m^2/ha)', 'Stems/ha',
                                                      'Height~of~the~10~tallest~trees~(dm)',
                                                      'Dead~trees~(stems/ha)'))]
        my.plots[['all_plots_figure']]  <-
          ggplot(xx, aes(period, t, group = plot.id, col = plot.id))   +
          geom_line() +
          ylim(0,NA) +
          facet_wrap(~ id.plot, scales = "free",
                     labeller = label_parsed) +
          ylab("") + xlab ("Simulation period") + 
          guides(color = guide_legend(title = "Plot ID"))
        
        
      } else {
        if(1 %in% plots) SBA.m2 <- SBA.m2.ha[,c("period", "t")] else {
          SBA.m2.ha <- NA}
        if(2 %in% plots) stems <- stems.ha[,c("period", "t")] else {
          stems.ha <- NA}
        if(3 %in% plots) heights.10 <- heights.10[,c("period", "t")] else {
          heights.10 <- NA}
        if(4 %in% plots) {
          num.dead.trees.ha <- num.dead.trees.ha[,c("period", "num.dead.trees.ha")]
          } else num.dead.trees.ha <- NA
        if(5 %in% plots) {
          num.removed.trees.ha <- num.removed.trees.ha[,c("period", "num.removed.trees.ha")]
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
        
        xx$plot.id  <-  as.factor(xx$id)
        xx[id.plot == 'SBA.m2.ha', id.plot := 'Average~SBA~(m^2/ha)']
        xx[id.plot == 'stems.ha', id.plot := 'Average~stems/ha']
        xx[id.plot == 'heights.10', id.plot := 'Average~height~of~the~10~tallest~trees~(dm)']
        xx[id.plot == 'num.dead.trees.ha', id.plot := 'Average~dead~trees~(stems/ha)']
        xx[, id.plot := factor(id.plot, levels = c('Average~SBA~(m^2/ha)',
                                                   'Average~stems/ha',
                                                   'Average~height~of~the~10~tallest~trees~(dm)',
                                                   'Average~dead~trees~(stems/ha)'))]

        my.plots[['all_plots_figure']]  <-
          
          ggplot(xx, aes(period, t, group = plot.id, col = plot.id))   +
          geom_line()+ ylim (0,NA) +
          facet_wrap(~id.plot, scales = "free",
                     labeller = label_parsed) +
          ylab("") + xlab ("Simulation period") + 
          theme(legend.position="none")
       print(my.plots[['all_plots_figure']])     
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
