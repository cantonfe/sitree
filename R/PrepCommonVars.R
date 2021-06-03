## SUBPLOT LEVEL
## QMD and DQ and SDI
## PLOT LEVEL
## PBAL, pr.spp.ba

################################
## START CALCULATING VARIABLES
################################
prep.common.vars.fun <- function(tr,
                                 fl, 
                                 i.period,
                                 this.period,
                                 common.vars, 
                                 vars.required,
                                 period.length, n.periods,...){


  if (length(common.vars) > 1) res <- common.vars else res <- list()
  others <- list(...)
 all.plot.vars <- data.table(plot.id = fl$plot.id,
                              SI.m = fl$SI.m,
                              SI.spp = fl$SI.spp,
                              tree2ha = fl$tree2ha,
                              ha2total = fl$ha2total,
                              kom = fl$kom)
  ########################
  ## BASIC STUFF
  ########################
  res$i.stand <- match(tr$data[["plot.id"]], fl[[ "plot.id"]])
  ## an object where we store plot (not subplot) ID
  
  ## NA mean no trees in that plot
  res$i.tree <- match(fl$plot.id, tr$data$plot.id)
  res$tree.BA.m2 <- pi * (tr$data[["dbh.mm"]][, this.period]/1000/2)^2
  
  ## SBA.m2.ha

  res$SBA.m2.ha <- tapply(res$tree.BA.m2 * fl[["tree2ha"]][res$i.stand],
                          list(plot.id =  tr$data[["plot.id"]]), FUN = sum)
  i.match.tapply <- match(tr$data[["plot.id"]], names(res$SBA.m2.ha) )
  res$SBA.m2.ha <- as.vector(res$SBA.m2.ha[i.match.tapply])
  
  ## SPP
  res$spp    <- sp.classification(tree.sp = tr$data[["tree.sp"]],
                                  species.spruce = c(1, 2, 3),
                                  species.pine = c(10, 11, 20, 21, 29),
                                  species.harw =  c(30, 31))
  

  all.tree.vars <- data.table(treeid = tr$data$treeid,
                              plot.id = tr$data$plot.id,
                              dbh.mm = tr$data$dbh.mm[, this.period],
                              height.dm = tr$data$height.dm[, this.period],
                              tree.sp = tr$data$tree.sp)
  all.tree.vars[all.plot.vars, tree2ha := i.tree2ha, on = list(plot.id)]
  
  all.tree.vars[, BA.m2 := pi * (dbh.mm /1000 /2)^2]

  
  ## add 
  ## QMD and DQ and SDI - REINEKE'S STAND DENSITY INDEX
  ## We need to calculate these variables based on subplot, not plot 
  ## since they mean to say something about the state of development of the stand

  ## QMD is the same as Dg
  QMD.cm <- tapply(tr$data[["dbh.mm"]][, this.period]
                 , list(tr$data[["plot.id"]]), function(x.mm)  {
                   x.mm <- x.mm[is.finite(x.mm)]
                   (sqrt(sum((x.mm/10)^2)/length(x.mm)))
                 }
                 )
  res$QMD.cm <- as.vector(QMD.cm[i.match.tapply])
  tph    <- tapply(fl[["tree2ha"]][res$i.stand],  tr$data[["plot.id"]],  sum)
  res$tph    <- as.vector(tph[i.match.tapply])
  res$SDI    <- res$tph * (res$QMD.cm / (10*2.54)) ^ 1.605
  
  ## SPP COMPOSITION
  ## With spp composition we are trying to describe the type of competition that the
  ## tree is enduring we calculate this at plot level

  ## by ba 
  pr.spp.ba <- data.frame(spru = rep(0, length(res$i.stand)),
                          pine = 0,
                          harw= 0,
                          birch = 0,
                          other = 0)
  pr.spp.ba$spru[res$spp == "spruce"]              <- 1
  pr.spp.ba$pine[res$spp == "pine"]                <- 1
  pr.spp.ba$birch[res$spp %in% c("birch")] <- 1
  pr.spp.ba$other[res$spp %in% c("other")] <- 1
  pr.spp.ba$harw[res$spp %in% c("birch", "other")] <- 1
  pr.spp.ba <- pr.spp.ba * res$tree.BA.m2
  
  dum.s <- tapply(pr.spp.ba$spru, tr$data$plot.id, sum)
  dum.p <- tapply(pr.spp.ba$pine, tr$data$plot.id, sum)
  dum.h <- tapply(pr.spp.ba$harw, tr$data$plot.id, sum)
  dum.b <- tapply(pr.spp.ba$birch, tr$data$plot.id, sum)
  dum.o <- tapply(pr.spp.ba$other, tr$data$plot.id, sum)
  pr.spp.ba <- data.frame(spru = as.vector(dum.s),
                          pine = as.vector(dum.p),
                          harw = as.vector(dum.h),
                          birch = as.vector(dum.b),
                          other = as.vector(dum.o)
                          )
  pr.spp.ba <- pr.spp.ba / with(pr.spp.ba, spru + pine + harw)
  pr.spp.ba <- pr.spp.ba[ match(tr$data$plot.id, names(dum.s)),]*100 ## in %
  res$pr.spp.ba <- pr.spp.ba; rm(pr.spp.ba)
  
  
  ## PBAL
  ## PBAL is calculated at plot level, not at subplot level, so
  ## to convert it from "per plot" to "per ha" we should use always 40, not tree2ha
  ## or we would be overestimating competition.
  
  
  
  ## 10000 / fl[["plot.size.m2"]][res$i.stand] to convert per tree to ha
  res$PBAL.m2.ha <- ave(res$tree.BA.m2 * 10000 / fl[["plot.size.m2"]][res$i.stand], ##fl[["tree2ha"]][res$i.stand],
                        tr$data$plot.id,
                        FUN = function(X){
                          ord.x <- order(X)
                          X <- sum(X[ord.x]) - cumsum(X[ord.x])
                          X <- X[match(1:length(X), ord.x)]
                          return(X)
                        }
                        )

  

  
  ## ## STAND.AGE.YEARS
   previous.period <- paste0("t", i.period -1)
  
  if ('stand.age.years' %in% names(fl)){
    if (i.period == 0 &!is.data.frame(fl$stand.age.years)){
      my.age <- fl$stand.age.years
      fl$stand.age.years <- data.frame(matrix(NA, ncol = n.periods,  nrow = length(fl$plot.id)))
      names(fl$management) <- paste0("t", 1:n.periods)
      fl$stand.age.years[, this.period] <- my.age 
    }
    
    ## by default stand age in years is old stand +5  
    if (i.period > 0 & 'stand.age.years' %in% names(fl)){
      fl$stand.age.years[,this.period] <- fl$stand.age.years[, previous.period ] + 5
      
      stand.age.dt <- data.table(
        plot.id         = fl$plot.id,
        SI.spp          = fl$SI.spp,
        SI.m            = fl$SI.m,
        stand.age.years = fl$stand.age.years[,this.period],
        waiting.time = 15
        ##old.dev.class = common.vars$dev.class
      )
      
      
      if (any(!is.na(fl$management[, this.period]))) {
        ## if there is no management we don't need this
        stand.age.dt[, stands.ff := !substr(fl$management[, this.period], 1, 1) %in% c('0','3') ]
        
        ## new age for final felled stands
        stand.age.dt[stands.ff == TRUE,  stand.age.years := - waiting.time +  period.length/2]
        stand.age.dt[, dev.class := calculate.development.class(
          SI.spp          = SI.spp,
          SI.m            = SI.m,
          stand.age.years = stand.age.years)]
      }
      
      fl$stand.age.years[, this.period] <- stand.age.dt[, stand.age.years]
      
      if (any(is.na(stand.age.dt$stand.age.years))) browser()
    }
    
    #  dev.class
    res$dev.class <- calculate.development.class(
      SI.spp          = fl$SI.spp,
      SI.m            = fl$SI.m,
      stand.age.years = fl$stand.age.years[, this.period])
    
    res$tree.age <-
      data.table(age.years = fl$stand.age.years[, this.period][match(tr$data$plot.id, fl$plot.id) ],
                 treeid = tr$data$treeid)
    
    table(fl$stand.age.years[, this.period], useNA = 'always' )    
  }
    
   ################################
  ## VOLUME PER HA
  ################################

  
  ## Calculate individual tree volume without bark
  vuprha.m3.ha <- NULL
  ## Volume without bark per ha
  ## Vest fylke
  

  ## calculate volume
  all.tree.vars[all.plot.vars, kom := kom, on = 'plot.id']
  ## new species treslag 12
  all.tree.vars[tree.sp == 12, tree.sp := 10]## Jeg behandler det som furu. Rune teams 13 mai 2020

  all.tree.vars[dbh.mm > 0, c('vol.w.tr.m3', 'vol.wo.tr.m3') :=
                              volume.norway(dbh.mm, height.dm,
                                            as.numeric(levels(tree.sp))[tree.sp], kom)]

  ################################
  ## VOLUME PER HA
  ################################

  
  ## Calculate individual tree volume without bark
  vuprha.m3.ha <- NULL
  ## Volume without bark per ha
  ## Vest fylke
  

  ## calculate volume
  all.tree.vars[all.plot.vars, kom := i.kom, on = 'plot.id']
  ## new species treslag 12
  all.tree.vars[tree.sp == 12, tree.sp := 10]## Jeg behandler det som furu. Rune teams 13 mai 2020
  
  all.tree.vars[dbh.mm > 0, c('vol.w.tr.m3', 'vol.wo.tr.m3') :=
                              volume.norway(dbh.mm, height.dm,
                                            as.numeric(levels(tree.sp))[tree.sp], kom)]
  all.tree.vars[, vuprha.m3.ha := vol.wo.tr.m3 * tree2ha ]

  vuprha.m3.ha <- all.tree.vars[, sum(vuprha.m3.ha, na.rm = TRUE), by = plot.id]
  all.plot.vars[vuprha.m3.ha, vuprha.m3.ha := V1, on = 'plot.id']
  ## if there is NA, then volume is actually 0, no trees
  all.plot.vars[is.na(vuprha.m3.ha), vuprha.m3.ha := 0]
  res$vuprha.m3.ha  <- all.plot.vars$vuprha.m3.ha
  res$vol.wo.tr.m3.ha <- all.tree.vars$vuprha.m3.ha

  
  if(i.period==0){
    time.intern <- rep(NA,length.out=length(fl$plot.id))
  }else{
    
    time.intern <- fl$time.since.final.felling
    ## harvested in the last period
    harv.last <- substr(fl$management[,this.period],1,1) %in% c("1","2") 
    time.intern[harv.last] <- period.length/2
    ## harvested in previous periods
    time.intern[!harv.last & !is.na(time.intern)] <- time.intern[!harv.last & !is.na(time.intern)] + period.length
    
  }
  
  fl$time.since.final.felling <- time.intern
  


  invisible(list(res = res,
                 fl = fl))
}
## reassignInPackage("prep.common.vars.fun", "sitree", prep.common.vars.fun)
