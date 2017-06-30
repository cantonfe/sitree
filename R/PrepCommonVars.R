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
                                 period.length, ...){


  if (length(common.vars) > 1) res <- common.vars else res <- list()
  others <- list(...)

########################
  ## BASIC STUFF
########################
  res$i.stand <- match(tr$data[["plot.id"]], fl[[ "plot.id"]])
  ## an object where we store plot (not subplot) ID
 
  ## NA mean no trees in that plot
  res$i.tree <- match(fl$plot.id, tr$data$plot.id)
  res$tree.BA.m2 <- pi * (tr$data[["dbh.mm"]][, this.period]/1000/2)^2
  
  ## SBA.m2.ha
  ## if (any(vars.required == "SBA.m2.ha")){ ## we will calculate SBA.m2.ha always
  ## since it is widely used. We will use this to calculate i.match.tapply which
  ## will be used further tdown
  ## SBA.m2.ha is calculated per subplot right tnow
  res$SBA.m2.ha <- tapply(res$tree.BA.m2 * fl[["tree2ha"]][res$i.stand],
                          list(plot.id =  tr$data[["plot.id"]]), FUN = sum)
  i.match.tapply <- match(tr$data[["plot.id"]], names(res$SBA.m2.ha) )
  res$SBA.m2.ha <- as.vector(res$SBA.m2.ha[i.match.tapply])
  
  
  ## SPP
  if (any(vars.required %in% c("spp", "pr.pine.ba", "pr.spru.ba", "pr.harw.ha",
                               "biomass.tr.components.kg")))  {
    res$spp    <- sp.classification(tree.sp = tr$data[["tree.sp"]],
                                    species.spruce = others$species.spruce,
                                    species.pine = others$species.pine,
                                    species.harw = others$species.harw)
  }

  ## QMD and DQ and SDI - REINEKE'S STAND DENSITY INDEX
  ## We need to calculate these variables based on subplot, not plot 
  ## since they mean to say something about the state of development of the stand
  if (any(vars.required %in% c("DQ", "QMD.cm", "SDI", "tphd"))){
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
  }
  ## SPP COMPOSITION
  ## With spp composition we are trying to describe the type of competition that the
  ## tree is enduring we calculate this at plot level
  if (any(vars.required %in% c("pr.pine.ba", "pr.spru.ba", "pr.harw.ha"))){
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
  }
  
  ## PBAL
  ## PBAL is calculated at plot level, not at subplot level, so
  ## to convert it from "per plot" to "per ha" we should use always 40, not tree2ha
  ## or we would be overestimating competition.
  
  if (any(vars.required %in% c("PBAL.m2.ha"))){
    
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
  }
  

  
  ## ## STAND.AGE.YEARS
  
  if (any(vars.required %in% c("stand.age.years", "AgeTo5"))){
    ventetid <- data.frame(
      bonitet   = c(26, 23, 20, 17, 14, 11,  8,  6),
      granfuru  = c( 0,  0,  0,  0,  5,  5, 15, 15),
      lauv      = c( 0,  0,  0,  0,  0,  5,  5,  5))
    
    ## stand age is always calculated as the tree average trees.
    ## but tree age for class dev I is the same regardless of size. tree ages are recalculated
    ## when class development 5 is reached.
    ## stand age for dev.class 1 is calculated +5 and starting with waiting time
    ## (can be negative)
    ## after harvesting. Otherwise it is calculated as average of tree ages.    
    if (i.period == 0){
      res$dev.class <- calculate.development.class(SI.spp          = fl$SI.spp,
                                                   SI.m            = fl$SI.m,
                                                   stand.age.years =
                                                     fl$stand.age.years[,this.period])
      ## ASSIGN TREE AGE TO ALL TREES
      ## This function estimates tree age based on stand age
      tree.age.years <- tree.age (stand.age.years  =
                                    fl$stand.age.years[,this.period][res$i.stand] ,
                                  plot.id         = tr$data$plot.id,
                                  tree.BA.m2       = res$tree.BA.m2       ,
                                  dbh.mm           = tr$data[["dbh.mm"]][, this.period],
                                  SI.spp     = fl$SI.m[res$i.stand] ,
                                  SI.m       = fl$SI.spp[res$i.stand] ,
                                  spp        = res$spp,
                                  dev.class  = res$dev.class[res$i.stand]
                                  )
      ## recalcualte stand.age.years based on tree data
      tree.BA.m2 <- plot.id <-
                NULL ## this avoid note no visible binding for global variable
      DT <- data.table(tree.BA.m2 = res$tree.BA.m2,
                       tree.age.years = tree.age.years,
                       plot.id = tr$data$plot.id
                       ) ## +- 1 year
      setkeyv(DT, 'plot.id')
      
      
      s.age <- DT[ ,
                  list(wret = mean(tree.age.years),
		  w = (tree.BA.m2 /sum(tree.BA.m2))
                      )
                , by = plot.id]
      
      
      ## in dev.class 1 there might not be trees so
      
      ## There exists several stands with dev.class > 1 without trees. What to do with

      ## them?
      ## 25 stands -ages- 90  95  95  85  85  82  75 130  90  30 120 105  65
      ## 60  88 130 135 150  40 92  80 100  80  30  25
      fl$stand.age.years[,this.period][res$dev.class > 1] <-
        round(s.age$wret)[match(fl$plot.id[res$dev.class > 1], s.age$plot.id)]
      ## if we have dev.class > 1 but no trees, we will assume class dev 3 -1 year
      devel.class <- data.frame(
        SI.m = rep(c(26, 23, 20, 17, 14, 11, 8, 6), 2),
        spp = rep(c("conif", "birch"), each = 8), 
        class3 = c(20, 20, 20, 25, 30, 35, 45, 55, 15, 15, 15, 
                   20, 25, 25, 25, 30)
      )
      devel.class$ID <- with(devel.class, paste0(spp, "-", SI.m))
      spp.c <- fl$SI.spp
      spp.c[spp.c %in% 3] <- "birch"
      spp.c[spp.c %in% c(1:2)] <- "conif"
      spp.c <- data.frame(ID = paste0(spp.c, "-", fl$SI.m))
      spp.c[, c("III")] <- devel.class[match(spp.c$ID, devel.class$ID), c("class3")]

      fl$stand.age.years[,this.period][is.na(fl$stand.age.years[,this.period])] <-
        spp.c$III[is.na(fl$stand.age.years[,this.period])] - 1
      
      res$tree.age.years <- data.frame(age.years = tree.age.years,
                                       treeid = tr$data$treeid)
      
      
    } else { ## from the if (i.period == 0)
      
      res$tree.age.years <- common.vars$tree.age.years
      previous.period <- paste0("t", i.period -1)
      
      fl$stand.age.years[,this.period] <- fl$stand.age.years[, previous.period ]
      res$dev.class <- calculate.development.class(SI.spp          = fl$SI.spp,
                                                   SI.m            = fl$SI.m,
                                                   stand.age.years =
                                                     fl$stand.age.years[,this.period])
      if (all(is.na(fl$management[, this.period]))) {
        stands.final.felling <- rep(FALSE, nrow(fl$management))
      } else if (any(is.na(fl$management[, this.period]))){
        stop ("Some of the management is NA and it should not. Check your code!")
      } else {
        stands.final.felling <- substr(fl$management[, this.period], 1, 1) != '0'
      }  
      res$dev.class[stands.final.felling] <- 1


      i.conif <- stands.final.felling & fl$SI.spp %in% c(1,2)
      
      if (sum(i.conif) > 0){
        fl$stand.age.years[i.conif, this.period] <-
          -ventetid[match(fl$SI.m[i.conif], ventetid$bonitet), "granfuru"] +
          period.length/2
      }
      
      i.harw <- stands.final.felling & fl$SI.spp %in% c(3)
      if (sum(i.harw) > 0){
        
        fl$stand.age.years[i.harw, this.period] <-
          -ventetid[match(fl$SI.m[i.harw], ventetid$bonitet), "lauv"] +
          period.length/2
      }
      
      ## TREE
      ## removed trees
      res$tree.age.years <- res$tree.age.years[res$tree.age.years$treeid %in%
                                               tr$data$treeid,]
      ## existing trees
      res$tree.age.years$age.years <- res$tree.age.years$age.years + period.length
      ## new trees
      if (!all (tr$data$treeid %in% res$tree.age.years$treeid)){
        
        i.new.trees <- tr$data$treeid[! tr$data$treeid %in%
                                      res$tree.age.years$treeid]
        rows.new.trees <- (
          (nrow(res$tree.age.years)+1) : (
                                         nrow(res$tree.age.years) +
                                         length(i.new.trees)
                                       )
          )
        res$tree.age.years[rows.new.trees,] <- cbind(NA, i.new.trees)
        i.trees  <- match(res$tree.age.years$treeid[rows.new.trees], tr$data$treeid)
        res$i.stands <- match(tr$data$plot.id[i.trees], fl$plot.id)
        res$tree.age.years$age.years[rows.new.trees] <-
          tree.age (stand.age.years  = fl$stand.age.years[,this.period][res$i.stands],
                    plot.id         = tr$data$plot.id[i.trees],
                    tree.BA.m2       = res$tree.BA.m2[i.trees],
                    dbh.mm           = tr$data[["dbh.mm"]][, this.period] [i.trees],
                    SI.spp           = fl$SI.spp[res$i.stands],
                    SI.m             = fl$SI.m[res$i.stands],
                    spp              = res$spp[i.trees],
                    dev.class        = res$dev.class[res$i.stands],
                    apply.correction = FALSE
                    )
      } else { print("No new trees!!!!")}

      ## STAND
      ## recalculate stand.age.years based on tree data
      DT <- data.table(data.frame(tree.BA.m2 = res$tree.BA.m2,
                                  tree.age.years = res$tree.age.years$age.years,
                                  plot.id = tr$data$plot.id)
                       ) ## +- 1 year
      
      s.age <- DT[,list(
        wret = weighted.mean(
          tree.age.years,
          w  = tree.BA.m2/sum(tree.BA.m2))
      ) , by = plot.id]

      ## in dev.class 1 there might not be trees so
      fl$stand.age.years[,this.period][res$dev.class == 1 & !stands.final.felling] <-
        fl$stand.age.years[,this.period][res$dev.class == 1 &
                                         !stands.final.felling] +
        period.length
      res$dev.class <- calculate.development.class(SI.spp          = fl$SI.spp,
                                                   SI.m            = fl$SI.m,
                                                   stand.age.years =
                                                     fl$stand.age.years[,this.period])
      fl$stand.age.years[,this.period][res$dev.class > 1] <-
        round(s.age$wret)[match(fl$plot.id[res$dev.class > 1], s.age$plot.id)]
      ## if we have dev.class > 1 but no trees, we will assume class dev 3 -1 year
      devel.class <- data.frame(
        SI.m = rep(c(26, 23, 20, 17, 14, 11, 8, 6), 2),
        spp = rep(c("conif", "birch"), each = 8), 
        class3 = c(20, 20, 20, 25, 30, 35, 45, 55, 15, 15, 15, 
                   20, 25, 25, 25, 30)
      )
      devel.class$ID <- with(devel.class, paste0(spp, "-", SI.m))
      spp.c <- fl$SI.spp
      spp.c[spp.c %in% 3] <- "birch"
      spp.c[spp.c %in% c(1:2)] <- "conif"
      spp.c <- data.frame(ID = paste0(spp.c, "-", fl$SI.m))
      spp.c[, c("III")] <-
        devel.class[match(spp.c$ID, devel.class$ID), c("class3")]
      
      fl$stand.age.years[,this.period][is.na(fl$stand.age.years[,this.period])] <-
        spp.c$III[is.na(fl$stand.age.years[,this.period])] - 1
      
      ## recalculate dev.class with new tree ages
      res$dev.class <- calculate.development.class(SI.spp          = fl$SI.spp,
                                                   SI.m            = fl$SI.m,
                                                   stand.age.years =
                                                     fl$stand.age.years[,this.period])
    } ## ending the else from the if (i.period == 0)
  }



  
################################
  ## VOLUME PER HA
################################
  if (any(vars.required %in% c("vuprha.m3.ha", "vol.wo.tr.m3.ha"))){
    
    ## Calculate individual tree volume without bark
    vuprha.m3.ha <- NULL
    ## Volume without bark per ha
    ## Vest fylke
    kom <- fl[["kom"]][res$i.stand]
    kom <- as.character(kom, lenght = 4)
    komnr.n <- nchar(kom) == 3
    kom[komnr.n] <- paste("0", kom[komnr.n], sep = "")
    fylkene <- substr(kom, start = 1, stop = 2)
    
    i.fylnr.1   <- fylkene %in% c('11', '12', '14', '15')
    i.tree.sp.1 <- tr$data[["tree.sp"]] %in% c(  1,  3, 21, 29) ## Picea abies
    i.tree.sp.2 <- tr$data[["tree.sp"]] %in% c(  2) ## Sitka spruce
    i.tree.sp.3 <- tr$data[["tree.sp"]] %in% c(10, 11, 20) ## pine
    i.tree.sp.4 <- as.numeric(levels(tr$data[["tree.sp"]]))[tr$data[["tree.sp"]]] > 29
    
    vol.bark.l <- vol.wo.bark.l <- rep(NA, nrow(tr$data$dbh.mm))
    vol.reduksjon <- rep(0, nrow(tr$data$dbh.mm))
    
    ## To deal with dbh 0
    i.small.dbh <- tr$data[["dbh.mm"]][,this.period] < 50
    i.tree.sp.1[i.small.dbh] <- i.tree.sp.2[i.small.dbh] <- i.tree.sp.3[i.small.dbh] <-
      i.tree.sp.4[i.small.dbh] <- FALSE
    
    ## GRAN
    i11 <- i.tree.sp.1 & i.fylnr.1
    vol.bark.l[i11] <- GranVolV(dbh   = tr$data[["dbh.mm"  ]][i11, this.period],
                                trh   = tr$data[["height.dm"]][i11, this.period],
                                bark  = 'mb',
                                enhet = 'l'
                                ) *
      (1 - ifelse(!is.finite(vol.reduksjon[i11]), 0, vol.reduksjon[i11])/100)
    
    vol.wo.bark.l[i11] <- GranVolV(dbh   = tr$data[["dbh.mm"  ]][i11, this.period],
                                   trh   = tr$data[["height.dm"]][i11, this.period],
                                   bark  = 'ub',
                                   enhet = 'l'
                                   ) *
      (1 - ifelse(!is.finite(vol.reduksjon[i11]), 0, vol.reduksjon[i11])/100);
    
    i12 <- i.tree.sp.1 & !i.fylnr.1 
    vol.bark.l[i12] <- GranVol(dbh   = tr$data[["dbh.mm"  ]][i12, this.period], 
                               ##f.dbh = tr$data[["dbh.mm"  ]][i12, this.period],
                               trh   = tr$data[["height.dm"]][i12, this.period],
                               bark  = 'mb',
                               enhet = 'l'
                               ) *
      (1 - ifelse(!is.finite(vol.reduksjon[i12]), 0,
                                               vol.reduksjon[i12])/100);
    vol.wo.bark.l[i12] <- GranVol(dbh   = tr$data[["dbh.mm"  ]][i12, this.period],
                                  ##f.dbh = tr$data[["dbh.mm"  ]][i12, this.period],
                                  trh   = tr$data[["height.dm"]][i12, this.period],
                                  bark  = 'ub',
                                  enhet = 'l'
                                  ) *
      (1 - ifelse(!is.finite(vol.reduksjon[i12]), 0,
                                               vol.reduksjon[i12])/100);
    
    ## SITKA
    vol.bark.l[ i.tree.sp.2] <- SitkaVol(dbh   = tr$data[["dbh.mm"  ]][i.tree.sp.2,
                                                                       this.period],
                                         trh   = tr$data[["height.dm"]][i.tree.sp.2,
                                                                        this.period],
                                         bark  = 'mb',
                                         enhet = 'l'
                                         ) * (1 - ifelse(!is.finite(vol.reduksjon[i.tree.sp.2]), 0, vol.reduksjon[i.tree.sp.2])/100);
    vol.wo.bark.l[ i.tree.sp.2] <- SitkaVol(dbh   = tr$data[["dbh.mm"  ]][i.tree.sp.2,
                                                                          this.period],
                                            trh   = tr$data[["height.dm"]][i.tree.sp.2,
                                                                           this.period],
                                            bark  = 'ub',
                                            enhet = 'l'
                                            ) * (1 - ifelse(!is.finite(vol.reduksjon[i.tree.sp.2]), 0, vol.reduksjon[i.tree.sp.2])/100);
    
    ## FURU
    i31 <-  i.tree.sp.3 & i.fylnr.1 
    vol.bark.l[i31] <- FuruVolV(dbh   = tr$data[["dbh.mm"  ]][i31, this.period],
                                trh   = tr$data[["height.dm"]][i31, this.period],
                                bark  = 'mb',
                                enhet = 'l'
                                ) * 
      (1 - ifelse(!is.finite(vol.reduksjon[i31]), 0,
                                               vol.reduksjon[i31])/100);
    vol.wo.bark.l[i31] <- FuruVolV(dbh   = tr$data[["dbh.mm"  ]][i31, this.period],
                                   trh   = tr$data[["height.dm"]][i31, this.period],
                                   bark  = 'ub',
                                   enhet = 'l'
                                   ) *
      (1 - ifelse(!is.finite(vol.reduksjon[i31]), 0,
                                               vol.reduksjon[i31])/100);
    
    i32 <-  i.tree.sp.3 & !i.fylnr.1 
    vol.bark.l[i32]<- FuruVol(dbh   = tr$data[["dbh.mm"  ]][i32, this.period],
                              ##f.dbh = tr$data[["dbh.mm"  ]][i32, this.period],
                              trh   = tr$data[["height.dm"]][i32, this.period],
                              bark  = 'mb',
                              enhet = 'l'
                              ) * (1 - ifelse(!is.finite(vol.reduksjon[i32]), 0,
                                               vol.reduksjon[i32])/100);
    vol.wo.bark.l[i32] <- FuruVol(dbh   = tr$data[["dbh.mm"   ]][i32, this.period],
                                  ##f.dbh = tr$data[["dbh.mm"   ]][i32, this.period],
                                  trh   = tr$data[["height.dm"]][i32, this.period],
                                  bark  = 'ub',
                                  enhet = 'l'
                                  ) *  (1 - ifelse(!is.finite(vol.reduksjon[i32]), 0,
                                               vol.reduksjon[i32])/100);
    
    ## LAUV
    vol.bark.l[i.tree.sp.4] <- LauvVol(tsl   = as.integer(tr$data$tree.sp[i.tree.sp.4]), 
                                       dbh   = tr$data[["dbh.mm"  ]][i.tree.sp.4,
                                                                     this.period],
                                       trh   = tr$data[["height.dm"]][i.tree.sp.4,
                                                                      this.period],
                                       bark  = 'mb',
                                       enhet = 'l'
                                       ) *  (1 - ifelse(!is.finite(vol.reduksjon[i.tree.sp.4]), 0, vol.reduksjon[i.tree.sp.4])/100);
    
    vol.wo.bark.l[i.tree.sp.4] <- LauvVol(tsl    = as.integer(tr$data$tree.sp[i.tree.sp.4]),
                                          dbh    = tr$data[["dbh.mm"  ]][i.tree.sp.4,
                                                                         this.period],
                                          trh    = tr$data[["height.dm"]][i.tree.sp.4,
                                                                          this.period],
                                          bark   = 'ub',
                                          enhet  = 'l'
                                          ) *  (1 - ifelse(!is.finite(vol.reduksjon[i.tree.sp.4]), 0, vol.reduksjon[i.tree.sp.4])/100);
    
    ## individual tree volume to vuprha.m3.ha
    vuprha.m3.ha <- tapply( X    = vol.wo.bark.l/1000*fl$tree2ha[res$i.stand],
                           INDEX = list (plot.id = tr$data$plot.id),
                           FUN   = sum, na.rm = TRUE)
    
    ## making sure it is correctly ordered 
    res$vuprha.m3.ha <- as.vector(vuprha.m3.ha[match(fl$plot.id,
                                                     names(vuprha.m3.ha))])
    res$vuprha.m3.ha[is.na(res$vuprha.m3.ha)] <- 0
    res$vol.wo.tr.m3.ha <- vol.wo.bark.l / 1000 * fl$tree2ha[res$i.stand]
    ## added by Nikolas, 15.9.2014
    res$vol.wo.tr.m3 <- vol.wo.bark.l / 1000
    res$vol.tr.m3 <- vol.bark.l / 1000
    ## added by Nikolas, 25.6.2015
    ## the proportion of volume from single trees
    ## weighted with the probability of being harvested
    res$vol.prob.cc.left <- ave(res$vol.wo.tr.m3.ha,
                                res$i.stand,
                                FUN=function(x){
                                  ## the relative volume per tree
                                  X1 <- x/sum(x)
                                  ## the rel. volume is kept if a random nuber is larger
                                  X2 <- ifelse(runif(n=length(X1),0,1)>=X1,1,NA)*X1
                                  ## cumulative volumes, starting from the smallest
                                  X3 <- cumsum(X2[order(X2)])[match(1:length(X2), order(X2))]*100
                                  ## replace NA with 0
                                  X3[is.na(X3)] <- 0
                                  ## return
                                  return(X3)
                                })
    
  }



  if(any(vars.required %in% c("time.since.final.felling"))){
    
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
  }


  invisible(list(res = res,
                 fl = fl))
}
## reassignInPackage("prep.common.vars.fun", "sitree", prep.common.vars.fun)
