
### Management definition file
## In this management option "ManagementProb" the external modifiers applied are
## (1) Harvest Final felling & Thinning
## (3) Natural regeneration

management.prob <- function(
                            tr = tr,
                            fl = fl,
                            fun.final.felling = "harv.prob",
                            fun.thinning      = "thin.prob",
                            common.vars       ,
                            this.period       ,
                            next.period       ,
                            ...){

  others <- list(...)
  devel.class <-  data.frame(
    SI.m = rep(c(26,23,20,17,14,11,8,6),2),
    spp     = rep(c("conif","birch"),each=8),
    class3 = c(
      20, 20, 20, 25, 30, 35, 45, 55,
      15,15,15,20,25,25,25,30),
    class5 = c(
      60, 60, 70, 80, 90,100,110,120,
      40,40,50,60,70,70,70,80)
  )
  devel.class$ID <- with(devel.class, paste0(spp, "-", SI.m))
  my.spp <- as.character(fl$SI.spp)
  my.spp[my.spp %in% c(1, 2)] <- "conif"
  my.spp[my.spp %in% c(3)] <- "birch"
  ID <- paste0(my.spp, "-", fl$SI.m)
  sa <- devel.class[match(ID, devel.class$ID), c("class3", "class5")]
  AgeTo5 <- unlist(fl$stand.age.years[,this.period] - sa$class5)
  AgeTo3 <- unlist(fl$stand.age.years[,this.period] - sa$class3)
  
  ## Probability harvest and thinning models

  ## We harvest the plot when the random number from a uniform distrib.
  ## is less or equals the probability of harvest
  
  i.harvestable <- rep(FALSE, length(fl$ustandID))
  i.harvestable[fl$land.use %in% 1 & fl$land.type %in% 1]      <- TRUE
  i.harvestable[is.finite(common.vars$dev.class) &
                common.vars$dev.class %in% c(1,2) ] <- FALSE
  
  ## DEFINE POTENTIAL PLOTS TO BE THINNED .
  ## Same as tbiH but no classes 21 and 22, because by definition only classes III-V
  ## can be "thinned"
  ##TO BE INCLUDED FOR THINNING (we will exclude later harv. plots)
  i.thinnable <- rep(FALSE, length(fl$ustandID))
  i.thinnable[fl$land.use %in% 1 & fl$land.type %in% 1] <- TRUE
  i.thinnable[is.finite(AgeTo3) & AgeTo3 < -6]          <- FALSE

  harv <- thin <- rep(0, length(fl$ustandID))
  harv[i.harvestable] <- do.call (
    fun.final.felling,
    args = list(
      region                = fl$region[i.harvestable]
    , skidding.distance.100m = fl$skidding.distance.100m[i.harvestable]
    , AgeTo5              = AgeTo5[i.harvestable]
    , vuprha.m3.ha        = common.vars$vuprha.m3.ha[i.harvestable]
    , slope.per           = fl$slope.per[i.harvestable]
    , SI.m                = fl$SI.m     [i.harvestable]
    , SI.spp              = fl$SI.spp   [i.harvestable]
      
    )
  )
  ## exclude the ones that have been harvested or thinned
  thin[i.thinnable] <- do.call (
    fun.thinning,
    args = list(
      region                = fl$region[i.thinnable]
    , skidding.distance.100m = fl$skidding.distance.100m[i.thinnable]
    , AgeTo5              = AgeTo5[i.thinnable]
    , vuprha.m3.ha        = common.vars$vuprha.m3.ha[i.thinnable]
    , slope.per           = fl$slope.per[i.thinnable]
    , SI.m                = fl$SI.m     [i.thinnable]
    , SI.spp              = fl$SI.spp   [i.thinnable]
      
    )
  )
  ## Since we are not implementing uncertainty for now,
  ## keep the total harvested/thinned close to mean
  harv.total <- sum(harv * fl$ha2total * common.vars$vuprha.m3.ha)
  thin.total <- sum(thin * fl$ha2total * common.vars$vuprha.m3.ha)

  vol <- 0
  while (abs(vol - harv.total)/harv.total * 100 > 30) {
    final.felling <- ifelse( runif(length(harv),0,1) <= harv,  TRUE, FALSE)
    vol <- sum(final.felling * fl$ha2total * common.vars$vuprha.m3.ha)
  }

  vol <- 0
  while (abs(vol - thin.total)/thin.total * 100 > 30) {
    thinning <- ifelse( runif(length(thin),0,1) <= thin,  TRUE, FALSE)
    vol <- sum(thinning * fl$ha2total * common.vars$vuprha.m3.ha)
  }
  
  mng <- fl$management[, next.period]
  substring(mng[final.felling], 1) <- '1'
  substring(mng[thinning]     , 2) <- '1'


  management <- list()
  management$management <- mng
  return(management)
} 

## reassignInPackage("management.prob", "sitree", management.prob)
