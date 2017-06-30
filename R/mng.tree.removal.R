## This function defines how trees are removed given a certain management option.

mng.tree.removal <- function(tr,
                             fl,
                             common.vars,
                             this.period,
                             next.period,...){
   
    per.vol.harv <- list(...)$per.vol.harv

    ## Which trees should be removed when 'Final felling' is the selected management
    ## About 83% of the volume is harvested on final fellings.
    ## The largest trees will have higher probability of being harvested.
  final.felling <- ifelse (substr(fl$management[, next.period],1,1) == '1', TRUE, FALSE)
  thinning <- ifelse (substr(fl$management[, next.period],2,2) == '1', TRUE, FALSE)
    
    i.harvestable <- tr$data$plot.id %in% fl$plot.id [final.felling]
    vols <- common.vars$vol.wo.tr.m3.ha[i.harvestable]
    uid <- tr$data$plot.id[i.harvestable]
    sum.vols <- aggregate(vols ~ uid, FUN = sum)

    vols <- data.frame(
        treid = tr$data$treeid[i.harvestable],
        vols = vols,
        sum.vols = sum.vols$vols[match(uid, sum.vols$uid)])
    
    vols$prob <-  with(vols, vols/sum(vols))

    vols$prob.vols <- with(vols, prob * vols)
    
    ff <- by (data = vols, INDICES = uid, FUN = function(x) {
        n <- sum(x$vols)/ sum(x$prob * x$vols)
        data.frame(harvested = ifelse( x$prob >= runif(nrow(x), 0, 1/n) , TRUE, FALSE),
                   treeid = x$treid)
    })
    

    tr.removed <- rep(FALSE, length(tr$data$treeid))
    tr.removed[match(ff$treeid, tr$data$treeid )] <- ff$harvested  
        
    ## Which trees should be removed when 'Thinning' is the selected management
    i.harvestable <- tr$data$plot.id %in% fl$plot.id [thinning]
    vols <- common.vars$vol.wo.tr.m3.ha[i.harvestable]
    uid <- tr$data$plot.id[i.harvestable]
    sum.vols <- aggregate(vols ~ uid, FUN = sum)

    vols <- data.frame(
        treid = tr$data$treeid[i.harvestable],
        vols = vols,
        sum.vols = sum.vols$vols[match(uid, sum.vols$uid)])
    
    vols$prob <-  with(vols, vols/sum(vols))

    vols$prob.vols <- with(vols, prob * vols)
   
    ff <- by (data = vols, INDICES = uid, FUN = function(x) {
        n <- sum(x$vols)/ sum(x$prob * x$vols)
        data.frame(harvested = ifelse( x$prob >= runif(nrow(x), 0, 1/n) , TRUE, FALSE),
                   treeid = x$treid)
    }
    )
    ff <- do.call(rbind,ff)
    
    tr.removed[match(ff$treeid, tr$data$treeid )] <- ff$harvested  
    
    return(tr.removed)
}
## reassignInPackage("mng.tree.removal", "sitree", mng.tree.removal)
