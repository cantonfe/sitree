recr.BBG2008 <-
function (tr,
          fl, 
          common.vars,
          i.period,
          this.period,
          next.period,
           ...) 
{

    all.spp <- c("spruce", "pine", "birch", "other")
    
    recr.ha <- do.call(BBG2008,
                       list(SBA.m2.ha   = common.vars$SBA.m2.ha , 
                            SI.m        = fl$SI.m[common.vars$i.stand],
                            pr.spru.ba  = common.vars$pr.spp.ba$spru ,
                            pr.pine.ba  = common.vars$pr.spp.ba$pine ,
                            pr.birch.ba = common.vars$pr.spp.ba$birch ,
                            pr.other.ba = common.vars$pr.spp.ba$other )
                       )
    recr.ha[, "ustandID"] <- tr$data$ustandID
    recr.ha[, "tree2ha"] <- fl[["tree2ha"]][common.vars$i.stand]
    recr.ha[, "subplot.size.m2"] <- fl[["subplot.size.m2"]][common.vars$i.stand]
    
    recr.ha[, paste(all.spp, ".p", sep = "")] <- recr.ha[, paste(all.spp, 
        ".p", sep = "")] * recr.ha[, "subplot.size.m2"]/250
    recr.ha[, paste(all.spp, ".p", sep = "")] <- recr.ha[, paste(all.spp, 
        ".p", sep = "")] >= cbind(runif(nrow(recr.ha), 0, 1), 
        runif(nrow(recr.ha), 0, 1), runif(nrow(recr.ha), 0, 1), 
        runif(nrow(recr.ha), 0, 1))
    recr.ha[, paste(all.spp, ".e", sep = "")] <- round(recr.ha[, 
        paste(all.spp, ".e", sep = "")]/recr.ha[, "tree2ha"]) * 
        recr.ha[, paste(all.spp, ".p", sep = "")]
    noingrowth <- all(unlist(recr.ha[, paste(all.spp, ".e", sep = "")]) == 
        0)
    foo2 <- function(mvec, period) {
        mdt <- matrix(0, nrow = length(mvec), ncol = (tr$nperiods + 
            1))
        colnames(mdt) <- paste("t", 0:tr$nperiods, sep = "")
        mdt[, period] <- mvec
        return(as.data.frame(mdt))
    }
    n.newtrees <- colSums(recr.ha[, paste(all.spp, ".e", sep = "")])

    list(ustandID = if (noingrowth) 0 else rep.int(rep.int(recr.ha[, "ustandID"],
                                                           length(all.spp)),
                                                   unlist(recr.ha[,
                                                                  paste(all.spp, 
                                                                        ".e",
                                                                        sep = "")])),
         treeid    = max(tr$data[["treeid"]]) + 
             (1:sum(recr.ha[, paste(all.spp, ".e", sep = "")])),
         dbh.mm    = foo2(rep(50, sum(n.newtrees)), next.period),
         height.dm = foo2(rep(120, sum(n.newtrees)), next.period),
         yrs.sim   = rep(2.5, sum(n.newtrees)), 
         tree.sp   = if (noingrowth) 0 else rep.int(c(1,10,30, 40), 
                                                    n.newtrees)
         )
}

