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
    ## til here recr.ha should be size 100
    i.tree <- common.vars$i.tree
    spru <- common.vars$pr.spp.ba$spru[i.tree]
    pine <-  common.vars$pr.spp.ba$pine[i.tree]
    birch <- common.vars$pr.spp.ba$birch[i.tree]
    other <- common.vars$pr.spp.ba$other[i.tree]
    ## we assume all empty plots are hardwood
    spru[is.na(spru)] <- 0
    pine[is.na(pine)] <- 0
    birch[is.na(birch)] <- 0
    other[is.na(other)] <- 100
    SBA <- common.vars$SBA.m2.ha [i.tree]
    SBA[is.na(SBA)] <- 0.5 ## jst because with 0 functions give -inf
    recr.ha <- do.call(BBG2008,
                       list(SBA.m2.ha   = SBA, 
                            SI.m        = fl$SI.m,
                            pr.spru.ba  = spru ,
                            pr.pine.ba  = pine,
                            pr.birch.ba = birch,
                            pr.other.ba = other )
                       )
    recr.ha[, "ustandID"] <- fl$ustandID
    recr.ha[, "tree2ha"]  <- fl[["tree2ha"]]
    recr.ha[, "subplot.size.m2"] <- fl[["subplot.size.m2"]]
    
    recr.ha[, paste(all.spp, ".p", sep = "")] <-
        recr.ha[, paste(all.spp, ".p", sep = "")] *
        recr.ha[, "subplot.size.m2"]/250
    
    recr.ha[, paste(all.spp, ".p", sep = "")] <-
        recr.ha[, paste(all.spp, ".p", sep = "")] >=
        cbind(runif(nrow(recr.ha), 0, 1), 
              runif(nrow(recr.ha), 0, 1),
              runif(nrow(recr.ha), 0, 1), 
              runif(nrow(recr.ha), 0, 1))

   
    recr.ha[, paste(all.spp, ".e", sep = "")] <-
        round(recr.ha[, paste(all.spp, ".e", sep = "")] / recr.ha[, "tree2ha"]) * 
        recr.ha[, paste(all.spp, ".p", sep = "")
                ]
    noingrowth <- all(unlist(recr.ha[, paste(all.spp, ".e", sep = "")]) == 
                      0)
    
    foo2 <- function(mvec, period) {
        mdt <- matrix(0, nrow = length(mvec),
                      ncol = (tr$nperiods + 
                              1))
        colnames(mdt) <- paste("t", 0:tr$nperiods, sep = "")
        mdt[, period] <- mvec
        return(as.data.frame(mdt))
    }
    n.newtrees <- colSums(recr.ha[, paste(all.spp, ".e", sep = "")])

    uid <-  rep.int(recr.ha[, "ustandID"], length(all.spp))
    reps <- recr.ha[, paste(all.spp, ".e", sep = "")]
    reps <- unlist(reps)
    
            
    list(ustandID = if (noingrowth) 0 else rep.int(uid, reps),
         treeid    = max(tr$data[["treeid"]]) + 
             (1:sum(recr.ha[, paste(all.spp, ".e", sep = "")])),
         dbh.mm    = foo2(rep(50, sum(n.newtrees)), next.period),
         height.dm = foo2(rep(120, sum(n.newtrees)), next.period),
         yrs.sim   = rep(2.5, sum(n.newtrees)), 
         tree.sp   = if (noingrowth) 0 else rep.int(c(1,10,30, 40), 
                                                    n.newtrees)
         )
}

