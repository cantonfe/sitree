
dbhi.BN2009 <- function (tr,
                         fl,
                         common.vars,
                         this.period, ...) 
{
   
        others <- list(...)
  spp       <- sp.classification(tr$data[["tree.sp"]],
                                 species.spruce = others$species.spruce,
                                 species.pine = others$species.pine,
                                 species.harw = others$species.harw
                                 )
  
  dbh.mm    <- tr$data[["dbh.mm"]][, this.period]
  DQ        <- tr$data[["dbh.mm"]][, this.period]/10/common.vars$QMD.cm
  SI.m      <- fl[["SI.m"]][common.vars$i.stand]
  SBA.m2.ha <- common.vars$SBA.m2.ha
  lat.deg   <- fl[["lat.deg"]][common.vars$i.stand]
  
if (!all(unique(spp) %in% c("spruce", "pine", "birch", "other"))) {
        stop("spp should only contain values spruce, pine, birch, other")
    }

  pars <- data.frame(b1 = c(1.3615, 1.3548, 1.0085, 1.4168),
                     b2 =  c(503.63, 443.85, 2651.94, 646.02),
                     g0 =  c(1824106, 2586.91, 49080465, 6528),
                     g1 =  c(0.5574, 0.4245, 0.6251, 0.3354),
                     g2 =  c(1.1997, 0.8743, 1.0225, 0.5562),
                     g3 =  c(-0.5254, -0.4219, -0.3011, -0.3362),
                     g4 = c(-1.6726, 0, -2.3007, 0)
                     )
  rownames(pars) <- c("spruce", "pine", "birch", "other")
  
  
  dbhi <- (
    ((pars[spp, "b1"]/
      pars[spp, "b2"]) *
     (dbh.mm / pars[spp, "b2"]) ^
      (pars[spp, "b1"] - 1) *
      exp(-(dbh.mm/pars[spp, "b2"]) ^
          pars[spp, "b1"])) *
    (pars[spp, "g0"] *
     DQ        ^ pars[spp, "g1"] *
                 SI.m      ^ pars[spp, "g2"] *
                             SBA.m2.ha ^ pars[spp, "g3"] * 
                                         lat.deg   ^ pars[spp, "g4"]
    )
  )
    
  return(dbhi)
}
## reassignInPackage("dbhi.BN2009", "sitree", dbhi.BN2009)
