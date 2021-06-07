mort.B2007 <-
    function (tr, fl, 
              common.vars ,
              this.period ,
              ...) 
{

    if (!all(unique(common.vars$spp) %in% c("spruce", "pine", "birch", "other"))) {
        message("spp should only contain values spruce, pine, birch, other")
    }

    dbh.mm <- tr$data[["dbh.mm"]][, this.period]

  p.functions <- data.frame(a0 = c(-2.492, -1.808, -2.188, -1.551),
                               a1 = c(-0.020, -0.027, -0.016, -0.011),
                               a2 = c(3.2, 3.3, 2.7, 1.4),
                               a3 = c(0.031, 0.055, 0.030, 0.016)
                               )
  rownames(p.functions) <- c("spruce", "pine", "birch", "other")
 
  
    logit <- 
                  p.functions[common.vars$spp, "a0"] +
                  p.functions[common.vars$spp, "a1"] * dbh.mm +
                  p.functions[common.vars$spp, "a2"] * 
                  1e-05 * (dbh.mm)^2 + p.functions[common.vars$spp, "a3"] * 
                  common.vars$SBA.m2.ha
             
    mort.B <- (1/(1 + exp(-logit)))

  mort <- ifelse(mort.B  >= runif(nrow(tr$data$dbh.mm), 0,1), TRUE, FALSE )
 
  sum(mort)
  
    return(mort)
}
    
