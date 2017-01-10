## recruitment
BBG2008 <-
function (SBA.m2.ha, SI.m, pr.spru.ba, pr.pine.ba, pr.birch.ba, pr.other.ba) 
{
  p.recr <- e.recr <- rep(0, length(SBA.m2.ha))
  pars <- list(
    alpha.i0 =  c(-2.291, -3.552, -0.904, -3.438),
    alpha.i1 = c(-0.018, -0.062, -0.037, -0.029),
    alpha.i2 = c(0.0660, 0.0000, 0.0000, 0.0123),
    alpha.i3 = c(0.019, 0.031, 0.016, 0.048),
    beta.i0 = c(43.142, 67.152, 64.943, 31.438),
    beta.i1 = c(-0.1570, -0.0760, -0.1610, -0.1695),
    beta.i2 = c(0.368, 0.000, 0.143, 0.442),
    beta.i3 = c(0.051, 0.000, 0.104, 0.193),
    row.names = c("spruce", "pine",   "birch",  "other") 
    
  )
  p.recr.function <- function(i) {
    1/(1 + exp(-(pars[["alpha.i0"]][i] + 
                 pars[["alpha.i1"]][i] * SBA.m2.ha + 
                 pars[["alpha.i2"]][i] * SI.m + 
                 pars[["alpha.i3"]][i] * switch(i, 
                                                pr.spru.ba,
                                                pr.pine.ba,
                                                pr.birch.ba,
                                                pr.other.ba))))
  }
  e.recr.function <- function(i) {
    (pars[["beta.i0"]][i] * SBA.m2.ha^pars[["beta.i1"]][i] * 
                                      SI.m^pars[["beta.i2"]][i] * 
                                           switch(i, pr.spru.ba, pr.pine.ba, pr.birch.ba, pr.other.ba)^pars[["beta.i3"]][i])
  }
  data.frame(spruce.p = p.recr.function(1), spruce.e = e.recr.function(1), 
             pine.p = p.recr.function(2), pine.e = e.recr.function(2), 
             birch.p = p.recr.function(3), birch.e = e.recr.function(3), 
             other.p = p.recr.function(4), other.e = e.recr.function(4))
}
