
thin.prob <- function( region
                      , skidding.distance.100m
                      , AgeTo5
                      , vuprha.m3.ha
                      , slope.per
                      , SI.m
                      , SI.spp
                      
                       ){

  ## Intercept, V, ....
  AgeTo5[AgeTo5 > -1] <- -1
  pars <- c(-6.3897, 0.5699, -0.038, -0.0693, 0.8227, 1.1354, -21.71,
            -0.0955, -0.0012)
  return(1 / (1 + exp(- (
      pars[1]  + 
      pars[2]  * log(vuprha.m3.ha) +
      pars[3]  * slope.per +
      pars[4]  * skidding.distance.100m +
      pars[5]  * (SI.spp == 2) +
      pars[6]  * (region != "Vest") +
      pars[7]  * 1/SI.m +
      pars[8]  * AgeTo5 +
      pars[9]  * AgeTo5^2
    )))

    )
}
