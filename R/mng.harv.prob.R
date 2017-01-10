
harv.prob <- function(  region
                      , skidding.distance.100m
                      , AgeTo5
                      , vuprha.m3.ha
                      , slope.per
                      , SI.m
                      , SI.spp){

  ## Intercept, V, ....
    slope.per[slope.per < 30] <- 30
    AgeTo5[AgeTo5 < -50] <- -50
pars <-c( -3.6904, 0.0016, -0.0345, -0.0404,-0.5981, 0.7194, -9.4143, 0.2407, -7.9924, 0.1946)
  return(1 / (1 + exp(- (
      pars[1]  + 
      pars[2]  * vuprha.m3.ha +
      pars[3]  * slope.per +
      pars[4]  * skidding.distance.100m +
      pars[5]  * (SI.spp == 2) +
      pars[6]  * (region != 'Vest') +
      pars[7]  * 1/SI.m +
      pars[8]  * (AgeTo5 + 51)^0.5 +
      pars[9]  * 1/SI.m * (SI.spp != 3) +
      pars[10] * (AgeTo5 + 51)^0.5 * (SI.spp != 3)
      )))
         )
}
