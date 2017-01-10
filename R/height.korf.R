height.korf <- function (common.vars = common.vars, this.period = this.period, tr, ...) 
{
  spp <- common.vars$spp
  height <- rep(NA, nrow(tr$data$dbh.mm))
  height[spp == 'spruce'] <- 1017.27 *
    exp(-12.697 * tr$data$dbh.mm[spp == 'spruce',this.period]^ (-0.3562))
  height[spp == 'pine'] <- 299.34 *
    exp(-34.2159 * tr$data$dbh.mm[spp == 'pine',this.period]^ (-0.6932))
  height[spp %in% c('birch', 'other')] <- 714.0037 *
    exp(-11.0299 * tr$data$dbh.mm[spp %in% c('birch', 'other'),this.period]^ (-0.347))
  ## height should never be below 13
  height[height < 13] <- 13
  height.inc.dm <- height - tr$data$height.dm[,this.period]

  return(height.inc.dm)
  
}

