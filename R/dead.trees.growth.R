dead.trees.growth <-
function (dt, growth, mort, this.period = this.period) 
{
    data.frame(dbh.mm = dt$dbh.mm[, this.period] +
                 growth[mort, "dbh.inc.mm"]/2,
               height.dm = dt$height.dm[, this.period] + 
                 growth[mort, "hgt.inc.dm"]/2)
}
## reassignInPackage("dead.trees.growth", "sitree", dead.trees.growth)
