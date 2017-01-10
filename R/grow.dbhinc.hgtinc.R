grow.dbhinc.hgtinc <-
function (tr,
          fl,
          common.vars,
          this.period,
          functions, ...) 
{
    fn.dbh.inc <- functions$fn.dbh.inc
    fn.hgt.inc <- functions$fn.hgt.inc

    
    ## dbh increment
    dbh.inc.mm = do.call(what = fn.dbh.inc,
                             args = list(common.vars = common.vars,
                                         this.period    = this.period,
                                         tr             = tr,
                                         fl             = fl, ...)
                         )

    ## In case some increments are negative we make sure all trees
    ## keep a minimum diameter of 50mm
    dbh.mm    <- tr$data[["dbh.mm"]][, this.period]
    i.small.dbh <- (dbh.mm + dbh.inc.mm) < 50
    dbhi.small.dbh <- 50 - dbh.mm
    dbh.inc.mm[i.small.dbh] <- dbhi.small.dbh[i.small.dbh]
    
    ## height increment
    if (!is.null(fn.hgt.inc)){
      ##print('---------- Using height increment')
      hgt.inc.dm =   do.call(what = fn.hgt.inc,
                             args = list(common.vars = common.vars,
                                         this.period = this.period,
                                         tr = tr,
                                         fl = fl,
                                         ...))
    } else {
      hgt.inc.dm <- rep(0, length(dbh.inc.mm))

    }
    
    return(data.frame (dbh.inc.mm = dbh.inc.mm, hgt.inc.dm = hgt.inc.dm))
}
## reassignInPackage("grow.dbhinc.hgtinc", "sitree", grow.dbhinc.hgtinc)
