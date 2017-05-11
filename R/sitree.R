
sitree <- function(tree.df,
                    stand.df,
                    functions,
                    n.periods = 5,
                    period.length,
                    mng.options = NA,
                    print.comments = FALSE,
                    ... ){
  
  others <- list(...)
  
  ## check that functions is a list
  if (!is.list(functions)) stop ('functions should be a list')
  if (!is.data.frame(tree.df)) stop ('tree.df should be a data.frame')
  if ( !(is.data.frame(stand.df) | is.list(stand.df) ) ) {
    stop ('stand.df should be a data.frame or a list')
  }
  ## check that all functions are defined
  if (!all(c('fn.growth', 'fn.mort', 'fn.recr', 'fn.management', 'fn.modif',
             'fn.tree.removal', 'fn.prep.common.vars') %in%
           names(functions))){
    stop ('There are some missing definitions for the functions. Please read sitree man')
  }

  ## check that tree.df and stand.df have all neccessary information
  ## No missing dbh or heights.
  names.tr <- c('ustandID', 'treeid', 'dbh', 'height', 'tree.sp') 
  if (!all(names.tr %in%
           names(tree.df))){
    stop ('There are some missing or incorrecly named columns in tree.df')
  }
  ## tree list should contain at least dbh, height, treeid, ustandID, and species
  if (any(!is.finite(tree.df$dbh)))    stop('Some dbh are NA, please correct it')
  if (any(!is.finite(tree.df$height))) stop('Some heights are NA, please correct it')
  if (any(is.na(tree.df$treeid))) stop('Some treeid are missing, please correct it')
  if (any(is.na(tree.df$tree.sp)))  stop('Some tree.sp are missing, please correct it')
  if (any(is.na(tree.df$ustandID))) stop('Some ustandID in tree.df are missing')

  ## stand
  if (!all(c('ustandID') %in% names(stand.df))) stop ('ustandID is missing from stand.df')
  if (any(is.na(stand.df$ustandID))) stop('Some ustandID in stand.df are missing')

  ## BUILD THE OBJECTS tr and fl
### tree data
  foo <- function(mvec, n.periods) {
    mdt <- matrix(0, nrow = length(mvec), ncol = (n.periods +1))
    mdt[,1] <- mvec
    colnames(mdt) <- paste("t", 0:n.periods, sep = "")
    return(as.data.frame(mdt))
  }

  ## make sure ustandID in tr are defined in stands
  if (any(!tree.df$ustandID %in% stand.df$ustandID)) {
    stop ('ustandID missing in stand.df')
  }
  tr.rest <- as.list(tree.df[, !names(tree.df) %in% names.tr])
  trl <- list(
    ustandID  = tree.df$ustandID,
    treeid    = tree.df$treeid,
    dbh.mm    = foo(tree.df$dbh, n.periods = n.periods ),
    height.dm = foo(tree.df$height, n.periods = n.periods  ),
    yrs.sim   = rep(0, nrow(tree.df)),
    tree.sp   = factor(tree.df$tree.sp)
  )
  trl <- c(trl, tr.rest)
  tr <- trList$new(data = trl, nperiods = as.integer(n.periods))
  ## clean up
  rm(trl, tr.rest)

### stand data, a data.frame or list
  fl <- as.list(stand.df)
  ## create the management data frame if it does not exist
  if (!"management" %in% names(fl)){
    fl$management <- data.frame(matrix(NA, ncol = n.periods,
                                       nrow = length(fl$ustandID)))
    names(fl$management) <- paste0("t", 1:n.periods)
  }
######################################
  ## VARS REQUIRED
######################################
  ## Calculate the variables required by the functions
  ## This is meant to avoid recalculating common variables for each function

  vars.required <- fn.vars.required(my.functions = functions, ...)

  
  
  ## a loop over all periods
  for(i.period in (0: (tr$nperiods - 1))){
    if (print.comments) print(paste0("period: ", i.period))
    
    this.period <- paste0("t", i.period  )
    next.period <- paste0("t", i.period+1)
    
    ## PREP shared variables
    if (i.period == 0)  {
      existing.common.vars <-  "NULL"
    } else {
      existing.common.vars <-  common.vars
    }
    
    prep.common.vars <-  do.call(functions$fn.prep.common.vars,
                                    args = list(
                                      tr = tr,
                                      fl = fl,
                                      this.period    = this.period,
                                      i.period       = i.period,
                                      common.vars    = existing.common.vars,
                                      mng.options    = mng.options,
                                      vars.required  = vars.required,
                                      period.length = period.length,
                                      print.comments = print.comments,
                                      ...
                                    )
                                    )
    
    common.vars <- prep.common.vars$res
    fl <- prep.common.vars$fl
    if (print.comments) print('Passed common vars')

    ## No hgt.inc but hgt function
    if (is.null(functions$fn.hgt.inc) & !is.null(functions$fn.hgt)){
      tr$data$height.dm[, this.period] <-
        do.call(functions$fn.hgt,
                args = list(
                  tr = tr,
                  fl = fl,
                  this.period    = this.period,
                  i.period       = i.period,
                  common.vars    = common.vars,
                  vars.required  = vars.required,
                  period.length  = period.length,
                  print.comments = print.comments,
                  ...
                )
                )
    }
    if (print.comments) print('Passed heights')
    ## EXTERNAL MODIFIERS
    ## External modifiers other than management, e.g. climate change
    ##   external modifieres should be a list with the elements
    ##   to be replaced in fl
    if (!is.null(functions$fn.modif) ){
      ext.modif <- do.call(functions$fn.modif,#.scen,
                           args = list(
                             tr = tr,
                             fl = fl,
                             this.period = this.period,
                             i.period    = i.period,
                             common.vars    = common.vars,
                             vars.required  = vars.required,
                             period.length  = period.length,
                             print.comments = print.comments,
                             ...
                           )
                           )
      ## apply external modifiers, they might change some variables
      if ( !is.null(ext.modif) ) {
        fl[names(ext.modif)] <- ext.modif
        ## recalculate prep.common.vars, it probably change things
        
        prep.common.vars <- do.call(functions$fn.prep.common.vars,
                                    args = list(
                                      tr = tr,
                                      fl = fl,
                                      this.period    = this.period,
                                      i.period       = i.period,
                                      common.vars    = existing.common.vars,
                                      mng.options    = mng.options,
                                      vars.required  = vars.required,
                                      period.length = period.length,
                                      print.comments = print.comments,
                                      ...
                                    )
                                    )
        common.vars <- prep.common.vars$res
        fl <- prep.common.vars$fl
      }
    }
    
    if (print.comments) print('Passed Exter mod')
    
    ## Management - return the management of the different plots
    if ( !is.null(functions$fn.management)){
      management <- do.call(functions$fn.management,#.scen,
                            args = list(
                              tr = tr,
                              fl = fl,
                              common.vars = common.vars,
                              this.period = this.period,
                              next.period = next.period,
                              i.period = i.period,
                              mng.options    = mng.options,
                              print.comments = print.comments,
                              ...)
                            )
      
      if (print.comments) print('Passed management')
      
      ## save that certain stands have been managed in the management table,
      ## not sure it has to be done here, but if possible we should move it down
      ## this was done before inside the fn.management function--- does not work
      ## when working on parallel
      ## CHECK IF POSSIBLE
      
      fl$management[, next.period] <- management$management
      
      removed <- do.call(
        functions$fn.tree.removal,
        args = list(
          tr = tr,
          fl = fl, 
          common.vars = common.vars,
          management = management,
          this.period = this.period,
          next.period = next.period,
          i.period    = i.period,
          mng.options = mng.options,
          print.comments = print.comments,
          ...)
      )
      if (print.comments) print(paste0('sum removed ', sum(removed)))
      if (print.comments) print('Passed removed')
    } else {
      ## we don't remove trees if there is no management
      management <- list(management = rep(NA, length(fl$ustandID)))
      fl$management[, next.period] <- management$management
      removed <- rep(FALSE, nrow(tr$data$dbh.mm))     
    }

    
    ## GROWTH
    growth   <- do.call(
      functions$fn.growth,
      args = list(
        tr = tr,
        fl = fl,
        common.vars = common.vars,
        this.period = this.period,
        next.period = next.period,
        i.period = i.period,
        functions = functions,
        print.comments = print.comments,
        ...)
    )
    if (print.comments) print('Passed growth')
    
    ## DEATH - return a FALSE and TRUE(dead) vector
    mort    <- do.call(
      functions$fn.mort,
      args = list(
        tr = tr,
        fl = fl,
        common.vars = common.vars,
        this.period = this.period,
        next.period = next.period,
        i.period = i.period,
        functions = functions,
        print.comments = print.comments,
        ...)
    )
    if (print.comments) print('Passed mortality')


    ## INGROWTH -natural and artificial  on all plots
    ## returns a data.frame with the new trees
    max.tree.id <-
      ifelse (exists('dead.trees') & exists('removed.trees'), 
              max(c(tr$data$treeid, dead.trees$data$treeid,
                    removed.trees$data$treeid)),
              ifelse(exists('dead.trees'),
                     max(c(tr$data$treeid, dead.trees$data$treeid)),
                     ifelse(exists('removed.trees'),
                            max(c(tr$data$treeid, removed.trees$data$treeid) ),
                            max(tr$data$treeid)
                     )))
    ingrowth <- do.call(
      functions$fn.recr,
      args = list(
        tr = tr,
        fl = fl, 
        common.vars = common.vars,
        i.period    = i.period,
        this.period = this.period,
        next.period = next.period,
        management = management,
        period.length = period.length,
        vars.required  = vars.required,
        mng.options = mng.options,
        print.comments = print.comments,
        max.tree.id = max.tree.id,
        ...)
    )
    
    if (print.comments) print('Passed ingrowth')

        
    ## book keeping
    i.removed <- tr$data$treeid[removed]
    
    ## YEARS IN SIMULATION
    tr$data$yrs.sim <- period.length + tr$data$yrs.sim

    ## GROWTH
    ## grow dbh
    tr$data$dbh.mm[ next.period] <-
      tr$data$dbh.mm[, this.period] + growth[, "dbh.inc.mm"]
    ##i.small.dbh <- tr$data$dbh.mm[ next.period] < 50
    ##tr$data$dbh.mm[ next.period] [i.small.dbh] <- 50
    
    ## grow heigth
    tr$data$height.dm[, next.period] <-
      tr$data$height.dm[, this.period] + growth[, "hgt.inc.dm"]
   
    if (print.comments) print("Growth applied")
    
    ## DEAD TREES
    i.dead.trees <- mort & !removed ## not sure if this is neccessary
    ## extract
    if (sum(i.dead.trees) > 0) {
      new.dead.trees <-  tr$extractTrees(which(i.dead.trees))
    
      ## create dead trees class object
      new.dead.trees <- trListDead$new(
        data = new.dead.trees,
        last.measurement = cbind(
          do.call("dead.trees.growth"
                , args = list(
                  dt     = new.dead.trees,
                  growth = growth,
                  mort   = i.dead.trees,
                  this.period = this.period)
                  ),
          found.dead = next.period
        ),
        nperiods = tr$nperiods
      )
       
      ## remove last measurement --- we could probably skip this step if
      ## we change the code slightly, if only alived trees are grown
      new.dead.trees$remove.next.period(next.period = next.period)
      ## If this is the first period create the dead.trees object, if not
      ## add the new dead trees to the dead.trees object
      if (i.period == 0){
        dead.trees <- new.dead.trees
      } else{
        dead.trees$addTrees(new.dead.trees)
      }
    } else if (i.period == 0) dead.trees <- new.dead.trees
    if (print.comments) print("Dead trees applied")
    
    ## REMOVED TREES
    ## extract
    if (sum(removed) > 0){
      new.removed.trees <- tr$extractTrees(
        which(tr$data$treeid %in% i.removed))
     
      ## create removed trees class object
      new.removed.trees <- trListDead$new(
        data = new.removed.trees,
        last.measurement = cbind(
          do.call("dead.trees.growth"
                , args = list(
                    dt    = new.removed.trees,
                    growth = growth,
                    mort   = removed,
                    this.period = this.period)
                  ),
          found.removed = next.period
        ),
        nperiods = tr$nperiods
      )
      ## remove last measurement --- we could probably skip this step if
      ## we change the code slightly, if only alived trees are grown
      new.removed.trees$remove.next.period(next.period = next.period)
      ## If this is the first period create the removed.trees object, if not
      ## add the new removed trees to the removed.trees object

      if (!exists("removed.trees")){
        removed.trees <- new.removed.trees
      } else {
        removed.trees$addTrees(new.removed.trees)
      }
    }
    if (print.comments) print("removed trees applied")

    ## Apply ingrowth
    if (length(ingrowth$treeid) > 0) tr$addTrees(ingrowth)
    
    ## remove objects
    rm(growth, mort, new.dead.trees, management, removed)

    ## End of the period-loop 
  }
  if (print.comments) print('---- Fixing last period')

  ## For the last period calculate some of the common variables (height)
  ## need to be recalculated, so everything is updated for the last period
  i.period <- tr$nperiods 
  this.period <- paste0("t", i.period  )
  next.period <- paste0("t", i.period+1)
  
  
  prep.common.vars <-  do.call(functions$fn.prep.common.vars,
                                    args = list(
                                      tr = tr,
                                      fl = fl,
                                      this.period    = this.period,
                                      i.period       = i.period,
                                      common.vars    = existing.common.vars,
                                      mng.options    = mng.options,
                                      vars.required  = vars.required,
                                      period.length = period.length,
                                      ...
                                    )
                                    )
  
   fl <- prep.common.vars$fl

 
  
  if (!exists('removed.trees')) removed.trees <- NULL
  
  invisible(list(live =  recover.state(tr   = tr,
                              dead.trees    = dead.trees,
                              removed.trees = removed.trees
                              ),
                 dead = dead.trees,
                 removed = removed.trees,
                 plot.data = fl)
            )
  
}

## reassignInPackage("sitree", "sitree", sitree)
