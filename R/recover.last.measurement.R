
## Function to recover the last measurement

recover.last.measurement <- function(tr.list.dead){

    if ("found.removed" %in% names(tr.list.dead$last.measurement)) {
        found <- "found.removed"} else {found <- "found.dead"}
    
    periods <- paste0("t", 1:(ncol(tr.list.dead$data$height.dm) - 1))
    
    ## res$removed$data only contains the "history of the tree", but we would
    ## like to recover the dbh and height of the tree at harvest time,
    ## which is recorded in $last.measurement

    ## removed data before the death of the trees
    tr.list.dead$data$dbh.mm[tr.list.dead$data$dbh.mm > 0] <- 0
    tr.list.dead$data$height.dm[tr.list.dead$data$height.dm > 0] <- 0
    ## add last measurement
    dbh.mm <- reshape(tr.list.dead$last.measurement,
                      v.names = "dbh.mm",
                      timevar = found,
                      idvar = "treeid",
                      direction = "wide"
                      )
    tr.list.dead$data$dbh.mm[, periods] <- dbh.mm[, paste0("dbh.mm.",  periods)]
    
    height.dm <- reshape(tr.list.dead$last.measurement,
                         v.names = "height.dm",
                         timevar = found,
                         idvar = "treeid",
                         direction = "wide"
                         )
    
    
    tr.list.dead$data$height.dm[, periods] <-
        height.dm[, paste0("height.dm.",  periods)]
   
    
    ## convert NA to 0 to be consistent
    tr.list.dead$data$dbh.mm[is.na(tr.list.dead$data$dbh.mm)] <- 0
    tr.list.dead$data$height.dm[is.na(tr.list.dead$data$height.dm)] <- 0
    return(tr.list.dead)
}
