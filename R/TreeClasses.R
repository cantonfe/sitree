
########################
## DEFINE CLASS TREELIST
########################
trList <- setRefClass(Class    = "trList",
                      fields   = list(
                          data     = "list",
                          nperiods = "integer"),
                      methods  = list(
                        getTrees = function(i, j = (1:(nperiods+1))){
                          'Gets tree(s) i info all fields.
                           j defines the columns or period names, by default it selects all
                          It does not display it, just passes the value'
                          ## I think i only works when is numeric (not TRUE/FALSE)
                          are.data.frame <-  sapply(data, "is.data.frame")
                          ## create same structure as data
                          x <- list()
                          x[names(data) [!are.data.frame]] <- 
                            sapply(data[!are.data.frame], "[", i, simplify = FALSE)
                          x[names(data) [are.data.frame]] <-
                            sapply(data[are.data.frame], "[",
                                   i = i,
                                   j = j,
                                   simplify = FALSE, drop = FALSE)
                          return(x[names(data)])
                        },
                        extractTrees = function(i){
                          'Extract tree(s). Removes tree data from the tree list and
                           returns (invisible) the data for the trees'
                          are.data.frame <-  sapply(data, "is.data.frame")
                          ## get trees' data
                          x <- getTrees(i)
                          ## remove trees
                          data[!are.data.frame] <<-
                            sapply(data[!are.data.frame], "[", -i, simplify = FALSE)
                          data[names(data) [are.data.frame]] <<-
                            sapply(data[are.data.frame], "[",
                                   i = -i,
                                   j = (1:(nperiods+1)),
                                   simplify = FALSE, drop = FALSE)
                          invisible(x[names(data)])
                        },
                        addTrees = function(value){
                            'add new trees to existing trList object'
                          ## I need to add some object validation for value
                            if(!is.list(value)){stop("Error the new data whould be a list")}
                            if(!all( names(data) %in% names(value))){
                                stop("Error: missing elements in the list")}
                            if(sum(names(data) != names(value)) > 0){
                                stop("The elements of the list are not in the correct order")}
                            data <<- mapply(toBindLists, x = data, y = value)
                            invisible("done")                            
                        },
                        show = function() {
                          'Method for automatically printing matrix editors'
                          cat("Reference tree list object of class",
                              classLabel(class(.self)), "\n")
                          cat("Data: \n")
                          cat("Showing the first 20 trees \n")
                          for ( my.var in names(data)){
                            if (is.data.frame(data[[my.var]])){
                              cat(my.var); cat(" \n")
                              print(data[[my.var]] [1:20,])
                            } else {
                              cat(my.var); cat(" \n"); print(data[[my.var]] [1:20])
                            }
                            cat(" \n")
                          }
                          
                      },
                          as.list = function(){
                              'Method for converting to class list'
                              return(getTrees(
                                  i = 1:length(data$ustandID)
                                  , j = 1:(nperiods+1)
                                  ))
                          }
                          )
)






## This class builds on the TreeList (trList) class.
## This classs is meant to keep the info of the dead trees in a very
## similar way as the live trees but with some extra info such as diameter
## and height at last measurement (1/2 of the BAi predicted)


trListDead <- setRefClass(Class    = "trListDead",
                          contains = "trList",
                          fields   = list( last.measurement     = "data.frame"),
                          methods  = list(
                            remove.next.period = function(next.period){
                              are.data.frame <-  sapply(data, "is.data.frame")
                              ## NEXT.PERIOD TO ZERO
                              fn.zero <- function(x, next.period){
                                x[, next.period] <- 0
                                x
                              }
                              data[are.data.frame ] <<-
                                sapply(
                                  data[are.data.frame],
                                  FUN         = fn.zero,
                                  next.period = next.period,
                                  simplify    = FALSE
                                  )
                              data$yrs.sim <<- data$yrs.sim - 2.5
                            },
                            addTrees = function(value){
                              ## I need to add some object validation for value
                              data <<- mapply(toBindLists,
                                              x = data,
                                              y = value$data[names(data)])
                              last.measurement <<- rbind(last.measurement,
                                                         value$last.measurement)
                              invisible("done")
                            },
                            last.time.alive = function(){
                              ## calculate when dead trees where last alive
                              last.zero <- function(x){
                                names(x[length(x):1][match(FALSE,x[length(x):1])])
                              }
                              dum <- as.matrix(data$dbh.mm) == 0
                              dum <- apply(dum, 1, last.zero)
                              invisible(dum)
                            }
                              )
                          )
