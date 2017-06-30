## We need to find a distribution of age by SI.m, SI.spp, and age class.

## This function estimates tree age based on stand age
tree.age <- function(stand.age.years  ,# = fl$stand.age.year[i.stand] ,
                     plot.id         ,# = tr$data$plot.id,
                     tree.BA.m2       ,
                     dbh.mm           ,
                     SI.spp           ,
                     SI.m             ,
                     spp              ,
                     dev.class        ,
                     apply.correction = TRUE
                     )
  ## Age in the NNFI is calculated proportional to the BA
{
  coefs.tree <- list(Intercept =  40.0890843,
                     dbh.mm    =   0.1691102,
                     pine      = -14.5538376,
                     spruce    = -19.2002979,
                     stand.age = 0.5057425,
                     SI.SI.spp.1 = -1.8147865,
                     SI.SI.spp.2 = -2.1057339,
                     SI.SI.spp.3 = -2.1398690
                     )
  
  ## if stand.age is NA is because it is very young hogtskl 11 and 12
  ## so we assign it 0 to calculate tree.age
  ## but we need to consider this when we recalculate tree age to fit stand age
  
  i.young <- dev.class < 3
  tree.ages <- (
    coefs.tree[["Intercept"]] +
      coefs.tree[["dbh.mm"]]    * dbh.mm +
      coefs.tree[["pine"]]      * (spp == "pine") +
      coefs.tree[["spruce"]]    * (spp == "spruce") +
      coefs.tree[["stand.age"]] * stand.age.years +
      coefs.tree[["SI.SI.spp.1"]] * SI.m * (SI.spp==1) +
      coefs.tree[["SI.SI.spp.3"]] * SI.m * (SI.spp==2) +
      coefs.tree[["SI.SI.spp.3"]] * SI.m * (SI.spp==3)
  )
  ## min tree.age is 13
  tree.ages[tree.ages < 13] <- 13
  
  tree.ages[i.young] <- stand.age.years[i.young]

  if (apply.correction){ ## only when calculating whole stands
    ## Correction to age
    correction <- function(x){
      w <- x$tree.BA.m2/sum(x$tree.BA.m2)
      return(x$stand.age.years[1] - weighted.mean(x = x$tree.ages,
                                                  w = w))
    }
    
    x <- by(data.frame(tree.ages, stand.age.years, tree.BA.m2),
            list( plot.id),
            FUN = correction)
    x <- data.frame(plot.id = names(x), x = as.numeric(x))
    ## head(x$x[match(plot.id, x$plot.id)][i.young])
    
    tree.ages <- round(tree.ages) + round(x$x[match(plot.id,
                                                    x$plot.id)])
    
    ## For stands with devel.class below III we don't need age because
    ## we calculate stand
    ## age at stand level
    stand.age.min <- 20
    tree.age.min  <- 13
    old.i <- length(unique(plot.id))
        
    while (sum(tree.ages[!i.young] < 13 |  tree.ages[!i.young] > 400) > 0){
      
      i.fix <- unique( plot.id[tree.ages[!i.young] < 13 |
                                  tree.ages[!i.young] > 400])
      i.tree <- plot.id %in% i.fix
      ## anything below 13 will be 13
      if (old.i == length(i.fix)) tree.age.min <- tree.age.min + 5
      tree.ages[!i.young & tree.ages < 13] <- tree.age.min
      ## anything above 400 is 400
      tree.ages[tree.ages > 400] <- 400
      x <- by(data.frame(  tree.ages       = tree.ages[i.tree]
                       , stand.age.years = stand.age.years[i.tree]
                       , tree.BA.m2      = tree.BA.m2[i.tree]),
              list( plot.id[i.tree]),
              FUN = correction)
      x <- data.frame(plot.id = names(x), x = as.numeric(x))
      
      tree.ages[i.tree] <- round(tree.ages)[i.tree] +
        round(x$x[match(plot.id[i.tree], x$plot.id)])
      old.i <- length(i.fix)
    }
  }
  return(round(tree.ages))
}
## reassignInPackage("tree.age", "sitree", tree.age)
