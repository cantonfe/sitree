
## Calculates volume the way it is not in the NFI
## all variables except for vol.w and vol.wo need to be of the same length
## dbh.mm needs to be at least 50mm

volume.norway <- function(dbh.mm, height.dm, tree.sp, kom, vol.reduksjon = NULL,
                          vol.w = TRUE, vol.wo = TRUE){

nvl <-
function (...) 
{
    for (e in eval(substitute(alist(...)))) {
        x <- eval(e, parent.frame())
        if (!is.null(x)) 
            break
    }
    x
}

  if (length(kom) != length(dbh.mm)) stop('kom should be the same length as dhb.mm')
  if (is.null(vol.reduksjon)) vol.reduksjon <- rep(0, length(dbh.mm))
  if (any (dbh.mm < 50)) stop ('all dbh.mm should be at least 50mm')

  ## Find out in which fylke and kommune the stand is
  ## FYLKE
  kom <- as.character(kom, lenght = 4)
  komnr.n <- nchar(kom) == 3
  kom[komnr.n] <- paste("0", kom[komnr.n], sep = "")
  fylkene <- substr(kom, start = 1, stop = 2)
  
  i.fylnr.1   <- fylkene %in% c('11', '12', '14', '15')
  
  ## making sure the species is numeric and not a factor
  if (is.factor(tree.sp)){
    num.species <- as.numeric(levels(tree.sp)[tree.sp])
  } else{
    num.species <- tree.sp
  }
  i.tree.sp.1 <- num.species %in% c( 1,  3, 21, 29) ## Picea abies
  i.tree.sp.2 <- num.species %in% c( 2) ## Sitka spruce
  i.tree.sp.3 <- num.species %in% c(10, 11, 12, 20) ## pine
  i.tree.sp.4 <- num.species  > 29

  if ((sum(i.tree.sp.1) + sum(i.tree.sp.2) +
         sum(i.tree.sp.3) + sum(i.tree.sp.4)) != length(num.species))
     {
                             print( num.species[which(!i.tree.sp.1 & !i.tree.sp.2 & !i.tree.sp.3 & !i.tree.sp.4)])
                              stop('some species are not recognized')
                            }
  
  vol.bark.l <- vol.wo.bark.l <- rep(NA, length(dbh.mm))
  
  i11 <- i.tree.sp.1 & i.fylnr.1
  i12 <- i.tree.sp.1 & !i.fylnr.1
  i31 <-  i.tree.sp.3 & i.fylnr.1
  i32 <-  i.tree.sp.3 & !i.fylnr.1 
  
  ##################
  ## WITHOUT BARK
  ###################
 
  if (vol.wo){
    
    ## GRAN WEST
    vol.wo.bark.l[i11] <-
      picea.abies.volV (dbh   = dbh.mm[i11],
               trh   = height.dm[i11],
               bark  = 'ub',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i11],0)/100));

    ## GRAN
    vol.wo.bark.l[i12] <-
      picea.abies.vol(dbh   = dbh.mm[i12],
              #f.dbh = dbh.mm[i12],
              trh   = height.dm[i12],
              bark  = 'ub',
              units = 'l'
              ) * (1 - (nvl(vol.reduksjon[i12],0)/100))
    
    ## SITKA
    vol.wo.bark.l[ i.tree.sp.2] <-
      sitka.vol(dbh   = dbh.mm[i.tree.sp.2],
               trh   = height.dm[i.tree.sp.2],
               bark  = 'ub',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i.tree.sp.2],
                             0)/100));
    
    ## FURU WEST
    vol.wo.bark.l[i31] <-
      pinus.sylvestris.volV (dbh   = dbh.mm[i31],
               trh   = height.dm[i31],
               bark  = 'ub',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i31],0)/100));
    
    ## FURU 
    vol.wo.bark.l[i32] <-
      pinus.sylvestris.vol(dbh   = dbh.mm[i32],
              #f.dbh = dbh.mm[i32],
              trh   = height.dm[i32],
              bark  = 'ub',
              units = 'l'
              ) * (1 - (nvl(vol.reduksjon[i32],0)/100));
    
    ## LAUV
    vol.wo.bark.l[i.tree.sp.4] <-
      harw.nor.vol(tsl    = as.integer(tree.sp[i.tree.sp.4]),
              dbh    = dbh.mm[i.tree.sp.4],
              trh    = height.dm[i.tree.sp.4],
              bark   = 'ub',
              units  = 'l'
              ) * (1 - (nvl(vol.reduksjon[i.tree.sp.4],0)/100));
    
  }

  ##################
  ## WITH BARK
  ###################
  ## GRAN WEST
  if (vol.w){
    vol.bark.l[i11] <-
      picea.abies.volV (dbh   = dbh.mm[i11],
               trh   = height.dm[i11],
               bark  = 'mb',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i11], 0)/100))


    ## GRAN
    vol.bark.l[i12] <- picea.abies.vol(dbh   = dbh.mm[i12], 
                               #f.dbh = dbh.mm[i12],
                               trh   = height.dm[i12],
                               bark  = 'mb',
                               units = 'l'
                               ) * (1 - (nvl(vol.reduksjon[i12],0)/100));


    ## SITKA
    vol.bark.l[ i.tree.sp.2] <-
      sitka.vol(dbh   = dbh.mm[i.tree.sp.2],
               trh   = height.dm[i.tree.sp.2],
               bark  = 'mb',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i.tree.sp.2],0)/100));


    ## FURU WEST
    vol.bark.l[i31] <-
      pinus.sylvestris.volV (dbh   = dbh.mm[i31],
               trh   = height.dm[i31],
               bark  = 'mb',
               units = 'l'
               ) * (1 - (nvl(vol.reduksjon[i31],0)/100));

    ## FURU 
    vol.bark.l[i32]<- pinus.sylvestris.vol(dbh   = dbh.mm[i32],
                              #f.dbh = dbh.mm[i32],
                              trh   = height.dm[i32],
                              bark  = 'mb',
                              units = 'l'
                              ) * (1 - (nvl(vol.reduksjon[i32],0)/100));
    
    
    ## LAUV
    vol.bark.l[i.tree.sp.4] <-
      harw.nor.vol(tsl   = as.integer(tree.sp[i.tree.sp.4]), 
              dbh   = dbh.mm[i.tree.sp.4],
              trh   = height.dm[i.tree.sp.4],
              bark  = 'mb',
              units = 'l'
              ) * (1 - (nvl(vol.reduksjon[i.tree.sp.4],0)/100));
    
    
  }


  volume.norway <- list()
  if (vol.w)  volume.norway$vol.w.tr.m3  <- vol.bark.l    / 1000
  if (vol.wo) volume.norway$vol.wo.tr.m3 <- vol.wo.bark.l / 1000
  return(volume.norway)
}

