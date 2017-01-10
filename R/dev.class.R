
calculate.development.class <- function(SI.spp, SI.m, stand.age.years){
    spp.c <- SI.spp
    spp.c[spp.c %in% 3] <- "birch"
    spp.c[spp.c %in% c(1:2)] <- "conif"
    spp.c <- data.frame( ID = paste0(spp.c, "-", SI.m))
    
    devel.class <-  data.frame(
        SI.m = rep(c(26,23,20,17,14,11,8,6),2),
        spp     = rep(c("conif","birch"),each=8),
        class3 = c(
            20,20,20,25,30,35,45,55,
            15,15,15,20,25,25,25,30),
        class4 = c(
            40,40,45,55,60,70,75,85,
            25,25,30,40,45,45,45,55),
        class5 = c(
            60,60,70,80,90,100,110,120,
            40,40,50,60,70,70,70,80)
        )
    devel.class$ID <- with(devel.class, paste0(spp, "-", SI.m))

    spp.c[, c("III", "IV", "V")] <- devel.class[match(spp.c$ID, devel.class$ID)
                                                , c("class3", "class4", "class5")]
    dev.class <- rep(1, length(stand.age.years))
    dev.class[is.finite(stand.age.years) & stand.age.years >= spp.c$III] <- 3
    dev.class[stand.age.years >= spp.c$IV] <- 4
    dev.class[stand.age.years >= spp.c$V] <- 5
    return(dev.class)
}

