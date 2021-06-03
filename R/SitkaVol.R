sitka.vol <-
function (dbh, trh, bark, units) 
{
    dmb <- dbh/10
    h <- trh/10
    if (bark == "ub") {
        dub <- dmb - ((1.07745 + 0.88992 * dmb - 0.00327 * dmb * 
            dmb - 0.01346 * h * h + 0.03278 * dmb * dmb/(h * 
            h))/10)
        dub <- round(dub, 3)
    }
    if (bark == "mb") {
        vol <- 0.1614 * h^(3.706) * dmb^(1.9747) * (h - 1.3)^(-2.2905) * 
            (dmb + 40)^(-0.6665)
    }
    else if (bark == "ub") {
        vol <- 0.1507 * (h^3.7177) * (dub^1.962) * ((h - 1.3)^-2.2808) * 
            ((dub + 40)^-0.659)
    }
    if (units == "l") {
        return(round(vol))
    }
    else if (units == "c") {
        return(round(vol * 100))
    }
}
