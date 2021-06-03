picea.abies.volV <-
function (dbh, trh, bark, units) 
{
    dmb <- dbh/10
    h <- trh/10
    if (bark == "ub") {
        dub <- dmb - ((1.07745 + 0.88992 * dmb - 0.00327 * dmb * 
            dmb - 0.01346 * h * h + 0.03278 * dmb * dmb/(h * 
            h))/10)
    }
    if (bark == "mb") {
        vol <- 0.6844 * (h^3.0296) * (dmb^2.056) * ((h - 1.3)^(-1.7377)) * 
            ((dmb + 40)^(-0.9756))
    }
    else if (bark == "ub") {
        vol <- 0.6127 * (h^2.9157) * (dub^2.0294) * ((h - 1.3)^(-1.6173)) * 
            ((dub + 40)^(-0.9359))
    }
    if (units == "l") {
        return(round(vol))
    }
    else if (units == "c") {
        return(round(vol * 100))
    }
}
