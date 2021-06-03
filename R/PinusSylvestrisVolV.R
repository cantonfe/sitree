pinus.sylvestris.volV <-
function (dbh, trh, bark, units) 
{
    dmb <- round(dbh/10, 3)
    h <- round(trh/10, 1)
    b <- round(3.17935 + 1.0289 * dmb - 0.27023 * dmb/h, 2)
    if (bark == "ub") {
        dub <- round(dmb - (b/10), 3)
    }
    if (bark == "mb") {
        vol <- round(0.1424 * (h^2.0786) * (dmb^1.9028) * ((h - 
            1.3)^(-1.0259)) * ((dmb + 100)^(-0.264)), 2)
    }
    else if (bark == "ub") {
        vol <- round(0.1346 * (h^2.0041) * (dub^1.9116) * ((h - 
            1.3)^(-0.8778)) * ((dub + 100)^(-0.2844)), 2)
    }
    if (units == "l") {
        return(round(vol))
    }
    else if (units == "c") {
        return(round(vol * 100))
    }
}
