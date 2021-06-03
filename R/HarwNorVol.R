harw.nor.vol <-
function (tsl, dbh, trh, bark, units) 
{
    dub <- rep(NA, length(dbh))
    dmb <- round(dbh/10, 3)
    h <- round(trh/10, 1)
    if (bark == "ub") {
        u <- tsl == "30"
        dub[u] <- dmb[u] - ((0.83979 + 0.84069 * dmb[u])/10)
        u <- tsl == "31"
        dub[u] <- dmb[u] - ((-4.04765 + 1.25672 * dmb[u])/10)
        u <- tsl == "32"
        dub[u] <- dmb[u] - ((2.42692 + 1.06035 * dmb[u])/10)
        u <- tsl == "40"
        dub[u] <- dmb[u] - ((7.7772 + 0.61607 * dmb[u])/10)
        u <- tsl >= "41" & tsl <= "49"
        dub[u] <- dmb[u] - ((5.45042 + 0.35234 * dmb[u])/10)
        u <- tsl >= "50" & tsl <= "58"
        dub[u] <- dmb[u] - ((1.60844 + 0.74334 * dmb[u])/10)
        u <- !is.finite(dub)
        dub[u] <- dmb[u] - ((2.95966 + 0.68558 * dmb[u])/10)
        dub <- round(dub, 3)
    }
    if (bark == "mb") {
        vol <- 0.1 * (-18.6827 + (2.1461 * (dmb^2)) + (0.1283 * 
            (dmb^2) * h) + (0.138 * dmb * (h^2)) - (0.6311 * 
            (h^2)))
    }
    else if (bark == "ub") {
        vol <- 0.1 * (-14.8081 + (1.6949 * (dub^2)) + (0.1834 * 
            (dub^2) * h) + (0.1018 * dub * (h^2)) - (0.451 * 
            (h^2)))
    }
    if (units == "l") {
        return(round(vol))
    }
    else if (units == "c") {
        return(round(vol * 100))
    }
}
