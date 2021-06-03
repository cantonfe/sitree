picea.abies.vol <-
function (dbh,  trh, bark, units) 
{
    f.dbh <- dbh
    dmb <- dbh/10
    f.dmb <- f.dbh/10
    h <- trh/10
    vol <- rep(NA, length(dbh))
    if (bark == "ub") {
        dub <- dmb - ((1.07745 + 0.88992 * dmb - 0.00327 * dmb * 
            dmb - 0.01346 * h * h + 0.03278 * dmb * dmb/(h * 
            h))/10)
        dub <- round(dub, 3)
        f.dub <- f.dmb - ((1.07745 + 0.88992 * f.dmb - 0.00327 * 
            f.dmb * f.dmb - 0.01346 * h * h + 0.03278 * f.dmb * 
            f.dmb/(h * h))/10)
        f.dub <- round(f.dub, 1)
    }
    if (bark == "mb") {
        i <- f.dmb < 10.1
        vol[i] <- 0.52 + (0.02403 * (dmb[i]^2) * h[i]) + (0.01463 * 
            dmb[i] * h[i]^2) - (0.10983 * h[i]^2) + (0.15195 * 
            dmb[i] * h[i])
        i <- f.dmb >= 10.1 & f.dmb <= 12.9
        vol[i] <- -31.57 + (0.0016 * dmb[i] * h[i]^2) + (0.0186 * 
            h[i]^2) + (0.63 * dmb[i] * h[i]) - (2.34 * h[i]) + 
            (3.2 * dmb[i])
        i <- f.dmb > 12.9
        vol[i] <- 10.14 + (0.0124 * (dmb[i]^2) * h[i]) + (0.03117 * 
            dmb[i] * h[i]^2) - (0.36381 * h[i]^2) + (0.28578 * 
            dmb[i] * h[i])
    }
    else if (bark == "ub") {
        i <- f.dmb < 10.1
        vol[i] <- 0.38 + (0.02524 * (dub[i]^2) * h[i]) + (0.01269 * 
            dub[i] * h[i]^2) - (0.07726 * h[i]^2) + (0.111671 * 
            dub[i] * h[i])
        i <- f.dmb >= 10.1 & f.dmb <= 12.9
        vol[i] <- -27.19 + (0.0073 * dub[i] * h[i]^2) + (0.0228 * 
            h[i]^2) + (0.5667 * dub[i] * h[i]) - (1.98 * h[i]) + 
            (2.75 * dub[i])
        i <- f.dmb > 12.9
        vol[i] <- 8.66 + (0.01218 * (dub[i]^2) * h[i]) + (0.02976 * 
            dub[i] * h[i]^2) - (0.31373 * h[i]^2) + (0.25452 * 
            dub[i] * h[i])
    }
    if (units == "l") {
        return(round(vol, 0))
    }
    else if (units == "c") {
        return(round(vol * 100, 0))
    }
}
