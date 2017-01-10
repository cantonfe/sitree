QMD.cm.f <-
function (x.mm) 
{
    x.mm <- x.mm[is.finite(x.mm)]
    (sqrt(sum((x.mm/10)^2)/length(x.mm)))
}
