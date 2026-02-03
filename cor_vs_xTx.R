## Show that we can use eigen values of correlation matrix are the same as those
## of X^TX.

USArrests

eigen(cor(USArrests))

x <- scale(as.matrix(USArrests))

XX <- t(x) %*% x


eigen(XX)

### Now using prcomp




