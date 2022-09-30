# f.test <- function(xx, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {
#   
#   x1 <- xx[1]
#   x2 <- xx[2]
#   
#   # modified for BORA: ************************************
#   x1 <- x1 - 5
#   # *******************************************************
#   
#   term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
#   term2 <- s*(1-t)*cos(x1)
#   
#   y <- term1 + term2 + s
#   
#   return(y)
# }



case1 <- function(x.t, nu ) {
  stopifnot( is.vector(x.t) && length(x.t)==length(nu) )
  thr = apply(cbind(rep(1,length(nu)),x.t/nu),1,min )
  # smp = runif(length(nu))
  # out = smp<=thr
  out = rbernoulli(length(nu),thr)
  out = as.numeric(out)
  return( list( out=out, thr=thr ) ) 
}





# nu = c(25,50)
# X = expand.grid( x1=seq(0,100,by=1), x2=seq(0,100,by=1) )
# X = as.matrix(X)
# y = thr = NULL
# for( i in 1:nrow(X) ) {
#   res = case1( X[i,], nu )
#   y = rbind( y, res$out )
#   thr = rbind( thr, res$thr )
# }
# 
# library(plot3D)
# image2D( z=matrix(apply(y,1,sum),length(unique(X[,1]))),
#          x=sort(unique(X[,1])), y=sort(unique(X[,2])),
#          col=rev(heat.colors(100)) )
# for( b in budget)
#   lines( c(0,b), c(b,0), col="blue", lwd=2, lty=2 )
# 
# 
# 
# budget = runif(1)
# budget = round(budget * (100-10) + 10)
# 
# X = runif(10,0,budget)
# X = cbind(X,budget-X)
# y = thr = NULL
# for( i in 1:nrow(X) ) {
#   res = case1( X[i,], nu )
#   y = rbind( y, res$out )
#   thr = rbind( thr, res$thr )
# }
# 
# plot( sort(X[,1]), apply(y,1,sum)[order(X[,1])], type="o" )
# 
# library(DiceKriging)
# gp = km( design=data.frame(x=X[,1]), response=apply(y,1,sum), covtype="gauss",
#          nugget.estim=T, control=list(trace=0) )
# x_ = seq(0,budget,length.out=50)
# y_ = predict( gp, data.frame(x=x_), "UK" )
# lines( x_, y_$mean, col="blue", lwd=2 )
# polygon( c(x_,rev(x_)), c(y_$mean+y_$sd,rev(y_$mean-y_$sd)),
#          col=adjustcolor("blue",alpha.f=0.2), border=NA )
