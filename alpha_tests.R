rm(list=ls()); graphics.off(); cat("\014")

source("core.R")
source("test_problems.R")

set.seed(42)
n0 = 5
N = 20
covtype="gauss"
nugget.estim=T
maximize=F

# Branin modified test function (in BORA is defined over [0,15] x [0,15] )
# and then internally translated to [-5,10] x [0,15]
r.min = 5; r.max = 30
r.budgets = round(runif(N,r.min,r.max))
r.budgets[which(r.budgets<r.min)] = r.min
r.budgets[which(r.budgets>r.max)] = r.max

search.space = data.frame( lower=rep(0,2), upper=rep(15,2) )

X = round(runif(n=n0,min=search.space$lower[1],max=search.space$upper[1]))
X = data.frame(x1=X,x2=r.budgets[1:n0]-X)

y = apply(X,1,f.test)

while( nrow(X)<N ) {
  cbo.res = cbo( X=X, y=y, r.budget=r.budgets[nrow(X)+1], search.space=search.space,
                 maximize=F, beta_=1, covtype="gauss", nugget.estim=nugget.estim )
  
  cbo.res$res$par = round(cbo.res$res$par,2)
  cbo.res$res$par = globalpass.r.budget * cbo.res$res$par/sum(cbo.res$res$par)
  x.next = cbo.res$res$par
  cat("> Next query:",x.next,"\n" )
  X = rbind(X,x.next)
  y = c(y, f.test(x.next) )
}



# visualization ******************************************************
library(plot3D)
n.points = 50
xs = expand.grid(x1=seq(search.space$lower[1],search.space$upper[1],length.out=n.points),
                 x2=seq(search.space$lower[2],search.space$upper[2],length.out=n.points))

ys = apply(xs,1,f.test)
image2D( x=sort(unique(xs$x1)), y=sort(unique(xs$x2)), z=matrix(ys,n.points),
         col=rev(heat.colors(100)) )
contour2D( x=sort(unique(xs$x1)), y=sort(unique(xs$x2)), z=matrix(ys,n.points),
           col="black", nlevels=40, add=T )
points2D( X$x1, X$x2, pch=19, col="blue", cex=1.5, add=T )

for( i in 1:N ) 
  lines( c(0,r.budgets[i]), c(r.budgets[i],0), 
         lty=3, lwd=3, col="blue" )


gp.y = predict( cbo.res$gp, xs, "UK", checkNames=F )
image2D( x=sort(unique(xs$x1)), y=sort(unique(xs$x2)), z=matrix(gp.y$mean,n.points),
         col=rev(cm.colors(100)) )
contour2D( x=sort(unique(xs$x1)), y=sort(unique(xs$x2)), z=matrix(gp.y$mean,n.points),
           col="blue", nlevels=30, add=T )
points2D( X$x1, X$x2, pch=19, col="black", add=T, cex=1.5 )



# ********************************************************************

y.star = 0.397887
best.seen = cummin(c(min(y[1:n0]),y[(n0+1):N]))
plot( n0:N, best.seen, type="l", lwd=4, col="blue",
      ylim=c(y.star,max(best.seen,na.rm=T)), xlim=c(n0,N))
abline(h=y.star,col="red", lwd=3, lty=2)

