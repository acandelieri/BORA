rm(list=ls()); graphics.off(); cat("\014")

source("core.R")

library(plot3D)

# best pictures:
# a) seed=1 & iter.lim=5
# b) 
seed = 3
is.const.budget = T
n.pts = 100
iter.lim = 10

set.seed(seed)

files = list.files("results_case1/",recursive=F)
files = files[grep("arms_2.",files,fixed=T)]
files = files[grep(paste0("constbgt_",is.const.budget),files,fixed=T)]
files = files[grep(paste0("_",seed,"_"),files,fixed=T)]
files = files[grep("BORA",files,fixed=T)]

df1 = readRDS( paste0("results_case1/",files[grep("BORA1",files,fixed=T)]) )
df2 = readRDS( paste0("results_case1/",files[grep("BORA2",files,fixed=T)]) )
df3 = readRDS( paste0("results_case1/",files[grep("BORA3",files,fixed=T)]) )

gp1 = km( design=df1[1:iter.lim,-3], response=df1$y[1:iter.lim], covtype="gauss",
          nugget.estim=T, control=list(trace=0) )

#************************************************************************
# 2D approximation
#************************************************************************
Xs = expand.grid( x1=seq(0,100,length.out=n.pts),
                  x2=seq(0,100,length.out=n.pts) )
y1 = predict( gp1, Xs, "UK", checkNames=F )

par( mfrow=c(2,2) ); curr.mar = par("mar") 

# GP's posterior mean
par(mar=c(1.1,1.1,2.1,1.1))
persp3D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)),
         z=matrix(y1$mean,n.pts), col=rev(heat.colors(100)),
         main="GP's posterior mean", colkey=F, cex.lab=1.5, cex.main=1.5,
         xlab="x1", ylab="x2",  zlab="Immediate reward" )
par(mar=c(4.6,4.6,3.1,3.6))
image2D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)),
         cex.lab=1.5, cex.main=1.5, cex.axis=1.5, 
         z=matrix(y1$mean,n.pts), col=rev(heat.colors(100)),
         xlab="x1", ylab="x2", main="GP's posterior mean (contour plot)" )
contour2D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)), nlevels=10,
           z=matrix(y1$mean,n.pts), col="firebrick", add=T )
points2D( df1$X.1[1:iter.lim], df1$X.2[1:iter.lim],
          pch=19, cex=1.2, col="black", add=T )


# GP's posterior sd
par(mar=c(1.1,1.1,2.1,1.1))
persp3D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)),
         z=matrix(y1$sd,n.pts), col=cm.colors(100),
         main="GP's prosterior std. dev.", colkey=F, cex.lab=1.5, cex.main=1.5,
         xlab="x1", ylab="x2",  zlab="" )
par(mar=c(4.6,4.6,3.1,3.6))
image2D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)),
         cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
         z=matrix(y1$sd,n.pts), col=cm.colors(100),
         xlab="x1", ylab="x2", main="GP's posterior std. dev. (contour plot)")
contour2D( x=sort(unique(Xs$x1)), y=sort(unique(Xs$x2)), nlevels=10,
           z=matrix(y1$sd,n.pts), col="blue", add=T )
points2D( df1$X.1[1:iter.lim], df1$X.2[1:iter.lim],
          pch=19, cex=1.2, col="black", add=T )

par( mfrow=c(1,1) )

invisible(readline("Press [RETURN] to continue..."))

#************************************************************************
# 1D projection
#************************************************************************

budget = df1$X.1[1] + df1$X.2[1]

Xs = round(seq(0,budget,length.out=n.pts),2)
Xs = data.frame( x1=Xs, x2=budget-Xs )
Xs = Xs[which(Xs$x2>=0),]

y1 = predict(gp1,Xs,"UK",checkNames=F)
par(mar=c(4.1,4.6,2.1,2.1))
plot( Xs$x1, y1$mean, type="l", ylim=c(0,max(y1$mean+y1$sd)),
      xlab=expression(x[1]), ylab="GP's posterior mean",
      cex.lab=1.5, cex.axis=1.5 )
polygon( c(Xs$x1,rev(Xs$x1)), c(y1$mean+y1$sd,rev(y1$mean-y1$sd)),
         col=adjustcolor("blue",alpha.f=0.2), border="blue" )
lines( Xs$x1, y1$mean, type="l", lwd=4, col="blue" )
points( df1$X.1[1:iter.lim], df1$y[1:iter.lim], pch=19, col="blue", cex=1.5 )

legend("bottomright", legend=c("BORA1's GP"), lwd=4, col=c("blue"), cex=1.5 )

invisible(readline("Press [RETURN] to continue..."))

#************************************************************************
# Other GPs
#************************************************************************
res2 = bora2( A=df2[1:iter.lim,3:4], y=df2$y[1:iter.lim], maximize=T,
              beta_=1, covtype="gauss", nugget.estim=T )
gp2 = res2$gp


As = seq(0,1,length.out=n.pts)
As = data.frame( a1=As, a2=1-As )


y2 = predict(gp2,As,"UK",checkNames=F)
polygon( c(As$a1*budget,rev(As$a1*budget)), c(y2$mean+y2$sd,rev(y2$mean-y2$sd)),
         col=adjustcolor("green3",alpha.f=0.2), border="green3" )
lines( As$a1*budget, y2$mean, type="l", lwd=4, col="green3" )
points( df2$X.1[1:iter.lim], df2$y[1:iter.lim], pch=19, col="green3", cex=1.5 )


legend("bottomright", legend=c("BORA1's GP","BORA2's GP"),
       lwd=4, col=c("blue","green3"), cex=1.5 )

invisible(readline("Press [RETURN] to continue..."))

res3 = bora3( A=as.matrix(df3[1:iter.lim,3:4]), y=df3$y[1:iter.lim], maximize=T,
              beta_=1, covtype="gauss", nugget.estim=T )

k.vec = numeric(iter.lim)
K = computeKwse( as.matrix(df3[1:iter.lim,3:4]),
                 res3$kernel.params$sig, res3$kernel.params$rho )
K = K + diag(res3$kernel.params$nugget,nrow(K))
K.inv = solve.default(K)

y3.mu = y3.sd = numeric(n.pts)
for( jj in 1:n.pts ) {
  for( i in 1:iter.lim ) {
    k.vec[i] = wsek(As[jj,],df3[i,3:4],
                    res3$kernel.params$sig,
                    res3$kernel.params$rho )
  }
  y3.mu[jj] = t(k.vec) %*% K.inv %*% df3$y[1:iter.lim]
  y3.sd[jj] = wsek(As[jj,],As[jj,],
                   res3$kernel.params$sig,
                   res3$kernel.params$rho) - t(k.vec) %*% K.inv %*% k.vec
}

polygon( c(As$a1*budget,rev(As$a1*budget)), c(y3.mu+y3.sd,rev(y3.mu-y3.sd)),
         col=adjustcolor("red",alpha.f=0.2), border="red" )
lines( As$a1*budget, y3.mu, type="l", lwd=4, col="red" )
points( df3$X.1[1:iter.lim], df3$y[1:iter.lim], pch=19, col="red", cex=1.5 )

legend("bottomright", legend=c("BORA1's GP","BORA2's GP","BORA3's GP"),
       lwd=4, col=c("blue","green3","red"), cex=1.5 )
