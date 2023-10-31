rm(list=ls()); graphics.off(); cat("\014")


lim.T = Inf
display.const.budget = F
is.2D.case = F


files = list.files("results_case1/",recursive=F)
if( is.2D.case ) {
  files = files[-grep("arms_20",files,fixed=T)]
} else {
  files = files[grep("arms_20",files,fixed=T)]
}


df = NULL
for( f in files ) {
  cat("> Loading file: '",f,"'...\n",sep="")
  str = unlist(strsplit( gsub(".RDS","",f), "_", fixed=T))
  algo = str[1]
  is.stochastic = as.logical(str[3])
  seed = as.integer(str[4])
  is.constant.budget = as.logical(str[6])
  ds = readRDS( paste0("results_case1/",f ) )
  if( algo %in% c("BORA2","BORA3","LATTIMORE2014optinit","LATTIMORE2014rndinit") ) {
    N = (ncol(ds)-1)/2
    ds = ds[,-c((N+1):(2*N))]
    cat("!!!!! Warning: rimosse colonne relative alle misure (per conformita' con il dataframe)\n")
  }
  df = rbind( df,
              data.frame( algo=rep(algo,nrow(ds)),
                          standard.output=rep(is.stochastic,nrow(ds)),
                          seed=rep(seed,nrow(ds)),
                          constant.budget=rep(is.constant.budget,nrow(ds)),
                          iter=1:nrow(ds), ds ) )
}


# constant budget: BORA1 vs BORA2 --------------------------------------------------------
b1 = df[which(df$algo=="BORA1" & df$standard.output==T & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
b2 = df[which(df$algo=="BORA2" & df$standard.output==T & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
b3 = df[which(df$algo=="BORA3" & df$standard.output==T & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
lat2014opt = df[which(df$algo=="LATTIMORE2014optinit" & df$standard.output==T & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
# lat2014rnd = df[which(df$algo=="LATTIMORE2014rndinit" & df$standard.output==T & df$constant.budget==display.const.budget & df$iter<=lim.T), ]

Y.MAX = log10(N * max(df$iter))
plot( rep(1,max(df$iter)), col=NA, ylim=c(0,Y.MAX) )
b1M = NULL
for( s in unique(b1$seed) ) {
  lines( log10(cumsum(b1$y[which(b1$seed==s)])), col="green3", lwd=2 )
  b1M = rbind(b1M,log10(cumsum(b1$y[which(b1$seed==s)])))
}
b2M = NULL
for( s in unique(b2$seed) ) {
  lines( log10(cumsum(b2$y[which(b2$seed==s)])), col="green3", lwd=2 )
  b2M = rbind(b2M,log10(cumsum(b2$y[which(b2$seed==s)])))
}
b3M = NULL
for( s in unique(b3$seed) ) {
  lines( log10(cumsum(b3$y[which(b3$seed==s)])), col="red", lwd=2 )
  b3M = rbind(b3M,log10(cumsum(b3$y[which(b3$seed==s)])))
}
lat2014optM = NULL
for( s in unique(lat2014opt$seed) ) {
  lines( log10(cumsum(lat2014opt$y[which(lat2014opt$seed==s)])), col="purple", lwd=2 )
  lat2014optM = rbind(lat2014optM,log10(cumsum(lat2014opt$y[which(lat2014opt$seed==s)])))
}
# lat2014rndM = NULL
# for( s in unique(lat2014rnd$seed) ) {
#   lines( log10(cumsum(lat2014rnd$y[which(lat2014rnd$seed==s)])), col="red", lwd=2 )
#   lat2014rndM = rbind(lat2014rndM,log10(cumsum(lat2014rnd$y[which(lat2014rnd$seed==s)])))
# }
# legend("bottomright",legend=c("BORA1","BORA2","BORA3","Lattimore et al.2014","Lattimore et al. 2014 (rand. init.)"),lwd=2,
#        col=c("green3","green3","red","purple","black"))
legend("bottomright",legend=c("BORA1","BORA2","BORA3","SBF"),lwd=2,
       col=c("green3","green3","red","purple"))

b1.m = apply(b1M,2,mean)
if( nrow(b1M)>1 ) {
  b1.s = apply(b1M,2,sd)
} else {
  b1.s = rep(0,length(b1.m))
}

b2.m = apply(b2M,2,mean)
if( nrow(b2M)>1 ) {
  b2.s = apply(b2M,2,sd)
} else {
  b2.s = rep(0,length(b2.m))
}

b3.m = apply(b3M,2,mean)
if( nrow(b3M)>1 ) {
  b3.s = apply(b3M,2,sd)
} else {
  b3.s = rep(0,length(b3.m))
}

lat2014opt.m = apply(lat2014optM,2,mean)
if( nrow(lat2014optM)>1 ) {
  lat2014opt.s = apply(lat2014optM,2,sd)
} else {
  lat2014opt.s = rep(0,length(lat2014optM.m))
}

# lat2014rnd.m = apply(lat2014rndM,2,mean)
# if( nrow(lat2014rndM)>1 ) {
#   lat2014rnd.s = apply(lat2014rndM,2,sd)
# } else {
#   lat2014rnd.s = rep(0,length(lat2014rndM.m))
# }

# curr.mar = par("mar")
# par( mar=c(4.1,4.6,2.1,1.1) )
# 
# plot( b1.m, type="l", ylim=c(0,Y.MAX), main=paste(ifelse(is.2D.case,"2 arms","20 arms"),"-",ifelse(display.const.budget,"Constant budget","changing budget")),
#       cex.axis=1.5, cex.lab=1.5, xlab="time-step t", ylab=expression(paste("Cumulative Reward (",log[10],")" )) )
# polygon( c(1:length(b1.m),length(b1.m):1), c(b1.m+b1.s,rev(b1.m-b1.s)),
#          col=adjustcolor("green3",alpha.f=0.2), border=NA )
# polygon( c(1:length(b2.m),length(b2.m):1), c(b2.m+b2.s,rev(b2.m-b2.s)),
#          col=adjustcolor("green3",alpha.f=0.2), border=NA )
# polygon( c(1:length(b3.m),length(b3.m):1), c(b3.m+b3.s,rev(b3.m-b3.s)),
#          col=adjustcolor("red",alpha.f=0.2), border=NA )
# polygon( c(1:length(lat2014opt.m),length(lat2014opt.m):1), c(lat2014opt.m+lat2014opt.s,rev(lat2014opt.m-lat2014opt.s)),
#          col=adjustcolor("purple",alpha.f=0.2), border=NA )
# # polygon( c(1:length(lat2014rnd.m),length(lat2014rnd.m):1), c(lat2014rnd.m+lat2014rnd.s,rev(lat2014rnd.m-lat2014rnd.s)),
# #          col=adjustcolor("black",alpha.f=0.2), border=NA )
# lines( b1.m, col="green3",lwd=4 )
# lines( b2.m, col="green3",lwd=4 )
# lines( b3.m, col="red",lwd=4 )
# lines( lat2014opt.m, col="purple",lwd=4 )
# # lines( lat2014rnd.m, col="black",lwd=4 )
# # legend("bottomright",legend=c("BORA1","BORA2","BORA3","Lattimore et al.2014","Lattimore et al. 2014 (rand. init.)"),lwd=3,
# #        col=c("green3","green3","red","purple","black"), cex=1.5)
# legend("bottomright",legend=c("BORA1","BORA2","BORA3","SBF"),lwd=3,
#        col=c("green3","green3","red","purple"), cex=1.5)
# 
# lines( log10(seq(1,length(b1.m),by=1)*(ncol(b1)-6)), lty=2, lwd=3, col="grey" )
# legend("topleft",legend="utopian",col="grey",lwd=2,lty=2,cex=1.5)
# 
# par( mar=curr.mar )
# 
# 
# 
# 
# invisible(readline("Press [RETURN] for zooming on BORA algorithms..."))
# 
# t0 = as.numeric(readline("> Set t0:"))
# tf = as.numeric(readline("> Set tf:"))
# 
# curr.mar = par("mar")
# par( mar=c(4.1,4.6,2.1,1.1) )
# 
# Y.MAX = max((b1.m+b1.s)[t0:tf],(b2.m+b2.s)[t0:tf],(b2.m+b2.s)[t0:tf])
# Y.MIN = min((b1.m-b1.s)[t0:tf],(b2.m-b2.s)[t0:tf],(b2.m-b2.s)[t0:tf])
# plot( t0:tf, b1.m[t0:tf], type="l", ylim=c(Y.MIN,Y.MAX), main=paste(ifelse(is.2D.case,"2 arms","20 arms"),"-",ifelse(display.const.budget,"Constant budget","changing budget")),
#       cex.axis=1.5, cex.lab=1.5, xlab="time-step t", ylab=expression(paste("Cumulative Reward (",log[10],")" )) )
# polygon( c(t0:tf,tf:t0), c( (b1.m+b1.s)[t0:tf],rev((b1.m-b1.s)[t0:tf])),
#          col=adjustcolor("green3",alpha.f=0.2), border=NA )
# polygon( c(t0:tf,tf:t0), c( (b2.m+b2.s)[t0:tf],rev((b2.m-b2.s)[t0:tf])),
#          col=adjustcolor("green3",alpha.f=0.2), border=NA )
# polygon( c(t0:tf,tf:t0), c( (b3.m+b3.s)[t0:tf],rev((b3.m-b3.s)[t0:tf])),
#          col=adjustcolor("red",alpha.f=0.2), border=NA )
# lines( t0:tf, b1.m[t0:tf], col="green3",lwd=4 )
# lines( t0:tf, b2.m[t0:tf], col="green3",lwd=4 )
# lines( t0:tf, b3.m[t0:tf], col="red",lwd=4 )
# 
# legend("bottomright",legend=c("BORA1","BORA2","BORA3"),lwd=3,
#        col=c("green3","green3","red"), cex=1.5)
# 
# par( mar=curr.mar )


cat("BORA1's Averge Cumulative Reward at the end:",round(10^b1.m[length(b1.m)],2),"\n")
cat("BORA2's Averge Cumulative Reward at the end:",round(10^b2.m[length(b2.m)],2),"\n")
cat("BORA3's Averge Cumulative Reward at the end:",round(10^b3.m[length(b3.m)],2),"\n")


# boxplot(apply(b1M,2,sum),apply(b2M,2,sum),apply(b3M,2,sum))
# plot(density(apply(b1M,2,sum)),col="green3", ylim=c(0,0.3), lwd=3 )
# lines(density(apply(b2M,2,sum)),col="green3", lwd=3)
# lines(density(apply(b3M,2,sum)),col="red", lwd=3)





# for NUMTA 2023

curr.mar = par("mar")
par( mar=c(4.1,4.6,2.1,1.1) )

plot( b1.m, type="l", ylim=c(0,Y.MAX), main=paste(ifelse(is.2D.case,"2 arms","20 arms"),"-",ifelse(display.const.budget,"Constant budget","changing budget")),
      cex.axis=1.75, cex.lab=1.75, xlab="time-step t", ylab=expression(paste("Cumulative Reward (",log[10],")" )) )
polygon( c(1:length(b1.m),length(b1.m):1), c(b1.m+b1.s,rev(b1.m-b1.s)),
         col=adjustcolor("green3",alpha.f=0.2), border=NA )
polygon( c(1:length(b2.m),length(b2.m):1), c(b2.m+b2.s,rev(b2.m-b2.s)),
         col=adjustcolor("red",alpha.f=0.2), border=NA )
polygon( c(1:length(lat2014opt.m),length(lat2014opt.m):1), c(lat2014opt.m+lat2014opt.s,rev(lat2014opt.m-lat2014opt.s)),
         col=adjustcolor("purple",alpha.f=0.2), border=NA )

lines( b1.m, col="green3",lwd=4 )
lines( b2.m, col="red",lwd=4 )
lines( lat2014opt.m, col="purple",lwd=4 )
# lines( lat2014rnd.m, col="black",lwd=4 )
legend("bottomright",legend=c("CBO","BORA","SBF"),lwd=4,
       col=c("green3","red","purple"), cex=1.75)

lines( log10(seq(1,length(b1.m),by=1)*(ncol(b1)-6)), lty=2, lwd=4, col="grey" )
legend("topleft",legend="utopian",col="grey",lwd=4,lty=2,cex=1.75)

par( mar=curr.mar )