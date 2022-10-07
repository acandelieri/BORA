rm(list=ls()); graphics.off(); cat("\014")


lim.T = Inf
display.const.budget = F
use.shaded.area = T
use.bordered.area = T
stopifnot( use.shaded.area || use.bordered.area )


files = list.files("results_case2/",recursive=F)
files = files[grep("arms_15",files,fixed=T)]


df = NULL
for( f in files ) {
  cat("> Loading file: '",f,"'...\n",sep="")
  str = unlist(strsplit( gsub(".RDS","",f), "_", fixed=T))
  algo = str[1]
  seed = as.integer(str[2])
  is.constant.budget = as.logical(str[4])
  ds = readRDS( paste0("results_case2/",f ) )
  if( algo %in% c("BORA2","BORA3","BORA3rescaled") ) {
    N = (ncol(ds)-1)/2
    ds = ds[,-c((N+1):(2*N))]
    cat("!!!!! Warning: rimosse colonne relative alle misure (per conformita' con il dataframe)\n")
  }
  df = rbind( df,
              data.frame( algo=rep(algo,nrow(ds)),
                          seed=rep(seed,nrow(ds)),
                          constant.budget=rep(is.constant.budget,nrow(ds)),
                          iter=1:nrow(ds), ds ) )
}


# constant budget: BORA1 vs BORA2 --------------------------------------------------------
b1 = df[which(df$algo=="BORA1" & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
b2 = df[which(df$algo=="BORA2" & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
b3 = df[which(df$algo=="BORA3" & df$constant.budget==display.const.budget & df$iter<=lim.T), ]
# b3r = df[which(df$algo=="BORA3rescaled" & df$constant.budget==display.const.budget & df$iter<=lim.T), ]

Y.MAX = log10(5000)
plot( rep(1,max(df$iter)), col=NA, ylim=c(0,Y.MAX) )
b1M = NULL
for( s in unique(b1$seed) ) {
  lines( log10(cumsum(b1$y[which(b1$seed==s)])), col="blue", lwd=2 )
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
# b3rM = NULL
# for( s in unique(b3r$seed) ) {
#   lines( log10(cumsum(b3r$y[which(b3r$seed==s)])), col="orange", lwd=2 )
#   b3rM = rbind(b3rM,log10(cumsum(b3r$y[which(b3r$seed==s)])))
# }
legend("bottomright",legend=c("BORA1","BORA2","BORA3"),lwd=2,
       col=c("blue","green3","red"))

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

# b3r.m = apply(b3rM,2,mean)
# if( nrow(b3rM)>1 ) {
#   b3r.s = apply(b3rM,2,sd)
# } else {
#   b3r.s = rep(0,length(b3r.m))
# }


curr.mar = par("mar")
par( mar=c(4.1,4.6,2.1,1.1) )

plot( b1.m, type="l", ylim=c(0,Y.MAX), main=paste("15 arms","-",ifelse(display.const.budget,"Constant budget","changing budget")),
      cex.axis=1.5, cex.lab=1.5, xlab="time-step t", ylab=expression(paste("Cumulative Reward (",log[10],")" )) )
if( use.shaded.area ) {
  polygon( c(1:length(b1.m),length(b1.m):1), c(b1.m+b1.s,rev(b1.m-b1.s)),
           col=adjustcolor("blue",alpha.f=0.2), border=NA )
  polygon( c(1:length(b2.m),length(b2.m):1), c(b2.m+b2.s,rev(b2.m-b2.s)),
           col=adjustcolor("green3",alpha.f=0.2), border=NA )
  polygon( c(1:length(b3.m),length(b3.m):1), c(b3.m+b3.s,rev(b3.m-b3.s)),
           col=adjustcolor("red",alpha.f=0.2), border=NA )
  # if( !is.constant.budget )
  #   polygon( c(1:length(b3r.m),length(b3r.m):1), c(b3r.m+b3r.s,rev(b3r.m-b3r.s)),
  #           col=adjustcolor("orange",alpha.f=0.2), border=NA )
}
if( use.bordered.area) {
  lines( b1.m+b1.s, col="blue" ); lines( b1.m-b1.s, col="blue" )
  lines( b2.m+b2.s, col="green3" ); lines( b2.m-b2.s, col="green3" )
  lines( b3.m+b3.s, col="red" ); lines( b3.m-b3.s, col="red" )
}

lines( b1.m, col="blue",lwd=4 )
lines( b2.m, col="green3",lwd=4 )
lines( b3.m, col="red",lwd=4 )
# if( !is.constant.budget )
#   lines( b3r.m, col="orange",lwd=4 )

# if( !is.constant.budget ) {
#   legend("bottomright",legend=c("BORA1","BORA2","BORA3","BORA3_rescaled"),lwd=3,
#          col=c("blue","green3","red","orange"), cex=1.5)
# } else {
  legend("bottomright",legend=c("BORA1","BORA2","BORA3"),lwd=3,
        col=c("blue","green3","red"), cex=1.5)
# }

par( mar=curr.mar )

cat("BORA1's Averge Cumulative Reward at the end:",round(10^b1.m[length(b1.m)],2),"\n")
cat("BORA2's Averge Cumulative Reward at the end:",round(10^b2.m[length(b2.m)],2),"\n")
cat("BORA3's Averge Cumulative Reward at the end:",round(10^b3.m[length(b3.m)],2),"\n")

