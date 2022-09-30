rm(list=ls()); graphics.off(); cat("\014")


lim.T = Inf

files = list.files("results_case1/")
#files = files[-grep("LATTIMORE",files,fixed=T)]

df = NULL
for( f in files ) {
  cat("> Loading file: '",f,"'...\n",sep="")
  str = unlist(strsplit( gsub(".RDS","",f), "_", fixed=T))
  algo = str[1]
  is.stochastic = as.logical(str[3])
  seed = as.integer(str[4])
  is.constant.budget = as.logical(str[6])
  ds = readRDS( paste0("results_case1/",f ) )
  if( algo=="BORA3" ) {
    ds = ds[,-c(3:4)]
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
b1 = df[which(df$algo=="BORA1" & df$standard.output==T & df$constant.budget==T & df$iter<=lim.T), ]
b2 = df[which(df$algo=="BORA2" & df$standard.output==T & df$constant.budget==T & df$iter<=lim.T), ]
b3 = df[which(df$algo=="BORA3" & df$standard.output==T & df$constant.budget==T & df$iter<=lim.T), ]
lat2014 = df[which(df$algo=="LATTIMORE2014" & df$standard.output==T & df$constant.budget==T & df$iter<=lim.T), ]

Y.MAX = 2.5
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
lat2014M = NULL
for( s in unique(lat2014$seed) ) {
  lines( log10(cumsum(lat2014$y[which(lat2014$seed==s)])), col="red", lwd=2 )
  lat2014M = rbind(lat2014M,log10(cumsum(lat2014$y[which(lat2014$seed==s)])))
}
legend("bottomright",legend=c("BORA1","BORA2","BORA3","Lattimore et al. 2014"),lwd=2,
       col=c("blue","green3","red","purple"))

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

lat2014.m = apply(lat2014M,2,mean)
if( nrow(lat2014M)>1 ) {
  lat2014.s = apply(lat2014M,2,sd)
} else {
  lat2014.s = rep(0,length(lat2014M.m))
}

curr.mar = par("mar")
par( mar=c(4.1,4.6,1.1,1.1) )

plot( b1.m, type="l", ylim=c(0,max(b1.m+b1.s,b2.m+b2.s,b3.m+b3.s,log10(length(b1.m)*(ncol(b1)-6)))),
      cex.axis=1.5, cex.lab=1.5, xlab="time-step t", ylab=expression(log[10](R[t])))
polygon( c(1:length(b1.m),length(b1.m):1), c(b1.m+b1.s,rev(b1.m-b1.s)),
         col=adjustcolor("blue",alpha.f=0.2), border=NA )
polygon( c(1:length(b2.m),length(b2.m):1), c(b2.m+b2.s,rev(b2.m-b2.s)),
         col=adjustcolor("green3",alpha.f=0.2), border=NA )
polygon( c(1:length(b3.m),length(b3.m):1), c(b3.m+b3.s,rev(b3.m-b3.s)),
         col=adjustcolor("red",alpha.f=0.2), border=NA )
polygon( c(1:length(lat2014.m),length(lat2014.m):1), c(lat2014.m+lat2014.s,rev(lat2014.m-lat2014.s)),
         col=adjustcolor("purple",alpha.f=0.2), border=NA )
lines( b1.m, col="blue",lwd=4 )
lines( b2.m, col="green3",lwd=4 )
lines( b3.m, col="red",lwd=4 )
lines( lat2014.m, col="purple",lwd=4 )
legend( "bottomright", legend=c("BORA_1","BORA_2","BORA_3","Lattimore et al. 2014"), lwd=3, 
        col=c("blue","green2","red","purple"), cex=1.5 )

par( mar=curr.mar )

tmp1 = aggregate(b1$y,by=list(b1$seed),sum); colnames(tmp1)=c("seed","cumulative.reward")
tmp2 = aggregate(b2$y,by=list(b2$seed),sum); colnames(tmp2)=c("seed","cumulative.reward")
tmp3 = aggregate(b3$y,by=list(b3$seed),sum); colnames(tmp3)=c("seed","cumulative.reward")
cat("[Winnings count @ t =",lim.T,"]\n")
cat("BORA_1 winning:",
    length(which(tmp1$cumulative.reward>tmp2$cumulative.reward & 
             tmp1$cumulative.reward>tmp3$cumulative.reward ) ), "\n" )
cat("BORA_2 winning:",
    length(which(tmp2$cumulative.reward>tmp1$cumulative.reward & 
                   tmp2$cumulative.reward>tmp3$cumulative.reward ) ), "\n" )
cat("BORA_3 winning:",
    length(which(tmp3$cumulative.reward>tmp1$cumulative.reward & 
                   tmp3$cumulative.reward>tmp2$cumulative.reward ) ), "\n" )

lines( log10(seq(1,length(b1.m),by=1)*(ncol(b1)-6)), lty=2, lwd=3, col="grey" )
legend("topleft",legend="utopic",col="grey",lwd=2,lty=2,cex=1.5)
# ----


# changing budget: BORA1 vs BORA2 --------------------------------------------------------
b1 = df[which(df$algo=="BORA1" & df$standard.output==T & df$constant.budget==F & df$iter<=lim.T), ]
b2 = df[which(df$algo=="BORA2" & df$standard.output==T & df$constant.budget==F & df$iter<=lim.T), ]
b3 = df[which(df$algo=="BORA3" & df$standard.output==T & df$constant.budget==F & df$iter<=lim.T), ]
lat2014 = df[which(df$algo=="LATTIMORE2014" & df$standard.output==T & df$constant.budget==F & df$iter<=lim.T), ]

Y.MAX = 2.5
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
lat2014M = NULL
for( s in unique(lat2014$seed) ) {
  lines( log10(cumsum(lat2014$y[which(lat2014$seed==s)])), col="red", lwd=2 )
  lat2014M = rbind(lat2014M,log10(cumsum(lat2014$y[which(lat2014$seed==s)])))
}
legend("bottomright",legend=c("BORA1","BORA2","BORA3","Lattimore et al. 2014"),lwd=2,
       col=c("blue","green3","red","purple"))

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

lat2014.m = apply(lat2014M,2,mean)
if( nrow(lat2014M)>1 ) {
  lat2014.s = apply(lat2014M,2,sd)
} else {
  lat2014.s = rep(0,length(lat2014M.m))
}

curr.mar = par("mar")
par( mar=c(4.1,4.6,1.1,1.1) )

plot( b1.m, type="l", ylim=c(0,max(b1.m+b1.s,b2.m+b2.s,b3.m+b3.s,log10(length(b1.m)*(ncol(b1)-6)))),
      cex.axis=1.5, cex.lab=1.5, xlab="time-step t", ylab=expression(log[10](R[t])))
polygon( c(1:length(b1.m),length(b1.m):1), c(b1.m+b1.s,rev(b1.m-b1.s)),
         col=adjustcolor("blue",alpha.f=0.2), border=NA )
polygon( c(1:length(b2.m),length(b2.m):1), c(b2.m+b2.s,rev(b2.m-b2.s)),
         col=adjustcolor("green3",alpha.f=0.2), border=NA )
polygon( c(1:length(b3.m),length(b3.m):1), c(b3.m+b3.s,rev(b3.m-b3.s)),
         col=adjustcolor("red",alpha.f=0.2), border=NA )
polygon( c(1:length(lat2014.m),length(lat2014.m):1), c(lat2014.m+lat2014.s,rev(lat2014.m-lat2014.s)),
         col=adjustcolor("purple",alpha.f=0.2), border=NA )
lines( b1.m, col="blue",lwd=4 )
lines( b2.m, col="green3",lwd=4 )
lines( b3.m, col="red",lwd=4 )
lines( lat2014.m, col="purple",lwd=4 )
legend( "bottomright", legend=c("BORA_1","BORA_2","BORA_3","Lattimore et al. 2014"), lwd=3, 
        col=c("blue","green2","red","purple"), cex=1.5 )

par( mar=curr.mar )


tmp1 = aggregate(b1$y,by=list(b1$seed),sum); colnames(tmp1)=c("seed","cumulative.reward")
tmp2 = aggregate(b2$y,by=list(b2$seed),sum); colnames(tmp2)=c("seed","cumulative.reward")
tmp3 = aggregate(b3$y,by=list(b3$seed),sum); colnames(tmp3)=c("seed","cumulative.reward")
cat("[Winnings count @ t =",lim.T,"]\n")
cat("BORA_1 winning:",
    length(which(tmp1$cumulative.reward>tmp2$cumulative.reward & 
                   tmp1$cumulative.reward>tmp3$cumulative.reward ) ), "\n" )
cat("BORA_2 winning:",
    length(which(tmp2$cumulative.reward>tmp1$cumulative.reward & 
                   tmp2$cumulative.reward>tmp3$cumulative.reward ) ), "\n" )
cat("BORA_3 winning:",
    length(which(tmp3$cumulative.reward>tmp1$cumulative.reward & 
                   tmp3$cumulative.reward>tmp2$cumulative.reward ) ), "\n" )

lines( log10(seq(1,length(b1.m),by=1)*(ncol(b1)-6)), lty=2, lwd=3, col="grey" )
legend("topleft",legend="utopic",col="grey",lwd=2,lty=2,cex=1.5)

# ----
