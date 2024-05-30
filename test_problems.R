case1 <- function(x.t, nu ) {
  stopifnot( is.vector(x.t) && length(x.t)==length(nu) )
  thr = apply(cbind(rep(1,length(nu)),x.t/nu),1,min )
  out = numeric(length(nu))
  for( i in 1:length(out) ) {
    out[i] = rbernoulli(1,thr[i])
  }
  out = as.numeric(out)
  return( list( out=out, thr=thr ) ) 
}

case2 <- function(x.t, ni.mu, ni.sd ) {
  stopifnot( is.vector(x.t) &&
               length(x.t)==length(ni.mu) &&
               length(x.t)==length(ni.sd) )
  
  r = numeric(length(x.t))
  for( i in 1:length(r) ) 
    r[i] = max(0,rnorm(1,ni.mu,ni.sd))
  
  terms = round(sum(x.t * r),2)
  
  return( list(value=round(sum(terms),2), terms=terms) ) 
}