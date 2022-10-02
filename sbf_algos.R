library(purrr)

internal.function <- function( R, V2, delta ) {
  
  delta0 = delta / (3 * ((R+1)^2) * (V2+1)^2 )
  
  return( ((R+1)/3)*log(2/delta0) + 
            sqrt( 2*(V2+1)*log(2/delta0)+(((R+1)/3)^2)*(log(2/delta0))^2 ) ) 
}




sbf.lattimore2014 <- function( f, T.max, m, budgets, actual.nu, nu.lo=rep(1,length(m)) ) {
  
  # stopifnot( sum(budget.const-budget.const[1])==0 )
  
  delta = 1/(T.max*m)^2
  nu.up = rep(Inf,m)
  
  X = NULL
  OUT = NULL
  W = NULL
  
  for( t in 1:T.max ) {
    
    x = numeric(m)
    
    for( i in 1:m ) {
      ixs = which(x==0)
      ix = which.min(nu.lo[ixs])
      x[ixs[ix]] = min( nu.lo[ixs[ix]], 1-sum(x) )
    }
    
    y = f( round(x*budgets[t],2), actual.nu )
  
    w = 1 / (1-x/nu.up)
    
    X = rbind(X,x)
    OUT = rbind(OUT,y$out)
    W = rbind(W,w)
    
    v.inv_ = apply( W * OUT, 2, sum ) / apply( W * X, 2, sum )
    
    R = apply(W,2,max)
    
    V2 = apply( W * X, 2, sum ) / nu.lo
    
    eps = internal.function(R,V2,delta) / apply(W*X,2,sum)
    
    nu.lo.inv = apply( cbind(1/nu.lo, v.inv_+eps),1, min )
    nu.up.inv = apply( cbind(1/nu.up, v.inv_-eps),1, max )
    
    nu.lo = 1/nu.lo.inv
    nu.up = 1/nu.up.inv
    
  }
  
  A = X 
  for( i in 1:ncol(X) ) 
    X[,i] = round(X[,i] * budgets,2)
  return( list(X=X, A=A, OUT=OUT) )
  
}