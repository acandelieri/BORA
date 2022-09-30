library(DiceKriging)
library(nloptr)
library(transport)
library(purrr)
library(doParallel)
library(foreach)


#**********************************************************************************************************
#* Internal functions
#**********************************************************************************************************
# internal function: GP-CB for BORA1 and BORA2 algorithms
gpcb1_2 <- function( x, gp, is.UCB, beta_ ) {
  
  stopifnot( is.vector(x) )
  
  gp.y = predict( gp, data.frame(t(x)), "UK", checkNames=F )
  if( is.UCB ) {
    res = gp.y$mean + sqrt(beta_)*gp.y$sd
    # we have to change sign because slsqp performs minimization!
    res = - res 
  } else {
    res = gp.y$mean - sqrt(beta_)*gp.y$sd
  }
  
  return( res )
}

# internal function: GP-CB for BORA3 algorithm
gpcb3 <- function( x, X, y, sig, rho, nugget, is.UCB, beta_ ) {
  
  stopifnot( is.vector(x) )
  
  k.vec = numeric(nrow(X))
  K = computeKwse( X, sig, rho )
  K = K + diag(nugget,nrow(K))
  K.inv = solve.default(K)
  for( i in 1:nrow(X) ) 
    k.vec[i] = wsek(x,X[i,],sig,rho)
  
  y.mean = t(k.vec) %*% K.inv %*% y
  y.sd = wsek(x,x,sig,rho) - t(k.vec) %*% K.inv %*% k.vec
  
  if( is.UCB ) {
    res = y.mean + sqrt(beta_)*y.sd
    # we have to change sign because slsqp performs minimization!
    res = - res 
  } else {
    res = y.mean - sqrt(beta_)*y.sd
  }
  
  return( res )
  
}

# internal function: constraint for BORA_1 algorithm
bora1.constraint <- function( x ) {
  
  stopifnot( is.vector(x) )
  
  return( sum(x)-globalpass.r.budget )
}

# internal function: constraint for BORA_2 and BORA_3 algorithms
bora2_3.constraint <- function( x ) {
  
  stopifnot( is.vector(x) )
  
  return( sum(x)-1 )
}

# internal function: Wasserstein Squared Exponential kernel
wsek = function( x, y, sig, rho ) {
  x = round(x,2)
  y = round(y,2)
  w = wasserstein(x,y,p=1,costm=1-diag(length(x)))
  return( (sig^2) * exp(-0.5*(w/rho))^2 )
}

# internal function: computing the kernel matrix for the Wasserstein Squared Exponential kernel
computeKwse = function( X, sig, rho ) {
  stopifnot(is.matrix(X))
  K = matrix(NA,nrow(X),nrow(X))
  for( i in 1:nrow(X) ) {
    for( j in i:nrow(X) ) {
      K[i,j] = wsek( X[i,], X[j,], sig, rho ) 
      if( i != j ) {
        K[j,i] = K[i,j]
      }
    }
  }
  return(K)
} 

# internal function: computing the MLE for the WSE kernel
wse.mle = function( par, X, y ) {
  
  sig = par[1]
  rho = par[2]
  
  nugget = 0
  
  K = computeKwse(X,sig,rho)
  K.inv = NULL
  
  while( is.null(K.inv) && nugget<10^8 ) {
    
    try( expr=(K.inv = solve.default(K+diag(nugget^2,nrow(X)))), silent=T )
    
    if( is.null(K.inv) ) {
      if( nugget==0 ) { nugget = 10^-8 } else { nugget = nugget * sqrt(10) }
    } else {
      term1 = -0.5 * t(y) %*% K.inv %*% y
      term2 = -0.5 * log(det(K))
      term3 = -0.5 * nrow(X)* log(2*pi)
      if( is.nan(term2) | is.infinite(term2) ) {
        K.inv = NULL
        if( nugget==0 ) { nugget = 10^-8 } else { nugget = nugget * sqrt(10) }
      }
    }
  }
  
  if( is.null(K.inv) ) {
    res = -Inf
  } else {
    res = term1+term2+term3
  }

  return( list(res=res, nugget=nugget) )
}

# internal function: tuning WSE kernel hyperparams via focus search
focus.search.mle <- function( X, y, par.search.space, Np=20*nrow(search.space), tau=0.75,
                              max.iter=100, early.stop=5 ) {
  
  stopifnot( is.data.frame(par.search.space) & nrow(par.search.space)==2 )
  
  w = par.search.space$upper-par.search.space$lower
  par = (par.search.space$lower + par.search.space$upper)/2
  
  PARS = VALUES = NUGS = NULL
  iters = no.improv = 0
  curr.max = -Inf
  
  while( iters<max.iter && no.improv < early.stop ) {
    if( iters %% 10 == 0 ) 
      cat(".")
    pars = lhs::maximinLHS( Np, length(par) )
    pars[,1] = pars[,1] * w[1] + par[1]-w[1]/2
    pars[,2] = pars[,2] * w[2] + par[2]-w[2]/2
    
    # ****************************************************************************************************
    # Serial implementation
    # ****************************************************************************************************
    # # if( iters==1 )
    # #   start.at = Sys.time()
    # values = apply( pars, 1, function(x) { wse.mle(x,X=X,y=y)} )
    # # if( iters==1 )
    # #   cat("|X| =",nrow(X),"has required",difftime(Sys.time(),start.at,units="secs"),"[secs]\n")
    # values = matrix(unlist(values),2)
    # ****************************************************************************************************
    
    
    # ****************************************************************************************************
    # Parallel implementation
    # ****************************************************************************************************
    # start.at = Sys.time()
    values = foreach( i = 1:nrow(pars), .combine=cbind ) %dopar% {
      source("core.R")
      wse.mle( par=pars[i,], X=X, y=y )
    }
    # cat("*** MLE computed in ",difftime(Sys.time(),start.at,units="secs"),"[secs]\n")
    values = matrix(as.numeric(values),2)
    # ****************************************************************************************************
    
    
    PARS = rbind( PARS, pars )
    VALUES = c( VALUES, values[1,] )
    NUGS = c( NUGS, values[2,] )
    ix = which.max(VALUES)
    par = PARS[ix,]
    
    if( is.infinite(VALUES[ix]) && VALUES[ix]>0 )
      stop("ERRORE GRAVE!")
    
    
    # remove useless points
    ixs = which( PARS[,1]<par[1]-w[1]/2 | PARS[,1]>par[1]+w[1]/2 |
                   PARS[,2]<par[2]-w[2]/2 | PARS[,2]>par[2]+w[2]/2 )
    PARS = PARS[-ixs,]
    VALUES = VALUES[-ixs]
    NUGS = NUGS[-ixs]
    
    
    if( max(VALUES)>curr.max ) {
      curr.max = max(VALUES)
      no.improv = 0
    } else {
      no.improv = no.improv+1
    }
    
    w = tau * w
    iters = iters + 1
  }
  cat("\n")
  return( list(par=c(par, NUGS[which.max(VALUES)]), value=max(VALUES) ) )
}



#**********************************************************************************************************
#* Main functions
#**********************************************************************************************************


# BORA_1 algorithm
bora1 <- function( X, y, r.budget, search.space, maximize=T, beta_=1, covtype="gauss", nugget.estim=F ) {
  
  stopifnot( is.data.frame(X) & is.vector(y) & nrow(X)==length(y) &
               is.data.frame(search.space) & nrow(search.space)==ncol(X) )
  
  # fitting the GP on the complete search space
  gp = km( design=X, response=y, covtype=covtype, nugget.estim=nugget.estim,
           control=list(trace=0) )
  
  # optimizing the acquisition function constrained to the budget
  # be careful! slsqp minimizes fn!!!!
  x0 = round(runif(1,search.space$lower[1],min(search.space$upper[1],r.budget)),2)
  x0 = round(c(x0,min(search.space$upper[2],r.budget-x0)),2)
  globalpass.r.budget <<- r.budget
  res = slsqp( x0=x0, fn=gpcb, heq=cbo.constraint, lower=search.space$lower, upper=search.space$upper,
               gp=gp, is.UCB=maximize, beta_=beta_ )
  
  return( list( res=res, gp=gp ) )
}


# BORA_2 algorithm
bora2 <- function( A, y, maximize=T, beta_=1, covtype="gauss", nugget.estim=F ) {
  
  stopifnot( is.data.frame(A) & is.vector(y) & nrow(A)==length(y) &
               is.data.frame(search.space) & nrow(search.space)==ncol(X) )
  
  # fitting the GP on the complete search space
  gp = km( design=A, response=y, covtype=covtype, nugget.estim=nugget.estim,
           control=list(trace=0) )
  
  # optimizing the acquisition function over the simplex
  # be careful! slsqp minimizes fn!!!!
  a0 = round(runif(1),2)
  a0 = round(c(a0,min(1,1-a0)),2)
  res = slsqp( x0=a0, fn=gpcb, heq=bora.constraint, lower=rep(0,length(a0)), upper=rep(1,length(a0)),
               gp=gp, is.UCB=maximize, beta_=beta_ )
  
  return( list( res=res, gp=gp ) )
}


# BORA_3 algorithm
bora3 <- function( A, y, maximize=T, beta_=1, covtype="gauss", nugget.estim=F, n.proc=detectCores()-1 ) {
  
  stopifnot( is.matrix(A) & is.vector(y) & nrow(A)==length(y) &
               is.data.frame(search.space) & nrow(search.space)==ncol(X) )
  
  # fitting the GP with WSE kernel on the complete search space
  ss = data.frame(lower=10^c(-4,-4),upper=10^c(4,4), rownames=c("sigma","rho") )
  start.at=Sys.time()
  fs.res = focus.search.mle( X=A, y=y, par.search.space=ss, Np=20, tau=0.75, max.iter=100, early.stop=10 )
  cat("*** Focus-search elapsed time:",difftime(Sys.time(),start.at,units="secs"),"[secs]\n")
  
  # optimizing the acquisition function over the simplex
  # be careful! slsqp minimizes fn!!!!
  a0 = round(runif(1),2)
  a0 = round(c(a0,min(1,1-a0)),2)
  res = slsqp( x0=a0, fn=gpcb3, heq=bora2_3.constraint, lower=rep(0,length(a0)), upper=rep(1,length(a0)),
               X=A, y=y, sig=fs.res$par[1], rho=fs.res$par[2], nugget=fs.res$par[3],
               is.UCB=maximize, beta_=beta_ )
  
  return( list( res=res, kernel.params=list(sig=fs.res$par[1],rho=fs.res$par[2],nugget=fs.res$par[3]) ) )
}