rm(list=ls()); graphics.off(); cat("\014")

source("test_problems.R")
source("core.R")



# *********************************************************************
# Case study specific parameters 
# *********************************************************************

# test case function
f = case1
# reward function's params (i.e., length of 'nu' is the number of arms)
nu = c(1,2,3,2,1,5,3,12,2,5,10,2,3,4,5,4,3,2,1,5)
# budget at step 't' is sampled uniformly in [min-budget; max.budget]
min.budget = 10; max.budget = 100
# search space: min-max allocable resource for each arm
search.space = data.frame( lower=rep(0,length(nu)), upper=rep(max.budget,length(nu)) )
# horizon
t.max = 100
# *********************************************************************

# *********************************************************************
# other technical parameters
# *********************************************************************

save.results = T
seed = 30
stochastic = T
constant.budget = T

set.seed(seed)

# *********************************************************************
# MAIN
# *********************************************************************

elapsed <- Sys.time()

if( !constant.budget) {
  bs = round( runif(t.max) * (max.budget-min.budget) + min.budget, 2)
} else {
  bs = rep(round( runif(1) * (max.budget-min.budget) + min.budget, 2), t.max)
}

X = y = NULL


for( t in 1:t.max ) {
  
  cat("> t:",t,"\n")
  cat(" - budget:",bs[t],"\n")
  
  if( t > length(nu)+1 && sd(y)!=0 ) {
    cat(" - BORA1 (i.e.,CBO)...\n")
    res = bora1( X=as.data.frame(X), y=y, r.budget=bs[t], search.space=search.space,
               maximize=T, beta_=1, covtype="gauss", nugget.estim=T )
    x.next = round(res$res$par,2)
  } else {
    cat(" - RS...\n")
    x.next = round(runif(length(nu))*(search.space$upper-search.space$lower) + search.space$lower,2)
    x.next = round( x.next/sum(x.next) * bs[t], 2)
    # ...and just to be sure:
    x.next[1] = bs[t] - sum(x.next[-1])
  }
  
  cat(" - x.next =",x.next,"\n")
  X = rbind(X,x.next)
  y.next = f( x.next, nu )
  
  if( stochastic ) {
    y.next = sum(y.next$out)
  } else {
    y.next = sum(y.next$thr)
  }
  y = c(y,y.next)
  cat(" - y.next =",y.next,"\n")
  
}


cat("\n> Elapsed time:",difftime(Sys.time(),elapsed,units="secs"),"[secs]\n\n")

if( save.results ) {
  results = data.frame( X=X, y=y )
  if( !dir.exists("results_case1") )
    dir.create("results_case1")
  saveRDS( results, paste0("results_case1/BORA1_stoch_",stochastic,"_",seed,"_constbgt_",constant.budget,"_arms_",length(nu),".RDS") )
}
