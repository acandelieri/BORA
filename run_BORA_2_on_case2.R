rm(list=ls()); graphics.off(); cat("\014")

source("test_problems.R")
source("core.R")


# *********************************************************************
# Case study specific parameters 
# *********************************************************************

# test case function and # of arms
f = case2
n.arms = 15

# reward function's params
ni.mu = round(runif(n.arms),2)
ni.sd = round(runif(n.arms,0,0.2),2)

# budget at step 't' is sampled from a normal distribution with mean=50 and sd=10
budget.mu = 50; budget.sd = 10

# search space: min-max allocable resource for each arm
max.budget = budget.mu+3*budget.sd
search.space = data.frame( lower=rep(0,n.arms), upper=rep(max.budget,n.arms) )

# horizon
t.max = 100
# *********************************************************************

# *********************************************************************
# other technical parameters
# *********************************************************************

save.results = T
seed = 5
constant.budget = F

set.seed(seed)

# *********************************************************************
# MAIN
# *********************************************************************

if( !constant.budget ) {
  bs = numeric(t.max)
  for( i in 1:length(bs) )
    bs[i] = round( rnorm(1,budget.mu,budget.sd), 2)
} else {
  bs = rep( round( rnorm(1,budget.mu,budget.sd), 2), t.max)
}

X = A = y = NULL


for( t in 1:t.max ) {
  cat("> t:",t,"\n")
  cat(" - budget:",bs[t],"\n")
  if( t > n.arms+1 && sd(y)!=0 ) {
    cat(" - BORA2...\n")
    res = bora2( A=as.data.frame(A), y=y, maximize=T, beta_=1, covtype="gauss", nugget.estim=T )
    a.next = round(res$res$par,2)
  } else {
    cat(" - RS...\n")
    a.next = round(runif(n.arms),2)
    a.next = round( a.next/sum(a.next), 2)
    # ...and just to be sure:
    a.next[1] = 1 - sum(a.next[-1])
  }
  
  x.next = round(a.next * bs[t],2)
  
  cat(" - a.next =",a.next,"\n")
  A = rbind(A,a.next)
  cat(" - x.next =",x.next,"\n")
  X = rbind(X,x.next)
  y.next = f( x.next, ni.mu, ni.sd )

  y = c(y,y.next)
  cat(" - y.next =",y.next,"\n")
  
}

if( save.results ) {
  results = data.frame( X=X, A=A, y=y )
  if( !dir.exists("results_case2") )
    dir.create("results_case2")
  saveRDS( results, paste0("results_case2/BORA2_",seed,"_constbgt_",constant.budget,"_arms_",n.arms,".RDS") )
}