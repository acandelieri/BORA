rm(list=ls()); graphics.off(); cat("\014")

source("test_problems.R")
source("sbf_algos.R")

library(DiceKriging)
library(plot3D)


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
seed = 1
constant.budget = T

set.seed(seed)

lattimore2014 = T
optimal.nu.lo.init = T

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

X = y = NULL

if( lattimore2014 ){
  cat(" - (Lattimore et al. 2014)\n")
  if( optimal.nu.lo.init ) {
    # nu.lo = c(0.1,0.9)
    nu.lo = rep(NA,n.arms)
    t = 1
    cat("> Optimal initialization...\n")
    while( length(which(is.na(nu.lo)))>0 ) {
      alloc = rep(2^-t,length(nu.lo))
      obs = f(alloc*bs[t],ni.mu,ni.sd) # We must use the assumption of a constant budget because t --> Inf
      ixs = which(obs==0)
      nu.lo[which(is.na(nu.lo[ixs]))] = 2^-t
      t = t+1
    }
    cat("...",t-1,"evaluations to initialize...\n")
  } else {
    cat("> Random initialization...\n")
    nu.lo = rep(1/(0.5*n.arms*(n.arms+1)),n.arms)
    # nu.lo[1] = round(1-sum(nu.lo[-1]),4)
  }
  
  cat("...Done!\n")
  res = sbf.lattimore2014( f=f, T.max=t.max, m=n.arms, budgets=bs,
                           actual.nu=nu, nu.lo=nu.lo )
} else {
  cat(" - (???????????)")
}

if( lattimore2014 ) {
  if( optimal.nu.lo.init ) {
    fileName = "LATTIMORE2014optinit"
  } else {
    fileName = "LATTIMORE2014rndinit"
  }
} else {
  fileName = "???????"
}
  
if( save.results ) {
  results = data.frame( X=res$X, A=res$A, y=apply(res$OUT,1,sum) )
  if( !dir.exists("results_case2") )
    dir.create("results_case2")
  saveRDS( results, paste0("results_case2/",fileName,"_",seed,"_constbgt_",constant.budget,"_arms_",n.arms,".RDS") )
}
