rm(list=ls()); graphics.off(); cat("\014")

source("test_problems.R")
source("sbf_algos.R")

library(DiceKriging)
library(plot3D)


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
seed = 5
stochastic = T
constant.budget = F

lattimore2014 = T
optimal.nu.lo.init = T

set.seed(seed)

# *********************************************************************
# MAIN
# *********************************************************************

if( !constant.budget) {
  bs = round( runif(t.max) * (max.budget-min.budget) + min.budget, 2)
} else {
  bs = rep(round( runif(1) * (max.budget-min.budget) + min.budget, 2), t.max)
}

X = y = NULL

if( lattimore2014 ){
  cat(" - (Lattimore et al. 2014)\n")
  if( optimal.nu.lo.init ) {
    # nu.lo = c(0.1,0.9)
    nu.lo = rep(NA,length(nu))
    t = 1
    cat("> Optimal initialization...\n")
    while( length(which(is.na(nu.lo)))>0 ) {
      alloc = rep(2^-t,length(nu.lo))
      obs = f(alloc*bs[t],nu) # We must use the assumption of a constant budget because t --> Inf
      ixs = which(obs$out==0)
      nu.lo[which(is.na(nu.lo[ixs]))] = 2^-t
      t = t+1
    }
    cat("...",t-1,"evaluations to initialize...\n")
  } else {
    cat("> Random initialization...\n")
    nu.lo = rep(1/(0.5*length(nu)*(length(nu)+1)),length(nu))
    # nu.lo[1] = round(1-sum(nu.lo[-1]),4)
  }
  
  cat("...Done!\n")
  res = sbf.lattimore2014( f=f, T.max=t.max, m=length(nu), budgets=bs,
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
  if( !dir.exists("results_case1") )
    dir.create("results_case1")
  saveRDS( results, paste0("results_case1/",fileName,"_stoch_",stochastic,"_",seed,"_constbgt_",constant.budget,"_arms_",length(nu),".RDS") )
}
