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
nu = c(25,50)
# budget at step 't' is sampled uniformly in [min-budget; max.budget]
min.budget = 10; max.budget = 100
# search space: min-max allocable resource for ech arm
search.space = data.frame( lower=rep(0,2), upper=rep(max.budget,2) )
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

# TODO: extends to >2 dimensions!
if( nrow(search.space)!=2 ) {
  stop("NOT IMPLEMENTED FOR",nrow(search.space),"ARMS!")
}


if( lattimore2014 ){
  cat(" - (Lattimore et al. 2014)")
  res = sbf.lattimore2014( f=f, T.max=t.max, m=length(nu), budget.const=bs,
                           actual.nu=nu, nu.lo=c(0.1,0.9) )
} else {
  cat(" - (???????????)")
}

if( lattimore2014 ) {
  fileName = "LATTIMORE2014"
} else {
  fileName = "???????"
}
  
if( save.results ) {
  results = data.frame( x1=res$X[,1], x2=res$X[,2], y=apply(res$OUT,1,sum) )
  if( !dir.exists("results_case1") )
    dir.create("results_case1")
  saveRDS( results, paste0("results_case1/",fileName,"_stoch_",stochastic,"_",seed,"_constbgt_",constant.budget,".RDS") )
}
