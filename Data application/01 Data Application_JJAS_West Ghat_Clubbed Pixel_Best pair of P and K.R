library(fda)

rm(list = ls())

##=================================================================
#####Necessary functions:

#adaptive metropolis
mhupdate <- function(acc, att, mh, nattempts = 50, lower = 0.8, higher = 1.2){
  acc.rate     <- acc / att
  these.update <- att > nattempts
  these.low    <- (acc.rate < 0.30) & these.update
  these.high   <- (acc.rate > 0.50) & these.update
  
  mh[these.low]  <- mh[these.low] * lower
  mh[these.high] <- mh[these.high] * higher
  
  acc[these.update] <- 0
  att[these.update] <- 0
  
  results <- list(acc = acc, att = att, mh = mh)
  return(results)
}

#GE log-likelihood
ldens_ge <- function(Y,alpha,lambda.vec)
{
  out <- log(lambda.vec) + log(alpha) - lambda.vec * Y + (alpha-1) * log(1 - exp(-lambda.vec * Y))
  return(out)
}

#pdf of PC prior
ldens_PCprior <- function(al.f, th.f)
{
  out <- log(th.f / 2) - th.f * sqrt( 2*log(al.f) + 2 * (1-al.f) / al.f  )
  - 0.5 * log( 2*log(al.f) + 2 * (1-al.f) / al.f )
  + log(abs(1/al.f - 1/al.f^2))
  return(out)
}

#alpha update
alpha.update <- function(alpha, lambda.vec, Y, cur.ll, alpha.theta,
                         att.alpha, acc.alpha, mh.alpha){
  
  att.alpha <- att.alpha + 1
  
  alpha.star <- log(alpha)
  can.alpha.star <- rnorm(1, alpha.star, mh.alpha)
  can.alpha <- exp(can.alpha.star)
  
  can.ll <- sum(ldens_ge(Y, can.alpha, lambda.vec))
  
  # ratio <- can.ll - cur.ll +
  #   dgamma(can.alpha, alpha.theta, alpha.theta) -
  #   dgamma(alpha, alpha.theta, alpha.theta) +
  #   can.alpha.star - alpha.star
  ratio <- can.ll - cur.ll +
    ldens_PCprior(can.alpha, alpha.theta) -
    ldens_PCprior(alpha, alpha.theta) +
    can.alpha.star - alpha.star
  
  
  if(log(runif(1)) < ratio){
    alpha <- can.alpha
    cur.ll <- can.ll
    acc.alpha <- acc.alpha + 1}
  
  results <- list(alpha = alpha, cur.ll = cur.ll,
                  att.alpha = att.alpha, acc.alpha = acc.alpha)
  results}

#beta update
beta.update <- function(alpha, beta, Y, cur.ll, B,
                        beta.mean, beta.sd,
                        att.beta, acc.beta, mh.beta){
  
  att.beta <- att.beta + 1
  
  P <- length(beta)
  
  for(pp in 1:P){
    can.beta <- beta
    can.beta[pp] <- rnorm(1, beta[pp], mh.beta)
    can.lambda.vec <- exp(as.vector(B %*% can.beta))
    
    can.ll <- sum(ldens_ge(Y, alpha, can.lambda.vec))
    
    ratio <- can.ll - cur.ll +
      dnorm(can.beta[pp], beta.mean, beta.sd, log = T) - 
      dnorm(beta[pp], beta.mean, beta.sd, log = T)
    
    if(log(runif(1)) < ratio){
      beta[pp] <- can.beta[pp]
      cur.ll <- can.ll
      acc.beta[pp] <- acc.beta[pp] + 1}
  }
  
  results <- list(beta = beta, cur.ll = cur.ll,
                  att.beta = att.beta, acc.beta = acc.beta)
  results}

#MCMC
mcmc.GE <- function(Y, B, alpha.init = NULL, beta.init = NULL,
                    # priors
                    alpha.theta = 2.5,
                    beta.mean = 0, beta.sd = 10,
                    # mcmc settings
                    iters = 4000, burn = 2000, thin = 5){
  P <- ncol(B)
  
  if(is.null(alpha.init)){
    alpha <- 1
  }else{alpha <- alpha.init}
  
  if(is.null(beta.init)){
    beta <- rep(-log(mean(Y)), P)
  }else{beta <- beta.init}
  
  lambda.vec <- exp(as.vector(B %*% beta))
  
  cur.ll <- sum(ldens_ge(Y, alpha, lambda.vec))
  
  acc.alpha <- att.alpha <- mh.alpha <- 1
  acc.beta <- att.beta <- mh.beta <- rep(1, P)
  
  keepers.alpha <- rep(NA, iters)
  keepers.beta <- matrix(NA, iters, P)
  keepers.mi.vi <- matrix(NA, iters, length(Y))
  
  for(iter in 1:iters){
    for(ttt in 1:thin){
      alpha.update.details <- alpha.update(alpha, lambda.vec, Y, cur.ll, 
                                           alpha.theta,
                                           att.alpha, acc.alpha, mh.alpha)
      alpha <- alpha.update.details$alpha
      cur.ll <- alpha.update.details$cur.ll
      att.alpha <- alpha.update.details$att.alpha
      acc.alpha <- alpha.update.details$acc.alpha
      
      beta.update.details <- beta.update(alpha, beta, Y, cur.ll,B,
                                         beta.mean, beta.sd,
                                         att.beta, acc.beta, mh.beta)
      beta <- beta.update.details$beta
      cur.ll <- beta.update.details$cur.ll
      att.beta <- beta.update.details$att.beta
      acc.beta <- beta.update.details$acc.beta
      
      lambda.vec <- exp(as.vector(B %*% beta))
      
      if(iter < (burn / 2)){
        this.update <- mhupdate(acc = acc.alpha, att = att.alpha, mh = mh.alpha)
        acc.alpha <- this.update$acc
        att.alpha <- this.update$att
        mh.alpha <- this.update$mh
        
        this.update <- mhupdate(acc = acc.beta, att = att.beta, mh = mh.beta)
        acc.beta <- this.update$acc
        att.beta <- this.update$att
        mh.beta <- this.update$mh
      }
    }
    
    # storage
    keepers.alpha[iter] <- alpha
    keepers.beta[iter, ] <- beta
    keepers.mi.vi[iter, ] <- ldens_ge(Y, alpha, lambda.vec)
    
    #counting
    if (iter %% 10 == 0)
    {
      print(paste0(iter," iterations completed."))
    }
    
  }#end iters
  
  return.iters <- (burn + 1):iters
  
  results <- list(alpha = keepers.alpha[return.iters],
                  beta = keepers.beta[return.iters, ],
                  mi.vi = keepers.mi.vi[return.iters, ]
  )
  
  return(results)
}


#####==========================================================================================
#####==========================================================================================
#set the working directory
#load this .Rdata file into environment
load("00 Data_Store_Raw Cleaned Data.Rdata")

#------------------------------------
#function for evaluating WAIC

eval.WAIC <- function(alpha.theta, n.B.MCMC_fit, reg, mcmc.iters = 1e4, burn.in = 3e3)
{
  
  if (reg == 1){
    clubbed.loc.ind <- 1:5
  }else if (reg == 2){
    clubbed.loc.ind <- 6:14
  }else{
    clubbed.loc.ind <- 15:21
  }
  
  cur.data <- numeric()
  covariate <- numeric()
  
  for (t in 1:length(all.data))
  {
    if (t %% 4 == 0){
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.leap])
    }else{
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.nonleap])
    }
    
    cur.year.data.nz <- cur.year.data[!(cur.year.data == 0)]
    
    cur.data <- c(cur.data, cur.year.data.nz)
    covariate <- c(covariate, rep((1900 + t), length(cur.year.data.nz)))
  }
  
  ##-------------------------------
  
  #Setting the basis functions for MCMC run
  basis <-  create.bspline.basis(c(1901,2022), nbasis = n.B.MCMC_fit)
  B.fit <- eval.basis(covariate, basis)
  
  #MCMC chain
  fit.observed.GE.semipar <- mcmc.GE(cur.data, B.fit, 
                                     alpha.init = NULL, beta.init = NULL,
                                     # priors
                                     alpha.theta = alpha.theta,
                                     beta.mean = 0, beta.sd = 100,
                                     # mcmc settings
                                     iters = mcmc.iters, burn = burn.in, thin = 5)
  
  #-- -- -- -- -- -- -- -- 
  #calculating the WAIC
  mi <- colMeans(fit.observed.GE.semipar$mi.vi)
  vi <- apply(fit.observed.GE.semipar$mi.vi, 2, var)
  ret <- -2 * sum(mi) + 2 * sum(vi)
  
  return(ret)
  
}

#------------------------------------
#Running the function

wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30
JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)


WAIC.store <- list()
theta.vec <- seq(0.5,5,0.5)
P.vec <- seq(4,15,1)


#three loops here, one for region, one for theta, one for K; we can implement parallel here
for (i in  1:3)
{
  cur.reg.WAIC <- matrix(NA, nrow = length(theta.vec), ncol = length(P.vec))
  
  for (j in 1:length(theta.vec))
  {
    for (k in 1:length(P.vec))
    {
      cur.reg.WAIC[j,k] <- eval.WAIC(theta.vec[j], P.vec[k], i)
    }
  }
  
  WAIC.store[[i]] <- cur.reg.WAIC
}

#=======================================================================================

WAIC.northern  <- WAIC.store[[1]]
WAIC.middle <- WAIC.store[[2]]
WAIC.southern <- WAIC.store[[3]]

save(WAIC.northern, WAIC.middle, WAIC.southern, file = "02 Data_Store_WAIC_best.pair.Rdata")


