rm(list = ls())

library(Rmpfr)
library(splines)
library(MASS)
library(pcaPP)
library(rainbow)
library(RCurl)
library(fds)
library(deSolve)
library(fda)
library(parallel)
library(foreach)
library(iterators)
library(doParallel)

#####==========================================================================================
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
  if (sum(out) == Inf)
  {
    a_mpfr <- mpfr(lambda.vec * Y, precBits = 128)
    val <- as.numeric(log(1 - exp(-a_mpfr))) 
    out <- log(lambda.vec) + log(alpha) - lambda.vec * Y + (alpha-1) * val
  }
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

#alpha update for PC
alpha.update <- function(alpha, lambda.vec, Y, cur.ll, alpha.hyper,
                         att.alpha, acc.alpha, mh.alpha, prior){
  
  att.alpha <- att.alpha + 1
  
  alpha.star <- log(alpha)
  can.alpha.star <- rnorm(1, alpha.star, mh.alpha)
  can.alpha <- exp(can.alpha.star)
  
  can.ll <- sum(ldens_ge(Y, can.alpha, lambda.vec))
  
  #-- -- -- -- -- -- -- -- -- --
  #prior here
  if (prior == "PC"){
    ratio <- can.ll - cur.ll +
      ldens_PCprior(can.alpha, alpha.hyper) - 
      ldens_PCprior(alpha, alpha.hyper) +
      can.alpha.star - alpha.star
  }
  else{
    ratio <- can.ll - cur.ll +
      dgamma(can.alpha, alpha.hyper, alpha.hyper, log = T) - 
      dgamma(alpha, alpha.hyper, alpha.hyper, log = T) +
      can.alpha.star - alpha.star
  }
  #-- -- -- -- -- -- -- -- -- --
  
  if(log(runif(1)) < ratio){
    alpha <- can.alpha
    cur.ll <- can.ll
    acc.alpha <- acc.alpha + 1}
  
  results <- list(alpha = alpha, cur.ll = cur.ll,
                  att.alpha = att.alpha, acc.alpha = acc.alpha)
  results}


#updating beta
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
                    alpha.hyper, beta.mean = 0, beta.sd = 10,
                    # mcmc settings
                    iters = 5000, burn = 2500, thin = 5, prior){
  
  tick <- proc.time()[3]
  
  P <- ncol(B)
  
  #initializing initial values if not given by users
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
  keepers.deviance <- rep(NA, iters)
  keepers.mi.vi <- matrix(NA, iters, length(Y))
  
  for(iter in 1:iters)
  {
    for(ttt in 1:thin)
    {
      #alpha update
      alpha.update.details <- alpha.update(alpha, lambda.vec, Y, cur.ll, 
                                           alpha.hyper,
                                           att.alpha, acc.alpha, mh.alpha, prior)
      alpha <- alpha.update.details$alpha
      cur.ll <- alpha.update.details$cur.ll
      att.alpha <- alpha.update.details$att.alpha
      acc.alpha <- alpha.update.details$acc.alpha
      
      #beta update
      beta.update.details <- beta.update(alpha, beta, Y, cur.ll, B, 
                                         beta.mean, beta.sd,
                                         att.beta, acc.beta, mh.beta)
      beta <- beta.update.details$beta
      cur.ll <- beta.update.details$cur.ll
      att.beta <- beta.update.details$att.beta
      acc.beta <- beta.update.details$acc.beta
      
      lambda.vec <- exp(as.vector(B %*% beta))             #needed for alpha update
      
      #adapting until half burn-in period
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
    }#end ttt
    
    #storage
    keepers.alpha[iter] <- alpha
    keepers.beta[iter, ] <- beta
    keepers.deviance[iter] <- -2 * cur.ll
    keepers.mi.vi[iter, ] <- ldens_ge(Y, alpha, lambda.vec)
    
    if (iter %% 1000 == 0)
    {
      print(paste0(iter," iterations completed."))
    }
  }#end iters
  
  tock <- proc.time()[3]
  return.iters <- (burn + 1):iters
  
  results <- list(alpha = keepers.alpha[return.iters],
                  beta = keepers.beta[return.iters, ],
                  deviance = keepers.deviance[return.iters],
                  mi.vi = keepers.mi.vi[return.iters, ],
                  minutes = (tock - tick)
  )
  
  return(results)
}

#####==========================================================================================
####the main function

sim_all_tog <- function(n, alpha.true, alpha.hyper, model.true, model.fit, prior)
{
  if (!(model.true %in% c("Linear", "Non_Linear"))){
    stop("True Model specification discripancy.")
  }
  
  if (!(model.fit %in% c("Par", "Semi_Par"))){
    stop("Fitting model specification discrapancy.")
  }
  
  if (!(prior %in% c("Non_PC", "PC"))){
    stop("Prior specification discripancy.")
  }
  
  if((prior == "PC") & (alpha.hyper %in% c(0.01, 0.1, 1) ) ){
    stop("hyperparamter specification discrepancy.")
  }
  if((prior == "Non_PC") & (alpha.hyper %in% c(2.5, 5, 10) ) ){
    stop("hyperparamter specification discrepancy.")
  }
  
  beta.true.linear <- c(-5, 5, 3)
  beta.true.nonlinear <- c(-5, 2, 3)
  coverage.rep <- 1e3
  mcmc.iters <- 5e3
  
  X <- seq((1 / (n+1)), (1 - 1 / (n+1)), length.out = n)
  set.seed(100)
  ran.X <- runif(n, 0, 0.5)
  
  #model.true modification
  if (model.true == "Linear"){
    B.true <- matrix(c(rep(1, n), X, ran.X), ncol = 3)
    f.val.true <- as.vector(B.true %*% beta.true.linear)
  }else{
    B.true <- matrix(c(rep(1, n), X^2, sin(2*pi*ran.X)), ncol = 3)
    f.val.true <- as.vector(B.true %*% beta.true.nonlinear)
  }
  
  lambda.vec.true <- exp(f.val.true)
  
  #model.fit modification
  if (model.fit == "Par"){
    B <- matrix(c(rep(1, n), X, ran.X), ncol = 3)
  }else{
    basis <-  create.bspline.basis(c(0,1), nbasis = 4)
    B <- eval.basis(X, basis)
    B.ran <- eval.basis(ran.X, basis)
    B <- cbind(B, B.ran)
  }
  
  #replications
  
  n_cores <- detectCores(logical = TRUE)
  
  cl <- makeCluster(min(n_cores,60))
  registerDoParallel(cl)
  
  clusterExport(cl, varlist = c("B", "alpha.hyper", "mcmc.iters", "prior", "n",
                                "alpha.true", "lambda.vec.true", "mcmc.GE",
                                "ldens_ge", "alpha.update", "beta.update",
                                "ldens_PCprior", "mhupdate", "mpfr"),
                envir = environment())
  
  ret <- parLapply(cl, 1:coverage.rep, function(i){
    Y <- -log(1 - runif(n)^(1 / alpha.true)) / lambda.vec.true
    
    # MCMC
    fit.observed.GE <- mcmc.GE(Y, B, alpha.init = NULL, beta.init = NULL,
                               #priors
                               alpha.hyper = alpha.hyper,
                               beta.mean = 0, beta.sd = 100,
                               #mcmc settings
                               iters = mcmc.iters, burn = 2500, thin = 5, 
                               prior = prior)
    
    #f.val----
    beta.it <- fit.observed.GE$beta
    f.val.it <- t(B %*% t(beta.it))
    mean.f.val <- colMeans(f.val.it)
    
    #DIC----
    D.bar <- mean(fit.observed.GE$deviance)
    
    beta.post <- colMeans(fit.observed.GE$beta)
    alpha.post <- mean(fit.observed.GE$alpha)
    lambda.vec.post <- exp(as.vector(B %*% beta.post))
    cur.ll.post <- sum(ldens_ge(Y, alpha.post, lambda.vec.post))
    D.Y.theta.cap <- -2 * cur.ll.post
    
    DIC <- 2 * D.bar - D.Y.theta.cap
    
    #WAIC----
    mi <- colMeans(fit.observed.GE$mi.vi)
    vi <- apply(fit.observed.GE$mi.vi, 2, var)
    WAIC <- -2 * sum(mi) + 2 * sum(vi)
    
    # Final Object
    ret.final <- list(fit.observed.GE, DIC, WAIC)
    
    ret.final})
  
  stopCluster(cl)
  
  return(ret)
}

####==========================================================================================
####Setting 1: Linear, parametric, Non-PC
###n = 24
out_24_0.5_0.01_linear_par_Non_PC <- sim_all_tog(24, 0.5, 0.01, "Linear", "Par", "Non_PC")
save(out_24_0.5_0.01_linear_par_Non_PC, file = "out_24_0.5_0.01_linear_par_Non_PC.Rdata")
rm(out_24_0.5_0.01_linear_par_Non_PC)

out_24_0.5_0.1_linear_par_Non_PC <- sim_all_tog(24, 0.5, 0.1, "Linear", "Par", "Non_PC")
save(out_24_0.5_0.1_linear_par_Non_PC, file = "out_24_0.5_0.1_linear_par_Non_PC.Rdata")
rm(out_24_0.5_0.1_linear_par_Non_PC)

out_24_0.5_1_linear_par_Non_PC <- sim_all_tog(24, 0.5, 1, "Linear", "Par", "Non_PC")
save(out_24_0.5_1_linear_par_Non_PC, file = "out_24_0.5_1_linear_par_Non_PC.Rdata")
rm(out_24_0.5_1_linear_par_Non_PC)

out_24_1_0.01_linear_par_Non_PC <- sim_all_tog(24, 1, 0.01, "Linear", "Par", "Non_PC")
save(out_24_1_0.01_linear_par_Non_PC, file = "out_24_1_0.01_linear_par_Non_PC.Rdata")
rm(out_24_1_0.01_linear_par_Non_PC)

out_24_1_0.1_linear_par_Non_PC <- sim_all_tog(24, 1, 0.1, "Linear", "Par", "Non_PC")
save(out_24_1_0.1_linear_par_Non_PC, file = "out_24_1_0.1_linear_par_Non_PC.Rdata")
rm(out_24_1_0.1_linear_par_Non_PC)

out_24_1_1_linear_par_Non_PC <- sim_all_tog(24, 1, 1, "Linear", "Par", "Non_PC")
save(out_24_1_1_linear_par_Non_PC, file = "out_24_1_1_linear_par_Non_PC.Rdata")
rm(out_24_1_1_linear_par_Non_PC)

out_24_2_0.01_linear_par_Non_PC <- sim_all_tog(24, 2, 0.01, "Linear", "Par", "Non_PC")
save(out_24_2_0.01_linear_par_Non_PC, file = "out_24_2_0.01_linear_par_Non_PC.Rdata")
rm(out_24_2_0.01_linear_par_Non_PC)

out_24_2_0.1_linear_par_Non_PC <- sim_all_tog(24, 2, 0.1, "Linear", "Par", "Non_PC")
save(out_24_2_0.1_linear_par_Non_PC, file = "out_24_2_0.1_linear_par_Non_PC.Rdata")
rm(out_24_2_0.1_linear_par_Non_PC)

out_24_2_1_linear_par_Non_PC <- sim_all_tog(24, 2, 1, "Linear", "Par", "Non_PC")
save(out_24_2_1_linear_par_Non_PC, file = "out_24_2_1_linear_par_Non_PC.Rdata")
rm(out_24_2_1_linear_par_Non_PC)

###n = 99
out_99_0.5_0.01_linear_par_Non_PC <- sim_all_tog(99, 0.5, 0.01, "Linear", "Par", "Non_PC")
save(out_99_0.5_0.01_linear_par_Non_PC, file = "out_99_0.5_0.01_linear_par_Non_PC.Rdata")
rm(out_99_0.5_0.01_linear_par_Non_PC)

out_99_0.5_0.1_linear_par_Non_PC <- sim_all_tog(99, 0.5, 0.1, "Linear", "Par", "Non_PC")
save(out_99_0.5_0.1_linear_par_Non_PC, file = "out_99_0.5_0.1_linear_par_Non_PC.Rdata")
rm(out_99_0.5_0.1_linear_par_Non_PC)

out_99_0.5_1_linear_par_Non_PC <- sim_all_tog(99, 0.5, 1, "Linear", "Par", "Non_PC")
save(out_99_0.5_1_linear_par_Non_PC, file = "out_99_0.5_1_linear_par_Non_PC.Rdata")
rm(out_99_0.5_1_linear_par_Non_PC)

out_99_1_0.01_linear_par_Non_PC <- sim_all_tog(99, 1, 0.01, "Linear", "Par", "Non_PC")
save(out_99_1_0.01_linear_par_Non_PC, file = "out_99_1_0.01_linear_par_Non_PC.Rdata")
rm(out_99_1_0.01_linear_par_Non_PC)

out_99_1_0.1_linear_par_Non_PC <- sim_all_tog(99, 1, 0.1, "Linear", "Par", "Non_PC")
save(out_99_1_0.1_linear_par_Non_PC, file = "out_99_1_0.1_linear_par_Non_PC.Rdata")
rm(out_99_1_0.1_linear_par_Non_PC)

out_99_1_1_linear_par_Non_PC <- sim_all_tog(99, 1, 1, "Linear", "Par", "Non_PC")
save(out_99_1_1_linear_par_Non_PC, file = "out_99_1_1_linear_par_Non_PC.Rdata")
rm(out_99_1_1_linear_par_Non_PC)

out_99_2_0.01_linear_par_Non_PC <- sim_all_tog(99, 2, 0.01, "Linear", "Par", "Non_PC")
save(out_99_2_0.01_linear_par_Non_PC, file = "out_99_2_0.01_linear_par_Non_PC.Rdata")
rm(out_99_2_0.01_linear_par_Non_PC)

out_99_2_0.1_linear_par_Non_PC <- sim_all_tog(99, 2, 0.1, "Linear", "Par", "Non_PC")
save(out_99_2_0.1_linear_par_Non_PC, file = "out_99_2_0.1_linear_par_Non_PC.Rdata")
rm(out_99_2_0.1_linear_par_Non_PC)

out_99_2_1_linear_par_Non_PC <- sim_all_tog(99, 2, 1, "Linear", "Par", "Non_PC")
save(out_99_2_1_linear_par_Non_PC, file = "out_99_2_1_linear_par_Non_PC.Rdata")
rm(out_99_2_1_linear_par_Non_PC)

####==========================================================================================
####Setting 2: Non-Linear, parametric, Non-PC
###n = 24
out_24_0.5_0.01_nonlinear_par_Non_PC <- sim_all_tog(24, 0.5, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_24_0.5_0.01_nonlinear_par_Non_PC, file = "out_24_0.5_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_24_0.5_0.01_nonlinear_par_Non_PC)

out_24_0.5_0.1_nonlinear_par_Non_PC <- sim_all_tog(24, 0.5, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_24_0.5_0.1_nonlinear_par_Non_PC, file = "out_24_0.5_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_24_0.5_0.1_nonlinear_par_Non_PC)

out_24_0.5_1_nonlinear_par_Non_PC <- sim_all_tog(24, 0.5, 1, "Non_Linear", "Par", "Non_PC")
save(out_24_0.5_1_nonlinear_par_Non_PC, file = "out_24_0.5_1_nonlinear_par_Non_PC.Rdata")
rm(out_24_0.5_1_nonlinear_par_Non_PC)

out_24_1_0.01_nonlinear_par_Non_PC <- sim_all_tog(24, 1, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_24_1_0.01_nonlinear_par_Non_PC, file = "out_24_1_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_24_1_0.01_nonlinear_par_Non_PC)

out_24_1_0.1_nonlinear_par_Non_PC <- sim_all_tog(24, 1, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_24_1_0.1_nonlinear_par_Non_PC, file = "out_24_1_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_24_1_0.1_nonlinear_par_Non_PC)

out_24_1_1_nonlinear_par_Non_PC <- sim_all_tog(24, 1, 1, "Non_Linear", "Par", "Non_PC")
save(out_24_1_1_nonlinear_par_Non_PC, file = "out_24_1_1_nonlinear_par_Non_PC.Rdata")
rm(out_24_1_1_nonlinear_par_Non_PC)

out_24_2_0.01_nonlinear_par_Non_PC <- sim_all_tog(24, 2, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_24_2_0.01_nonlinear_par_Non_PC, file = "out_24_2_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_24_2_0.01_nonlinear_par_Non_PC)

out_24_2_0.1_nonlinear_par_Non_PC <- sim_all_tog(24, 2, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_24_2_0.1_nonlinear_par_Non_PC, file = "out_24_2_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_24_2_0.1_nonlinear_par_Non_PC)

out_24_2_1_nonlinear_par_Non_PC <- sim_all_tog(24, 2, 1, "Non_Linear", "Par", "Non_PC")
save(out_24_2_1_nonlinear_par_Non_PC, file = "out_24_2_1_nonlinear_par_Non_PC.Rdata")
rm(out_24_2_1_nonlinear_par_Non_PC)

###n = 99
out_99_0.5_0.01_nonlinear_par_Non_PC <- sim_all_tog(99, 0.5, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_99_0.5_0.01_nonlinear_par_Non_PC, file = "out_99_0.5_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_99_0.5_0.01_nonlinear_par_Non_PC)

out_99_0.5_0.1_nonlinear_par_Non_PC <- sim_all_tog(99, 0.5, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_99_0.5_0.1_nonlinear_par_Non_PC, file = "out_99_0.5_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_99_0.5_0.1_nonlinear_par_Non_PC)

out_99_0.5_1_nonlinear_par_Non_PC <- sim_all_tog(99, 0.5, 1, "Non_Linear", "Par", "Non_PC")
save(out_99_0.5_1_nonlinear_par_Non_PC, file = "out_99_0.5_1_nonlinear_par_Non_PC.Rdata")
rm(out_99_0.5_1_nonlinear_par_Non_PC)

out_99_1_0.01_nonlinear_par_Non_PC <- sim_all_tog(99, 1, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_99_1_0.01_nonlinear_par_Non_PC, file = "out_99_1_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_99_1_0.01_nonlinear_par_Non_PC)

out_99_1_0.1_nonlinear_par_Non_PC <- sim_all_tog(99, 1, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_99_1_0.1_nonlinear_par_Non_PC, file = "out_99_1_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_99_1_0.1_nonlinear_par_Non_PC)

out_99_1_1_nonlinear_par_Non_PC <- sim_all_tog(99, 1, 1, "Non_Linear", "Par", "Non_PC")
save(out_99_1_1_nonlinear_par_Non_PC, file = "out_99_1_1_nonlinear_par_Non_PC.Rdata")
rm(out_99_1_1_nonlinear_par_Non_PC)

out_99_2_0.01_nonlinear_par_Non_PC <- sim_all_tog(99, 2, 0.01, "Non_Linear", "Par", "Non_PC")
save(out_99_2_0.01_nonlinear_par_Non_PC, file = "out_99_2_0.01_nonlinear_par_Non_PC.Rdata")
rm(out_99_2_0.01_nonlinear_par_Non_PC)

out_99_2_0.1_nonlinear_par_Non_PC <- sim_all_tog(99, 2, 0.1, "Non_Linear", "Par", "Non_PC")
save(out_99_2_0.1_nonlinear_par_Non_PC, file = "out_99_2_0.1_nonlinear_par_Non_PC.Rdata")
rm(out_99_2_0.1_nonlinear_par_Non_PC)

out_99_2_1_nonlinear_par_Non_PC <- sim_all_tog(99, 2, 1, "Non_Linear", "Par", "Non_PC")
save(out_99_2_1_nonlinear_par_Non_PC, file = "out_99_2_1_nonlinear_par_Non_PC.Rdata")
rm(out_99_2_1_nonlinear_par_Non_PC)

####==========================================================================================
####Setting 5: Linear, parametric, PC
###n = 24
out_24_0.5_2.5_linear_par_PC <- sim_all_tog(24, 0.5, 2.5, "Linear", "Par", "PC")
save(out_24_0.5_2.5_linear_par_PC, file = "out_24_0.5_2.5_linear_par_PC.Rdata")
rm(out_24_0.5_2.5_linear_par_PC)

out_24_0.5_5_linear_par_PC <- sim_all_tog(24, 0.5, 5, "Linear", "Par", "PC")
save(out_24_0.5_5_linear_par_PC, file = "out_24_0.5_5_linear_par_PC.Rdata")
rm(out_24_0.5_5_linear_par_PC)

out_24_0.5_10_linear_par_PC <- sim_all_tog(24, 0.5, 10, "Linear", "Par", "PC")
save(out_24_0.5_10_linear_par_PC, file = "out_24_0.5_10_linear_par_PC.Rdata")
rm(out_24_0.5_10_linear_par_PC)

out_24_1_2.5_linear_par_PC <- sim_all_tog(24, 1, 2.5, "Linear", "Par", "PC")
save(out_24_1_2.5_linear_par_PC, file = "out_24_1_2.5_linear_par_PC.Rdata")
rm(out_24_1_2.5_linear_par_PC)

out_24_1_5_linear_par_PC <- sim_all_tog(24, 1, 5, "Linear", "Par", "PC")
save(out_24_1_5_linear_par_PC, file = "out_24_1_5_linear_par_PC.Rdata")
rm(out_24_1_5_linear_par_PC)

out_24_1_10_linear_par_PC <- sim_all_tog(24, 1, 10, "Linear", "Par", "PC")
save(out_24_1_10_linear_par_PC, file = "out_24_1_10_linear_par_PC.Rdata")
rm(out_24_1_10_linear_par_PC)

out_24_2_2.5_linear_par_PC <- sim_all_tog(24, 2, 2.5, "Linear", "Par", "PC")
save(out_24_2_2.5_linear_par_PC, file = "out_24_2_2.5_linear_par_PC.Rdata")
rm(out_24_2_2.5_linear_par_PC)

out_24_2_5_linear_par_PC <- sim_all_tog(24, 2, 5, "Linear", "Par", "PC")
save(out_24_2_5_linear_par_PC, file = "out_24_2_5_linear_par_PC.Rdata")
rm(out_24_2_5_linear_par_PC)

out_24_2_10_linear_par_PC <- sim_all_tog(24, 2, 10, "Linear", "Par", "PC")
save(out_24_2_10_linear_par_PC, file = "out_24_2_10_linear_par_PC.Rdata")
rm(out_24_2_10_linear_par_PC)

###n = 99
out_99_0.5_2.5_linear_par_PC <- sim_all_tog(99, 0.5, 2.5, "Linear", "Par", "PC")
save(out_99_0.5_2.5_linear_par_PC, file = "out_99_0.5_2.5_linear_par_PC.Rdata")
rm(out_99_0.5_2.5_linear_par_PC)

out_99_0.5_5_linear_par_PC <- sim_all_tog(99, 0.5, 5, "Linear", "Par", "PC")
save(out_99_0.5_5_linear_par_PC, file = "out_99_0.5_5_linear_par_PC.Rdata")
rm(out_99_0.5_5_linear_par_PC)

out_99_0.5_10_linear_par_PC <- sim_all_tog(99, 0.5, 10, "Linear", "Par", "PC")
save(out_99_0.5_10_linear_par_PC, file = "out_99_0.5_10_linear_par_PC.Rdata")
rm(out_99_0.5_10_linear_par_PC)

out_99_1_2.5_linear_par_PC <- sim_all_tog(99, 1, 2.5, "Linear", "Par", "PC")
save(out_99_1_2.5_linear_par_PC, file = "out_99_1_2.5_linear_par_PC.Rdata")
rm(out_99_1_2.5_linear_par_PC)

out_99_1_5_linear_par_PC <- sim_all_tog(99, 1, 5, "Linear", "Par", "PC")
save(out_99_1_5_linear_par_PC, file = "out_99_1_5_linear_par_PC.Rdata")
rm(out_99_1_5_linear_par_PC)

out_99_1_10_linear_par_PC <- sim_all_tog(99, 1, 10, "Linear", "Par", "PC")
save(out_99_1_10_linear_par_PC, file = "out_99_1_10_linear_par_PC.Rdata")
rm(out_99_1_10_linear_par_PC)

out_99_2_2.5_linear_par_PC <- sim_all_tog(99, 2, 2.5, "Linear", "Par", "PC")
save(out_99_2_2.5_linear_par_PC, file = "out_99_2_2.5_linear_par_PC.Rdata")
rm(out_99_2_2.5_linear_par_PC)

out_99_2_5_linear_par_PC <- sim_all_tog(99, 2, 5, "Linear", "Par", "PC")
save(out_99_2_5_linear_par_PC, file = "out_99_2_5_linear_par_PC.Rdata")
rm(out_99_2_5_linear_par_PC)

out_99_2_10_linear_par_PC <- sim_all_tog(99, 2, 10, "Linear", "Par", "PC")
save(out_99_2_10_linear_par_PC, file = "out_99_2_10_linear_par_PC.Rdata")
rm(out_99_2_10_linear_par_PC)

####==========================================================================================
####Setting 6: Non-Linear, parametric, PC
###n = 24
out_24_0.5_2.5_nonlinear_par_PC <- sim_all_tog(24, 0.5, 2.5, "Non_Linear", "Par", "PC")
save(out_24_0.5_2.5_nonlinear_par_PC, file = "out_24_0.5_2.5_nonlinear_par_PC.Rdata")
rm(out_24_0.5_2.5_nonlinear_par_PC)

out_24_0.5_5_nonlinear_par_PC <- sim_all_tog(24, 0.5, 5, "Non_Linear", "Par", "PC")
save(out_24_0.5_5_nonlinear_par_PC, file = "out_24_0.5_5_nonlinear_par_PC.Rdata")
rm(out_24_0.5_5_nonlinear_par_PC)

out_24_0.5_10_nonlinear_par_PC <- sim_all_tog(24, 0.5, 10, "Non_Linear", "Par", "PC")
save(out_24_0.5_10_nonlinear_par_PC, file = "out_24_0.5_10_nonlinear_par_PC.Rdata")
rm(out_24_0.5_10_nonlinear_par_PC)

out_24_1_2.5_nonlinear_par_PC <- sim_all_tog(24, 1, 2.5, "Non_Linear", "Par", "PC")
save(out_24_1_2.5_nonlinear_par_PC, file = "out_24_1_2.5_nonlinear_par_PC.Rdata")
rm(out_24_1_2.5_nonlinear_par_PC)

out_24_1_5_nonlinear_par_PC <- sim_all_tog(24, 1, 5, "Non_Linear", "Par", "PC")
save(out_24_1_5_nonlinear_par_PC, file = "out_24_1_5_nonlinear_par_PC.Rdata")
rm(out_24_1_5_nonlinear_par_PC)

out_24_1_10_nonlinear_par_PC <- sim_all_tog(24, 1, 10, "Non_Linear", "Par", "PC")
save(out_24_1_10_nonlinear_par_PC, file = "out_24_1_10_nonlinear_par_PC.Rdata")
rm(out_24_1_10_nonlinear_par_PC)

out_24_2_2.5_nonlinear_par_PC <- sim_all_tog(24, 2, 2.5, "Non_Linear", "Par", "PC")
save(out_24_2_2.5_nonlinear_par_PC, file = "out_24_2_2.5_nonlinear_par_PC.Rdata")
rm(out_24_2_2.5_nonlinear_par_PC)

out_24_2_5_nonlinear_par_PC <- sim_all_tog(24, 2, 5, "Non_Linear", "Par", "PC")
save(out_24_2_5_nonlinear_par_PC, file = "out_24_2_5_nonlinear_par_PC.Rdata")
rm(out_24_2_5_nonlinear_par_PC)

out_24_2_10_nonlinear_par_PC <- sim_all_tog(24, 2, 10, "Non_Linear", "Par", "PC")
save(out_24_2_10_nonlinear_par_PC, file = "out_24_2_10_nonlinear_par_PC.Rdata")
rm(out_24_2_10_nonlinear_par_PC)

###n = 99
out_99_0.5_2.5_nonlinear_par_PC <- sim_all_tog(99, 0.5, 2.5, "Non_Linear", "Par", "PC")
save(out_99_0.5_2.5_nonlinear_par_PC, file = "out_99_0.5_2.5_nonlinear_par_PC.Rdata")
rm(out_99_0.5_2.5_nonlinear_par_PC)

out_99_0.5_5_nonlinear_par_PC <- sim_all_tog(99, 0.5, 5, "Non_Linear", "Par", "PC")
save(out_99_0.5_5_nonlinear_par_PC, file = "out_99_0.5_5_nonlinear_par_PC.Rdata")
rm(out_99_0.5_5_nonlinear_par_PC)

out_99_0.5_10_nonlinear_par_PC <- sim_all_tog(99, 0.5, 10, "Non_Linear", "Par", "PC")
save(out_99_0.5_10_nonlinear_par_PC, file = "out_99_0.5_10_nonlinear_par_PC.Rdata")
rm(out_99_0.5_10_nonlinear_par_PC)

out_99_1_2.5_nonlinear_par_PC <- sim_all_tog(99, 1, 2.5, "Non_Linear", "Par", "PC")
save(out_99_1_2.5_nonlinear_par_PC, file = "out_99_1_2.5_nonlinear_par_PC.Rdata")
rm(out_99_1_2.5_nonlinear_par_PC)

out_99_1_5_nonlinear_par_PC <- sim_all_tog(99, 1, 5, "Non_Linear", "Par", "PC")
save(out_99_1_5_nonlinear_par_PC, file = "out_99_1_5_nonlinear_par_PC.Rdata")
rm(out_99_1_5_nonlinear_par_PC)

out_99_1_10_nonlinear_par_PC <- sim_all_tog(99, 1, 10, "Non_Linear", "Par", "PC")
save(out_99_1_10_nonlinear_par_PC, file = "out_99_1_10_nonlinear_par_PC.Rdata")
rm(out_99_1_10_nonlinear_par_PC)

out_99_2_2.5_nonlinear_par_PC <- sim_all_tog(99, 2, 2.5, "Non_Linear", "Par", "PC")
save(out_99_2_2.5_nonlinear_par_PC, file = "out_99_2_2.5_nonlinear_par_PC.Rdata")
rm(out_99_2_2.5_nonlinear_par_PC)

out_99_2_5_nonlinear_par_PC <- sim_all_tog(99, 2, 5, "Non_Linear", "Par", "PC")
save(out_99_2_5_nonlinear_par_PC, file = "out_99_2_5_nonlinear_par_PC.Rdata")
rm(out_99_2_5_nonlinear_par_PC)

out_99_2_10_nonlinear_par_PC <- sim_all_tog(99, 2, 10, "Non_Linear", "Par", "PC")
save(out_99_2_10_nonlinear_par_PC, file = "out_99_2_10_nonlinear_par_PC.Rdata")
rm(out_99_2_10_nonlinear_par_PC)

####==========================================================================================
####Setting 3: Linear, semiparametric, Non-PC
###n = 24
out_24_0.5_0.01_linear_semipar_Non_PC <- sim_all_tog(24, 0.5, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_0.01_linear_semipar_Non_PC, file = "out_24_0.5_0.01_linear_semipar_Non_PC.Rdata")
rm(out_24_0.5_0.01_linear_semipar_Non_PC)

out_24_0.5_0.1_linear_semipar_Non_PC <- sim_all_tog(24, 0.5, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_0.1_linear_semipar_Non_PC, file = "out_24_0.5_0.1_linear_semipar_Non_PC.Rdata")
rm(out_24_0.5_0.1_linear_semipar_Non_PC)

out_24_0.5_1_linear_semipar_Non_PC <- sim_all_tog(24, 0.5, 1, "Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_1_linear_semipar_Non_PC, file = "out_24_0.5_1_linear_semipar_Non_PC.Rdata")
rm(out_24_0.5_1_linear_semipar_Non_PC)

out_24_1_0.01_linear_semipar_Non_PC <- sim_all_tog(24, 1, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_24_1_0.01_linear_semipar_Non_PC, file = "out_24_1_0.01_linear_semipar_Non_PC.Rdata")
rm(out_24_1_0.01_linear_semipar_Non_PC)

out_24_1_0.1_linear_semipar_Non_PC <- sim_all_tog(24, 1, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_24_1_0.1_linear_semipar_Non_PC, file = "out_24_1_0.1_linear_semipar_Non_PC.Rdata")
rm(out_24_1_0.1_linear_semipar_Non_PC)

out_24_1_1_linear_semipar_Non_PC <- sim_all_tog(24, 1, 1, "Linear", "Semi_Par", "Non_PC")
save(out_24_1_1_linear_semipar_Non_PC, file = "out_24_1_1_linear_semipar_Non_PC.Rdata")
rm(out_24_1_1_linear_semipar_Non_PC)

out_24_2_0.01_linear_semipar_Non_PC <- sim_all_tog(24, 2, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_24_2_0.01_linear_semipar_Non_PC, file = "out_24_2_0.01_linear_semipar_Non_PC.Rdata")
rm(out_24_2_0.01_linear_semipar_Non_PC)

out_24_2_0.1_linear_semipar_Non_PC <- sim_all_tog(24, 2, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_24_2_0.1_linear_semipar_Non_PC, file = "out_24_2_0.1_linear_semipar_Non_PC.Rdata")
rm(out_24_2_0.1_linear_semipar_Non_PC)

out_24_2_1_linear_semipar_Non_PC <- sim_all_tog(24, 2, 1, "Linear", "Semi_Par", "Non_PC")
save(out_24_2_1_linear_semipar_Non_PC, file = "out_24_2_1_linear_semipar_Non_PC.Rdata")
rm(out_24_2_1_linear_semipar_Non_PC)

###n = 99
out_99_0.5_0.01_linear_semipar_Non_PC <- sim_all_tog(99, 0.5, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_0.01_linear_semipar_Non_PC, file = "out_99_0.5_0.01_linear_semipar_Non_PC.Rdata")
rm(out_99_0.5_0.01_linear_semipar_Non_PC)

out_99_0.5_0.1_linear_semipar_Non_PC <- sim_all_tog(99, 0.5, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_0.1_linear_semipar_Non_PC, file = "out_99_0.5_0.1_linear_semipar_Non_PC.Rdata")
rm(out_99_0.5_0.1_linear_semipar_Non_PC)

out_99_0.5_1_linear_semipar_Non_PC <- sim_all_tog(99, 0.5, 1, "Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_1_linear_semipar_Non_PC, file = "out_99_0.5_1_linear_semipar_Non_PC.Rdata")
rm(out_99_0.5_1_linear_semipar_Non_PC)

out_99_1_0.01_linear_semipar_Non_PC <- sim_all_tog(99, 1, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_99_1_0.01_linear_semipar_Non_PC, file = "out_99_1_0.01_linear_semipar_Non_PC.Rdata")
rm(out_99_1_0.01_linear_semipar_Non_PC)

out_99_1_0.1_linear_semipar_Non_PC <- sim_all_tog(99, 1, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_99_1_0.1_linear_semipar_Non_PC, file = "out_99_1_0.1_linear_semipar_Non_PC.Rdata")
rm(out_99_1_0.1_linear_semipar_Non_PC)

out_99_1_1_linear_semipar_Non_PC <- sim_all_tog(99, 1, 1, "Linear", "Semi_Par", "Non_PC")
save(out_99_1_1_linear_semipar_Non_PC, file = "out_99_1_1_linear_semipar_Non_PC.Rdata")
rm(out_99_1_1_linear_semipar_Non_PC)

out_99_2_0.01_linear_semipar_Non_PC <- sim_all_tog(99, 2, 0.01, "Linear", "Semi_Par", "Non_PC")
save(out_99_2_0.01_linear_semipar_Non_PC, file = "out_99_2_0.01_linear_semipar_Non_PC.Rdata")
rm(out_99_2_0.01_linear_semipar_Non_PC)

out_99_2_0.1_linear_semipar_Non_PC <- sim_all_tog(99, 2, 0.1, "Linear", "Semi_Par", "Non_PC")
save(out_99_2_0.1_linear_semipar_Non_PC, file = "out_99_2_0.1_linear_semipar_Non_PC.Rdata")
rm(out_99_2_0.1_linear_semipar_Non_PC)

out_99_2_1_linear_semipar_Non_PC <- sim_all_tog(99, 2, 1, "Linear", "Semi_Par", "Non_PC")
save(out_99_2_1_linear_semipar_Non_PC, file = "out_99_2_1_linear_semipar_Non_PC.Rdata")
rm(out_99_2_1_linear_semipar_Non_PC)

####==========================================================================================
####Setting 4: Non-Linear, semiparametric, Non-PC
###n = 24
out_24_0.5_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(24, 0.5, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_0.01_nonlinear_semipar_Non_PC, file = "out_24_0.5_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_0.5_0.01_nonlinear_semipar_Non_PC)

out_24_0.5_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 0.5, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_0.1_nonlinear_semipar_Non_PC, file = "out_24_0.5_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_0.5_0.1_nonlinear_semipar_Non_PC)

out_24_0.5_1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 0.5, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_0.5_1_nonlinear_semipar_Non_PC, file = "out_24_0.5_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_0.5_1_nonlinear_semipar_Non_PC)

out_24_1_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(24, 1, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_1_0.01_nonlinear_semipar_Non_PC, file = "out_24_1_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_1_0.01_nonlinear_semipar_Non_PC)

out_24_1_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 1, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_1_0.1_nonlinear_semipar_Non_PC, file = "out_24_1_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_1_0.1_nonlinear_semipar_Non_PC)

out_24_1_1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 1, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_1_1_nonlinear_semipar_Non_PC, file = "out_24_1_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_1_1_nonlinear_semipar_Non_PC)

out_24_2_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(24, 2, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_2_0.01_nonlinear_semipar_Non_PC, file = "out_24_2_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_2_0.01_nonlinear_semipar_Non_PC)

out_24_2_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 2, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_2_0.1_nonlinear_semipar_Non_PC, file = "out_24_2_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_2_0.1_nonlinear_semipar_Non_PC)

out_24_2_1_nonlinear_semipar_Non_PC <- sim_all_tog(24, 2, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_24_2_1_nonlinear_semipar_Non_PC, file = "out_24_2_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_24_2_1_nonlinear_semipar_Non_PC)

###n = 99
out_99_0.5_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(99, 0.5, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_0.01_nonlinear_semipar_Non_PC, file = "out_99_0.5_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_0.5_0.01_nonlinear_semipar_Non_PC)

out_99_0.5_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 0.5, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_0.1_nonlinear_semipar_Non_PC, file = "out_99_0.5_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_0.5_0.1_nonlinear_semipar_Non_PC)

out_99_0.5_1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 0.5, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_0.5_1_nonlinear_semipar_Non_PC, file = "out_99_0.5_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_0.5_1_nonlinear_semipar_Non_PC)

out_99_1_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(99, 1, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_1_0.01_nonlinear_semipar_Non_PC, file = "out_99_1_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_1_0.01_nonlinear_semipar_Non_PC)

out_99_1_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 1, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_1_0.1_nonlinear_semipar_Non_PC, file = "out_99_1_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_1_0.1_nonlinear_semipar_Non_PC)

out_99_1_1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 1, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_1_1_nonlinear_semipar_Non_PC, file = "out_99_1_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_1_1_nonlinear_semipar_Non_PC)

out_99_2_0.01_nonlinear_semipar_Non_PC <- sim_all_tog(99, 2, 0.01, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_2_0.01_nonlinear_semipar_Non_PC, file = "out_99_2_0.01_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_2_0.01_nonlinear_semipar_Non_PC)

out_99_2_0.1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 2, 0.1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_2_0.1_nonlinear_semipar_Non_PC, file = "out_99_2_0.1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_2_0.1_nonlinear_semipar_Non_PC)

out_99_2_1_nonlinear_semipar_Non_PC <- sim_all_tog(99, 2, 1, "Non_Linear", "Semi_Par", "Non_PC")
save(out_99_2_1_nonlinear_semipar_Non_PC, file = "out_99_2_1_nonlinear_semipar_Non_PC.Rdata")
rm(out_99_2_1_nonlinear_semipar_Non_PC)

####==========================================================================================
####Setting 7: Linear, semiparametric, PC
###n = 24
out_24_0.5_2.5_linear_semipar_PC <- sim_all_tog(24, 0.5, 2.5, "Linear", "Semi_Par", "PC")
save(out_24_0.5_2.5_linear_semipar_PC, file = "out_24_0.5_2.5_linear_semipar_PC.Rdata")
rm(out_24_0.5_2.5_linear_semipar_PC)

out_24_0.5_5_linear_semipar_PC <- sim_all_tog(24, 0.5, 5, "Linear", "Semi_Par", "PC")
save(out_24_0.5_5_linear_semipar_PC, file = "out_24_0.5_5_linear_semipar_PC.Rdata")
rm(out_24_0.5_5_linear_semipar_PC)

out_24_0.5_10_linear_semipar_PC <- sim_all_tog(24, 0.5, 10, "Linear", "Semi_Par", "PC")
save(out_24_0.5_10_linear_semipar_PC, file = "out_24_0.5_10_linear_semipar_PC.Rdata")
rm(out_24_0.5_10_linear_semipar_PC)

out_24_1_2.5_linear_semipar_PC <- sim_all_tog(24, 1, 2.5, "Linear", "Semi_Par", "PC")
save(out_24_1_2.5_linear_semipar_PC, file = "out_24_1_2.5_linear_semipar_PC.Rdata")
rm(out_24_1_2.5_linear_semipar_PC)

out_24_1_5_linear_semipar_PC <- sim_all_tog(24, 1, 5, "Linear", "Semi_Par", "PC")
save(out_24_1_5_linear_semipar_PC, file = "out_24_1_5_linear_semipar_PC.Rdata")
rm(out_24_1_5_linear_semipar_PC)

out_24_1_10_linear_semipar_PC <- sim_all_tog(24, 1, 10, "Linear", "Semi_Par", "PC")
save(out_24_1_10_linear_semipar_PC, file = "out_24_1_10_linear_semipar_PC.Rdata")
rm(out_24_1_10_linear_semipar_PC)

out_24_2_2.5_linear_semipar_PC <- sim_all_tog(24, 2, 2.5, "Linear", "Semi_Par", "PC")
save(out_24_2_2.5_linear_semipar_PC, file = "out_24_2_2.5_linear_semipar_PC.Rdata")
rm(out_24_2_2.5_linear_semipar_PC)

out_24_2_5_linear_semipar_PC <- sim_all_tog(24, 2, 5, "Linear", "Semi_Par", "PC")
save(out_24_2_5_linear_semipar_PC, file = "out_24_2_5_linear_semipar_PC.Rdata")
rm(out_24_2_5_linear_semipar_PC)

out_24_2_10_linear_semipar_PC <- sim_all_tog(24, 2, 10, "Linear", "Semi_Par", "PC")
save(out_24_2_10_linear_semipar_PC, file = "out_24_2_10_linear_semipar_PC.Rdata")
rm(out_24_2_10_linear_semipar_PC)

###n = 99
out_99_0.5_2.5_linear_semipar_PC <- sim_all_tog(99, 0.5, 2.5, "Linear", "Semi_Par", "PC")
save(out_99_0.5_2.5_linear_semipar_PC, file = "out_99_0.5_2.5_linear_semipar_PC.Rdata")
rm(out_99_0.5_2.5_linear_semipar_PC)

out_99_0.5_5_linear_semipar_PC <- sim_all_tog(99, 0.5, 5, "Linear", "Semi_Par", "PC")
save(out_99_0.5_5_linear_semipar_PC, file = "out_99_0.5_5_linear_semipar_PC.Rdata")
rm(out_99_0.5_5_linear_semipar_PC)

out_99_0.5_10_linear_semipar_PC <- sim_all_tog(99, 0.5, 10, "Linear", "Semi_Par", "PC")
save(out_99_0.5_10_linear_semipar_PC, file = "out_99_0.5_10_linear_semipar_PC.Rdata")
rm(out_99_0.5_10_linear_semipar_PC)

out_99_1_2.5_linear_semipar_PC <- sim_all_tog(99, 1, 2.5, "Linear", "Semi_Par", "PC")
save(out_99_1_2.5_linear_semipar_PC, file = "out_99_1_2.5_linear_semipar_PC.Rdata")
rm(out_99_1_2.5_linear_semipar_PC)

out_99_1_5_linear_semipar_PC <- sim_all_tog(99, 1, 5, "Linear", "Semi_Par", "PC")
save(out_99_1_5_linear_semipar_PC, file = "out_99_1_5_linear_semipar_PC.Rdata")
rm(out_99_1_5_linear_semipar_PC)

out_99_1_10_linear_semipar_PC <- sim_all_tog(99, 1, 10, "Linear", "Semi_Par", "PC")
save(out_99_1_10_linear_semipar_PC, file = "out_99_1_10_linear_semipar_PC.Rdata")
rm(out_99_1_10_linear_semipar_PC)

out_99_2_2.5_linear_semipar_PC <- sim_all_tog(99, 2, 2.5, "Linear", "Semi_Par", "PC")
save(out_99_2_2.5_linear_semipar_PC, file = "out_99_2_2.5_linear_semipar_PC.Rdata")
rm(out_99_2_2.5_linear_semipar_PC)

out_99_2_5_linear_semipar_PC <- sim_all_tog(99, 2, 5, "Linear", "Semi_Par", "PC")
save(out_99_2_5_linear_semipar_PC, file = "out_99_2_5_linear_semipar_PC.Rdata")
rm(out_99_2_5_linear_semipar_PC)

out_99_2_10_linear_semipar_PC <- sim_all_tog(99, 2, 10, "Linear", "Semi_Par", "PC")
save(out_99_2_10_linear_semipar_PC, file = "out_99_2_10_linear_semipar_PC.Rdata")
rm(out_99_2_10_linear_semipar_PC)

####==========================================================================================
####Setting 8: Non-Linear, semiparametric, PC
###n = 24
out_24_0.5_2.5_nonlinear_semipar_PC <- sim_all_tog(24, 0.5, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_24_0.5_2.5_nonlinear_semipar_PC, file = "out_24_0.5_2.5_nonlinear_semipar_PC.Rdata")
rm(out_24_0.5_2.5_nonlinear_semipar_PC)

out_24_0.5_5_nonlinear_semipar_PC <- sim_all_tog(24, 0.5, 5, "Non_Linear", "Semi_Par", "PC")
save(out_24_0.5_5_nonlinear_semipar_PC, file = "out_24_0.5_5_nonlinear_semipar_PC.Rdata")
rm(out_24_0.5_5_nonlinear_semipar_PC)

out_24_0.5_10_nonlinear_semipar_PC <- sim_all_tog(24, 0.5, 10, "Non_Linear", "Semi_Par", "PC")
save(out_24_0.5_10_nonlinear_semipar_PC, file = "out_24_0.5_10_nonlinear_semipar_PC.Rdata")
rm(out_24_0.5_10_nonlinear_semipar_PC)

out_24_1_2.5_nonlinear_semipar_PC <- sim_all_tog(24, 1, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_24_1_2.5_nonlinear_semipar_PC, file = "out_24_1_2.5_nonlinear_semipar_PC.Rdata")
rm(out_24_1_2.5_nonlinear_semipar_PC)

out_24_1_5_nonlinear_semipar_PC <- sim_all_tog(24, 1, 5, "Non_Linear", "Semi_Par", "PC")
save(out_24_1_5_nonlinear_semipar_PC, file = "out_24_1_5_nonlinear_semipar_PC.Rdata")
rm(out_24_1_5_nonlinear_semipar_PC)

out_24_1_10_nonlinear_semipar_PC <- sim_all_tog(24, 1, 10, "Non_Linear", "Semi_Par", "PC")
save(out_24_1_10_nonlinear_semipar_PC, file = "out_24_1_10_nonlinear_semipar_PC.Rdata")
rm(out_24_1_10_nonlinear_semipar_PC)

out_24_2_2.5_nonlinear_semipar_PC <- sim_all_tog(24, 2, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_24_2_2.5_nonlinear_semipar_PC, file = "out_24_2_2.5_nonlinear_semipar_PC.Rdata")
rm(out_24_2_2.5_nonlinear_semipar_PC)

out_24_2_5_nonlinear_semipar_PC <- sim_all_tog(24, 2, 5, "Non_Linear", "Semi_Par", "PC")
save(out_24_2_5_nonlinear_semipar_PC, file = "out_24_2_5_nonlinear_semipar_PC.Rdata")
rm(out_24_2_5_nonlinear_semipar_PC)

out_24_2_10_nonlinear_semipar_PC <- sim_all_tog(24, 2, 10, "Non_Linear", "Semi_Par", "PC")
save(out_24_2_10_nonlinear_semipar_PC, file = "out_24_2_10_nonlinear_semipar_PC.Rdata")
rm(out_24_2_10_nonlinear_semipar_PC)

###n = 99
out_99_0.5_2.5_nonlinear_semipar_PC <- sim_all_tog(99, 0.5, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_99_0.5_2.5_nonlinear_semipar_PC, file = "out_99_0.5_2.5_nonlinear_semipar_PC.Rdata")
rm(out_99_0.5_2.5_nonlinear_semipar_PC)

out_99_0.5_5_nonlinear_semipar_PC <- sim_all_tog(99, 0.5, 5, "Non_Linear", "Semi_Par", "PC")
save(out_99_0.5_5_nonlinear_semipar_PC, file = "out_99_0.5_5_nonlinear_semipar_PC.Rdata")
rm(out_99_0.5_5_nonlinear_semipar_PC)

out_99_0.5_10_nonlinear_semipar_PC <- sim_all_tog(99, 0.5, 10, "Non_Linear", "Semi_Par", "PC")
save(out_99_0.5_10_nonlinear_semipar_PC, file = "out_99_0.5_10_nonlinear_semipar_PC.Rdata")
rm(out_99_0.5_10_nonlinear_semipar_PC)

out_99_1_2.5_nonlinear_semipar_PC <- sim_all_tog(99, 1, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_99_1_2.5_nonlinear_semipar_PC, file = "out_99_1_2.5_nonlinear_semipar_PC.Rdata")
rm(out_99_1_2.5_nonlinear_semipar_PC)

out_99_1_5_nonlinear_semipar_PC <- sim_all_tog(99, 1, 5, "Non_Linear", "Semi_Par", "PC")
save(out_99_1_5_nonlinear_semipar_PC, file = "out_99_1_5_nonlinear_semipar_PC.Rdata")
rm(out_99_1_5_nonlinear_semipar_PC)

out_99_1_10_nonlinear_semipar_PC <- sim_all_tog(99, 1, 10, "Non_Linear", "Semi_Par", "PC")
save(out_99_1_10_nonlinear_semipar_PC, file = "out_99_1_10_nonlinear_semipar_PC.Rdata")
rm(out_99_1_10_nonlinear_semipar_PC)

out_99_2_2.5_nonlinear_semipar_PC <- sim_all_tog(99, 2, 2.5, "Non_Linear", "Semi_Par", "PC")
save(out_99_2_2.5_nonlinear_semipar_PC, file = "out_99_2_2.5_nonlinear_semipar_PC.Rdata")
rm(out_99_2_2.5_nonlinear_semipar_PC)

out_99_2_5_nonlinear_semipar_PC <- sim_all_tog(99, 2, 5, "Non_Linear", "Semi_Par", "PC")
save(out_99_2_5_nonlinear_semipar_PC, file = "out_99_2_5_nonlinear_semipar_PC.Rdata")
rm(out_99_2_5_nonlinear_semipar_PC)

out_99_2_10_nonlinear_semipar_PC <- sim_all_tog(99, 2, 10, "Non_Linear", "Semi_Par", "PC")
save(out_99_2_10_nonlinear_semipar_PC, file = "out_99_2_10_nonlinear_semipar_PC.Rdata")
rm(out_99_2_10_nonlinear_semipar_PC)
