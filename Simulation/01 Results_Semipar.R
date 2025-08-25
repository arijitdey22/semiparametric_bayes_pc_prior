library(Rmpfr)
library(splines)
library(MASS)
library(pcaPP)
library(rainbow)
library(RCurl)
library(fds)
library(deSolve)
library(fda)

rm(list = ls())

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

#----------------------------
file.names <- c("out_24_0.5_0.01_linear_par_Non_PC.Rdata", "out_24_0.5_0.1_linear_par_Non_PC.Rdata", "out_24_0.5_1_linear_par_Non_PC.Rdata",
                "out_24_0.5_0.01_linear_semipar_Non_PC.Rdata", "out_24_0.5_0.1_linear_semipar_Non_PC.Rdata", "out_24_0.5_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_1_0.01_linear_par_Non_PC.Rdata", "out_24_1_0.1_linear_par_Non_PC.Rdata", "out_24_1_1_linear_par_Non_PC.Rdata",
                "out_24_1_0.01_linear_semipar_Non_PC.Rdata", "out_24_1_0.1_linear_semipar_Non_PC.Rdata", "out_24_1_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_0.01_linear_par_Non_PC.Rdata", "out_24_2_0.1_linear_par_Non_PC.Rdata", "out_24_2_1_linear_par_Non_PC.Rdata",
                "out_24_2_0.01_linear_semipar_Non_PC.Rdata", "out_24_2_0.1_linear_semipar_Non_PC.Rdata", "out_24_2_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_0.5_0.01_linear_par_Non_PC.Rdata", "out_99_0.5_0.1_linear_par_Non_PC.Rdata", "out_99_0.5_1_linear_par_Non_PC.Rdata",
                "out_99_0.5_0.01_linear_semipar_Non_PC.Rdata", "out_99_0.5_0.1_linear_semipar_Non_PC.Rdata", "out_99_0.5_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_1_0.01_linear_par_Non_PC.Rdata", "out_99_1_0.1_linear_par_Non_PC.Rdata", "out_99_1_1_linear_par_Non_PC.Rdata",
                "out_99_1_0.01_linear_semipar_Non_PC.Rdata", "out_99_1_0.1_linear_semipar_Non_PC.Rdata", "out_99_1_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_0.01_linear_par_Non_PC.Rdata", "out_99_2_0.1_linear_par_Non_PC.Rdata", "out_99_2_1_linear_par_Non_PC.Rdata",
                "out_99_2_0.01_linear_semipar_Non_PC.Rdata", "out_99_2_0.1_linear_semipar_Non_PC.Rdata", "out_99_2_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_2.5_linear_par_PC.Rdata", "out_24_0.5_5_linear_par_PC.Rdata", "out_24_0.5_10_linear_par_PC.Rdata",
                "out_24_0.5_2.5_linear_semipar_PC.Rdata", "out_24_0.5_5_linear_semipar_PC.Rdata", "out_24_0.5_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_24_1_2.5_linear_par_PC.Rdata", "out_24_1_5_linear_par_PC.Rdata", "out_24_1_10_linear_par_PC.Rdata",
                "out_24_1_2.5_linear_semipar_PC.Rdata", "out_24_1_5_linear_semipar_PC.Rdata", "out_24_1_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_linear_par_PC.Rdata", "out_24_2_5_linear_par_PC.Rdata", "out_24_2_10_linear_par_PC.Rdata",
                "out_24_2_2.5_linear_semipar_PC.Rdata", "out_24_2_5_linear_semipar_PC.Rdata", "out_24_2_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_0.5_2.5_linear_par_PC.Rdata", "out_99_0.5_5_linear_par_PC.Rdata", "out_99_0.5_10_linear_par_PC.Rdata",
                "out_99_0.5_2.5_linear_semipar_PC.Rdata", "out_99_0.5_5_linear_semipar_PC.Rdata", "out_99_0.5_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_1_2.5_linear_par_PC.Rdata", "out_99_1_5_linear_par_PC.Rdata", "out_99_1_10_linear_par_PC.Rdata",
                "out_99_1_2.5_linear_semipar_PC.Rdata", "out_99_1_5_linear_semipar_PC.Rdata", "out_99_1_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_linear_par_PC.Rdata", "out_99_2_5_linear_par_PC.Rdata", "out_99_2_10_linear_par_PC.Rdata",
                "out_99_2_2.5_linear_semipar_PC.Rdata", "out_99_2_5_linear_semipar_PC.Rdata", "out_99_2_10_linear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_0.01_nonlinear_par_Non_PC.Rdata", "out_24_0.5_0.1_nonlinear_par_Non_PC.Rdata", "out_24_0.5_1_nonlinear_par_Non_PC.Rdata",
                "out_24_0.5_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_0.5_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_0.5_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_1_0.01_nonlinear_par_Non_PC.Rdata", "out_24_1_0.1_nonlinear_par_Non_PC.Rdata", "out_24_1_1_nonlinear_par_Non_PC.Rdata",
                "out_24_1_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_1_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_1_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_0.01_nonlinear_par_Non_PC.Rdata", "out_24_2_0.1_nonlinear_par_Non_PC.Rdata", "out_24_2_1_nonlinear_par_Non_PC.Rdata",
                "out_24_2_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_2_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_2_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_0.5_0.01_nonlinear_par_Non_PC.Rdata", "out_99_0.5_0.1_nonlinear_par_Non_PC.Rdata", "out_99_0.5_1_nonlinear_par_Non_PC.Rdata",
                "out_99_0.5_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_0.5_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_0.5_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_1_0.01_nonlinear_par_Non_PC.Rdata", "out_99_1_0.1_nonlinear_par_Non_PC.Rdata", "out_99_1_1_nonlinear_par_Non_PC.Rdata",
                "out_99_1_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_1_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_1_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_0.01_nonlinear_par_Non_PC.Rdata", "out_99_2_0.1_nonlinear_par_Non_PC.Rdata", "out_99_2_1_nonlinear_par_Non_PC.Rdata",
                "out_99_2_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_2_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_2_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_2.5_nonlinear_par_PC.Rdata", "out_24_0.5_5_nonlinear_par_PC.Rdata", "out_24_0.5_10_nonlinear_par_PC.Rdata",
                "out_24_0.5_2.5_nonlinear_semipar_PC.Rdata", "out_24_0.5_5_nonlinear_semipar_PC.Rdata", "out_24_0.5_10_nonlinear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_24_1_2.5_nonlinear_par_PC.Rdata", "out_24_1_5_nonlinear_par_PC.Rdata", "out_24_1_10_nonlinear_par_PC.Rdata",
                "out_24_1_2.5_nonlinear_semipar_PC.Rdata", "out_24_1_5_nonlinear_semipar_PC.Rdata", "out_24_1_10_nonlinear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_nonlinear_par_PC.Rdata", "out_24_2_5_nonlinear_par_PC.Rdata", "out_24_2_10_nonlinear_par_PC.Rdata",
                "out_24_2_2.5_nonlinear_semipar_PC.Rdata", "out_24_2_5_nonlinear_semipar_PC.Rdata", "out_24_2_10_nonlinear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_0.5_2.5_nonlinear_par_PC.Rdata", "out_99_0.5_5_nonlinear_par_PC.Rdata", "out_99_0.5_10_nonlinear_par_PC.Rdata",
                "out_99_0.5_2.5_nonlinear_semipar_PC.Rdata", "out_99_0.5_5_nonlinear_semipar_PC.Rdata", "out_99_0.5_10_nonlinear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_1_2.5_nonlinear_par_PC.Rdata", "out_99_1_5_nonlinear_par_PC.Rdata", "out_99_1_10_nonlinear_par_PC.Rdata",
                "out_99_1_2.5_nonlinear_semipar_PC.Rdata", "out_99_1_5_nonlinear_semipar_PC.Rdata", "out_99_1_10_nonlinear_semipar_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_nonlinear_par_PC.Rdata", "out_99_2_5_nonlinear_par_PC.Rdata", "out_99_2_10_nonlinear_par_PC.Rdata",
                "out_99_2_2.5_nonlinear_semipar_PC.Rdata", "out_99_2_5_nonlinear_semipar_PC.Rdata", "out_99_2_10_nonlinear_semipar_PC.Rdata")


#----------------------------

all.storage <- matrix(0, ncol = 6, nrow = 96)

for (j in 1:length(file.names))
{
  
  load(file.names[j])
  
  list_name <- substr(file.names[j], 1, nchar(file.names[j]) - 6)
  cur.list <- get(list_name)
  
  ####----------------------------------------
  if ((j %% 18 == 0) | (j %% 18 > 12)){
    alpha.true = 2
  }else if((j %% 18 < 13) & (j %% 18)>6){
    alpha.true = 1
  }else{
    alpha.true = 0.5
  }
  
  if (ceiling(j/18) %% 2 == 1){
    n = 24
  }else{
    n = 99
  }
  
  X <- seq((1 / (n+1)), (1 - 1 / (n+1)), length.out = n)
  set.seed(100)
  ran.X <- runif(n, 0, 0.5)
  ran.u <- runif(n)
  
  if (j < 73){
    beta.true <- c(-5, 5, 3)
    B.true <- matrix(c(rep(1, n), X, ran.X), ncol = 3)
  }else{
    beta.true <- c(-5, 2, 3)
    B.true <- matrix(c(rep(1, n), X^2, sin(2*pi*ran.X)), ncol = 3)
  }
  
  f.val.true <- as.vector(B.true %*% beta.true)
  
  lambda.vec.true <- exp(f.val.true)
  
  if (ceiling(j / 3) %% 2 == 1){
    B <- matrix(c(rep(1, n), X, ran.X), ncol = 3)
  }else{
    basis <-  create.bspline.basis(c(0,1), nbasis = 4)
    B <- eval.basis(X, basis)
    B.ran <- eval.basis(ran.X, basis)
    B <- cbind(B, B.ran)
  }
  ####----------------------------------------
  
  coverage.rep <- 1e3
  
  mean.f.val = matrix(NA, nrow = coverage.rep, ncol = length(f.val.true))
  DIC <- numeric(coverage.rep)
  WAIC <- numeric(coverage.rep)
  
  for (i in 1:coverage.rep)
  {
    
    ret.final <- cur.list[[i]]
    fit.observed.GE <- ret.final[[1]]
    
    #f.val----
    beta.it <- fit.observed.GE$beta
    f.val.it <- t(B %*% t(beta.it))
    mean.f.val[i,] <- colMeans(f.val.it)
    
    #DIC----
    DIC[i] <- ret.final[[2]]
    
    #WAIC----
    WAIC[i] <- ret.final[[3]]
  }
  
  ###Estimation error
  f.hat <- colMeans(mean.f.val)
  f.val.mse <- round(sum((f.val.true - f.hat)^2),3)
  f.val.abbs_error <- round(sum(abs(f.val.true - f.hat)),3)
  
  ##--   --   --   --   --   --   --   --   --   --
  #Related to the model as a whole
  
  ###DIC
  DIC <- round(mean(DIC),3)
  
  ###WAIC
  WAIC <- round(mean(WAIC),3)
  
  #----------------------------------------------
  col <- ifelse(j %% 6 == 0, 6, j%%6) 
  row <- ((ceiling(j/6)-1)*4) + 1
  
  all.storage[row, col] <- f.val.mse
  all.storage[(row+1), col] <- f.val.abbs_error
  all.storage[(row+2), col] <- DIC
  all.storage[(row+3), col] <- WAIC
  #----------------------------------------------
  rm(cur.list)
  rm(list = list_name)
  print(paste0("We finished setting ", j))
}


df <- data.frame(all.storage)

write.table(df, file = "00 Results_Semipar.csv", sep = ",",
            append = TRUE, quote = TRUE,
            col.names = FALSE, row.names = FALSE)


