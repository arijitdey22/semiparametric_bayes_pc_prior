rm(list = ls())

#----------------------------
file.names <- c("out_24_0.5_2.5_linear_par_PC.Rdata", "out_24_0.5_5_linear_par_PC.Rdata", "out_24_0.5_10_linear_par_PC.Rdata",
                "out_24_0.5_0.01_linear_par_Non_PC.Rdata", "out_24_0.5_0.1_linear_par_Non_PC.Rdata", "out_24_0.5_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_24_1_2.5_linear_par_PC.Rdata", "out_24_1_5_linear_par_PC.Rdata", "out_24_1_10_linear_par_PC.Rdata",
                "out_24_1_0.01_linear_par_Non_PC.Rdata", "out_24_1_0.1_linear_par_Non_PC.Rdata", "out_24_1_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_linear_par_PC.Rdata", "out_24_2_5_linear_par_PC.Rdata", "out_24_2_10_linear_par_PC.Rdata",
                "out_24_2_0.01_linear_par_Non_PC.Rdata", "out_24_2_0.1_linear_par_Non_PC.Rdata", "out_24_2_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_0.5_2.5_linear_par_PC.Rdata", "out_99_0.5_5_linear_par_PC.Rdata", "out_99_0.5_10_linear_par_PC.Rdata",
                "out_99_0.5_0.01_linear_par_Non_PC.Rdata", "out_99_0.5_0.1_linear_par_Non_PC.Rdata", "out_99_0.5_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_1_2.5_linear_par_PC.Rdata", "out_99_1_5_linear_par_PC.Rdata", "out_99_1_10_linear_par_PC.Rdata",
                "out_99_1_0.01_linear_par_Non_PC.Rdata", "out_99_1_0.1_linear_par_Non_PC.Rdata", "out_99_1_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_linear_par_PC.Rdata", "out_99_2_5_linear_par_PC.Rdata", "out_99_2_10_linear_par_PC.Rdata",
                "out_99_2_0.01_linear_par_Non_PC.Rdata", "out_99_2_0.1_linear_par_Non_PC.Rdata", "out_99_2_1_linear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_2.5_nonlinear_par_PC.Rdata", "out_24_0.5_5_nonlinear_par_PC.Rdata", "out_24_0.5_10_nonlinear_par_PC.Rdata",
                "out_24_0.5_0.01_nonlinear_par_Non_PC.Rdata", "out_24_0.5_0.1_nonlinear_par_Non_PC.Rdata", "out_24_0.5_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_24_1_2.5_nonlinear_par_PC.Rdata", "out_24_1_5_nonlinear_par_PC.Rdata", "out_24_1_10_nonlinear_par_PC.Rdata",
                "out_24_1_0.01_nonlinear_par_Non_PC.Rdata", "out_24_1_0.1_nonlinear_par_Non_PC.Rdata", "out_24_1_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_nonlinear_par_PC.Rdata", "out_24_2_5_nonlinear_par_PC.Rdata", "out_24_2_10_nonlinear_par_PC.Rdata",
                "out_24_2_0.01_nonlinear_par_Non_PC.Rdata", "out_24_2_0.1_nonlinear_par_Non_PC.Rdata", "out_24_2_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_0.5_2.5_nonlinear_par_PC.Rdata", "out_99_0.5_5_nonlinear_par_PC.Rdata", "out_99_0.5_10_nonlinear_par_PC.Rdata",
                "out_99_0.5_0.01_nonlinear_par_Non_PC.Rdata", "out_99_0.5_0.1_nonlinear_par_Non_PC.Rdata", "out_99_0.5_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_1_2.5_nonlinear_par_PC.Rdata", "out_99_1_5_nonlinear_par_PC.Rdata", "out_99_1_10_nonlinear_par_PC.Rdata",
                "out_99_1_0.01_nonlinear_par_Non_PC.Rdata", "out_99_1_0.1_nonlinear_par_Non_PC.Rdata", "out_99_1_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_nonlinear_par_PC.Rdata", "out_99_2_5_nonlinear_par_PC.Rdata", "out_99_2_10_nonlinear_par_PC.Rdata",
                "out_99_2_0.01_nonlinear_par_Non_PC.Rdata", "out_99_2_0.1_nonlinear_par_Non_PC.Rdata", "out_99_2_1_nonlinear_par_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_2.5_linear_semipar_PC.Rdata", "out_24_0.5_5_linear_semipar_PC.Rdata", "out_24_0.5_10_linear_semipar_PC.Rdata",
                "out_24_0.5_0.01_linear_semipar_Non_PC.Rdata", "out_24_0.5_0.1_linear_semipar_Non_PC.Rdata", "out_24_0.5_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_24_1_2.5_linear_semipar_PC.Rdata", "out_24_1_5_linear_semipar_PC.Rdata", "out_24_1_10_linear_semipar_PC.Rdata",
                "out_24_1_0.01_linear_semipar_Non_PC.Rdata", "out_24_1_0.1_linear_semipar_Non_PC.Rdata", "out_24_1_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_linear_semipar_PC.Rdata", "out_24_2_5_linear_semipar_PC.Rdata", "out_24_2_10_linear_semipar_PC.Rdata",
                "out_24_2_0.01_linear_semipar_Non_PC.Rdata", "out_24_2_0.1_linear_semipar_Non_PC.Rdata", "out_24_2_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_0.5_2.5_linear_semipar_PC.Rdata", "out_99_0.5_5_linear_semipar_PC.Rdata", "out_99_0.5_10_linear_semipar_PC.Rdata",
                "out_99_0.5_0.01_linear_semipar_Non_PC.Rdata", "out_99_0.5_0.1_linear_semipar_Non_PC.Rdata", "out_99_0.5_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_1_2.5_linear_semipar_PC.Rdata", "out_99_1_5_linear_semipar_PC.Rdata", "out_99_1_10_linear_semipar_PC.Rdata",
                "out_99_1_0.01_linear_semipar_Non_PC.Rdata", "out_99_1_0.1_linear_semipar_Non_PC.Rdata", "out_99_1_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_linear_semipar_PC.Rdata", "out_99_2_5_linear_semipar_PC.Rdata", "out_99_2_10_linear_semipar_PC.Rdata",
                "out_99_2_0.01_linear_semipar_Non_PC.Rdata", "out_99_2_0.1_linear_semipar_Non_PC.Rdata", "out_99_2_1_linear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_0.5_2.5_nonlinear_semipar_PC.Rdata", "out_24_0.5_5_nonlinear_semipar_PC.Rdata", "out_24_0.5_10_nonlinear_semipar_PC.Rdata",
                "out_24_0.5_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_0.5_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_0.5_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_24_1_2.5_nonlinear_semipar_PC.Rdata", "out_24_1_5_nonlinear_semipar_PC.Rdata", "out_24_1_10_nonlinear_semipar_PC.Rdata",
                "out_24_1_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_1_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_1_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_24_2_2.5_nonlinear_semipar_PC.Rdata", "out_24_2_5_nonlinear_semipar_PC.Rdata", "out_24_2_10_nonlinear_semipar_PC.Rdata",
                "out_24_2_0.01_nonlinear_semipar_Non_PC.Rdata", "out_24_2_0.1_nonlinear_semipar_Non_PC.Rdata", "out_24_2_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_0.5_2.5_nonlinear_semipar_PC.Rdata", "out_99_0.5_5_nonlinear_semipar_PC.Rdata", "out_99_0.5_10_nonlinear_semipar_PC.Rdata",
                "out_99_0.5_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_0.5_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_0.5_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - - 
                "out_99_1_2.5_nonlinear_semipar_PC.Rdata", "out_99_1_5_nonlinear_semipar_PC.Rdata", "out_99_1_10_nonlinear_semipar_PC.Rdata",
                "out_99_1_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_1_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_1_1_nonlinear_semipar_Non_PC.Rdata",
                #- - - - - - - - -
                "out_99_2_2.5_nonlinear_semipar_PC.Rdata", "out_99_2_5_nonlinear_semipar_PC.Rdata", "out_99_2_10_nonlinear_semipar_PC.Rdata",
                "out_99_2_0.01_nonlinear_semipar_Non_PC.Rdata", "out_99_2_0.1_nonlinear_semipar_Non_PC.Rdata", "out_99_2_1_nonlinear_semipar_Non_PC.Rdata")

#----------------------------

all.storage <- matrix(0, ncol = 6, nrow = 96)

for (j in 1:length(file.names))
{
  load(file.names[j])
  
  if ((j %% 18 == 0) | (j %% 18 > 12)){
    alpha.true = 2
  }else if((j %% 18 < 13) & (j %% 18)>6){
    alpha.true = 1
  }else{
    alpha.true = 0.5
  }
  
  mean.alpha <- var.alpha <- numeric(length = 1e3)
  prcntl.alpha.025 <- prcntl.alpha.975 <- numeric(length = 1e3)
  
  list_name <- substr(file.names[j], 1, nchar(file.names[j]) - 6)
  cur.list <- get(list_name)
  
  for (i in 1:1e3)
  {
    ret.final <- cur.list[[i]]
    fit.observed.GE <- ret.final[[1]]
    
    #alpha----
    mean.alpha[i] <- mean(fit.observed.GE$alpha)
    var.alpha[i] <- var(fit.observed.GE$alpha)
    prcntl.alpha.025[i] <- as.numeric(quantile(fit.observed.GE$alpha, 0.025))
    prcntl.alpha.975[i] <- as.numeric(quantile(fit.observed.GE$alpha, 0.975))
  }
  
  ####-------------------------------------------
  #Coverage probability of alpha
  (alpha.coverage <- round(mean((prcntl.alpha.025 < alpha.true) & (prcntl.alpha.975 > alpha.true)),3))
  
  #Absolute Bias:
  (alpha.abs.bias <- round(mean(abs(mean.alpha - alpha.true)),3))
  
  #Variance:
  (alpha.var <- round(mean(var.alpha),3))
  
  #MSE:
  (alpha.mse <- round(mean((mean.alpha - alpha.true)^2),3))
  
  #----------------------------------------------
  col <- ifelse(j %% 6 == 0, 6, j%%6) 
  row <- ((ceiling(j/6)-1)*4) + 1
  
  all.storage[row, col] <- alpha.coverage
  all.storage[(row+1), col] <- alpha.abs.bias
  all.storage[(row+2), col] <- alpha.var
  all.storage[(row+3), col] <- alpha.mse
  #----------------------------------------------
  rm(cur.list)
  rm(list = list_name)
  print(paste0("We finished setting ", j))
}


df <- data.frame(all.storage)

write.table(df, file = "00 Results_PC.csv", sep = ",",
            append = TRUE, quote = TRUE,
            col.names = FALSE, row.names = FALSE)
