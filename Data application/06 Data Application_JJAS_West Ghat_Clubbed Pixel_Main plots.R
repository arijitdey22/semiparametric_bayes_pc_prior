library(fda)
library(ggplot2)
library(univOutl)
library(pracma)
library(latex2exp)
library(coda)
library(patchwork)
library(cowplot)
library(tidyverse)
library(gridExtra)
library(grid)
#library(splines2)

##================================================================================
#Getting alpha estimates and visualizing them for all the locations
library(bayesplot)
library(ggplot2)

rm(list = ls())

load("00 Data_Store_Raw Cleaned Data.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_semipar.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_par.Rdata")
load("02 Data_Store_WAIC_best.pair.Rdata")


alpha.est.store <- numeric()
alpha.chain.store.semipar <- list()
alpha.chain.store.par <- list()
plot.trace.alpha.all <- list()

for (i in 1:length(all.chain.store.clubbed.semipar))
{
  alpha.chain.store.semipar[[i]] <- all.chain.store.clubbed.semipar[[i]][ , 1]
  alpha.chain.store.par[[i]] <- all.chain.store.clubbed.par[[i]][ , 1]
  
  alpha.est.store[i] <- mean(alpha.chain.store.semipar[[i]])
}

alpha.est.store

#----

mat1 <- matrix(c(alpha.chain.store.semipar[[1]],
                 alpha.chain.store.semipar[[2]],
                 alpha.chain.store.semipar[[3]]), ncol = 3)
mat2 <- matrix(c(alpha.chain.store.par[[1]],
                 alpha.chain.store.par[[2]],
                 alpha.chain.store.par[[3]]), ncol = 3)

colnames(mat1) <- c("Northern Region", "Middle Region", "Southern Region")
colnames(mat2) <- c("Northern Region", "Middle Region", "Southern Region")

foo1 <- list(mat1)
foo2 <- list(mat2)

trace.plot.1 <- mcmc_trace(foo1)
trace.plot.2 <- mcmc_trace(foo2)

trace.plot.1 + 
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 18),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    legend.position = "none"
  ) + 
  xlab("Iteration (After burn−in)") + 
  ylab(TeX(r"($\alpha$ )"))

trace.plot.2 + 
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 18),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    legend.position = "none"
  ) + 
  xlab("Iteration (After burn−in)") + 
  ylab(TeX(r"($\alpha$ )"))


#----

round(alpha.est.store, 3)


##================================================================================
##theta vs WAIC plots
rm(list = ls())
load("02 Data_Store_WAIC_best.pair.Rdata")

(reg.N.best.pair <- theta.P.pairs[which(WAIC.northern == min(WAIC.northern)), ])

(reg.M.best.pair <- theta.P.pairs[which(WAIC.middle == min(WAIC.middle)), ])

(reg.S.best.pair <- theta.P.pairs[which(WAIC.southern == min(WAIC.southern)), ])

theta.vec <- seq(0.5,5,0.5)

#-- -- -- -- -- -- -- -- --
#Northern Region
opt.K.N <- as.numeric(reg.N.best.pair[2])
#- - - - - -
#changed into
opt.K.N <- 12
#- - - - - -
WAIC.vec.N <- WAIC.northern[which(theta.P.pairs[,2] == opt.K.N)]
WAIC.vec.N.cen <- WAIC.vec.N - mean(WAIC.vec.N)
round(WAIC.vec.N.cen, 3)

df.theta.vs.WAIC.N <- data.frame(theta.vec, WAIC.vec.N)
colnames(df.theta.vs.WAIC.N) <- c("theta", "WAIC")

plot.th.vs.W.N <- ggplot(data = df.theta.vs.WAIC.N, aes(x = theta, y = WAIC)) + 
  geom_line(lwd = 0.8, col = "#65bcd6") +
  geom_point(col = "#036ffc") +
  labs(x = TeX(r"($\theta$ )"), y = "WAIC") +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 17, angle = 60, vjust = 0.5, hjust = 0.5))  +  
  scale_y_continuous(breaks = c(115221, 115225, 115229),
                     limits = c(115220, 115230))

plot(plot.th.vs.W.N)

#-- -- -- -- -- -- -- -- --
#Middle region

opt.K.M <- as.numeric(reg.M.best.pair[2])
#- - - - - -
#changed into
opt.K.M <- 12
#- - - - - -
WAIC.vec.M <- WAIC.middle[which(theta.P.pairs[,2] == opt.K.M)]
WAIC.vec.M.cen <- WAIC.vec.M - mean(WAIC.vec.M)
round(WAIC.vec.M.cen, 3)

df.theta.vs.WAIC.M <- data.frame(theta.vec, WAIC.vec.M)
colnames(df.theta.vs.WAIC.M) <- c("theta", "WAIC")

plot.th.vs.W.M <- ggplot(data = df.theta.vs.WAIC.M, aes(x = theta, y = WAIC)) + 
  geom_line(lwd = 0.8, col = "#65bcd6") +
  geom_point(col = "#036ffc") +
  labs(x = TeX(r"($\theta$ )"), y = "WAIC") +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 17, angle = 60, vjust = 0.5, hjust = 0.5))  + 
  scale_y_continuous(breaks = c(116431, 116435, 116439),
                     limits = c(116430, 116440))

plot(plot.th.vs.W.M)

#-- -- -- -- -- -- -- -- --
#Southern region

opt.K.S <- as.numeric(reg.S.best.pair[2])
#- - - - - -
#changed into
opt.K.S <- 12
#- - - - - -
WAIC.vec.S <- WAIC.southern[which(theta.P.pairs[,2] == opt.K.S)]
WAIC.vec.S.cen <- WAIC.vec.S - mean(WAIC.vec.S)
round(WAIC.vec.S.cen, 3)

df.theta.vs.WAIC.S <- data.frame(theta.vec, WAIC.vec.S)
colnames(df.theta.vs.WAIC.S) <- c("theta", "WAIC")

plot.th.vs.W.S <- ggplot(data = df.theta.vs.WAIC.S, aes(x = theta, y = WAIC)) + 
  geom_line(lwd = 0.8, col = "#65bcd6") + #ylim(115220,115230) +
  geom_point(col = "#036ffc") +
  labs(x = TeX(r"($\theta$ )"), y = "WAIC") +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 17, angle = 60, vjust = 0.5, hjust = 0.5))  + 
  scale_y_continuous(breaks = c(95814, 95818, 95822),
                     limits = c(95813, 95823))

plot(plot.th.vs.W.S)

#-- -- -- -- -- -- -- -- --

combined_plot <- plot.th.vs.W.N + plot.th.vs.W.M + plot.th.vs.W.S + plot_layout(ncol = 3) + 
  plot_annotation(theme = theme(legend.position = "top")) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)

A <- wrap_elements(grid::textGrob("WAIC", x = 0.3, y = 0.57, rot = 90, 
                                  gp = gpar(fontsize = 18))) + combined_plot  +
  plot_layout(ncol = 2, widths = c(0.15,2))


A / wrap_elements(grid::textGrob(TeX(r"($\theta$ )"), x = 0.55, y = 0.7, 
                                 gp = gpar(fontsize = 19))) +
  plot_layout(nrow = 2, heights = c(2,0.33))



##================================================================================
#data for posterior density under different theta

library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(gridExtra)
library(grid)

rm(list = ls())
load("07 Post.vs.Theta.all.store.Rdata")

post.vs.theta.all.plot <- list()

for (i in 1:3)
{
  if (i == 1){
    title = "Northern Region"
  }else if (i == 2) {
    title = "Middle Region"
  }else{
    title = "Southern Region"
  }
  
  cur.reg.lis <- post.vs.theta.all.store[[i]]
  
  type.vec <- c(rep("theta.2", 3000), rep("theta.4", 3000), rep("theta.6", 3000),
                rep("theta.8", 3000), rep("theta.10", 3000))
  alpha.vec <- c(cur.reg.lis[[1]], cur.reg.lis[[2]], cur.reg.lis[[3]],
                 cur.reg.lis[[4]], cur.reg.lis[[5]])
  
  
  df <- data.frame(alpha.vec, type.vec)
  
  plot.post.vs.theta <- ggplot(df, aes(alpha.vec, color = type.vec)) +
    geom_density(adjust = 5, lwd = 0.8, alpha = 0.1) + 
    labs(x = TeX(r"($\alpha$ )"), y = "Density") +
    ggtitle(title) + 
    theme(legend.title=element_blank(),
          legend.position = "bottom", 
          legend.text = element_text(size=18),
          axis.title = element_text(size = 20), 
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5)) +
    scale_color_discrete(
      breaks = c("theta.2", "theta.4", "theta.6", "theta.8", "theta.10"),
      labels = c(TeX(r"(    $\theta$ = 2)"),
                 TeX(r"(    $\theta$ = 4)"),
                 TeX(r"(    $\theta$ = 6)"),
                 TeX(r"(    $\theta$ = 8)"),
                 TeX(r"(    $\theta$ = 10)")))
  
  if (i == 1)
  {
    plot.post.vs.theta = plot.post.vs.theta + 
      scale_x_continuous(breaks = c(0.835, 0.86, 0.885))
  }
  
  post.vs.theta.all.store[[i]] <- plot.post.vs.theta
}


p1 <- post.vs.theta.all.store[[1]]
p2 <- post.vs.theta.all.store[[2]]
p3 <- post.vs.theta.all.store[[3]]

combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3) + 
  plot_annotation(theme = theme(legend.position = "bottom")) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)

A <- wrap_elements(grid::textGrob("Density", rot = 90, x = 0.3, y = 0.47, 
                                  gp = gpar(fontsize = 18))) + combined_plot  +
  plot_layout(ncol = 2, widths = c(0.1,2))

A


# A / wrap_elements(grid::textGrob(TeX(r"($\alpha$ )"), x = 0.55, y = 0.7, 
#                                  gp = gpar(fontsize = 18))) +
#   plot_layout(nrow = 2, heights = c(2,0.3))



#====================================================================================
#fitting plots and other necessary quantities

rm(list = ls())

load("00 Data_Store_Raw Cleaned Data.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_semipar.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_par.Rdata")
load("02 Data_Store_WAIC_best.pair.Rdata")

wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30
JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)

mu.1901.vec <- numeric()
mu.2022.vec <- numeric()
mu.avg.vec <- numeric()
alpha.est.var <- numeric()
alpha.est.mean <- numeric()
mse.pred <- numeric()

for (i in 1:3)
{
  
  if (i == 1){
    clubbed.loc.ind <- 1:5
    title <- "Northern Region"
  }else if (i == 2){
    clubbed.loc.ind <- 6:14
    title <- "Middle Region"
  }else{
    clubbed.loc.ind <- 15:21
    title <- "Southern Region"
  }
  n.B.MCMC_fit <- 12
  
  #response values and the covariates
  annual.av <- numeric()
  year <- numeric()
  
  for (t in 1:length(all.data))
  {
    if (t %% 4 == 0){
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.leap])
    }else{
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.nonleap])
    }
    
    cur.year.data.nz <- cur.year.data[!(cur.year.data == 0)]

    annual.av <- c(annual.av, mean(cur.year.data.nz))
    year <- c(year, 1900+t)
  }
  
  #---------------------------
  #Semi parametric set up
  #---------------------------
  
  cur.MCMC.sp <- all.chain.store.clubbed.semipar[[i]]
  alpha.chain.sp <- cur.MCMC.sp[, 1]
  beta.chain.sp <- cur.MCMC.sp[, -1]
  
  #band and lambda
  X <- 1901:2022
  basis <-  create.bspline.basis(c(1901,2022), nbasis = n.B.MCMC_fit)
  B.fit.sp <- eval.basis(X, basis)
  
  mean.val.sp <- numeric(length = length(X))
  prcntl.025.val.sp <- numeric(length = length(X))
  prcntl.975.val.sp <- numeric(length = length(X))
  
  lambda.vec.chain.sp <- t(exp(B.fit.sp %*% t(beta.chain.sp)))
  
  psi.alpha.sp <- numeric(length = length(alpha.chain.sp))
  
  for (j in 1:length(alpha.chain.sp))
  {
    psi.alpha.sp[j] <- psi(0, (alpha.chain.sp[j] + 1))
  }
  
  for (ii in 1:length(X))
  {
    lam.vec.sp <- as.vector(lambda.vec.chain.sp[,ii])
    vals.sp <- ( (psi.alpha.sp - psi(0,1))  / lam.vec.sp )
    
    mean.val.sp[ii] <- mean(vals.sp)
    prcntl.025.val.sp[ii] <- as.numeric(quantile(vals.sp, 0.025))
    prcntl.975.val.sp[ii] <- as.numeric(quantile(vals.sp, 0.975))
  }
  
  #-- -- -- -- 
  #storing necessary values (only for Semi parametric)
  mu.1901.vec[i] <- mean.val.sp[1]
  mu.2022.vec[i] <- mean.val.sp[122]
  mu.avg.vec[i] <- mean(mean.val.sp)
  
  alpha.est.mean[i] <- mean(alpha.chain.sp)
  alpha.est.var[i] <- var(alpha.chain.sp)
  
  
  #---------------------------
  #parametric set up
  #---------------------------
  
  cur.MCMC.p <- all.chain.store.clubbed.par[[i]]
  alpha.chain <- cur.MCMC.p[, 1]
  beta.chain.p <- cur.MCMC.p[, -1]
  
  #band and lambda
  X <- 1901:2022
  X.scaled <- scale(X, center = T, scale = T)
  B.fit.p <- matrix(c(rep(1, length(X.scaled)), X.scaled), ncol = 2)
  
  mean.val.p <- numeric(length = length(X.scaled))
  prcntl.025.val.p <- numeric(length = length(X.scaled))
  prcntl.975.val.p <- numeric(length = length(X.scaled))
  
  lambda.vec.chain.p <- t(exp(B.fit.p %*% t(beta.chain.p)))
  
  psi.alpha.p <- numeric(length = length(alpha.chain))
  for (j in 1:length(alpha.chain))
  {
    psi.alpha.p[j] <- psi(0, (alpha.chain[j] + 1))
  }
  
  for (ii in 1:length(X.scaled))
  {
    lam.vec.p <- as.vector(lambda.vec.chain.p[,ii])
    vals.p <- ( (psi.alpha.p - psi(0,1))  / lam.vec.p )
    
    mean.val.p[ii] <- mean(vals.p)
    prcntl.025.val.p[ii] <- as.numeric(quantile(vals.p, 0.025))
    prcntl.975.val.p[ii] <- as.numeric(quantile(vals.p, 0.975))
  }
  
  #---------------------------
  #plots
  
  p <- ggplot() +
    geom_bar(aes(x = year, y = annual.av), stat = "identity") +
    geom_line(aes(x = X, y = mean.val.sp), color = 'Red', lwd = 1) +
    geom_ribbon(aes(x= X, ymin = prcntl.025.val.sp, 
                    ymax = prcntl.975.val.sp), fill = 'blueviolet', alpha = 0.3) +
    geom_line(aes(x = X, y = mean.val.p), col = 'blue', lwd = 1) +
    geom_ribbon(aes(x= X, ymin = prcntl.025.val.p, 
                    ymax = prcntl.975.val.p), fill = 'cyan3', alpha = 0.3) +
    labs(col = '',
         x = 'Year', 
         y = 'Daily Average Rainfall') +
    theme(axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 17),
          axis.title.x = element_text(size = 21)) +
    scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  coord_cartesian(ylim=c(5.5,NA))
  
  
  plot(p)
  
  mse.pred[i] <- sqrt(mean((annual.av-mean.val.sp)^2))
  
}

#-------------------------------
#alpha estimates and corresponding variances
round(alpha.est.mean, 3)
round(sqrt(alpha.est.var), 4)

#-------------------------------
####decadal change
round(mu.1901.vec,3)
round(mu.2022.vec,3)
round((mu.2022.vec - mu.1901.vec)/12.1, 3)


#====================================================================================
#Rate-of-change plots

library(pracma)
library(fda)
library(ggplot2)

rm(list = ls())

#keep this in the working directory 
load("04 Data_Store_MCMC Chains_clubbed_semipar.Rdata")

for (i in 1:3)
{
  
  if (i == 1){
    title <- "Northern Region"
  }else if (i == 2){
    title <- "Middle Region"
  }else{
    title <- "Southern Region"
  }
  
  ###MCMC chains
  cur.MCMC.sp <- all.chain.store.clubbed.semipar[[i]]
  alpha.chain.sp <- cur.MCMC.sp[, 1]
  beta.chain.sp <- cur.MCMC.sp[, -1]
  
  #calculating chain of psi values for chains of alpha
  psi.alpha.sp <- numeric(length = length(alpha.chain.sp))
  for (j in 1:length(alpha.chain.sp))
  {
    psi.alpha.sp[j] <- psi(0, (alpha.chain.sp[j] + 1))
  }
  
  #basis function and it's derivative
  X <- 1901:2022
  basis <-  create.bspline.basis(c(1901,2022), nbasis = 12)
  
  B.fit.sp <- eval.basis(X, basis)
  B.deriv.sp <- eval.basis(X, basis, Lfdobj = 1)
  
  #lambda chains
  lambda.vec.chain.deriv <- t(B.deriv.sp %*% t(beta.chain.sp))
  lambda.vec.chain.sp <- t(exp(B.fit.sp %*% t(beta.chain.sp)))
  
  #storage
  rate.of.change.cur.mean <- numeric()
  rate.of.change.cur.975 <- numeric()
  rate.of.change.cur.025 <- numeric()
  
  #for loop for each year
  for (ii in 1:length(X))
  {
    lam.vec.sp <- as.vector(lambda.vec.chain.sp[ ,ii])
    first.term.vec <- - ( psi.alpha.sp - psi(0,1) ) / lam.vec.sp^2
    
    second.term.vec <- lambda.vec.chain.deriv[, ii]
    
    foo <- first.term.vec * second.term.vec
    
    rate.of.change.cur.mean[ii] <- mean(foo)
    rate.of.change.cur.025[ii] <- as.numeric(quantile(foo, 0.025))
    rate.of.change.cur.975[ii] <- as.numeric(quantile(foo, 0.975))
    
  }
  
  #plot
  plot.change <- ggplot() +
    geom_line(aes(x = X, y = rate.of.change.cur.mean), col = 'red', lwd = 1) +
    geom_hline(yintercept = 0, lwd = 0.8) +
    geom_ribbon(aes(x= X, ymin = rate.of.change.cur.025,
                    ymax = rate.of.change.cur.975), fill = 'blueviolet', alpha = 0.3) +
    labs(x = "Year", y = "Rate of change") + 
    ggtitle(title) +
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size = 24),
          plot.title = element_text(hjust = 0.5, size = 25))  +
    scale_x_continuous(breaks = c(1900, 1930, 1960, 1990, 2020)) #+ ylim(-10,10)
  
  plot(plot.change)
  
}



#====================================================================================
#KLD plots

rm(list = ls())

load("00 Data_Store_Raw Cleaned Data.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_semipar.Rdata")
load("04 Data_Store_MCMC Chains_clubbed_par.Rdata")
load("02 Data_Store_WAIC_best.pair.Rdata")

my.ecdf  <-  function(x)   {
  x   <-   sort(x)
  x.u <-   unique(x)
  n  <-  length(x) 
  x.rle  <-  rle(x)$lengths
  y  <-  (cumsum(x.rle)-0.5) / n
  FUN  <-  approxfun(x.u, y, method="linear", yleft=0, yright=1,
                     rule=2)
  FUN
}

KL_est  <-  function(x, y)   {
  dx  <-  diff(sort(unique(x)))
  dy  <-  diff(sort(unique(y)))
  ex  <-  min(dx) ; ey  <-  min(dy)
  e   <-  min(ex, ey)/2
  n   <-  length(x)    
  P  <-   my.ecdf(x) ; Q  <-  my.ecdf(y)
  KL  <-  sum( log( (P(x)-P(x-e))/(Q(x)-Q(x-e)))) / n
  KL              
}

reg.N.best.pair <- theta.P.pairs[which(WAIC.northern == min(WAIC.northern)), ]
reg.M.best.pair <- theta.P.pairs[which(WAIC.middle == min(WAIC.middle)), ]
reg.S.best.pair <- theta.P.pairs[which(WAIC.southern == min(WAIC.southern)), ]

wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30
JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)

KLD.sp.all <- list()
KLD.p.all <- list()

for (i in 1:3)
{
  
  if (i == 1){
    clubbed.loc.ind <- 1:5
    n.B.MCMC_fit <- as.numeric(reg.N.best.pair[2])
  }else if (i == 2){
    clubbed.loc.ind <- 6:14
    n.B.MCMC_fit <- as.numeric(reg.M.best.pair[2])
  }else{
    clubbed.loc.ind <- 15:21
    n.B.MCMC_fit <- as.numeric(reg.S.best.pair[2])
  }
  n.B.MCMC_fit <- 12
  
  KLD.sp.cur <- numeric()
  KLD.p.cur <- numeric()
  
  cur.MCMC.sp <- all.chain.store.clubbed.semipar[[i]]
  alpha.chain.sp <- cur.MCMC.sp[, 1]
  beta.chain.sp <- cur.MCMC.sp[, -1]
  
  cur.MCMC.p <- all.chain.store.clubbed.par[[i]]
  alpha.chain.p <- cur.MCMC.p[, 1]
  beta.chain.p <- cur.MCMC.p[, -1]
  
  X <- 1901:2022
  basis <-  create.bspline.basis(c(1901,2022), nbasis = n.B.MCMC_fit)
  B.fit.sp <- eval.basis(X, basis)
  X.scaled <- scale(X, center = T, scale = T)
  B.fit.p <- matrix(c(rep(1, length(X.scaled)), X.scaled), ncol = 2)
  
  for (t in 1:length(all.data))
  {
    
    ####sample values
    if (t %% 4 == 0){
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.leap])
    }else{
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.nonleap])
    }
    
    cur.year.data.nz <- cur.year.data[!(cur.year.data == 0)]
    
    ####population values
    #-------------------
    #semiparametric
    lambda.chain.sp <- as.vector(exp(   beta.chain.sp %*% B.fit.sp[t,]  ))
  
    U.sp <- runif(length(alpha.chain.sp))
    pop.samp.sp <- - (  log(1 - U.sp ^ alpha.chain.sp)   ) / lambda.chain.sp
    
    #KLD es
    KLD.sp.cur[t] <- KL_est(cur.year.data.nz, pop.samp.sp)
    
    #-------------------
    #parametric
    lambda.chain.p <- as.vector(exp(   beta.chain.p %*% B.fit.p[t,]  ))
    
    U.p <- runif(length(alpha.chain.p))
    pop.samp.p <- - (  log(1 - U.p ^ alpha.chain.p)   ) / lambda.chain.p
    
    #KLD es
    KLD.p.cur[t] <- KL_est(cur.year.data.nz, pop.samp.p)
    
  }

  KLD.sp.all[[i]] <- KLD.sp.cur
  KLD.p.all[[i]] <- KLD.p.cur 
}

mean(KLD.sp.all[[1]] > KLD.p.all[[1]] )
mean(KLD.sp.all[[2]] > KLD.p.all[[2]] )
mean(KLD.sp.all[[3]] > KLD.p.all[[3]] )


#=================================================================================
#probability rainfall plot

library(ggplot2)
library(fda)

rm(list = ls())

load("04 Data_Store_MCMC Chains_clubbed_semipar.Rdata")

for (i in 1:3)
{
  if (i == 1){
    title <- "Northern Region"
  }else if(i == 2){
    title <- "Middle Region"
  }else{
    title <- "Southern Region"
  }
  
  cur.MCMC.sp <- all.chain.store.clubbed.semipar[[i]]
  alpha.chain.sp <- cur.MCMC.sp[, 1]
  beta.chain.sp <- cur.MCMC.sp[, -1]
  
  n.B.MCMC_fit <- 12
  X <- 1901:2022
  basis <-  create.bspline.basis(c(1901,2022), nbasis = n.B.MCMC_fit)
  B.fit.sp <- eval.basis(X, basis)
  
  val.3.025 <- val.3.mean <- val.3.975 <- numeric()
  val.5.025 <- val.5.mean <- val.5.975 <- numeric()
  val.7.025 <- val.7.mean <- val.7.975 <- numeric()
  
  for (t in 1:nrow(B.fit.sp))
  {
    lambda.chain.sp <- as.vector(exp(   beta.chain.sp %*% B.fit.sp[t,]  ))
    
    vals.3 <- - (  log(1 - 0.7^alpha.chain.sp)  ) / lambda.chain.sp
    vals.5 <- - (  log(1 - 0.5^alpha.chain.sp)  ) / lambda.chain.sp
    vals.7 <- - (  log(1 - 0.3^alpha.chain.sp)  ) / lambda.chain.sp
    
    val.3.025[t] <- as.numeric(quantile(vals.3, 0.025))
    val.3.mean[t] <- mean(vals.3)
    val.3.975[t] <- as.numeric(quantile(vals.3, 0.975))
    
    val.5.025[t] <- as.numeric(quantile(vals.5, 0.025))
    val.5.mean[t] <- mean(vals.5)
    val.5.975[t] <- as.numeric(quantile(vals.5, 0.975))
    
    val.7.025[t] <- as.numeric(quantile(vals.7, 0.025))
    val.7.mean[t] <- mean(vals.7)
    val.7.975[t] <- as.numeric(quantile(vals.7, 0.975))
      
  }
  
  ###plots
  p <- ggplot() +
    geom_line(aes(x = X, y = val.3.mean), col = 'Red', lwd = 1) +
    geom_ribbon(aes(x= X, ymin = val.3.025, 
                    ymax = val.3.975), fill = 'blueviolet', alpha = 0.2) +
    geom_line(aes(x = X, y = val.5.mean), col = 'blue', lwd = 1) +
    geom_ribbon(aes(x= X, ymin = val.5.025, 
                    ymax = val.5.975), fill = 'cyan3', alpha = 0.2) +
    geom_line(aes(x = X, y = val.7.mean), col = 'Green', lwd = 1) +
    geom_ribbon(aes(x= X, ymin = val.7.025, 
                    ymax = val.7.975), fill = 'aquamarine3', alpha = 0.2) +
    labs(col = '',
         x = 'Year', 
         y = 'Probability Rainfall') + ggtitle(title) +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 20),
          plot.title = element_text(hjust = 0.5, size = 28)) +
    scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020))
  
  plot(p)

}

