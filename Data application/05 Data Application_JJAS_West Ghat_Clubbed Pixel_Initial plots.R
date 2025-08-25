rm(list = ls())

library(fda)
library(ggplot2)
library(univOutl)
library(pracma)
library(latex2exp)
library(coda)
library(viridis)
library(ggmap)
library(ggpubr)


##================================================================================
#Western Ghat region plot

library(ggmap)
library(ggplot2)

rm(list = ls())

load("00 Data_Store_Raw Cleaned Data.Rdata")

wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30
JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)

bdbox <- make_bbox(lon = c(70, 82), 
                   lat = c(21,7), f = .1)
india_map <- get_map(location = bdbox, zoom = 7, maptype = "terrain", source = "stamen")

ggmap(india_map)

X <- c(rep("Northern Region",5), rep("Middle Region",9), rep("Southern Region",7))
wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

df <- data.frame(x = wst.ght.loc[ , 1], y = wst.ght.loc[ , 2], data = as.character(X))

plot.reg <- ggmap(india_map) +
  geom_point(data = df, aes(x = x, y = y, color = data), size = 6.7, shape = 15, alpha = 0.65) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "none") + 
  scale_color_manual(
    name = "",
    values = c("Northern Region" = "red", "Middle Region" = "blue", "Southern Region" = "green")
  )

plot(plot.reg)


##================================================================================
#Other two sets of plots

rm(list = ls())
load("00 Data_Store_Raw Cleaned Data.Rdata")

wst.ght.loc.ind <- c(98,82,69,57,47,39,40,32,25,18,33,26,19,13,8,14,9,4,1,2,3)     #done manually
wst.ght.loc <- S[wst.ght.loc.ind, ]

jan2may.leap <- 31 + 29 + 31 + 30 + 31
jan2may.nonleap <- 31 + 28 + 31 + 30 + 31
JJAS.days <- 30 + 31 + 31 + 30
JJAS.leap <- (jan2may.leap + 1):(jan2may.leap + JJAS.days)
JJAS.nonleap <- (jan2may.nonleap + 1):(jan2may.nonleap + JJAS.days)

#----------------------------
#functions needed
sqdiff.mme <- function(theta, X){
  lambda <- exp(theta[1])
  nu <- exp(theta[2])
  out <- (sqrt(psi(1, 1) - psi(1, nu + 1)) / (psi(0, nu + 1) - psi(0, 1)) - sd(X) / mean(X))^2 +
    (lambda - (psi(0, nu + 1) - psi(0, 1)) / mean(X))^2
  out}

mme_ge <- function(X){
  sqdiff.out <- optim(c(0, 0), sqdiff.mme, X = X, hessian = F)
  exp(sqdiff.out$par)}

dge <- function(x, lambda, nu){
  lambda * nu * exp(-lambda * x) * (1 - exp(-lambda * x))^(nu - 1)}

#----------------------------

n.B.trend_fit <- 12

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
  
  cur.data <- numeric()
  covariate <- numeric()
  log.ann.avg <- numeric()
  
  for (t in 1:length(all.data))
  {
    if (t %% 4 == 0){
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.leap])
    }else{
      cur.year.data <- colMeans(all.data[[t]][wst.ght.loc.ind[clubbed.loc.ind], JJAS.nonleap])
    }
    
    cur.year.data.nz <- cur.year.data[!(cur.year.data == 0)]
    
    log.ann.avg[t] <- mean(log(cur.year.data.nz))
    cur.data <- c(cur.data, cur.year.data.nz)
    covariate <- c(covariate, rep((1900 + t), length(cur.year.data.nz)))
  }
  
  #==============================================================================
  #First Plot
  basis <-  create.bspline.basis(c(1901, 2022), nbasis = n.B.trend_fit)
  B.semipar.trend_fit <- eval.basis(1901:2022, basis)

  model2 <- lm(log.ann.avg ~ 0 + B.semipar.trend_fit)
  fit.param <- as.vector(model2$coefficients)
  fit.beta <- c(B.semipar.trend_fit %*% fit.param)

  #---------

  df.ann.ave <- data.frame(1901:2022, log.ann.avg, fit.beta)
  colnames(df.ann.ave) <- c("Year", "Avg_Ann_Rainfall", "fit_beta")

  plot.fitt.mean <- ggplot(df.ann.ave, aes(x = Year, y = Avg_Ann_Rainfall)) +
    geom_bar(stat = "identity") + coord_cartesian(ylim=c(0.8,NA)) +
    geom_line(aes(x=Year,
                  y = fit.beta),
              color = "red", lwd = 1.5) +
    labs(x = "Year", y = "Fitted Mean") +
    ggtitle(title) +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 23),
          plot.title = element_text(size = 30, hjust = 0.5)) +
    scale_x_continuous(breaks = c(1910, 1935, 1960, 1985, 2010))

  plot(plot.fitt.mean)
  
  #==============================================================================
  #Second Plot

  basis <-  create.bspline.basis(c(1901, 2022), nbasis = n.B.trend_fit)
  B.semipar.trend_fit <- eval.basis(covariate, basis)

  model2 <- lm(log(cur.data) ~ 0 + B.semipar.trend_fit)
  res.model2 <- exp(model2$residuals)

  #---------------------
  #removing outlier
  out.res <- boxB(model2$residuals, method = "adjbox")
  out.ind<- out.res$outliers
  res.model2 <- res.model2[-out.ind]

  #--------------------
  #estimating parameters
  est.ge <- mme_ge(res.model2)
  lambda.est <- est.ge[1]
  alpha.est <- est.ge[2]


  #--------
  #creating histogram and density plot
  plot.hist.dens <- ggplot() + geom_histogram(aes(x = res.model2, y = ..density..),
                                            color = "black",
                                            breaks = hist(res.model2, plot = F)$breaks) +
   ylab("Density") + xlab("Daily Rainfall (in mm)") +
    ggtitle(title) +
   geom_line(aes(x=seq(0,15,0.01),
                  y = dge(seq(0,15,0.01),lambda.est, alpha.est )),
               color = "red", lwd = 1.2) +
    theme(axis.title = element_text(size = 25),
         axis.text = element_text(size = 23),
         plot.title = element_text(hjust = 0.5, size = 30))

  plot(plot.hist.dens)
  
  
}

