library(ggplot2)
library(tidyverse)
library(pracma)
library(latex2exp)

#==================================================================================
##pdf:

ge.pdf.gg <- function(alpha, lambda, grid)
{
  y.val <- alpha*lambda * (1 - exp(-lambda*grid))^(alpha-1) * exp(-grid * lambda)
  return(y.val)
}

X <- seq(0.01,5,length.out = 501)
Y1 <- ge.pdf.gg(0.5,1,X)
Y2 <- ge.pdf.gg(1,1,X)
Y3 <- ge.pdf.gg(2,1,X)

df <- data.frame(X, Y1, Y2, Y3)
df_long <- df %>% pivot_longer(cols = -X, names_to = "Line", values_to = "Y")

ggplot(df_long, aes(x = X, y = Y, color = Line)) +
  geom_line(size = 1) +
  labs(x = "X", y = "Density") +
  ylim(0, 2) +
  guides(color = guide_legend(title = NULL)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position = c(0.77, 0.8)) +
  scale_color_manual(
    breaks = c("Y1", "Y2", "Y3"),
    labels = c(TeX(r"(    $\alpha$ = 0.5, $\lambda$ = 1)"),
               TeX(r"(    $\alpha$ = 1, $\lambda$ = 1   )"),
               TeX(r"(    $\alpha$ = 2, $\lambda$ = 1   )")),
    values = c("Y1" = "red", "Y2" = "green", "Y3" = "blue")
  )


#=========================================================================================================================
##hazard function:

ge.haz.gg <- function(alpha, lambda, grid)
{
  val <- 1 - exp(-lambda*grid)
  y.val <- alpha * lambda * val^(alpha-1) * exp(- lambda*grid) / (1 - val^alpha)
  return(y.val)
}

X <- seq(0.01,5,length.out = 501)
Y1 <- ge.haz.gg(0.5,1,X)
Y2 <- ge.haz.gg(1,1,X)
Y3 <- ge.haz.gg(2,1,X)

df <- data.frame(X, Y1, Y2, Y3)
df_long <- df %>% pivot_longer(cols = -X, names_to = "Line", values_to = "Y")

ggplot(df_long, aes(x = X, y = Y, color = Line)) +
  geom_line(size = 1) +
  labs(x = "X-axis", y = "Y-axis", color = "Line") +
  ylim(0,2) + labs(x = "X", y = "Hazard") +
  guides(color = guide_legend(title = NULL)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 17),
        legend.position = "none") +
  scale_color_manual(
    breaks = c("Y1", "Y2", "Y3"),
    labels = c(TeX(r"(    $\alpha$ = 0.5, $\lambda$ = 1)"),
               TeX(r"(    $\alpha$ = 1, $\lambda$ = 1   )"),
               TeX(r"(    $\alpha$ = 2, $\lambda$ = 1   )")),
    values = c("Y1" = "red", "Y2" = "green", "Y3" = "blue")
  )


 #=========================================================================================================================
##Mean

rep <- 301

alpha <- seq(0.5,2,length.out = rep)
lambda <- seq(0.5,2,length.out = rep)

col1 <- numeric()
col2 <- numeric()
col3 <- numeric()

for (i in 1:rep)
{
  for (j in 1:rep)
  {
    a <- alpha[i]
    l <- lambda[j]
    val <- (psi(0, a +1) - psi(0,1)) / l
    
    col1[(i-1)*rep + j] <- a
    col2[(i-1)*rep + j] <- l
    col3[(i-1)*rep + j] <- val
  }
}

df <- data.frame(col1, col2, col3)
names(df) <- c("alpha", "lambda", "Mean")

ggplot(data = df, aes(y=alpha, x=lambda, fill=Mean)) + 
  geom_raster() + scale_fill_viridis_c() +
  labs(x = TeX(r"($\alpha$ )"), y = TeX(r"($\lambda$ )")) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.position = c(0.77, 0.62))


#=========================================================================================================================
##Variance

rep <- 301

alpha <- seq(0.5,2,length.out = rep)
lambda <- seq(0.5,2,length.out = rep)

col1 <- numeric()
col2 <- numeric()
col3 <- numeric()
for (i in 1:rep)
{
  for (j in 1:rep)
  {
    a <- alpha[i]
    l <- lambda[j]
    val <- (psi(1,1) - psi(1, a +1)) / l^2
    
    col1[(i-1)*rep + j] <- a
    col2[(i-1)*rep + j] <- l
    col3[(i-1)*rep + j] <- val
  }
}
df <- data.frame(col1, col2, col3)
names(df) <- c("alpha", "lambda", "Variance")

ggplot(data = df, aes(y=alpha, x=lambda, fill=Variance)) + 
  geom_raster() + scale_fill_viridis_c() +
  labs(x = TeX(r"($\alpha$ )"), y = TeX(r"($\lambda$ )")) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.position = c(0.71, 0.62))

#=========================================================================================================================
##Skewness

alpha <- seq(0.01,2,length.out = 200)
Skewness <- (psi(2, alpha + 1) - psi(2, 1)) / (psi(1, 1) - psi(1,alpha + 1))^(3/2)

df <- data.frame(alpha, Skewness)

ggplot(data = df, aes(x = alpha, y = Skewness)) + 
  geom_line(size = 0.9) + 
  labs(x = TeX(r"($\alpha$ )"), y = "Skewness") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 17))
  