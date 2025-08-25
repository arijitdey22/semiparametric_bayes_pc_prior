library(ggplot2)
library(tidyverse)
library(latex2exp)

PC_PDF <- function(grid, theta)
{
  n <- length(grid)
  y.axis <- numeric(length = n)
  t <- theta
  for (i in 1:n)
  {
    a <- grid[i]
    y.axis[i] <- (t/2) * exp(-t * sqrt(2*log(a) + 2/a - 2) ) * 
      (2*log(a) + 2/a - 2)^(-0.5) * abs(1/a - 1/a^2)
  }
  return(y.axis)
}


#########################################################################################
###                 Plotting the PC prior pdf 
#########################################################################################

X <- seq(0,50,by = 0.001)
Y0 <- PC_PDF(X, 1)
Y1 <- PC_PDF(X, 2.5)
Y2 <- PC_PDF(X, 5)
Y3 <- PC_PDF(X, 10)

df <- data.frame(X, Y0, Y1, Y2, Y3)
df_long <- df %>% pivot_longer(cols = -X, names_to = "Line", values_to = "Y")

ggplot(df_long, aes(x = X, y = Y, color = Line)) +
  geom_line(size = 0.9) +
  labs(x = "X-axis", y = "Y-axis", color = "Line") +
  ylim(0,5) + xlim(0,4) + labs(x = TeX(r"( $\alpha$ )"), y = TeX(r"( $\pi(\alpha)$ )")) +
  guides(color = guide_legend(title = NULL)) +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 13),
        legend.text = element_text(size=17),
        legend.position = c(0.75, 0.75)) +
  scale_color_discrete(
    breaks = c("Y0", "Y1", "Y2", "Y3"),
    labels = c(TeX(r"(  $\theta$ = 1   )"),
               TeX(r"(  $\theta$ = 2.5)"),
               TeX(r"(  $\theta$ = 5   )"),
               TeX(r"(  $\theta$ = 10 )")))


#########################################################################################
###                 getting modal values 
#########################################################################################

alpha.grid <- seq(0.95, 1.05, by = 0.0001)
alpha.grid <- alpha.grid[-which(alpha.grid == 1)]
theta.cur <- 1.33

alpha.max <- 0

while(alpha.max != 1)
{
  theta.cur <- theta.cur + 0.000001
  
  pdf.vec <- PC_PDF(alpha.grid, theta.cur)
  alpha.max.cur <- alpha.grid[which(pdf.vec == max(pdf.vec))]

  print(paste0("We are at theta = ", theta.cur))
  if (alpha.max.cur == 0.9999)
  {
    alpha.max = 1  
    print(theta.cur)
  }
}
