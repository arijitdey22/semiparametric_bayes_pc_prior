#put the frame at 9cm * 19cm

#####################################################
##          Effectiveness of PC prior
#####################################################

#=====================================================================================
##the coverage probability

library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(gridExtra)
library(grid)

rm(list = ls())

data <- read.csv("00 Results_PC.csv", header = F)

col.ind <- c(1,2,4,6)
row.ind <- seq(1,93, by = 4)

df1 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[1:3], col.ind] %>% as.vector()))

df2 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[4:6], col.ind] %>% as.vector()))

df3 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[7:9], col.ind] %>% as.vector()))

df4 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[10:12], col.ind] %>% as.vector()))

df5 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[13:15], col.ind] %>% as.vector()))

df6 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[16:18], col.ind] %>% as.vector()))

df7 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[19:21], col.ind] %>% as.vector()))

df8 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[22:24], col.ind] %>% as.vector()))


# Plotting
p1 <- ggplot(df1, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p2 <- ggplot(df2, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p3 <- ggplot(df3, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p4 <- ggplot(df4, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p5 <- ggplot(df5, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p6 <- ggplot(df6, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p7 <- ggplot(df7, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p8 <- ggplot(df8, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

combined_plot <- p1 + p3 + p5 + p7 + p2 + p4 + p6 + p8 + plot_layout(ncol = 4) + 
  plot_annotation(theme = theme(legend.position = "top")) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)

A <- wrap_elements(textGrob(TeX(r"(Coverage probability of $\alpha$ )"), rot = 90,
                                  x = 0.3, y = 0.47,
                                  gp = gpar(col = "black", fontsize = 15))) + combined_plot +
  plot_layout(ncol = 2, widths = c(0.1,2))


A / wrap_elements(grid::textGrob(TeX(r"($\alpha$ )"), x = 0.55, y = 0.27,
                                 gp = gpar(col = "black", fontsize = 15))) +
  plot_layout(nrow = 2, heights = c(2,0.15))


#=====================================================================================
##The absolute bias

library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(gridExtra)
library(grid)

rm(list = ls())

data <- read.csv("00 Results_PC_3dig.csv", header = F)

col.ind <- c(1,2,4,6)
row.ind <- seq(2,94, by = 4)

df1 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[1:3], col.ind] %>% as.vector()))

df2 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[4:6], col.ind] %>% as.vector()))

df3 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[7:9], col.ind] %>% as.vector()))

df4 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[10:12], col.ind] %>% as.vector()))

df5 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[13:15], col.ind] %>% as.vector()))

df6 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[16:18], col.ind] %>% as.vector()))

df7 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[19:21], col.ind] %>% as.vector()))

df8 <- data.frame(param = c("PC(2.5)", "PC(2.5)", "PC(2.5)", "PC(5)", "PC(5)", "PC(5)",
                            "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(0.01,0.01)", "Gamma(1,1)", "Gamma(1,1)", "Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[22:24], col.ind] %>% as.vector()))


# Plotting
p1 <- ggplot(df1, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p2 <- ggplot(df2, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p3 <- ggplot(df3, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12))

p4 <- ggplot(df4, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12))

p5 <- ggplot(df5, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p6 <- ggplot(df6, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14))

p7 <- ggplot(df7, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12))

p8 <- ggplot(df8, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + ylim(0,1.1) +
  scale_linetype_manual(values = c("Non-PC" = "solid", "PC" = "dotted")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12))

combined_plot <- p1 + p3 + p5 + p7 + p2 + p4 + p6 + p8 + plot_layout(ncol = 4) + 
  plot_annotation(theme = theme(legend.position = "none")) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)

A <- wrap_elements(grid::textGrob("Absolute Bias", rot = 90, x = 0.3, y = 0.47, 
                                  gp = gpar(fontsize = 17))) + combined_plot  +
  plot_layout(ncol = 2, widths = c(0.1,2))


A / wrap_elements(grid::textGrob(TeX(r"($\alpha$ )"), x = 0.55, y = 0.27, 
                                 gp = gpar(fontsize = 15))) +
  plot_layout(nrow = 2, heights = c(2,0.15))


#=====================================================================================
#####################################################
##    Effectiveness of semi parametric regression
#####################################################
library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(gridExtra)
library(grid)

rm(list = ls())

data <- read.csv("00 Results_Semipar.csv", header = F)

#=====================================================================================
##WAIC

col.ind.gam <- c(1,3,4,6)
col.ind.PC <- c(1,2,4,5)
row.ind <- seq(4,96, by = 4)

df1 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[1:3], col.ind.gam] %>% as.vector()))

df2 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[4:6], col.ind.gam] %>% as.vector()))

df3 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[7:9], col.ind.PC] %>% as.vector()))

df4 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[10:12], col.ind.PC] %>% as.vector()))

df5 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[13:15], col.ind.gam] %>% as.vector()))

df6 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[16:18], col.ind.gam] %>% as.vector()))

df7 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[19:21], col.ind.PC] %>% as.vector()))

df8 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[22:24], c(1,4)] %>% as.vector()))


# Plotting
p1 <- ggplot(df1, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) +
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6"))

p2 <- ggplot(df2, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) +
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6"))

p3 <- ggplot(df3, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p4 <- ggplot(df4, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p5 <- ggplot(df5, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) +
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6"))

p6 <- ggplot(df6, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) +
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6"))

p7 <- ggplot(df7, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p8 <- ggplot(df8, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

combined_plot <- p1 + p3 + p5 + p7 + p2 + p4 + p6 + p8 + plot_layout(ncol = 4) + 
  plot_annotation(theme = theme(legend.position = "none", 
                                legend.box = "vertical",  # Display the legend as two lines
                                legend.spacing.y = unit(-0.1, "lines"))) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)

A <- wrap_elements(grid::textGrob("WAIC", rot = 90, x = 0.3, y = 0.47, 
                                  gp = gpar(fontsize = 15))) + combined_plot  +
  plot_layout(ncol = 2, widths = c(0.1,2))


A / wrap_elements(grid::textGrob(TeX(r"($\alpha$ )"), x = 0.55, y = 0.3, 
                                 gp = gpar(fontsize = 15))) +
  plot_layout(nrow = 2, heights = c(2,0.16))


#=====================================================================================
##Absolute error

library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(gridExtra)
library(grid)

rm(list = ls())

data <- read.csv("00 Results_Semipar.csv", header = F)

col.ind.gam <- c(1,3,4,6)
col.ind.PC <- c(1,2,4,5)
row.ind <- seq(2,94, by = 4)

df1 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)",  "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)",  "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[1:3], col.ind.gam] %>% as.vector()))

df2 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)",  "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)",  "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[4:6], col.ind.gam] %>% as.vector()))

df3 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[7:9], col.ind.PC] %>% as.vector()))

df4 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[10:12], col.ind.PC] %>% as.vector()))

df5 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)",  "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)",  "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[13:15], col.ind.gam] %>% as.vector()))

df6 <- data.frame(param = c("Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)", "Parametric_Gamma(0.01,0.01)",
                            "Parametric_Gamma(1,1)",  "Parametric_Gamma(1,1)", "Parametric_Gamma(1,1)",
                            "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)", "Semiparametric_Gamma(0.01,0.01)",
                            "Semiparametric_Gamma(1,1)",  "Semiparametric_Gamma(1,1)", "Semiparametric_Gamma(1,1)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[16:18], col.ind.gam] %>% as.vector()))

df7 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                            "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                            "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                            "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[19:21], col.ind.PC] %>% as.vector()))

df8 <- data.frame(param = c("Parmetric_PC(2.5)", "Parmetric_PC(2.5)", "Parmetric_PC(2.5)",
                             "Parmetric_PC(5)", "Parmetric_PC(5)", "Parmetric_PC(5)",
                             "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)", "Semiparmetric_PC(2.5)",
                             "Semiparmetric_PC(5)", "Semiparmetric_PC(5)", "Semiparmetric_PC(5)"),
                  alpha = c("0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2", "0.5", "1", "2"),
                  cov.p = unlist(data[row.ind[22:24], col.ind.PC] %>% as.vector()))


# Plotting
p1 <- ggplot(df1, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6")) +
  guides(color = guide_legend(nrow = 2))

p2 <- ggplot(df2, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6")) +
  guides(color = guide_legend(nrow = 2)) + 
  scale_y_continuous(breaks = c(0.8, 1.2, 1.6))

p3 <- ggplot(df3, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p4 <- ggplot(df4, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p5 <- ggplot(df5, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6")) +
  guides(color = guide_legend(nrow = 2))

p6 <- ggplot(df6, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#4287f5", "#8f3fd0", "#79e36d", "#65bcd6")) +
  guides(color = guide_legend(nrow = 2))

p7 <- ggplot(df7, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

p8 <- ggplot(df8, aes(x = alpha, y = cov.p, group = param)) +
  geom_line(aes(color = param), lwd = 0.8) +
  geom_point(aes(color = param)) +
  labs(y = "", x = "", color = "") + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text=element_text(size=14)) + 
  scale_color_manual(values=c("#869681", "#34495e", "#ffb533", "#ff477e"))

combined_plot <- p1 + p3 + p5 + p7 + p2 + p4 + p6 + p8 + plot_layout(ncol = 4) + 
  plot_annotation(theme = theme(legend.position = "top", 
                                legend.box = "vertical",  # Display the legend as two lines
                                legend.spacing.y = unit(-0.1, "lines"))) +
  plot_layout(guides = "collect") & xlab(NULL) & ylab(NULL)


A <- wrap_elements(grid::textGrob("Absolute Fitting Error", rot = 90, x = 0.3, y = 0.47, 
                                  gp = gpar(fontsize = 15))) + combined_plot  +
  plot_layout(ncol = 2, widths = c(0.1,2))


A / wrap_elements(grid::textGrob(TeX(r"($\alpha$ )"), x = 0.55, y = 0.3, 
                                 gp = gpar(fontsize = 15))) +
  plot_layout(nrow = 2, heights = c(2,0.17))


