###############################################################################
# Creates one brownian motion.
###############################################################################
# load packages
library(ggplot2)
library(reshape2)
library(tikzDevice)

# load functions
source("R/bm_functions.R")


#### Parameters --------------------------------
# random seed
set.seed(14)

# last time value
time.max <- 1
# number of steps
n.steps <- 100

# t parameter for drift.
t <- 0.3


#### Create Brownian Motion --------------------------------
# size of one step
step.size <- time.max / n.steps
# values of steps
steps <- seq(0, time.max, by=step.size)

# standard brownian motion
increments <- rnorm(n.steps, mean = 0, sd = step.size)
bm <- c(0, cumsum(increments))
bm[1:5] <- abs(bm[1:5])

tikz('fig_bm_half.tex',standAlone = TRUE, width = 1.5,height = 1.5)
plot <- ggplot() +
  geom_line(aes(x=steps, y=bm)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept=0)) +
  theme_bw() +
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")
print(plot)
dev.off()
