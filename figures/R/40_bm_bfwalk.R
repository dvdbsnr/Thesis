###############################################################################
# Like bm_simulation, but plots the graphs using TikZ.
###############################################################################
# load packages
library(ggplot2)
library(reshape2)
library(tikzDevice)
library(dplyr)

# load functions
source("R/bm_functions.R")


#### Parameters --------------------------------
# random seed
set.seed(1)

# last time value
time.max <- 5
# number of steps
n.steps <- 400


# t parameter for positive drift.
tpos <- 1.2
# t parameter for negative drift.
tneg <- -1.2


#### Create Brownian Motion positive t --------------------------------

# size of one step
step.size <- time.max / n.steps
# values of steps
steps <- seq(0, time.max, by=step.size)

# standard brownian motion
increments <- rnorm(n.steps, mean = 0, sd = sqrt(step.size))
bm <- c(0, cumsum(increments))

# all brownian motions in one dataframe
df <- data.frame(
  step = steps,
  bm = 1.7*bm
)

plot <- ggplot(df,
               aes(x = step, y = bm)) +
  geom_line()
plot(plot)


# Write BM-points to txt file ---------------------------------------------

string <- "(0,0) "
for(i in 1:nrow(df)){
  string <- paste0(
    string, "(", i*step.size, ",", round(df$bm[i], 3), ") "
  )
}

write(string, 
      file = "output/bm_points.txt")



