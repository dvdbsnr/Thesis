###############################################################################
# Simulates a standard brownian motion in discrete steps
# Each increment with normal distribution, mean 0, variance 1
#
# Simulates a brownian motion with drift t-s
# bm.drift(s) = bm(s) + t*s -0.5*s^2
#
# Simulates brownian motion with drift with reflection at 0
# bm.ref(s) = bm.drift(s) - min(bm.drift(s'), s' <= s)
###############################################################################
# load packages
library(ggplot2)
library(reshape2)

# load functions
source("R/bm_functions.R")


## parameters
# last time value
time.max <- 2
# number of steps
n.steps <- 1000


## calculate step sizes
# size of one step
step.size <- time.max / n.steps
# values of steps
steps <- seq(0, time.max, by=step.size)


# standard brownian motion
increments <- rnorm(n.steps, mean = 0, sd = step.size)
bm <- c(0, cumsum(increments))


# brownian motion with drift t-s
# bm.drift(s) = bm(s)  + t*s - 0.5s^2
t <- 0.5
bm.drift <- bm + t*steps - 0.5*steps*steps


# brownian motion with reflection at 0
bm.ref <- reflect.brownian.motion(bm)
# brownian motion with drift with reflection at 0
bm.drift.ref <- reflect.brownian.motion(bm.drift)


## calculate excursion lengths
#  real excursion lengths of bm.drift.ref, order by size
excursion.lengths <- get.excursion.lengths(bm.drift.ref)
excursion.lengths <- excursion.lengths[order(-excursion.lengths)]
# normalized excursion lengths
excursion.lengths.norm <- excursion.lengths * step.size


## all brownian motions in one dataframe
df <- data.frame(
  step = steps,
  bm,
  bm.ref,
  bm.drift,
  bm.drift.ref
)
df <- melt(df, id.vars="step")


## plot all brownian motions
plot <- ggplot(data=df[df$variable=="bm.drift.ref", ], 
               aes(x=step, y=value, group=variable)) +
  geom_line(aes(color=variable)) +
  geom_hline(aes(yintercept = 0))
print(plot)
