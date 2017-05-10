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
set.seed(600)

# last time value
time.max <- 4
# number of steps
n.steps <- 2000


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

# brownian motion with drift t-s
bm.drift <- bm + tpos*steps - 0.5*steps*steps

# brownian motion with reflection at 0
bm.ref <- reflect.brownian.motion(bm)
# brownian motion with drift with reflection at 0
bm.drift.ref <- reflect.brownian.motion(bm.drift)

# all brownian motions in one dataframe
df.pos <- data.frame(
  step = steps,
  bm,
  bm.ref,
  bm.drift,
  bm.drift.ref
) %>% 
  melt(id.vars="step")

plot <- ggplot(df.pos %>% filter(variable == "bm"),
               aes(x = step, y = value)) +
  geom_line()
plot(plot)

#### Plot positive t --------------------------------

# Plot regular Brownian motion
tikz('fig_bm.tex',standAlone = FALSE, width = 1.5,height = 1.5)
plot <- ggplot(df.pos %>% filter(variable=="bm"), 
               aes(x=step, y=value)) +
  geom_line() +
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

# Plot Brownian motion with drift.
tikz('fig_bm_drift_pos.tex',standAlone = FALSE, width = 1.5,height = 1.5)
plot <- ggplot(df.pos %>% filter(variable=="bm.drift"), 
               aes(x=step, y=value)) +
  geom_line() +
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

# Plot Brownian motion with drift and reflection.
tikz('fig_bm_drift_ref_pos.tex',standAlone = FALSE, width = 1.5,height = 1.5)
plot <- ggplot(df.pos %>% filter(variable=="bm.drift.ref"), 
               aes(x=step, y=value)) +
  geom_line() +
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


#### Create Brownian Motion negative t --------------------------------

# brownian motion with drift t-s
bm.drift <- bm + tneg*steps - 0.5*steps*steps

# brownian motion with reflection at 0
bm.ref <- reflect.brownian.motion(bm)
# brownian motion with drift with reflection at 0
bm.drift.ref <- reflect.brownian.motion(bm.drift)

# all brownian motions in one dataframe
df.neg <- data.frame(
  step = steps,
  bm,
  bm.ref,
  bm.drift,
  bm.drift.ref
) %>% 
  melt(id.vars="step")

#### Plot negative t --------------------------------

# Plot Brownian motion with drift.
tikz('fig_bm_drift_neg.tex',standAlone = FALSE, width = 1.5,height = 1.5)
plot <- ggplot(df.neg %>% filter(variable=="bm.drift"), 
               aes(x=step, y=value)) +
  geom_line() +
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

# Plot Brownian motion with drift and reflection.
tikz('fig_bm_drift_ref_neg.tex',standAlone = FALSE, width = 1.5,height = 1.5)
plot <- ggplot(df.neg %>% filter(variable=="bm.drift.ref"), 
               aes(x=step, y=value)) +
  geom_line() +
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