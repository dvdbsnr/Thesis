library(gridExtra)
plot <- grid.arrange(
  df.pos %>% filter(variable == "bm") %>%
    ggplot(aes(x=step, y = value)) +
    geom_line(),
  df.pos %>% filter(variable == "bm.drift") %>%
    ggplot(aes(x=step, y = value)) +
    geom_line(),
  df.pos %>% filter(variable == "bm.drift.ref") %>%
    ggplot(aes(x=step, y = value)) +
    geom_line(),
  df.neg %>% filter(variable == "bm.drift") %>%
    ggplot(aes(x=step, y = value)) +
    geom_line(),
  df.neg %>% filter(variable == "bm.drift.ref") %>%
    ggplot(aes(x=step, y = value)) +
    geom_line(),
  ncol = 3
)
plot(plot)