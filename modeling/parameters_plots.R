# load packages
library(tidyverse)
library(ggbeeswarm)
library(brms)
library(tidyboot)
library(tidyjson)
library(jsonlite)
library(tidybayes)
library(patchwork)
library(GGally)
library(cowplot)
library(bayesplot)
library(BayesFactor)
library(aida)   # custom helpers: https://github.com/michael-franke/aida-package
library(faintr) # custom helpers: https://michael-franke.github.io/faintr/index.html
library(cspplot)

##################################################

# these options help Stan run faster
options(mc.cores = parallel::detectCores(),
        brms.backend = "cmdstanr")

# use the CSP-theme for plotting
theme_set(theme_csp())

# global color scheme from CSP
project_colors = cspplot::list_colors() |> pull(hex)

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
}

##################################################


parameter_posteriors <- read_json("modeling/parameters_posterior.json", simplifyVector = FALSE)
df <- parameter_posteriors%>% map_df(~ c(.x$value, score = .x$score))

ggplot(df, aes(x = alpha))+
  geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1,1)+
  theme_minimal()+
  geom_vline(xintercept = quantile(df$alpha, probs = c(0.025, 0.975)), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(df$alpha), linetype = "solid", color = "blue", size = 1)

ggplot(df, aes(x = beta))+
   geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1,1)+
  theme_minimal()+
  geom_vline(xintercept = quantile(df$beta, probs = c(0.025, 0.975)), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(df$beta), linetype = "solid", color = "blue", size = 1)

ggplot(df, aes(x = gamma))+
   geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1,1)+
  theme_minimal()+
  geom_vline(xintercept = quantile(df$gamma, probs = c(0.025, 0.975)), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(df$gamma), linetype = "solid", color = "blue", size = 1)

posterior_beta <- as.matrix(df["beta"])  # keep as a data frame column
colnames(posterior_beta) <- "beta"
# # Basic density plot with bayesplot
# color_scheme_set("blue")  # nice color palette

mcmc_areas(
  as.matrix(posterior_beta),
  prob = 0.95,            # 95% credible interval
  prob_outer = 1,         # full range
  point_est = "mean"      # show mean
) +
  ggtitle("Posterior density of Beta") +
  xlab(expression(beta)) +
  theme_minimal(base_size = 14)

df_long <- df %>%
  rename(value = gamma) %>%  # tidybayes expects 'value'
  mutate(dist = "gamma")     # dummy 'dist' column for grouping

ggplot(df_long, aes(x = value)) +
  stat_lineribbon(
    .width = c(.66, .95),
    .color = "blue",
    .fill = "lightblue"
  ) +
  xlim(-1, 1) +
  theme_minimal()


df %>%
  ggplot(aes(x = gamma)) +
