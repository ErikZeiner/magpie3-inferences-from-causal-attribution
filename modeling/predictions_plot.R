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

hyp12 <- reshape(read.csv("modeling/hyp12_model_predictions.csv"),
                     varying = c("blue", "red"),
                     v.names = "value",
                     timevar = "color",
                     times = c("blue", "red"),
                     direction = "long")
hyp3 <- reshape(read.csv("modeling/hyp3_model_predictions.csv"),
                     varying = c("blue", "red"),
                     v.names = "value",
                     timevar = "color",
                     times = c("blue", "red"),
                     direction = "long")
hyp4 <- reshape(read.csv("modeling/hyp4_model_predictions.csv"),
                     varying = c("blue", "red"),
                     v.names = "value",
                     timevar = "color",
                     times = c("blue", "red"),
                     direction = "long")

ggplot(hyp12, aes(x = color, y = value, fill = color)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ structure, nrow = 1) +
  theme_minimal()

ggplot(hyp3, aes(x = color, y = value, fill = color)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ valence, nrow = 1) +s
  theme_minimal()

#TODO: is this correct what it shows?
ggplot(hyp4, aes(x = color, y = value, fill = color)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ known, nrow = 1) +
  theme_minimal()






