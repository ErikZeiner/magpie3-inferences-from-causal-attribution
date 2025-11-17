# load packages
library(tidyverse)
library(ggbeeswarm)
library(brms)
library(tidyboot)
library(tidyjson)
library(tidybayes)
library(patchwork)
library(GGally)
library(cowplot)
library(BayesFactor)
library(aida)   # custom helpers: https://github.com/michael-franke/aida-package
library(faintr) # custom helpers: https://michael-franke.github.io/faintr/index.html
library(cspplot)
library(ggdist)
library(posterior)
library(bayesplot)


# install.packages("devtools")
# devtools::install_github("mhtess/rwebppl")
library(rwebppl)
install_webppl_package('webppl-csv')
# install_webppl_package('webppl-json')
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

set.seed(1138)

RECOMPUTE <- FALSE

run_model <- function(exec_code) {
  base <- "
// Cumulative logit function
var logisticFun = function (x) {
    return 1 / (1 + Math.exp(-x))
}

var cumulativeLogitFun = function (x, j, thresholds) {
    if (j == 0) {
        return logisticFun(thresholds[0] - x)
    }
    if (j == thresholds.length) {
        return 1 - logisticFun(thresholds[thresholds.length - 1] - x)
    } else {
        return logisticFun(thresholds[j] - x) - logisticFun(thresholds[j - 1] - x)
    }
}

// constant values
//var thresholds = [1 / 7, 2 / 7, 3 / 7, 4 / 7, 5 / 7, 6 / 7]
var thresholds = [-3.5 ,-2.5, -1.5, -0.5, 0.5, 1.5]
//var thresholds = [-3, -2, -0.5, 0, 0.5, 2]

var likertScale = ['1', '2', '3', '4', '5', '6', '7']

// input values
var utterances = {'blue': 1, 'red': -1}
var structures = {'conjunctive': -1, 'disjunctive': 1}
var norms = {'blue': 1, 'none': 0, 'red': -1}
var valences = {'pleasant': 1, 'neutral': 0, 'unpleasant': -1}

// utility components

var utilityKnown = function (known, structure, utterance) {
    return structure * known * utterance
}

var utilityPrivate = function (private, structure, utterance) {
    return structure * private * utterance
}

var utilityValence = function (known, private, utterance, valence) {
    return (known*utterance*valence + private*utterance*valence)/2
}

// Utility function
var utilityTotal = function (known, private, structure, utterance, valence, alpha, beta, gamma) {
    return alpha * utilityPrivate(private,structure,utterance)
    + beta * utilityKnown(known,structure, utterance)
    + gamma * utilityValence(known,private,utterance,valence)
}

// speaker function
var speaker = function (known, private, structure, valence, alpha, beta, gamma, lambda) {
    Infer({
        model: function () {
            var utterance = uniformDraw(Object.keys(utterances))
            factor(lambda * utilityTotal(known, private, structure, utterances[utterance], valence, alpha, beta, gamma))
            return utterance
        }
    })
}

// listener function
var listener = function (known, structure, utterance, valence, alpha, beta, gamma, lambda) {
    Infer({
        model: function () {
            var private = uniformDraw(['blue', 'red'])
            observe(speaker(norms[known], norms[private], structures[structure], valences[valence], alpha, beta, gamma, lambda), utterance)
            return private
        }
    })
}
"
  return(webppl(paste(base, exec_code), packages = c("webppl-csv")))
}

run_predictions <- function(exec_code) {
  run_model(paste("
    var alpha = 0.5
    var beta = -0.5
    var gamma = 0.5
    var lambda = 1",
                  exec_code))
}

hyp12_pred <- run_predictions("
var res = {
conjunctive: listener('none', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
disjunctive: listener('none', 'disjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda)
}
res
")

df_hyp12 <- map_df(hyp12_pred, ~{ tibble(prob = .x$probs[.x$support == "red"])
}, .id = "Condition") %>%
  mutate(Condition = str_replace_all(Condition, c("conjunctive" = "Conjunctive",
                                                  "disjunctive" = "Disjunctive")))


my_theme <- theme_classic() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside",  # Moves facet labels to the top
        strip.background = element_blank()  # Optional: removes background of facet labels
  )

hyp12_plot <- ggplot(df_hyp12, aes(x = Condition, y = prob, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.35) +
  scale_fill_manual(values = c(project_colors[4], project_colors[5])) +
  labs(y = 'Probability that cause is norm-violating', x = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  my_theme +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme(axis.title.y = element_text(size = 14))

hyp12_plot


hyp3_pred <- run_predictions("
var res = {
unpleasant: listener('none', 'conjunctive', 'blue', 'unpleasant', alpha, beta, gamma, lambda),
neutral: listener('none', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
pleasant: listener('none', 'conjunctive', 'blue', 'pleasant', alpha, beta, gamma, lambda)
}
res
")

df_hyp3 <- map_df(hyp3_pred, ~{ tibble(prob = .x$probs[.x$support == "red"])
}, .id = "Valence") %>%
  mutate(Valence = str_replace_all(Valence, c("pleasant" = "Pleasant",
                                              "neutral" = "Neutral", "unPleasant" = "Unpleasant")))


hyp3_plot <- ggplot(df_hyp3, aes(x = Valence, y = prob, fill = Valence)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_x_discrete(limits = c("Unpleasant", "Neutral", "Pleasant")) +
  ylim(0, 1) +
  labs(y = '', x = '') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  my_theme +
  geom_hline(yintercept = 0.5, linetype = "dashed")

hyp3_plot

hyp4_pred <- run_predictions("
var res = {
stat_norm_conj: listener('blue', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
stat_abnorm_conj: listener('red', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
//stat_none_conj: listener('none', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
stat_norm_disj: listener('blue', 'disjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
stat_abnorm_disj: listener('red', 'disjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
//stat_none_disj: listener('none', 'disjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda)
}
res
")


df_hyp4 <- map_df(hyp4_pred, ~{ tibble(prob = .x$probs[.x$support == "red"])
}, .id = "stat_norm")


df_hyp4 <- df_hyp4 %>%
  mutate(Structure = str_sub(stat_norm, -4)) %>%
  mutate(norm = str_sub(stat_norm, 1, -5)) %>%
  mutate(Structure = str_replace_all(Structure, c("conj" = "Conjunctive",
                                                  "disj" = "Disjunctive"))) %>%
  mutate(norm = str_replace_all(norm, c("stat_abnorm_" = "Statistically abnormal",
                                        "stat_norm_" = "Statistically normal")))


hyp4_plot <- ggplot(df_hyp4, aes(x = Structure, y = prob, fill = Structure)) +
  facet_grid(~norm, scales = "free_x") +
  geom_bar(stat = "identity", position = "dodge", width = 0.65) +
  ylim(0, 1) +
  scale_fill_manual(values = c(project_colors[4], project_colors[5])) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(y = '', x = '') +
  my_theme +
  geom_hline(yintercept = 0.5, linetype = "dashed")

combined_plot <- plot_grid(
  hyp12_plot + theme(axis.title.y = element_text(size = 14)),
  hyp3_plot + theme(axis.title.y = element_blank()),
  hyp4_plot + theme(axis.title.y = element_blank()),
  nrow = 1,
  align = "h"
)
combined_plot
ggsave("../BAthesis/images/predictions.png", plot = combined_plot, width = 14, height = 5, dpi = 300)


# pred <- run_predictions("
# var res = {
# neg_conj: listener('none', 'conjunctive', 'blue', 'unpleasant', alpha, beta, gamma, lambda),
# norm_conj: listener('none', 'conjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
# pos_conj: listener('none', 'conjunctive', 'blue', 'pleasant', alpha, beta, gamma, lambda),
# neg_disj: listener('none', 'disjunctive', 'blue', 'unpleasant', alpha, beta, gamma, lambda),
# norm_disj: listener('none', 'disjunctive', 'blue', 'neutral', alpha, beta, gamma, lambda),
# pos_disj: listener('none', 'disjunctive', 'blue', 'pleasant', alpha, beta, gamma, lambda)
# }
# res
# ")
#
# dfpred <- map_df(pred, ~{tibble(prob = .x$probs[.x$support == "red"])
# }, .id = "valence")
#
# dfpred <- dfpred %>%
#   mutate(strut = str_sub(valence, -4))%>%
#   mutate(norm = str_sub(valence, 1, -5))
#
#
# ggplot(dfpred, aes(x = strut, y = prob, fill = strut)) +
#   facet_grid(~norm)+
#   geom_bar(stat = "identity", position = "dodge", width = 0.5) +
#   ylim(0,1)+
#   theme_minimal()+
#   scale_fill_manual(values=c(project_colors[4], project_colors[5]))+
#   theme(    legend.position = "bottom",
#     legend.direction = "horizontal")

parameter_estimation <- function(file) {
  run_model(paste0("
    var parameterSampling = function (data) {

    var alpha = uniform({a: -1, b: 1})
    var beta = uniform({a: -1, b: 1})
    var gamma = uniform({a: -1, b: 1})
    var lambda = uniform({a: 0, b: 20})

    map(function (row) {
        var utterance = row[1]
        var valence = row[2]
        var structure = row[3]
        var response = row[4]
        var known = row.length > 5 ? row [5]: 'none'

        var listenerPredictions = listener(known, structure, utterance, valence, alpha, beta, gamma, lambda)
        var notUttered = utterance == 'blue' ? 'red' : 'blue'
        var probs = map(function (j) {
                return cumulativeLogitFun(listenerPredictions.score(notUttered), j, thresholds)
            },
            _.range(0, 7))
        observe(Categorical({vs: likertScale, ps: probs}), response)
    }, data)

    return {alpha: alpha, beta: beta, gamma: gamma, lambda: lambda}
}

var data = csv.read('", file, "').slice(1, -1)
var infer = Infer({
        method: 'MCMC',
        samples: 20000,
        burn: 5000,
        model: function () {
            return parameterSampling(data)
        }
    })

console.log(MAP(infer))
infer
"))
}

if (RECOMPUTE){

df_exp1 <- parameter_estimation('01-experiments/01-exp-descrNormInference/data/01-pilot/01-data-processed-pilot-01.csv')
df_exp2_pilot1 <- parameter_estimation('01-experiments/02-exp-statNormInference/data/01-data-processed-exp-02.csv')
df_exp2_pilot2 <- parameter_estimation('01-experiments/02-exp-statNormInference/data/02-data-processed-exp-02.csv')
df_exp3 <- parameter_estimation('01-experiments/03-exp-prescrNormInferenceBiased/data/01-data-processed-exp-03.csv')
# SAVE PRINTED MAP OUTPUTS MANUALLY, THERE SEEMS TO BE NO BETTER WAY
write.csv(df_exp1,file='modeling/df_exp1.csv')
write.csv(df_exp2_pilot1,file='modeling/df_exp2_pilot1.csv')
write.csv(df_exp2_pilot2,file='modeling/df_exp2_pilot2.csv')
write.csv(df_exp3,file='modeling/df_exp3.csv')
}else{
df_exp1 <- read.csv('modeling/df_exp1.csv')
df_exp2_pilot1 <- read.csv('modeling/df_exp2_pilot1.csv')
df_exp2_pilot2 <- read.csv('modeling/df_exp2_pilot2.csv')
df_exp3 <- read.csv('modeling/df_exp3.csv')

}

get_HDI = function(df) {
  return(list(
  alpha = quantile((df %>% filter(Parameter == 'alpha'))$value, c(0.025, 0.975)),
  beta  = quantile((df %>% filter(Parameter == 'beta'))$value, c(0.025, 0.975)),
  gamma = quantile((df %>% filter(Parameter == 'gamma'))$value, c(0.025, 0.975)),
  lambda= quantile((df %>% filter(Parameter == 'lambda'))$value, c(0.025, 0.975))
))
}

get_HDI(df_exp1)
get_HDI(df_exp2_pilot1)
get_HDI(df_exp2_pilot2)
get_HDI(df_exp3)

plot_posterior <- function(df, betascale = 0.2) {
  ggplot() +
    stat_eye(data = df[df_exp1$Parameter == 'alpha',],
             aes(x = value, fill = stat(x < 0)),
             .width = 0.95, adjust = 1) +
    stat_eye(data = df[df_exp1$Parameter == 'beta',],
             aes(x = value, fill = stat(x > 0)),
             .width = 0.95, adjust = 1, scale = betascale) +
    stat_eye(data = df[df_exp1$Parameter == 'gamma',],
             aes(x = value, fill = stat(x < 0)),
             .width = 0.95, adjust = 1) +
    scale_fill_manual(values = c(project_colors[1], project_colors[2])) +
    theme_classic() +
  scale_x_continuous(
    limits = c(-1, 1),                # x-axis range from -1 to 1
    breaks = seq(-1, 1, by = 0.2)     # tick marks every 0.10
  )+
    facet_grid(rows = vars(Parameter),
               labeller = labeller(Parameter = c(alpha = "α", beta = "β", gamma = "γ"
               ))) +
    guides(fill = "none") +
    scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
    labs(y = 'Density', x = 'Parameter value') +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(panel.spacing = unit(0, "lines"),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          strip.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 16),
          strip.background = element_blank()
    )
}




exp1_post <- plot_posterior(df_exp1)
exp1_post
exp2_pilot1_post <- plot_posterior(df_exp2_pilot1)
exp2_pilot1_post
exp2_pilot2_post <- plot_posterior(df_exp2_pilot2)
exp2_pilot2_post
exp3_post <- plot_posterior(df_exp3, 1)
exp3_post
ggsave("../BAthesis/images/exp1_posterior.png", plot = exp1_post, width = 10, height = 4, dpi = 300)
ggsave("../BAthesis/images/exp2_pilot1_posterior.png", plot = exp2_pilot1_post, width = 10, height = 4, dpi = 300)
ggsave("../BAthesis/images/exp2_pilot2_posterior.png", plot = exp2_pilot2_post, width = 10, height = 4, dpi = 300)
ggsave("../BAthesis/images/exp3_posterior.png", plot = exp3_post, width = 10, height = 4, dpi = 300)

# df %>%
#   filter(Parameter != "lambda") %>%
#   ggplot(aes(x = value)) +
#   stat_halfeye(.width = 0.95, fill = project_colors[1], alpha = 0.6) +
#   theme_minimal() +
#   geom_vline(xintercept = 0, linetype = "dashed", color = project_colors[2]) +
#   facet_wrap(~Parameter, scales = "free_y") +
#   # scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     plot.margin = margin(10, 10, 10, 10),
#     panel.spacing = unit(1.5, "lines"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )+
#   xlim(-1,1)
# }

# exp1_post <- df_exp1 %>%
#   filter(Parameter != "lambda") %>%
#   ggplot(aes(x = value, y = Parameter)) +
#   stat_halfeye(.width = 0.95, fill = project_colors[1], alpha = 0.6, scale = 1, adjust = 1) +
#   theme_minimal()+
#   geom_vline(xintercept = 0, linetype = "dashed", color = project_colors[2]) +
#   scale_y_discrete(expand = expansion(mult = c(0.02, 0.02)))

exp2_pilot1_post <- df_exp2_pilot1 %>%
  filter(Parameter != "lambda") %>%
  # filter(Parameter != "beta") %>%
  ggplot(aes(x = value, y = Parameter)) +
  stat_halfeye(.width = 0.95, fill = project_colors[1], alpha = 0.6, scale = 1, adjust = 1) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = project_colors[2]) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02)))
exp2_pilot1_post

exp2_pilot2_post <- df_exp2_pilot2 %>%
  filter(Parameter != "lambda") %>%
  filter(Parameter == "alpha") %>%
  # filter(Parameter != "beta") %>%
  ggplot(aes(x = value)) +
  stat_halfeye(.width = 0.95, fill = project_colors[1], alpha = 0.6, scale = 1, adjust = 1) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = project_colors[2]) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02)))

exp2_pilot2_post


exp3_post <- df_exp3 %>%
  filter(Parameter != "lambda") %>%
  # filter(Parameter != "beta") %>%
  ggplot(aes(x = value, y = Parameter)) +
  stat_halfeye(.width = 0.95, fill = project_colors[1], alpha = 0.6, scale = 1, adjust = 1) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = project_colors[2]) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02)))
exp3_post


ggplot(df_exp1 %>% filter(Parameter != 'lambda'), aes(x = value)) +
  facet_grid(~Parameter) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1, 1) +
  theme_minimal()


ggplot(df_exp2_pilot1 %>% filter(Parameter != 'lambda'), aes(x = value)) +
  facet_wrap(~Parameter) +
  geom_density(fill = project_colors[1], alpha = 0.5) +
  xlim(-1, 1) +
  theme_minimal()


ggplot(df_exp2_pilot2 %>% filter(Parameter != 'lambda'), aes(x = value)) +
  facet_grid(~Parameter) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1, 1) +
  theme_minimal()

ggplot(df_exp3 %>% filter(Parameter != 'lambda'), aes(x = value)) +
  facet_grid(~Parameter) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  xlim(-1, 1) +
  theme_minimal()

mcmc_areas(
  df_exp1,
  pars = c("alpha", "beta", "gamma"),
  prob = 0.95  # 95% credible interval
) + ggplot2::labs(title = "Posterior Distributions with 95% Credible Intervals")
