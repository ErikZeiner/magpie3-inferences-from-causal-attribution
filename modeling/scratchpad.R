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

utterances <- c('A' = 1, 'B' = -1)
structures <- c('and' = -1, 'or' = 1)
presNorms <- c('A' = 1, 'none' = 0, 'B' = -1)
statNorms <- c('A' = 1, 'none' = 0, 'B' = -1)
valences <- c('pleasant' = 1, 'neutral' = 0, 'unpleasant' = -1)

UtilityNorm <- function(utterance, structure, norm, gamma = 0.3) {
  return((abs(norm * ((1 + structure * utterance * norm) / 2)) - utterance * norm * structure * gamma))
}

UtilityValence <- function(valence) {
  if (valence == 1) return(0)
  if (valence == 0) return(0)
  if (valence == -1) return(0)
  return(valence)
}

Utility <- function(utterance, structure, presNorm, statNorm, valence, gamma, alpha = 1, beta = 0) {
  return(alpha * UtilityNorm(utterance, structure, presNorm, gamma) +
           (1 - alpha) * UtilityNorm(utterance, structure, statNorm, gamma) +
           UtilityValence(valence)
  )
}

test_gamma <- 0.35
# p
p_a <- Utility(utterances[['A']], structures[['and']], presNorms[['A']], statNorms[['none']], valences[['pleasant']], test_gamma)
p_b <- Utility(utterances[['B']], structures[['and']], presNorms[['B']], statNorms[['none']], valences[['pleasant']], test_gamma)

# 1-p
comp_p_a <- Utility(utterances[['A']], structures[['and']], presNorms[['B']], statNorms[['none']], valences[['neutral']], test_gamma)
comp_p_b <- Utility(utterances[['B']], structures[['and']], presNorms[['A']], statNorms[['none']], valences[['neutral']], test_gamma)

# q
q_a <- Utility(utterances[['A']], structures[['or']], presNorms[['A']], statNorms[['none']], valences[['neutral']], test_gamma)

q_b <- Utility(utterances[['B']], structures[['or']], presNorms[['B']], statNorms[['none']], valences[['neutral']], test_gamma)

# 1-q
comp_q_a <- Utility(utterances[['A']], structures[['or']], presNorms[['B']], statNorms[['none']], valences[['neutral']], test_gamma)
comp_q_b <- Utility(utterances[['B']], structures[['or']], presNorms[['A']], statNorms[['none']], valences[['neutral']], test_gamma)

# Print results
print(mget(c("p_a", "p_b", "comp_p_a", "comp_p_b",
             "q_a", "q_b", "comp_q_a", "comp_q_b")))

# Tests
p_a < 0.5
0.5 < q_a
p_b < 0.5
0.5 < q_b
isTRUE(all.equal(p_a + comp_p_a, 1))
isTRUE(all.equal(p_b + comp_p_b, 1))
isTRUE(all.equal(q_a + comp_q_a, 1))
isTRUE(all.equal(q_b + comp_q_b, 1))

beta_sample <- function(p, spread) {
  splits <- seq(0, 1, length.out = 8)
  par_alpha <- p * spread
  par_beta <- (1 - p) * spread

  cdfs <- pbeta(splits, par_alpha, par_beta)
  probs <- pmax(diff(cdfs), 0)
  probs <- probs / sum(probs)

  data.frame(label = c(1:7), prob = probs)
}

df <- beta_sample(0.5, 3)

ggplot(df, aes(x = label, y = prob)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7)) +
  theme_minimal()

df1 <- data.frame(
  structure = character(),
  norm = character(),
  utterance = character(),
  effect_valence = character()
)

df2 <- data.frame(
  structure = character(),
  norm = character(),
  response = character(),
  effect_valence = character()
)
dodgewidth <- .35
gamma <- 0.35

for (structure in c('and', 'or')) {
  for (norm in c('A', 'B')) {
    for (valence in c('pleasant', 'neutral', 'unpleasant')) {
      for (trial in 0:20) {
        Pa <- Utility(utterances[['A']], structures[[structure]], presNorms[[norm]], statNorms[['none']], valences[[valence]], gamma)
        Pb <- Utility(utterances[['B']], structures[[structure]], presNorms[[norm]], statNorms[['none']], valences[[valence]], gamma)
        utterance <- sample(c('A', 'B'), size = 1, prob = c(Pa, Pb))
        df1 <- rbind(df1, data.frame(structure = structure, norm = norm, utterance = utterance, effect_valence = valence))
        distribution <- beta_sample(Pa, 1)
        response <- sample(distribution$label, size = 1, replace = TRUE, prob = distribution$prob)
        df2 <- rbind(df2, data.frame(structure = structure, norm = norm, response = response, effect_valence = valence))
      }
    }
  }
}


speaker <- function(structure, normPresc, normStat, valence) {
  Pa <- Utility(utterances[['A']], structures[[structure]], presNorms[[norm]], statNorms[['none']], valences[[valence]], gamma)
  Pb <- Utility(utterances[['B']], structures[[structure]], presNorms[[norm]], statNorms[['none']], valences[[valence]], gamma)
  utterance <- sample(c('A', 'B'), size = 1, prob = c(Pa, Pb))
  data.frame(structure = structure, norm = norm, utterance = utterance, effect_valence = valence)
}


listenerPresNorm <- function(utterance,structure,presNorms, statNorms, valence){

}
# df2 <- df2 %>%
#     mutate(response=ifelse(utterance=='B', as.numeric(response),
#                                   8-as.numeric(response)))


#
# df2 %>%
#   group_by(structure,response)
#
# a <- df2 %>%
#   filter(norm == 'A')
#
# b <- df2 %>%
#   filter(norm == 'B')
# plot(table(a$response))
#
# df2 %>%
#   group_by(structure,effect_valence,response)%>%
#   summarise(
#     total_n = count(n),
#     total_percent = sum(percent),
#     .groups = "drop"
#   )

df2 %>%
  ggplot(aes(x = structure, y = response)) +
  stat_summary(fun = 'mean', position = position_dodge(width = dodgewidth)) +
  stat_summary(fun = 'mean', geom = 'line', aes(group = effect_valence),
               position = position_dodge(width = dodgewidth)) +

  stat_summary(fun.data = 'mean_se', position = position_dodge(width = dodgewidth)) +
  geom_jitter(aes(shape = valence), position = position_jitterdodge(jitter.width = 0.2,
                                                                    jitter.height = .2,
                                                                    dodge.width = dodgewidth),
              color = 'black', alpha = .2)
# labs(x='Structure',y='Inference that cause is (prescriptive) norm-violating',color='Outcome valence')+
# scale_y_continuous(breaks=1:7)+
# coord_cartesian(ylim=c(1,7))+
# geom_hline(aes(yintercept=4), linetype='dashed')+
# theme_classic()+
# guides(shape='none')+
# theme(axis.title=element_text(size=16),
#       axis.text=element_text(size=14),
#       legend.title=element_text(size=16),
#       legend.text=element_text(size=13),
#       legend.position='bottom'
#       )

df1 <- df1 %>%
  mutate(match = ifelse(utterance == norm, "normative", "norm violation"))
df_counts <- df1 %>%
  count(structure, match, effect_valence) %>%
  mutate(match = factor(match, levels = c("normative", "norm violation")))

df_pct <- df_counts %>%
  group_by(structure) %>%
  mutate(percent = n / sum(n) * 100)

df_sum <- df_pct %>%
  group_by(structure, match) %>%
  summarise(
    total_n = sum(n),
    total_percent = sum(percent),
    .groups = "drop"
  )

df_valence <- df_pct %>%
  filter(match == 'norm violation')

# bar plot
ggplot(df_sum, aes(x = structure, y = total_percent, fill = match)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal()

# ggplot(df_valence, aes(x = structure, y = percent,color=valence)) +
#   geom_point()+
#   geom_line(aes(group=valence))+
#   coord_cartesian(ylim=c(0,100))+
#   theme_minimal()

