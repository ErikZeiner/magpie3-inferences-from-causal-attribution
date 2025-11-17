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


# import data
d <- read.csv('01-experiments/03-exp-prescrNormInferenceBiased/data/01-data-raw-exp-03.csv')

# match the style of CorrectResponse, e.g. 'blue' -> 'Blue marble' (only pertains to first four submissions)
d <- d %>%
    mutate(correctResponse = str_replace(correctResponse, "blue", "Blue marble")) %>%
    mutate(correctResponse = str_replace(correctResponse, "red", "Red marble"))


# identify if response is correct (for comprehension questions)
d <- d %>% mutate(correct = ifelse(is.na(correctResponse), 1, correctResponse==response))


# identify participants failing comprehension question(s)
getCorrectness <- function(id){
  g <- d %>% filter(prolific_pid==id)
  if(sum(g$correct)==nrow(g)){return(1)}
  else{return(0)}
}

d <- d %>%
  mutate(participantCorrect = unlist(
    pmap(
      list(prolific_pid),
      getCorrectness
    )
  ))

orig_sample <- length(unique(d$prolific_pid))
# remove bad participants
d <- d %>% filter(participantCorrect==1)
# n for final sample
final_sample <- length(unique(d$prolific_pid))
final_sample


# focus on critical trials, recode response variable
d2 <- d %>%  filter() %>%
  filter(trialType=='critical') %>% 
  mutate(response=ifelse(actual_cause=='red', 8-as.numeric(response),
                                  as.numeric(response)))


dodgewidth <- .3



# acutal cause matches bias
d3 <- d2%>%
  mutate(stat_norm=ifelse(actual_cause == bias, 'Statistically normal','Statistically abnormal'))

d3 %>% count(effect_valence, mechanism, stat_norm)
## Save for parameter inference script in WebPPL
d3_key_columns <- d3 %>%
  filter(effect_valence== 'pleasant') %>%
  select(actual_cause, effect_valence,mechanism, response, bias, stat_norm)
write.csv(d3_key_columns, '01-experiments/03-exp-prescrNormInferenceBiased/data/01-data-processed-exp-03.csv')

# figure
d3 %>% ggplot(aes(x=mechanism,response,color=effect_valence))+
  facet_grid(~stat_norm)+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=effect_valence),
               position=position_dodge(width=dodgewidth))+
  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=effect_valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2)+
 ylab('Inference that cause is (prescriptive) norm-violating')+
  xlab('Structure')

d3 %>% ggplot(aes(x=mechanism,response))+
  facet_grid(~stat_norm)+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line',
               position=position_dodge(width=dodgewidth))+
  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=effect_valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2)+
 ylab('Inference that cause is norm-violating')+
                xlab('Structure')

final_plot<- ggplot(d3,aes(x=mechanism,response,color=stat_norm, shape=stat_norm))+
  facet_grid(~factor(effect_valence,levels = c('pleasant','neutral','unpleasant')))+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=stat_norm),
               position=position_dodge(width=dodgewidth))+
stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=stat_norm), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2)+
  scale_y_continuous(breaks=1:7)+
  coord_cartesian(ylim=c(1,7))+
  scale_color_manual(values=c(project_colors[6], project_colors[7]))+
  labs(x='Structure',y='Inference that cause is (prescriptive) norm-violating',color='Statistical norm')+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        legend.position='bottom',

        # strip.placement = "outside",  # Moves facet labels to the top
    strip.background = element_blank(),
        strip.text = element_text(size=14)
  )

final_plot
ggsave("../BAthesis/images/exp3_results.png", plot = final_plot, width = 8, height = 6.5, dpi = 300)

d2 %>%
  ggplot(aes(x=mechanism, y=response, color=effect_valence, shape=effect_valence))+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=effect_valence),
               position=position_dodge(width=dodgewidth))+

  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=effect_valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2) +
  scale_color_manual(values=c('blue', 'darkgreen', 'red'),
                     name='outcome valence')+
  ylab('Inference that cause is norm-violating')+
  xlab('Structure')+
  scale_y_continuous(breaks=1:7)+
  coord_cartesian(ylim=c(1,7))+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        #legend.position='top'
        )




# "omnibus" regression model:
fit <- brms::brm(response ~ mechanism * effect_valence * stat_norm,
                 data=d3 |> mutate(response = factor(response, ordered=T)),
                 family=brms::cumulative("logit"),
                 iter=4000, warmup=2000)

# some tests

## unpleasant > neutral? (yes)
faintr::compare_groups(
  fit, 
  higher = effect_valence == 'unpleasant', 
  lower = effect_valence == 'neutral')

## neutral > pleasant? (yes)
faintr::compare_groups(
  fit, 
  higher = effect_valence == 'neutral', 
  lower = effect_valence == 'pleasant')

## disjunctive vs conjunctive for each level of effect_valence

### pleasant (no effect)
faintr::compare_groups(
  fit, 
  higher = effect_valence == 'pleasant' & mechanism == 'conjunctive',
  lower  = effect_valence == 'pleasant' & mechanism == 'disjunctive' 
)

### neutral (no effect)
faintr::compare_groups(
  fit, 
  higher = effect_valence == 'neutral' & mechanism == 'conjunctive',
  lower = effect_valence == 'neutral' & mechanism == 'disjunctive' 
  )                       

### unpleasant (no effect)
faintr::compare_groups(
  fit, 
  higher = effect_valence == 'unpleasant' & mechanism == 'disjunctive',
  lower  = effect_valence == 'unpleasant' & mechanism == 'conjunctive'
)

## 'mechanism-effect' for conjunctive vs. 'mechanism-effect' for disjunctive
## -> not credible
faintr::extract_cell_draws(fit) |> 
  mutate(
    mechDiff_conj = `conjunctive:pleasant` - `conjunctive:unpleasant`,
    mechDiff_disj = `disjunctive:pleasant` - `disjunctive:unpleasant`,
    interaction   = mechDiff_disj - mechDiff_conj
    ) |> 
  pull(interaction) |> 
  aida::summarize_sample_vector()

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'pleasant' & mechanism == 'conjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'pleasant' & mechanism == 'conjunctive'
)

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'neutral' & mechanism == 'conjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'neutral' & mechanism == 'conjunctive'
)

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'unpleasant' & mechanism == 'conjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'unpleasant' & mechanism == 'conjunctive'
)

##########

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'pleasant' & mechanism == 'disjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'pleasant' & mechanism == 'disjunctive'
)

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'neutral' & mechanism == 'disjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'neutral' & mechanism == 'disjunctive'
)

faintr::compare_groups(
  fit,
  higher = stat_norm == 'Statistically abnormal' & effect_valence == 'unpleasant' & mechanism == 'disjunctive',
  lower  = stat_norm == 'Statistically normal' & effect_valence == 'unpleasant' & mechanism == 'disjunctive'
)

plot(fit)


### frequentist analysis---------------

# anova
av <- aov(response ~ mechanism * effect_valence * stat_norm, data=d3)
summary(av)
TukeyHSD(av)

# t-test in the neutral condition
dneutral <- d2 %>% filter(effect_valence=='neutral')
t.test(response~mechanism, data=dneutral)

# compare to the scale midpoint
dpos <- d2 %>% filter(effect_valence=='pleasant')
dneg <- d2 %>% filter(effect_valence=='unpleasant')
dneutralconj <- dneutral %>% filter(mechanism=='conjunctive')
dneutraldis <- dneutral %>% filter(mechanism=='disjunctive')

t.test(dpos$response, mu=4)
t.test(dneg$response, mu=4)
t.test(dneutralconj$response, mu=4)
t.test(dneutraldis$response, mu=4)


get_norm_sampled <- function(data,...){
  conj_conditional <- filter(data,mechanism=='conjunctive',...)
  disj_conditional <- filter(data,mechanism=='disjunctive',...)
  p_hat <- (count(filter(conj_conditional, response < 4))/count(conj_conditional)) %>% pull()
  q_hat <- (count(filter(disj_conditional, response < 4))/count(disj_conditional)) %>% pull()
  list(p_hat=p_hat, q_hat=q_hat)
}


test_hypo <- function(prob1,prob2, comp_op){
  comp <- comp_op(prob1,prob2)
  print(prob1)
  print(prob2)
  print(comp)
}

pos_statnorm <- get_norm_sampled(d3,effect_valence=='pleasant',stat_norm=='Statistically normal')
pos_statabno <- get_norm_sampled(d3,effect_valence=='pleasant',stat_norm=='Statistically abnormal')
test_hypo(pos_statnorm$p_hat, pos_statabno$p_hat,`>`)
