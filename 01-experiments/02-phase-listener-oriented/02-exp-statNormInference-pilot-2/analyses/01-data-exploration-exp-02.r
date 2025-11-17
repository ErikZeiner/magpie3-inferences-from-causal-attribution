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


getwd()
# import data
d1 <- read.csv('01-experiments/02-exp-statNormInference/data/01-data-raw-exp-02.csv')
d2 <- read.csv('01-experiments/02-exp-statNormInference/data/02-data-raw-exp-02.csv')
length(unique(d1$prolific_pid))
length(unique(d2$prolific_pid))

# identify if response is correct (for comprehension questions)
d1 <- d1 %>% mutate(correct =
                  ifelse(is.na(correctResponse), 1,
                         correctResponse==response))
d2 <- d2 %>% mutate(correct =
                  ifelse(is.na(correctResponse), 1,
                         correctResponse==response))

# identify participants failing comprehension question(s)

getCorrectness <- function(id,d){
  g <- d %>% filter(prolific_pid==id)
  if(sum(g$correct)==nrow(g)){return(1)}
  else{return(0)}
  
}

d1 <- d1 %>% mutate(participantCorrect = unlist(
  pmap(
    list(prolific_pid,list(d1)),
    getCorrectness
  )
))
d2 <- d2 %>% mutate(participantCorrect = unlist(
  pmap(
    list(prolific_pid,list(d2)),
    getCorrectness
  )
))

d1 %>%
  filter(correctResponse=='only the islanders know')%>%
  count(response)
d2 %>%
  filter(correctResponse=='only the islanders know')%>%
  count(response)



# remove bad participants
d1 <- d1 %>% filter(participantCorrect==1)
d2 <- d2 %>% filter(participantCorrect==1)
# n for final samples
length(unique(d1$prolific_pid))
length(unique(d2$prolific_pid))

# focus on critical trials, recode response variable
d1 <- d1 %>%  filter() %>%
  filter(trialType=='critical') %>%
  mutate(response=ifelse(actual_cause=='red', as.numeric(response),
                                  8-as.numeric(response)))

d2 <- d2 %>%  filter() %>%
  filter(trialType=='critical') %>%
  mutate(response=ifelse(actual_cause=='red', as.numeric(response),
                                  8-as.numeric(response)))

## Save for parameter inference script in WebPPL
d1_key_columns <- d1 %>%
  select(actual_cause, effect_valence,mechanism, response)
d2_key_columns <- d2 %>%
  select(actual_cause, effect_valence,mechanism, response)
write.csv(d1_key_columns, '01-experiments/02-exp-statNormInference/data/01-data-processed-exp-02.csv')
write.csv(d2_key_columns, '01-experiments/02-exp-statNormInference/data/02-data-processed-exp-02.csv')

d1 %>% count(effect_valence, mechanism)
d2 %>% count(effect_valence, mechanism)

dodgewidth <- .3

d1$Source <- "d1"
d2$Source <- "d2"
mix <- rbind(d1,d2)
length(mix$prolific_pid)
# figure
p1<- d1 %>%
  ggplot(aes(x=mechanism, y=response, color=effect_valence, shape=effect_valence))+
  # facet_grid(~Source)+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=effect_valence),
               position=position_dodge(width=dodgewidth))+

  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=effect_valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2) +
  labs(x='Structure',y='Inference that cause is (statistical) norm-violating',color='Outcome valence')+
  scale_y_continuous(breaks=1:7)+
  coord_cartesian(ylim=c(1,7))+
  # ggtitle('Pilot 1')+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        legend.position='bottom'
        )

p2 <- d2 %>%
  ggplot(aes(x=mechanism, y=response, color=effect_valence, shape=effect_valence))+
  # facet_grid(~Source)+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=effect_valence),
               position=position_dodge(width=dodgewidth))+

  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=effect_valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2) +
  labs(x='Structure',y='Inference that cause is (statistical) norm-violating',color='Outcome valence')+
  scale_y_continuous(breaks=1:7)+
  coord_cartesian(ylim=c(1,7))+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  # ggtitle('Pilot 2')+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        legend.position='bottom'
        )

(p1 + p2) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
p1
ggsave("../BAthesis/images/exp2_pilot1_results.png", plot = p1, width = 6.5, height = 6.5, dpi = 300)

p2
ggsave("../BAthesis/images/exp2_pilot2_results.png", plot = p2, width = 6.5, height = 6.5, dpi = 300)

# "omnibus" regression model:
fit <- brms::brm(response ~ mechanism * effect_valence,
                 data=d2 |> mutate(response = factor(response, ordered=T)),
                 family=brms::cumulative("logit"),
                 iter=4000, warmup=2000)

# some tests

faintr::compare_groups(
  fit,
  higher = mechanism == 'conjunctive',
  lower = mechanism == 'disjunctive')


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





### frequentist analysis---------------


kruskal.test(response ~ mechanism * effect_valence, data=d1)
# anova
av <- aov(response ~ mechanism * effect_valence, data=d2)
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
