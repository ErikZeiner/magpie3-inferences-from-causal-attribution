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

df1 <- read.csv('modeling/simulation_output_experiments_1_2.csv')
df2 <- read.csv('modeling/simulation_output_experiments_3.csv')

df3 <- df2%>%
  mutate(stat_norm=ifelse(known == utterance, 'Statistically normal','Statistically abnormal'))

# ggplot(aes(x=mechanism, y=response, color=valence, shape=valence))+
# ggplot(df1, aes(x=structure,y=red,color=valence,shape=valence))+
#     geom_point()+
#     geom_line(aes(group=valence))+
#   theme_minimal()+
#   coord_cartesian(ylim=c(0,1))

#
# ggplot(df1, aes(x=structure,y=red,color=stat_norm,shape=stat_norm))+
#   facet_grid(~factor(valence,levels = c('pleasant','neutral','unpleasant')))+
#       geom_point()+
#     geom_line(aes(group=stat_norm))+
#   theme_minimal()+
#   coord_cartesian(ylim=c(0,1))


dodgewidth <- .3
df1 %>%
  ggplot(aes(x=structure, y=response, color=valence, shape=valence))+
  stat_summary(fun='mean', position=position_dodge(width=dodgewidth))+
  stat_summary(fun='mean', geom='line', aes(group=valence),
               position=position_dodge(width=dodgewidth))+
  stat_summary(fun.data='mean_se', position=position_dodge(width=dodgewidth))+
  geom_jitter(aes(shape=valence), position = position_jitterdodge(jitter.width = 0.05,
                                              jitter.height = .2,
                                              dodge.width = dodgewidth),
              color='black', alpha=.2) +
  labs(x='Structure',y='Inference that cause is (prescriptive) norm-violating',color='Outcome valence')+
  scale_y_continuous(breaks=1:7)+
  coord_cartesian(ylim=c(1,7))+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        legend.position='bottom'
        )

df3 %>% ggplot(aes(x=structure,response,color=stat_norm, shape=stat_norm))+
  facet_grid(~factor(valence,levels = c('pleasant','neutral','unpleasant')))+
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
  scale_color_manual(values=c(project_colors[3], project_colors[4]))+
  labs(x='Structure',y='Inference that cause is (prescriptive) norm-violating',color='Statistical norm')+
  geom_hline(aes(yintercept=4), linetype='dashed')+
  theme_classic()+
  guides(shape='none')+
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text=element_text(size=13),
        legend.position='bottom'
  )