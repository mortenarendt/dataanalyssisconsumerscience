rm(list = ls())
library(readr)
library(tidyverse)

X <- read_delim("~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/food_uncertainty/Fitt-DST15 - reduced.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
X %>% colnames()
X$USDA_foodinsecurity_DICH %>% hist()

X %>% gather(var,val,-USDA_foodinsecurity_DICH) %>%
  mutate(val = val %>% as.numeric()) %>% 
  filter(val < 90) %>% 
  ggplot(data = ., aes(factor(USDA_foodinsecurity_DICH),val)) + 
  facet_wrap(~var, scales = 'free') + geom_violin()

X %>% gather(var,val,LifeSatisfaction, HighPsychDistress, Gender) %>%
  mutate(val = val %>% as.numeric()) %>%
  filter(!is.na(val)) %>% 
  group_by(var,val,USDA_foodinsecurity_DICH) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(var,USDA_foodinsecurity_DICH) %>% 
  mutate(prc = n / sum(n)) %>% 
  ggplot(data = ., aes(factor(USDA_foodinsecurity_DICH),prc, fill = factor(val))) + 
  facet_wrap(~var, scales = 'free') + geom_bar(stat = 'identity')
