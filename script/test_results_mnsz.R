# -- head -- #

setwd('~/Documents/R/vh_misc/vh2/')  # project root

library(tidyverse)  # data wrangling and I/O
library(glmmTMB)
library(performance)
library(sjPlot)
library(emmeans)
library(patchwork)

# -- read -- #

d = read_tsv('dat/mnsz_counts.tsv')

# -- setup -- #

# we want to tally back / front forms. so emberhez and emberhöz are both front etc.
dw = d |> 
  summarise(
    n = sum(n),
    .by = c('genre','llfpm10','stem','suffix','suffix_vowel')
  ) |> 
  pivot_wider(names_from = suffix_vowel, values_from = n, values_fill = 0) |> 
  mutate(genre = factor(genre, levels = c('sci','off','pers','spok','lit')))

# d |> 
#   dplyr::summarise(n = dplyr::n(), .by = c(genre, stem, suffix, n)) |>
#   dplyr::filter(n > 1L)

word_freq = d |> 
  distinct(stem,llfpm10) |> 
  arrange(-llfpm10)

dw_top50 = dw |> 
  filter(stem %in% word_freq[1:50,]$stem)

dw_top100 = dw |> 
  filter(stem %in% word_freq[1:100,]$stem)

# -- fit -- #

fit1a = glmmTMB(cbind(back,front) ~ genre + (1|stem) + (1|suffix), data = dw, family = binomial)
fit1b = glmmTMB(cbind(back,front) ~ genre + (1|stem) + (1|suffix), data = dw_top100, family = binomial)
fit1c = glmmTMB(cbind(back,front) ~ genre + (1|stem) + (1|suffix), data = dw_top50, family = binomial)

check_model(fit1a)
check_model(fit1c)

p1a = plot_model(fit1a, 'pred') + ylim(0,.18) + coord_flip() + ggtitle('all')
p1b = plot_model(fit1b, 'pred') + ylim(0,.18) + coord_flip() + ggtitle('100')
p1c = plot_model(fit1c, 'pred') + ylim(0,.18) + coord_flip() + ggtitle('50')

p1a | p1b | p1c

fit2b = glmmTMB(cbind(back,front) ~ genre + (1|stem) + (0 + genre|stem) + (1|suffix), data = dw_top100, family = binomial)
fit2c = glmmTMB(cbind(back,front) ~ genre + (1|stem) + (0 + genre|stem) + (1|suffix), data = dw_top50, family = binomial)

# nope