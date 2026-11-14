# fit models for noun task per AsPredicted #315,048.
#
# prereg calls for "bayesian glm" with weakly informative priors, participant and word
# random intercepts, model selection by bf and loo -- this needs brms (bridgesampling
# for bf, native loo support), not glmmTMB (frequentist, no bf). flagged as a deviation
# from the CLAUDE.md default because the prereg's own requirements rule it out.

# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

library(tidyverse)
library(brms)
library(loo)
library(broom.mixed)
library(glue)

set.seed(1337)

# -- read data -- #

d = read_tsv('dat/noun_task_filtered.tsv') |>
  mutate(
    suffix_type = factor(suffix_type),
    noun_class = factor(noun_class),
    font = factor(font),
    id = factor(id),
    target = factor(target)
  )

# -- quick check -- #

n_part = length(unique(d$id))
n_part_g = length(unique(d[d$font == 'EB Garamond',]$id))
n_part_s = length(unique(d[d$font == 'Open Sans',]$id))

d |> 
  summarise(
    p = mean(accept), 
    n = n(),
    se = sqrt((p * (1 - p)) / n),
    .by = c(noun_class,font)
  ) |> 
  mutate(
    range = glue::glue('{round(p-2*se,2)}:{round(p+2*se,2)}')
  ) |> 
  select(noun_class,font,range) |> 
  pivot_wider(names_from = noun_class, values_from = range) |> 
  knitr::kable()

glue('with {n_part} participants (Garamond: {n_part_g}; Sans: {n_part_s})')

# # -- priors -- #
# 
# # weakly informative: wide normal on intercept/coefficients, exponential on group-level sd
# 
# prior_accept = c(
#   prior(normal(0, 3), class = 'Intercept'),
#   prior(normal(0, 2), class = 'b'),
#   prior(exponential(1), class = 'sd')
# )
# 
# prior_rt = c(
#   prior(normal(0, 3), class = 'Intercept'),
#   prior(normal(0, 2), class = 'b'),
#   prior(exponential(1), class = 'sd'),
#   prior(exponential(1), class = 'sigma')
# )
# 
# # -- fit: accept ~ conditions -- #
# 
# fit_accept_main = brm(
#   accept ~ suffix_type + noun_class + font + (1 | id) + (1 | target),
#   data = d,
#   family = bernoulli(),
#   prior = prior_accept,
#   save_pars = save_pars(all = T),
#   chains = 4, cores = 4, seed = 1337,
#   file = 'fit/fit_accept_main'
# )
# 
# fit_accept_full = brm(
#   accept ~ suffix_type * noun_class * font + (1 | id) + (1 | target),
#   data = d,
#   family = bernoulli(),
#   prior = prior_accept,
#   save_pars = save_pars(all = T),
#   chains = 4, cores = 4, seed = 1337,
#   file = 'fit/fit_accept_full'
# )
# 
# # -- fit: rt ~ conditions -- #
# 
# fit_rt_main = brm(
#   rt ~ suffix_type + noun_class + font + (1 | id) + (1 | target),
#   data = d,
#   family = gaussian(),
#   prior = prior_rt,
#   save_pars = save_pars(all = T),
#   chains = 4, cores = 4, seed = 1337,
#   file = 'fit/fit_rt_main'
# )
# 
# fit_rt_full = brm(
#   rt ~ suffix_type * noun_class * font + (1 | id) + (1 | target),
#   data = d,
#   family = gaussian(),
#   prior = prior_rt,
#   save_pars = save_pars(all = T),
#   chains = 4, cores = 4, seed = 1337,
#   file = 'fit/fit_rt_full'
# )
# 
# # -- model selection: loo + bf -- #
# 
# fit_accept_main = add_criterion(fit_accept_main, 'loo')
# fit_accept_full = add_criterion(fit_accept_full, 'loo')
# fit_rt_main = add_criterion(fit_rt_main, 'loo')
# fit_rt_full = add_criterion(fit_rt_full, 'loo')
# 
# loo_accept = loo_compare(fit_accept_main, fit_accept_full)
# loo_rt = loo_compare(fit_rt_main, fit_rt_full)
# 
# bf_accept = bayes_factor(fit_accept_full, fit_accept_main)
# bf_rt = bayes_factor(fit_rt_full, fit_rt_main)
# 
# print(loo_accept)
# print(loo_rt)
# print(bf_accept)
# print(bf_rt)
# 
# # -- report winning models -- #
# 
# # pick whichever model loo favours (lower elpd difference within ~se -> prefer simpler main-effects model)
# 
# tidy_accept = broom.mixed::tidy(fit_accept_full, conf.int = T)
# tidy_rt = broom.mixed::tidy(fit_rt_full, conf.int = T)
# 
# write_tsv(tidy_accept, 'dat/noun_task_fit_accept.tsv')
# write_tsv(tidy_rt, 'dat/noun_task_fit_rt.tsv')
