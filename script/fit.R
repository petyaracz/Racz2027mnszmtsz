# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

set.seed(1337)

library(tidyverse)
library(glmmTMB)
library(performance)
library(emmeans)
library(sjPlot)
library(broom.mixed)
library(patchwork)

# -- read -- #

mtsz_raw = read_tsv('dat/mtsz_Racz2027vh_hits_wrangled.tsv')
mnsz_raw = read_tsv('dat/mnsz_counts.tsv')

# -- wrangle: mtsz -- #

# same filters as viz.R: drop the 'mater' stem (dominant, false-positive-prone),
# keep only short genuine suffix matches, derive year, scale it, and collapse the
# register info to a fiction/non-fiction flag (the MTSZ stand-in for "genre")
mtsz = mtsz_raw |>
  filter(stem != 'mater')

keep_suffix = mtsz |>
  count(suffix, sort = T) |>
  filter(
    str_detect(suffix, '[aáeéiíoóöüuúüű]'),
    nchar(suffix) < 4,
    !str_detect(suffix, '(ház|ív|író|ing|nő|tű|-|”|~)$'),
    !str_detect(suffix, '-')
  ) |>
  pull(suffix)

mtsz2 = mtsz |>
  filter(suffix %in% keep_suffix) |>
  mutate(
    year = str_extract(wdate, '^[0-9]{4}') |> as.double(),
    year_z = as.numeric(scale(year)),
    lit = lform_register == 'szépir' | lform_fiction == 1,
    lit = ifelse(is.na(lit), F, lit),
    lit = factor(lit, levels = c(FALSE, TRUE), labels = c('non-fiction', 'fiction')),
    is_back = suffix_harmony == 'back',
    # Yiddish borrowings into Hungarian are predominantly of German-Yiddish stock,
    # so pool the two into one "Germanic" level -- also fixes Yiddish's thin cells
    # (8-9 stems total) by combining them with German's much larger stem count
    language = ifelse(borrowing_language %in% c('German', 'Yiddish'), 'Germanic', borrowing_language)
  ) |>
  filter(!is.na(is_back), !is.na(language), !is.na(year))

cat('mtsz2: ', nrow(mtsz2), ' tokens, ', n_distinct(mtsz2$stem), ' stems\n', sep = '')

# -- wrangle: mnsz -- #

# mnsz_counts.tsv is one row per attested word-FORM x genre, with a token count n and
# that form's suffix_vowel class. collapse to binomial counts per stem x genre x
# language cell for the GLMM: cbind(n_back, n_front) ~ ...
# Yiddish folded into "Germanic" with German, see mtsz2 comment above for rationale
mnsz_agg = mnsz_raw |>
  filter(suffix_vowel %in% c('back', 'front'), !is.na(borrowing_language)) |>
  mutate(language = ifelse(borrowing_language %in% c('German', 'Yiddish'), 'Germanic', borrowing_language)) |>
  summarise(
    n_back = sum(n[suffix_vowel == 'back']),
    n_front = sum(n[suffix_vowel == 'front']),
    .by = c(stem, genre, language)
  ) |>
  mutate(
    n_total = n_back + n_front,
    genre = fct_relevel(genre, 'lit', 'pers', 'spok', 'off', 'press', 'sci'),
    language = fct_relevel(language, 'other', 'English', 'Latin', 'French', 'Germanic')
         ) |>
  filter(n_total > 0)

cat('mnsz_agg: ', nrow(mnsz_agg), ' stem x genre cells, ', n_distinct(mnsz_agg$stem), ' stems\n', sep = '')

# -- mnsz models -- #

m_mnsz1 = glmmTMB(
  cbind(n_back, n_front) ~ genre * language + (1 | stem),
  data = mnsz_agg, family = binomial
)

m_mnsz2 = glmmTMB(
  cbind(n_back, n_front) ~ genre + language + (1 | stem),
  data = mnsz_agg, family = binomial
)

check_model(m_mnsz1)
check_model(m_mnsz2)

summary(m_mnsz2)

p1 = m_mnsz2 |> 
  emmeans('genre') |> 
  plot() +
  scale_x_continuous(name = 'emmean, log odds', breaks = -4:-1, sec.axis = sec_axis(transform = plogis, name = 'emmean, p', breaks = c(.01, .05, .1)))

p2 = m_mnsz2 |> 
  emmeans('language') |> 
  plot() +
  scale_x_continuous(name = 'emmean, log odds', breaks = -4:0, sec.axis = sec_axis(transform = plogis, name = 'emmean, p', breaks = c(.01, .05, .1, .25)))

p1 + p2 + plot_annotation(title = 'MNSZ')
ggsave('viz/mnsz.png', dpi = 900, width = 7, height = 3)

# -- mtsz models -- #

m_mtsz1 = glmmTMB(
  is_back ~ lit * year_z * language + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz2 = glmmTMB(
  is_back ~ lit + year_z * language + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz3 = glmmTMB(
  is_back ~ lit * year_z + language + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz4 = glmmTMB(
  is_back ~ lit * language + year_z + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz5 = glmmTMB(
  is_back ~ lit * year_z + year_z * language + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz6 = glmmTMB(
  is_back ~ lit + year_z + language + (1 | stem),
  data = mtsz2, family = binomial
)

m_mtsz7 = glmmTMB(
  as.double(is_back) ~ lit * year_z + (1|language) + (1 | stem),
  data = mtsz2, family = binomial
)

check_collinearity(m_mtsz1)
check_collinearity(m_mtsz2)
check_collinearity(m_mtsz3)
check_collinearity(m_mtsz4) # hmm
check_collinearity(m_mtsz5)
check_collinearity(m_mtsz6)
check_overdispersion(m_mtsz2)
check_overdispersion(m_mtsz3)
check_overdispersion(m_mtsz4)
check_residuals(m_mtsz2)
check_residuals(m_mtsz3)
check_residuals(m_mtsz4)

plot(compare_performance(m_mtsz2,m_mtsz3,m_mtsz4,m_mtsz5,m_mtsz6, metrics = 'common'))
test_likelihoodratio(m_mtsz4,m_mtsz6)
test_likelihoodratio(m_mtsz3,m_mtsz6)
test_bf(m_mtsz3, m_mtsz4)

summary(m_mtsz3)
summary(m_mtsz7)

plot_model(m_mtsz7, 'pred', terms = c('year_z','lit')) +
  theme_bw() +
  scale_fill_viridis_d() +
  scale_colour_viridis_d()

ggsave('viz/mtsz.png', width = 4, height = 3, dpi = 900)
