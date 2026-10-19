# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

set.seed(1337)

library(tidyverse)
library(glmmTMB)
library(performance)
library(emmeans)
library(sjPlot)
library(broom.mixed)

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
  mutate(n_total = n_back + n_front) |>
  filter(n_total > 0)

cat('mnsz_agg: ', nrow(mnsz_agg), ' stem x genre cells, ', n_distinct(mnsz_agg$stem), ' stems\n', sep = '')

# -- mnsz models -- #

# as proposed: genre x language as a full fixed interaction
m_mnsz_full = glmmTMB(
  cbind(n_back, n_front) ~ genre * language + (1 | stem),
  data = mnsz_agg, family = binomial
)

# alternative: language as a partially-pooled random intercept instead of a fixed
# factor crossed with genre -- kept for comparison now that Germanic/Yiddish are
# merged, to see whether pooling still buys anything once the thinnest level is gone
m_mnsz_alt = glmmTMB(
  cbind(n_back, n_front) ~ genre + language + (1 | stem),
  data = mnsz_agg, family = binomial
)

cat('\n== MNSZ: n distinct stems per genre x language cell ==\n')
mnsz_agg |>
  summarise(.by = c(genre, language), n_stem = n_distinct(stem), n_tok = sum(n_total)) |>
  arrange(language, genre) |>
  print(n = 100)

cat('\n-- MNSZ full model: genre * language + (1|stem) --\n')
print(summary(m_mnsz_full))
cat('singular fit: ', performance::check_singularity(m_mnsz_full), '\n', sep = '')
cat('convergence ok: ', performance::check_convergence(m_mnsz_full), '\n', sep = '')

cat('\n-- MNSZ alt model: genre + language + (1|stem) --\n')
print(summary(m_mnsz_alt))
cat('singular fit: ', performance::check_singularity(m_mnsz_alt), '\n', sep = '')
cat('convergence ok: ', performance::check_convergence(m_mnsz_alt), '\n', sep = '')
performance::check_collinearity(m_mnsz_full)
performance::check_collinearity(m_mnsz_alt)

cat('\n-- MNSZ model comparison --\n')
print(performance::compare_performance(m_mnsz_full, m_mnsz_alt))

write_tsv(broom.mixed::tidy(m_mnsz_full, conf.int = TRUE), 'dat/tab_mnsz_full_coef.tsv')
write_tsv(broom.mixed::tidy(m_mnsz_alt, conf.int = TRUE), 'dat/tab_mnsz_alt_coef.tsv')

plot_model(m_mnsz_alt, 'pred', terms = c('genre','language'))
plot_model(m_mnsz_alt, 'pred', terms = c('language','genre'))

# -- mtsz models -- #

# as proposed: fiction x year x language as a full fixed 3-way interaction
m_mtsz_full = glmmTMB(
  is_back ~ lit * year_z * language + (1 | stem),
  data = mtsz2, family = binomial
)

# intermediate: keep the lit x year interaction (does fiction's drift over time
# differ from non-fiction's?), drop language out of the interaction entirely
m_mtsz_lit_year = glmmTMB(
  is_back ~ lit * year_z + language + (1 | stem),
  data = mtsz2, family = binomial
)

# alternative: fully additive, year continuous (scaled) instead of ntile() bins
m_mtsz_alt = glmmTMB(
  is_back ~ lit + year_z + language + (1 | stem),
  data = mtsz2, family = binomial
)

cat('\n== MTSZ: n distinct stems per fiction x language cell ==\n')
mtsz2 |>
  summarise(.by = c(lit, language), n_stem = n_distinct(stem), n_tok = n()) |>
  arrange(language, lit) |>
  print(n = 100)

cat('\n-- MTSZ full model: lit * year * language + (1|stem) --\n')
print(summary(m_mtsz_full))
cat('singular fit: ', performance::check_singularity(m_mtsz_full), '\n', sep = '')
cat('convergence ok: ', performance::check_convergence(m_mtsz_full), '\n', sep = '')
cat('collinear?: ', performance::check_collinearity(m_mtsz_full), '\n', sep = '')

cat('\n-- MTSZ lit*year model: lit * year + language + (1|stem) --\n')
print(summary(m_mtsz_lit_year))
cat('singular fit: ', performance::check_singularity(m_mtsz_lit_year), '\n', sep = '')
cat('convergence ok: ', performance::check_convergence(m_mtsz_lit_year), '\n', sep = '')
performance::check_collinearity(m_mtsz_lit_year)

cat('\n-- MTSZ alt model: lit + year + language + (1|stem) --\n')
print(summary(m_mtsz_alt))
cat('singular fit: ', performance::check_singularity(m_mtsz_alt), '\n', sep = '')
cat('convergence ok: ', performance::check_convergence(m_mtsz_alt), '\n', sep = '')
performance::check_collinearity(m_mtsz_alt)

cat('\n-- MTSZ model comparison (AIC/BIC) --\n')
print(performance::compare_performance(m_mtsz_lit_year, m_mtsz_alt))

cat('\n-- MTSZ nested likelihood-ratio cascade: alt -> lit*year -> full --\n')
print(anova(m_mtsz_lit_year, m_mtsz_alt))

write_tsv(broom.mixed::tidy(m_mtsz_full, conf.int = TRUE), 'dat/tab_mtsz_full_coef.tsv')
write_tsv(broom.mixed::tidy(m_mtsz_lit_year, conf.int = TRUE), 'dat/tab_mtsz_lit_year_coef.tsv')
write_tsv(broom.mixed::tidy(m_mtsz_alt, conf.int = TRUE), 'dat/tab_mtsz_alt_coef.tsv')

p_mtsz_lit_year = plot_model(m_mtsz_lit_year, show.values = TRUE, value.offset = .3) +
  theme_bw() +
  ggtitle('MTSZ: lit * year + language + (1|stem)')
ggsave('viz/fit_mtsz_lit_year_coef.png', p_mtsz_lit_year, width = 6, height = 5, dpi = 900)

p_mtsz_alt = plot_model(m_mtsz_alt, show.values = TRUE, value.offset = .3) +
  theme_bw() +
  ggtitle('MTSZ: lit + year + language + (1|stem)')
ggsave('viz/fit_mtsz_alt_coef.png', p_mtsz_alt, width = 6, height = 5, dpi = 900)

cat('\n-- MTSZ lit*year model: year slope within each lit level --\n')
print(emtrends(m_mtsz_lit_year, ~ lit, var = 'year_z', infer = TRUE))

cat('\n-- MTSZ alt model: pairwise language contrasts (log-odds scale) --\n')
print(emmeans(m_mtsz_alt, pairwise ~ language)$contrasts)

# -- emmeans plots -- #

# mnsz: estimated P(back) by genre, from the alt model (genre is the only fixed
# effect there, so this is an unambiguous marginal estimate -- the full model's
# genre marginal would need an arbitrary weighting scheme across language)
mnsz_genre_emm = emmeans(m_mnsz_alt, ~ genre, type = 'response') |>
  as.data.frame() |>
  mutate(genre = fct_relevel(genre, 'lit', 'pers', 'spok', 'off', 'sci', 'press'))

p_mnsz_genre_emm = ggplot(mnsz_genre_emm, aes(genre, prob, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  theme_bw() +
  ylab('P(back)') +
  xlab('genre') +
  ggtitle('MNSZ: estimated P(back) by genre', subtitle = 'genre + (1|stem) + (1|language)')
ggsave('viz/fit_mnsz_genre_emmeans.png', p_mnsz_genre_emm, width = 5, height = 4, dpi = 900)

# mtsz: estimated P(back) over year, fiction vs non-fiction, from the lit*year model.
# year_z is the scaled covariate the model was fit on; back-transform the prediction
# grid to real years for a readable x-axis. language is averaged over (equal weight)
year_mean = mean(mtsz2$year)
year_sd = sd(mtsz2$year)
year_grid = seq(min(mtsz2$year), max(mtsz2$year), length.out = 50)

mtsz_lit_year_emm = emmeans(
  m_mtsz_lit_year, ~ lit, by = 'year_z',
  at = list(year_z = (year_grid - year_mean) / year_sd),
  type = 'response'
) |>
  as.data.frame() |>
  mutate(year = year_z * year_sd + year_mean)

p_mtsz_lit_year_emm = ggplot(mtsz_lit_year_emm, aes(year, prob, ymin = asymp.LCL, ymax = asymp.UCL, colour = lit, fill = lit)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ylab('P(back)') +
  xlab('year') +
  labs(colour = 'genre', fill = 'genre') +
  ggtitle('MTSZ: estimated P(back) by year, fiction vs non-fiction', subtitle = 'lit * year + language + (1|stem)')
ggsave('viz/fit_mtsz_lit_year_emmeans.png', p_mtsz_lit_year_emm, width = 6, height = 4.5, dpi = 900)

# plot_model(m_mtsz_lit_year, 'pred', terms = c('year_z','lit'))
