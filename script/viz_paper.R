# -- head -- #

# figures for the NyK paper: Hungarian labels, black-and-white-safe, 12.6 cm wide
# refits the final models from fit.R, does not touch the originals in viz/

setwd('~/Github/Racz2027mnszmtsz/')

set.seed(1337)

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(ggeffects)
library(patchwork)

# -- read -- #

mtsz_raw = read_tsv('dat/mtsz_Racz2027vh_hits_wrangled.tsv')
mnsz_raw = read_tsv('dat/mnsz_counts.tsv')

# -- wrangle: mtsz (as in fit.R) -- #

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
    lit = factor(lit, levels = c(FALSE, TRUE), labels = c('nem szépirodalmi', 'szépirodalmi')),
    is_back = suffix_harmony == 'back',
    language = ifelse(borrowing_language %in% c('German', 'Yiddish'), 'Germanic', borrowing_language)
  ) |>
  filter(!is.na(is_back), !is.na(language), !is.na(year))

# year_z -> year mapping for the prediction plot
year_mean = mean(mtsz2$year)
year_sd = sd(mtsz2$year)

# -- wrangle: mnsz (as in fit.R) -- #

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

# -- models (final ones from fit.R) -- #

m_mnsz2 = glmmTMB(
  cbind(n_back, n_front) ~ genre + language + (1 | stem),
  data = mnsz_agg, family = binomial
)

m_mtsz7 = glmmTMB(
  as.double(is_back) ~ lit * year_z + (1 | language) + (1 | stem),
  data = mtsz2, family = binomial
)

# -- figure 1: mnsz marginal means, genre + language -- #

genre_labels = c(
  lit = 'szépirodalmi', pers = 'személyes', spok = 'beszélt',
  off = 'hivatalos', press = 'sajtó', sci = 'tudományos'
)

language_labels = c(
  Germanic = 'germán', French = 'francia', Latin = 'latin',
  English = 'angol', other = 'egyéb'
)

em_genre = emmeans(m_mnsz2, 'genre') |>
  summary() |>
  as_tibble() |>
  mutate(cimke = genre_labels[as.character(genre)] |> fct_reorder(emmean))

em_language = emmeans(m_mnsz2, 'language') |>
  summary() |>
  as_tibble() |>
  mutate(cimke = language_labels[as.character(language)] |> fct_reorder(emmean))

# shared geometry: point + CI segment, black on white, secondary probability axis
plot_emmeans = function(d, breaks_lo, breaks_p){
  d |>
    ggplot(aes(x = emmean, y = cimke)) +
    geom_linerange(aes(xmin = asymp.LCL, xmax = asymp.UCL), linewidth = .75, colour = 'grey40') +
    geom_point(size = 2.5) +
    scale_x_continuous(
      name = 'becsült átlag, log-esély', breaks = breaks_lo,
      sec.axis = sec_axis(transform = plogis, name = 'becsült átlag, p(hátsó)', breaks = breaks_p)
    ) +
    theme_bw() +
    theme(axis.title.y = element_blank())
}

p1 = plot_emmeans(em_genre, -4:-1, c(.02, .05, .1)) + ggtitle('zsáner')
p2 = plot_emmeans(em_language, -5:0, c(.01, .05, .1, .25)) + ggtitle('átadó nyelv')

p1 + p2
ggsave('viz/nyk_mnsz.png', dpi = 900, width = 12.6/2.54, height = 2.4)

# -- figure 2: mtsz predicted probabilities, fiction x year -- #

pred_mtsz = ggpredict(m_mtsz7, terms = c('year_z [n=60]', 'lit')) |>
  as_tibble() |>
  mutate(year = year_mean + x * year_sd)

pred_mtsz |>
  ggplot(aes(x = year, y = predicted, linetype = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15, colour = NA) +
  geom_line(linewidth = .75) +
  scale_y_continuous(name = 'hátsó toldalék becsült aránya', labels = scales::percent) +
  scale_x_continuous(name = 'év') +
  scale_linetype_manual(values = c('szépirodalmi' = 'solid', 'nem szépirodalmi' = 'dashed')) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'bottom')
ggsave('viz/nyk_mtsz.png', dpi = 900, width = 12.6/2.54, height = 3.2)

# -- figure 3: mnsz random intercepts + lm on them -- #

ranef_mnsz = ranef(m_mnsz2, condVar = T) |>
  as_tibble() |>
  arrange(-condval) |>
  mutate(
    grp = fct_reorder(grp, condval),
    category = case_when(
      str_detect(grp, 'ek$') ~ '-ek (matek)',
      str_detect(grp, 'esz$') ~ '-esz (kolesz)',
      T ~ 'egyéb'
    ) |> fct_relevel('egyéb', '-esz (kolesz)', '-ek (matek)')
  )

ranef_lm = lm(condval ~ category, data = ranef_mnsz)

# left panel: top 24 stems, shapes distinguish the stem-final classes (bw-safe)
p3 = ranef_mnsz |>
  slice(1:24) |>
  ggplot(aes(y = grp, shape = category)) +
  geom_segment(aes(x = 0, xend = condval, yend = grp), linewidth = .5, colour = 'grey60') +
  geom_point(aes(x = condval), size = 2.2) +
  scale_shape_manual(values = c('egyéb' = 16, '-esz (kolesz)' = 17, '-ek (matek)' = 15)) +
  labs(x = 'tő random intercept, log-esély', y = NULL, shape = 'tővég') +
  theme_bw() +
  ggtitle('a 24 legnagyobb random intercept')

# right panel: lm estimates per stem-final class
em_ranef = emmeans(ranef_lm, 'category') |>
  summary() |>
  as_tibble()

p4 = em_ranef |>
  ggplot(aes(x = emmean, y = category)) +
  geom_vline(xintercept = 0, linetype = 'dotted', colour = 'grey50') +
  geom_linerange(aes(xmin = lower.CL, xmax = upper.CL), linewidth = .75, colour = 'grey40') +
  geom_point(size = 2.5) +
  labs(x = 'becsült átlag, log-esély', y = NULL) +
  theme_bw() +
  ggtitle('csoportátlagok (lm)')

p3 + p4 + plot_layout(guides = 'collect', widths = c(1.2, 1)) &
  theme(legend.position = 'bottom', plot.title = element_text(size = 10))
ggsave('viz/nyk_mnsz_ranef.png', dpi = 900, width = 12.6/2.54, height = 4.2)
