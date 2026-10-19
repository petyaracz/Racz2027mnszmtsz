# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

library(tidyverse)
library(patchwork)

# -- mtsz -- #

mtsz = read_tsv('dat/mtsz_Racz2027vh_hits_wrangled.tsv') |> 
  filter(stem != 'mater')

# -- limit -- #

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
  filter(suffix %in% keep_suffix)

# -- wrangle -- #

mtsz2 = mtsz2 |> 
  mutate(
    year = str_extract(wdate, '^[0-9]{4}') |> as.double(),
    lit = lform_register == 'szépir' | lform_fiction == 1,
    lit = ifelse(is.na(lit), F, lit),
    year_bin = ntile(year,5)
  ) |> 
  mutate(
    year_min = min(year),
    year_max = max(year),
    year_bin_tidy = glue::glue('{year_min}-{year_max}') |> fct_reorder(year_bin),
    .by = year_bin
  )

# -- sum -- #

mtsz_bins = mtsz2 |> 
  count(
    borrowing_language, suffix_harmony, lit, year_bin_tidy
  ) |> 
  filter(!is.na(suffix_harmony),!is.na(lit)) |> 
  pivot_wider(names_from = suffix_harmony, values_from = n, values_fill = 0) |> 
  mutate(
    n = back + front,
    p_back = back / (back + front),
    se_back = sqrt((p_back * (1 - p_back))/n)
         )

# -- flavour -- #

mtsz2 |> 
  sample_n(10) |> 
  select(year,left,target,suffix,suffix_harmony)

mtsz2 |> 
  filter(borrowing_language == 'Latin', year_bin == 1, lit == F) |> 
  select(year,left,target,suffix,suffix_harmony) |> 
  sample_n(10)

# -- viz -- #

p_mtsz = mtsz_bins |> 
  mutate(
    label = ifelse(lit, 'fiction', 'non-fiction'),
    borrowing_language = fct_relevel(borrowing_language, 'Yiddish','German','English','French','Latin','other')
    ) |> 
  ggplot(aes(year_bin_tidy,p_back, colour = label)) +
  geom_point() +
  geom_linerange(aes(ymin = p_back - 2 * se_back, ymax = p_back + 2 * se_back)) +
  theme_bw() +
  facet_wrap(~ borrowing_language) +
  scale_colour_viridis_d() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
        ) +
  ylab('p(back)') + 
  ggtitle('back + <e>; MTSZ')

# -- mnsz -- #

# -- load data -- #

mnsz = read_tsv('dat/mnsz_counts.tsv') |>  # word x genre counts with suffix/vowel info
  mutate(is_back = suffix_vowel == 'back')  # convenience flag for the outcome of interest

# -- simple plot -- #

mnsz_bins = mnsz |> 
  count(suffix_vowel,genre) |> 
  pivot_wider(names_from = suffix_vowel, values_from = n, values_fill = 0) |> 
  mutate(
    n = back + front,
    p_back = back / (back + front),
    se_back = sqrt((p_back * (1 - p_back))/n)
  )

# -- plot -- #

p_mnsz = mnsz_bins |> 
  mutate(genre = fct_relevel(genre, 'lit','pers','press','spok','sci','off')) |> 
  ggplot(aes(genre,p_back)) +
  geom_point() +
  geom_linerange(aes(ymin = p_back - 2 * se_back, ymax = p_back + 2 * se_back)) +
  theme_bw() +
  scale_colour_viridis_d() +
  ylab('p(back)') + 
  ggtitle('back + <e>; MNSZ')

(p_mnsz + plot_spacer()) / p_mtsz + plot_layout(heights = c(1,2))
ggsave('viz/genre_mtsz_mnsz.pdf', width = 7, height = 6)

# -- claude plots -- #

genres = mnsz |>  # take the full table
  distinct(genre) |>  # unique genre labels
  pull(genre)  # as a character vector, for looping below

ks = c(0, 1, 2, 5, 10, 20)  # number of top-n rows to drop, for the sensitivity check in fig4

# -- fig1: token-weighted vs type-weighted p(back) by genre -- #
# token-weighted = naive proportion, sensitive to a handful of hyperfrequent forms
# type-weighted = each distinct stem x suffix x vowel combination counts once, regardless of frequency

p_back_token = mnsz |>  # start from the full table
  summarise(.by = genre, n_total = sum(n), n_back = sum(n[is_back])) |>  # sum tokens per genre
  mutate(p_back = n_back / n_total, weighting = 'token')  # proportion back, tag the weighting scheme

p_back_type = mnsz |>  # start from the full table
  distinct(genre, stem, suffix, suffix_vowel) |>  # collapse to one row per attested combination
  summarise(.by = genre, n_total = n(), n_back = sum(suffix_vowel == 'back')) |>  # count combinations per genre
  mutate(p_back = n_back / n_total, weighting = 'type')  # proportion back, tag the weighting scheme

fig1_dat = bind_rows(p_back_token, p_back_type)  # stack both weightings for plotting

# write_tsv(fig1_dat, 'dat/tab_weighted_vs_type.tsv')  # save the numbers behind fig1

fig1 = ggplot(fig1_dat, aes(genre, p_back, colour = weighting)) +  # genre on x, p(back) on y, coloured by weighting
  geom_point(size = 3, position = position_dodge(width = 0.3)) +  # dodge the two weightings apart per genre
  scale_colour_viridis_d(option = 'H') +  # discrete viridis palette
  theme_bw() +  # house plotting theme
  labs(x = 'genre', y = 'P(back)', colour = 'weighting',
       title = 'Token- vs type-weighted P(back) by genre')  # axis/legend/title text

# ggsave('viz/fig1_weighted_vs_type.png', fig1, width = 6, height = 4, dpi = 900)  # write figure

# -- fig2: stem x genre heatmap of p(back) -- #
# restricted to stems attested in >= 4 genres with >= 10 tokens in each, so cells are comparable

stem_genre = mnsz |>  # start from the full table
  summarise(.by = c(stem, genre), n_total = sum(n), n_back = sum(n[is_back])) |>  # sum tokens per stem x genre
  mutate(p_back = n_back / n_total) |>  # proportion back per cell
  filter(n_total >= 10)  # drop cells too sparse to be informative

qualifying_stems = stem_genre |>  # start from the filtered cells
  count(stem) |>  # count qualifying genres per stem
  filter(n >= 4) |>  # keep stems seen in at least 4 genres
  pull(stem)  # as a character vector

stem_genre_sub = stem_genre |>  # start from the filtered cells
  filter(stem %in% qualifying_stems)  # keep only the comparable stems

# write_tsv(stem_genre_sub, 'dat/tab_stem_genre_pback.tsv')  # save the numbers behind fig2

fig2_height = max(4, 0.15 * n_distinct(stem_genre_sub$stem))  # scale figure height to the number of stems shown

fig2 = ggplot(stem_genre_sub, aes(genre, stem, fill = p_back)) +  # genre x stem grid, fill = proportion back
  geom_tile() +  # heatmap tiles
  scale_fill_viridis_c() +  # continuous viridis palette
  theme_bw() +  # house plotting theme
  theme(axis.text.y = element_text(size = 6)) +  # shrink stem labels, there are many
  labs(x = 'genre', y = 'stem', fill = 'P(back)',
       title = 'P(back) by stem and genre (stems in >=4 genres, n>=10)')  # axis/legend/title text

# ggsave('viz/fig2_stem_genre_heatmap.png', fig2, width = 6, height = fig2_height, dpi = 900)  # write figure

# -- fig3: suffix case x genre heatmap of p(back) -- #

suffix_genre = mnsz |>  # start from the full table
  summarise(.by = c(genre, suffix), n_total = sum(n), n_back = sum(n[is_back])) |>  # sum tokens per case x genre
  mutate(p_back = n_back / n_total)  # proportion back per cell

# write_tsv(suffix_genre, 'dat/tab_suffix_genre_pback.tsv')  # save the numbers behind fig3

fig3 = ggplot(suffix_genre, aes(genre, suffix, fill = p_back, alpha = n_total)) +  # genre x case grid, fill = P(back), alpha = support
  geom_tile() +  # heatmap tiles
  scale_fill_viridis_c() +  # continuous viridis palette
  scale_alpha_continuous(trans = 'log10', range = c(0.3, 1)) +  # fade low-n cells, n spans orders of magnitude
  theme_bw() +  # house plotting theme
  labs(x = 'genre', y = 'suffix (case)', fill = 'P(back)', alpha = 'n',
       title = 'P(back) by suffix case and genre')  # axis/legend/title text

# ggsave('viz/fig3_suffix_genre_heatmap.png', fig3, width = 6, height = 6, dpi = 900)  # write figure

# -- fig4: sensitivity of genre-level p(back) to top-n rows -- #
# drops the k most frequent word forms per genre and recomputes p(back); flat lines = robust estimate

sensitivity = mnsz |>  # start from the full table
  nest(.by = genre) |>  # one row per genre, with its data nested
  mutate(sens = map(data, function(rows) {  # for each genre's rows
    rows = rows |> arrange(desc(n))  # order by frequency, most frequent first
    map(ks, function(k) {  # for each number of rows to drop
      remaining = if (k == 0) rows else rows |> slice(-seq_len(min(k, nrow(rows))))  # k=0 guarded: slice(-seq_len(0)) drops everything, not nothing
      tibble(k = k, p_back = sum(remaining$n[remaining$is_back]) / sum(remaining$n))  # recompute p(back)
    }) |> bind_rows()  # stack the k values for this genre
  })) |>  # end mutate
  select(genre, sens) |>  # keep genre and the nested sensitivity results
  unnest(sens)  # flatten to one row per genre x k

# write_tsv(sensitivity, 'dat/tab_sensitivity.tsv')  # save the numbers behind fig4

fig4 = ggplot(sensitivity, aes(k, p_back, colour = genre)) +  # rows dropped on x, p(back) on y, one line per genre
  geom_line() +  # trend per genre
  geom_point() +  # mark the actual computed values
  scale_colour_viridis_d(option = 'H') +  # discrete viridis palette
  theme_bw() +  # house plotting theme
  labs(x = 'top-n rows dropped', y = 'P(back)', colour = 'genre',
       title = 'Sensitivity of P(back) to the most frequent word forms')  # axis/legend/title text

# ggsave('viz/fig4_sensitivity.png', fig4, width = 6, height = 4, dpi = 900)  # write figure

# -- fig5: suffix case repertoire by genre -- #

repertoire = mnsz |>  # start from the full table
  summarise(.by = c(genre, suffix), n_total = sum(n)) |>  # sum tokens per case x genre
  mutate(.by = genre, prop = n_total / sum(n_total))  # proportion of each genre's tokens in each case

fig5 = ggplot(repertoire, aes(genre, prop, fill = suffix)) +  # genre on x, stacked proportion by case
  geom_col() +  # stacked bars
  scale_fill_viridis_d(option = 'H') +  # discrete viridis palette, many levels
  theme_bw() +  # house plotting theme
  theme(legend.text = element_text(size = 6), legend.key.size = unit(0.3, 'cm')) +  # shrink legend, 24 cases
  labs(x = 'genre', y = 'proportion of tokens', fill = 'suffix (case)',
       title = 'Suffix case repertoire by genre')  # axis/legend/title text

ggsave('viz/fig5_suffix_repertoire.png', fig5, width = 7, height = 5, dpi = 900)  # write figure

# -- fig6: stem coverage across genres -- #

stem_coverage = mnsz |>  # start from the full table
  distinct(genre, stem) |>  # one row per stem attested in a genre
  count(stem, name = 'n_genres')  # how many genres each stem is attested in

# write_tsv(stem_coverage, 'dat/tab_stem_coverage.tsv')  # save the numbers behind fig6

fig6 = ggplot(stem_coverage, aes(factor(n_genres))) +  # number of genres, as a discrete axis
  geom_bar(fill = 'grey30') +  # count of stems per number of genres covered
  theme_bw() +  # house plotting theme
  labs(x = 'number of genres a stem is attested in', y = 'number of stems',
       title = 'Stem coverage across genres')  # axis/title text

# ggsave('viz/fig6_stem_coverage.png', fig6, width = 5, height = 4, dpi = 900)  # write figure

# -- fig7: top-5 word-form concentration by genre -- #
# high share = genre total is dominated by a handful of hyperfrequent forms

concentration = mnsz |>  # start from the full table
  nest(.by = genre) |>  # one row per genre, with its data nested
  mutate(top5_share = map_dbl(data, function(rows) {  # for each genre's rows
    sum(sort(rows$n, decreasing = TRUE)[1:5]) / sum(rows$n)  # share of total n held by the top 5 forms
  })) |>  # end mutate
  select(genre, top5_share)  # keep genre and the concentration share

# write_tsv(concentration, 'dat/tab_concentration.tsv')  # save the numbers behind fig7

fig7 = ggplot(concentration, aes(genre, top5_share)) +  # genre on x, concentration share on y
  geom_col(fill = 'grey30') +  # bar per genre
  theme_bw() +  # house plotting theme
  labs(x = 'genre', y = 'share of tokens in top 5 word forms',
       title = 'Concentration of genre totals in the 5 most frequent forms')  # axis/title text

# ggsave('viz/fig7_concentration.png', fig7, width = 5, height = 4, dpi = 900)  # write figure

# -- fig8: per-stem difference in P(back), lit vs other genres -- #
# restricted to stems with n >= 10 on both sides, so the difference isn't a small-sample artefact

lit_vs_other = mnsz |>  # start from the full table
  mutate(group = if_else(genre == 'lit', 'lit', 'other')) |>  # collapse to lit vs everything else
  summarise(.by = c(stem, group), n_total = sum(n), n_back = sum(n[is_back])) |>  # sum tokens per stem x group
  mutate(p_back = n_back / n_total) |>  # proportion back per group
  pivot_wider(id_cols = stem, names_from = group, values_from = c(n_total, p_back))  # one row per stem, lit/other side by side

diff_dat = lit_vs_other |>  # start from the wide table
  filter(n_total_lit >= 10, n_total_other >= 10) |>  # keep stems with adequate data on both sides
  mutate(
    diff = p_back_lit - p_back_other,  # difference in P(back), lit minus other
    n_total = n_total_lit + n_total_other,  # total tokens behind this stem's estimate
    label = paste0(stem, ' (n = ', n_total, ')')  # axis label with sample size
  )

# write_tsv(diff_dat, 'dat/tab_lit_vs_other.tsv')  # save the numbers behind fig8

fig8_height = max(4, 0.15 * nrow(diff_dat))  # scale figure height to the number of stems shown

fig8 = ggplot(diff_dat, aes(fct_reorder(label, diff), diff)) +  # stems ordered by effect size
  geom_col(fill = 'grey30') +  # one bar per stem
  geom_hline(yintercept = 0, linetype = 'dashed') +  # zero line: no difference between lit and other
  coord_flip() +  # horizontal bars, labels stay readable
  theme_bw() +  # house plotting theme
  theme(axis.text.y = element_text(size = 6)) +  # shrink stem labels, there are many
  labs(x = NULL, y = 'P(back | lit) - P(back | other)',
       title = 'Difference in P(back) between literature and other genres, by stem')  # axis/title text

# ggsave('viz/fig8_lit_vs_other.png', fig8, width = 8, height = fig8_height, dpi = 900)  # write figure
