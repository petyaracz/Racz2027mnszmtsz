# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

library(tidyverse)

# -- read -- #

dat = read_tsv('dat/real_words_krr_lo_preds.tsv')

# -- translate labels -- #

lang_hu = c(
  'English' = 'angol',
  'French' = 'francia',
  'German' = 'német',
  'Latin' = 'latin',
  'Yiddish' = 'jiddis',
  'other' = 'egyéb'
)

period_hu = c(
  'before 16th c' = '16. század előtt',
  '16th c' = '16. század',
  '17th c' = '17. század',
  '18th c' = '18. század',
  '19th c' = '19. század',
  '20th c' = '20. század',
  'unknown' = 'ismeretlen'
)

# -- wrangle -- #

tab = dat |>
  select(stem, stem_freq, log_odds_back, borrowing_language2, borrowing_period2, borrowing_category) |>
  mutate(
    borrowing_language2 = str_remove(borrowing_language2, ' \\(n = [0-9]+\\)'),
    borrowing_period2 = str_remove(borrowing_period2, ' \\(n = [0-9]+\\)'),
    borrowing_language2 = lang_hu[borrowing_language2],
    borrowing_period2 = period_hu[borrowing_period2],
    borrowing_category = ifelse(is.na(borrowing_category), '–', borrowing_category),
    log_odds_back = round(log_odds_back, 2)
  ) |>
  arrange(stem)

# -- escape latex special characters in free-text fields -- #

latex_escape = function(x) {
  x |>
    str_replace_all('&', '\\\\&') |>
    str_replace_all('_', '\\\\_')
}

tab = tab |>
  mutate(
    borrowing_category = latex_escape(borrowing_category)
  )

# -- write longtable fragment -- #

header = c(
  '\\begin{longtable}{l r r l l l}',
  '\\caption{A vizsgált tövek gyakorisága, hátsó/elöl log-odds-a és etimológiai adatai.}',
  '\\label{tab:appendix-etym} \\\\',
  '\\hline',
  'T\\H{o} & Gyakoris\\\'ag & Log-odds (h\\\'ats\\\'o) & Nyelv & Kor & Kategória \\\\',
  '\\hline',
  '\\endfirsthead',
  '\\multicolumn{6}{l}{\\textit{-- folytat\\\'as az el\\H{o}z\\H{o} oldalr\\\'ol}} \\\\',
  '\\hline',
  'T\\H{o} & Gyakoris\\\'ag & Log-odds (h\\\'ats\\\'o) & Nyelv & Kor & Kategória \\\\',
  '\\hline',
  '\\endhead',
  '\\hline',
  '\\endfoot',
  '\\hline',
  '\\endlastfoot'
)

body = tab |>
  mutate(
    row = str_c(stem, ' & ', stem_freq, ' & ', log_odds_back, ' & ',
                borrowing_language2, ' & ', borrowing_period2, ' & ',
                borrowing_category, ' \\\\')
  ) |>
  pull(row)

footer = '\\end{longtable}'

writeLines(c(header, body, footer), 'latex/tab_appendix_etym.tex')
