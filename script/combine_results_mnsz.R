# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')  # project root

library(tidyverse)  # data wrangling and I/O

# -- body -- #

words = read_tsv('~/Github/Racz2027vh/dat/real_words_krr_lo_preds.tsv')  # full word-level KRR table: predictions, log-odds, borrowing info
corpus = read_tsv('dat/dat_long.tsv')  # corpus-derived stem/suffix reference table
names_list = list.files('dat') |>  # list all files in dat/
  str_extract('mnsz.*') |>  # keep only filenames starting with "mnsz"
  na.omit()  # drop non-matches (files that aren't mnsz exports)

mnsz = map(names_list, ~ read_csv(paste0('dat/', .), skip = 4) |>  # read each mnsz csv, skipping the 4-line header
      mutate(file = .)  # tag rows with source filename before binding
    ) |>
  bind_rows() |>  # stack all files into one table
  mutate(
    genre = str_extract(file, '(?<=\\_)[a-z]+(?=\\.csv)'),  # pull genre label out of the filename
    word = str_to_lower(KWIC) |>  # normalise case
      str_remove_all('[-”“]')  # strip stray punctuation from the KWIC field
  ) |>
  count(word,genre)  # tally occurrences by word and genre

corpus_matcher = corpus |>
  rename(
    word = form  # align join key name with mnsz$word
         ) |>
  select(word,stem,lfpm10,llfpm10,suffix_vowel,suffix_initial,linking_vowel,suffix,suffixes,examples)  # keep only columns needed for the join

d = mnsz |>
  inner_join(corpus_matcher, by = 'word') |>  # attach stem-level corpus info; inner join drops words with no corpus match
  inner_join(words, by = 'stem')  # attach the full KRR table (predictions, log-odds, borrowing_language, etc.); inner join drops stems with no entry

write_tsv(d, 'dat/mnsz_counts.tsv')  # export combined counts table
