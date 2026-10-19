# -- head -- #

setwd('~/Documents/R/vh_misc/vh2/')  # project root

library(tidyverse)  # data wrangling and I/O

# -- body -- #

d = read_tsv('dat/real_words_krr_lo_preds.tsv')  # word list with predicted log-odds

stems = d |>  # take the word list
  pull(stem)  # extract stem column as a character vector

# suffixes = '[bjnr]?[ae][nk]?|h?[oeö][knz]?|[br][óő]l'  # compressed suffix regex (disabled below)

# query = as_vector(outer(stems,suffixes,paste0)) |>  # cross stems with suffixes to build inflected forms
  # paste(collapse = '|')  # collapse into a single alternation
# NB: disabled for now -- query matches bare stems only, suffix expansion not needed yet

query = paste(stems, collapse = '|')  # build regex alternation over stems only

query = paste0('(',query,')')  # wrap in a capture group for the query field

write_lines(query, 'dat/query_fotel.txt')  # write query to file for use in the regex search field
