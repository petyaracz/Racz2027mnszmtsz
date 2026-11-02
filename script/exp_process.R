# combine and filter noun task data per AsPredicted #315,048.
# raw data are not available in the anonymous supplement.

# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

library(tidyverse)
library(glue)

# -- read files -- #

pretest = read_tsv('dat/nonword_pretest.tsv')

path = '~/Gitlab/noun-task-index/data/'

d = tibble(
  path = glue('{path}{list.files(path)}'),
  start_time = str_extract(path, '(?<=SESSION_).*(?=\\.csv$)')
) |>
  filter(str_detect(path, 'noun_guy')) |>  # this label is inserted in the filename upon completion
  mutate(
    data = map(path, read_csv, show_col_types = F)
  )

# -- add info -- #

# a real neptun kód is six uppercase letters/digits; trial runs (proba1, proba2_nyomi_font, ...) fail this
isNeptun = function(x) str_detect(x, '^[A-Z0-9]{6}$')

doPart = function(dat){

  observations = dat |>
    filter(trial_kind == 'noun trial') |>
    arrange(trial_index) |>
    mutate(
      i = 1:n(),
      accept = case_when(
        response == 'f' ~ F,
        response == 'j' ~ T
      ),
      rt = as.double(rt),
      total_time = sum(rt),
      font = typeface
    ) |>
    select(target, suffix_type, noun_class, item_name, font, accept, i, rt, total_time)

  metadata = dat |>
    filter(trial_type == 'survey-text') |>
    pull(response)

  observations |>
    mutate(
      id = str_extract(metadata, '(?<=\\{\\\"Q0\\\"\\:\\\").*(?=\\\",\\\"Q1)'),
      yob = str_extract(metadata, '(?<=Q1\\\"\\:\\\")[0-9]+') |> as.double(),
      gender = str_extract(metadata, '(?<=Q2\\\"\\:\\\").*(?=\\\"\\})'),
      age = as.double(format(Sys.Date(), '%Y')) - yob
    ) |>
    select(id, yob, age, gender, font, total_time, i, target, suffix_type, noun_class, item_name, accept, rt)
}

# -- format d -- #

d = d |>
  mutate(
    data2 = map(data, doPart)
  ) |>
  select(data2) |>
  unnest(data2) |>
  filter(isNeptun(id)) # drop trial runs (proba1, proba2_nyomi_font, ...) that slipped into noun_guy files

# -- add pretest info -- #

pretest = rename(pretest, target = word)

d = left_join(d,pretest, by = join_by(target))

# -- drop things -- #

# trials over 4s excluded, participants faster / slower than median completion time +- 3 mean absolute
# deviations excluded, participants rejecting every form excluded

unfilt = d |>
  summarise(
    median_time = median(total_time),
    mad_time = mad(total_time)
  ) |>
  mutate(
    upper_time = median_time + 3 * mad_time,
    lower_time = median_time - 3 * mad_time
  ) |>
  select(upper_time, lower_time) |>
  bind_cols(d) |>
  relocate(lower_time, .after = rt) |>
  relocate(upper_time, .after = rt)

glue('{length(unique(unfilt$id))} participants in total')

keep_time_ids = unfilt |>
  filter(total_time > lower_time, total_time < upper_time) |>
  distinct(id) |>
  pull(id)

glue('{length(unique(keep_time_ids))} participants within completion time bounds')

keep_accept_ids = unfilt |>
  count(id, accept) |>
  filter(accept) |>
  pull(id)

glue('{length(unique(keep_accept_ids))} participants who accepted anything')

filt = unfilt |>
  filter(
    rt < 4000,
    id %in% keep_time_ids,
    id %in% keep_accept_ids
  )

glue('{length(unique(filt$id))} participants remain after filtering')

# -- write -- #

write_tsv(unfilt, 'dat/noun_task_unfiltered.tsv')
write_tsv(filt, 'dat/noun_task_filtered.tsv')
