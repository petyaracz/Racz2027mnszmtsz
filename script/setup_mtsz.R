# -- head -- #

setwd('~/Github/Racz2027mnszmtsz/')

library(tidyverse)

# -- read -- #

# https://clara.nytud.hu/mtsz hits for words in s in (...|...) format
hits_lines = read_lines('dat/mtsz_Racz2027vh_hits.txt')
s = read_tsv('~/Github/Racz2027vh/dat/real_words_krr_lo_preds.tsv')

# -- parse hits metadata + concordance -- #

# keep only concordance rows: metadata TAB concordance. drops the header/comment
# lines (starting with #) and the blank separator lines at the top of the file
data_lines = hits_lines[str_detect(hits_lines, '\t')]

meta_str = str_split_fixed(data_lines, '\t', 2)[, 1]
conc_str = str_split_fixed(data_lines, '\t', 2)[, 2]

# doc.lform_field codes. when only one of doc.lform_register / doc.lform_field is
# present in a row, this (closed, observed-in-data) vocabulary tells them apart:
# a token in this list is the field, anything else is the register
field_codes = c('Anat', 'Ásv', 'Bánya', 'Biol', 'Csill', 'Ép', 'Erd', 'Fil', 'Film', 'Fiz',
                 'Föld', 'Földr', 'Fotó', 'Gépk', 'Hajó', 'Inf', 'Ipar', 'irodt', 'Irodt', 'Isk',
                 'Jog', 'Kat', 'Kém', 'Ker', 'Konyha', 'Könyv', 'Körny', 'Közg', 'Közig', 'Közl',
                 'Lov', 'Mat', 'Média', 'Met', 'Mezőg', 'Mit', 'Műsz', 'Műv', 'Népr', 'Növ',
                 'Nyelvt', 'Nyelvt.', 'Orv', 'Orvos', 'Pénz', 'Pol', 'Posta', 'Pszich', 'Pszicho',
                 'Rep', 'Sp', 'Sport', 'Szính', 'Szoc', 'Szőlő', 'Tör', 'Tört', 'Tud', 'Űrh', 'Vad',
                 'Vall', 'Vasút', 'Vill', 'Víz', 'Zene')

# metadata is author?,lform_rhythm,lform_fiction,lform_register?,lform_field?,wdate
# author and the register/field pair are independently optional, so the comma count
# per row varies (3-6 fields). author is missing whenever the first field is "0"/"1"
# (i.e. it is actually lform_rhythm, not a name)
parse_meta = function(x) {
  f = str_split(x, ',')[[1]]
  author_present = !(f[1] %in% c('0', '1'))
  author = if (author_present) f[1] else NA_character_
  rest = if (author_present) f[-1] else f
  n = length(rest)
  lform_rhythm = rest[1]
  lform_fiction = rest[2]
  wdate = rest[n]
  if (n == 3) {
    lform_register = NA_character_
    lform_field = NA_character_
  } else if (n == 4) {
    tok = rest[3]
    if (tok %in% field_codes) {
      lform_register = NA_character_
      lform_field = tok
    } else {
      lform_register = tok
      lform_field = NA_character_
    }
  } else {
    lform_register = rest[3]
    lform_field = rest[4]
  }
  tibble(author = author, lform_rhythm = lform_rhythm, lform_fiction = lform_fiction,
         lform_register = lform_register, lform_field = lform_field, wdate = wdate)
}

meta = map_dfr(meta_str, parse_meta)

# concordance is "left context < target > right context"
conc = str_match(conc_str, '^(.*)<\\s*(.*?)\\s*>(.*)$')
d = meta |>
  mutate(left = str_trim(conc[, 2]), target = str_trim(conc[, 3]), right = str_trim(conc[, 4]))

# -- match target word to stem, derive suffix, join predictors -- #

# dat/words.txt holds the stem alternation (stem1|stem2|...|stemN) pulled from the
# hits query regex. the original query matched this alternation anywhere inside the
# hit word (e.g. "mater" inside "immateriálitásnak"), which also wrongly matches a
# stem occurring mid-word in an unrelated word (e.g. "lumen" inside "Blumen"). here
# the pattern is anchored to only match: (1) at the start of target -- the word has
# "whitespace before" it, so a stem can't match mid-word -- and (2) followed by at
# least one more character -- the word has no "whitespace after" it, so the bare,
# unsuffixed stem (nothing following) doesn't match, only suffixed forms do
stem_alt = read_lines('dat/words.txt')[1]
stem_pattern = regex(paste0('^', stem_alt, '(.+)$'), ignore_case = TRUE)

stem_match = str_match(d$target, stem_pattern)
d$stem = str_to_lower(stem_match[, 2])
d$suffix = stem_match[, 3]

# suffix vowel harmony: back vowels vs front vowels (incl. the neutral i/í)
d = d |>
  mutate(suffix_harmony = case_when(
    str_detect(suffix, '[aáoóuú]') ~ 'back',
    str_detect(suffix, '[eéöőüűií]') ~ 'front',
    TRUE ~ NA_character_
  ))

d = d |> inner_join(s, by = 'stem')

# -- write -- #

write_tsv(d, 'dat/mtsz_Racz2027vh_hits_wrangled.tsv')
