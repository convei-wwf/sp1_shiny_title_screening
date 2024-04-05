library(tidyverse)
library(here)
library(synthesisr)

all_df <- read_refs(here('_data/sample1000.ris'))
screened_fs <- list.files(here('_data'), pattern = 'screen2_', full.names = TRUE)
screened_fs <- screened_fs[file.size(screened_fs) > 0]

combine_screened <- function(df, files) {
  for(f in files) {
    col_name <- basename(f) %>% str_remove_all('.ris')
    comment_name <- paste0('comments_', str_remove(col_name, '.+_'))
    
    screened <- read_refs(f) %>%
      rename(!!col_name := sequence_data, 
             !!comment_name := comments)
    df <- left_join(df, screened)
  }
  df_out <- df %>%
    pivot_longer(names_to = 'name', values_to = 'val', 
                 cols = contains(c('screen', 'comment'))) %>%
    filter(!is.na(val)) %>%
    pivot_wider(names_from = 'name', values_from = 'val')
  return(df_out)
}

screened_df <- combine_screened(all_df, screened_fs) %>%
  select(-doi, -keywords) %>%
  setNames(str_remove(names(.), 'screen2_'))

check_screened <- screened_df %>%
  pivot_longer(cols = c(cco, mbs, sc)) %>%
  filter(!is.na(value)) %>%
  mutate(decision = case_when(str_detect(value, 'N') ~ 'exclude',
                              str_detect(value, '\\?|_') ~ 'uncertain',
                              value == 'YYYYY' ~ 'include',
                              TRUE ~ 'huh?')) %>%
  group_by(title, author, year) %>%
  filter(n() > 1) %>%
  mutate(match = n_distinct(decision) == 1) %>%
  ungroup()

agree_pct <- check_screened %>%
  select(author, title, year, match) %>%
  distinct() %>%
  summarize(agree = sum(match) / n()) %>%
  .$agree

disagree <- check_screened %>%
  filter(!match)

conf_mtx <- check_screened %>%
  select(-value) %>%
  pivot_wider(names_from = name, values_from = decision)

table(conf_mtx %>% select(cco, mbs, sc))

rescreen <- all_df %>%
  inner_join(check_screened %>% 
               filter(!match) %>% 
               select(author, title) %>% 
               distinct())

rescreen_ris <- write_refs(rescreen, format = 'ris') %>%
  paste0(collapse = '\n') %>%
  write_file(here('_data/rescreen_240404.ris'))

set.seed(42)
new_subsample <- all_df %>%
  slice_sample(n = 100)

newsample_ris <- write_refs(new_subsample, format = 'ris') %>%
  paste0(collapse = '\n') %>%
  write_file(here('_data/newsample100_240404.ris'))
