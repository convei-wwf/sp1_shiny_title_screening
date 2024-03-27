### The app reads in a bibliography file and displays a title, author, year, journal, 
### and which search terms appear in the abstract, to give a little context.
### Criteria are displayed and the user can classify accordingly, then
### click "next" to move on to the next doc.

library(shiny)
library(shinyFiles)
library(tidyverse)
library(DT)
library(here)
library(synthesisr)
library(tidytext)

roots <- c(wd = here::here()) ### for shinyFileChoose etc

clean_bib <- function(bib_df, ext = '.ris') {
  if(nrow(bib_df) == 0) bib_df <- null_bib

  bib_clean_df <- bib_df %>%
    janitor::clean_names() %>%
    select(source_type, author, title, journal, year, abstract) %>%
    ### select the first author last name only
    mutate(author = str_remove_all(author, ',.+')) %>%
    # rename(first_author = author) %>%
    mutate(title = str_remove_all(title, '\\{|\\}')) %>%
    mutate(title = str_to_sentence(title),
           journal = str_to_title(journal),
           author = str_to_title(author))
    
  return(bib_clean_df)
    
}

null_bib <- data.frame(source_type = NA, author = NA, title = NA, 
                       journal = NA, year = NA, abstract = NA)

import_refs <- function(file_select, roots) {
  message('in import_refs()')
  req(file_select)
  if (is.null(file_select))
    return(NULL)    
  
  f <- parseFilePaths(roots, file_select)$datapath
  if(length(f) == 0) return(null_bib)
  if(!is.na(f) & file.size(f) > 0) {
    return(read_refs(f))
  } else return(null_bib)
}

append_decision <- function(current_doc, decision, notes, file_select, roots) {
  ### Translate current doc to RIS and add in a PA (personal note) field with the screening decision
  current_doc <- current_doc %>%
    select(source_type, author, title, journal, year)
  ### collapse decision criteria letters to a single block; if none selected, assign "unsure"
  if(length(decision) == 0) decision <- 'E'
  decision <- paste0(decision, collapse = '')
  
  out_ris <- write_refs(current_doc, format = 'ris', file = FALSE) %>%
    paste0(collapse = '\n') %>%
    str_replace('ER  -', paste('SD  -', decision, '\nER  -\n\n'))
  
  if(notes != '') {
    out_ris <- out_ris %>%
      str_replace('ER  -', paste('NO  -', notes, '\nER  -\n\n'))
  }
  
  message(out_ris)
  
  ### Append the out_ris with EXTRA field to the screened output file
  req(file_select)
  if (is.null(file_select))
    stop('No output file selected!')  
  
  f <- parseFilePaths(roots, file_select)$datapath
  write_file(out_ris, f, append = TRUE)
  
}

### stitch a search term string
esi_terms <- 'satellite|space.based|remote observation|remote sensing|earth observation|remotely.sens[a-z]+|modis|landsat|sentinel'
dec_terms <- 'decision|optimization|risk analysis|management|policy|cost.benefit analysis|benefit.cost analysis|investment|contingent valuation|counterfactual|value of information'
value_terms <- 'value|valuation|benefit|utility'
social_terms <- 'social|societal|cultural|([a-z]+-?)?economic|environmental|ecosystem service|sustainable development|protected area|heritage site|non.?use value'
appl_sci_terms <- 'capacity.building|climate|resilience|disaster|health|air quality|water|resources|ecolog[a-z]+|conserv[a-z]+|agricultur[a-z]+|wild[a-z]* ?fire'

search_terms <- paste(esi_terms, dec_terms, value_terms, social_terms, sep = '|')

embolden <- function(text, terms = search_terms) {
  indices <- str_locate_all(tolower(text), terms) 
  ### increase index of end positions:
  indices[[1]][ , 2] <- indices[[1]][ , 2] + 1
  ### set up as vector and go from the end to the start!
  i_vec <- indices %>% unlist() %>% sort(decreasing = TRUE)
  text_sub <- str_to_sentence(text)
  for(i in i_vec) {
    ### i <- 7
    stringi::stri_sub(text_sub, i, i-1) <- '**'
  }
  text_out <- markdown(text_sub) %>%
    str_replace_all('<strong>', '<strong style="color:#FF0000";>')
  return(text_out)
}

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

all_df <- read_refs(here('_data/title_screen_sample1000.ris'))
screened_fs <- list.files(here('_data'), pattern = 'screened_', full.names = TRUE)
screened_df <- combine_screened(all_df, screened_fs)

screened_df %>%
  filter(!is.na(screened_cco2) & !is.na(screened_mbs)) %>%
  select(screened_cco2, screened_mbs) %>%
  table()

screened_df %>%
  filter(!is.na(screened_cco2) & !is.na(screened_sc)) %>%
  select(screened_cco2, screened_sc) %>%
  table()

screened_df %>%
  filter(!is.na(screened_sc) & !is.na(screened_mbs)) %>%
  select(screened_sc, screened_mbs) %>%
  table()

screen_to_check_df <- screened_df %>%
  filter(!is.na(screened_cco2)) %>%
  filter(!is.na(screened_sc) | !is.na(screened_mbs)) %>%
  mutate(mismatch = (screened_cco2 != screened_sc | screened_cco2 != screened_mbs)) %>%
  filter(mismatch) %>%
  select(author, title, journal, year, abstract, starts_with('screened'))
  
### Screening for bigrams in abstract including "sentinel" and "satellite"
abstr_bigram_df <- read_refs(here('_data/title_screen_sample1000.ris')) %>%
  select(author, title, abstract) %>%
  unnest_tokens(output = abstr_sentence, input = abstract, token = 'sentences', drop = FALSE) %>%
  unnest_tokens(output = abstr_bigram, input = abstr_sentence, token = 'ngrams', n = 2) %>%
  filter(str_detect(abstr_bigram, '^satellite|^sentinel')) %>%
  group_by(abstr_bigram) %>%
  mutate(idf = 1/n_distinct(title)) %>%
  group_by(author, title, abstract, abstr_bigram) %>%
  summarize(idf = first(idf), tf = n(), .groups = 'drop') %>%
  mutate(tf_idf = tf * idf)

title_bigram_df <- read_refs(here('_data/title_screen_sample1000.ris')) %>%
  select(author, title) %>%
  unnest_tokens(output = title_bigram, input = title, token = 'ngrams', n = 2, drop = FALSE) %>%
  filter(str_detect(title_bigram, '^satellite|^sentinel')) %>%
  group_by(title_bigram) %>%
  mutate(idf = 1/n_distinct(title)) %>%
  group_by(author, title, title_bigram) %>%
  summarize(idf = first(idf), tf = n(), .groups = 'drop') %>%
  mutate(tf_idf = tf * idf)

problem_bigrams <- c('sentinel study', 'sentinel node', 'sentinel site', 
                     'sentinel surve', 'sentinel lymph', 'sentinel pig', 
                     'sentinel species', 'satellite account', 'sentinel behavior',
                     'sentinel catalyst', 'satellite office', 'sentinel event') %>%
  paste(collapse = '|')
drop_df <- all_df %>%
  filter(str_detect(tolower(title), problem_bigrams) | str_detect(tolower(abstract), problem_bigrams))
