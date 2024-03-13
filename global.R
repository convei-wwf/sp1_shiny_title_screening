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

