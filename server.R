
server <- shinyServer(function(input, output) {
  
  ####################################
  ###  Set up criteria info page   ###
  ####################################
  
  output$criteria_long <- reactive({
    f <- switch(input$criteria,
                chart = 'chart.md',
                'crit1' = 'crit_1_long.md',
                'crit2' = 'crit_2_long.md',
                'crit3' = 'crit_3_long.md')
    return(markdown(read_file(f)))
  })

  ####################################
  ###  Read in bibliography files  ###
  ####################################
  
  v <- reactiveValues(bib_all = null_bib,
                      bib_screened = null_bib,
                      bib_toscreen = null_bib,
                      current_doc  = null_bib)
  
  observeEvent(input$bib_file, {
    message('bib_file observeEvent triggered')
    shinyFileChoose(input, 'bib_file', roots = roots, filetypes=c('', 'txt', 'bib', 'ris'),
                    defaultPath='', defaultRoot='wd')
    v$bib_all <- import_refs(input$bib_file, roots)
    print(head(v$bib_all))
  })
    
  observeEvent(input$screened_file, {
    message('screened_file observeEvent triggered')
    shinyFileChoose(input, 'screened_file', roots = roots, filetypes=c('', 'txt', 'bib', 'ris'),
                    defaultPath='', defaultRoot='wd')
    v$bib_screened <- import_refs(input$screened_file, roots)
    print(head(v$bib_screened))  
  }) 
  
  observeEvent(input$merge_bibs, {
    message('in merge_bibs')
    v$bib_toscreen <- anti_join(v$bib_all, v$bib_screened)
    v$current_doc <- v$bib_toscreen %>% slice(1)
    print(head(v$bib_toscreen))  
  })

  output$toscreen_preview <- renderDataTable({
    df <- switch(input$df_preview,
                 all      = v$bib_all,
                 screened = v$bib_screened,
                 toscreen = v$bib_toscreen) 
    if(nrow(df) == 0) 
      df <- null_df
    
    df <- df %>% 
      select(author, title, journal, year)

    DT::datatable(df)
  })
  
  
  ###################################
  ###  Screen and update output   ###
  ###################################
  
  observeEvent(input$skip_doc, {
    message('in skip_doc observeEvent')
    ### update the checkbox input and text input to blank out selections
    updateCheckboxGroupInput(inputId = 'screen_decision', selected = character(0))
    updateTextInput(inputId = 'notes', value = '')
    ### drop the current first row from the bib_toscreen
    v$bib_toscreen <- v$bib_toscreen %>%
      slice(-1)
    ### choose the new first row to operate upon as a new doc
    v$current_doc <- v$bib_toscreen %>%
      slice(1)
  })
  
  observeEvent(input$shuffle, {
    message('in shuffle observeEvent')
    ### update the checkbox input and text input to blank out selections
    updateCheckboxGroupInput(inputId = 'screen_decision', selected = character(0))
    updateTextInput(inputId = 'notes', value = '')

    v$bib_toscreen <- slice_sample(v$bib_toscreen, prop = 1, replace = FALSE)
    v$current_doc <- v$bib_toscreen %>%
      slice(1)
  })
  
  
  observeEvent(input$screen_action, {
    message('in screen_action observeEvent')
    if(length(input$screen_decision) == 0) {
      message('No decision selected! (zero length)')
      return(NULL)
    }
    if(is.null(input$screen_decision)) {
      message('No decision selected! (null)')
      return(NULL)
    }
    ### Translate current doc to RIS and add in a PA (personal note) field with the screening decision
    append_decision(v$current_doc, input$screen_decision, input$notes, input$screened_file, roots)

    ### update the checkbox input and text input to blank out selections
    updateCheckboxGroupInput(inputId = 'screen_decision', selected = character(0))
    updateTextInput(inputId = 'notes', value = '')
    ### drop the current first row from the bib_toscreen
    v$bib_toscreen <- v$bib_toscreen %>%
      slice(-1)
    ### choose the new first row to operate upon as a new doc
    v$current_doc <- v$bib_toscreen %>%
      slice(1)
  })
  
  output$doc_fields_text <- renderUI({
    # browser()
    ### output to display selected doc for screening: highlight search terms in title and abstract
    title <- v$current_doc$title %>% str_to_sentence() %>% str_remove_all('\\{|\\}')
    title_out <- embolden(text = title) %>%
      str_replace_all('p>', 'h3>') ### turn into a header instead of paragraph

    author <- v$current$author %>% str_to_title() %>% markdown()
    journal <- v$current_doc$journal %>% str_to_title() %>% markdown()
    abstract <- embolden(text = v$current_doc$abstract)
    html_out <- paste(title_out, '<hr>', author, journal, '<hr>', abstract)
    return(HTML(html_out))
  })
  
})
