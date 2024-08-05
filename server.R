
server <- shinyServer(function(input, output) {
  
  ####################################
  ###  Set up criteria info page   ###
  ####################################
  
  output$criteria_long <- reactive({
    f <- switch(input$criteria,
                chart = 'chart.md',
                'crit1' = 'crit_1_long.md',
                'crit2' = 'crit_2_long.md',
                'crit3a' = c('crit_3_long.md', 'crit_3a_long.md'),
                'crit3b' = c('crit_3_long.md', 'crit_3b_long.md'),
                'crit3c' = c('crit_3_long.md', 'crit_3c_long.md'))
    md <- sapply(f, read_file) %>% paste(collapse = '\n\n')
    return(markdown(md))
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
    v$bib_toscreen <- v$bib_all
    v$current_doc <- v$bib_toscreen %>%
      slice(1)
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
  
  
  #####################################
  ###  Set up screening checkboxes  ###
  #####################################
  
  ### The proxy to update the DT
  proxy <- dataTableProxy('crit_table')
  
  #The initial data for the checkboxes
  checkboxes <- data.frame(
    Y = c(NA, NA, NA, NA, NA),
    N = c(NA, NA, NA, NA, NA),
    U = c(NA, NA, NA, NA, NA),
    row.names = paste('Crit. ', c('1', '2', '3a', '3b', '3c'))
  )
  
  ### The reactive version of the data
  tableData = reactiveValues(checkboxes = checkboxes)
  
  ### Update the table when clicked
  observeEvent(req(input$crit_table_cells_selected), {
    tableData$checkboxes[input$crit_table_cells_selected] =
      ifelse(is.na(tableData$checkboxes[input$crit_table_cells_selected]),
             as.character(icon("ok", lib = "glyphicon")), NA)
    
    ### Send proxy (no need to refresh whole table)
    replaceData(proxy, tableData$checkboxes)
    
  })
  
  ### The "checkbox" table
  output$crit_table = renderDataTable({
    checkboxes
  }, 
    ### These are options to make the table look like checkboxes
    selection = list(mode = "single", target = 'cell'), 
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      dom = "t", ordering = F
    ),
    escape = F
  )

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
    
    check_df <- tableData$checkboxes
    if(sum(!is.na(check_df)) == 0) {
      message('No decision selected! (zero length)')
      return(NULL)
    }
    
    ### Translate current doc to RIS and add in a PA (personal note) 
    ### field with the screening decision
    checkbox_summary <- summarize_checks(check_df)
    append_decision(v$current_doc, checkbox_summary, input$notes, 
                    file_select = input$screened_file, roots = roots)

    ### clear checkboxes and text input to blank out selections
    tableData$checkboxes <- checkboxes
    replaceData(proxy, tableData$checkboxes)
    
    updateTextInput(inputId = 'notes', value = '')
    
    ### drop the current first row from the bib_toscreen
    v$bib_toscreen <- v$bib_toscreen %>%
      slice(-1)
    ### choose the new first row to operate upon as a new doc
    v$current_doc <- v$bib_toscreen %>%
      slice(1)
  })
  
  output$doc_fields_text <- renderUI({
    ### output to display selected doc for screening: highlight search terms 
    ### in title and abstract
    
    ### clean up title; if all caps or all lower case, convert to sentence, 
    ### then embolden it
    title <- v$current_doc$title %>% str_remove_all('\\{|\\}')
    if(title == toupper(title) | title == tolower(title)) {
      title <- str_to_sentence(title)
    }
    title_out <- embolden(text = title) %>%
      str_replace_all('p>', 'h3>') ### turn into a header instead of paragraph

    ### clean up doi for inclusion as a link
    doi <- v$current_doc$doi
    if(is.na(doi)) {
      doi <- ''
    } else {
      doi <- sprintf('<a href="%s">%s</a>', doi, doi)
    }
    
    ### Clean up author and journal
    author <- v$current_doc$author %>% str_to_title() %>% markdown()
    journal <- v$current_doc$journal %>% str_to_title() %>% markdown()
    
    ### embolden the abstract
    abstract <- embolden(text = v$current_doc$abstract)
    
    ### combine it all together!
    html_out <- paste(title_out, '<hr>', author, journal, doi, '<hr>', abstract)
    return(HTML(html_out))
  })
  
})
