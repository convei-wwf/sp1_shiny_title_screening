
server <- shinyServer(function(input, output) {

  v <- reactiveValues(bib_all = null_bib,
                      bib_screened = null_bib,
                      bib_toscreen = null_bib,
                      current_doc  = null_bib)
  
  ####################################
  ###  Read in bibliography files  ###
  ####################################
  
  observeEvent(input$bib_file, {
    message('bib_file observeEvent triggered')
    
    roots <- c(wd = here::here())
    shinyFileChoose(input, 'bib_file', roots = roots, filetypes=c('', 'txt', 'bib', 'ris'),
                    defaultPath='', defaultRoot='wd')
    v$bib_all <- import_refs(input$bib_file, roots)
    print(head(v$bib_all))
  })
    
  observeEvent(input$screened_file, {
    message('screened_file observeEvent triggered')
    roots <- c(wd = here::here())
    shinyFileChoose(input, 'screened_file', roots = roots, filetypes=c('', 'txt', 'bib', 'ris'),
                    defaultPath='', defaultRoot='wd')
    
    v$bib_screened <- import_refs(input$screened_file, roots)
    print(head(v$bib_screened))  
  }) 
  
  observeEvent(input$merge_bibs, {
    message('in bib_toscreen')
    v$bib_toscreen <- anti_join(v$bib_all, v$bib_screened)
    v$current_doc <- v$bib_toscreen %>% slice(1)
    print(head(v$bib_toscreen))  
  })

  output$toscreen_preview <- renderDataTable({
    df <- switch(input$df_preview,
                 all      = v$bib_all,
                 screened = v$bib_screened,
                 toscreen = v$bib_toscreen) %>%
      select(author, title, journal, year)

    DT::datatable(df)
  })
  
})
