ui <- navbarPage(
  title = "Shiny Title Screening",
  # theme = bslib::bs_theme(bootswatch = 'morph'),
  
  
  # ### First panel: Welcome/introduction
  tabPanel(title = 'Welcome',
    ### Sidebar with a DT::datatable of the entire bib set?
    fluidRow(
      column(width = 3,
        h5('Choose a .ris bibliography file:'),
        shinyFilesButton('bib_file', 'File select', 'Please select a file', FALSE),
        
        
        h5('Choose .ris file for screened results (create empty .ris file to begin):'),
        shinyFilesButton('screened_file', 'File select', 'Please select a file', FALSE),
        
        
        h5('Remove already-screened results from bibliography:'),
        actionButton(
          inputId = 'merge_bibs',
          label = 'Prep to screen', icon = icon('book')),
        
        radioButtons(
          inputId = 'df_preview',
          label = 'Preview:',
          choices = c('all', 'screened', 'to screen' = 'toscreen'),
          selected = 'all'),
       ), ### end column 1
             
       ### Show a preview of the loaded bibtex
       column(width = 9,
              DTOutput('toscreen_preview')
       ) ### end main panel
     )
  ), ### end Welcome tabPanel
  
  ### Second panel: criteria
  tabPanel(title = 'Criteria',
    fluidRow(
      column(width = 2),
      column(width = 8,
        includeMarkdown('criteria_long.md'))
    )
  ), ### end tabPanel 2: screening criteria

  ### Third panel: perform the screening
  tabPanel(title = 'Screening',

    ### Sidebar with checkboxes for title screening criteria
    sidebarLayout(
      sidebarPanel(
        includeMarkdown('criteria_short.md'),

        ### Radio buttons for categorization
        checkboxGroupInput(
          inputId = 'screen_decision',
          label = 'Criteria according to title?:',
          choices = c('Explicit valuation' = "A",
                      'Earth Observation context' = 'B',
                      'Societal value context' = 'C',
                      'Spurious/no criteria met' = 'D',
                      'Uncertain how to classify' = 'E'),
          selected = character(0)
          ), ### end of checkboxGroupInput
        ### * Append record to an output file with categorization in extra field
        # actionButton(
        #   inputId = 'reveal_abstr',
        #   label = 'Reveal abstract?'
        # ),
        actionButton(
          inputId = 'screen_action',
          label = 'Log it!'
        ),
        hr(),
        actionButton(
          inputId = 'skip_doc',
          label = 'Skip document!'
        )
      ), ### end sidebar panel

      ### Show information on a selected title
      mainPanel(
        htmlOutput('doc_fields_text')
      ) ### end main panel

    ) ### end sidebarLayout
  ) ### end tabPanel for screening
)
