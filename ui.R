ui <- navbarPage(
  title = "Shiny Title Screening",
  # theme = bslib::bs_theme(bootswatch = 'morph'),
  
  ### First panel: criteria
  tabPanel(title = 'Criteria',
    fluidRow(
      column(
        width = 12,
        h2('Overview'),
        includeMarkdown('criteria_overview.md')
      )
    ),
    fluidRow(
      column(
        width = 4,
        h3('Criteria'),
         radioButtons(
           inputId = 'criteria',
           label = NULL,
           choices = c(
             'Flow chart' = 'chart',
             '1. Does the context of the study relate to justice, 
                 equity, and/or one of the NASA Applied Science themes?' = 'crit1', 
             '2. Does the title and/or abstract clearly mention 
                 Earth Observation/satellite/remote sensing data?' = 'crit2', 
             '3a. Does the title and/or abstract explicitly mention 
                 valuation of information/data as a primary goal/result?' = 'crit3',
             '3b. Does the study apply the valuation methodology
                 to EO information/data?' = 'crit3'),
           selected = 'chart')
      ),
      column(
        width = 8,
        uiOutput('criteria_long')
      )
    )
  ), ### end tabPanel 2: screening criteria
  
  ### Second panel: set up data
  tabPanel(title = 'Setup',
    ### Sidebar with a DT::datatable of the entire bib set?
    fluidRow(
      column(width = 3,
        h5('Choose a .ris bibliography file:'),
        shinyFilesButton('bib_file', 'File select', 'Please select a file', FALSE),
        
        h5('Choose .ris file for screened results (to start, rename screened_blank.ris with your initials):'),
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
          choices = c('Societal value context' = '1',
                      'Earth Observation context' = '2',
                      'Valuation of data' = '3a',
                      'Valuation of EO data' = '3b',
                      'No criteria met' = 'X',
                      'Spurious' = 'S',
                      'Uncertain how to classify' = 'U'),
          selected = character(0)
          ), ### end of checkboxGroupInput
        textInput(
          inputId = 'notes',
          label = 'Notes:',
          value = ''
        ),
        actionButton(
          inputId = 'screen_action',
          label = 'Log it!'
        ),
        hr(),
        actionButton(
          inputId = 'skip_doc',
          label = 'Skip document!'
        ),
        actionButton(
          inputId = 'shuffle',
          label = 'Shuffle the docs!'
        )
      ), ### end sidebar panel

      ### Show information on a selected title
      mainPanel(
        htmlOutput('doc_fields_text')
      ) ### end main panel

    ) ### end sidebarLayout
  ) ### end tabPanel for screening
  
)
