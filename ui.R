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
          label = 'For more details on criteria, choose one:',
          choices = c(
            'Flow chart' = 'chart',
            '1.  Does the context of the study relate to justice, 
                 equity, and/or one of the NASA Applied Science themes?' = 'crit1', 
            '2.  Does the title and/or abstract clearly mention 
                 Earth Observation, satellite, or remote sensing data?' = 'crit2', 
            '3a. Does the title and/or abstract indicate that the study 
                 assesses/measures the value of a societal benefit in economic terms 
                 (e.g., monetary value) or utility (e.g., benefit for a defined user(s), 
                 purpose, or decision), as a primary goal or result?' = 'crit3a',
            '3b. Does the title and/or abstract attribute/link the benefit/value to the
                 application/use of a specific dataset or data source?' = 'crit3b',
            '3c. Does the study apply the valuation methodology specifically to an 
                 EO-relevant dataset or data source?' = 'crit3c'),
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
        
        dataTableOutput('crit_table'),
        ### Radio buttons for categorization
        # checkboxGroupInput(
        #   inputId = 'screen_decision',
        #   label = 'Criteria according to title?:',
        #   choices = c('Societal value context' = '1',
        #               'Earth Observation context' = '2',
        #               'Valuation' = '3a',
        #               'Valuation of data' = '3b',
        #               'Valuation of specific EO data' = '3c',
        #               'No criteria met' = 'X',
        #               'Spurious' = 'S',
        #               'Uncertain how to classify' = 'U'),
        #   selected = character(0)
        #   ), ### end of checkboxGroupInput
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
