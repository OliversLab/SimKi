dashboardPage(
  dashboardHeader(title = "SimKi v1.0 by O.Scherf-Clavel (C) 2021",
                  titleWidth = 450),
  
  ## Sidebar
  dashboardSidebar(
    menuItem(span("Hospital data", style = "font-size: 14px"),startExpanded = TRUE, icon = icon("hospital-user"),
             textInput("hospID", "Hospital name:"),
             textInput("wardID", "Ward:")
             ),
    menuItem(span("Drug Therapy", style = "font-size: 14px"),startExpanded = TRUE,
             icon = icon("tablets"),
             selectInput("SELECT_DRUG", label="Substance:", 
                         choices = list("Afatinib" = 1,
                                        "Axitinib" = 2,
                                        "Cabozantinib" = 3,
                                        "Dabrafenib" = 4,
                                        "Nilotinib" = 5,
                                        "Ruxolitinib" = 6,
                                        "Trametinib" = 7)
                         )
             
             ),
    menuItem(span("Patient", style = "font-size: 14px"),startExpanded = TRUE,
             icon = icon("procedures"),
             textInput("pat_ID", "Patient ID:"),
             textInput("pat_AGE", "Age:"),
             selectInput("pat_SEX", label="Sex:", 
                         choices = list("male" = 1,
                                        "female" = 0)
                         )
             ),

    menuItem(span("Comment", style = "font-size: 14px"), startExpanded = TRUE,
             icon = icon("comment-alt"),
             textAreaInput("comment", ""))
    
  ),
  
  ## Main body
  dashboardBody(
    tags$head(        
      #in line styling
      tags$style(HTML(
        
        #siderbar background
        ".sidebar-menu li a { font-size: 30px; }",
        '.skin-blue .main-sidebar {
              background-color: black;}',
        
        
        #siderbar text color
        '.skin-blue .main-sidebar .sidebar{
              color: orange;}'
      )
      )
    ),
    
    
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      ### Dose optimization
      box(width = 7, title = "Dose optimization",
          
          actionButton("submit", "Submit Changes"),
          downloadButton("download_report", label = "Generate PDF"),
          DT::dataTableOutput("custom_scheme"), br(),
          DT::dataTableOutput("dosing_schemes"), br(),
          
          column(width=3,
                 numericInput("n_doses_sim", label = "Doses to simulate", value=1, max=10)
          ),
          column(width=3,
                 dateInput("start_sim_from", "Start next dose at:")
          ),
          column(width=2,
                 textInput("start_hh", label="hour")
          ),
          column(width=2,
                 textInput("start_min", label="min")
          )
          
      ),
      ### Plots and Parameter predictions
      box(width = 5, title = "Predictions and PK",
          plotOutput("par_dist", height=100),
          plotOutput("PKPlot", height = 450)
      ),
      ### TDM Data
      box(width = 12, title = "TDM Data",
          ## Element 1
          fileInput("loadPT", "Import dataset from Excel", multiple = FALSE, 
                    accept = c(".xlsx", ".xls"),                                     
                    width = NULL,buttonLabel = "Browse...", 
                    placeholder = "No file selected"),
          
          actionButton("add_ev", "Add event",  icon = icon("plus-square"), width = "49.8%"),
          actionButton("del_ev", "Delete Event",  icon = icon("minus-square"), width = "49.8%"),
          br(),br(),br(),
          ## Element 2
          DT::dataTableOutput("data_set")
      )
    ),
    bsModal("ADD_MODAL", title="Add new Data Entry", trigger="add_ev", size = "small", 
            
            dateInput(inputId="ADD_DATE", label="Date:", value = today()),
            timeInput(inputId = "ADD_TIME", label = "Time [HH:MM]:", value = Sys.time(), seconds = F),
            
            selectInput("SELECT_TYPE", label="Entry type:", 
                        choices = list("Dosing Event" = 1,
                                       "TDM Measurement" = 2)
            ),
            conditionalPanel(condition="input.SELECT_TYPE==1",
                             numericInput("ADD_AMT", "Dose [mg]:", min = 0, max = 5000, value = 1000)
            ),
            conditionalPanel(condition="input.SELECT_TYPE==2",
                             numericInput("ADD_TDM", "Measured drug concentration [ng/mL]:", min = 0, max = 200, value = 0)
            ),
            br(),
            actionButton("ADD_OK", "OK")
    ),
    bsModal("LOAD_PT_MODAL", title="Load Patient", trigger="ld_pat", size = "large", 
            
            DT::dataTableOutput("patient_db"),
            br(),
            actionButton("LOAD_PT_OK", "OK")
    )
  )
)