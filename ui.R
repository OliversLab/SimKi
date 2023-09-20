

dashboardPage(
  dashboardHeader(title = "SimKi v0.0.1 by O.Scherf-Clavel (C) 2022", dropdownMenuOutput("messageMenu"),
                  titleWidth = 450),
  
  ## Sidebar
  
  
  dashboardSidebar(
    ## Tooltips on the side bar
    bsTooltip("SELECT_DRUG", "Select the drug to simulate", placement = "right", trigger = "hover", options = NULL),
    
    menuItem(span("Hospital data", style = "font-size: 14px"),startExpanded = FALSE, icon = icon("hospital-user"),
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
                                        "Lenvatinib" = 5,
                                        "Nilotinib" = 6,
                                        "Ruxolitinib" = 7,
                                        "Trametinib" = 8)
                         )
             
             ), 
    menuItem(span("Patient", style = "font-size: 14px"),startExpanded = TRUE,
             icon = icon("procedures"),
             textInput("pat_ID", "Patient ID:", value="00001"),
             numericInput("AGE", "Age:", value=60),
             selectInput("SEX", label="Sex:", 
                         choices = list("male" = 0,
                                        "female" = 1)
                         ),
             numericInput("WT", label="Body weight [kg]:", value = 70
                          
                          ),
             conditionalPanel(condition="input.SELECT_DRUG==1", # Afatinib
                              selectInput("FOOD", label="Dosing with Food?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              ),
                              selectInput("ECOG", label="ECOG performance?", 
                                          choices = list("0" = 0,
                                                         "1" = 1,
                                                         ">1" = 2)
                              ),
                              selectInput("IND", label="Disease?", 
                                          choices = list("BC or NSCLC" = 0,
                                                         "HNSCC" = 1)
                              ),
                              numericInput("LDH", label="LDH [U/L]", 
                                           value = 241
                              ),
                              numericInput("AP", label="AP [U/L]", 
                                           value = 251
                              ),
                              numericInput("TPRO", label="Total proteins [g/L]", 
                                           value = 72
                              ),
                              numericInput("CLCR", label="Creatinine Clearance [mL/min]", 
                                           value = 120
                              )
             ),
             
             conditionalPanel(condition="input.SELECT_DRUG==2", # Axitinib
                              selectInput("SMOKING", label="Smoker ?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              ),
                              selectInput("JAPANESE", label="Japanese ?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                                          )
                              ),
             conditionalPanel(condition="input.SELECT_DRUG==5", # Lenvatinib
                              selectInput("ALB", label="Serum albumin < 30g/L ?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              ),
                              selectInput("ALP", label="Alkaline phosphatase elevated ?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              ),
                              selectInput("INDUCER", label="CYP3A4 inducer", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              ),
                              selectInput("INHIBITOR", label="CYP3A4 inhibitor", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                              )
             ),
             conditionalPanel(condition="input.SELECT_DRUG==6", # Nilotinib
                              numericInput("TBIL", label="Total bilirubin [mg/dL]", 
                                          value = 0.5
                                          ),
                              selectInput("MD", label="Morning dose?", 
                                          choices = list("yes" = 1,
                                                         "no" = 0)
                                          )
                              ),
             conditionalPanel(condition="input.SELECT_DRUG==3", # Cabozantinib
                              selectInput("RACE", label="Ethnic group?", 
                                          choices = list("caucasian" = 0,
                                                         "black" = 1,
                                                         "asian"=2,
                                                         "other"=3)
                              ),
                              selectInput("CAPS", label="Formulation?", 
                                          choices = list("Cabometyx" = 0,
                                                         "Cometriq" = 1)
                              ),
                              selectInput("MAL", label="Disease?", 
                                          choices = list("RCC" = 0,
                                                         "CRPC" = 1,
                                                         "MTC" =2,
                                                         "GB"=3,
                                                         "HCC"=4,
                                                         "other"=5)
                              ),
                              selectInput("LIVER", label="Liver dysfunction?", 
                                          choices = list("no" = 0,
                                                         "mild" = 1,
                                                         "moderate and severe" =2)
                              )
             )

             ),
    

    menuItem(span("Comment", style = "font-size: 14px"), startExpanded = TRUE,
             icon = icon("comment-alt"),
             textAreaInput("comment", ""))
    
  ),
  
  ## Main body
  dashboardBody(
    ## Tooltips for the Main body
    bsTooltip("submit", "Calculate profile. You will need at least 1 dosing event!", placement = "right", trigger = "hover", options = NULL),
    bsTooltip("download_report", "Sadly this is not working! ... yet", placement = "right", trigger = "hover", options = NULL),
    
    bsTooltip("custom_scheme", "Edit Dose or Interval (double click). Accept changes with Ctrl+Enter. Abort with Escape", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("dosing_schemes", "Select predefined dosing scheme for simulation. Cmin and AUC are median of 1000 simulations with interquartile range. If the MAP approach is used the individual most-likely value is displayed.", placement = "bottom", trigger = "hover", options = NULL),
    
    bsTooltip("n_doses_sim", "Specify number of doses to simulate AFTER the observed dosing events.", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("start_sim_from", "Specify date to start simulating ADDITIONAL doses", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("start_hh", "Specify time to start simulating additional doses", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("start_min", "Specify time to start simulating additional doses", placement = "bottom", trigger = "hover", options = NULL),
    
    bsTooltip("par_dist", "Interindividual variability of the popPK model. If you simulate MAP (with TDM measurement) a red line will tell you how different your patient is from the population", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("PKPlot", "If no TDM measurement is entered, this is the population prediction only. With TDM measurement, the individual prediction using maximum a posteriori bayesian estimation (MAP) will be displayed as black line", placement = "bottom", trigger = "hover", options = NULL),
    
    bsTooltip("at_ss", "Specify whether or not the program should assume steady state at the earliest dose entered below.", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("SELECT_II", "If steady state should be assumed, the interdose interval needs to be specified", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("loadPT", "Unfortunately, this is not working... yet!", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("add_ev", "Add an event to the TDM table (either Dosing or Measurement). At least one Dosing event is necessary to run the simulation!", placement = "bottom", trigger = "hover", options = NULL),
    bsTooltip("del_ev", "Delete an event from the TDM table after selecting the row to delete.", placement = "bottom", trigger = "hover", options = NULL),
    
    bsTooltip("data_set", "Here are all entered dosing or measurement events listed. Edited with double click and accept changes with Ctrl+Enter. Abort with Escape.", placement = "top", trigger = "hover", options = NULL),
    
    
        
    use_busy_spinner(spin = "fading-circle"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") ),
    
    
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      ### Dose optimization
      box(width = 7, title = "Dose optimization", collapsible =  T, status = "warning",
          
          actionButton("submit", "Perform calculation!"),
          downloadButton("download_report", label = "Generate PDF"),
          
          DT::dataTableOutput("custom_scheme"), br(),
          DT::dataTableOutput("dosing_schemes"), br(),
          
  
          column(width=3,
                 numericInput("n_doses_sim", label = "Doses to simulate", value=1, max=10)
          ),
          column(width=3,
                 dateInput("start_sim_from", "Start simulation date:")
          ),
          column(width=2,
                 textInput("start_hh", label="hour to start")
          ),
          column(width=2,
                 textInput("start_min", label="min to start")
          )
          
          
      ),
      ### Plots and Parameter predictions
      box(width = 5, title = "Predictions and PK", collapsible =  T,  status = "warning",
          
          shinycssloaders::withSpinner(
            plotOutput("par_dist", height=100)
          ),
          shinycssloaders::withSpinner(
            plotOutput("PKPlot", height = 450)
          )
      ),
      ### TDM Data
      box(width = 12, title = "TDM Data", collapsible =  T, status = "warning",
          checkboxInput("at_ss", label="Patient is at steady state?", width = "20%"),
          conditionalPanel(condition="input.at_ss==1",
                           p(code("Steady state is assumed for the first dose entered!"))
          ),
          selectInput("SELECT_II", label="Dosing Frequency:", width = "20%",
                      choices = list("q24h" = 24,
                                     "q12h" = 12,
                                     "q8h" = 8)
          ),
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
    )
  )
)
