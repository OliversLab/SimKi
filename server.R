

shinyServer(function(input, output, session) { 
  
  ### static data
  
  custom_scheme_data <- data.frame(amt=c(1000), ii=c(12),  cmin="?", auc="?")
  
  row.names(custom_scheme_data) <- c("Custom") 
  
  scheme_data <- data.frame(amt=c(1500, 1250, 1000, 750, 500), ii=c(12,12,12,12,12), 
                            cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?"))
  
  row.names(scheme_data) <- c("+50%", "+25%", "Standard", "-25%", "-50%")
  
  
  ### reactive AppData
  
  app_data <- reactiveValues(
    
    ## AppData used in simulation
    
    custom_scheme_data = custom_scheme_data,
    scheme_data = scheme_data,
    
    
    disc_shown = F,
    user_data_set = NULL,
    data_set = NULL,
    time_reference = NULL,
    tdm_samples_available = FALSE,
    last_known_dose = NULL,
    last_known_dose_orig = NULL,
    pk_plots = NULL,
    
    params = NULL,
    times_to_calculate = NULL,
    covariates = NULL,
    mc_result = NULL,
    mcmc_result = NULL,
    dist_plots = NULL,
    
    current_scheme = data.frame(),
    
    current_lab = NULL,
    current_patient = NULL,
    current_diag = NULL,
    
    pop_median_cp = NULL,
    pop_percent_in_target = NULL
  )
  
  ### Render HTML outputs
  
  
  
  ### Render TDM Dataset table
  
  output$data_set <- DT::renderDT({
    
    if(!app_data$disc_shown){
      app_data$disc_shown = T
      showModal(modalDialog(
        title = "Disclaimer",
        HTML(paste(read_file("./HTML/disclaimer.html"))),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(is.null(app_data$data_set)){
      
      app_data$user_data_set <- read.xlsx("./XLSX/blank.xlsx")
      
    }
    
    
    DT::datatable(app_data$user_data_set, editable="row",
                  colnames = c("Date", "Time", "Dose [mg]", "Conc [ng/mL]", "Event ID"),
                  class = 'cell-border strip hover',selection = "single",
                  options=list(pageLength=10, searching=FALSE, lengthChange=FALSE))
    
    
  })
  
  ### Render TDM Dataset until here
  
  
  ### Render custom Scheme DataTable
  
  output$custom_scheme <- DT::renderDT({
    
    
    
    DT::datatable(app_data$custom_scheme_data ,
                  options=list(pageLength=10, searching=FALSE, lengthChange=FALSE, paging=FALSE, info=FALSE, ordering=FALSE),
                  colnames = c("Dose [mg]", "Interval [h]", "Cmin [ng/mL]", "AUC [ng/mL hr]"),
                  selection = "single",
                  class = 'cell-border strip hover',
                  editable = list(target = "row", disable = list(columns = c(0, 3, 4)))) %>% formatStyle(0, cursor = 'pointer')
    
  })
  
  ### Render alternative Schemes DataTable
  
  output$dosing_schemes <- DT::renderDT({
    
    
    DT::datatable(app_data$scheme_data,
                  options=list(pageLength=10, searching=FALSE, lengthChange=FALSE, paging=FALSE, info=FALSE, ordering=FALSE),
                  colnames = c("Dose [mg]", "Interval [h]", "Cmin [ng/mL]", "AUC [ng/mL hr]"),
                  editable = FALSE, selection = "single",
                  class = 'cell-border strip hover') %>% formatStyle(0, cursor = 'pointer')
    
  })
  
  
  ## TODO There is some weird bug, when clicked twice in the same cell after the
  ## TODO table has been deselected by clicking in the dosing_schemes table
  ## TODO it will not trigger a cell clicked
  ## FIXED: TABLEID_rows_selected instead of TABLEID_cell_clicked fixed this issue
  observeEvent(input$custom_scheme_rows_selected,  {
    
    app_data$current_scheme <- app_data$custom_scheme_data[input$custom_scheme_rows_selected,]
    
    
    pro <-dataTableProxy(
      outputId="dosing_schemes",
      session = session,
      deferUntilFlush = TRUE
    )
    DT::selectRows(pro, NULL)
    
    
  })
  
  observeEvent(input$dosing_schemes_rows_selected, {
    
    app_data$current_scheme <- app_data$scheme_data[input$dosing_schemes_rows_selected,]
    
    pr <-dataTableProxy(
      outputId="custom_scheme",
      session = session,
      deferUntilFlush = TRUE
    )
    DT::selectRows(pr, NULL)
    
    
  })
  
  ### Render PK Prediction plot
  
  output$PKPlot <- renderPlot({
    
    if(is.null(app_data$pk_plots)){
      return()
    } 
    
    if(app_data$tdm_samples_available) {
      app_data$pk_plots[[1]]
    } else {
      app_data$pk_plots[[2]]
    }
    
    
  })
  
  ### End Render PK Prediction plot
  
  observeEvent(input$custom_scheme_cell_edit, {
    
    info = input$custom_scheme_cell_edit
    
    app_data$custom_scheme_data$amt <- as.numeric(info$value[2]) 
    app_data$custom_scheme_data$ii <- as.numeric(info$value[3]) 
    
    app_data$current_scheme <- app_data$custom_scheme_data
    
  })
  
  
  ## TODO collect doubled code (loadPT) in a single function
  observeEvent(input$data_set_cell_edit, {
    
    info = input$data_set_cell_edit
    
    app_data$user_data_set[info$row[1], ] <- info$value[-1]
    
    temp <- convert_xlsx_to_NMTRAN(app_data$user_data_set)
    
    
    refresh_data_set(temp)
  })
  
  
  ## TODO update start next dose
  observeEvent(input$ADD_OK,{
    
    ## Get Entry type
    new_date = as.character(input$ADD_DATE)
    new_time = (strsplit((as.character(input$ADD_TIME)), " ")[[1]][2])
    
    
    if(input$SELECT_TYPE==1) { ## New Dose
      
      new_entry = data.frame('DATE'=new_date,
                             'TIME'=new_time,
                             'AMT'=input$ADD_AMT,
                             'CONC'=".",
                             'EVID'=1)
      
    } else if(input$SELECT_TYPE==2) { ## TDM Measurement
      new_entry = data.frame('DATE'=new_date,
                             'TIME'=new_time,
                             'AMT'=".",
                             'CONC'=input$ADD_TDM,
                             'EVID'=0)
    }
    
    
    if(nrow(app_data$user_data_set)>0){  
      app_data$user_data_set$DATE <- as.character(app_data$user_data_set$DATE )
      app_data$user_data_set$TIME <- as.character(app_data$user_data_set$TIME )
      app_data$user_data_set$CONC <- as.character(app_data$user_data_set$CONC )

      app_data$user_data_set <- rbind(app_data$user_data_set, new_entry)
      
      app_data$user_data_set <- app_data$user_data_set[with(app_data$user_data_set, order(DATE, TIME)),]
      
    } else {
      app_data$user_data_set <- new_entry
    }
    
    
    
    temp <- convert_xlsx_to_NMTRAN(app_data$user_data_set)
    
    refresh_data_set(temp)
    
    
    
    toggleModal(session, "ADD_MODAL", toggle = "close")
  })
  
  
  
  ### Render Distribution plots
  
  output$par_dist <- renderPlot({
    
    
    if(is.null(app_data$mc_result)){
      return()
    }
    
    
    
    app_data$dist_plots[[1]]
    
  })
  

  
  ## Load an Excel file with data
  
  observeEvent(input$loadPT,{
    inFile <- input$loadPT
    
    if (is.null(inFile))
      return(NULL)
    
    tryCatch({
      
      path_info <- read_excel(inFile$datapath, sheet = "PATHOGEN")
      
      pat_info <- read_excel(inFile$datapath, sheet = "PATIENT")
      
      app_data$current_patient <- pat_info$ID[1]
      
      #updateTextInput(session, inputId = "pat_ID", value = pat_info$ID[1])
      #updateNumericInput(session, inputId = "WT", value = as.numeric(pat_info$WT[1]))
      #updateNumericInput(session, inputId = "CRCL", value = as.numeric(pat_info$CRCL[1]))
      #updateCheckboxInput(session, inputId = "has_dialysis", value=pat_info$DIAL[1])
      
      #updateNumericInput(session, inputId = "MIC", value = as.numeric(path_info$MIC[1]))
      #updateSelectInput(session, inputId = "choose_pathogen", selected = as.numeric(path_info$PATHOGEN[1]))
      
      
      temp_data <- read_excel(inFile$datapath, sheet = "DATA_SET") 
      temp <- convert_xlsx_to_NMTRAN(temp_data)
      
      refresh_data_set(temp)
      
      
      
    },
    error = function(e){
      showModal(modalDialog(
        title = "ERROR",
        HTML(paste("File not recognized!<br>Details:<br><br>",e)),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
  })
  
  ## Convert Exceldataset to NMTRAN
  convert_xlsx_to_NMTRAN <- function(data){
    date <- (as.character(data$DATE))
    
    hour <- (as.character(data$TIME))
    
    
    hour_time <- as.POSIXct(paste(date, hour, sep= " "))
    
    
    
    orig_data <- data.frame(DATE = date,
                            TIME = hour,
                            AMT = data$AMT,
                            CONC = data$CONC,
                            EVID = data$EVID, stringsAsFactors = FALSE) 
    
    
    
    t_ref <- hour_time[1]
    
    hour_time <- as.numeric(hour_time)
    
    
    
    
    hour_time <- (hour_time-min(hour_time))/3600
    
    conv_data <- data.frame(time=hour_time,
                            amt = as.numeric(data$AMT),
                            conc = as.numeric(data$CONC),
                            evid = data$EVID)
    
    
    
    return(list(original_data=orig_data,
                conv_data=conv_data,
                time_reference=t_ref))
  }
  
  
 
  
  ## Update PK Plot
  
  updatePKPlot <- function(){
    
    ## create simulated doses
    event_data <- app_data$data_set
    
    ## check if something is selected in the custom or fixed dose scheme table
    
    any_scheme_selected <- !(is.null(input$dosing_schemes_rows_selected) & is.null(input$custom_scheme_rows_selected))
    
    
    if(any_scheme_selected) {
      for(index in 1:input$n_doses_sim){
        
        ## get start date for simulation
        sim_dose_start_date <- paste0(input$start_sim_from," ",input$start_hh,":",input$start_min,":00")
        
        ## calculate relative time difference in hours to the first dose
        ## TODO try-catch
        t_start <- (as.numeric(as.POSIXct(sim_dose_start_date)) - as.numeric(app_data$time_reference)) /3600
        
        ## test if t_start is after last dose
        test_ii <- t_start - tail(app_data$data_set[app_data$data_set$evid == 1, ],1)$time
        if(test_ii >0) {
          event_data <- rbind(event_data, data.frame(time=t_start+app_data$current_scheme$ii*(index-1),  
                                                     amt=app_data$current_scheme$amt, 
                                                     conc=NA, 
                                                     evid=1)
                              ) 
        } else {
          ## simulate only existing doses if test_ii is below 0
          any_scheme_selected = FALSE
          
          showModal(modalDialog(
            title = "Warning",
            HTML(paste("Start for new dosing scheme is prior to last existing Dose!<BR>",
                       "Simulating only existing doses!")),
            easyClose = TRUE,
            footer = NULL
            
          ))
        }
      }
      
    } 
    
    
    existing_times <- app_data$data_set$time
    
    to_simulate_times <- event_data$time
    
    if(length(to_simulate_times) == 1){
      times <- c(times, times+12)
    }
    
    x_min <- as.POSIXct.numeric(min(to_simulate_times)*3600,origin=app_data$time_reference)
    x_max <- as.POSIXct.numeric((max(to_simulate_times)+12 ## TODO replace 12 by input$simulate.t
    )*3600,origin=app_data$time_reference)
    
    
    ## get tdm data from the table
    ## TODO deal with missing tdm_data
    tdm_data <- data.frame(conc=as.numeric(as.character(app_data$data_set[app_data$data_set$evid==0,]$conc)),
                           time=as.numeric(as.character(app_data$data_set[app_data$data_set$evid==0,]$time)))
    

    ## --- get time variant covariates
    
    
    temp_time <- seq(min(to_simulate_times), max(to_simulate_times)+12, by=0.1) ## replace 12 by input input$simulate.t, replace 0.1 input$delta.t
    
    temp_time <- c(temp_time, tdm_data$time)
    
    temp_time <- unique(temp_time)
    
    temp_time <- sort(temp_time)
    
    app_data$times_to_calculate <- temp_time
    
    
    tdm_data$time <- as.POSIXct.numeric(tdm_data$time*3600,origin=app_data$time_reference)
    
    ## read the model file according to selection
    mrgsolve::mread(paste0("./CPP/",MODEL_FILES[as.numeric(input$SELECT_DRUG)]))
    
    if(app_data$tdm_samples_available) {
      
      
      
      
    }
    
    
    ### Prepare Distribution plots
    
    print(tdm_data)
    print(event_data)
    
  }
  
  
  ## Submit Action
  
  observeEvent(input$submit, {
    
    if(is.null(app_data$data_set)){
      showModal(modalDialog(
        title = "ERROR",
        HTML("No data entry present!"),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    tryCatch({
      app_data$pk_plots <- updatePKPlot()
    },
    error = function(e){
      showModal(modalDialog(
        title = "ERROR",
        HTML(paste("Error while generating the plot!<br>Details:<br><br>",e)),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    
  })
  
  observeEvent(input$del_ev, {
    
    if(is.null(input$data_set_rows_selected)){
      showModal(modalDialog(
        title = "ERROR",
        HTML("No data selected!"),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    
    app_data$user_data_set <- app_data$user_data_set[-input$data_set_rows_selected,]
    
    
    temp <- convert_xlsx_to_NMTRAN(app_data$user_data_set)
    
    refresh_data_set(temp)
    
    
    
    
  })
  
  refresh_data_set <- function(temp){
    app_data$data_set <- temp$conv_data
    app_data$user_data_set <- temp$original_data
    app_data$time_reference <- temp$time_reference
    
    
    
    
    ii = 12
    doses = temp$conv_data[temp$conv_data$evid==1,]
    if(nrow(doses)>1){
      ii = doses$time[nrow(doses)] - doses$time[nrow(doses)-1]
    }
    
    if(nrow(temp$conv_data[temp$conv_data$evid==0,])>=1){
      app_data$tdm_samples_available <- TRUE
    } else {
      app_data$tdm_samples_available <- FALSE
    }
    
    print(app_data$tdm_samples_available)
    
    app_data$last_known_dose <- tail(temp$conv_data[temp$conv_data$evid==1,],1)
    app_data$last_known_dose_orig <- tail(temp$original_data[temp$original_data$EVID==1,],1)
    
    app_data$last_known_dose$ii <- ii
    app_data$last_known_dose_orig$II <- ii
    
    ## Update the "Start next dose at" in a way, that it automatically starts 12hours or an ii after the last dose
    ## TODO correct II
    new_start_date <- as.character(as.POSIXct(paste0(app_data$last_known_dose_orig$DATE, " ", app_data$last_known_dose_orig$TIME) )+12*3600)
    
    date <- strsplit(new_start_date, " ")[[1]][1]
    hh <- strsplit(strsplit(new_start_date, " ")[[1]][2], ":")[[1]][1]
    mm <- strsplit(strsplit(new_start_date, " ")[[1]][2], ":")[[1]][2]
    
    
    updateTextInput(session, inputId = "start_hh", value = hh ) 
    updateTextInput(session, inputId = "start_min", value = mm ) 
    updateDateInput(session, inputId =  "start_sim_from", value = date)
  }
  
})

