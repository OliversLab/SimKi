

shinyServer(function(input, output, session) { 
  
  ### static data ## Mostly default values to initialize the GUI
  
  custom_scheme_data <- data.frame(amt=c(1000), ii=c(12),  cmin="?", auc="?")
  
  row.names(custom_scheme_data) <- c("Custom") 
  
  scheme_data <- data.frame(amt=c(1500, 1250, 1000, 750, 500), ii=c(12,12,12,12,12), 
                            cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?"))
  
  row.names(scheme_data) <- c("++", "+", "0", "-", "--")
  
  
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
    pk_model = NULL,
    current_plot = NULL,
    
    clinical_eval = NULL,
    
    params = NULL,
    times_to_calculate = NULL,
    dist_plots = NULL,
    cov_table = NULL,
    current_scheme = data.frame(),
    
    current_lab = NULL,
    current_patient = NULL,
    current_diag = NULL,
    
    pop_median_cp = NULL,
    pop_percent_in_target = NULL,
    
    messageData = data.frame(
      status = NULL,
      message = NULL,
      stringsAsFactors = FALSE
    )
  )
  
  
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    try(
      msgs <- apply(app_data$messageData, 1, function(row) {
        notificationItem(text = row[["message"]], status = row[["status"]])
      }),
      return()
    )
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
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
                  colnames = c("Dose [mg]", "Interval [h]", "Steady State Cmin [ng/mL]", "Steady State AUC (tau) [ng/mL h]"),
                  selection = "single",
                  class = 'cell-border strip hover',
                  editable = list(target = "row", disable = list(columns = c(0, 3, 4)))) %>% formatStyle(0, cursor = 'pointer')
    
  })
  
  ### Render alternative Schemes DataTable
  
  output$dosing_schemes <- DT::renderDT({
    
    
    DT::datatable(app_data$scheme_data,
                  options=list(pageLength=10, searching=FALSE, lengthChange=FALSE, paging=FALSE, info=FALSE, ordering=FALSE),
                  colnames = c("Dose [mg]", "Interval [h]", "Steady State Cmin [ng/mL]", "Steady State AUC (tau) [ng/mL h]"),
                  editable = FALSE, selection = "single",
                  class = 'cell-border strip hover') %>% formatStyle(0, cursor = 'pointer')
    
  })
  
   ### Render PK Prediction plot
  
  output$PKPlot <- renderPlot({
  
    
    if(is.null(app_data$pk_plots)){
      return()
    } 
    

   
    
    app_data$data_set %>% filter(evid==0) %>% mutate(x=time, y=conc) -> obs
    openlabr::create_pred_int(app_data$pk_plots[["PLASMA"]] %>% filter(time>0)) -> pk_plot_time_conc_pred
    
    pk_plot_time_conc_pred$time <- as.POSIXct.numeric(pk_plot_time_conc_pred$time*3600, origin=app_data$time_reference)
    
    ## TODO there is a bug when no simulated data can be found
    ## - fixed -
    
    pk_plot_time_conc_pred$is_simulated <- (as.numeric(pk_plot_time_conc_pred$time) >= as.numeric(as.POSIXct(paste0(input$start_sim_from," ",input$start_hh,":",input$start_min,":00"))))
 
    if(sum(pk_plot_time_conc_pred$is_simulated) > 0) { #if there is simulated data
      pk_plot_time_conc_pred$is_simulated %>% 
        factor(labels = c("from TDM data","simulated")) -> pk_plot_time_conc_pred$is_simulated
    } else { # if not ...
      pk_plot_time_conc_pred$is_simulated %>% 
        factor(labels = c("from TDM data")) -> pk_plot_time_conc_pred$is_simulated
    }
    
    obs <- obs %>% mutate(x=as.POSIXct.numeric(x*3600, origin=app_data$time_reference))
    
    pk_plot_time_conc_pred %>% 
      ggplot(aes(x=time)) + plot_theme +
      geom_ribbon(aes(ymin=perc_5, ymax=perc_25, alpha="90% PI", fill=is_simulated), show.legend = F) +
      geom_ribbon(aes(ymin=perc_75, ymax=perc_95, alpha="90% PI", fill=is_simulated), show.legend = F) +
      geom_ribbon(aes(ymin=perc_25, ymax=perc_75, alpha="50% PI", fill=is_simulated)) +
      labs(x="", y="Plasmaconcentration [ng/mL]", colour="", alpha="", fill="")+
      ggtitle("Population prediction") +
      guides(fill=guide_legend(nrow=2, byrow=TRUE),
             alpha=guide_legend(nrow=2, byrow=TRUE)) +
      scale_x_datetime(labels = date_format("%d.%m.%Y\n%H:%M", tz = "CET")) +
      scale_alpha_manual(values=c("50% PI"=0.5,
                                  "90% PI"=0.25)) -> temp_pl
    #if there are observations
    if(nrow(obs > 0)) {
      
      temp_ind_plasma <- app_data$pk_plots[["IND_PLASMA"]] %>% filter(time>0) %>% 
        mutate(time=as.POSIXct.numeric(time*3600, origin=app_data$time_reference))
    
      temp_pl + 
        geom_point(data = obs, aes(x=x, y=y, colour="measured"), size =5, shape=1) +
        geom_line(data = temp_ind_plasma, aes(x=time, y=IPRED, colour="ind prediction"), lwd=1) +
        ggtitle("Population prediction", "with overlaid individual MAP estimate") +
        scale_color_manual(values = c("black",
                                      "blue")) +
        guides(colour = guide_legend(override.aes = list(linetype=c(1,NA),
                                                         shape=c(NA,1)),
                                     nrow=2, byrow=TRUE))  -> temp_pl
      
      ### Evaluate result
      clinical_eval <- NULL
      
      for (j in 1:nrow(obs)) {
        cur_pred_dv <- pk_plot_time_conc_pred[pk_plot_time_conc_pred$time== obs$x[j],]
        if(cur_pred_dv$perc_5 > obs$y[j]) {
          cur_eval <- paste(obs$y[j], "ng/mL measured on", obs$x[j], "is below the 90% prediction interval. This value is unusually low.")
        } 
        if(cur_pred_dv$perc_25 > obs$y[j] & cur_pred_dv$perc_5 < obs$y[j]) {
          cur_eval <- paste(obs$y[j], "ng/mL measured on", obs$x[j], "is below the 50% prediction interval.")
        } 
        if(cur_pred_dv$perc_75 > obs$y[j] & cur_pred_dv$perc_25 < obs$y[j]) {
          cur_eval <- paste(obs$y[j], "ng/mL measured on", obs$x[j], "is within the 50% prediction interval.")
        } 
        if(cur_pred_dv$perc_95 > obs$y[j] & cur_pred_dv$perc_75 < obs$y[j]) {
          cur_eval <- paste(obs$y[j], "ng/mL measured on", obs$x[j], "is above the 50% prediction interval. Monitor closely for adverse events")
        } 
        if(cur_pred_dv$perc_95 < obs$y[j]) {
          cur_eval <- paste(obs$y[j], "ng/mL measured on", obs$x[j], "is above the 90% prediction interval. Risk of increased toxicity.")
        }
        
        clinical_eval <- c(clinical_eval, cur_eval)
        
      }
      app_data$clinical_eval <- clinical_eval
      
      
    } 
    
    app_data$current_plot <-temp_pl
    
    temp_pl
  })
  
  ### End Render PK Prediction plot 
  
  ### Render Distribution plots
  
  output$par_dist <- renderPlot({
    
    
    if(is.null(app_data$pk_plots[["DIST"]])){
      return()
    }

    
    
    eta_data_proc <- data.frame()
    

    for(i in 1:ncol(app_data$pk_plots[["DIST"]])) {
      app_data$pk_plots[["DIST"]][i] %>% simplify()-> temp_values
      colnames(app_data$pk_plots[["DIST"]])[i] %>% simplify() -> temp_key
      eta_data_proc <- rbind(eta_data_proc, data.frame(value=temp_values, key=temp_key))
    }
    
    eta_data_proc %>% 
      ggplot(aes(x=value)) + geom_histogram() + facet_wrap(.~key, ncol=ncol(app_data$pk_plots[["DIST"]]), scales = "free") +
      theme_bw() + labs(x="", y="") -> dist_plots
    
    if(!is.null(app_data$pk_plots[["IND_PLASMA"]])){
      
      etas_estimated <- NULL
      for (i in 1:length(ETAS[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]])) {
        etas_estimated <- c(etas_estimated, paste0("ETA",i))
      }
      
      eta_data_proc <- data.frame()
      
      for(i in 1:ncol(app_data$pk_plots[["DIST"]])) {
        app_data$pk_plots[["DIST"]][i] %>% simplify()-> temp_values
        colnames(app_data$pk_plots[["DIST"]])[i] %>% simplify() -> temp_key
        eta_data_proc <- rbind(eta_data_proc, data.frame(value=temp_values, key=temp_key ,ind=app_data$pk_plots[["IND_PLASMA"]][1,etas_estimated[i]]))
      }
      
      eta_data_proc %>% 
        ggplot(aes(x=value)) + geom_histogram() + facet_wrap(.~key, ncol=ncol(app_data$pk_plots[["DIST"]]), scales = "free") +
        geom_vline(aes(xintercept=ind[1]), colour="red", lwd=1) +
        theme_bw() + labs(x="", y="") -> dist_plots
      
    }
    
    return(dist_plots)
    
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
  

  
  observeEvent(input$custom_scheme_cell_edit, {
    
    info = input$custom_scheme_cell_edit
    
    app_data$custom_scheme_data$amt <- as.numeric(info$value[2]) 
    app_data$custom_scheme_data$ii <- as.numeric(info$value[3]) 
    
    app_data$current_scheme <- app_data$custom_scheme_data
    
    reset_pk_plots()
    
  })
  
  
  ## TODO collect doubled code (loadPT) in a single function
  observeEvent(input$data_set_cell_edit, {
    
    info = input$data_set_cell_edit
    
    app_data$user_data_set[info$row[1], ] <- info$value[-1]
    
    temp <- convert_xlsx_to_NMTRAN(app_data$user_data_set)
    
    reset_pk_plots()
    
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
    
    reset_pk_plots()
    
    refresh_data_set(temp)
    
    app_data$messageData <- rbind(app_data$messageData, data.frame(status="info", message=paste("Event added", Sys.time())))
    
    toggleModal(session, "ADD_MODAL", toggle = "close")
  })
  
  
  

  
  ## when changing the drug what should happen ...
  ## TODO 
  observeEvent(input$SELECT_DRUG, {
    
    ## read the model file according to selection
    MODELS[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]] -> app_data$pk_model
    
    
    # update new dose dialog also 
    app_data$scheme_data <- SCHEMES[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]] 
    
    updateNumericInput(session, inputId = "ADD_AMT", value = as.numeric(app_data$scheme_data$amt[3]))
    
    updateSelectInput(session, inputId = "SELECT_II", selected = as.numeric(app_data$scheme_data$ii[3]))
    
    # TODO align symbols with SmPC recommendations
    row.names(app_data$scheme_data) <- c("++", "+", "0", "-", "--")
    
    app_data$custom_scheme_data <- CUSTOM_SCHEMES[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]] 
    
    row.names(app_data$custom_scheme_data)  <- c("Custom")
    
    clearMessages()
    
    app_data$messageData <- rbind(app_data$messageData, data.frame(status="primary", message=paste(MODEL_FILES[as.numeric(input$SELECT_DRUG)], "selected", Sys.time())))
    
    reset_pk_plots()
    
  })
  
  clearMessages <- function() {
    app_data$messageData = data.frame(
      status = NULL,
      message = NULL,
      stringsAsFactors = FALSE
    )
  }
  
  reset_pk_plots <- function() {
    # delete old plots
    app_data$pk_plots <- NULL
    app_data$current_plot <- NULL
    app_data$cov_table <- NULL
    app_data$clinical_eval <- NULL
    
    app_data$scheme_data$auc <- "?"
    app_data$scheme_data$cmin <- "?"
    
    app_data$custom_scheme_data$auc <- "?"
    app_data$custom_scheme_data$cmin <- "?"
    
  }
  
  output$download_report = downloadHandler(
    
    
    ## TODO display error message if there is no plot
    
    
    ## Filename includes patient ID
    filename = paste("report_", input$pat_ID, ".pdf", sep=""),
    content = function(file) {
      withProgress(message = "Compiling report ...", style="notification", value =0 ,{
        
        
        incProgress(0.33)
        out <- render('report.Rmd', output_format=pdf_document(latex_engine = "xelatex"))
        incProgress(0.33)
        file.rename(out, file)
        incProgress(0.34)
      })
    }
  )
  
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
    
    # TODO warning conc becomes NA for dosing events
    # not a problem but warnings keep coming up
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
    
    # TODO get interdose interval from data_set
    event_data %>% mutate(ss=0, ii=as.numeric(input$SELECT_II)) -> event_data
    
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
                                                     evid=1,
                                                     ss=0,
                                                     ii=app_data$current_scheme$ii)
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
    
    # If there is only one data entry
    if(length(to_simulate_times) == 1){
      to_simulate_times <- c(to_simulate_times, to_simulate_times+12)
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
    
    

    # TODO find cause why evid is not numeric after data set table edit
    event_data <- event_data %>% mutate(evid=as.numeric(evid)) %>% 
      mutate(DOSE=amt) 
    
    temp_ev_data <- data.frame()
    
    # multiplicate dosing events to allow split doses in mixed absorption models
    # e.g. Cabozantinib
    # Important: in mixed absorption models, the ZO input cmt needs to be the first
    # in the order of input events.
    
    for(i in 1:length(DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]])) {
      
      cur_event <- event_data %>% 
        mutate(rate = DOSING_RATE[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][i],
               cmt  = DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][i],
               amt  = amt * DOSING_FRACTION[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][i])
      
      if (i > 1)
        cur_event <- cur_event %>% filter(evid==1) ## prevent dublication of TDM measurements
      
      temp_ev_data <- rbind(temp_ev_data, cur_event) 
      
    }
    
    event_data <- temp_ev_data
    
    # TODO get covariates
    
    cov_table <- COVARIATES[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]]
    
    
    for(i in 1:ncol(cov_table)) {
      cov_table[,i]=as.numeric(input[[colnames(cov_table)[i] ]])
      
    }
    
    app_data$cov_table <- cov_table
    
    event_data <- cbind(event_data,cov_table)
    
    
    
    #  TODO this will cause an error in the case ob cabozantinib
    #  because ss=2 is not supported in models with lag-time
    #  will need to use a workaround (i.e. simulate 14 days prior)
    if(input$at_ss) {
      
      if (length(DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]])==1)
        event_data[1, "ss"] <- 1
      else if (length(DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]])==2) {
        event_data[1, "ss"] <- 1
        event_data[2, "ss"] <- 2
      } else{
        showErrorMessage("Dataset Error!<BR>Please check model definitions of steady state!", minorMsg = "Dataset Error (ss)")
        return()
      }
      
    }
      
    
    # TODO if there is a TDM data, generate MAP estimate
    # DONE
    
    event_data <- event_data %>% arrange(time, cmt)
    
    # in case of Cabozantinib AND steady state add 14 doses
    # and drop ss column
    # TODO remove when mrgsim supports ss=2 and lag time
    
    if ((MODEL_FILES[as.numeric(input$SELECT_DRUG)] == "Cabozantinib")& input$at_ss ){
      for(add_dose in 1:20) {
        temp <- event_data[1:2,]
        temp$time <- -24*add_dose
        event_data <- c(as.ev(temp), as.ev(event_data))
        event_data <- event_data %>% as.data.frame()
      }
      event_data$ss <- 0
    }
    
    

    
    show_modal_spinner() # show the spinner
    
    map_sim_res <- NULL
    etas_estimated <- NULL
    
    if(nrow(tdm_data)>0) {
      
      # create ETA1, ETA2, ... according to popPK model
      etas_to_estimate <- NULL
      init_etas <- NULL
      for (i in 1:length(ETAS[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]])) {
        etas_to_estimate <- c(etas_to_estimate, paste0("ETA",i))
        init_etas <- c(init_etas, 0)
      }
      names(init_etas) <- etas_to_estimate 
      
      # TODO create an option simulate to steady state
      # TODO using a ss_options list which includes time and ii or number of doses
      # TODO and ii to assume steady state for
      # -done- handled via the event_data argument!!
      openlabr::create_MAP_estimate(x=app_data$pk_model,
                                    tdm_data=event_data,
                                    ycol_name = "conc",
                                    dvcol_name = "IPRED",
                                    init_etas = init_etas) -> map_est
      
      map_est[1, etas_to_estimate] %>% simplify() -> etas_estimated
      
      app_data$pk_model %>% ev(cbind(map_est, cmt=1)) %>% 
        carry.out(evid, etas_to_estimate) %>% zero_re() %>% 
        mrgsim(end=max(event_data %>% filter(evid==1) %>% select(time) )+event_data %>% tail(1) %>% select(ii) %>% simplify() %>% unname(), delta=0.1) %>% as.data.frame() -> map_sim_res
      
    }

    
    
    ## Population predictions
    
    
    
    app_data$pk_model %>% 
      ev(as.ev(event_data %>% ev_rep(1:1000))) %>% 
      carry.out(evid) %>% 
      mrgsim(end=max(event_data %>% filter(evid==1) %>% select(time) )+event_data %>% tail(1) %>% select(ii) %>% simplify() %>% unname(), delta=0.1, ss_n=1000, ss_fixed=FALSE,
             ss_rtol=1e-2, ss_atol=1e-3) %>% 
      as.data.frame() -> mc_sim_res


    ######## Simulate custom tabulated values to obtain Cmin and AUC
    
    simulate_custom_values <- function(etas_estimated) {
      
      
      
      custom_events <- list(cbind(data.frame(time=0, amt=app_data$custom_scheme_data$amt[1], ii = app_data$custom_scheme_data$ii[1], ss = 1, evid=1), cov_table),
                            cbind(data.frame(time=0, amt=app_data$scheme_data$amt[1], ii = app_data$scheme_data$ii[1], ss = 1, evid=1), cov_table),
                            cbind(data.frame(time=0, amt=app_data$scheme_data$amt[2], ii = app_data$scheme_data$ii[2], ss = 1, evid=1), cov_table),
                            cbind(data.frame(time=0, amt=app_data$scheme_data$amt[3], ii = app_data$scheme_data$ii[3], ss = 1, evid=1), cov_table),
                            cbind(data.frame(time=0, amt=app_data$scheme_data$amt[4], ii = app_data$scheme_data$ii[4], ss = 1, evid=1), cov_table),
                            cbind(data.frame(time=0, amt=app_data$scheme_data$amt[5], ii = app_data$scheme_data$ii[5], ss = 1, evid=1), cov_table)
      )
        

      
      temp_res <- data.frame()
      for (i in 1:length(custom_events)) {
        
        custom_events[[i]] <- custom_events[[i]]  %>% mutate(DOSE=amt) 
        temp_ev_data <- data.frame()
        
        # multiplicate dosing events to allow split doses in mixed absorption models
        
        for(j in 1:length(DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]])) {
          
          cur_event <- custom_events[[i]]  %>% 
            mutate(rate = DOSING_RATE[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][j],
                   cmt  = DOSING_CMT[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][j],
                   amt  = amt * DOSING_FRACTION[[MODEL_FILES[as.numeric(input$SELECT_DRUG)] ]][j],
                   ss = j) # TODO set ss on 2 for the second dose (Cabozantinib) but model crashes => workaround simulate 14 days
          
  
          temp_ev_data <- rbind(temp_ev_data, cur_event) 
          
        }
        custom_events[[i]] <- temp_ev_data
        
        
        # CAVE: if order of cmt is inverse => BUG in simulating SS!!
        custom_events[[i]] <- custom_events[[i]] %>% arrange(time, cmt)
        
 
        # in case of Cabozantinib AND steady state add 20 days
        # and drop ss column
        # TODO remove when mrgsim supports ss=2 and lag time
        
        if (MODEL_FILES[as.numeric(input$SELECT_DRUG)] == "Cabozantinib"){
          for(add_dose in 1:(round(20*24/custom_events[[i]]$ii[1]))  ) {
            temp <- custom_events[[i]][1:2,]
            temp$time <- -custom_events[[i]]$ii[1] *add_dose
            custom_events[[i]] <- c(as.ev(temp), as.ev(custom_events[[i]]))
            custom_events[[i]] <- custom_events[[i]] %>% as.data.frame()
          }
          custom_events[[i]]$ss <- 0
        }
        
        ## If there is no ETA estimated use population prediction else use individual prediction
        if(!is.null(etas_estimated)) {
          app_data$pk_model %>% 
            ev(custom_events[[i]]) %>% 
            carry.out(evid) %>% zero_re() %>% param( etas_estimated) %>% 
            mrgsim(end=custom_events[[i]]$ii[1], delta=1, ss_n=1000, ss_fixed=FALSE,
                   ss_rtol=1e-2, ss_atol=1e-3) %>% 
            as.data.frame() -> custom_sim_res
          
        } else {
          app_data$pk_model %>% 
            ev(as.ev(custom_events[[i]] %>% ev_rep(1:1000))) %>% 
            carry.out(evid) %>% 
            mrgsim(end=custom_events[[i]]$ii[1], delta=1, ss_n=1000, ss_fixed=FALSE,
                   ss_rtol=1e-2, ss_atol=1e-3) %>% 
            as.data.frame() -> custom_sim_res
          
        }
        
        
        auc_custom_tau <- custom_sim_res %>% filter(time > 0) %>% 
          group_by(ID) %>%  summarise(AUC_tau = max(AUC)-min(AUC)) %>% 
          summarise(AUC_tau_median=median(AUC_tau),AUC_tau_low=summary(AUC_tau)[2],AUC_tau_up=summary(AUC_tau)[5]) %>% unname()
        
        
        cmin_custom_tau <- custom_sim_res %>% 
          filter(time==custom_events[[i]]$ii[1]) %>% 
          summarise(cmin_median=median(IPRED),cmin_low=summary(IPRED)[2], cmin_up=summary(IPRED)[5]) %>% unname()
        
        
        if(!is.null(etas_estimated)) {
          temp_cmin <- paste0(round(cmin_custom_tau[1,1],2))
          
          temp_auc <- paste0(round(auc_custom_tau[1,1],1))
          
        } else {
          temp_cmin <- paste0(round(cmin_custom_tau[1,1],2), " (", round(cmin_custom_tau[1,2],2), "-", round(cmin_custom_tau[1,3],2),")")
          
          temp_auc <- paste0(round(auc_custom_tau[1,1],1), " (", round(auc_custom_tau[1,2],1), "-", round(auc_custom_tau[1,3],1),")")
        }
        
        temp_res <- rbind(temp_res, data.frame(cmin=temp_cmin, auc=temp_auc))
      }
      return(temp_res)
    }
    
    
    temp_scheme_results <- simulate_custom_values(etas_estimated)
    
    app_data$custom_scheme_data$cmin[1] <- temp_scheme_results$cmin[1]
    app_data$custom_scheme_data$auc[1] <- temp_scheme_results$auc[1]
    
    app_data$scheme_data$cmin <- temp_scheme_results$cmin[-1]
    app_data$scheme_data$auc <- temp_scheme_results$auc[-1]

    ########
    
    mc_sim_res %>% filter(time==0 & evid==0) %>% select(ETAS[[MODEL_FILES[as.numeric(input$SELECT_DRUG)]]]) -> mc_eta_res
    
    remove_modal_spinner() # hide the spinner
  
    app_data$messageData <- rbind(app_data$messageData, data.frame(status="success", message=paste("Simulation complete", Sys.time())))
    
    # TODO correct prediction plots
    return(list("PLASMA"=mc_sim_res,"DIST"=mc_eta_res, "IND_PLASMA"=map_sim_res))
    
    
  }
  
 
  
  showErrorMessage <- function(msg, minorMsg, title="ERROR") {
    showModal(modalDialog(
      title = title,
      HTML(msg),
      easyClose = TRUE,
      footer = NULL
    ))
    
    app_data$messageData <- rbind(app_data$messageData, data.frame(status="danger", message=paste(minorMsg, Sys.time())))
    
  }
  
  ## Submit Action
  
  observeEvent(input$submit, {
    
    if(is.null(app_data$data_set)){
      
      showErrorMessage("No data entry present!<BR>Please enter a dosing event below!", minorMsg = "No data entry")
      
      ## TODO simulate only for the selected scheme
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
  
  ## is called when "delete event" button ist pressed
  observeEvent(input$del_ev, {
    
    # Show error message if no line selected
    if(is.null(input$data_set_rows_selected)){
      showModal(modalDialog(
        title = "ERROR",
        HTML("No data selected!"),
        easyClose = TRUE,
        footer = NULL
      ))
      app_data$messageData <- rbind(app_data$messageData, data.frame(status="danger", message=paste("No data selected", Sys.time())))
      return()
    }
    
    # remove selected line from dataset
    app_data$user_data_set <- app_data$user_data_set[-input$data_set_rows_selected,]
    
    if(nrow(app_data$user_data_set)>0) {
      
      # rebuild the modified dataset only if there is data left
      temp <- convert_xlsx_to_NMTRAN(app_data$user_data_set)
      # replace the old data.set with the new one
      refresh_data_set(temp)
      
    } else {
      # else use the blank excel file
      app_data$user_data_set <- read.xlsx("./XLSX/blank.xlsx")
    }

    app_data$messageData <- rbind(app_data$messageData, data.frame(status="info", message=paste("Event deleted", Sys.time())))
    
    reset_pk_plots()

  })
  
  ## When changing covariates, reset the plot to zero
  observeEvent(c(input$ECOG, input$IND, input$LDH, input$AP, input$TPRO, input$CLCR,
                 input$SMOKING, input$JAPANESE, input$TBIL, input$MD, input$RACE,
                 input$CAPS, input$MAL, input$LIVER, input$AGE, input$SEX, input$WT,
                 input$ALP, input$ALB, input$INDUCER, input$INHIBITOR), {
    reset_pk_plots()
  })
  
  ## is called after input$del_ev (delete event button pressed)
  refresh_data_set <- function(temp){
    
    # replace dataset with NONMEM-like data
    app_data$data_set <- temp$conv_data
    
    # replace the dataset with original data (Date and Time)
    app_data$user_data_set <- temp$original_data
    
    # replace the time reference (the first Dosing event in the original dataset)
    app_data$time_reference <- temp$time_reference
    
    
    
    # TODO 
    # does this even work in the way it is supposed to??
     
    ii = as.numeric(input$SELECT_II)
    
    # filter all events that are dosing events to get number of doses
    doses = temp$conv_data[temp$conv_data$evid==1,]
    
    # if number of doses is > 1, try autodetect ii
    if(nrow(doses)>1){
      ii = doses$time[nrow(doses)] - doses$time[nrow(doses)-1]
    }
    ## TODO else take from the selection ?
    
    # check if there are TDM data among the dataset and set flag
    if(nrow(temp$conv_data[temp$conv_data$evid==0,])>=1){
      app_data$tdm_samples_available <- TRUE
    } else {
      app_data$tdm_samples_available <- FALSE
    }

    
    app_data$last_known_dose <- tail(temp$conv_data[temp$conv_data$evid==1,],1)
    app_data$last_known_dose_orig <- tail(temp$original_data[temp$original_data$EVID==1,],1)
    
    ## Update the "Start next dose at" in a way, that it automatically starts 12hours or an ii after the last dose
    ## TODO correct II get it from the input data_table selected
    new_start_date <- as.character(as.POSIXct(paste0(app_data$last_known_dose_orig$DATE, " ", app_data$last_known_dose_orig$TIME) )+as.numeric(input$SELECT_II)*3600)
    
    date <- strsplit(new_start_date, " ")[[1]][1]
    hh <- strsplit(strsplit(new_start_date, " ")[[1]][2], ":")[[1]][1]
    mm <- strsplit(strsplit(new_start_date, " ")[[1]][2], ":")[[1]][2]
    
    # update the input fields for the next simulation start
    updateTextInput(session, inputId = "start_hh", value = hh ) 
    updateTextInput(session, inputId = "start_min", value = mm ) 
    updateDateInput(session, inputId =  "start_sim_from", value = date)
  }
  
})

