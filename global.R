library('ggplot2')
library('openxlsx')
library('shiny')
library('shinyjs')
library('shinyBS')
library('shinyTime')
library('shinythemes')
library('gridExtra')
library('lubridate')
library('DT')
library('scales')
library('kableExtra')
library('rmarkdown')
library('shinydashboard')
library('tidyverse')
library('mrgsolve')

##
#  --- Define global variables here ---
##

# ---- Labels for selectInput ---------

GLOB_PATHOGENS <- c("MRSA"=1, 
                    "Unidentified"=2, 
                    "Mixed infection"=3)

GLOB_RECOMMENDATIONS <- c("Continue on this dose"=1, 
                          "Adapt Dosing Strategy"=2)

GLOB_ADAPT_FOR <- c("Cmin in therapeutic range"=1)

GLOB_ADAPT_WHAT <- c("Dose"=1, 
                     "Interdose Interval"=2,
                     "Infusion Duration"=3,
                     "All of those"=4)

# ---- color scheme and plot theme ---

main_plot_col <- "#E95420"

limit_plot_col <-"#990000"
text_plot_col <- "grey20"
cont_plot_col <- "lightgrey"
backg_plot_col <-"white"
line_plot_col <- "gray"
plot_grid_col <- "gray"


plot_theme <- theme(axis.text.x = element_text(colour=text_plot_col,size=8,angle=0,hjust=.5,vjust=.5,face="plain"),
                    axis.text.y = element_text(colour=text_plot_col,size=8,angle=0,hjust=1,vjust=0,face="plain"),  
                    axis.title.x = element_text(colour=text_plot_col,size=8,angle=0,hjust=.5,vjust=0,face="bold"),
                    axis.title.y = element_text(colour=text_plot_col,size=8,angle=90,hjust=0.5,vjust=2,face="bold"), 
                    legend.position = "bottom", legend.justification = c(0,1), legend.text=element_text(size=8),
                    panel.background = element_rect(fill = backg_plot_col, colour = cont_plot_col), panel.border = element_blank(), panel.grid.major = element_line(colour = plot_grid_col, linetype = 2),
                    panel.grid.minor = element_line(colour = plot_grid_col, linetype = 2), axis.line = element_line(colour = line_plot_col))

# ---- model files according to the selection UI

MODEL_FILES <- c("afatinib", "axitinib", "cabozantinib", "dabrafenib",
                 "nilotinib", "ruxolitinib", "trametinib")

##
# ---- End definition of global variables
##


##
#  ---- Begin Definition of global available functions
##



# ---- Process a NONMEM like dataset and perform mcmc 


  
  
  

  
  
  