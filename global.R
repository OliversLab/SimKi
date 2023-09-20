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
library('openlabr')
library('shinybusy')

##
#  --- Define global variables here ---
##


# ---- model files according to the selection UI

MODEL_FILES <- c("Afatinib", 
                 "Axitinib", 
                 "Cabozantinib", 
                 "Dabrafenib",
                 "Lenvatinib",
                 "Nilotinib", 
                 "Ruxolitinib", 
                 "Trametinib"
                 )

# ---- prebuild all models

MODELS <- list("Afatinib"     = mrgsolve::mread("./CPP/afatinib"),
               "Axitinib"     = mrgsolve::mread("./CPP/axitinib"),
               "Cabozantinib" = mrgsolve::mread("./CPP/cabozantinib"),
               "Dabrafenib"   = mrgsolve::mread("./CPP/dabrafenib"),
               "Lenvatinib"   = mrgsolve::mread("./CPP/lenvatinib"),
               "Nilotinib"    = mrgsolve::mread("./CPP/nilotinib"),
               "Ruxolitinib"  = mrgsolve::mread("./CPP/ruxolitinib"),
               "Trametinib"   = mrgsolve::mread("./CPP/trametinib")
               )

## Order must be the same as in omega block in model !! # Used to generate the MAP estimate and the 
## IND ETA plot the numbers of ETA to estimate (ETA1, ETA2, ...) is derived from 
## vector length
ETAS <- list("Afatinib"     = c("EF1", "EKA"),
             "Axitinib"     = c("ECL", "EVC", "EQ",  "EKA"),
             "Cabozantinib" = c("ECL", "EVC", "EF1", "EKA"),
             "Dabrafenib"   = c("ECL", "EVC", "EQ",  "EKA"),
             "Lenvatinib"   = c("ECL", "EV1", "EV2", "EV3", "EKA", "ED1", "EF1"),
             "Nilotinib"    = c("EF1", "ECL", "EV1", "EV2"),
             "Ruxolitinib"  = c("ECL", "EVC", "EVP", "EKA"),
             "Trametinib"   = c("ECL", "EVC", "EQ",  "EKA")
             )

# Covariate names from the model. # If present here, an input with the SAME NAME needs to be present in UI.R
# Will only be changed during simulation if present here AND has an UI counterpart
# Every UI input needs to be listed in the server section in order to reset the PK plots
COVARIATES <- list("Afatinib"     = data.frame(WT=70, SEX=0, FOOD=0, ECOG=0, LDH=241, AP=251, CLCR=120, IND=0, TPRO=72),
                   "Axitinib"     = data.frame(WT=70, AGE=60, SMOKING=0, JAPANESE=0),
                   "Cabozantinib" = data.frame(WT=70, SEX=0, CAPS=0, MAL=0, AGE=60, LIVER=0),
                   "Dabrafenib"   = data.frame(WT=70, SEX=0),
                   "Lenvatinib"   = data.frame(WT=75, INHIBITOR=0, INDUCER=0, ALB=0, ALP=0), ## TODO: complete covariates
                   "Nilotinib"    = data.frame(SEX=0, TBIL=0.5, MD=0),
                   "Ruxolitinib"  = data.frame(SEX=0, WT=70),
                   "Trametinib"   = data.frame(WT=70)
                   )

# These are the default values for the pre-defined dosing Schemes
SCHEMES <- list("Afatinib"     = data.frame(amt=c(50 , 40 , 30 , 20 , 10 ), ii=c(24,24,24,24,24), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Axitinib"     = data.frame(amt=c(7  , 5  , 3  , 1  , 0.5), ii=c(12,12,12,12,12), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Cabozantinib" = data.frame(amt=c(160, 80 , 60 , 40 , 20 ), ii=c(24,24,24,24,24), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Dabrafenib"   = data.frame(amt=c(150, 125, 75 , 50 , 25 ), ii=c(12,12,12,12,12), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Lenvatinib"   = data.frame(amt=c(14 , 12 , 10 , 8  , 4  ), ii=c(24,24,24,24,24), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Nilotinib"    = data.frame(amt=c(400, 300, 150, 100, 50 ), ii=c(12,12,12,12,12), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Ruxolitinib"  = data.frame(amt=c(40 , 20 , 15 , 10 , 5  ), ii=c(12,12,12,12,12), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?")),
                "Trametinib"   = data.frame(amt=c(3  , 2  , 1.5, 1  , 0.5), ii=c(24,24,24,24,24), cmin=c("?","?","?","?","?"), auc=c("?","?","?","?","?"))
                )

# These are the default values for the custimizable dosing scheme
CUSTOM_SCHEMES <- list("Afatinib"     = data.frame(amt=c(40) , ii=c(24),  cmin="?", auc="?"),
                       "Axitinib"     = data.frame(amt=c(3)  , ii=c(12),  cmin="?", auc="?"),
                       "Cabozantinib" = data.frame(amt=c(60) , ii=c(24),  cmin="?", auc="?"),
                       "Dabrafenib"   = data.frame(amt=c(75) , ii=c(12),  cmin="?", auc="?"),
                       "Lenvatinib"   = data.frame(amt=c(14) , ii=c(24),  cmin="?", auc="?"),
                       "Nilotinib"    = data.frame(amt=c(150), ii=c(12),  cmin="?", auc="?"),
                       "Ruxolitinib"  = data.frame(amt=c(20) , ii=c(12),  cmin="?", auc="?"),
                       "Trametinib"   = data.frame(amt=c(2)  , ii=c(24),  cmin="?", auc="?")
                       )

# Specify the default dosing compartment of the model
DOSING_CMT <- list("Afatinib"     = (cmt="GUT"),
                   "Axitinib"     = (cmt="GUT"),
                   "Cabozantinib" = (cmt=c("GUT", "CENT")), # Cabozantinib has mixed absorption !! Important !! first CENT dosing with zero order rate
                   "Dabrafenib"   = (cmt="GUT"),
                   "Lenvatinib"   = (cmt="GUT"), # TODO: check if mixed absorption model is correctly implemented
                   "Nilotinib"    = (cmt="CENT"),
                   "Ruxolitinib"  = (cmt="GUT"),
                   "Trametinib"   = (cmt="GUT")
                   )

# Specify models with zero-order absorption or mixed absorption
DOSING_RATE <- list("Afatinib"     = (rate=0),
                    "Axitinib"     = (rate=0),
                    "Cabozantinib" = c(rate=0, rate=-2), # Cabozantinib shows mixed absorption
                    "Dabrafenib"   = (rate=0),
                    "Lenvatinib"   = (rate=0), # TODO: check if implemented correctly
                    "Nilotinib"    = (rate=-2), # Nilotinib shows zero order absorption rate
                    "Ruxolitinib"  = (rate=0),
                    "Trametinib"   = (rate=0)
                    )

# Define fraction of dose to be absorbed by mixed absorption models
DOSING_FRACTION<- list("Afatinib"     = (fraction=1),
                       "Axitinib"     = (fraction=1),
                       "Cabozantinib" = c(fraction=1, fraction=1), # Dose is split in the model itself
                       "Dabrafenib"   = (fraction=1),
                       "Lenvatinib"   = (fraction=1),
                       "Nilotinib"    = (fraction=1),
                       "Ruxolitinib"  = (fraction=1),
                       "Trametinib"   = (fraction=1))

# ---- color scheme and plot theme ---

main_plot_col  <- "#E95420"

limit_plot_col <- "#990000"
text_plot_col  <- "grey20"
cont_plot_col  <- "lightgrey"
backg_plot_col <- "white"
line_plot_col  <- "gray"
plot_grid_col  <- "gray"


text_size <- 12

plot_theme <- theme(axis.text.x = element_text(colour=text_plot_col,size=text_size,angle=45,hjust=.5,vjust=.5,face="plain"),
                    axis.text.y = element_text(colour=text_plot_col,size=text_size,angle=0,hjust=1,vjust=0,face="plain"),  
                    axis.title.x = element_text(colour=text_plot_col,size=text_size+1,angle=0,hjust=.5,vjust=0,face="bold"),
                    axis.title.y = element_text(colour=text_plot_col,size=text_size+1,angle=90,hjust=0.5,vjust=2,face="bold"), 
                    legend.position = "bottom", legend.justification = c(0,1), legend.text=element_text(size=text_size-1),
                    legend.key = element_rect(fill = "white"),
                    plot.title = element_text(size=text_size+1, face = "bold"),
                    plot.subtitle = element_text(size=text_size),
                    panel.background = element_rect(fill = backg_plot_col, colour = cont_plot_col), panel.border = element_blank(), panel.grid.major = element_line(colour = plot_grid_col, linetype = 2),
                    panel.grid.minor = element_line(colour = plot_grid_col, linetype = 2), axis.line = element_line(colour = line_plot_col))


##
# ---- End definition of global variables
##


##
#  ---- Begin Definition of global available functions
##
  

  
  
  