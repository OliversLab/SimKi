library(mrgsolve)
library(ggplot2)

mread("./CPP/cabozantinib") -> cab_mod

# TODO Nilotinib Modell darf nicht ins periphere Überarbeiten!!
AFA_mod %>% ev(amt=150, ii=12, ss=1, SEX=0, rate=-2) %>% zero_re() %>%
  mrgsim(end=12, delta=0.1) %>% plot()
  
param(SEX=0, TPRO=60, AP=76, LDH=76) %>% 
  mrgsim(end=24, delta=0.1) %>% filter(time > 0) %>%  
  ggplot(aes(x=time, y= IPRED)) + geom_line() 


mread("./CPP/Axitinib") -> AXI_mod


AXI_mod %>% ev(amt=5, cmt="GUT", ii=24, ss=1) %>% zero_re() %>% 
  mrgsim(end=24, delta=0.1) %>% filter(time > 0) %>%  
  ggplot(aes(x=time, y= IPRED)) + geom_line() 



code <- '
$PARAM CL = 1, VC = 20, KA = 1.1, FRAC = 0.2, DUR = 0.5
 
$CMT DEPOT CENT

$MAIN
F_DEPOT = 1-FRAC;
F_CENT = FRAC;
D_CENT = DUR;
ALAG_DEPOT = DUR;
 
$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT = KA*DEPOT - (CL/VC)*CENT;

$SET end = 24, delta = 0.1
 '

library(mrgsolve)
library(dplyr)
split_dose <- function(amt, cmt = c("CENT", "DEPOT"), ... ) {
  stopifnot(length(cmt)==2)
  immed <- ev(amt = amt, cmt = cmt[1], rate = -2, ...)
  slow <-  ev(amt = amt, cmt = cmt[2], ...)
  c(immed, slow)
}

e <- split_dose(100, ss=1, ii=24)

mcode("mod", code) -> my_mod


my_mod %>% ev(e) %>% 
  mrgsim(end=24, delta=0.1) %>% plot()



split_dose_inverse <- function(amt, cmt = c("CENT", "DEPOT"), ... ) {
  stopifnot(length(cmt)==2)
  immed <- ev(amt = amt, cmt = cmt[1], rate = -2, ...)
  slow <-  ev(amt = amt, cmt = cmt[2], ...)
  c(slow, immed)
}

e_inv <- split_dose_inverse(100, ss=1, ii=24)

my_mod %>% ev(e_inv) %>%
  mrgsim(end=24, delta=0.1) %>% plot()

