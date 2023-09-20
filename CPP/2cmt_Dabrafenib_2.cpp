[PROB]

# Dabrafenib popPK-model

- Author: "Oliver"
- Date: "`r date()`"
- Source: "Balakirouchenane et al. 2020 Cancers, 12, 931"
- URL: "doi: 10.3390/cancers12040931"

```{r, echo=TRUE}
mod %>% ev(amt=150, ii=12, addl=7) %>% mrgsim %>% plot
```


[SET] delta = 0.1, end=1

[PARAM] @annotated

THETA1 : 19.3  : apparent Clearance CL/F (L/hr)
THETA2 : 39.1  : apparent vol cent compartment VC/F (L)
THETA3 : 18.7 : apparent vol per compartment VP/F (L)
THETA4 : 3.4  : apparent intercomp Clearance Q/F (L/hr)
THETA5 : 1.8  : Absorption rate constant (1/hr)
THETA6 : 0.499 : Tlag (hr)
THETA7 : 23.2 : Clearance of metabolite (L/hr)
THETA8 : 5.11 : Central compartment of metabolite (L)
THETA9 : 7.21 : intercompartmental clearance of metabolite (L/hr)
THEAT10 : 27.1 : peripheral compartment of metabolite (L)

THETA11 :-0.536 : age on Dabrafenib Clearance
THETA12 :-0.589 : age on OHDabrafenib Clearance
THETA13 :0.83 : sex on dabrafenib clearance

[PARAM] @annotated

SEX  : 0   : 0 = male, 1 = female
AGE : 61.2 : age in years

ETA1 : 0 : individual ETA on CL/F used in MAP Bayes
ETA2 : 0 : individual ETA on VC used in MAP Bayes
ETA3 : 0 : individual ETA on CL_M used in MAP Bayes
ETA4 : 0 : individual ETA on VC_M used in MAP Bayes

[CMT] @annotated
GUT : Dosing compartment (mg)
PER : Peripheral compartment (mg)
AUC : cumulative AUC (ng/mL*h)

CENT_M : central compartment of metabolite (mg)
PER_M : Peripheral compartment of metabolite (mg)
AUC_M : cumulative AUC of metabolite (ng/mL*h)

[INIT] @annotated
CENT : 0 : Central compartment (mg)

[OMEGA] @annotated @block
ECL  : 0.0256                    : ETA on CL/F
EVC  : 0.000 0.258               : ETA on VC
ECLM : 0.000 0.000 0.0576        : ETA on CL_M
EVCM : 0.000 0.000 0.0000 0.226  : ETA on VC_M

[SIGMA] @name SGMA @annotated @block
PROP   : 0.237  : Proportional residual error
PROPM  : 0.058 0.282  : Additive residual error (ng/mL)

[MAIN]

double CL_D_ind =  THETA1 * (1+THETA11*(AGE-61.2)/61.2) * pow(THETA13, SEX) * exp(ECL+ETA1);
double CL_M_ind =  THETA7 * (1+THETA12*(AGE-61.2)/61.2)  * exp(ECLM+ETA3);
double VC_D_ind =  THETA2 * exp(EVC+ETA2);
double VC_M_ind =  THETA8 * exp(EVCM+ETA4);

double KA = THETA5;

ALAG_GUT = THETA6;

[ODE]


dxdt_GUT  = - KA*GUT;
dxdt_CENT = + KA*GUT - (CL_D_ind/VC_D_ind) * CENT + (THETA4/THETA3) * PER - (THETA4/VC_D_ind) * CENT;
dxdt_PER  = - (THETA4/THETA3) * PER + (THETA4/VC_D_ind) * CENT;
dxdt_AUC  = CENT/VC_D_ind*1000;

dxdt_CENT_M = + (CL_D_ind/VC_D_ind) * CENT + (THETA9/THEAT10) * PER_M - (THETA9/VC_M_ind) * CENT_M - (CL_M_ind/VC_M_ind)*CENT_M;
dxdt_PER_M  = - (THETA9/THEAT10) * PER_M + (THETA9/VC_M_ind) * CENT_M;
dxdt_AUC_M  = CENT_M/VC_M_ind*1000;


[TABLE]
capture IPRED = CENT/VC_D_ind*1000;
capture IPRED_M = CENT_M/VC_M_ind*1000;
double DV = IPRED*(1+PROP);
while (DV < 0) {
  simeps();
  DV = IPRED*(1+PROP);
}
double DV_M = IPRED_M*(1+PROPM);
while (DV_M < 0) {
  simeps();
  DV_M = IPRED_M*(1+PROPM);
}

[CAPTURE] @annotated
DV  : Plasma concentration (ng/mL)
DV_M : HydroxyDabrafenib plasma concentration (ng/mL)
AGE : age in years
SEX : SEX = 0 => male 1 => female
