[PROB]

# Dabrafenib popPK-model

- Author: "Oliver"
- Date: "`r date()`"
- Source: "FDA-Application 202806Orig1s000"
- URL: "www.accessdata.fda.gov/drugsatfda_docs/nda/2013/202806Orig1s000ClinPharmR.pdf"

```{r, echo=TRUE}
mod %>% ev(amt=150, ii=12, addl=7) %>% mrgsim %>% plot
```


[SET] delta = 0.1, end=1, ss_cmt = "CENT"

[PARAM] @annotated

THETA1 :17.0  : non-inducible apparent Clearance CL0/F (L/hr)
THETA2 :70.3  : apparent vol cent compartment VC/F (L)
THETA3 :154.0 : apparent vol per compartment VP/F (L)
THETA4 :3.30  : apparent intercomp Clearance Q/F (L/hr)
THETA5 :1.88  : Absorption rate constant (1/hr)
THETA6 :0.482 : Tlag (hr)
THETA7 :17.3  : inducible apparent clearance CLind,SS/F (L/hr)
THETA8 :0.927 : Alpha
THETA9 :67.3  : T50 (hr)
THETA10 :0.555 : FGel
THETA11 :0.331 : exponent Clearance on WT
THETA12 :0.914 : covariate Clearance on SEX
THETA13 :0.384 : exponent central compartment volume on WT
THETA14 :1.22  : exponent intercompartimental Clearance on WT

[PARAM] @annotated

SEX  : 0   : 0 = male, 1 = female
WT   : 80  : weight (kg)
DOSE : 300 : total daily dose (mg)

ETA1 : 0 : individual ETA on CL/F used in MAP Bayes
ETA2 : 0 : individual ETA on VC used in MAP Bayes
ETA3 : 0 : individual ETA on Q used in MAP Bayes
ETA4 : 0 : individual ETA on KA used in MAP Bayes

[FIXED]  @annotated
D_REF : 300 : reference dose (mg)

[CMT] @annotated
GUT : Dosing compartment (mg)
PER : Peripheral compartment (mg)
AUC : cumulative AUC (ng/mL*h)

[INIT] @annotated
CENT : 0 : Central compartment (mg)

[OMEGA] @annotated @block
ECL : 0.343                    : ETA on CL/F
EVC : 0.292 0.281              : ETA on VC
EQ  : 0.000 0.000 0.980        : ETA on Q
EKA : 0.000 0.000 0.0000 2.57  : ETA on KA

[SIGMA] @name SGMA @annotated
PROP : 0.28  : Proportional residual error
ADD  : 17.6  : Additive residual error (ng/mL)

[MAIN]

double CL_ind = THETA7 * pow((DOSE*THETA10/D_REF), THETA8) * (1-exp(-0.693*TIME/THETA9));
double CL = (THETA1+CL_ind) * pow(WT/80,THETA11) * pow(THETA12, SEX) * exp(ETA1+ECL);
double VC = THETA2 * pow(WT/80,THETA13)  * exp(ETA2+EVC);
double VP = THETA3;
double KA = THETA5 * exp(ETA4+EKA);
double Q  = THETA4 * pow(WT/80, THETA14) * exp(ETA3+EQ);

ALAG_GUT = THETA6;
F_GUT = 0.95;

[ODE]


dxdt_GUT  = - KA*GUT;
dxdt_CENT = + KA*GUT - (CL/VC) * CENT + (Q/VP) * PER - (Q/VC) * CENT;
dxdt_PER  = - (Q/VP) * PER + (Q/VC) * CENT;
dxdt_AUC  = + CENT/VC*1000;


[TABLE]
capture IPRED = CENT/VC*1000;
double DV = IPRED*(1+PROP) + ADD;
while (DV < 0) {
  simeps();
  DV = IPRED*(1+PROP) + ADD;
}

[CAPTURE] @annotated
  DV  : Plasma concentration (ng/mL)
  IPRED : Plasma concentration (ng/mL)
  ECL : ETA on CL/F
  EVC : ETA on VC
  EQ  : ETA on Q
  EKA : ETA on KA
  
  ETA1: MAP estimate on CL
  ETA2: MAP estimate on VC
  ETA3: MAP estimate on Q
  ETA4: MAP estimate on KA