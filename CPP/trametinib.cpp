$PROB

# Trametinib popPK-model

- Author: "Oliver"
- Date: "`r date()`"
- Source: "FDA-Application 204114Orig1s000, Ouellet et al. 2016"
- URL: "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2013/204114Orig1s000ClinPharmR.pdf"

```{r, echo=TRUE}
mod %>% ev(amt=2, ii=12, addl=7) %>% mrgsim %>% plot
  ```
$GLOBAL

double LAST_DOSE = 0;

$SET delta = 0.1, end=72, ss_cmt = "CENT"

$PARAM @annotated
  
  CL_F  : 4.91  : apparent Clearance CL/F (L/hr)
  VC_F  : 214   : apparent vol cent compartment VC/F (L)
  Q_F   : 60.0  : apparent intercomp Clearance Q/F (L/hr)
  VP_F  : 568.0 : apparent vol per compartment VP/F (L)
  KA_1  : 0.142 : first order absorption rate  before MTIME (1/hr)
  KA_2  : 2.05  : first order absorption rate  after MTIME (1/hr)
  MTIME : 0.400 : time when KA changes (h)
  
  THETA1 : 0.211 : Weight on CL/F
  THETA2 : 1.26  : Sex on CL/F
  THETA3 : 5.90  : Weight on Q/F
  
  SEX  : 0   : 1 = male, 0 = female
  WT   : 79  : weight (kg)

  ETA1 : 0 : individual ETA on CL/F used in MAP Bayes
  ETA2 : 0 : individual ETA on VC used in MAP Bayes
  ETA3 : 0 : individual ETA on Q used in MAP Bayes
  ETA4 : 0 : individual ETA on KA used in MAP Bayes
  
  
$CMT @annotated
  GUT : Dosing compartment (mg)
  PER : Peripheral compartment (mg)
  AUC : cumulative AUC (ng/mL*h)
  
$INIT @annotated
  CENT : 0 : Central compartment (mg)
  
$OMEGA @annotated @block
  ECL : 0.056                    : ETA on CL/F
  EVC : 0.000 0.465              : ETA on VC
  EQ  : 0.000 0.000 1.730        : ETA on Q
  EKA : 0.000 0.000 0.0000 0.654 : ETA on KA
  
$SIGMA @name SGMA @annotated
  PROP : 0.05  : Proportional residual error
  ADD  : 1.88  : Additive residual error (ng/mL)
  
$MAIN

  double TVCL = CL_F*pow(WT/79, THETA1)*pow(THETA2, SEX);
  double CL_i = TVCL * exp(ETA1+ECL);
  
  double TVVC = VC_F;
  double VC_i = TVVC * exp(ETA2+EVC);
  
  double TVVP = VP_F;
  double VP_i = TVVP;
  
  double TVQ  = Q_F * pow(WT/79, THETA3);
  double Q_i  = TVQ * exp(ETA3+EQ);
  
  double KA1_i = KA_1 * exp(ETA4+EKA);
  double KA2_i = KA_2;
  
  F_GUT = 1.0;
  
  if(EVID==1){
     LAST_DOSE = TIME;
  }
  if(TIME <= LAST_DOSE+MTIME){
    double KA = KA1_i;
  }
  else{
    KA = KA2_i;
  }

$ODE

  dxdt_GUT  = - KA*GUT;
  dxdt_CENT = + KA*GUT - (CL_i/VC_i) * CENT + (Q_i/VP_i) * PER - (Q_i/VC_i) * CENT;
  dxdt_PER  = - (Q_i/VP_i) * PER + (Q_i/VC_i) * CENT;
  
  dxdt_AUC = CENT/VC_i*1000;


$TABLE
  capture IPRED = CENT/VC_i*1000;
  double DV = IPRED*(1+PROP) + ADD;
  while (DV < 0) {
    simeps();
    DV = IPRED*(1+PROP) + ADD;
  }

$CAPTURE @annotated
  DV  : Plasma concentration (ng/mL)
    
  ECL : ETA on CL/F
  EVC : ETA on VC
  EQ  : ETA on Q
  EKA : ETA on KA
    
  ETA1: MAP estimate on CL
  ETA2: MAP estimate on VC
  ETA3: MAP estimate on Q
  ETA4: MAP estimate on KA

  