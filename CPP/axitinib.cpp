[PROB]

# Axitinib popPK-Model

- Author "Oliver"
- Date: "`r date()`"
- Source: "CLINICAL PHARMACOLOGY AND BIOPHARMACEUTICS REVIEW(S) 202324Orig1s000"
- URL: "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2012/202324orig1s000clinpharmr.pdf"

[SET] delta = 0.1, end=24, ss_cmt = "CENT"

[THETA] @annotated
  -0.213 : THETA1 - age > 60 effect on CL
    1.02 : THETA2 - smoking on CL
  -0.249 : THETA3 - Japanese on CL
  
    0.77 : THETA4 - WT on Vc
  
    1.97 : THETA5 - fasting on KA
  
   0.330 : THETA6 - fasting on F
  -0.121 : THETA7 - XLI formulation effect on F
  

      
[FIXED] @annotated
  CL   : 14.6 : Clearance (L/h)
  VC   : 47.3 : Volume of central compartment (L)
  Q    : 4.0  : intercompartmental Clearance (L/h)
  Vp   : 393  : Volume of peripheral compartment (L)
  KA   : 0.482: Ka for fasting subjects (1/h)
  tlag : 0.454: lag time for absorption (h)
  F1   : 0.457: F for fed subjects and Formulation IV

      
[PARAM] @annotated
  AGE      : 60 : age (years)
  WT       : 60 : body weight (kg)
    
  SMOKING  : 0  : 0 - no  1 - yes
  JAPANESE : 0  : 0 - no  1 - yes
  FASTING  : 0  : 0 - no 1 - yes
  XLI      : 1  : 0 - no 1 - yes // XLI ist the commercial formulation !!
    
  ETA1: 0 : ETA for F1 used in MAP // random effects (OMEGA) are set to zero in case of MAP estimation
  ETA2: 0 : ETA for Vc used in MAP
  ETA3: 0 : ETA for Q  used in MAP
  ETA4: 0 : ETA for KA used in MAP
          
          
[CMT] @annotated
  GUT : gut compartment (mg) // important GUT MUST be the first compartment
  PER : peripheral compartment (mg)
  AUC : cumulative AUC (ng/mL*h)
          
[INIT] @annotated
  CENT: 0 : central compartment (mg)
          
[OMEGA] @annotated @block
  ECL: 0.359                       : ETA on CL
  EVC: 0.200 0.158                 : ETA on EVC
  EQ : 0.000 0.000 0.754           : ETA on Q
  EKA: 0.000 0.000 0.000 0.593     : ETA on KA
          
[SIGMA] @name SGMA @annotated
  PROP: 0.339 : proportional error // 0.582%
  ADD : 0.0 : ng/mL
          
[MAIN]

double HIGH_AGE = 0; // used to recode numeric age in category 1 or 2

if(AGE > 60) {
  HIGH_AGE = 1;
  }

double CL_ind = CL * (1+THETA2*SMOKING) * (1+ THETA1*HIGH_AGE) * (1+THETA3*JAPANESE) * exp(ECL+ETA1);

double Vc_ind = VC * pow(WT/74.1, THETA4) * exp(EVC+ETA2);
double Q_ind  = Q  * exp(EQ+ETA3);
double Vp_ind = Vp * exp((EQ+ETA3)*1.0);  // correlation between VP_ind and Q_ind is 100%
double KA_ind = KA * (1+THETA5 * FASTING) * exp(EKA+ETA4);
double F1_ind = F1 * (1+THETA6 * FASTING) * (1+THETA7 * XLI);  

F_GUT = F1_ind;
ALAG_GUT = tlag;

[ODE]

dxdt_GUT  = - KA_ind * GUT;
dxdt_CENT = + KA_ind * GUT - (CL_ind/Vc_ind) * CENT - (Q_ind/Vc_ind) * CENT + (Q_ind/Vp_ind) * PER;
dxdt_PER  = + (Q_ind/Vc_ind) * CENT - (Q_ind/Vp_ind) * PER;
dxdt_AUC  = + CENT/Vc_ind*1000;

[TABLE]
capture IPRED = CENT/Vc_ind * 1000; // convert from mg/L to ng/mL
double DV = IPRED*(1+PROP);

int i = 0;
while(DV < 0) {
  if(++i > 100) {
    mrg::report("Problem simulating positive CP");
    break;
  }
  simeps();
  DV = IPRED*(1+PROP);
}

[CAPTURE] @annotated
  DV : Plasma concentration (ng/L)
  ECL: ETA on CL
  EVC: ETA on EVC
  EQ : ETA on Q
  EKA: ETA on KA
  
  ETA1: MAP estimate on ETA CL
  ETA2: MAP estimate on ETA VC
  ETA3: MAP estimate on ETA Q
  ETA4: MAP estimate on ETA KA