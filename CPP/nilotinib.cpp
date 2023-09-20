[PROB]

# Nilotinib popPK-Model

  - Author "Oliver"
  - Date: "`r date()`"
  - Source: "Giles et al. Eur J Clin Pharmacol (2013) 69:813–823 "
  - URL: "https://doi.org/10.1007/s00228-012-1385-4"
  
  [SET] delta = 0.1, end=24, ss_cmt = "CENT"
  
  [THETA] @annotated
    1.000 : theta_f1 (fixed)
   -1.650 : theta_dose on F1
    0.649 : theta_md morning dose on F1
    0.835 : theta_male male on F1
    12.80 : theta_CL - pop Clearance (L/h)
   -0.117 : theta_CL_TBIL bilirubin on clearance
  
  [FIXED] @annotated
    V1: 56.00 : Volume of the central compartment (L)
    V2: 247.0 : Volume of the peripheral compartment (L)
     Q: 103.0 : Intercompartmental clearance (L/h)
    
  [PARAM] @annotated
    SEX : 1   : 1 - male, 0 - female
    TBIL: 0.5  : total bilirubin
    DOSE: 400 : administered dose (mg)
    MD  : 1   : 1 - morning dose, 0 - evening dose
    
    ETA1: 0 : ETA for F1 used in MAP
    ETA2: 0 : ETA for CL used in MAP
    ETA3: 0 : ETA for V1 used in MAP
    ETA4: 0 : ETA for V2 used in MAP
    
  [CMT] @annotated
    PER: peripheral compartment (mg)
    AUC : cumulative AUC (ng/mL*h)
    
  [INIT] @annotated
    CENT: 0 : central compartment (ng)
  
  [OMEGA] @annotated @block
    EF1: 0.3250                       : ETA on F1
    ECL: 0.0660 0.0722                : ETA on CL
    EV1: 0.0928 0.0277 0.9790         : ETA on V1
    EV2: 0.0000 0.0000 0.0000 0.4200  : ETA on V2
  
  [SIGMA] @name SGMA @annotated
    PROP: 0.333 : proportional error
    ADD: 62.9 : ng/mL
  
  [MAIN]
    double ind_SEX = 1;
    if(SEX==1) {
      ind_SEX = 0; // coding is different from other models needs to be inversed here
    } 
    double F1_ind = THETA1*exp(THETA2*(DOSE-50)/1150)*pow(THETA3,MD)*pow(THETA4, ind_SEX)*exp(EF1+ETA1);
    double CL_ind = THETA5*exp(THETA6*(TBIL-0.5))*exp(ECL+ETA2);
    double V1_ind = V1 * exp(EV1+ETA3);
    double V2_ind = V2 * exp(EV2+ETA4);
    
    ALAG_CENT = 0.746;  // lag-time (h)
    D_CENT = 3.02;     // zero-order input duration (h)
    F_CENT = F1_ind;
    
  [ODE]
    
    dxdt_CENT = - (CL_ind/V1_ind) * CENT + (Q/V2_ind) * PER - (Q/V1_ind) * CENT;
    dxdt_PER  = - (Q/V2_ind) * PER + (Q/V1_ind) * CENT;
    dxdt_AUC = CENT/V1_ind*1000;
  
  [TABLE]
    capture IPRED = CENT/V1_ind * 1000; // convert mg/L to ng/mL
    double DV = IPRED * (1+PROP)+ADD;
    
    int i = 0;
    while(DV < 0) {
      if(++i > 100) {
        mrg::report("Problem simulating positive CP");
        break;
      }
      simeps();
      DV = IPRED * (1+PROP)+ADD;
    }
  
  [CAPTURE] @annotated
    DV : Plasma concentration (ng/L)
      
    EF1 : eta F
    ECL : eta CL
    EV1 : eta V1
    EV2 : eta V2
      
    ETA1: MAP estimate on F
    ETA2: MAP estimate on CL
    ETA3: MAP estimate on V1
    ETA4: MAP estimate on V2