[PROB]

# Afatinib popPK-Model

  - Author "Oliver"
  - Date: "`r date()`"
  - Source: "Freiwald et al.Cancer Chemother Pharmacol (2014) 73:759-770"
  - URL: "https://doi.org/10.1007/s00280-014-2403-2"
  
  [SET] delta = 0.1, end=24, ss_cmt = "CENT"
  
  [THETA] @annotated
    0.485 : theta 1 theta_slope for Dose > 70mg on F1
    0.739 : theta 2 theta_food for Food on F1
    1.08  : theta 3 theta_ecog 1 on F1
    1.27  : theta 4 theta_ecog >=2 on F1
    0.000331 : theta 5 theta_LDH on F1
    0.00128 : theta 6 theta_AP on F1
    1.35    : theta 7 theta_ind (HNSCC) on F1 

    0.899   : theta 8 theta_wt on V2
    
    0.595   : theta 9 theta_wt on CL
    0.00484 : theta 10 theta CLCR on CL
    0.871   : theta 11 theta female on CL
    -0.00436: theta 12 theta TPRO on CL
  
  [FIXED] @annotated
    CL: 42.3   : Clearance CL/F (L/h)
    V2: 456    : Volume V2/F (L)
   K23: 0.170  : distribution cate konstant (1/h)
   K32: 0.0685 : distribution cate konstant (1/h)
    F1: 1.00   : rel. F
    KA: 0.252  : absorption rate constant
    
  [PARAM] @annotated
    SEX : 1   : 0 - male, 1 - female
    FOOD: 0   : with food
    DOSE: 70  : dose (mg)
    ECOG: 0   : 0, 1, >=2 
    LDH : 241: (U/L)
    AP  : 251   : <= 251 (U/L)
    WT  : 75  : body weight (kg)
    CLCR: 120   : CLCR (mL/min)
    IND : 0   :  0 - NSCLC or BC  1 - Indication HNSCC
    TPRO: 72  : (g/L)
    ETA1: 0 : ETA for F1 used in MAP
    ETA2: 0 : ETA for ka used in MAP

    
  [CMT] @annotated
    GUT : gut compartment (mg)
    PER : peripheral compartment (mg)
    AUC : cumulative AUC (ng/mL*h)

    
  [INIT] @annotated
    CENT: 0 : central compartment (mg)
  
  [OMEGA] @annotated @block
    EF1: 0.208849         : ETA on F1
    EKA: 0.000000 0.59753 : ETA on KA
  
  [SIGMA] @name SGMA @annotated
    PROP: 0.265 : proportional error
    ADD : 2.08  : ng/mL
  
  [MAIN]
    
    double F1_ind = F1;
      
    if (DOSE > 70) {
	      F1_ind = F1 * pow((DOSE/70), THETA1)* pow(THETA2, FOOD);
    } else {
	      F1_ind = F1* pow(THETA2, FOOD);
    }
    if (ECOG == 1) {
	      F1_ind = F1_ind * THETA3;
    } else if (ECOG >= 2) {
	      F1_ind = F1_ind * THETA4;
    } else {
	      F1_ind = F1_ind;
    }

    F1_ind = F1_ind * (1+THETA5*(LDH-241)) * (1+THETA6*(AP-251))*pow(THETA7, IND)*exp(ETA1+EF1);

    double V2_ind = V2 *pow(WT/62, THETA8);
    double CL_ind = CL * pow(WT/62, THETA9)*(1+THETA10*(CLCR-120))*pow(THETA11, SEX)*(1+THETA12*(TPRO-72));
    double KA_ind = KA * exp(ETA2+EKA);    

    F_GUT = F1_ind;
    
  [ODE]
    
    dxdt_GUT = - KA_ind * GUT;
    dxdt_CENT = + KA_ind * GUT - (CL_ind/V2_ind) * CENT - K23 * CENT + K32 * PER;
    dxdt_PER  = + K23 * CENT - K32 * PER;
    dxdt_AUC = CENT/V2_ind*1000;
  
  [TABLE]
    capture IPRED = CENT/V2_ind * 1000; // convert from mg/L to ng/mL
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
    F1_ind : individual F
    DV : Plasma concentration (ng/L)
    EF1 : eta F
    EKA : eta KA
      
    ETA1: MAP estimate on ETA F
    ETA2: MAP estimate on ETA KA