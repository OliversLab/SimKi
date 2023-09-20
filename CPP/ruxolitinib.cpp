[PROB]

# Ruxolitinib popPK-model

  - Author: "Oliver Scherf-Clavel"
  - Date: "`r date()`"
  - Source: "Chen, X., et al., J Clin Pharmacol"
  - URL: "https://doi.org/10.1002/jcph.102"
  


  
[SET] delta = 0.1, end=72, ss_cmt = "CENT"
  
[THETA] @annotated
  
4.12  : proportion ka (1/h)
22.1  : proportion CL/F for male (L/h)
0.80  : proportion of clearance reduction in females
58.6  : population VC/F (L)
11.2  : proportion Vp/F (L)


[PARAM] @annotated
    
SEX             : 0     : 0 = male, 1 = female
WT              : 72.9  : weight (kg)
  
ETA1            : 0     : ETA used in MAP Bayes for CL/F
ETA2            : 0     : ETA used in MAP Bayes for VC/F
ETA3            : 0     : ETA used in MAP Bayes for VP/F
ETA4            : 0     : ETA used in MAP Bayes for ka

[FIXED] @annotated

POP_WT : 72.9   : mean population WT (kg)
Qp_F   : 2.53   : Qp/F (L/h)
LAG_T  : 0.0545 : ALAG - lag time from intake to beginning ob absorption (h)
  
[CMT] @annotated
GUT : Dosing compartment (mg)
PER : Peripheral compartment (mg)
AUC : cumulative AUC (ng/mL*h)
    
[INIT] @annotated
CENT : 0 : Central compartment (mg)

[OMEGA] @annotated @block
ECL : 0.1423                                        : ETA on CL/F
EVC : 0.0000 0.0755                                 : ETA on VC/F
EVP : 0.0000 0.0000 0.7131                          : ETA on Vp/F
EKA : 0.0000 0.0000 0.0000 0.446                    : ETA on KA
    
[SIGMA] @name SGMA @annotated
PROP : 0.126  : Proportional residual error
ADD : 0 : no additive error
  
[MAIN]

double CL_ind = THETA2 * pow(THETA3, SEX) * exp(ECL+ETA1);
double VC_ind = THETA4 * (WT/POP_WT)  * exp(EVC+ETA2);
double VP_ind = THETA5 * exp(EVP+ETA3);
double KA_ind = THETA1 * exp(EKA+ETA4);

ALAG_GUT = LAG_T;
F_GUT = 1.0;

[ODE]


dxdt_GUT  = - KA_ind*GUT;
dxdt_CENT = + KA_ind*GUT - (CL_ind/VC_ind) * CENT + (Qp_F/VP_ind) * PER - (Qp_F/VC_ind) * CENT;
dxdt_PER  = - (Qp_F/VP_ind) * PER + (Qp_F/VC_ind) * CENT;
dxdt_AUC = CENT/VC_ind*1000;

  
[TABLE]
capture IPRED = CENT/VC_ind*1000;
double DV = IPRED*(1+PROP);

while (DV < 0) {
  simeps();
  DV = IPRED*(1+PROP);
}

[CAPTURE] @annotated
  DV  : Plasma concentration (nmol/L)
  
  ECL : ETA on CL/F
  EVC : ETA on VC
  EVP : ETA on VP
  EKA : ETA on KA
  
  ETA1: MAP estimate on CL
  ETA2: MAP estimate on VC
  ETA3: MAP estimate on VP
  ETA4: MAP estimate on KA
  
