[PROB]

# Cabozantinib popPK-model

  - Author: "Oliver"
  - Date: "`r date()`"
  - Source: "Nguyen et al. J Clin Pharmacol, 2019, p.1-11"
  - URL: "https://doi.org/10.1002/jcph.1467"
  
```{r, echo=TRUE}
  mod %>% ev(amt=60, ii=24, addl=7) %>% mrgsim %>% plot
```

  
[SET] delta = 0.25, end=1, ss_cmt = "CENT"
  

[THETA] @annotated
  1.23  : ref value Ka (1/h)
  2.53  : Duration of absorption for the zero-order absorption process (h)
  2.47  : ref value Cl/F (L/h)
  214   : ref value Vc/F (L)
  30.2  : ref value Q/F  (L/h)
  179   : ref value Vp/F (L)
  0.82  : ALAG1 (h)
  0.83  : transformed estimate of pop F1
  0.564 : Dose-dependent Ka
  0.528 : Capsule on Ka
  0.841 : Capsule on overall relative oral availability
  -0.16 : Age on Cl/F
  0.762 : Female on Cl/F
  1.18  : Race (black) on Cl/F
  0.934 : Race (asian) on Cl/F
  1.02  : Race (other) on Cl/F
  -0.0209: Weight on Cl/F
  0.862: RCC on Cl/F
  0.968: CRPC on Cl/F
  1.88 : MTC on Cl/F
  1.2 : GB on Cl/F
  1.14 : other malignancies on Cl/F
  0.0077: Age on Vc/F
  1.08  : Female on Vc/F
  1.07: Race (black) on Vc/F
  0.739  : Race (asian) on Vc/F
  0.965: Race (other) on Vc/F
  1.2 : Weight on Vc/F
  0.711 : RCC on Vc/F
  0.721: CRPC on Vc/F
  0.912 : MTC on Vc/F
  0.448: GB on Vc/F
  0.749: other malignancies on Vc/F
  0.82: HCC on Cl/F //34
  0.81: HCC on Vc/F //35
  1.12 : mild liver dysfunction on Cl/F
  1.04 : mild liver dysunction on Vc/F
  0.978: moderate and severe liver dysfunction on Cl/F
    
[PARAM] @annotated
    
SEX  : 0   : 0 = male, 1 = female
WT   : 80  : weight (kg)
RACE : 0   : 0 = white, 1 = black, 2 = asian, 3 = other
CAPS : 0   : 0 = tablet, 1 = capsule
MAL  : 0   : 0 = RCC, 1 = CRPC, 2 = MTC, 3 = GB, 4 = HCC, 5=other
AGE  : 34  : age (years)
DOSE : 60  : Dose (mg)
LIVER: 0   : 0 = normal liver function, 1=mild liver dysfunction, 2=moderate and severe liver dysfunction  


ETA1 : 0 : individual ETA on CL/F used in MAP Bayes
ETA2 : 0 : individual ETA on VC used in MAP Bayes
ETA3 : 0 : individual ETA on F1 used in MAP Bayes
ETA4 : 0 : individual ETA on KA used in MAP Bayes
    
[FIXED]  @annotated
WT_REF   : 82.1 : reference (mean) weight (kg)
AGE_REF  : 61.3 : reference (mean) age (years)
DOSE_REF : 60  : reference (find out) dose (mg)
    
[CMT] @annotated
GUT : GUT compartment (mg)
PER : Peripheral compartment (mg)
AUC : cumulative AUC (ng/mL*h)

    
[INIT] @annotated
CENT : 0 : Central compartment (mg)
    
[OMEGA] @annotated @block
ECL : 0.210                    : ETA on CL/F
EVC : 0.199 0.430              : ETA on VC  
EF1 : 0.000 0.000 2.73         : ETA on F1
EKA : 0.000 0.000 0.0000 2.21  : ETA on KA
    
[SIGMA] @name SGMA @annotated
PROP : 0.0      : no prop error 
LADD : 0.127    : log transformed additive error 
    
[MAIN]
  
double TVCL = THETA3*pow(AGE/AGE_REF, THETA12) * pow(THETA13,SEX)*pow(WT/WT_REF, THETA17);
double TVVC = THETA4*pow(AGE/AGE_REF, THETA23) * pow(THETA24,SEX)*pow(WT/WT_REF, THETA28);

if (RACE == 1){
  TVCL = TVCL * (THETA14);
  TVVC = TVVC * (THETA25);
}
else if (RACE == 2){
  TVCL = TVCL * (THETA15);
  TVVC = TVVC * (THETA26);
}
else if (RACE == 3){
  TVCL = TVCL * (THETA16);
  TVVC = TVVC * (THETA27);
}

if (MAL == 0){
  TVCL = TVCL * (THETA18);
  TVVC = TVVC * (THETA29);
}
else if (MAL == 1){
  TVCL = TVCL * (THETA19);
  TVVC = TVVC * (THETA30);
}
else if (MAL == 2){
  TVCL = TVCL * (THETA20);
  TVVC = TVVC * (THETA31);
}
else if (MAL == 3){
  TVCL = TVCL * (THETA21);
  TVVC = TVVC * (THETA32);
}
else if (MAL == 4){
  TVCL = TVCL * (THETA34);
  TVVC = TVVC * (THETA35);
}
else if (MAL == 5){
  TVCL = TVCL * (THETA22);
  TVVC = TVVC * (THETA33);
}

if(LIVER==1){
  TVCL = TVCL * (THETA36);
  TVVC = TVVC * (THETA37);
} else if (LIVER==2) {
  TVCL = TVCL * (THETA38);
}

double CL_ind = TVCL*exp(ETA1+ECL);
double VC_ind = TVVC*exp(ETA2+EVC);
double VP = THETA6;
double KA = THETA1* pow(DOSE/DOSE_REF, THETA9) * pow(THETA10, CAPS) * exp(ETA4+EKA); 
double Q  = THETA5;

ALAG_CENT = THETA7; 
ALAG_GUT = THETA7;
D_CENT = THETA2;

double F_typ = pow(THETA11,CAPS)*THETA8;
       
double F1=(exp(log(F_typ/(1-F_typ))+(ETA3+EF1)))/(1+exp(log(F_typ/(1-F_typ))+(ETA3+EF1)));

F_GUT  = F1;
F_CENT = (1-F1);

[ODE]

dxdt_GUT  = - KA*GUT;
dxdt_CENT = + KA*GUT - (CL_ind/VC_ind) * CENT + (Q/VP) * PER - (Q/VC_ind) * CENT;
dxdt_PER  = - (Q/VP) * PER + (Q/VC_ind) * CENT;
dxdt_AUC  = + CENT/VC_ind*1000;

  
[TABLE]
capture IPRED = CENT/VC_ind*1000;
double FLAG = 0;
if(IPRED == 0){
  FLAG = 1;
} 

double Y = (1-FLAG)*log(IPRED+FLAG) + LADD;

double DV = exp(Y);


[CAPTURE] @annotated
  DV  : Plasma concentration (ng/mL)
  ECL : ETA on CL/F
  EVC : ETA on VC
  EF1 : ETA on F1
  EKA : ETA on KA

  
  ETA1: MAP estimate on ECL
  ETA2: MAP estimate on Vc
  ETA3: MAP estimate on F1
  ETA4: MAP estimate on KA

