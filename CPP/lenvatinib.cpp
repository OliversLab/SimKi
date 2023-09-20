[PROB]

# Dabrafenib popPK-model

- Author: "Oliver"
- Date: "`r date()`"
- Source: "Gupta et al. Br J Clin Pharmacol (2016) 81 1124-1133"
- URL: ""

```{r, echo=TRUE}
mod %>% ev(amt=150, ii=12, addl=7) %>% mrgsim %>% plot
```


[SET] delta = 0.1, end=1, ss_cmt = "CENT"

[PARAM] @annotated

THETA1 :1.30  : Effect of CYP3A4 Inducers on CL
THETA2 :0.922  : Effect of CYP3A4 Inhibitors on CL
THETA3 :0.837 : Effect of  Albumin < 30g/L on CL
THETA4 :0.883 : Effect of ALP ratio >1 on CL
THETA5 :1.15  : Effect of population on CL 

THETA6 :6.56: basal CL (L/h) 
THETA7 :49.3  : basal V1/F (L) 
THETA8 :30.7 :  basal V2/F (L)
THETA9 :37.1  : basal V3/F (L)
THETA10 :3.52: basal Q1/F (L/h) 
THETA11 :0.769 : basal Q2/F (L/h) 
THETA12 :1.02 : basal ka (1/h) 
THETA13 :1.22 : basal D1 (h)
THETA14 :0.896 : relative bioavailability capsule vs tablet

[PARAM] @annotated

ALP  : 0   : 0 , 1
ALB  : 0  : 0 = > 30g/L, 1= < 30g/L
WT   : 75  : weight (kg)
CAPS : 1 : 0 = tablet, 1 = capsule
INDUCER : 0 : 0 = none, 1= any CYP3A4 inducer
INHIBITOR : 0 : 0 = none, 1= any CYP3A4 inhibitor
POP : 0 : 0 = cancer , 1 = healthy

ETA1 : 0 : individual ETA on CL/F used in MAP Bayes
ETA2 : 0 : individual ETA on V1/F used in MAP Bayes
ETA3 : 0 : individual ETA on V2/F used in MAP Bayes
ETA4 : 0 : individual ETA on V3/F used in MAP Bayes
ETA5 : 0 : individual ETA on KA used in MAP Bayes
ETA6 : 0 : individual ETA on D1 used in MAP Bayes //
ETA7 : 0 : individual ETA on F1 used in MAP Bayes //


[CMT] @annotated
GUT : Dosing compartment (mg)
PER : Peripheral compartment (mg)
PER2 : Peripheral compartment 2 (mg)
AUC : cumulative AUC (ng/mL*h)

[INIT] @annotated
CENT : 0 : Central compartment (mg)

[OMEGA] @annotated @block
ECL : 0.065                   			: ETA on CL/F
EV1 : 0.000 0.052              			: ETA on V1
EV2 : 0.000 0.000 0.152        			: ETA on V2
EV3 : 0.000 0.000 0.000 0.092 			: ETA on V3
EKA : 0.000 0.000 0.000 0.000 0.300		: ETA on KA
ED1 : 0.000 0.000 0.000 0.000 0.000 0.588	: ETA on D1
EF1 : 0.000 0.000 0.000 0.000 0.000 0.000 0.091	: ETA on F1

[SIGMA] @name SGMA @annotated
PROP : 0.333  : Proportional residual error
ADD  : 7.19  : Additive residual error (ng/mL)

[MAIN]

double CL_ind = THETA6 * pow((WT/75),0.75) * pow(THETA1, INDUCER) * pow(THETA2, INHIBITOR) * pow(THETA3, ALB) * pow(THETA4, ALP) * pow(THETA5, POP) * exp(ECL+ETA1);
double V1_ind = THETA7 * WT/75 * exp(EV1+ETA2);
double V2_ind = THETA8 * WT/75 * exp(EV2+ETA3);
double V3_ind = THETA9 * WT/75 * exp(EV3+ETA4);
double ka_ind = THETA12 * exp(ETA5+EKA);
double Q1_ind = THETA10*pow(WT/75,0.75);
double Q2_ind = THETA11*pow(WT/75,0.75);

double k10 = CL_ind / V1_ind;
double k12 = Q1_ind / V1_ind;
double k21 = Q1_ind / V2_ind;
double k23 = Q2_ind / V2_ind;
double k32 = Q2_ind / V3_ind;

F_GUT = 1 * pow(THETA14,CAPS) * exp(ETA7+EF1);
D_CENT = THETA13 * exp(ETA6+ED1); // zero-order input duration

[ODE]


dxdt_GUT  = - ka_ind *GUT;
dxdt_CENT = + ka_ind *GUT - k10 * CENT + k21 * PER - k12 * CENT;
dxdt_PER  = - k21 * PER + k12 * CENT - k23 * PER + k32 * PER2;
dxdt_PER2 = - k32 * PER2 + k23 * PER;
dxdt_AUC  = + CENT/V1_ind*1000;


[TABLE]
capture IPRED = CENT/V1_ind*1000;
double DV = IPRED*(1+PROP) + ADD;
while (DV < 0) {
  simeps();
  DV = IPRED*(1+PROP) + ADD;
}

[CAPTURE] @annotated
  DV  : Plasma concentration (ng/mL)
  IPRED : Plasma concentration (ng/mL)
  
  ETA1: MAP estimate on CL
  ETA2: MAP estimate on V1
  ETA3: MAP estimate on V2
  ETA4: MAP estimate on V3
  ETA5: MAP estimate on KA
  ETA6: MAP estimate on D1
  ETA7: MAP estimate on F1

  ECL :  ETA on CL/F
  EV1 :  ETA on V1
  EV2 :  ETA on V2
  EV3 :  ETA on V3
  EKA :  ETA on KA
  ED1 :  ETA on D1
  EF1 :  ETA on F1