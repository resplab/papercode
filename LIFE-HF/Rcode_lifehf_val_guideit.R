##### R code for validation of the LIFE-HF model in the GUIDE-IT trial
##### By Ricky Turgeon
##### Last revision: 2025-11-11

##### Required packages
library(dplyr)
library(mice)
library(mitools)
library(survival)
library(rms)
library(timeROC)
library(ggplot2)
library(riskRegression)

##### Multiple imputation (20 imputations)
# Convert categorical variables to factors
factor_vars <- c("sex", "us", "COPD", "dm", "excvd", "hosp", "nyha", 
                 "ACE", "ARB", "ARNI", "ACE_ARB_ARNI", "betablocker", "MRA", 
                 "duo", "trio", "ivabradine", "loop")
data[factor_vars] <- lapply(data[factor_vars], as.factor)

imp <- mice(data,
            m = 20,
            predictorMatrix = quickpred(data, exclude = c("id", "death", "fu_death", 
                                                          "composite", "fu_composite_1y", "fu_composite")),
            seed = 123)

##### LIFE-HF composite model: 1-year risk (adapted from code received from LIFE-HF investigators)

# Load coefficients and baseline hazards
## primep = CV death or first hospitalization for HF
## noncvdeath = Non-CV death
coef_primep <- c("sex"= 0.268156718544226,
                 "nyha"= 0.303324853835481,
                 "hosp"= 0.281875832818008,
                 "dm"= 0.381802933698494,
                 "excvd"= 0.263442832763941,
                 "ef"= 0.0215724731041233,
                 "ef.sq"= -0.000645078410541915,
                 "sbp"= -0.00709154181136319,
                 "ntbnp.log"= 0.453949374324377,
                 "gfr"= -0.0232331551799035,
                 "gfr.sq"= 0.000140788666635175)

coef_noncvdeath <- c("sex"= 0.437083919862472,
                     "nyha"= 0.017720456872499,
                     "hosp"= 0.136164986475636,
                     "dm"= 0.256824497213127,
                     "excvd"= 0.26108289650968,
                     "ef"= -0.00373052699109569,
                     "sbp"= 0.0000907392365348141,
                     "ntbnp"= 0.000216675470665736,
                     "ntbnp.sq"= -0.0000000112733837940806,
                     "gfr"= 0.00411132446484965)

base_primep <- c(0.9812845, 0.9813015, 0.9813184, 0.9813352, 0.9813520, 0.9813688, 0.9813854, 0.9814020, 0.9814185, 0.9814350, 0.9814514, 0.9814676, 0.9814839, 0.9815000, 0.9815161, 0.9815321
                 , 0.9815480, 0.9815639, 0.9815797, 0.9815955, 0.9816112, 0.9816268, 0.9816423, 0.9816578, 0.9816732, 0.9816885, 0.9817037, 0.9817188, 0.9817338, 0.9817488, 0.9817637, 0.9817785
                 , 0.9817932, 0.9818078, 0.9818224, 0.9818369, 0.9818513, 0.9818656, 0.9818798, 0.9818939, 0.9819079, 0.9819219, 0.9819357, 0.9819495, 0.9819631, 0.9819767, 0.9819902, 0.9820035
                 , 0.9820168, 0.9820300, 0.9820431, 0.9820560, 0.9820688, 0.9820815, 0.9820940, 0.9821064, 0.9821185, 0.9821305, 0.9821423, 0.9821538, 0.9821652, 0.9821763, 0.9821872, 0.9821979
                 , 0.9822083, 0.9822185, 0.9822285, 0.9822382, 0.9822478, 0.9822571, 0.9822661, 0.9822750, 0.9822835, 0.9822919, 0.9823000, 0.9823078, 0.9823153, 0.9823226, 0.9823296, 0.9823363
                 , 0.9823428, 0.9823490, 0.9823549, 0.9823605, 0.9823658, 0.9823709, 0.9823758, 0.9823805, 0.9823852, 0.9823897, 0.9823940, 0.9823982, 0.9824023, 0.9824061, 0.9824098, 0.9824133
                 , 0.9824165, 0.9824195, 0.9824223, 0.9824249, 0.9824272, 0.9824293, 0.9824310, 0.9824323, 0.9824331, 0.9824332, 0.9824326, 0.9824310, 0.9824283, 0.9824242, 0.9824187, 0.9824118
                 , 0.9824033, 0.9823891, 0.9823732, 0.9823557, 0.9823365, 0.9823155, 0.9822927, 0.9822681, 0.9822415, 0.9822128, 0.9821821, 0.9821493, 0.9821144, 0.9820773, 0.9820381, 0.9819967
                 , 0.9819531, 0.9819074, 0.9818596, 0.9818096, 0.9817574, 0.9817032, 0.9816468, 0.9815884, 0.9815279, 0.9814653, 0.9814007, 0.9813341, 0.9812654, 0.9811948, 0.9811221, 0.9810475
                 , 0.9809709, 0.9808923, 0.9808117, 0.9807291, 0.9806446, 0.9805581, 0.9804697, 0.9803792, 0.9802868, 0.9801925, 0.9800961, 0.9799978, 0.9798974, 0.9797951, 0.9796908, 0.9795845
                 , 0.9794762, 0.9793659, 0.9792536, 0.9791393, 0.9790230, 0.9789046, 0.9787843, 0.9786619, 0.9785374, 0.9784110, 0.9782825, 0.9781519, 0.9780193, 0.9778846, 0.9777479, 0.9776091
                 , 0.9774683, 0.9773253, 0.9771803, 0.9770332, 0.9768840, 0.9767327, 0.9765794, 0.9764239, 0.9762663, 0.9761065, 0.9759447, 0.9757807, 0.9756146, 0.9754463, 0.9752758, 0.9751032
                 , 0.9749283, 0.9747513, 0.9745720, 0.9743905, 0.9742068, 0.9740209, 0.9738328, 0.9736424, 0.9734498, 0.9732549, 0.9730578, 0.9728585, 0.9726569, 0.9724530, 0.9722469, 0.9720385
                 , 0.9718279, 0.9716150, 0.9713998, 0.9711823, 0.9709625, 0.9707405, 0.9705162, 0.9702896, 0.9700607, 0.9698295, 0.9695960, 0.9693602)

base_noncvdeath <- c(0.9991089, 0.9991049, 0.9991006, 0.9990962, 0.9990916, 0.9990868, 0.9990818, 0.9990766, 0.9990713, 0.9990657, 0.9990600, 0.9990542, 0.9990482, 0.9990421, 0.9990359, 0.9990295
                     , 0.9990231, 0.9990165, 0.9990098, 0.9990031, 0.9989962, 0.9989892, 0.9989822, 0.9989750, 0.9989678, 0.9989605, 0.9989531, 0.9989456, 0.9989381, 0.9989305, 0.9989229, 0.9989152
                     , 0.9989074, 0.9988996, 0.9988918, 0.9988839, 0.9988760, 0.9988680, 0.9988600, 0.9988519, 0.9988437, 0.9988355, 0.9988271, 0.9988186, 0.9988100, 0.9988013, 0.9987924, 0.9987833
                     , 0.9987740, 0.9987645, 0.9987548, 0.9987449, 0.9987347, 0.9987244, 0.9987139, 0.9987034, 0.9986932, 0.9986885, 0.9986841, 0.9986797, 0.9986756, 0.9986718, 0.9986683, 0.9986650
                     , 0.9986619, 0.9986589, 0.9986557, 0.9986522, 0.9986484, 0.9986440, 0.9986391, 0.9986338, 0.9986281, 0.9986220, 0.9986155, 0.9986085, 0.9986007, 0.9985921, 0.9985825, 0.9985720
                     , 0.9985606, 0.9985482, 0.9985350, 0.9985209, 0.9985061, 0.9984904, 0.9984738, 0.9984562, 0.9984378, 0.9984185, 0.9983985, 0.9983779, 0.9983567, 0.9983349, 0.9983125, 0.9982895
                     , 0.9982657, 0.9982413, 0.9982163, 0.9981908, 0.9981648, 0.9981385, 0.9981119, 0.9980852, 0.9980583, 0.9980315, 0.9980045, 0.9979775, 0.9979505, 0.9979236, 0.9978968, 0.9978701
                     , 0.9978435, 0.9978169, 0.9977903, 0.9977638, 0.9977374, 0.9977111, 0.9976852, 0.9976597, 0.9976345, 0.9976098, 0.9975855, 0.9975616, 0.9975382, 0.9975151, 0.9974924, 0.9974701
                     , 0.9974482, 0.9974268, 0.9974056, 0.9973847, 0.9973640, 0.9973419, 0.9973194, 0.9972964, 0.9972693, 0.9972399, 0.9972083, 0.9971742, 0.9971378, 0.9970989, 0.9970576, 0.9970138
                     , 0.9969675, 0.9969187, 0.9968675, 0.9968139, 0.9967579, 0.9966995, 0.9966388, 0.9965757, 0.9965102, 0.9964425, 0.9963724, 0.9963000, 0.9962253, 0.9961483, 0.9960690, 0.9959874
                     , 0.9959034, 0.9958172, 0.9957287, 0.9956379, 0.9955448, 0.9954493, 0.9953515, 0.9952514, 0.9951489, 0.9950441, 0.9949368, 0.9948271, 0.9947150, 0.9946005, 0.9944835, 0.9943640
                     , 0.9942420, 0.9941174, 0.9939903, 0.9938606, 0.9937284, 0.9935934, 0.9934559, 0.9933156, 0.9931727, 0.9930270, 0.9928787, 0.9927277, 0.9925739, 0.9924174, 0.9922581, 0.9920960
                     , 0.9919311, 0.9917634, 0.9915928, 0.9914193, 0.9912430, 0.9910638, 0.9908818, 0.9906968, 0.9905089, 0.9903181, 0.9901244, 0.9899278, 0.9897282, 0.9895257, 0.9893203, 0.9891119
                     , 0.9889005, 0.9886862, 0.9884689, 0.9882487, 0.9880255, 0.9877992, 0.9875700, 0.9873378, 0.9871026, 0.9868644, 0.9866232, 0.9863790)

## For model validation we are not interested in treatment effect prediction, so set treatment effects to zero (= HR of 1)
hrFutureEvent <- 1
hrFutureComp <- 1

calculate_comp_1y <- function(age, sex, nyha, hosp, dm, excvd, ef, sbp, ntbnp, gfr, region,
                              betablocker, ACE_ARB_ARNI) {
  
  ef.sq      <- ef^2
  ntbnp.sq   <- ntbnp^2
  ntbnp.log  <- log(ntbnp)
  gfr.sq     <- gfr^2
  
  survo <- 1
  riskEvent <- c()
  riskComp  <- c()
  survival  <- c()
  
  for (i in 1:220) {
    lp_primep <- as.numeric(coef_primep["sex"]) * sex +
      as.numeric(coef_primep["nyha"]) * nyha +
      as.numeric(coef_primep["hosp"]) * hosp +
      as.numeric(coef_primep["dm"]) * dm +
      as.numeric(coef_primep["excvd"]) * excvd +
      as.numeric(coef_primep["ef"]) * (ef - 30) +
      as.numeric(coef_primep["ef.sq"]) * (ef.sq - 900) +
      as.numeric(coef_primep["sbp"]) * (sbp - 130) +
      as.numeric(coef_primep["ntbnp.log"]) * (ntbnp.log - log(2500)) +
      as.numeric(coef_primep["gfr"]) * (gfr - 70) +
      as.numeric(coef_primep["gfr.sq"]) * (gfr.sq - 4900) -
      log(1.01) +
      ifelse(region == 0, log(1.12),
             ifelse(region == 1, log(0.98),
                    ifelse(region == 2, log(1.03),
                           ifelse(region == 3, log(0.96),
                                  ifelse(region == 4, log(0.93), 0))))) +
      ifelse(betablocker == 0, log(1 / 0.77), 0) + #adjustment for baseline beta-blocker non-use
      ifelse(ACE_ARB_ARNI == 0, log(1 / 0.74), 0) #adjustment for baseline acei/arb/arni non-use
    
    lp_noncvdeath_comp <- as.numeric(coef_noncvdeath["sex"]) * sex +
      as.numeric(coef_noncvdeath["nyha"]) * nyha +
      as.numeric(coef_noncvdeath["hosp"]) * hosp +
      as.numeric(coef_noncvdeath["dm"]) * dm +
      as.numeric(coef_noncvdeath["excvd"]) * excvd +
      as.numeric(coef_noncvdeath["ef"]) * (ef - 30) +
      as.numeric(coef_noncvdeath["sbp"]) * (sbp - 130) +
      as.numeric(coef_noncvdeath["ntbnp"]) * (ntbnp - 2500) +
      as.numeric(coef_noncvdeath["ntbnp.sq"]) * (ntbnp.sq - 6250000) +
      as.numeric(coef_noncvdeath["gfr"]) * (gfr - 70) -
      log(1.18) -
      log(0.71)
    
    surv1 <- base_primep[age - 40 + i + 3 * (age - 40)]^(exp(lp_primep))
    surv2 <- base_noncvdeath[age - 40 + i + 3 * (age - 40)]^(exp(lp_noncvdeath_comp))
    
    event <- (1 - surv1) / ((1 - surv1) + (1 - surv2)) * (survo - survo * (1 - (1 - surv1) - (1 - surv2)))
    comp  <- (1 - surv2) / ((1 - surv1) + (1 - surv2)) * (survo - survo * (1 - (1 - surv1) - (1 - surv2)))
    
    riskEvent <- c(riskEvent, max(event, 0))
    riskComp  <- c(riskComp,  max(comp, 0))
    
    survo <- survo * (1 - (1 - surv1) - (1 - surv2))
    survival <- c(survival, max(survo, 0))
  }
  
  survival <- c(1, survival)  # prepend baseline survival at time 0
  time_index <- (age * 4):380  # matches original survival frame
  survival <- cbind(time_index / 4, survival)[1:length(time_index), ]  # columns: time (years), survival
  
  return(list(
    survival = survival,  # full vector over time
    lp_primep = lp_primep,
    lp_noncvdeath_comp = lp_noncvdeath_comp,
    survival1yr_composite = round(survival[5, 2], 4),  # survival at 1 year (quarter 4)
    currentOneYearRisk_composite = round(sum(riskEvent[1:4], na.rm = TRUE), 4),
    currentOneYearRisk_compositeComp = round(sum(riskComp[1:4], na.rm = TRUE), 4)
  ))
}

##### LIFE-HF mortality model: 1-year risk (adapted from code received from LIFE-HF investigators)

# Load coefficients and baseline hazards
coef_cvdeath <- c("sex"= 0.312324762096136,
                  "nyha"= 0.345985965213897,
                  "hosp"= 0.122764541970514,
                  "dm"= 0.231590535237815,
                  "excvd"= 0.251726047634299,
                  "ef"= -0.0127391423138566,
                  "sbp"= -0.00854243424924433,
                  "ntbnp.log"= 0.489694705815132,
                  "gfr"= -0.0252751482141098,
                  "gfr.sq"= 0.000164796948426936)

base_cvdeath <- c(0.9893391, 0.9893087, 0.9892796, 0.9892518, 0.9892254, 0.9892004, 0.9891768, 0.9891547, 0.9891339, 0.9891144, 0.9890963, 0.9890794, 0.9890638, 0.9890495, 0.9890364, 0.9890246
                  , 0.9890139, 0.9890044, 0.9889960, 0.9889886, 0.9889821, 0.9889765, 0.9889715, 0.9889673, 0.9889636, 0.9889605, 0.9889577, 0.9889552, 0.9889530, 0.9889511, 0.9889494, 0.9889480
                  , 0.9889470, 0.9889465, 0.9889466, 0.9889476, 0.9889496, 0.9889532, 0.9889587, 0.9889669, 0.9889785, 0.9889946, 0.9890160, 0.9890428, 0.9890749, 0.9891063, 0.9891359, 0.9891618
                  , 0.9891834, 0.9892013, 0.9892165, 0.9892300, 0.9892423, 0.9892537, 0.9892635, 0.9892712, 0.9892767, 0.9892807, 0.9892843, 0.9892881, 0.9892925, 0.9892974, 0.9893031, 0.9893103
                  , 0.9893195, 0.9893306, 0.9893430, 0.9893558, 0.9893683, 0.9893797, 0.9893899, 0.9893992, 0.9894085, 0.9894184, 0.9894291, 0.9894404, 0.9894523, 0.9894650, 0.9894792, 0.9894955
                  , 0.9895145, 0.9895363, 0.9895608, 0.9895873, 0.9896152, 0.9896436, 0.9896724, 0.9897011, 0.9897296, 0.9897576, 0.9897850, 0.9898108, 0.9898342, 0.9898546, 0.9898714, 0.9898842
                  , 0.9898927, 0.9898975, 0.9898989, 0.9898972, 0.9898930, 0.9898864, 0.9898777, 0.9898666, 0.9898531, 0.9898371, 0.9898182, 0.9897962, 0.9897706, 0.9897415, 0.9897087, 0.9896721
                  , 0.9896318, 0.9895879, 0.9895403, 0.9894896, 0.9894365, 0.9893824, 0.9893280, 0.9892736, 0.9892189, 0.9891632, 0.9891064, 0.9890481, 0.9889880, 0.9889257, 0.9888617, 0.9887961
                  , 0.9887295, 0.9886624, 0.9885952, 0.9885287, 0.9884636, 0.9884008, 0.9883409, 0.9882836, 0.9882289, 0.9881766, 0.9881272, 0.9880811, 0.9880390, 0.9880014, 0.9879680, 0.9879378
                  , 0.9879093, 0.9878811, 0.9878519, 0.9878210, 0.9877871, 0.9877488, 0.9877040, 0.9876516, 0.9875915, 0.9875098, 0.9874184, 0.9873169, 0.9872054, 0.9870844, 0.9869540, 0.9868147
                  , 0.9866668, 0.9865105, 0.9863461, 0.9861737, 0.9859935, 0.9858053, 0.9856093, 0.9854055, 0.9851938, 0.9849743, 0.9847467, 0.9845112, 0.9842677, 0.9840162, 0.9837565, 0.9834888
                  , 0.9832128, 0.9829285, 0.9826359, 0.9823348, 0.9820251, 0.9817067, 0.9813796, 0.9810436, 0.9806987, 0.9803449, 0.9799823, 0.9796107, 0.9792300, 0.9788403, 0.9784415, 0.9780335
                  , 0.9776164, 0.9771901, 0.9767547, 0.9763101, 0.9758563, 0.9753935, 0.9749214, 0.9744403, 0.9739500, 0.9734505, 0.9729419, 0.9724241, 0.9718972, 0.9713611, 0.9708158, 0.9702614
                  , 0.9696978, 0.9691251, 0.9685432, 0.9679521, 0.9673519, 0.9667425, 0.9661240, 0.9654963, 0.9648595, 0.9642135, 0.9635584, 0.9628941)

calculate_3 <- function(age, sex, nyha, hosp, dm, excvd, ef, sbp, ntbnp, gfr, region, ageFuture = age, betablocker, ACE_ARB_ARNI) {
  
  # Precomputed terms
  ntbnp.log <- log(ntbnp)
  ntbnp.sq <- ntbnp^2
  gfr.sq <- gfr^2
  
  # Region recalibration factors
  region_adj <- c("0" = log(1.42), "1" = log(0.92), "2" = log(1.42), "3" = log(0.80), "4" = log(0.85))
  reg_factor <- ifelse(as.character(region) %in% names(region_adj), region_adj[as.character(region)], 0)
  
  survo <- 1
  riskEvent <- riskComp <- survival <- numeric(220)
  
  for (i in 1:220) {
    # Linear predictors
    lp_cvdeath <- sum(
      coef_cvdeath["sex"] * sex,
      coef_cvdeath["nyha"] * nyha,
      coef_cvdeath["hosp"] * hosp,
      coef_cvdeath["dm"] * dm,
      coef_cvdeath["excvd"] * excvd,
      coef_cvdeath["ef"] * (ef - 30),
      coef_cvdeath["sbp"] * (sbp - 130),
      coef_cvdeath["ntbnp.log"] * (ntbnp.log - log(2500)),
      coef_cvdeath["gfr"] * (gfr - 70),
      coef_cvdeath["gfr.sq"] * (gfr.sq - 4900)
    ) - log(1.09) - reg_factor - log(1.28) +
      ifelse(betablocker == 0, log(1 / 0.76), 0) + #adjustment for baseline beta-blocker non-use
      ifelse(ACE_ARB_ARNI == 0, log(1 / 0.84), 0) #adjustment for baseline acei/arb/arni non-use
    
    lp_noncvdeath <- sum(
      coef_noncvdeath["sex"] * sex,
      coef_noncvdeath["nyha"] * nyha,
      coef_noncvdeath["hosp"] * hosp,
      coef_noncvdeath["dm"] * dm,
      coef_noncvdeath["excvd"] * excvd,
      coef_noncvdeath["ef"] * (ef - 30),
      coef_noncvdeath["sbp"] * (sbp - 130),
      coef_noncvdeath["ntbnp"] * (ntbnp - 2500),
      coef_noncvdeath["ntbnp.sq"] * (ntbnp.sq - 6250000),
      coef_noncvdeath["gfr"] * (gfr - 70)
    ) - log(1.09) - log(0.56)
    
    # Survival estimates
    idx <- age - 40 + i + 3 * (age - 40)
    surv1 <- base_cvdeath[idx]^(exp(lp_cvdeath))
    surv2 <- base_noncvdeath[idx]^(exp(lp_noncvdeath))
    
    # Risk decomposition
    event <- (1 - surv1) / ((1 - surv1) + (1 - surv2)) * (survo * ((1 - surv1) + (1 - surv2)))
    comp <- (1 - surv2) / ((1 - surv1) + (1 - surv2)) * (survo * ((1 - surv1) + (1 - surv2)))
    
    riskEvent[i] <- max(0, event)
    riskComp[i]  <- max(0, comp)
    
    survo <- survo * surv1 * surv2
    survival[i] <- max(0, survo)
  }
  
  survival <- c(1, survival)
  # 1-year = 4 time steps; for 2-year validation use 4 * 2 = 8 + 1 (index 5)
  return(list(
    survival = survival,
    lp_cvdeath = lp_cvdeath,
    survival1yr_death = round(survival[5], 4),
    currentOneYearRisk_death = round(sum(riskEvent[1:4], na.rm = TRUE), 4),
    currentOneYearRiskComp_death = round(sum(riskComp[1:4], na.rm = TRUE), 4)
  ))
}

##### Discrimination: Time-dependent AUROC (for composite outcome model)
# For one imputation/dataset
roc <- timeROC(
      T = df$fu_composite,
      delta = df$composite,
      marker = 1 - df$survival1yr_composite,
      cause = 1,
      times = 1,
      iid = TRUE)
    
##### Calibration (for composite outcome model)
# Adapted from code from https://pubmed.ncbi.nlm.nih.gov/36571841/
# For multiple-imputed datasets
logit <- function(p) log(p / (1 - p))

# O/E, CITL, Cox-based calibration slope
OE_list_comp <- lapply(imputed_list, function(df) {
#Observed 1-year composite risk (KM)
  km_fit     <- survfit(Surv(fu_composite_1y, composite) ~ 1, data = df)
  km_summary <- summary(km_fit, times = 1)
  
  S1    <- km_summary$surv[1]
  se_S1 <- km_summary$std.err[1]
  obs   <- 1 - S1
  var_obs <- se_S1^2
  
#Expected 1-year composite risk from model
  pred_surv_1y <- df$survival1yr_composite
  exp          <- 1 - mean(pred_surv_1y, na.rm = TRUE)
  
#O/E and variance
  OE <- obs / exp
  n <- nrow(df)
  var_exp <- var(1 - pred_surv_1y, na.rm = TRUE) / n
  var_OE <- (obs / exp)^2 * (var_obs / obs^2 + var_exp / exp^2)
  
#CITL
  citl <- logit(obs) - logit(exp)
  d_logit  <- 1 / (obs * (1 - obs))
  var_citl <- (d_logit^2) * var_obs
  
#Slope
  pred_risk_1y <- 1 - pred_surv_1y
  lp_val       <- log(-log(1 - pred_risk_1y))  # cloglog transform
  keep <- is.finite(lp_val) & !is.na(lp_val)
  df_cal   <- df[keep, ]
  lp_val_k <- lp_val[keep]
  center      <- mean(lp_val_k)
  lp_centered <- lp_val_k - center
  df_cal$lp_centered <- lp_centered
  fit_cal <- coxph(Surv(fu_composite_1y, composite) ~ lp_centered, data = df_cal)
  slope     <- coef(fit_cal)[1]
  var_slope <- vcov(fit_cal)[1, 1]
  
  list(
    OE            = OE,
    var_OE        = var_OE,
    obs           = obs,
    exp           = exp,
    citl          = citl,
    var_citl      = var_citl,
    slope         = slope,
    var_slope     = var_slope,
  )
})

# Extract results/variances into MIcombine-friendly structures
## O/E
OE_values_comp <- lapply(OE_list_comp, function(x) c(OE = x$OE))
OE_vars_comp   <- lapply(OE_list_comp, function(x)
  matrix(x$var_OE, nrow = 1, ncol = 1, dimnames = list("OE", "OE"))
)
## CITL
citl_values_comp <- lapply(OE_list_comp, function(x) c(CITL = x$citl))
citl_vars_comp   <- lapply(OE_list_comp, function(x)
  matrix(x$var_citl, nrow = 1, ncol = 1, dimnames = list("CITL", "CITL"))
)

## Calibration slope
slope_values_comp <- lapply(OE_list_comp, function(x) c(slope = x$slope))
slope_vars_comp   <- lapply(OE_list_comp, function(x)
  matrix(x$var_slope, nrow = 1, ncol = 1, dimnames = list("slope", "slope"))
)

# Pool with Rubin's rules
OE_MI_comp    <- MIcombine(results = OE_values_comp,   variances = OE_vars_comp)
summary(OE_MI_comp)

citl_MI_comp  <- MIcombine(results = citl_values_comp, variances = citl_vars_comp)
summary(citl_MI_comp)

slope_MI_comp <- MIcombine(results = slope_values_comp, variances = slope_vars_comp)
summary(slope_MI_comp)

# Moderate calibration (flexible calibration curve)
year.risk <- 1

# 1: Compute calibration predictions for each imputed dataset
cal_list <- lapply(imputed_list, function(df) {
  df$survival1yr_composite.cll <- log(-log(1 - df$survival1yr_composite))
  
  fit <- cph(Surv(fu_composite_1y, composite) ~ rcs(survival1yr_composite.cll, 3),
             x = TRUE, y = TRUE, surv = TRUE, data = df)
  
  pred_surv <- survest(fit, times = year.risk, newdata = df)
  
  data.frame(
    id    = seq_len(nrow(df)),
    pred  = 1 - df$survival1yr_composite,
    obs   = 1 - pred_surv$surv,
    lower = 1 - pred_surv$upper,
    upper = 1 - pred_surv$lower
  )
})

# 2: Pool predictions across imputations by patient ID
dat_cal_all <- bind_rows(cal_list, .id = "imp")

dat_cal_avg <- dat_cal_all %>%
  group_by(id, pred) %>%
  summarise(
    obs = mean(obs, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )

# 3: Trim to 1st–99th percentile of predicted risk
range_min <- quantile(dat_cal_avg$pred, 0.01, na.rm = TRUE)
range_max <- quantile(dat_cal_avg$pred, 0.99, na.rm = TRUE)

dat_cal_trimmed <- dat_cal_avg %>%
  filter(pred >= range_min, pred <= range_max)

# 4: Loess smoothing
loess_obs   <- loess(obs ~ pred, data = dat_cal_trimmed, span = 0.5)
loess_lower <- loess(lower ~ pred, data = dat_cal_trimmed, span = 0.5)
loess_upper <- loess(upper ~ pred, data = dat_cal_trimmed, span = 0.5)

smoothed_df <- data.frame(
  pred  = dat_cal_trimmed$pred,
  obs   = predict(loess_obs),
  lower = predict(loess_lower),
  upper = predict(loess_upper)
)

# 5: Inverted top-tertile density (height doubled)
stacked_data <- bind_rows(lapply(imputed_list, function(df) {
  df$pred_event <- 1 - df$survival1yr_composite
  df
}))

top_risk_data <- stacked_data %>%
  arrange(desc(pred_event)) %>%
  slice(1:floor(nrow(.) / 3))

density_data <- density(top_risk_data$pred_event, from = 0, to = 1)
density_df <- data.frame(
  x = density_data$x,
  ymin = 1.0 - (density_data$y / max(density_data$y) * 0.16),  # double height
  ymax = 1.0
)

# 6: Final plot
ggplot(smoothed_df, aes(x = pred, y = obs)) +
  geom_ribbon(
    data = density_df,
    aes(x = x, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "gray70", alpha = 0.5
  ) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  coord_fixed() +
  labs(
    x = "Predicted 1-year composite event risk",
    y = "Observed 1-year risk (KM estimate)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# ICI
# Calculate absolute differences per imputation
absdiff_list <- lapply(cal_list, function(df) {
  abs(df$pred - df$obs)
})

# Compute numerical summaries within each imputation
num_summaries <- lapply(absdiff_list, function(diffs) {
  c(
    ICI = mean(diffs),
    E50 = quantile(diffs, 0.5),
    E90 = quantile(diffs, 0.9),
    Emax = max(diffs)
  )
})

# Combine and average across imputations
numsum_cph <- Reduce("+", num_summaries) / length(num_summaries)

# View result
round(numsum_cph, 4)

##### Overall performance
brier_vals <- ipa_vals <- numeric()

for (i in seq_along(imputed_ready$imputations)) {
  df <- imputed_ready$imputations[[i]]
  preds <- 1 - df$survival1yr_composite
  
  sc <- riskRegression::Score(
    list(Model = preds),
    Surv(fu_composite_1y, composite) ~ 1,
    data = df,
    times = year.risk,
    cens.model = "km",
    metrics = "brier",
    summary = "ipa",
    conf.int = FALSE
  )
  
  bm <- sc$Brier$score$Brier[sc$Brier$score$model == "Model" & sc$Brier$score$times == year.risk]
  ipa <- sc$Brier$score$IPA[2]
  
  brier_vals[i] <- bm
  ipa_vals[i]   <- ipa
}
summary(sc)
c(brier_mean = mean(brier_vals),
  IPA_mean   = mean(ipa_vals))

##### Decision curve analysis
# Set thresholds and time horizon
thresholds <- seq(0, 1.0, by = 0.01)
year.risk <- 1

# Compute net benefit for one dataset
compute_nb <- function(data, thresholds, year.risk = 1) {
  survfit_all <- summary(survfit(Surv(fu_composite_1y, composite) ~ 1, data = data), times = year.risk)
  f_all <- 1 - survfit_all$surv
  
  results <- lapply(thresholds, function(ps) {
    NB_all <- f_all - (1 - f_all) * (ps / (1 - ps))
    
    p_exceed <- mean((1 - data$survival1yr_composite) > ps)
    subset_data <- data[(1 - data$survival1yr_composite) > ps, ]
    
    survfit_subset <- try(summary(survfit(Surv(fu_composite_1y, composite) ~ 1, data = subset_data), times = year.risk), silent = TRUE)
    
    if (inherits(survfit_subset, "try-error") || length(survfit_subset$surv) == 0) {
      NB <- 0
    } else {
      f_given_exceed <- 1 - survfit_subset$surv
      TP <- f_given_exceed * p_exceed
      FP <- (1 - f_given_exceed) * p_exceed
      NB <- TP - FP * (ps / (1 - ps))
    }
    
    data.frame(threshold = ps, NB = NB, treat_all = NB_all)
  })
  
  do.call(rbind, results)
}

# Apply the function to all imputed datasets
nb_list <- lapply(imputed_list, compute_nb, thresholds = thresholds, year.risk = year.risk)

# Combine and pool net benefit across imputations
df_nb <- bind_rows(nb_list, .id = "imp")
pooled_nb <- df_nb %>%
  group_by(threshold) %>%
  summarise(
    NB = mean(NB, na.rm = TRUE),
    treat_all = mean(treat_all, na.rm = TRUE),
    .groups = "drop"
  )

# Smooth model net benefit
smooth_nb <- smooth(pooled_nb$NB, twiceit = TRUE)

# Plot decision curve
par(xaxs = "i", yaxs = "i", las = 1)
plot(pooled_nb$threshold,
     smooth_nb,
     type = "l", 
     lwd = 3, 
     lty = 1,
     xlab = "Threshold probability", 
     ylab = "Net Benefit",
     xlim = c(0, 0.5), 
     ylim = c(-0.10, 0.40), 
     bty = "n",
     cex.lab = 1.2, 
     cex.axis = 1,
     col = 2
)
lines(pooled_nb$threshold, 
      pooled_nb$treat_all, 
      type = "l", 
      lwd = 3, 
      lty = 2,
      col = 4)
abline(h = 0, lwd = 3, lty = 4, col = 8)
legend("topright",
       c("Treat All", "LIFE-HF model", "Treat None"),
       lty = c(2, 1, 4), lwd = 3, 
       col = c(4, 2, 8),
       bty = "n"
)