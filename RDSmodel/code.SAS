
* Author: Abdollah Safari  <a.saffari72@gmail.com>;


ODS OUTPUT CorrMatParmEst=paramCorr CovMatParmEst=paramCov AdditionalEstimates=addParamEst ParameterEstimates=paramEst;
TITLE "ZERO-IFLATED RECURRENT EPISODE MODEL -- LOG-NORMAL";
PROC NLMIXED DATA = ExacerData METHOD=ISAMP MAXITER=2000 MAXFUNC=40000 NOAD QTOL=1.5 CORR COV;

   * Parameters starting values;
   PARMS
    gamma1=1.2249	gamma2=0.6204

	v11=0.598	v22=0.2951	v33=1.9435	v21=0.1715	v31=0.4986	v32=0.3498

	b0=4.5906	b_UTRTCD2=0.4717	b_UTRTCD3=0.3194	b_UTRTCD4=0.7792	b_UTRTCD5=0.7018	b_AGE10=0.09031	b_WOMAN=0.07627	b_FEV1PPBL100=-0.2091
	b_POLYPBL=0.1399	b_SACQBL=-0.1084	b_BMI10=0.06482	b_FEVREVPBL100=0.2425	b_BLEOSBL=-0.2572	b_SMOKCD=0.1181	b_DURYR10=-0.0212
	b_OCSDBL10=-0.04029	b_ETHNICCD=-0.2691	b_IgEBL1000=0.04344	b_PREEXAC10=-0.7584	b_PERIOD_S=-0.3092	b_PERIOD=0.09666

	c0=-4.2513	c_UTRTCD2=0.2193	c_UTRTCD3=0.5055	c_UTRTCD4=0.1307	c_UTRTCD5=0.2604	c_AGE10=-0.23	c_WOMAN=0.1808	c_FEV1PPBL100=-0.1169
	c_POLYPBL=-0.06518	c_SACQBL=-0.02694	c_BMI10=0.6637	c_FEVREVPBL100=-0.06106	c_BLEOSBL=0.699	c_SMOKCD=0.2674	c_DURYR10=0.08008	c_OCSDBL10=0.2793
	c_ETHNICCD=1.5954	c_IgEBL1000=0.0166	c_PREEXAC10=0.5033	c_PERIOD=0.1782

	p0=1.0697	p_UTRTCD2=0.7457	p_UTRTCD3=0.4612	p_UTRTCD4=0.962	p_UTRTCD5=0.3539	p_AGE10=-0.00187	p_WOMAN=-0.9856	p_FEV1PPBL100=1.8275
	p_POLYPBL=0.2417	p_SACQBL=-0.35	p_BMI10=-0.4626	p_FEVREVPBL100=-1.3324	p_BLEOSBL=0.4014	p_SMOKCD=-0.7578	p_DURYR10=0.003709	p_OCSDBL10=-1.6996
	p_ETHNICCD=0.2625	p_IgEBL1000=-0.4459	p_PREEXAC10=-1.1235

	d0=2.0394	d_UTRTCD2=-0.0331	d_UTRTCD3=-0.05386	d_UTRTCD4=-0.06103	d_UTRTCD5=-0.164	d_AGE10=0.02378	d_WOMAN=0.04454	d_FEV1PPBL100=0.1008
	d_POLYPBL=0.001237	d_SACQBL=0.007217	d_BMI10=0.1351	d_FEVREVPBL100=-0.1065	d_BLEOSBL=0.08123	d_SMOKCD=-0.04593	d_DURYR10=0.03024
	d_OCSDBL10=0.07842	d_ETHNICCD=-0.1621	d_IgEBL1000=-0.0416	d_PREEXAC10=-0.3081	d_PERIOD_S=-0.00993	d_PERIOD=0.03941;


   BOUND v11>0, v22>0, v33>0, gamma1>0, gamma2>0;

   
   * Rate component;
   HR_LIN_TEMP = b0 + b_UTRTCD4 * UTRTCD4 + b_UTRTCD3 * UTRTCD3 + b_UTRTCD2 * UTRTCD2 + b_UTRTCD5 * UTRTCD5 + b_AGE10 * AGE10 + b_WOMAN * WOMAN + b_FEV1PPBL100 * FEV1PPBL100 +
                b_POLYPBL * POLYPBL + b_SACQBL * SACQBL + b_BMI10 * BMI10 + b_FEVREVPBL100 * FEVREVPBL100 + b_BLEOSBL * BLEOSBL + b_SMOKCD * SMOKCD +
                b_DURYR10 * DURYR10 + b_OCSDBL10 * OCSDBL10 + b_ETHNICCD * ETHNICCD + b_IgEBL1000 * IgEBL1000 +
                b_PREEXAC10 * PREEXAC10 + b_PERIOD_S * PRE_PERIOD_S + b_PERIOD * PRE_PERIOD;
   HR_LIN = HR_LIN_TEMP + Z1;
   
   * Duration component;
   SR_LIN_TEMP = d0 + d_UTRTCD4 * UTRTCD4 + d_UTRTCD3 * UTRTCD3 + d_UTRTCD2 * UTRTCD2 + d_UTRTCD5 * UTRTCD5 + d_AGE10 * AGE10 + d_WOMAN * WOMAN + d_FEV1PPBL100 * FEV1PPBL100 +
                 d_POLYPBL * POLYPBL + d_SACQBL * SACQBL + d_BMI10 * BMI10 + d_FEVREVPBL100 * FEVREVPBL100 + d_BLEOSBL * BLEOSBL + d_SMOKCD * SMOKCD +
                 d_DURYR10 * DURYR10 + d_OCSDBL10 * OCSDBL10 + d_ETHNICCD * ETHNICCD + d_IgEBL1000 * IgEBL1000 +
                 d_PREEXAC10 * PREEXAC10 + d_PERIOD_S * PRE_PERIOD_S + d_PERIOD * PRE_PERIOD;
   SR_LIN = SR_LIN_TEMP + Z2;

   * SEVERITY COMPONENT;
   C_LIN_TEMP = c0 + c_UTRTCD4 * UTRTCD4 + c_UTRTCD3 * UTRTCD3 + c_UTRTCD2 * UTRTCD2 + c_UTRTCD5 * UTRTCD5 + c_AGE10 * AGE10 + c_WOMAN * WOMAN + c_FEV1PPBL100 * FEV1PPBL100 +
                c_POLYPBL * POLYPBL + c_SACQBL * SACQBL + c_BMI10 * BMI10 + c_FEVREVPBL100 * FEVREVPBL100 + c_BLEOSBL * BLEOSBL + c_SMOKCD * SMOKCD +
                c_DURYR10 * DURYR10 + c_OCSDBL10 * OCSDBL10 + c_ETHNICCD * ETHNICCD + c_IgEBL1000 * IgEBL1000 +
                c_PREEXAC10 * PREEXAC10 + c_PERIOD * PRE_PERIOD;
   C_LIN = C_LIN_TEMP + Z3;

   * ZI component;
   P_LIN_TEMP = p0 + p_UTRTCD4 * UTRTCD4 + p_UTRTCD3 * UTRTCD3 + p_UTRTCD2 * UTRTCD2 + p_UTRTCD5 * UTRTCD5 + p_AGE10 * AGE10 + p_WOMAN * WOMAN + p_FEV1PPBL100 * FEV1PPBL100 +
                p_POLYPBL * POLYPBL + p_SACQBL * SACQBL + p_BMI10 * BMI10 + p_FEVREVPBL100 * FEVREVPBL100 + p_BLEOSBL * BLEOSBL + p_SMOKCD * SMOKCD +
                p_DURYR10 * DURYR10 + p_OCSDBL10 * OCSDBL10 + p_ETHNICCD * ETHNICCD + p_IgEBL1000 * IgEBL1000 +
                p_PREEXAC10 * PREEXAC10;
   P_LIN = P_LIN_TEMP;
   
   
   EXP_HR_LIN = HR_LIN;
   EXP_SR_LIN = SR_LIN;
   P_1 = EXP(C_LIN)/(1 + EXP(C_LIN));
   P_0 = EXP(P_LIN)/(1 + EXP(P_LIN));
   
   
   
   scale_cons=365.25;
   
   IF (MAXPERIOD=1) THEN DO;       *NO EXACERBATIONS;
      S_t_1 = 1 - CDF('NORMAL', (LOG(STOP/scale_cons) - EXP_HR_LIN)/gamma1);
      L = P_0 + (1 - P_0) * S_t_1;
   END;
   ELSE DO;       *WITH EXACERBATIONS;
      
      IF (PERIOD=1) THEN DO;       *FIRST EXACERBATIONS;
         IF (STOP=START) THEN DO;       *START WITH AN EXACERBATION;
            S_t_1 = 1;
            S_t_2 = 1 - CDF('NORMAL', (LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2);
            
            h_t_1 = 1;
            h_t_2 = EXP(-0.5*((LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2)**2)/SQRT(2*CONSTANT('pi'))/gamma2/(SDUR/scale_cons)/S_t_2;
         END;
         ELSE DO;       *START WITH NO EXACERBATIONS;
            S_t_1 = 1 - CDF('NORMAL', (LOG(HDUR/scale_cons) - EXP_HR_LIN)/gamma1);
            S_t_2 = 1 - CDF('NORMAL', (LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2);
            
            h_t_1 = EXP(-0.5*((LOG(HDUR/scale_cons) - EXP_HR_LIN)/gamma1)**2)/SQRT(2*CONSTANT('pi'))/gamma1/(HDUR/scale_cons)/S_t_1;
            h_t_2 = EXP(-0.5*((LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2)**2)/SQRT(2*CONSTANT('pi'))/gamma2/(SDUR/scale_cons)/S_t_2;
         END;
      END;
      ELSE DO;       *AFTER THE FIRST EXACERBATION;
         IF (PERIOD=MAXPERIOD) THEN DO;       *LAST ROW OF A PATIENT;
            IF ((SDUR>0) AND (HDUR=.)) THEN DO;       *FINISHING WITH AN EXACERBATION;
               S_t_1 = 1;
               S_t_2 = 1 - CDF('NORMAL', (LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2);
               
               h_t_1 = 1;
               h_t_2 = EXP(-0.5*((LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2)**2)/SQRT(2*CONSTANT('pi'))/gamma2/(SDUR/scale_cons)/S_t_2;
            END;
            ELSE DO;       *FINISHING WITH NO EXACERBATIONS;
               IF (HDUR=.) THEN DO;
                  S_t_1 = 1;
                  S_t_2 = 1;
                  
                  h_t_1 = 1;
                  h_t_2 = 1;
               END;
               ELSE DO;
                  S_t_1 = 1 - CDF('NORMAL', (LOG(HDUR/scale_cons) - EXP_HR_LIN)/gamma1);
                  S_t_2 = 1;
                  
                  h_t_1 = 1;
                  h_t_2 = 1;
               END;
            END;
         END;
         ELSE DO;
            S_t_1 = 1 - CDF('NORMAL', (LOG(HDUR/scale_cons) - EXP_HR_LIN)/gamma1);
            S_t_2 = 1 - CDF('NORMAL', (LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2);
            
            h_t_1 = EXP(-0.5*((LOG(HDUR/scale_cons) - EXP_HR_LIN)/gamma1)**2)/SQRT(2*CONSTANT('pi'))/gamma1/(HDUR/scale_cons)/S_t_1;
            h_t_2 = EXP(-0.5*((LOG(SDUR/scale_cons) - EXP_SR_LIN)/gamma2)**2)/SQRT(2*CONSTANT('pi'))/gamma2/(SDUR/scale_cons)/S_t_2;
         END;
      END;
      
      
      L = h_t_1 * h_t_2 * S_t_1 * S_t_2;
      IF L~=1 THEN DO;
         IF (EXB_SEV>0) THEN DO;
            IF (EXB_SEV=1) THEN L = L * (1 - P_1);
            ELSE L = L * P_1;
         END;
         IF (PERIOD=1) THEN L = L * (1 - P_0);
      END;
      
   END;
   
   
   * Joint log-likelihood;
   ll = LOG(L);
   model EXB_SEV ~ general(ll);
   RANDOM Z1 Z2 Z3 ~ NORMAL([0 , 0 , 0] , [v11 , v21 , v22 , v31 , v32 , v33]) SUBJECT=USUBJID OUT=WORK._RE_EST;
   

   * ESTIMATED AFT & OR;
   ESTIMATE 'RATE: AFT TRT 75mg' exp(b_UTRTCD2);
   ESTIMATE 'RATE: AFT TRT 100mg' exp(b_UTRTCD3);
   ESTIMATE 'RATE: AFT TRT 250mg' exp(b_UTRTCD4);
   ESTIMATE 'RATE: AFT TRT 750mg' exp(b_UTRTCD5);
   ESTIMATE 'RATE: AFT age' exp(b_AGE10);
   ESTIMATE 'RATE: AFT female' exp(b_WOMAN);
   ESTIMATE 'RATE: AFT FEV1' exp(b_FEV1PPBL100);
   ESTIMATE 'RATE: AFT polyp' exp(b_POLYPBL);
   ESTIMATE 'RATE: AFT SACQ' exp(b_SACQBL);
   ESTIMATE 'RATE: AFT BMI' exp(b_BMI10);
   ESTIMATE 'RATE: AFT FEV rev' exp(b_FEVREVPBL100);
   ESTIMATE 'RATE: AFT Eos blood' exp(b_BLEOSBL);
   ESTIMATE 'RATE: AFT smoking' exp(b_SMOKCD);
   ESTIMATE 'RATE: AFT asthma dur' exp(b_DURYR10);
   ESTIMATE 'RATE: AFT OCS' exp(b_OCSDBL10);
   ESTIMATE 'RATE: AFT ethnicity' exp(b_ETHNICCD);
   ESTIMATE 'RATE: AFT IgE' exp(b_IgEBL1000);
   ESTIMATE 'RATE: AFT pre exacer' exp(b_PREEXAC10);
   ESTIMATE 'RATE: AFT Period' exp(b_PERIOD);
   ESTIMATE 'RATE: AFT Period_S' exp(b_PERIOD_S);
   
   ESTIMATE 'DUR: AFT TRT 75mg' exp(d_UTRTCD2);
   ESTIMATE 'DUR: AFT TRT 100mg' exp(d_UTRTCD3);
   ESTIMATE 'DUR: AFT TRT 250mg' exp(d_UTRTCD4);
   ESTIMATE 'DUR: AFT TRT 750mg' exp(d_UTRTCD5);
   ESTIMATE 'DUR: AFT age' exp(d_AGE10);
   ESTIMATE 'DUR: AFT female' exp(d_WOMAN);
   ESTIMATE 'DUR: AFT FEV1' exp(d_FEV1PPBL100);
   ESTIMATE 'DUR: AFT polyp' exp(d_POLYPBL);
   ESTIMATE 'DUR: AFT SACQ' exp(d_SACQBL);
   ESTIMATE 'DUR: AFT BMI' exp(d_BMI10);
   ESTIMATE 'DUR: AFT FEV rev' exp(d_FEVREVPBL100);
   ESTIMATE 'DUR: AFT Eos blood' exp(d_BLEOSBL);
   ESTIMATE 'DUR: AFT smoking' exp(d_SMOKCD);
   ESTIMATE 'DUR: AFT asthma dur' exp(d_DURYR10);
   ESTIMATE 'DUR: AFT OCS' exp(d_OCSDBL10);
   ESTIMATE 'DUR: AFT ethnicity' exp(d_ETHNICCD);
   ESTIMATE 'DUR: AFT IgE' exp(d_IgEBL1000);
   ESTIMATE 'DUR: AFT pre exacer' exp(d_PREEXAC10);
   ESTIMATE 'DUR: AFT Period' exp(d_PERIOD);
   ESTIMATE 'DUR: AFT Period_S' exp(d_PERIOD_S);

   ESTIMATE 'SEV: OR TRT 75mg' exp(c_UTRTCD2);
   ESTIMATE 'SEV: OR TRT 100mg' exp(c_UTRTCD3);
   ESTIMATE 'SEV: OR TRT 250mg' exp(c_UTRTCD4);
   ESTIMATE 'SEV: OR TRT 750mg' exp(c_UTRTCD5);
   ESTIMATE 'SEV: OR age' exp(c_AGE10);
   ESTIMATE 'SEV: OR female' exp(c_WOMAN);
   ESTIMATE 'SEV: OR FEV1' exp(c_FEV1PPBL100);
   ESTIMATE 'SEV: OR polyp' exp(c_POLYPBL);
   ESTIMATE 'SEV: OR SACQ' exp(c_SACQBL);
   ESTIMATE 'SEV: OR BMI' exp(c_BMI10);
   ESTIMATE 'SEV: OR FEV rev' exp(c_FEVREVPBL100);
   ESTIMATE 'SEV: OR Eos blood' exp(c_BLEOSBL);
   ESTIMATE 'SEV: OR smoking' exp(c_SMOKCD);
   ESTIMATE 'SEV: OR asthma dur' exp(c_DURYR10);
   ESTIMATE 'SEV: OR OCS' exp(c_OCSDBL10);
   ESTIMATE 'SEV: OR ethnicity' exp(c_ETHNICCD);
   ESTIMATE 'SEV: OR IgE' exp(c_IgEBL1000);
   ESTIMATE 'SEV: OR pre exacer' exp(c_PREEXAC10);
   ESTIMATE 'SEV: OR Period' exp(c_PERIOD);

   ESTIMATE 'ZI: OR TRT 75mg' exp(p_UTRTCD2);
   ESTIMATE 'ZI: OR TRT 100mg' exp(p_UTRTCD3);
   ESTIMATE 'ZI: OR TRT 250mg' exp(p_UTRTCD4);
   ESTIMATE 'ZI: OR TRT 750mg' exp(p_UTRTCD5);
   ESTIMATE 'ZI: OR age' exp(p_AGE10);
   ESTIMATE 'ZI: OR female' exp(p_WOMAN);
   ESTIMATE 'ZI: OR FEV1' exp(p_FEV1PPBL100);
   ESTIMATE 'ZI: OR polyp' exp(p_POLYPBL);
   ESTIMATE 'ZI: OR SACQ' exp(p_SACQBL);
   ESTIMATE 'ZI: OR BMI' exp(p_BMI10);
   ESTIMATE 'ZI: OR FEV rev' exp(p_FEVREVPBL100);
   ESTIMATE 'ZI: OR Eos blood' exp(p_BLEOSBL);
   ESTIMATE 'ZI: OR smoking' exp(p_SMOKCD);
   ESTIMATE 'ZI: OR asthma dur' exp(p_DURYR10);
   ESTIMATE 'ZI: OR OCS' exp(p_OCSDBL10);
   ESTIMATE 'ZI: OR ethnicity' exp(p_ETHNICCD);
   ESTIMATE 'ZI: OR IgE' exp(p_IgEBL1000);
   ESTIMATE 'ZI: OR pre exacer' exp(p_PREEXAC10);

   ESTIMATE 'SD of Z1' SQRT(v11);
   ESTIMATE 'SD of Z2' SQRT(v22);
   ESTIMATE 'SD of Z3' SQRT(v33);
   ESTIMATE 'Correlation coefficient Z1 & Z2' v21/SQRT(v11 * v22);
   ESTIMATE 'Correlation coefficient Z1 & Z3' v31/SQRT(v11 * v33);
   ESTIMATE 'Correlation coefficient Z2 & Z3' v32/SQRT(v33 * v22);

RUN;
TITLE;



