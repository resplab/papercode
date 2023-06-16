Libname Qdata "R:\qdata\";


data qdata.copd;
set edata.clinic_all_oad3;
if medcode in (11019, 104710, 46036, 10863, 45770, 10802, 9876,26018, 26306, 28755, 33450, 34202, 34215, 37371,
104985, 67040, 104481, 19434, 93568, 105457, 101042, 1001, 11287,4084, 18476, 794, 9520, 10980,14798, 23492, 
65733, 103007, 37247, 998, 12166, 1446, 104608, 44525, 45998, 38074, 42258, 42313, 
45771, 104117, 19003, 103864, 19106) then copd=1;
run;

data qdata.copd1;
set qdata.copd;
if copd=. then delete;
run;

proc sort data=qdata.copd1 out=qdata.copd2;
  by patid medcode copd patid_age;
  run;

data qdata.copd3;
set qdata.copd2;
keep patid copd clinicaldate;
run;

data qdata.copd3aa;
set qdata.copd3;
start_year='01Jan1998'd;
format start_year ddmmyy10.; 
run;

data qdata.copd3ab;
set qdata.copd3aa;
where clinicaldate >= start_year;
run;

proc sort data=qdata.copd3ab out=qdata.copd3ac;
by patid clinicaldate;
run;

data qdata.copd3ad;
set qdata.copd3ac;
by patid;
if first.patid;
run;


***//PREVALENT COPDS = 1,931//**;
data qdata.copd3ae;
set qdata.copd3aa;
where clinicaldate < start_year;
run;

data qdata.copd3af;
set qdata.copd3ae;
by patid;
if first.patid;
run;

data qdata.copd3ag;
set qdata.copd3af;
if copd=1 then pcopd=1;
run;

proc sql;
create table qdata.copd3ah as
select a.*, b.pcopd
from qdata.copd3ad a left join qdata.copd3ag b
on a.patid=b.patid;
quit;
run;

data qdata.copd3ai;
set qdata.copd3ah;
if copd=1 & pcopd=1 then delete;
run;

proc sql;
create table qdata.copd3aj as
select a.*, b.yob
from qdata.copd3ai a left join qdata.pts_file b
on a.patid=b.patid;
quit;
run;

data qdata.ages1;
set qdata.ages;
patid_age = cdate-yob;
run;

proc sql;
create table qdata.copd3ak as
select a.*, b.patid_age
from qdata.copd3aj a left join qdata.ages1 b
on a.patid=b.patid;
quit;
run;

data qdata.copd3al;
set qdata.copd3ak;
if patid_age=. then delete;
drop start_year pcopd yob;
run;

data qdata.copd3am;
set qdata.copd3al;
where patid_age>=40;
run;
data qdata.copd3an;
set qdata.copd3am;
where patid_age<=84;
run;

proc sql;
create table qdata.copd_n1 as
select a.*, b.regstart, b.regend
from qdata.copd3an a left join qdata.all_cases b
on a.patid=b.patid;
quit;
run;


data qdata.copd_n2;
set qdata.copd_n1;
copd_date=clinicaldate;
regstart_new=intnx('day', copd_date, -365.25);
format regstart_new date9.;
run;

data qdata.copd_n3;
set qdata.copd_n2;
if regstart_new < regstart then delete;
run;

proc sql;
create table qdata.copd_n4 as
select a.*, b.imd2015_5
from qdata.copd_n3 a left join qdata.imd_2015 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n5;
set qdata.copd_n4;
if imd2015_5=. then delete;
run;


data qdata.thera_1;
set edata.therapy_all;
drop sysdate consid staffid dosageid bnfcode qty numdays numpacks packtype issueseq;
run; 

proc sql;
create table qdata.thera2 as
select a.*, b.copd, b.copd_date, b.patid_age
from qdata.thera_1 a left join qdata.final_table b
on a.patid=b.patid;
quit;
run;


proc sql;
create table qdata.copd_n6 as
select a.*, b.copd, b.copd_date, b.patid_age
from qdata.thera_1 a left join qdata.copd_n5 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n7;
set qdata.copd_n6;
if copd=. then delete;
run;


data qdata.copd_statins;
set qdata.copd_n7;
where eventdate <= copd_date;
run;

data qdata.copd_statins1;
set qdata.copd_statins;
if prodcode in (60160,52098,34969,42,61665,6168,65193,67773,41657,50703,34820,13041,55034,51200,43218,7374,54947,40601,47774,64825,53460,53770,57834,53772,
49061,61360,34366,63787,67573,49751,48518,4961,55727,55032,25,63469,55912,52953,48221,17688,51233,50963,53887,9920,730,34746,44650,49587,72213,69413,
17683,39870,51622,62979,59452,58041,7554,59446,71029,53908,45219,3411,45235,52398,51483,50754,48078,54655,47988,62476,50483,58315,65679,
802,54992,58834,34502,5985,50236,57329,52397,53676,48058,34814,56146,22579,58742,2955,53890,44878,34353,64968,75,56182,48346,54535,51676,
72048,55444,6213,1223,5775,62429,28,60607,63074,57763,54266,54819,31930,45346,818,34316,59278,58110,72149,69427,72308,50925,51085,39675,
47948,52676,50790,50564,48097,65181,52812,39060,51715,379,50272,64307,57999,70987,59508,65901,48867,71015,68023,58418,51166,51876,56494,
9930,68686,52821,70693,9316,51,54976,53594,56248,55452,68048,2718,3690,50788,34476,49062,54435,34381,64868,53087,60989,34376,49558,745,
11627,5148,52257,53415,54607,54493,34312,67402,68827,56607,52755,58394,66963,52962,57117,64180,56916,56165,56841,59447,63140,69093,32909,
56481,2137,46878,67829,64702,52097,60464,56065,71773,62137,8380,53340,52460,50670,9897,47721,52211,45245,67745,54985,15252,7347,54240,61155,56893,
59776,7196,37434,54606,59272,56097,65925,57296,72050,34535,67328,34560,53966,1219,40382,46956,58868,72164,52459,67098,18442,34545,53822,
34891,59357,57397,32921,57568,5713734481,56564,48051,50882,64104,51890,68785,63249,68467,71014,34907,61321,9315,34879,71017,60251,39652,
1221,52168,64067,62219,68563,56016,58755,48973,58617,57348,67660,51359,9153,34955,59331,36377,44528,56735,48431,40340,48018,57108,67846,
70308,33082,59859,52625,57836,62148,64810,51134,490,713,60511,61149,61134,68156)
then statins=1;
run;

data qdata.copd_statins2;
set qdata.copd_statins1;
if statins=. then delete;
run;

proc sort data=qdata.copd_statins2 out=qdata.copd_statins3;
  by patid eventdate;
  run;

data qdata.copd_statins4;
set qdata.copd_statins3;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n8 as
select a.*, b.statins
from qdata.copd_n5 a left join qdata.copd_statins4 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n9;
set qdata.copd_n8;
if statins=1 then delete;
format copd_date ddmmyy10.;
run;

data qdata.CHD_c1;
set edata.clinic_all_add1;
keep patid enttype medcode clinicaldate data1 data2;
run;

proc sql;
create table qdata.chd_c2 as
select a.*, b.copd, b.copd_date
from qdata.CHD_c1 a left join qdata.copd_n9 b
on a.patid=b.patid;
quit;


data qdata.chd_c3;
set qdata.chd_c2;
if copd=. then delete;
run;


data qdata.chd_c4;
set qdata.chd_c3;
start_year='01Jan1998'd;
format start_year ddmmyy10.; 
run;

data qdata.chd_c5;
set qdata.chd_c4;
where (clinicaldate >= start_year);
run;


data qdata.chd_c6;
set qdata.chd_c5;
chd_10yrs=intnx('day', copd_date, +3652.50);
format chd_10yrs date9.;
run;

data qdata.chd_c7;
set qdata.chd_c6;
where (clinicaldate >= copd_date) & (clinicaldate <= chd_10yrs);
run;

data qdata.chd_c8;
set qdata.chd_c7;
if medcode in (240, 24783, 20416, 1792, 241, 13566, 2491, 30421, 1204, 1677, 13571, 17689, 12139, 5387, 40429, 17872,
14897, 8935, 29643, 23892, 14898, 63467, 3704, 9507, 10562, 1678, 30330, 17133, 32854, 29758, 12229, 34803, 28736, 62626,
41221, 46017, 14658, 27951, 9413, 23579, 36523, 4656, 39655, 1431, 7347, 19655, 17307, 61072, 55137, 34328, 18118, 11983,
54251, 39449, 9276, 68357, 39693, 21844, 27977, 4017, 16408, 17464, 1430, 20095, 18125, 29902, 11048, 36854, 28554, 25842,
66388, 54535, 7696, 1414, 32450, 9555, 26863, 12804, 28138, 5413, 1655, 1344, 3999, 5254, 36609, 7320, 29421, 34633, 24540,
23078, 35713, 15754, 18889, 18842, 45809, 38609, 72562, 46166, 36423, 24126,  23708, 37657, 59940, 69474, 29553, 32272,
46112, 46276, 106812, 41835, 68748, 22383, 1676, 35119, 96838, 1195, 63746, 23671, 24446, 8837, 5363, 569, 6155, 16517,
36717, 15019, 34758, 27975, 3149, 15252, 5602, 25615, 47642, 5185, 9985, 10504, 26424, 504, 3132, 1433, 2417, 23942, 5268,
23465, 44765, 50594, 10794, 19354, 15788, 1895, 55247, 16507, 1469, 1298, 6253, 6116, 7780, 12833, 39344, 40758, 33543,
91627, 53745, 90572, 92036, 101251) then CHD=1;
RUN;

data qdata.chd_c9;
set qdata.chd_c8;
if CHD=. then delete;
run;

proc sort data=qdata.chd_c9 out=qdata.chd_c10;
  by patid clinicaldate;
  run;

data qdata.chd_c11;
set qdata.chd_c10;
by patid;
if first.patid;
run;

data qdata.chd_c12;
set qdata.chd_c11;
chd_date=clinicaldate;
keep patid medcode CHD chd_date;
format chd_date date9.;
run;


data qdata.chd_cprev;
set qdata.chd_c3;
where clinicaldate <=copd_date;
run;

data qdata.chd_cprev1;
set qdata.chd_cprev;
if medcode in (240, 24783, 20416, 1792, 241, 13566, 2491, 30421, 1204, 1677, 13571, 17689, 12139, 5387, 40429, 17872,
14897, 8935, 29643, 23892, 14898, 63467, 3704, 9507, 10562, 1678, 30330, 17133, 32854, 29758, 12229, 34803, 28736, 62626,
41221, 46017, 14658, 27951, 9413, 23579, 36523, 4656, 39655, 1431, 7347, 19655, 17307, 61072, 55137, 34328, 18118, 11983,
54251, 39449, 9276, 68357, 39693, 21844, 27977, 4017, 16408, 17464, 1430, 20095, 18125, 29902, 11048, 36854, 28554, 25842,
66388, 54535, 7696, 1414, 32450, 9555, 26863, 12804, 28138, 5413, 1655, 1344, 3999, 5254, 36609, 7320, 29421, 34633, 24540,
23078, 35713, 15754, 18889, 18842, 45809, 38609, 72562, 46166, 36423, 24126,  23708, 37657, 59940, 69474, 29553, 32272,
46112, 46276, 106812, 41835, 68748, 22383, 1676, 35119, 96838, 1195, 63746, 23671, 24446, 8837, 5363, 569, 6155, 16517,
36717, 15019, 34758, 27975, 3149, 15252, 5602, 25615, 47642, 5185, 9985, 10504, 26424, 504, 3132, 1433, 2417, 23942, 5268,
23465, 44765, 50594, 10794, 19354, 15788, 1895, 55247, 16507, 1469, 1298, 6253, 6116, 7780, 12833, 39344, 40758, 33543,
91627, 53745, 90572, 92036, 101251) then PCHD=1;
RUN;

data qdata.chd_cprev2;
set qdata.chd_cprev1;
if PCHD=. then delete;
run;

data qdata.chd_cprev3;
set qdata.chd_cprev2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.chd_c13 as
select a.*, b.PCHD
from qdata.chd_c12 a left join qdata.chd_cprev3 b
on a.patid=b.patid;
quit;
run;

data qdata.chd_c14;
set qdata.chd_c13;
if PCHD=1 & CHD=1 then delete;
run;


proc sql;
create table qdata.copd_n10 as
select a.*, b.PCHD
from qdata.copd_n9 a left join qdata.chd_cprev3 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n11;
set qdata.copd_n10;
if PCHD=1 then delete;
run;


proc sql;
create table qdata.chd_hes as
select a.*, b.copd, b.copd_date
from qdata.hes_dxh1 a left join qdata.copd_n11 b
on a.patid=b.patid;
quit;

data qdata.chd_hes1;
set qdata.chd_hes;
if copd=. then delete;
drop indexdate spno;
run;

data qdata.chd_hes2;
set qdata.chd_hes1;
chd_10yrs=intnx('day', copd_date, +3652.50);
format chd_10yrs date9.;
run;

data qdata.chd_hes3;
set qdata.chd_hes2;
where (admidate >= copd_date) & (admidate <= chd_10yrs);
run;

data qdata.chd_hes4;
set qdata.chd_hes3;
found1=index(ICD, "G45");
found2=index(ICD, "I20");
found3=index(ICD, "I21");
found4=index(ICD, "I22");
found5=index(ICD, "I23");
found6=index(ICD, "I24");
found7=index(ICD, "I25");
found8=index(ICD, "I63");
found9=index(ICD, "I64");
foundtotal=sum(of found1 found2 found3 found4 found5 found6 found7 found8 found9);
RUN;

data qdata.chd_hes5;
set qdata.chd_hes4;
where foundtotal >0;
drop found1 found2 found3 found4 found5 found6 found7 found8 found9;
run;

proc sort data=qdata.chd_hes5 out=qdata.chd_hes6;
by patid admidate;
run;

data qdata.chd_hes7;
set qdata.chd_hes6;
by patid;
if first.patid;
run;

data qdata.chd_hes8;
set qdata.chd_hes7;
if foundtotal~=. then CHD_hes=1; 
hes_date=admidate;
format hes_date date9.;
keep patid CHD_hes hes_date ICD copd_date;
run;

data qdata.chd_prevhes1;
set qdata.chd_hes1;
where admidate <=copd_date;
run;

data qdata.chd_phes1;
set qdata.chd_prevhes1;
found1=index(ICD, "G45");
found2=index(ICD, "I20");
found3=index(ICD, "I21");
found4=index(ICD, "I22");
found5=index(ICD, "I23");
found6=index(ICD, "I24");
found7=index(ICD, "I25");
found8=index(ICD, "I63");
found9=index(ICD, "I64");
foundtotal=sum(of found1 found2 found3 found4 found5 found6 found7 found8 found9);
RUN;

data qdata.chd_phes2;
set qdata.chd_phes1;
where foundtotal >0;
drop found1 found2 found3 found4 found5 found6 found7 found8 found9;
run;

data qdata.chd_phes3;
set qdata.chd_phes2;
by patid;
if first.patid;
run;

data qdata.chd_phes4;
set qdata.chd_phes3;
if foundtotal~=. then PCHD_hes=1;
run;

proc sql;
create table qdata.chd_hes9 as
select a.*, b.PCHD_hes
from qdata.chd_hes8 a left join qdata.chd_phes4 b
on a.patid=b.patid;
quit;


data qdata.chd_hes10;
set qdata.chd_hes9;
if CHD_hes=1 & PCHD_hes=1 then delete;
run;

proc sql;
create table qdata.copd_n12 as
select a.*, b.PCHD_hes
from qdata.copd_n11 a left join qdata.chd_phes4 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n13;
set qdata.copd_n12;
if PCHD_hes=1 then delete;
run;


proc sql;
create table qdata.chd_m as
select a.*, b.copd, b.copd_date
from qdata.chd_mort a left join qdata.copd_n13 b
on a.patid=b.patid;
quit;

data qdata.chd_m1;
set qdata.chd_m;
if copd=. then delete;
drop indexdate spno;
run;

data qdata.chd_m2;
set qdata.chd_m1;
chd_10yrs=intnx('day', copd_date, +3652.50);
format chd_10yrs date9.;
run;

data qdata.chd_m3;
set qdata.chd_m2;
where (dod >= copd_date) & (dod <= chd_10yrs);
run;

data qdata.chd_m4;
set qdata.chd_m3;
found1=index(cause, "G45");
found2=index(cause, "I20");
found3=index(cause, "I21");
found4=index(cause, "I22");
found5=index(cause, "I23");
found6=index(cause, "I24");
found7=index(cause, "I25");
found8=index(cause, "I63");
found9=index(cause, "I64");
foundtotal=sum(of found1 found2 found3 found4 found5 found6 found7 found8 found9);
RUN;

data qdata.chd_m5;
set qdata.chd_m4;
where foundtotal >0;
drop found1 found2 found3 found4 found5 found6 found7 found8 found9;
run;

data qdata.chd_m6;
set qdata.chd_m5;
by patid;
if first.patid;
run;

data qdata.chd_m7;
set qdata.chd_m6;
if foundtotal ~=. then CHD_mort=1; 
run;


data qdata.chd_c15;
set qdata.chd_c14;
drop medcode PCHD; 
run;

data qdata.chd_hes11 (rename=(hes_date=chd_date));
set qdata.chd_hes10;
run;

data qdata.chd_hes12 (rename=(chd_hes=CHD));
set qdata.chd_hes11;
run;

data qdata.chd_hes13;
set qdata.chd_hes12;
drop ICD copd_date chd_hes hes_date pchd_hes; 
run;

data qdata.chd_m8 (rename=(chd_mort=CHD));
set qdata.chd_m7;
run;

data qdata.chd_m9 (rename=(dod=chd_date));
set qdata.chd_m8;
run;

data qdata.chd_m10;
set qdata.chd_m9;
drop pracid dor dod cause cause1 cause2 cause3 cause4 cause5 cause6 copd copd_date chd_10yrs foundtotal CHD_mort; 
run;


proc append base=qdata.chd_c15 data=qdata.chd_hes13;
run;

proc append base=qdata.chd_c15 data=qdata.chd_m10;
run;

proc sort data=qdata.chd_c15 out=qdata.chd_c16;
  by patid chd_date;
  run;

data qdata.chd_c17;
set qdata.chd_c16;
by patid;
if first.patid;
run;

data qdata.copd_n14;
set qdata.copd_n13;
DROP statins pchd pchd_hes clinicaldate;
run;

proc sql;
create table qdata.copd_n15 as
select a.*, b.chd, b.chd_date
from qdata.copd_n14 a left join qdata.chd_c17 b
on a.patid=b.patid;
quit;
run;

proc sql;
create table qdata.copd_n16 as
select a.*, b.gender
from qdata.copd_n15 a left join qdata.pts_file b
on a.patid=b.patid;
quit;
run;

proc freq data=qdata.copd_n16;
table CHD;
run;

proc sql;
create table qdata.copd_n17 as
select a.*, b.gen_ethnicity
from qdata.copd_n16 a left join qdata.hes_pts b
on a.patid=b.patid;
quit;
run;


data qdata.test_pred;
set qdata.test_all;
drop constype consid consid data4 data5 data6 data7 data8 indexdate;
run;

proc sql;
create table qdata.test_pred1 as
select a.*, b.copd, b.copd_date
from qdata.test_pred a left join qdata.copd_n17 b
on a.patid=b.patid;
quit;

data qdata.test_pred2;
set qdata.test_pred1;
if copd=. then delete;
run;


data qdata.test_pred3;
set qdata.test_pred2;
where testdate < copd_date;
run;

data qdata.pred_tscho;
set qdata.test_pred3;
if enttype in (163) then tscho="status"; 
if tscho =" " then delete;
run;

data qdata.pred_tscho1;
set qdata.pred_tscho;
drop data1 data3 data4;
if data2~=. then tscho=data2;
if tscho="status" then delete;
run;

proc sort data=qdata.pred_tscho1 out=qdata.pred_tscho2;
by patid descending testdate;
run;

data qdata.pred_tscho3;
set qdata.pred_tscho2;
by patid;
if first.patid;
run;


data qdata.tscho_statins;
set qdata.copd_n7;
where eventdate >= copd_date;
run;

data qdata.tscho_statins1;
set qdata.tscho_statins;
if prodcode in (60160,52098,34969,42,61665,6168,65193,67773,41657,50703,34820,13041,55034,51200,43218,7374,54947,40601,47774,64825,53460,53770,57834,53772,
49061,61360,34366,63787,67573,49751,48518,4961,55727,55032,25,63469,55912,52953,48221,17688,51233,50963,53887,9920,730,34746,44650,49587,72213,69413,
17683,39870,51622,62979,59452,58041,7554,59446,71029,53908,45219,3411,45235,52398,51483,50754,48078,54655,47988,62476,50483,58315,65679,
802,54992,58834,34502,5985,50236,57329,52397,53676,48058,34814,56146,22579,58742,2955,53890,44878,34353,64968,75,56182,48346,54535,51676,
72048,55444,6213,1223,5775,62429,28,60607,63074,57763,54266,54819,31930,45346,818,34316,59278,58110,72149,69427,72308,50925,51085,39675,
47948,52676,50790,50564,48097,65181,52812,39060,51715,379,50272,64307,57999,70987,59508,65901,48867,71015,68023,58418,51166,51876,56494,
9930,68686,52821,70693,9316,51,54976,53594,56248,55452,68048,2718,3690,50788,34476,49062,54435,34381,64868,53087,60989,34376,49558,745,
11627,5148,52257,53415,54607,54493,34312,67402,68827,56607,52755,58394,66963,52962,57117,64180,56916,56165,56841,59447,63140,69093,32909,
56481,2137,46878,67829,64702,52097,60464,56065,71773,62137,8380,53340,52460,50670,9897,47721,52211,45245,67745,54985,15252,7347,54240,61155,56893,
59776,7196,37434,54606,59272,56097,65925,57296,72050,34535,67328,34560,53966,1219,40382,46956,58868,72164,52459,67098,18442,34545,53822,
34891,59357,57397,32921,57568,5713734481,56564,48051,50882,64104,51890,68785,63249,68467,71014,34907,61321,9315,34879,71017,60251,39652,
1221,52168,64067,62219,68563,56016,58755,48973,58617,57348,67660,51359,9153,34955,59331,36377,44528,56735,48431,40340,48018,57108,67846,
70308,33082,59859,52625,57836,62148,64810,51134,490,713,60511,61149,61134,68156)
then statins=1;
run;

data qdata.tscho_statins2;
set qdata.tscho_statins1;
if statins=. then delete;
run;

data qdata.tscho_statins3;
set qdata.tscho_statins2;
follow_up=intnx('day', copd_date, 3652.5);
format follow_up date9.;
format copd_date date9.;
run;

data qdata.tscho_statins4;
set qdata.tscho_statins3;
where eventdate <= follow_up;
run;

proc sort data=qdata.tscho_statins4 out=qdata.tscho_statins5;
  by patid eventdate;
  run;

data qdata.tscho_statins6;
set qdata.tscho_statins5;
by patid;
if first.patid;
run;

data qdata.tscho_statins7;
set qdata.tscho_statins6;
statin_date=eventdate;
statin_rv=statins;
format statin_date date9.;
run;

proc sql;
create table qdata.tscho_st as
select a.*, b.statin_date
from qdata.test_pred2 a left join qdata.tscho_statins7 b
on a.patid=b.patid;
quit;

data qdata.tscho_st1;
set qdata.tscho_st;
if statin_date=. then delete;
where testdate < statin_date;
run;

data qdata.tscho_st2;
set qdata.tscho_st1;
if enttype in (163) then tscho1="status"; 
if tscho1 =" " then delete;
run;

data qdata.tscho_st3;
set qdata.tscho_st2;
drop data1 data3 data4;
if data2~=. then tscho1=data2;
if tscho1="status" then delete;
run;

proc sort data=qdata.tscho_st3 out=qdata.tscho_st4;
by patid descending testdate;
run;

data qdata.tscho_st5;
set qdata.tscho_st4;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n18 as
select a.*, b.tscho
from qdata.copd_n17 a left join qdata.pred_tscho3 b
on a.patid=b.patid;
quit;
run;

proc sql;
create table qdata.copd_n19 as
select a.*, b.tscho1
from qdata.copd_n18 a left join qdata.tscho_st5 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n20;
set qdata.copd_n19;
if tscho=0 then tscho=.;
if tscho=. then tscho=tscho1;
if tscho=0 then tscho=.;
drop tscho1;
run;


proc sql;
create table qdata.tscho_chd as
select a.*, b.chd_date
from qdata.test_pred2 a left join qdata.copd_n20 b
on a.patid=b.patid;
quit;


data qdata.tscho_chd1;
set qdata.tscho_chd;
if chd_date=. then delete;
where testdate < chd_date;
run;

data qdata.tscho_chd2;
set qdata.tscho_chd1;
if enttype in (163) then tscho2="status"; 
if tscho2 =" " then delete;
run;

data qdata.tscho_chd3;
set qdata.tscho_chd2;
drop data1 data3 data4;
if data2~=. then tscho2=data2;
if tscho2="status" then delete;
run;

proc sort data=qdata.tscho_chd3 out=qdata.tscho_chd4;
by patid descending testdate;
run;

data qdata.tscho_chd5;
set qdata.tscho_chd4;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n21 as
select a.*, b.tscho2
from qdata.copd_n20 a left join qdata.tscho_chd5 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n22;
set qdata.copd_n21;
if tscho=. then tscho=tscho2;
if tscho=0 then tscho=.;
drop tscho2;
run;


data qdata.pred_hdl;
set qdata.test_pred3;
if enttype in (175) then hdl="status"; 
if hdl =" " then delete;
run;

data qdata.pred_hdl1;
set qdata.pred_hdl;
drop data1 data3 data4;
if data2~=. then hdl=data2;
if hdl="status" then delete;
run;

proc sort data=qdata.pred_hdl1 out=qdata.pred_hdl2;
by patid descending testdate;
run;

data qdata.pred_hdl3;
set qdata.pred_hdl2;
by patid;
if first.patid;
run;


data qdata.hdl_statins;
set qdata.copd_n7;
where eventdate >= copd_date;
run;

data qdata.hdl_statins1;
set qdata.hdl_statins;
if prodcode in (60160,52098,34969,42,61665,6168,65193,67773,41657,50703,34820,13041,55034,51200,43218,7374,54947,40601,47774,64825,53460,53770,57834,53772,
49061,61360,34366,63787,67573,49751,48518,4961,55727,55032,25,63469,55912,52953,48221,17688,51233,50963,53887,9920,730,34746,44650,49587,72213,69413,
17683,39870,51622,62979,59452,58041,7554,59446,71029,53908,45219,3411,45235,52398,51483,50754,48078,54655,47988,62476,50483,58315,65679,
802,54992,58834,34502,5985,50236,57329,52397,53676,48058,34814,56146,22579,58742,2955,53890,44878,34353,64968,75,56182,48346,54535,51676,
72048,55444,6213,1223,5775,62429,28,60607,63074,57763,54266,54819,31930,45346,818,34316,59278,58110,72149,69427,72308,50925,51085,39675,
47948,52676,50790,50564,48097,65181,52812,39060,51715,379,50272,64307,57999,70987,59508,65901,48867,71015,68023,58418,51166,51876,56494,
9930,68686,52821,70693,9316,51,54976,53594,56248,55452,68048,2718,3690,50788,34476,49062,54435,34381,64868,53087,60989,34376,49558,745,
11627,5148,52257,53415,54607,54493,34312,67402,68827,56607,52755,58394,66963,52962,57117,64180,56916,56165,56841,59447,63140,69093,32909,
56481,2137,46878,67829,64702,52097,60464,56065,71773,62137,8380,53340,52460,50670,9897,47721,52211,45245,67745,54985,15252,7347,54240,61155,56893,
59776,7196,37434,54606,59272,56097,65925,57296,72050,34535,67328,34560,53966,1219,40382,46956,58868,72164,52459,67098,18442,34545,53822,
34891,59357,57397,32921,57568,5713734481,56564,48051,50882,64104,51890,68785,63249,68467,71014,34907,61321,9315,34879,71017,60251,39652,
1221,52168,64067,62219,68563,56016,58755,48973,58617,57348,67660,51359,9153,34955,59331,36377,44528,56735,48431,40340,48018,57108,67846,
70308,33082,59859,52625,57836,62148,64810,51134,490,713,60511,61149,61134,68156)
then statins=1;
run;

data qdata.hdl_statins2;
set qdata.hdl_statins1;
if statins=. then delete;
run;

data qdata.hdl_statins3;
set qdata.hdl_statins2;
follow_up=intnx('day', copd_date, 3652.5);
format follow_up date9.;
format copd_date date9.;
run;

data qdata.hdl_statins4;
set qdata.hdl_statins3;
where eventdate <= follow_up;
run;

proc sort data=qdata.hdl_statins4 out=qdata.hdl_statins5;
  by patid eventdate;
  run;

data qdata.hdl_statins6;
set qdata.hdl_statins5;
by patid;
if first.patid;
run;

data qdata.hdl_statins7;
set qdata.hdl_statins6;
statin_date=eventdate;
statin_rv=statins;
format statin_date date9.;
run;


proc sql;
create table qdata.hdl_st as
select a.*, b.statin_date
from qdata.test_pred2 a left join qdata.hdl_statins7 b
on a.patid=b.patid;
quit;

data qdata.hdl_st1;
set qdata.hdl_st;
if statin_date=. then delete;
where testdate < statin_date;
run;

data qdata.hdl_st2;
set qdata.hdl_st1;
if enttype in (175) then hdl1="status"; 
if hdl1 =" " then delete;
run;

data qdata.hdl_st3;
set qdata.hdl_st2;
drop data1 data3 data4;
if data2~=. then hdl1=data2;
if hdl1="status" then delete;
run;

proc sort data=qdata.hdl_st3 out=qdata.hdl_st4;
by patid descending testdate;
run;

data qdata.hdl_st5;
set qdata.hdl_st4;
by patid;
if first.patid;
run;


proc sql;
create table qdata.copd_n23 as
select a.*, b.hdl
from qdata.copd_n22 a left join qdata.pred_hdl3 b
on a.patid=b.patid;
quit;
run;

proc sql;
create table qdata.copd_n24 as
select a.*, b.hdl1
from qdata.copd_n23 a left join qdata.hdl_st5 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n25;
set qdata.copd_n24;
if hdl=0 then hdl=.;
if hdl=. then hdl=hdl1;
if hdl=0 then hdl=.;
drop hdl1;
run;


proc sql;
create table qdata.hdl_chd as
select a.*, b.chd_date
from qdata.test_pred2 a left join qdata.copd_n25 b
on a.patid=b.patid;
quit;


data qdata.hdl_chd1;
set qdata.hdl_chd;
if chd_date=. then delete;
where testdate < chd_date;
run;

data qdata.hdl_chd2;
set qdata.hdl_chd1;
if enttype in (175) then hdl2="status"; 
if hdl2 =" " then delete;
run;

data qdata.hdl_chd3;
set qdata.hdl_chd2;
drop data1 data3 data4;
if data2~=. then hdl2=data2;
if hdl2="status" then delete;
run;

proc sort data=qdata.hdl_chd3 out=qdata.hdl_chd4;
by patid descending testdate;
run;

data qdata.hdl_chd5;
set qdata.hdl_chd4;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n26 as
select a.*, b.hdl2
from qdata.copd_n25 a left join qdata.hdl_chd5 b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n27;
set qdata.copd_n26;
if hdl=. then hdl=hdl2;
if hdl=0 then hdl=.;
drop hdl2;
run;


data qdata.smoke;
set qdata.variable;
drop indexdate patid_age data5 data6 data7;
run;

proc sql;
create table qdata.smoke1 as
select a.*, b.copd, b.copd_date
from qdata.smoke a left join qdata.copd_n27 b
on a.patid=b.patid;
quit;

data qdata.smoke2;
set qdata.smoke1;
if copd=. then delete;
run;

data qdata.var_pred;
set qdata.smoke2;
where clinicaldate < copd_date;
run; 


data qdata.smok_status;
set qdata.var_pred;
if enttype in (4) then smoking="status"; 
if smoking =" " then delete;
run;

proc sort data=qdata.smok_status out=qdata.smok_status1;
  by patid descending clinicaldate;
  run;

data qdata.smok_status2;
set qdata.smok_status1;
by patid;
if first.patid;
run;

data qdata.smok_status3;
set qdata.smok_status2;
if data1=1 then smoke = "yes";
if data1=3 then smoke = "ex";
if data1=2 then smoke = "no";
if smoke=" " then delete;
run;

data qdata.smok_status4;
set qdata.smok_status3;
if (10<=data2<20) then smoke_status="moderate";
else if (0<=data2<10) then smoke_status="light";
if data2 >= 20 then smoke_status="heavy";      
run;

data qdata.smok_status5;
set qdata.smok_status4;
if smoke_status=" " then smoke_status=smoke;
run;


proc sql;
create table qdata.copd_n28 as
select a.*, b.smoke_status
from qdata.copd_n27 a left join qdata.smok_status5 b
on a.patid=b.patid;
quit;


data qdata.copd_n29;
set qdata.copd_n28;
IF (tscho~=. & hdl~=.) then serum_hdl=(tscho/hdl);
serum_hdl_ratio = round(serum_hdl,0.01);
drop serum_hdl;
run;


data qdata.thera_1;
set edata.therapy_all;
drop sysdate consid staffid dosageid bnfcode qty numdays numpacks packtype issueseq;
run; 

proc sql;
create table qdata.thera2 as
select a.*, b.copd, b.copd_date
from qdata.thera_1 a left join qdata.copd_n29 b
on a.patid=b.patid;
quit;
run;

data qdata.thera3;
set qdata.thera2;
if copd=. then delete;
run;

data qdata.thera4;
set qdata.thera3;
px_date=intnx('day', copd_date, -28);
format px_date date9.;
run;

data qdata.thera5;
set qdata.thera4;
where ((eventdate <= copd_date) & (eventdate >= px_date));
run;

data qdata.thera6;
set qdata.thera5;
where prodcode in (10934,34660,25272,41515,58234,54118,60421,64007,32803,34393,58987,2704,34631,64128,53336,61052,59229,63082,34748,34914,34978,2368,34221,
66645,578,41745,28376,34452,34781,66015,64009,20095,65020,44,5913,45302,72421,31532,38407,59912,67076,28859,28375,63791,61162,67107,59338,
21417,54434,63549,53313,65626,34404,9727,55480,5490,51753,69568,34109,66550,58384,69811,59283,33691,95,56891,61132,33988,63066,32835,67559,
66914,34461,67507,557,58000,55024,23512,69686,820,58369,63172,27962,29333,64008,33990,64416,64221,68497,68374,7724,3065,36867,1971,50225,
68306,22577,64235,7286,15717,46280,51167,1332,66556,34166,6339,3754,54715,56319,13350,5118,61791,3651,49707,49498,66666,51849,51872,6098,
65984,64059,71620,58592,4535,51824,3418,59418,13043,66327,52053,51722,69964,14076,63138,1061,54794,57931,38054,64787,13929,1567,292,10677,
52969,48800,35040,48746,35688,1133,14982,5493,48748,71106,35349,33132,27413,9994,56347,60120,5157,21903,68182,66724,68593,69572,56443,
1280,64747,47238,70893,34880,34801,45234186,71926,4779,54793,36055,55401,70611,62909,34915,68489,53207,52396,35453,13972,64050,37500,
26454,31948,26300,61958,68103,68860,19259,26299,10657,61316,71404,60064,66524,66287,47598,21218,64766,53173,13952,58474,4943,14906,66200,
56940,21668,4233,11334,28215,34083,9375,17410,29112,20577,3992,41335,22555,990,10216,4488,8864,768,13981,1799,10732,30244,35578,15617,
48406,3510,14962,14335,50216,12074,4125,4123,37737,11123,40446,16583,22047,9368,24014,14958,1917,3703,19908,50026,16582,33131,23111);
by patid;

retain N;

if first.patid then N = 1;
    else N = N + 1;
    if last.patid then output;
    keep patid N eventdate prodcode copd copd_date px_date;
run;

data qdata.thera7;
set qdata.thera6;
WHERE N=2;
run;

data qdata.thera8;
set qdata.thera6;
WHERE N=1;
run;

proc sql;
create table qdata.thera9 as
select a.*, b.N
from qdata.thera4 a left join qdata.thera8 b
on a.patid=b.patid;
quit;

data qdata.thera10;
set qdata.thera9;
if N~=1 then delete;
run;

data qdata.thera11;
set qdata.thera10;
where (eventdate <px_date);
run;

data qdata.steroid_px;
set qdata.thera11;
if prodcode in (10934,34660,25272,41515,58234,54118,60421,64007,32803,34393,58987,2704,34631,64128,53336,61052,59229,63082,34748,34914,34978,2368,34221,
66645,578,41745,28376,34452,34781,66015,64009,20095,65020,44,5913,45302,72421,31532,38407,59912,67076,28859,28375,63791,61162,67107,59338,
21417,54434,63549,53313,65626,34404,9727,55480,5490,51753,69568,34109,66550,58384,69811,59283,33691,95,56891,61132,33988,63066,32835,67559,
66914,34461,67507,557,58000,55024,23512,69686,820,58369,63172,27962,29333,64008,33990,64416,64221,68497,68374,7724,3065,36867,1971,50225,
68306,22577,64235,7286,15717,46280,51167,1332,66556,34166,6339,3754,54715,56319,13350,5118,61791,3651,49707,49498,66666,51849,51872,6098,
65984,64059,71620,58592,4535,51824,3418,59418,13043,66327,52053,51722,69964,14076,63138,1061,54794,57931,38054,64787,13929,1567,292,10677,
52969,48800,35040,48746,35688,1133,14982,5493,48748,71106,35349,33132,27413,9994,56347,60120,5157,21903,68182,66724,68593,69572,56443,
1280,64747,47238,70893,34880,34801,45234186,71926,4779,54793,36055,55401,70611,62909,34915,68489,53207,52396,35453,13972,64050,37500,
26454,31948,26300,61958,68103,68860,19259,26299,10657,61316,71404,60064,66524,66287,47598,21218,64766,53173,13952,58474,4943,14906,66200,
56940,21668,4233,11334,28215,34083,9375,17410,29112,20577,3992,41335,22555,990,10216,4488,8864,768,13981,1799,10732,30244,35578,15617,
48406,3510,14962,14335,50216,12074,4125,4123,37737,11123,40446,16583,22047,9368,24014,14958,1917,3703,19908,50026,16582,33131,23111)
then steroid2=1;
run;

data qdata.steroid_px1;
set qdata.steroid_px;
if steroid2=. then delete;
run;

proc sort data=qdata.steroid_px1 out=qdata.steroid_px2;
  by patid descending eventdate;
  run;

data qdata.steroid_px3;
set qdata.steroid_px2;
by patid;
if first.patid;
run;

data qdata.steroid_px4;
set qdata.steroid_px3;
drop steroid2;
run;

proc append base=qdata.steroid_px4 data=qdata.thera7;
run;

proc sort data=qdata.steroid_px4 out=qdata.steroid_px5;
  by patid descending eventdate;
  run;

data qdata.steroid_px6;
set qdata.steroid_px5;
by patid;
if first.patid;
run;

data qdata.steroid_px7;
set qdata.steroid_px6;
if N~=. then steroid=1;
run;


proc sql;
create table qdata.copd_n30 as
select a.*, b.steroid
from qdata.copd_n29 a left join qdata.steroid_px7 b
on a.patid=b.patid;
quit;

data qdata.antipsycho6;
set qdata.thera5;
where prodcode in (52076,4992,6524,63598,34927,55625,51558,6482,5071,31576,11938,41714,62202,41702,26544,5927,6109,46969,46889,4876,16768,71481,16434,21709,
9794,65321,4820,65938,9340,667,65773,41070,37501,65472,11799,7039,53552,60842,71594,32076,63087,61748,54346,57114,64493,35141,57412,5283,
39237,31098,63511,9475,6373,21199,69637,1320,59215,38912,14767,40932,63255,35548,70992,46705,51178,64778,62943,16575,66479,38840,14112,
14859,65174,70192,61575,41428,302,1321,62517,44326,51444,57034,64210,58822,38885,63364,66255,70313,42242,56215,16908,52940,14789,68152,
2787,14858,51240,47233,40587,67717,45444,40586,47832,63359,68375,2786,62387,5040,70684,63797,46677,16986,63818,59548,63389,8047,29879,
57612,63363,49696,65137,66427,6573,36116,62531,58067,46764,5262,63494,35589,38080,5219,59829,62916,72326,58935,40779,8046,36954,62463,
5039,64639,63351,58821,6561,38913,70907,69544,65493,38375,57613,14344,16489,58936,38937,58425,35953,54483,9659,45839,63925,38906,11828,
63133,69328,38914,69787,64484,49699,30487,56387,70312,16561,44024,11821,61747,7382,16425,46871,61650,6864,61746,67334,38010,18132,
65671,64957,18013,55661,14813,47302,57217,69635,16006,24358,631,63049,56647,62924,37606,69836,37717,61075,17958);
by patid;

retain N;

if first.patid then N = 1;
    else N = N + 1;
    if last.patid then output;
    keep patid N eventdate prodcode copd copd_date px_date;
run;

data qdata.antipsycho7;
set qdata.antipsycho6;
WHERE N=2;
run;

data qdata.antipsycho8;
set qdata.antipsycho6;
WHERE N=1;
run;

proc sql;
create table qdata.antipsycho9 as
select a.*, b.N
from qdata.thera4 a left join qdata.antipsycho8 b
on a.patid=b.patid;
quit;

data qdata.antipsycho10;
set qdata.antipsycho9;
if N~=1 then delete;
run;

data qdata.antipsycho11;
set qdata.antipsycho10;
where (eventdate <px_date);
run;

data qdata.antipsycho_px;
set qdata.antipsycho11;
if prodcode in (10934,34660,25272,41515,58234,54118,60421,64007,32803,34393,58987,2704,34631,64128,53336,61052,59229,63082,34748,34914,34978,2368,34221,
66645,578,41745,28376,34452,34781,66015,64009,20095,65020,44,5913,45302,72421,31532,38407,59912,67076,28859,28375,63791,61162,67107,59338,
21417,54434,63549,53313,65626,34404,9727,55480,5490,51753,69568,34109,66550,58384,69811,59283,33691,95,56891,61132,33988,63066,32835,67559,
66914,34461,67507,557,58000,55024,23512,69686,820,58369,63172,27962,29333,64008,33990,64416,64221,68497,68374,7724,3065,36867,1971,50225,
68306,22577,64235,7286,15717,46280,51167,1332,66556,34166,6339,3754,54715,56319,13350,5118,61791,3651,49707,49498,66666,51849,51872,6098,
65984,64059,71620,58592,4535,51824,3418,59418,13043,66327,52053,51722,69964,14076,63138,1061,54794,57931,38054,64787,13929,1567,292,10677,
52969,48800,35040,48746,35688,1133,14982,5493,48748,71106,35349,33132,27413,9994,56347,60120,5157,21903,68182,66724,68593,69572,56443,
1280,64747,47238,70893,34880,34801,45234186,71926,4779,54793,36055,55401,70611,62909,34915,68489,53207,52396,35453,13972,64050,37500,
26454,31948,26300,61958,68103,68860,19259,26299,10657,61316,71404,60064,66524,66287,47598,21218,64766,53173,13952,58474,4943,14906,66200,
56940,21668,4233,11334,28215,34083,9375,17410,29112,20577,3992,41335,22555,990,10216,4488,8864,768,13981,1799,10732,30244,35578,15617,
48406,3510,14962,14335,50216,12074,4125,4123,37737,11123,40446,16583,22047,9368,24014,14958,1917,3703,19908,50026,16582,33131,23111)
then antipsycho2=1;
run;

data qdata.antipsycho_px1;
set qdata.antipsycho_px;
if antipsycho2=. then delete;
run;

proc sort data=qdata.antipsycho_px1 out=qdata.antipsycho_px2;
  by patid descending eventdate;
  run;

data qdata.antipsycho_px3;
set qdata.antipsycho_px2;
by patid;
if first.patid;
run;

data qdata.antipsycho_px4;
set qdata.antipsycho_px3;
drop antipsycho2;
run;

proc append base=qdata.antipsycho_px4 data=qdata.antipsycho7;
run;

proc sort data=qdata.antipsycho_px4 out=qdata.antipsycho_px5;
  by patid descending eventdate;
  run;

data qdata.antipsycho_px6;
set qdata.antipsycho_px5;
by patid;
if first.patid;
run;

data qdata.antipsycho_px7;
set qdata.antipsycho_px6;
if N~=. then antipsycho=1;
run;

proc sql;
create table qdata.copd_n31 as
select a.*, b.antipsycho
from qdata.copd_n30 a left join qdata.antipsycho_px7 b
on a.patid=b.patid;
quit;

proc freq data=qdata.copd_n31;
table antipsycho;
run;

data qdata.sbp_var1a;
set qdata.base_pred2;
sbp_5y=intnx('day', copd_date, -1826.25);
format sbp_5y date9.;
run;

data qdata.sbp_var1b;
set qdata.sbp_var1a;
where (clinicaldate < copd_date) & (clinicaldate >= sbp_5y) ;
run;

data qdata.sbp_var1c;
set qdata.sbp_var1b;
if (medcode=1 & enttype=1) then systolic=1;
if systolic=. then delete;
run;

data qdata.sbp_var2;
set qdata.sbp_var1c;
if medcode in (1);
by patid;
retain N;

if first.patid then N = 1;
    else N = N + 1;
    if last.patid then output;
    keep patid N data2;
run;

proc sql;
create table qdata.copd_n32 as
select a.*, b.dod
from qdata.copd_n31 a left join qdata.chd_mort b
on a.patid=b.patid;
quit;


data qdata.copd_n33;
set qdata.copd_n32;
tfollow_chd = chd_date - copd_date;
if tfollow_chd=. then tfollow_chd = (regend - copd_date);
total_fol=tfollow_chd;
run;

PROC SQL;
	SELECT COUNT(*) as n, SUM(chd)AS m, SUM(total_fol/365) AS PY, SUM(total_fol/365)/COUNT(*) AS EPY, 
SUM(chd)/SUM(total_fol)*365 AS incidence FROM qdata.copd_n33;
QUIT;

data qdata.sbp_var3;
set qdata.sbp_var1c;
keep patid data2;
run;

data qdata.sbp_var4;
set qdata.sbp_var3;
sbp=input(data2, 8.);
run;

proc means data=qdata.sbp_var4 n mean stddev;
  by patid;
  var sbp;
  output out=qdata.sbp_var5(drop=_type_ _freq_) mean=mean_sbp stddev=stddev_sbp;
run;

data qdata.sbp_var6;
set qdata.sbp_var5;
sbp = round(mean_sbp,1);
sbp_stddev = round(stddev_sbp,0.1);
drop mean_sbp stddev_sbp;
run;

proc sql;
create table qdata.copd_n34 as
select a.*, b.sbp, b.sbp_stddev
from qdata.copd_n33 a left join qdata.sbp_var6 b
on a.patid=b.patid;
quit;


data qdata.base_pred_med;
set qdata.var_qr1;
drop copd copd_date patid_age;
run;

proc sql;
create table qdata.base_pred1 as
select a.*, b.copd, b.copd_date
from qdata.base_pred_med a left join qdata.copd_n31 b
on a.patid=b.patid;
quit;

data qdata.base_pred2;
set qdata.base_pred1;
if copd=. then delete;
run;

data qdata.base_pred3;
set qdata.base_pred2;
where clinicaldate <=copd_date;
run;


data qdata.fh_cvd;
set qdata.base_pred3;
if medcode in (3198,7207,8223,12089,12709,12806,13269,13270,18661,19128,96212) then fh_cvd=1;
run;

data qdata.fh_cvd1;
set qdata.fh_cvd;
if fh_cvd=. then delete;
run;

proc sort data=qdata.fh_cvd1 out=qdata.fh_cvd2;
  by patid descending clinicaldate;
  run;

data qdata.fh_cvd3;
set qdata.fh_cvd2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n35 as
select a.*, b.fh_cvd
from qdata.copd_n34 a left join qdata.fh_cvd3 b
on a.patid=b.patid;
quit;

data qdata.copd_n36;
set qdata.copd_n35;
if fh_cvd=. then fh_cvd=0;
if steroid=. then steroid=0;
if CHD=. then CHD=0;
if antipsycho=. then antipsycho=0;
format tfollow_chd ddmmyy10.;
format total_fol ddmmyy10.;
run;


data qdata.diab_var;
set qdata.base_pred3;
if medcode in (506, 758, 1038, 1407, 1549, 1647, 2379, 2478, 2664, 5884, 6430, 6791, 6813, 7059, 8403, 8414, 8446,
9013, 10642, 11149, 12455, 13069, 14889, 16881, 17858, 17859, 18219, 18264, 18505, 21472, 22487, 22884, 24423, 24458,
25041, 25627, 26108, 26666, 26667, 26855, 29979, 31310, 32193, 32359, 34541, 35288, 35316, 35321, 37648, 38078, 38103,
38129, 40682, 41686, 43857, 43921, 45250, 45913, 45914, 46521, 46577, 46624, 46850, 47058, 47144, 47315,  47954, 49949,
50609, 50960, 51261, 52236, 53392, 54600, 54601, 55431, 56448, 59991, 60107, 60208, 62613,
63017, 64142, 64668, 69043, 69676, 72702, 82474, 94330, 94777, 95636, 95813, 95992, 96235, 97281, 97446, 97474, 97849,
97894, 98392, 102316, 105337, 105784, 106604, 107824, 107881, 108013, 108724, 109103, 109133, 111106, 112365, 711, 1045,
10278, 8836, 11359, 11471, 11551, 14803, 24490, 34639, 38986, 43453, 45491, 46577, 47032, 49559, 50972, 52212, 60046,
61122, 63371, 63762, 64357, 64384, 96823
1682, 7795, 15690, 16230, 16491, 16502, 21482, 22573, 32403, 32556, 33254, 33343, 33807, 34283, 35105, 35107, 35399, 36695,
39317,40023, 41389, 42505, 42567, 43139, 53200, 54856, 63357, 65025, 65062, 67635, 67853, 68792, 68843, 69748, 70448, 70821,
72345, 93922,110997,2471, 2986, 3286, 4513, 6509, 7045, 7328, 9835, 9881, 9974, 10098, 10099, 10418, 10692, 10755, 10824, 11129, 11433,
11599, 11663, 11848, 12640,
12736, 13097, 13099, 13101, 13102, 13103, 13108, 13279, 17067, 17095, 17262, 17545, 18056, 18143, 18209, 18230, 18278, 18387, 18390, 18425,
18496, 18642, 18683, 18777, 19381, 21983, 22871, 22967, 23479, 24327, 24571, 24693, 24694, 24836, 25591, 26054, 26664, 27921, 28574, 30294,
30310, 30323, 30477, 31053, 31156, 31157, 31171, 31172, 31790, 32627, 33969, 34268, 34450, 34912, 35116, 35385, 35785, 36633, 37806, 38161,
38617, 39070, 39420, 40401, 40837, 41049, 41716, 42729, 42762, 42831, 43227, 43785, 44260, 44440, 44443, 44779, 44982, 45276, 45467, 45919,
46150, 46290, 46301, 46917, 46963, 47321, 47328, 47377, 47409, 47582, 47584, 47649, 47650, 47816, 48078, 48192, 49074, 49146, 49276, 49554,
49640, 49655, 50225, 50429, 50527, 50813, 51697, 51756, 51957, 52041, 52104, 52283, 52303, 52630, 53634, 54008, 54899, 55075, 55239, 55842,
56268, 57278, 57621, 58604, 59253, 59288, 59365, 59725, 60499, 60699, 60796, 61071, 61344, 61523, 61829, 62107, 62146, 62209, 62352, 62384,
62674, 63690, 64283, 64449, 64571, 65267, 65616, 65704, 66145, 66675, 66872, 66965, 67905, 68105, 68390, 69278, 69993, 70316, 70766, 72320,
85991, 91646, 91942, 91943, 93380, 93468, 93727, 93875, 93878, 95343, 95351, 95539, 98071, 98616, 98704, 98723, 99231, 99311, 99628, 99716,
99719, 100292, 100347, 100770, 100964, 101172, 101311, 101735, 101881, 102112, 102163, 102201, 102620, 102740, 102946, 103902, 104323, 104639,
105302, 105740, 105741, 106061, 106360, 106528, 107701, 108005, 108007, 108724, 109051, 109197, 109837,
109865, 110400, 110481, 111798, 112402) then diabetes=1;
run;

data qdata.diab_var1;
set qdata.diab_var;
if diabetes=. then delete;
run;

proc sort data=qdata.diab_var1 out=qdata.diab_var2;
  by patid descending clinicaldate;
  run;

data qdata.diab_var3;
set qdata.diab_var2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n37 as
select a.*, b.diabetes
from qdata.copd_n36 a left join qdata.diab_var3 b
on a.patid=b.patid;
quit;


data qdata.ckd_var;
set qdata.base_pred3;
if medcode in (1839,2997,3762,4139,5504,5911,11553,11745,104963,12479,105151,12585,24361,28158,44422,48833,49028,
54990,55151,56072,64176,64487,66705,69266,70874,72004,89924,90952,93366,94964,95122,95406,95405,95508,96095,96133,
96184,98364,103429,104049,104050,106301,109455,1803,2471,2773,2999,9840,15780,17365,19316,21947,21989,22852,23913,
27427,29634,47672,47922,49150,50472,56987,57926,61814,63786,94373,99201,99644,108816,108922,110749,111370,112548,
4669,7804,10809,15097,34998,56893,60857,60960,61494,63615,65064,65400,73026,97758,41285,54312,55389,58060,101358,
109945,4654,35360,48111,48855,57568,99631,512,53852,6712,7119,8330,11773,12720,17253,18774,20073,24292,
26862,46145,53940,66743,104201,104630,104905,104960,105724,105811,106620,106866,107752,108437) then ckd=1;
run;

data qdata.ckd_var1;
set qdata.ckd_var;
if ckd=. then delete;
run;

proc sort data=qdata.ckd_var1 out=qdata.ckd_var2;
  by patid descending clinicaldate;
  run;

data qdata.ckd_var3;
set qdata.ckd_var2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n38 as
select a.*, b.ckd
from qdata.copd_n37 a left join qdata.ckd_var3 b
on a.patid=b.patid;
quit;


data qdata.rheu_art;
set qdata.base_pred3;
if medcode in (844,6639,8350,21358,27603,28853,31054,31209,37431,44203,49227,56202,62401,70221,93715) 
then rheu_art=1;
run;

data qdata.rheu_art1;
set qdata.rheu_art;
if rheu_art=. then delete;
run;

proc sort data=qdata.rheu_art1 out=qdata.rheu_art2;
  by patid descending clinicaldate;
  run;

data qdata.rheu_art3;
set qdata.rheu_art2;
by patid;
if first.patid;
run;


data qdata.atrial_fib;
set qdata.base_pred3;
if medcode in (1268, 6345,9479,23437,35127,45773,96076,96277,108307,1757,84152,86416,93460,107472) 
then atrial_fib=1;
run;

data qdata.atrial_fib1;
set qdata.atrial_fib;
if atrial_fib=. then delete;
run;

proc sort data=qdata.atrial_fib1 out=qdata.atrial_fib2;
  by patid descending clinicaldate;
  run;

data qdata.atrial_fib3;
set qdata.atrial_fib2;
by patid;
if first.patid;
run;


proc sql;
create table qdata.copd_n39 as
select a.*, b.rheu_art
from qdata.copd_n38 a left join qdata.rheu_art3 b
on a.patid=b.patid;
quit;

proc sql;
create table qdata.copd_n40 as
select a.*, b.atrial_fib
from qdata.copd_n39 a left join qdata.atrial_fib3 b
on a.patid=b.patid;
quit;


data qdata.copd_n41;
set qdata.copd_n40;
if diabetes=. then diabetes=0;
if ckd=. then ckd=0;
if rheu_art=. then rheu_art=0;
if atrial_fib=. then atrial_fib=0;
run;


data qdata.migraine;
set qdata.base_pred3;
if medcode in (161,2424,2861,3220,3658,5029,5509,6433,9004,9633,11389,12511,14700,17762,27930,28031,28092,41497,
53813,103451,103502,103602,103973, 2554) then migraine=1;
run;

data qdata.migraine1;
set qdata.migraine;
if migraine=. then delete;
run;

proc sort data=qdata.migraine1 out=qdata.migraine2;
  by patid descending clinicaldate;
  run;

data qdata.migraine3;
set qdata.migraine2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n42 as
select a.*, b.migraine
from qdata.copd_n41 a left join qdata.migraine3 b
on a.patid=b.patid;
quit;

data qdata.copd_n43;
set qdata.copd_n42;
if migraine=. then migraine=0;
run;

data qdata.height;
set qdata.base_pred2;
if enttype in (14) then heights="status"; 
if heights=" " then delete;
if data1=" " then delete;
run;

proc sort data=qdata.height out=qdata.height1;
  by patid descending clinicaldate;
  run;

data qdata.height2;
set qdata.height1;
by patid;
if first.patid;
run;

data qdata.height3;
set qdata.height2;
height=data1;	
run;


data qdata.bmiheight;
set qdata.base_pred2;
if enttype in (13) then bmindex="status"; 
if bmindex=" " then delete;
if data1=" " then delete;
run;

proc sql;
create table qdata.bmiheight1 as
select a.*, b.height
from qdata.bmiheight a left join qdata.height3 b
on a.patid=b.patid;
quit;
run;

data qdata.bmi1a;
set qdata.bmiheight1;
if enttype in (13) then bmindex="status"; 
if data3=" " then data3=(data1/(height*height));
if bmindex=" " then delete;
if data3=" " or data3=> 204 or data3<=8 then delete;
run;

data qdata.bmi1b;
set qdata.bmi1a;
   data3a = round(data3,0.01);
run;

proc sort data=qdata.bmi1b out=qdata.bmi1;
  by patid descending clinicaldate;
  run;

data qdata.bmi2;
set qdata.bmi1;
by patid;
if first.patid;
run;

data qdata.bmi3;
set qdata.bmi2;
bmi=data3a;	
run;

proc sql;
create table qdata.copd_n44 as
select a.*, b.bmi
from qdata.copd_n43 a left join qdata.bmi3 b
on a.patid=b.patid;
quit;


data qdata.trt_hyper;
set qdata.copd_n7;
format copd_date ddmmyy10.;
where eventdate <= copd_date;
run;

data qdata.trt_hyper1;
set qdata.trt_hyper;
if prodcode in (51328, 18861, 9463, 40528, 37085, 46249, 504, 53896, 2680, 36840, 2968, 29560, 2967, 27137, 55368, 14495, 40899,
31220, 46795, 47264, 9697, 30967, 1296, 2362, 61116, 41639, 573, 71256, 64930, 2970, 38519, 70655, 58632, 36612, 40527, 64253,
43500, 47654, 63780, 63578, 45641, 13317, 59512, 32267, 71097, 29561, 63652, 28586, 15121, 33977, 42908, 52407, 709, 8026, 71025,
34768, 57658, 48214, 59972, 68372, 33894, 47998, 52399, 56850, 11983, 65936, 5800, 13589, 56505, 34544, 21231, 38308, 26995, 34382,
61270, 14960, 34453, 58294, 15096, 3310, 45264, 65273, 51807, 71491, 37930, 34652, 16708, 8106, 41417, 43563, 5861, 23252, 54986,
54345, 27871, 56510, 8105, 42894, 68094, 50780, 62860, 21943, 36742, 29627, 22439, 5275, 43413, 49164, 57333, 34798, 61694, 52010,
54544, 8268, 67269, 53915, 16196, 60823, 34953, 34937, 31587, 11937, 3839, 3720, 44527, 48049, 56079, 41573, 34505, 69600, 58874,
61499, 53058, 54288, 32241, 277, 46974, 34390, 69269, 60143, 34412, 41694, 11197, 56129, 45228, 66623, 12313, 38285, 66622, 59111, 
17120, 23642, 34657, 42081, 55896, 60757, 57073, 58843, 45816, 57048, 56162, 65985, 65416, 70709, 41522, 43566, 54620, 22708, 6408,
68247, 37087, 43012, 30039, 34539, 14478, 56148, 34712, 43507, 53719, 32560, 654, 35794, 52197, 37971, 17624, 50863, 633, 64877, 
6364, 52293, 71004, 9646, 43418, 14387, 19198, 57701, 68496, 45300, 72295, 29530, 20975, 16212, 65, 58461, 55299, 33811, 11987, 
1143, 34936, 56013, 59770, 56472, 43411, 71668, 82, 59603, 24041, 147, 43416, 4571, 58195, 7419, 54283, 62958, 78, 6807, 60349, 
16710, 60010, 58258, 66669, 34732, 43649, 46951, 65102, 67075, 64739, 24482, 6765, 34540, 71115, 7314, 69599, 72341, 1904, 29130, 
68480, 69192, 61292, 56509, 67194, 68021, 65536, 41743, 6806, 8830, 46957, 58451, 55639, 33057, 50509, 9693, 4103, 51433, 47159, 
60065, 57235, 65389, 17633, 12574, 70072, 43432, 80, 34719, 12815, 34431, 52499, 38510, 39227, 56279, 34799, 20188, 72038, 59788, 
46975, 60730, 30921, 66597, 45217, 15031, 5612, 41538, 31810, 63559, 1121, 55798, 56516, 45554, 32934, 16924, 6078, 34429, 31716, 
61117, 49491, 39421, 37080, 12412, 41532, 58863, 66895, 58871, 64055, 32857, 1144, 62564, 38034, 61262, 54037, 16701, 15605, 42902, 
448, 52882, 60232, 66162, 54298, 54512, 56763, 61693, 28127, 8800, 37965, 15085, 54941, 34357, 45340, 5735, 33095, 45324, 59699, 
69288, 9948, 50347, 53551, 34893, 63322, 65443, 33646, 34471, 15108, 35731, 51714, 63824, 3929, 63442, 63030, 18219, 71068, 44657,
46365, 54733, 40384, 28486, 53621, 62039, 56704, 6288, 61339, 62918, 66772, 56508, 57378, 45337, 41617, 42285, 34567, 34528, 71277,
34696, 18263, 10882, 34589, 1299, 5047, 21053, 12411, 56855, 37655, 3069, 53612, 41633, 59790, 47021, 57944, 39512, 54899, 39242, 
71040, 67741, 62036, 55456, 57864, 55002, 18269, 70994, 33336, 37778, 756, 25998, 34943, 61067, 50402, 13026, 59109, 8025, 48180, 
593, 56038, 71562, 54942, 60309, 196, 
34432,65599,64062,54928,32048,64602,19223,35007,28902,57346, 64902,34877,65570,72336,34698,58682,45938,19208,43813,57882, 
70667,72017,34562,34583,34952,34490,67789,53820,36753,69, 46890,66329,38854,61985,46979,37964,28820,58751,5159,65983, 761,33078,
97,23478,18325,34400,40355,38026,32597,28724,69074, 12858,15958,70916,42723,68381,43412,42901,55588,34651,57588, 59557,19204,
67307,50334,56506,32514,55903,48008,14477,16197, 66060,35302,56356,57801,41746,63594,45319,9731,69016,48053, 67719,28725,59996,
61133,51701,53271,39355,65749,6362,46851, 38899,56473,20849,71737,52088,70917,31307,67795,9915,6261, 68759,66558,59915,68192,6314,
60097,1807,34710,63010,13755, 56169,70955,60780,7338,47616,41232,6285,531,54414,53755, 67902,58274,4685,68948,16371,56970,58649, 
67929,47573,54049,54843,11348,57026,51601,61495,53833,71910, 54057,63411,52658,58201,3222,60076,5013,529,52972,65065, 52659,50185,
68603,11251,6217,17686,38395,54404,69667,58108, 13123,12836,5988,47727,37573,65094,59393,71096,40711,56606, 52208,61288,9745,54326,
57273,59903,55718,12874,6351,70628, 14965,31072,51117,72000,59340,51647,69858,70325,55017,54735, 65274,64888,71028,62388,60597,
68718,66958,5117,13821,63918, 58669,14983,9196,54740,58967,63337,50971,60506,57977,24359, 70431,55446,70805,69802,68340,1780,
63222,828,39199,57028, 68647,72215,17545,39944,66551,624,4226,60007,63717,57266, 63385,4155,48398,62415,6243,51186,59029,44778
,4818,66114, 16285,40571,64359,59448,53220,71019,575,6518,59750,70765, 52886,71080,55187,5723,49492,65228,61053,53680,71215,520,
20117,47006,14943,61754, 59351, 65479, 70251, 7043, 52427, 67663, 51368, 61781, 39786, 59802, 58910, 36939, 68751, 4741, 55296, 
62035, 59271, 1293, 18910, 61177, 11252, 51519, 66624, 45600, 6939, 59086, 56104, 4645, 51897, 61442, 59690, 49588, 62140, 46355, 
58646, 52559, 70455, 2971, 28949, 24228, 70961, 71342, 52017, 5181, 67074, 8975, 56467, 11966, 17338, 67890, 5513, 47027, 38964, 
66834, 45685, 3676, 46233, 31337, 21872, 27135, 17599, 70732, 12392, 26774, 13699, 66850, 38882, 26337, 18975, 2811, 47415, 11922, 
33025, 34377, 22217, 5296, 47331, 20257, 32590, 60620, 34581, 57531, 71413, 1262, 34522, 68499, 1130, 8310, 49762, 31761, 60856, 
30197, 7398, 61611, 66191, 3061, 34146, 662, 71702, 47732, 22619, 53500, 70827, 8558, 26309, 20311, 46724, 67662, 20459, 31489, 
66095, 15221, 46009, 4732, 66172, 737, 17666, 45759, 19457, 19013, 21918, 452, 47285, 69202, 29145, 25132, 5570, 18403, 15288, 
34975, 793, 4856, 3931, 72253, 24366,72321, 20890, 17640, 56767, 29044, 2605, 9374, 35343, 61422, 72199
35696, 32262, 48009, 59863, 13075, 66635, 43410, 53357, 13033, 13251
11973, 59001, 59163, 18852, 44192, 38634, 47222, 700, 25777, 53278
9485, 54696, 501, 71018, 2592, 33471, 11769, 57859, 5326, 14300
4308, 17448, 33932, 54654, 8213, 68531, 21886, 3370, 49001, 16328
27136, 39800, 58557, 8945, 6510, 27295, 41979, 48457, 5158, 42912
48282, 14861, 43818, 3711, 22241, 34607, 47529, 42210, 63246, 63515
59264, 3118, 43753, 38831, 39914, 35189, 749, 35646, 38434, 27401
12104, 68181, 62207, 9573, 22142, 70999, 11770, 43511, 491, 51261
3057, 26267, 34101, 41693, 5593, 34824, 1118, 29676, 69668, 1298
45308, 2521, 13302, 26270, 269, 10136, 40074, 13139, 55455, 8257, 64623, 56147, 13926, 35729, 29, 56994, 23233, 30199, 5194, 
47724, 25572, 31676, 21216, 65659, 38865, 12613, 9569, 37897, 35096, 69277, 34187, 27685, 5162, 11972, 43880, 28688, 28721, 
19129, 31336, 21795, 39171, 19426, 34959, 37774, 5234, 71939, 43790, 19459, 47217, 26460, 13410, 2528, 52276, 61532, 541, 6309,
26269, 1854, 1529, 49289, 729, 41635, 17425, 17406, 4635, 47230, 9750, 6856, 69028, 17325, 14305, 10267, 61374, 39357, 71009, 
35592, 62552, 68311, 69108, 3712, 8201, 62065, 64474, 4227, 16073, 2663, 64441, 2926, 13027, 51917, 21778, 42731, 20878, 13965,
35697, 1836, 63331, 2343, 3930, 12606, 52440, 26759, 66574, 636, 65349, 7280, 21763, 3343, 9334, 32917, 30462, 536, 54799, 57653, 
65636, 34247, 6477, 4239, 44859, 31490, 59762, 71030, 2746, 39804, 4408, 9708, 3302, 4852, 57444, 48288, 25646, 37530,65602, 38818, 
52701, 58580, 26265, 44887, 68221, 4808, 10688, 16038, 2686, 5054, 69206, 2453, 26463, 72221, 1120, 9386, 5477, 68429, 49636, 5277,
1748, 17586, 43394, 41679, 16677, 3917, 18834, 18038, 65504, 30242, 64719, 59585, 410, 66048, 10246, 55824, 8884, 46937, 40633,
4923, 38107, 64760, 45292, 1574, 1747, 1686, 41489, 40405, 16850, 3342, 13127, 41586, 70306, 23823, 49390, 36664, 5806, 25919, 
13856, 57594, 35329, 18379, 61230, 47002, 568, 43879, 46955, 34115, 34475, 57208, 21773, 64504, 38855, 1300, 67344, 45279, 36620, 
9410, 55306, 48272, 45070, 13672, 9670, 30557, 939, 15715, 32089, 10135, 9269, 19175, 64606, 18874, 69116, 48870, 47608, 71653, 
71, 59233, 11547, 19440, 47887, 30473, 32922, 37726, 32870, 60652, 24365, 32595, 61010, 53629, 64418, 33091, 43515, 7681, 47614, 
219, 61245, 20591, 66236, 64227, 7541, 38632, 16162, 55740, 12875, 10595, 72181, 43430, 38876, 11223, 3943, 66910, 23736, 67317,
17492, 54983, 4939, 43512, 30758, 30991, 11943, 68054, 62064, 30915, 64166 38545, 22019, 42625, 65745, 63917, 26674, 10153, 72049,
517, 5348 53990, 9723, 23805, 34093, 13243, 72091, 2888, 66817, 58990, 12639 39298, 1538, 12705, 58339, 42804, 42819, 38066, 60884,
1449, 39009 17557, 18830, 64917, 56469, 35084, 71353, 1995, 15652, 71339, 55257 9437, 37025, 51461, 64327, 1289, 21245, 19170, 
32658, 59098, 68828 62912, 60244, 3221, 17566, 67293, 71529, 43470, 13240, 56334, 18404 36202, 11777, 31737, 71601, 9553, 2280, 
49338, 63041, 69239, 64424 35317, 60699, 56758, 36583, 46887, 22696, 60415, 17342, 47530, 71969 43222, 71344, 66701, 23733, 60569,
66430, 31711, 64447, 9240, 7562 57680, 21145, 53868, 70990, 29637, 54515, 20642, 54633, 71760, 11512 46445, 37184, 5914, 47707, 
70509, 43516, 2612, 55259, 48099, 34551 56760, 26275, 26256, 49529, 54316, 40907, 56296, 42906, 7641, 48079 33083, 27957, 66517,
64907, 24190, 31820, 58, 67780, 7351, 70989 47844, 24189, 33415, 27689, 21867, 46302, 53812, 40149, 1209, 72083 7698, 40886, 
67738, 31670, 34803, 72042, 34059, 21803, 34602, 18973, 41517, 2, 67737, 27256 33651, 34124, 29991, 23427, 8526, 53033, 1294, 
119, 61066, 61283 47807, 4449, 53322, 5496, 63158, 493, 35603, 38461, 46066, 58276 46526, 51685, 45342, 65853, 61123, 34553, 
65159, 19193, 59209, 10088 59862, 50467, 582, 48150, 41543, 56145, 62019, 60319, 43695, 45328 37243, 40891, 7549, 64233, 69757,
25551, 8086, 33094, 68161, 755 57448, 45265, 69319, 57074, 34715, 34342, 34601, 25487, 55916, 20369 68022, 34625, 36023, 4802, 
40678, 57784, 72348, 36740, 62351, 63314 62158, 35272, 71405, 66065, 54785, 7547, 45583, 60200, 72346, 58325 55906, 19216, 45040,
5618, 47018, 787,17902, 69473, 57556, 58757, 23091, 54120, 41592, 71398, 11156, 43514, 32837, 29397, 11519, 35789, 46990, 12946, 
41660, 49388, 2142, 56274, 47687, 34296, 65582, 51652, 4960, 31529, 60343, 25494, 61025, 13264, 708, 19195, 53253, 71010, 58225, 
57104, 63309, 60660, 50079, 52970, 10214, 52366, 21911, 7991, 2389, 56536, 31219, 41706, 41074, 6815, 34908, 51720, 15052, 14109,
51933, 50370, 57933, 17950, 4161, 67913, 692, 58077, 46674, 7952, 65822, 45078, 56067,34347)
then trt_hyper=1;
run;

data qdata.trt_hyper2;
set qdata.trt_hyper1;
if trt_hyper=. then delete;
run;

proc sort data=qdata.trt_hyper2 out=qdata.trt_hyper3;
  by patid descending eventdate;
  run;

data qdata.trt_hyper4;
set qdata.trt_hyper3;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n45 as
select a.*, b.trt_hyper
from qdata.copd_n44 a left join qdata.trt_hyper4 b
on a.patid=b.patid;
quit;



data qdata.hypertension;
set qdata.base_pred3;
if medcode in (204, 245, 799, 1894, 2666, 3425, 3712, 3979, 4344, 4372, 4444, 4552, 4668, 5129, 5215, 5433, 5513, 7057, 7329, 7329, 8857, 9170,
10818, 10961, 10976, 11056, 12604, 12680, 12948, 13186, 13188, 15106, 15377, 15423, 16059, 16173, 16292, 16477, 16565, 18057, 18482, 18590,
18765, 19070, 19342, 20439, 20497, 21526, 21660, 21826, 21837, 22333, 22356, 24127, 25371, 26347, 26631, 27511, 27525, 27634, 28684, 28828,
28874, 29310, 30770, 30776, 30795, 31117, 31127, 31175, 31341, 31387, 31464, 31755, 31816, 32423, 32976, 34065, 34108, 34136, 34173, 34192,
34744, 35646, 35698, 36305, 37344, 38882, 39649, 40723, 41634, 41949, 42229, 42947, 43664, 43935, 44068, 44350, 44549, 44912, 45149, 47741,
51635, 52127, 52427, 52621, 53160, 54942, 55338, 55603, 57288, 57987, 59383, 60655, 61166, 61408, 61660, 62432, 62718, 63000, 63164, 63260,
63466, 63946, 64127, 65081, 66567, 67035, 67232, 68659, 69753, 70342, 71717, 71730, 72030, 72226, 72668, 73293, 73586, 73633, 83473, 89561,
90577, 90875, 92998, 93055, 93143, 94718, 95334, 95359, 96142, 96743, 97349, 97533, 97781, 98230, 99259, 102406, 102444, 102458, 103046, 103690,
105274, 105316, 105371, 105480, 105487, 105933, 105938, 105989, 106279, 107704, 108136, 108840, 109055, 109611, 109771, 109797, 109942, 110631,
112611, 101, 5020, 5341, 14643, 25553, 27534, 29390, 43719, 47932, 55411, 99234, 110087, 351, 676, 6598, 12519, 803, 351) 
then hypertension=1;
run;

data qdata.hypertension1;
set qdata.hypertension;
if hypertension=. then delete;
run;

proc sort data=qdata.hypertension1 out=qdata.hypertension2;
  by patid descending clinicaldate;
  run;

data qdata.hypertension3;
set qdata.hypertension2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n46 as
select a.*, b.hypertension
from qdata.copd_n45 a left join qdata.hypertension3 b
on a.patid=b.patid;
quit;

data qdata.copd_n47;
set qdata.copd_n46;
if hypertension=1 & trt_hyper=1 then trted_hyper=1;
drop trt_hyper hypertension;
if trted_hyper=. then trted_hyper=0; 
run;

data qdata.sle;
set qdata.base_pred3;
if medcode in (7871,11920,20007,29519,31564,36942,42719,44095,45726,47672,51798,58706,108072,57675) 
then sle=1;
run;

data qdata.sle1;
set qdata.sle;
if sle=. then delete;
run;

proc sort data=qdata.sle1 out=qdata.sle2;
  by patid descending clinicaldate;
  run;

data qdata.sle3;
set qdata.sle2;
by patid;
if first.patid;
run;


data qdata.mental;
set qdata.base_pred3;
if medcode in (694,2113,2114,3890,4500,6709,6710,8478,8766,11106,11244,11670,12771,12777,14965,16347,16905,
17607,17614,21455,21985,22080,23963,22188,23538,23835,24062,24171,24345,24387,26002,26143,26178,27770,27935,28168,28677,28863,29451,29651,29937,31633,31707,31738,31757,31877,
32159,32295,33410,33670,35274,37580,37764,39625,41022,47619,50218,51494,52678,52849,53848,54195,55829,56143,57993,61304,62222,63284,63701,
65811,67651,68111,68326,69155,101987,302,1362,1787,3630,4825,5611,6123,10945,11166,11265,12353,20514,21879,22359,22863,23642,24157,24927,
25128,27691,28373,28740,28962,30034,30191,30463,31638,31736,32052,32546,32588,32589,32602,32640,32820,32924,33493,33949,34174,34249,
34734,35200,35279,36143,36241,36337,37389,37568,37867,37887,37911,38429,38954,39016,39327,39412,39799,40224,40414,40500,40894,41317,
41950,42456,42520,42589,42886,43101,44299,44330,45133,45208,45309,45945,46504,46756,47335,47739,49509,49565,49566,49879,50069,50136,
50265,50302,50343,50606,50751,50947,50964,51268,51290,52602,52815,54179,54881,54983,55560,55678,55848,56144,56179,56504,56547,56577,
57199,57138,59009,60062,60473,60509,60676,60913,61342,61905,62706,62959,63273,63785,64210,65681,65826,66187,66383,67491,67535,69138,
70008,71196,73450,73954,86034,90276,93407,93419,97406,97561,97622,98618,100628,101519,101738,102582,103283,103991,106652,106673,106958,
107792,109465,111853,112529,576,854,1494,2117,3369,3984,6325,8407,9281,9422,10575,11055,11778,14747,15733,16764,17281,18053,19345,
20785,21595,21986,22104,23616,24107,25546,26859,30619,31362,31493,31950,32222,33338,33383,33693,33847,34236,34966,35848,35877,36172,
36720,37395,37681,38063,38371,39062,39316,40386,43405,43800,44498,48054,49420,49761,49852,50060,51322,51903,53032,53625,53985,54387,
56438,57376,57666,58532,58687,58716,58862,58866,59096,59285,60013,61098,61969,62449,63478,63867,64264,64533,64993,66410,66506,6713067768,
70884,71250,73295,91511,91547,92994,94001,94299,94604,96883,97919,99000,99070,99199,102311,102427,102446,104760,104763,107222,3702,4677,
4732,6874,8567,9521,14784,15923,16562,16808,24689,26299,27584,27890,27986,28277,31316,31535,33751,35607,35734,35738,36126,37296,44693,46434
49763,53840,55064,57465,57605,59011,63150,63583,63651,63698,
63784,68647,70721,72026,73423,104051,73924,103915,104065) then mental=1;
run;

data qdata.mental1;
set qdata.mental;
if mental=. then delete;
run;

proc sort data=qdata.mental1 out=qdata.mental2;
  by patid descending clinicaldate;
  run;

data qdata.mental3;
set qdata.mental2;
by patid;
if first.patid;
run;


data qdata.erect_dys;
set qdata.base_pred3;
if medcode in (3838,12066,12867,81439,102274,106360) 
then erect_dys=1;
run;

data qdata.erect_dys1;
set qdata.erect_dys;
if erect_dys=. then delete;
run;

proc sort data=qdata.erect_dys1 out=qdata.erect_dys2;
  by patid descending clinicaldate;
  run;

data qdata.erect_dys3;
set qdata.erect_dys2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n48 as
select a.*, b.sle
from qdata.copd_n47 a left join qdata.sle3 b
on a.patid=b.patid;
quit;

proc sql;
create table qdata.copd_n49 as
select a.*, b.erect_dys
from qdata.copd_n48 a left join qdata.erect_dys3 b
on a.patid=b.patid;
quit;

data qdata.copd_n50;
set qdata.copd_n49;
if sle=. then sle=0;
if erect_dys=. then erect_dys=0;
if gen_ethnicity="Unknown" then gen_ethnicity=" ";
run;


data qdata.test_base;
set qdata.test_pred2;
WHERE testdate<=copd_date;
run;

data qdata.hiv;
set qdata.test_base;
if enttype in (326) then hiv="ins"; 
run;

data qdata.hiv1;
set qdata.hiv;
if hiv in ("ins");
run;

data qdata.hiv2;
set qdata.hiv1;
if data1=" " then delete;
run;
																																								
data qdata.hiv3;
set qdata.hiv2;
if data1 in (17, 21, 24, 30, 31) then hiv_test="pos";
run;

data qdata.hiv4;
set qdata.hiv3;
if hiv_test=" " then delete;
run;

proc freq data=qdata.copd_n50;
table antipsycho;
run;

data qdata.copd_n50;
set qdata.copd_n49;
if sle=. then sle=0;
if erect_dys=. then erect_dys=0;
if gen_ethnicity="Unknown" then gen_ethnicity=" ";
run;

data qdata.bmi4;
set qdata.bmi3;
weight=input(data1, 8.);
heights=input(height, 8.);
run;

proc sql;
create table qdata.final_table as
select a.*, b.weight, b.heights
from qdata.copd_n50 a left join qdata.bmi4 b
on a.patid=b.patid;
quit;

data qdata.copd_n51;
set qdata.Final_table;
drop tfollow_chd total_fol;
run;

data qdata.copd_n52;
set qdata.copd_n51;
tfollow_chd = chd_date - copd_date;
if tfollow_chd=. then tfollow_chd = (regend - copd_date);
total_fol=tfollow_chd;
run;

proc freq data=qdata.copd_n55;
table ethnicity;
run;

data qdata.copd_n53;
set qdata.copd_n52;
drop gen_ethnicity diabetes smoke_status;
run;

proc sql;
create table qdata.copd_n54 as
select a.*, b.gen_ethnicity
from qdata.copd_n53 a left join qdata.hes_pts b
on a.patid=b.patid;
quit;
run;

data qdata.copd_n55;
set qdata.copd_n54;
if gen_ethnicity=" " then ethnicity="                     ";
if gen_ethnicity="Banglades" then ethnicity="Bangladeshi";
if gen_ethnicity="Bl_Afric" then ethnicity="Black_African";
if gen_ethnicity="Bl_Carib" then ethnicity="Black_Carib";
if gen_ethnicity="Bl_Other" then ethnicity="Other";
if gen_ethnicity="Chinese" then ethnicity="Chinese";
if gen_ethnicity="Indian" then ethnicity="Indian";
if gen_ethnicity="Mixed" then ethnicity="Other";
if gen_ethnicity="Oth_Asian" then ethnicity="Other_Asian";
if gen_ethnicity="Other" then ethnicity="Other_ethn_grp";
if gen_ethnicity="Pakistani" then ethnicity="Pakistani";
if gen_ethnicity="White" then ethnicity="white_or_not_stated";
if gen_ethnicity=" " then ethnicity="white_or_not_stated"; 
if gen_ethnicity="Unknown" then ethnicity="white_or_not_stated";
run;


data qdata.smok_status3;
set qdata.smok_status2;
if data1=2 then smoke = "non_smoker";
if data1=1 then smoke = "yes";
if data1=3 then smoke = "ex_smoker";
if smoke=" " then delete;
run;

data qdata.smok_status4;
set qdata.smok_status3;
if (10<=data2<20) then smoke_status="moderate_smoker";
else if (0<=data2<10) then smoke_status="light_smoker";
if data2 >= 20 then smoke_status="heavy_smoker";      
run;

data qdata.smok_status5;
set qdata.smok_status4;
if smoke_status=" " then smoke_status=smoke;
if smoke_status="yes" then smoke_status=" ";
run;

proc sql;
create table qdata.copd_n56 as
select a.*, b.smoke_status
from qdata.copd_n55 a left join qdata.smok_status5 b
on a.patid=b.patid;
quit;
run;


data qdata.diabe_type1_var;
set qdata.base_pred3;
if medcode in (17858, 24423, 21983, 49146, 51957, 38161, 41049, 46850, 45914, 42729, 17545, 18230, 1549, 12455, 47582, 47649,
42831, 44750, 49949, 18683, 18387, 35288, 40682, 46301, 10418, 39070, 49554, 18642, 54008, 30323, 30294, 10692, 40837, 22871,
55239, 109837, 61344, 62209, 62352, 95992, 62613, 96235, 63017, 66145, 66872, 68105, 69043, 69676, 69993, 91942, 108007, 91943,
93468, 97894, 98392, 99231, 99311, 102112, 102620, 108360, 108724, 109628, 60107, 61829, 97474, 68390, 97446, 70766, 110400, 102740)
then diabe_type1=1;
run;

data qdata.diabe_type1_var1;
set qdata.diabe_type1_var;
if diabe_type1=. then delete;
run;

proc sort data=qdata.diabe_type1_var1 out=qdata.diabe_type1_var2;
  by patid descending clinicaldate;
  run;

data qdata.diabe_type1_var3;
set qdata.diabe_type1_var2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n57 as
select a.*, b.diabe_type1
from qdata.copd_n56 a left join qdata.diabe_type1_var3 b
on a.patid=b.patid;
quit;


data qdata.diabe_type2_var;
set qdata.base_pred3;
if medcode in (758, 17859, 1407, 18278, 24836, 12640, 12736, 46150, 18143, 18777, 18209, 18219, 22884, 18264, 18390, 18425, 42762,
18496, 24458, 47315, 25041, 25591, 25627, 26054, 32627, 34268, 36633, 34450, 35385, 36695, 37806, 43227, 44779, 44982, 46917, 47321,
50527, 47409, 47816, 47954, 48192, 49074, 49655, 49869, 50225, 50813, 51756, 53392, 54899, 55075, 56268, 59725, 59991, 60796, 62107,
62674, 63690, 64571, 65267, 67905, 85991, 60699, 61071, 70316, 109197, 105784, 93727, 58604, 65704, 57278, 95351, 109103, 104639,
91646, 98723, 100964, 104323, 102201, 108005, 98616, 106061, 106528, 107701, 107824, 110611, 111798, 109865, 103902, 64668, 45913,
45919, 66965, 109103)
then diabe_type2=1;
run;

data qdata.diabe_type2_var1;
set qdata.diabe_type2_var;
if diabe_type2=. then delete;
run;

proc sort data=qdata.diabe_type2_var1 out=qdata.diabe_type2_var2;
  by patid descending clinicaldate;
  run;

data qdata.diabe_type2_var3;
set qdata.diabe_type2_var2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n58 as
select a.*, b.diabe_type2
from qdata.copd_n57 a left join qdata.diabe_type2_var3 b
on a.patid=b.patid;
quit;

proc freq data=qdata.copd_n58;
table diabe_type2 diabe_type1;
run;


data qdata.mental;
set qdata.base_pred3;
if medcode in (694,2113,2114,3890,4500,6709,6710,8478,8766,11106,11244,11670,12771,12777,14965,16347,16905,
17607,17614,21455,21985,22080,23963,22188,23538,23835,24062,24171,24345,24387,26002,26143,26178,27770,27935,28168,28677,28863,29451,29651,29937,31633,31707,31738,31757,31877,
32159,32295,33410,33670,35274,37580,37764,39625,41022,47619,50218,51494,52678,52849,53848,54195,55829,56143,57993,61304,62222,63284,63701,
65811,67651,68111,68326,69155,101987,302,1362,1787,3630,4825,5611,6123,10945,11166,11265,12353,20514,21879,22359,22863,23642,24157,24927,
25128,27691,28373,28740,28962,30034,30191,30463,31638,31736,32052,32546,32588,32589,32602,32640,32820,32924,33493,33949,34174,34249,
34734,35200,35279,36143,36241,36337,37389,37568,37867,37887,37911,38429,38954,39016,39327,39412,39799,40224,40414,40500,40894,41317,
41950,42456,42520,42589,42886,43101,44299,44330,45133,45208,45309,45945,46504,46756,47335,47739,49509,49565,49566,49879,50069,50136,
50265,50302,50343,50606,50751,50947,50964,51268,51290,52602,52815,54179,54881,54983,55560,55678,55848,56144,56179,56504,56547,56577,
57199,57138,59009,60062,60473,60509,60676,60913,61342,61905,62706,62959,63273,63785,64210,65681,65826,66187,66383,67491,67535,69138,
70008,71196,73450,73954,86034,90276,93407,93419,97406,97561,97622,98618,100628,101519,101738,102582,103283,103991,106652,106673,106958,
107792,109465,111853,112529,576,854,1494,2117,3369,3984,6325,8407,9281,9422,10575,11055,11778,14747,15733,16764,17281,18053,19345,
20785,21595,21986,22104,23616,24107,25546,26859,30619,31362,31493,31950,32222,33338,33383,33693,33847,34236,34966,35848,35877,36172,
36720,37395,37681,38063,38371,39062,39316,40386,43405,43800,44498,48054,49420,49761,49852,50060,51322,51903,53032,53625,53985,54387,
56438,57376,57666,58532,58687,58716,58862,58866,59096,59285,60013,61098,61969,62449,63478,63867,64264,64533,64993,66410,66506,6713067768,
70884,71250,73295,91511,91547,92994,94001,94299,94604,96883,97919,99000,99070,99199,102311,102427,102446,104760,104763,107222,3702,4677,
4732,6874,8567,9521,14784,15923,16562,16808,24689,26299,27584,27890,27986,28277,31316,31535,33751,35607,35734,35738,36126,37296,44693,46434
49763,53840,55064,57465,57605,59011,63150,63583,63651,63698,
63784,68647,70721,72026,73423,104051,73924,103915,104065) then sev_mental_ill=1;
run;

data qdata.mental1;
set qdata.mental;
if sev_mental_ill=. then delete;
run;

proc sort data=qdata.mental1 out=qdata.mental2;
  by patid descending clinicaldate;
  run;

data qdata.mental3;
set qdata.mental2;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n59 as
select a.*, b.sev_mental_ill
from qdata.copd_n58 a left join qdata.mental3 b
on a.patid=b.patid;
quit;

data qdata.final_table;
set qdata.copd_n59;
if diabe_type1=. then diabe_type1=0;
if diabe_type2=. then diabe_type2=0;
if sev_mental_ill=. then sev_mental_ill=0;
drop gen_ethnicity;
run;


data qdata.chd_m11;
set qdata.chd_m4;
if foundtotal >0 then delete;
if foundtotal <=0 then non_chd_rlt_mort=2;
run;

data qdata.chd_m12;
set qdata.chd_m11;
date_of_death=dod;
format date_of_death ddmmyy10.;
run;

proc sql;
create table qdata.copd_n60 as
select a.*, b.non_chd_rlt_mort
from qdata.final_table a left join qdata.chd_m12 b
on a.patid=b.patid;
quit;

data qdata.final_table;
set qdata.copd_n60;
if non_chd_rlt_mort=. then non_chd_rlt_mort=0;
if CHD=0 then CHD=non_chd_rlt_mort;
run;

proc freq data=qdata.copd_n61;
table sev_mental_ill diabe_type1 diabe_type2 smoke_status;
run;

proc print data=qdata.final_table;
    where patid = 11459718;
run;

proc print data=qdata.final_table;
where patid in (3824537, 5677406,19104107, 19898627);
var heights;
run;


data qdata.height1a;
set qdata.height1;
where patid in (3824537, 5677406,14140689,19104107, 19898627);
run;

proc sort data=qdata.height1a out=qdata.height1b;
  by patid clinicaldate;
  run;

data qdata.height1ba;
set qdata.height1b;
if data1="0" then delete;
run;

data qdata.height1c;
set qdata.height1ba;
by patid;
if first.patid;
run;


data qdata.height1d;
set qdata.height1c;
data_height=input(data1, 8.);
run;

proc sql;
create table qdata.copd_n62 as
select a.*, b.data_height
from qdata.final_table a left join qdata.height1d b
on a.patid=b.patid;
quit;

data qdata.copd_n63;
set qdata.copd_n62;
if patid in (3824537,14140689,19104107,19898627) then do;
  heights = data_height;
end;
run;

proc print data=qdata.copd_n64;
where patid in (3824537,19104107,14140689,19898627);
var heights;
run;

data qdata.copd_n64;
set qdata.copd_n63;
drop data_height;
run;

DATA qdata.final_table;
	SET qdata.final_table;
	fu_days = tfollow_chd*1;
RUN;

PROC SQL;
	SELECT COUNT(*) AS N, SUM(tfollow_chd)/365 AS FU, SUM(CHD) AS N_CHD, SUM(CHD)/(SUM(tfollow_chd)/365)*10000 AS Inc_CHD_10000 FROM qdata.Final_table;
QUIT;

DATA _temp1;
	SET Qdata.Final_table;
	IF patid_age<40 THEN age_group="<40    ";
	ELSE IF patid_age<45 THEN age_group="40-44";
	ELSE IF patid_age<50 THEN age_group="45-50";
	ELSE IF patid_age<55 THEN age_group="50-55";
	ELSE IF patid_age<60 THEN age_group="55-60";
	ELSE IF patid_age<65 THEN age_group="60-65";
	ELSE IF patid_age<70 THEN age_group="65-70";
	ELSE IF patid_age<75 THEN age_group="70-75";
	ELSE IF patid_age<80 THEN age_group="75-80";
	ELSE IF patid_age<85 THEN age_group="80-85";
	ELSE age_group=">85    ";
RUN;
	
PROC SQL;
	SELECT age_group, gender, COUNT(*) AS N, SUM(tfollow_chd)/365 AS FU, SUM(CHD) AS N_CHD, SUM(CHD)/(SUM(tfollow_chd)/365)*10000 AS Inc_CHD_10000 FROM _temp1 GROUP BY age_group, gender;
QUIT;

PROC SQL;
	SELECT gender, COUNT(*) AS N, SUM(tfollow_chd)/365 AS FU, SUM(CHD) AS N_CHD, SUM(CHD)/(SUM(tfollow_chd)/365)*10000 AS Inc_CHD_10000 FROM qdata.Final_table group by gender;
QUIT;

proc freq data=qdata.final_table;
table sev_mental_ill diabe_type1 diabe_type2 smoke_status;
run;


data qdata.copd_n54a;
set qdata.copd_n54;
if gen_ethnicity=" " then ethnicity="                     ";
if gen_ethnicity="Banglades" then ethnicity="Bangladeshi";
if gen_ethnicity="Bl_Afric" then ethnicity="Black_African";
if gen_ethnicity="Bl_Carib" then ethnicity="Black_Carib";
if gen_ethnicity="Bl_Other" then ethnicity="Other";
if gen_ethnicity="Chinese" then ethnicity="Chinese";
if gen_ethnicity="Indian" then ethnicity="Indian";
if gen_ethnicity="Mixed" then ethnicity="Other";
if gen_ethnicity="Oth_Asian" then ethnicity="Other_Asian";
if gen_ethnicity="Pakistani" then ethnicity="Pakistani"; 
if gen_ethnicity="Unknown" then ethnicity="Other";
if gen_ethnicity="White" then ethnicity="white_or_not_stated";
if gen_ethnicity=" " then ethnicity="white_or_not_stated";
run;

data qdata.copd_n54b;
set qdata.copd_n54a;
if ethnicity=" " then ethnicity="white_or_not_stated";
run;

proc freq data=qdata.copd_n54b;
table ethnicity;
run;

data qdata.copd_n66;
set qdata.copd_n65;
drop ethnicity;
run;

proc sql;
create table qdata.final_table as
select a.*, b.ethnicity
from qdata.copd_n66 a left join qdata.copd_n54b b
on a.patid=b.patid;
quit;

proc freq data=qdata.final_table;
table CHD;
run;

data qdata.copd_n75;
set qdata.final_table;
drop FU_days;
end_date = chd_date;
if end_date=. then end_date=dod;
else if dod=. then end_date=regend;
run;

data qdata.copd_n76;
set qdata.copd_n75;
if dod=. then end_date=regend;
run;

data qdata.final_table;
set qdata.copd_n76;
FU_days = end_date - copd_date;
drop end_date;
run;

data qdata.drugs_erect;
set qdata.copd_n7;
format copd_date ddmmyy10.;
where eventdate <= copd_date;
run;

data qdata.drugs_erect1;
set qdata.drugs_erect;
if prodcode in (554 ,704 ,794 ,1255 ,1257 ,1452 ,1456 ,1611 ,1732 ,2270 ,2727 ,2847,
3066, 5023, 5064, 5066, 5702, 5794, 5881, 5928, 6015, 6133 6148, 6203,
6207, 6214, 6457, 6777, 6809, 7028, 7047, 7204, 65513, 65911, 66159, 
9527, 9531, 10850, 10938, 11113, 11141, 11518, 11520, 11673, 12545
12617, 14860, 15119, 15965, 17846, 18035, 18387, 18766, 18773, 19317
19764, 20541, 21862,66569, 67589, 68443,
22712, 23010, 23635, 24044, 24871, 24872, 25010, 25749, 26061, 27023
27137, 27259, 28026, 28410, 28518, 30967, 31362, 31865, 33426, 33583
35192, 37104, 37556, 38201, 39096, 39138, 39236, 39243, 39285, 39289
39837, 45641, 45776, 45844, 46249, 46795, 47264, 48761, 48762, 48764
48939, 49322, 49347, 49411, 49766, 50177, 50233, 50362, 50511, 50728
50735, 50912, 51015, 51068, 51086, 51169, 52195, 52369, 57369, 57411
57432, 57453, 57491, 58151, 58199, 58615, 59771, 60333, 60334, 60755
60847, 60886, 61012, 61184, 61297, 61337, 61597, 61844, 61931, 62049
62352, 62622, 62659, 62715, 62844, 63179, 63578, 63611, 64194, 64204)
then drugs_erect=1;
run;

data qdata.drugs_erect2;
set qdata.drugs_erect1;
if drugs_erect=. then delete;
run;

proc sort data=qdata.drugs_erect2 out=qdata.drugs_erect3;
  by patid descending eventdate;
  run;

data qdata.drugs_erect4;
set qdata.drugs_erect3;
by patid;
if first.patid;
run;

proc sql;
create table qdata.copd_n78 as
select a.*, b.drugs_erect
from qdata.final_table a left join qdata.drugs_erect4 b
on a.patid=b.patid;
quit;


data qdata.copd_n79;
set qdata.copd_n78;
if drugs_erect=. then drugs_erect=0;
if erect_dys=0 then erect_dys=drugs_erect; 
run;

data qdata.copd_n80;
set qdata.copd_n79;
drop drugs_erect;
run;

data qdata.final_table;
set qdata.copd_n80;
drop drugs_erect;
run;
