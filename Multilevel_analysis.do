*** 데이터 전처리 ***
use "GEPS\1차년도_학교(중).dta", clear
keep SCHID Y1M_SH4 Y1M_SH6
rename SCHID schid
rename Y1M_SH4 region
rename Y1M_SH6 innsch
save geps_school_w1

use "GEPS\1차년도_학생(중).dta", clear
keep STUID SCHID Y1M_ST37 Y1M_ENG_S Y1M_MATH_S Y1M_KOR_S
rename STUID stuid
rename SCHID schid
rename Y1M_ST37 schsat_w1
rename Y1M_ENG_S escore_w1
rename Y1M_MATH_S mscore_w1
rename Y1M_KOR_S kscore_w1
drop if schsat_w1==-999 | escore_w1==-999 | mscore_w1==-999 | kscore==-999
save geps_student_w1



*** 2수준 데이터의 단일수준 분석: 통합 ***
use geps_school_w1, clear
merge 1:n schid using geps_student_w1
drop _merge
bysort schid: egen schsat_w1_m=mean(schsat_w1)
bysort schid: egen escore_w1_m=mean(escore_w1)
bysort schid: egen mscore_w1_m=mean(mscore_w1)
bysort schid: egen kscore_w1_m=mean(kscore_w1)
label var schsat_w1_m "학교만족도 학교 평균"
label var escore_w1_m "학업성취도 학교 평균_영어 원점수"
label var mscore_w1_m "학업성취도 학교 평균_수학 원점수"
label var kscore_w1_m "학업성취도 학교 평균_국어(서술형) 원점수"
bysort schid: gen index_n=_n
keep if index_n==1
drop stuid schsat_w1 escore_w1 mscore_w1 kscore_w1 index_n
save practice1

reg schsat_w1_m i.region i.innsch escore_w1_m mscore_w1_m kscore_w1_m



*** 2수준 데이터의 단일수준 분석: 분해 ***
use geps_student_w1, clear
merge n:1 schid using geps_school_w1
drop _merge
save practice2

reg schsat_w1 i.region i.innsch escore_w1 mscore_w1 kscore_w1



*** 다층모형 ***
use practice2, clear

*** 단일수준 모형 ***
reg schsat_w1, vce(cluster schid)

*** 확률절편모형: 무조건 모형 ***
mixed schsat_w1 || schid:, var
estat group

mixed schsat_w1 || schid:, nolog var
estat icc

*** 확률절편모형: 조건 모형 ***
mixed schsat_w1 escore_w1 mscore_w1 kscore_w1 || schid:, nolog var
mixed schsat_w1 escore_w1 mscore_w1 kscore_w1 i.region i.innsch || schid:, nolog var

*** 데이터 전처리 ***
use "gse2021\gse2021ym1_school.dta", clear
keep SCHID Q6_1 Q6_2
rename SCHID schid
rename Q6_1 policy1
rename Q6_2 policy2
label var schid "학교 id"
label var policy1 "혁신학교 여부"
label var policy2 "교육복지우선지원사업 여부"
save gse_school

use "gse2021\gse2021ym1_student.dta", clear
keep STUID SCHID Q28
rename STUID stuid
rename SCHID schid
rename Q28 schsat_stu
label var stuid "학생 id"
label var schid "학교 id"
label var schsat_stu "학교만족도(학생)"
save gse_student

use "gse2021\gse2021ym1_parent.dta", clear
keep STUID SCHID Q1_2 Q15
rename STUID stuid
rename SCHID schid
rename Q15 schsat_par
label var stuid "학생 id"
label var schid "학교 id"
label var schsat_par "학교만족도(학부모)"
keep if Q1_2==2
drop Q1_2
save gse_parent

use gse_student, clear
merge m:m stuid using gse_parent
keep if _merge==3
drop _merge
merge m:m schid using gse_school
drop _merge
save practice



*** 다층모형의 탐색 절차 ***
use practice, clear

*** bottom-up: ① 확률절편모형 ***
mixed schsat_stu || schid:, nolog
estat icc

*** bottom-up: ② 분산성분모형: 1수준 설명변인이 고정이 모형 ***
mixed schsat_stu schsat_par || schid:, nolog

*** bottom-up: ③ 분산성분모형: 2수준 설명변인이 고정이 모형 ***
mixed schsat_stu schsat_par i.policy1 || schid:, nolog
mixed schsat_stu schsat_par i.policy1 i.policy2 || schid:, nolog

*** bottom-up: ④ 확률계수모형 ***
mixed schsat_stu schsat_par i.policy1 i.policy2 || schid: schsat_par, nolog
mixed schsat_stu schsat_par i.policy1 i.policy2 || schid: schsat_par, nolog cov(un)

*** bottom-up: ⑤ 상호작용항을 포함한 모형 ***
mixed schsat_stu schsat_par i.policy1 i.policy2 c.schsat_par#i.policy2 || schid: schsat_par, nolog cov(un)



*** esttab 명령어의 활용 ***
use practice, clear

qui mixed schsat_stu || schid:, nolog
estimates store M1

qui mixed schsat_stu schsat_par || schid:, nolog
estimates store M2

qui mixed schsat_stu schsat_par i.policy1 i.policy2 || schid:, nolog
estimates store M3

qui mixed schsat_stu schsat_par i.policy1 i.policy2 || schid: schsat_par, nolog cov(un)
estimates store M4

qui mixed schsat_stu schsat_par i.policy1 i.policy2 c.schsat_par#i.policy2 || schid: schsat_par, nolog cov(un)
estimates store M5

esttab M1 M2 M3 M4 M5, star(* 0.05 ** 0.01 *** 0.001) se(%9.3f) mtitles(M1 M2 M3 M4 M5)



qui mixed schsat_stu || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M1

qui mixed schsat_stu schsat_par || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M2

qui mixed schsat_stu schsat_par i.policy1 i.policy2 || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M3

qui mixed schsat_stu schsat_par i.policy1 i.policy2 || schid: schsat_par, nolog cov(un)
estadd scalar v1=exp(2*_b[lns1_1_2:_cons])
estadd scalar v2=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estadd scalar cov=tanh(_b[atr1_1_1_2:_cons])*exp(_b[lns1_1_1:_cons])*exp(_b[lns1_1_2:_cons])
estimates store M4

qui mixed schsat_stu schsat_par i.policy1 i.policy2 c.schsat_par#i.policy2 || schid: schsat_par, nolog cov(un)
estadd scalar v1=exp(2*_b[lns1_1_2:_cons])
estadd scalar v2=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estadd scalar cov=tanh(_b[atr1_1_1_2:_cons])*exp(_b[lns1_1_1:_cons])*exp(_b[lns1_1_2:_cons])
estimates store M5

esttab M1 M2 M3 M4 M5, star(* 0.05 ** 0.01 *** 0.001) se(%9.3f) mtitles(M1 M2 M3 M4 M5) scalar(v1 v2 cov v_e) drop(lns1_1_1:_cons lns1_1_2:_cons lnsig_e:_cons atr1_1_1_2:_cons)



*** 다층모형의 평균 중심화 ***
use practice, clear

*** 전체평균 중심화 ***
egen schsat_par_GM=mean(schsat_par)
label var schsat_par_GM "학교만족도(학부모)_전체평균"
gen schsat_par_GMC=schsat_par-schsat_par_GM
label var schsat_par_GMC "학교만족도(학부모)_전체평균 중심화"

*** 집단평균 중심화 ***
bysort schid: egen schsat_par_gm=mean(schsat_par)
label var schsat_par_gm "학교만족도(학부모)_집단평균"
gen schsat_par_gmc=schsat_par-schsat_par_gm
label var schsat_par_gmc "학교만족도(학부모)_집단평균 중심화"

*** 모형 비교 ***
qui mixed schsat_stu schsat_par || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M1

qui mixed schsat_stu schsat_par_GMC || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M2

qui mixed schsat_stu schsat_par_gmc || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M3

esttab M1 M2 M3, star(* 0.05 ** 0.01 *** 0.001) se(%9.3f) mtitles(M1 M2 M3) scalar(v1 v_e) drop(lns1_1_1:_cons lnsig_e:_cons)



*** 조절효과 모형의 평균 중심화 ***
use practice, clear

egen schsat_par_mean=mean(schsat_par)
label var schsat_par_mean "학교만족도(학부모)_평균"
gen schsat_par_ms=schsat_par-schsat_par_mean
label var schsat_par_ms "학교만족도(학부모)_평균 중심화"

reg schsat_stu  schsat_par i.policy1 i.policy2 c.schsat_par#i.policy1 c.schsat_par#i.policy2
vif

reg schsat_stu  schsat_par_ms i.policy1 i.policy2 c.schsat_par_ms#i.policy1 c.schsat_par_ms#i.policy2
vif



*** 상호작용항의 평균 중심화 ***
use practice, clear

egen schsat_par_GM=mean(schsat_par)
label var schsat_par_GM "학교만족도(학부모)_전체평균"
gen schsat_par_GMC=schsat_par-schsat_par_GM
label var schsat_par_GMC "학교만족도(학부모)_전체평균 중심화"

recode policy1 (1=1) (2=0), gen(policy1_d)
label var policy1_d "혁신학교 여부(비운영)_운영"

recode policy2 (1=1) (2=0), gen(policy2_d)
label var policy2_d "교육복지우선지원사업 여부(비운영)_운영"

egen policy2_d_GM=mean(policy2_d)
label var policy2_d_GM "교육복지우선지원사업 여부_전체평균"
gen policy2_d_GMC=policy2_d-policy2_d_GM
label var policy2_d_GMC "교육복지우선지원사업 여부_전체평균 중심화"

*** 평균 중심화하지 않은 상호작용 모형 ***
qui mixed schsat_stu schsat_par policy1_d policy2_d || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M1

qui mixed schsat_stu schsat_par policy1_d policy2_d c.schsat_par#c.policy2_d || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M2

esttab M1 M2, star(* 0.05 ** 0.01 *** 0.001) se(%9.3f) mtitles(M1 M2) scalar(v1 v_e) drop(lns1_1_1:_cons lnsig_e:_cons)

*** 평균 중심화한 상호작용 모형 ***
qui mixed schsat_stu schsat_par_GMC policy1_d policy2_d_GMC || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M3

qui mixed schsat_stu schsat_par_GMC policy1_d policy2_d_GMC c.schsat_par_GMC#c.policy2_d_GMC || schid:, nolog
estadd scalar v1=exp(2*_b[lns1_1_1:_cons])
estadd scalar v_e=exp(2*_b[lnsig_e:_cons])
estimates store M4

esttab M3 M4, star(* 0.05 ** 0.01 *** 0.001) se(%9.3f) mtitles(M3 M4) scalar(v1 v_e) drop(lns1_1_1:_cons lnsig_e:_cons)