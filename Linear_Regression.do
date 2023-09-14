*** Linear Regression(1) ***
use KCYPS2018e4Yw1, clear
keep HID PID ARA2Aw1 YGENDERw1 YTIM1A01w1-YTIM1B02w1 YINT1B00w1 YINT2A01w1-YINT2A16w1 YPSY4A01w1-YPSY4A07w1 YPSY4E01w1-YPSY4E10w1 YPHY2A00w1 YPHY2B00w1

*** 변수 설정 ***
rename ARA2Aw1 region
rename YGENDERw1 gender
rename YTIM1A01w1 btime1
rename YTIM1A02w1 btime2
rename YTIM1B01w1 wtime1
rename YTIM1B02w1 wtime2
rename YINT1B00w1 acsat
forvalues i=1 (1) 9 {
	rename YINT2A0`i'w1 acengmt`i'
}
forvalues i=10 (1) 16 {
	rename YINT2A`i'w1 acengmt`i'
}
forvalues i=1 (1) 7 {
	rename YPSY4A0`i'w1 attn`i'
}
forvalues i=1 (1) 9 {
	rename YPSY4E0`i'w1 drp`i'
}
rename YPSY4E10w1 drp10
rename YPHY2A00w1 height
rename YPHY2B00w1 weight

gen sleep=(btime1+24)*60+btime2-(wtime1*60+wtime2)
egen acengmt=rowmean(acengmt1-acengmt16)
egen attn=rowmean(attn1-attn7)
egen drp=rowmean(drp1-drp10)

label var sleep "평일 수면시간(분)"
label var acengmt "학업 열의(평균)"
label var attn "주의집중(평균)"
label var drp "우울(평균)"

recode acsat (6=.)
recode height (9999=.)
recode weight (9999=.)

*** 단순회귀분석 ***
reg weight height

*** 다중회귀분석 ***
reg acsat sleep acengmt attn
reg acsat sleep acengmt attn, beta

*** 다중공선성 진단 ***
reg acsat sleep acengmt attn
vif

qui reg acsat sleep acengmt attn
vif

*** 회귀분석표 만들기 ***
reg acsat sleep acengmt attn if gender==1
estimate store M1
reg acsat sleep acengmt attn if gender==2
estimate store M2
estimate table M1 M2, b(%9.2f) se(%9.2f) stat(r2 N)
estimate table M1 M2, b(%9.2f) stat(r2 N) star(0.001 0.01 0.05)
estimate table M1 M2, b(%9.2f) stat(r2 N) star(0.001 0.01 0.05) style(column)



*** Linear Regression(2) ***
use klowf08_p, clear
keep OPID P99AG P99ML01 PP99ED15 P07ML07B P08ST10 PP15CN P27AF01-P27AF08 W01MD01 H05FC 

*** 변수 설정 ***
rename P99AG age
rename P99ML01 msts
rename PP99ED15 edu
rename P07ML07B mhp
rename P08ST10 hhwsat
rename PP15CN chn
forvalues i=1 (1) 8 {
	rename P27AF0`i' famval`i'
}
rename W01MD01 jobsts
rename H05FC ecosts

drop if edu==-9
drop if mhp==-9
drop if hhwsat==-9

recode hhwsat (1=5) (2=4) (3=3) (4=2) (5=1), gen(hhwsat_r)
recode chn (0=0) (1/9=1), gen(chn_d)
recode famval2 famval4-famval8 (1=4) (2=3) (3=2) (4=1)
recode jobsts (1=1) (2/18=0), gen(jobsts_d)
recode ecosts (1=5) (2=4) (3=3) (4=2) (5=1), gen(ecosts_r)

egen famval=rowmean(famval1-famval8)

label var hhwsat_r "남편 가사노동분담에 대한 만족도(역코딩)"
label var chn_d "자녀 유무"
label var famval "가족 내 역할 인식(평균)"
label var jobsts_d "취업 여부"
label var ecosts_r "현재경제상태(역코딩)"

label define hhwsat_l 1 "전혀 만족하지 않는다" 2 "별로 만족하지 않는다" 3 "보통이다" 4 "대체로 만족한다" 5 "매우 만족한다"
label values hhwsat_r hhwsat_l
label define chn_l 0 "없음" 1 "있음"
label values chn_d chn_l
label define famval_l 1 "전혀 그렇지 않다" 2 "별로 그렇지 않다" 3 "조금 그렇다" 4 "매우 그렇다"
label values famval2 famval_l
label values famval4-famval8 famval_l
label define jobsts_l 0 "일 안했음" 1 "일 했음"
label values jobsts_d jobsts_l
label define ecosts_l 1 "매우 어렵다" 2 "조금 어려운 편이다" 3 "보통이다" 4 "여유가 있는 편이다" 5 "매우 여유가 있다"
label values ecosts_r ecosts_l

keep if msts==2

*** 표준화 회귀계수 ***
reg mhp age edu jobsts_d chn_d ecosts_r hhwsat_r famval
reg mhp age edu jobsts_d chn_d ecosts_r hhwsat_r famval, beta

*** 더미변수 생성 ***
tab chn, gen(chn_d)

xi i.chn, prefix(d)
xi i.chn, prefix(dum) noomit

*** 더미변수를 포함한 회귀분석 ***
xi: reg mhp i.chn_d hhwsat_r

char chn [omit] 1
xi: reg mhp i.chn_d hhwsat_r

reg mhp b1.chn_d hhwsat_r

qui reg mhp i.chn_d hhwsat_r
qui margins chn_d, at(hhwsat_r=(1(1)5)) noatlegend
marginsplot, noci recast(line)

reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval

*** 이상점 진단: 레버리지 ***
qui reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval
predict lev, leverage

scalar meanh=(2*7+2)/6214
gen lev_n=(lev>meanh)

gsort-lev
list OPID age famval lev in 1/5

lvr2plot

*** 잔차의 정규성 ***
qui reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval
predict ehat, resid
kdensity ehat, normal
swilk ehat

*** 잔차의 이분산성: 잔차 그래프 ***
qui reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval 
rvfplot, yline(0)
rvpplot famval, yline(0)

*** 잔차의 이분산성: Breusch-Pagan's test ***
qui reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval
estat hettest
estat hettest famval

*** 잔차의 이분산성: White's test ***
qui reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval
estat imtest, white

*** 잔차의 이분산성을 고려한 robust 회귀분석 ***
reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval
reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval, vce(robust)
reg mhp age edu i.jobsts_d i.chn_d ecosts_r hhwsat_r famval, r

*** 위계적 회귀분석 ***
reg mhp age edu jobsts_d chn_d ecosts_r
reg mhp age edu jobsts_d chn_d ecosts_r hhwsat_r famval

hireg mhp (age edu jobsts_d chn_d ecosts_r) (hhwsat_r famval)
hireg mhp (age edu jobsts_d chn_d ecosts_r) (hhwsat_r famval), r(beta)



*** Logistic Regression ***
use klowf08_p, clear
keep OPID P99AG P99ML01 PP99ED15 P07ML07B P08ST10 PP15CN P27AF01-P27AF08 P27AM01-P27AM04 W01MD01 H05FC 

*** 변수 설정 ***
rename P99AG age
rename P99ML01 msts
rename PP99ED15 edu
rename P07ML07B mhp
rename P08ST10 hhwsat
rename PP15CN chn
forvalues i=1 (1) 8 {
	rename P27AF0`i' famrole`i'
}
forvalues i=1 (1) 4 {
	rename P27AM0`i' famspt`i'
}
rename W01MD01 jobsts
rename H05FC ecosts

drop if edu==-9
drop if mhp==-9
drop if hhwsat==-9

recode hhwsat (1=5) (2=4) (3=3) (4=2) (5=1), gen(hhwsat_r)
recode chn (0=0) (1/9=1), gen(chn_d)
recode chn (0=0) (1=1) (2=2) (3/9=3), gen(chn_c)
recode famrole2 famrole4-famrole8 (1=4) (2=3) (3=2) (4=1)
recode famspt1-famspt4 (1=4) (2=3) (3=2) (4=1)
recode jobsts (1=1) (2/18=0), gen(jobsts_d)
recode ecosts (1=5) (2=4) (3=3) (4=2) (5=1), gen(ecosts_r)

egen famrole=rowmean(famrole1-famrole8)
egen famspt=rowmean(famspt1-famspt4)

label var hhwsat_r "남편 가사노동분담에 대한 만족도(역코딩)"
label var chn_d "자녀 유무"
label var chn_c "자녀 수(범주)"
label var famrole "가족 내 역할 인식(평균)"
label var famspt "가족부양 인식(평균)"
label var jobsts_d "취업 여부"
label var ecosts_r "현재경제상태(역코딩)"

label define hhwsat_l 1 "전혀 만족하지 않는다" 2 "별로 만족하지 않는다" 3 "보통이다" 4 "대체로 만족한다" 5 "매우 만족한다"
label values hhwsat_r hhwsat_l
label define chn_l 0 "없음" 1 "있음"
label values chn_d chn_l
label define chn_l2 0 "없음" 1 "1명" 2 "2명" 3 "3명 이상"
label values chn_c chn_l2
label define famval_l 1 "전혀 그렇지 않다" 2 "별로 그렇지 않다" 3 "조금 그렇다" 4 "매우 그렇다"
label values famrole2 famval_l
label values famrole4-famrole8 famval_l
label values famspt1-famspt4 famval_l
label define jobsts_l 0 "일 안했음" 1 "일 했음"
label values jobsts_d jobsts_l
label define ecosts_l 1 "매우 어렵다" 2 "조금 어려운 편이다" 3 "보통이다" 4 "여유가 있는 편이다" 5 "매우 여유가 있다"
label values ecosts_r ecosts_l

keep if msts==2

*** 로지스틱 회귀분석 ***
logit chn_d age edu jobsts_d ecosts_r famrole famspt hhwsat_r

*** 로지스틱 회귀분석: 오즈비 ***
logit chn_d age edu jobsts_d ecosts_r famrole famspt hhwsat_r, or
logistic chn_d age edu jobsts_d ecosts_r famrole famspt hhwsat_r

*** 로지스틱 회귀분석의 분류 정확도 ***
logistic chn_d age edu jobsts_d ecosts_r famrole famspt hhwsat_r
estat class

*** Hosmer & Lemeshow's test ***
logistic chn_d age edu jobsts_d ecosts_r famrole famspt hhwsat_r
estat gof, group(10) table

*** 다항 로지스틱 회귀분석 ***
mlogit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r
mlogit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r, rrr
mlogit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r, rrr base(1)

*** 순서형 로지스틱 회귀분석 ***
ologit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r

*** 평행성 검정 ***
net search oparallel

ologit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r
oparallel

net install spost13_ado, from("https://jslsoc.sitehost.iu.edu/stata/") replace

ologit chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r
brant, detail

*** 일반화 순서형 로지스틱 회귀분석 ***
gologit2 chn_c age edu jobsts_d ecosts_r famrole famspt hhwsat_r



*** 패키지 설치 오류 해결 ***
adopath
sysdir set PLUS "C:\stata"
