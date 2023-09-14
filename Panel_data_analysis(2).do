*** 패널데이터 병합 ***
forvalues i=1 (1) 3 {
	use KCYPS2018m1Yw`i', clear
	keep hid pid ygenderw`i' ypsy1a01w`i' ypsy1a02w`i' ypsy1a03w`i' ypsy1a04w`i' ypsy1a05w`i' yphy1a00w`i' yedu1a00w`i'
	gen id=hid*10+pid
	drop hid pid
	order id
	save KCYPSw`i'
}

use KCYPSw1, clear
forvalues i=2 (1) 3 {
	merge m:m id using KCYPSw`i'
	drop _m
}
save KCYPS_wide

use KCYPS_wide, clear
drop ygenderw2 ygenderw3
label var id "개인 ID"
label var ygenderw1 "학생 성별"
label define ygenderw1_l 1 "남학생" 2 "여학생"
label values ygenderw1 ygenderw1_l
label define ypsy 1 "전혀 그렇지 않다" 2 "그렇지 않은 편이다" 3 "그런 편이다" 4 "매우 그렇다"
label define yphy 1 "전혀 건강하지 않다" 2 "건강하지 못한 편이다" 3 "건강한 편이다" 4 "매우 건강하다"
label define yedu 1 "매우 불만족" 2 "불만족" 3 "보통" 4 "만족" 5 "매우 만족" 6 "잘 모르겠음"
forvalues i=1 (1) 3 {
	forvalues j=1 (1) 5 {
		label var ypsy1a0`j'w`i' "삶의 만족도(`j')_`i'차"
		label values ypsy1a0`j'w`i' ypsy
	}
	egen ypsyw`i'=rowmean(ypsy1a01w`i'-ypsy1a05w`i')
	label var ypsyw`i' "삶의 만족도(평균)_`i'차"
	label var yphy1a00w`i' "주관적 건강상태_`i'차"
	label values yphy1a00w`i' yphy
	label var yedu1a00w`i' "학교생활 만족도_`i'차"
	label values yedu1a00w`i' yedu
	drop if yedu1a00w`i'==6
}
order yphy1a00w1 yphy1a00w2 yphy1a00w3 yedu1a00w1 yedu1a00w2 yedu1a00w3 ypsyw1 ypsyw2 ypsyw3, after(ygenderw1)
save KCYPS_wide, replace



*** 패널데이터 구조 변환 ***
use KCYPS_wide, clear
reshape long yphy1a00w yedu1a00w ypsyw ypsy1a01w ypsy1a02w ypsy1a03w ypsy1a04w ypsy1a05w, i(id) j(wave)
label var wave "조사 시점"
label var yphy1a00w "주관적 건강상태"
label var yedu1a00w "학교생활 만족도"
label var ypsyw "삶의 만족도(평균)"
forvalues i=1 (1)5 {
	label var ypsy1a0`i'w "삶의만족도(`i')"
}
save KCYPS_long



*** 패널데이터 명령어 ***
use KCYPS_long, clear

*** 패널데이터 선언 ***
xtset id
xtset
xtset, clear

xtset id wave
xtset, clear

xtset id wave, yearly

*** 패널데이터 요약통계량 ***
xtdes

xtsum ygenderw1 yphy1a00w yedu1a00w ypsyw

xttab yedu1a00w

xttrans yedu1a00w, freq
xttrans ygenderw1, freq



*** 패널분석의 기초 ***
use KCYPS_long, clear
xtset id wave, yearly

*** OLS ***
reg ypsyw ygenderw1 yphy1a00w yedu1a00w

*** Pooled OLS with Cluster Robust Standard Error ***
reg ypsyw ygenderw1 yphy1a00w yedu1a00w, vce(cluster id)

*** Between Effects ***
xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, be

*** Fixed Effects ***
xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, fe

*** Radom Effects ***
xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, re

*** OLS, FE, RE 회귀계수 비교 ***
qui reg ypsyw ygenderw1 yphy1a00w yedu1a00w, vce(cluster id)
estimate store OLS
qui xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, fe
estimate store FE
qui xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, re
estimate store RE

estimate table OLS FE RE, b(%9.3f) se(%9.4f) stat(r2 N)
estimate table OLS FE RE, b(%9.3f) stat(r2 N) star(0.001 0.01 0.05)

estout OLS FE RE, cell(b(star fmt(3)) se(par fmt(4))) stat(N, fmt(%9.0g)) legend
estout OLS FE RE, cell(b(star fmt(3)) se(par fmt(4))) keep(yedu1a00w) stat(N, fmt(%9.0g)) legend

*** Pooled OLS vs. FE: F-test ***
xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, fe

*** Pooled OLS vs. RE: Breusch & Pagan LM test ***
xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, re
xttest0

*** FE vs. RE: Hausman test ***
qui xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, fe
estimate store fe
qui xtreg ypsyw ygenderw1 yphy1a00w yedu1a00w, re
estimate store re
hausman fe re