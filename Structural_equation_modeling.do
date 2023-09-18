*** 데이터 처리 ***
use "가족실태조사2020", clear
keep hid h10 h11 q1 q14 q15 q18_1-q18_8 q19 q19_1 q89

rename h10 age
rename h11 edu
rename q1 gender
rename q14 mstat
rename q15 child
forvalues i=1 (1) 8 {
	rename q18_`i' msat`i'
}
rename q19 comtime
rename q19_1 comsat
rename q89 hecon

recode msat2 (1=5) (2=4) (3=3) (4=2) (5=1), gen(msat2_r)
recode msat4 (1=5) (2=4) (3=3) (4=2) (5=1), gen(msat4_r)
recode msat5 (1=5) (2=4) (3=3) (4=2) (5=1), gen(msat5_r)
recode msat6 (1=5) (2=4) (3=3) (4=2) (5=1), gen(msat6_r)

order msat1 msat2_r msat3 msat4_r msat5_r msat6_r msat7 msat8, after(child)

egen msat=rowmean(msat1-msat8)
label var msat "배우자와의 관계(평균)"

sum age-msat

bysort hid: egen hecon_new=sum(hecon)
label var hecon_new "현재 가구경제상태"
label define hecon_l 1 "매우 나쁜 편이다" 2 "나쁜 편이다" 3 "보통이다" 4 "좋은 편이다" 5 "매우 좋은 편이다"
label values hecon_new hecon_l
drop hecon
rename hecon_new hecon

tab mstat
tab mstat, nol
keep if mstat==2

save practice_230414


*** 데이터 분석 ***
use practice_230414, clear

*** regression ***
reg msat hecon comtime comsat

*** path analysis by regression ***
reg comsat hecon comtime, b
reg msat hecon comtime comsat, b

*** sem 툴바 ***
sembuilder
db sem

*** path analysis by sem ***
sem (hecon -> comsat) (comtime -> comsat) (hecon -> msat) (comtime -> msat) (hecon -> msat)
sem (hecon -> comsat) (comtime -> comsat) (hecon -> msat) (comtime -> msat) (hecon -> msat), standardized
sem (hecon -> comsat) (comtime -> comsat) (hecon -> msat) (comtime -> msat) (hecon -> msat), standardized cov(hecon*comtime)
sem (hecon -> comsat) (comtime -> comsat) (hecon -> msat) (comtime -> msat) (hecon -> msat), standardized cov(hecon*comtime) nocaplatent
*** 데이터 처리 ***
use "아동종합실태조사2018", clear
keep v1 v9 v17 v53-v65 v130-v134 v135-v144

rename v1 id
rename v9 byear
rename v17 health
forvalues i=53 (1) 65 {
	local j=`i'-52
	rename v`i' dpr`j'
	label var dpr`j' "우울(`j')"
}
forvalues i=130 (1) 134 {
	local j=`i'-129
	rename v`i' exp`j'
	label var exp`j' "자기표현(`j')"
}
forvalues i=135 (1) 144 {
	local j=`i'-134
	rename v`i' esteem`j'
	label var esteem`j' "자아존중감(`j')"
}

recode exp2 exp3 (1=4) (2=3) (3=2) (4=1), gen(exp2_r exp3_r)
forvalues i=6 (1) 10 {
	recode esteem`i' (1=4) (2=3) (3=2) (4=1), gen(esteem`i'_r)
}

order exp1 exp2_r exp3_r exp4 exp5, after(dpr13)
order esteem6_r esteem7_r esteem8_r esteem9_r esteem10_r, after(esteem5)

egen dpr=rowmean(dpr1-dpr13)
egen exp=rowmean(exp1-exp5)
egen esteem=rowmean(esteem1-esteem10_r)

label var health "주관적 건강상태"
label var dpr "우울(평균)"
label var exp "자기표현(평균)"
label var esteem "자아존중감(평균)"

order dpr exp esteem, after(health)

sum byear-esteem10

save practice_230421


*** 데이터 분석 ***
use practice_230421, clear

*** 분석자료 확인 ***
codebook byear-esteem10
codebook byear-esteem10, compact

*** 상관분석 ***
pwcorr exp1-exp5, star(0.05)
spearman exp1-exp5, star(0.05)

*** 최초 요인 추출: 주요인분석 ***
factor esteem1-esteem10_r, pf

*** 최초 요인 추출: 주성분분석 ***
factor esteem1-esteem10_r, pcf
factor esteem1-esteem10_r, pcf factor(1)
factor esteem1-esteem10_r, pcf mineigen(1)

*** 요인 수의 결정 ***
factor esteem1-esteem10_r, pcf
scree, yline(1)

*** 요인회전 ***
qui factor esteem1-esteem10_r, pcf factor(2)

rotate, varimax blanks(.4)
estat common

rotate, promax blanks(.4)
estat common

scoreplot

*** 요인점수 ***
qui factor esteem1-esteem10_r, pcf factor(2)
qui rotate, promax blanks(.4)
predict f1 f2

su f1 f2

*** 요인분석의 적합도 검증 ***
estat kmo

*** 요인분석의 신뢰도 검증 ***
alpha esteem1-esteem10_r
alpha esteem1-esteem10_r, std


*** 주성분분석 ***
corr achvp01-achvp15
pca achvp01-achvp15
screeplot, graphregion(color(white)) ytitle("Eigenvalue") xtitle("Component") title("Eigenvalues by Component")
loadingplot, graphregion(color(white)) title("Variable Loading to Component 1 and 2")
estat kmo


*** 데이터 처리 ***
use "전국다문화가족실태조사2018", clear
keep v1 v2 v3 v12 v43-v45 v57-v60 v139 v159-v161

rename v1 hid
rename v2 pid
rename v3 mstat
rename v12 msat
forvalues i=43 (1) 45 {
	local j=`i'-42
	rename v`i' chrel`j'
}
forvalues i=57 (1) 60 {
	local j=`i'-56
	rename v`i' klang`j'
}
rename v139 work
forvalues i=159 (1) 161 {
	local j=`i'-158
	rename v`i' mlang`j'
}

tab1 msat-mlang3

recode msat (1=5) (2=4) (3=3) (4=2) (5=1), gen(msat_r)
recode chrel1 chrel2 chrel3 (1=5) (2=4) (3=3) (4=2) (5=1), gen(chrel1_r chrel2_r chrel3_r)
recode klang1 klang2 klang3 klang4 (1=5) (2=4) (3=3) (4=2) (5=1), gen(klang1_r klang2_r klang3_r klang4_r)
recode mlang1 mlang2 mlang3 (1=5) (2=4) (3=3) (4=2) (5=1), gen(mlang1_r mlang2_r mlang3_r)

label define msat_l 1 "전혀 많족하지 않는다" 2 "별로 만족하지 않는다" 3 "보통이다" 4 "약간 만족한다" 5 "매우 만족한다"
label define chrel_l 1 "전혀 그렇지 않다" 2 "그렇지 않은 편이다" 3 "보통이다" 4 "대체로 그렇다" 5 "매우 그렇다"
label define klang_l 1 "전혀 못한다" 2 "별로 못한다" 3 "보통이다" 4 "약간 잘한다" 5 "매우 잘한다"
label define mlang_l 1 "전혀 그렇지 않다" 2 "그렇지 않은 편이다" 3 "보통이다" 4 "대체로 그렇다" 5 "매우 그렇다"

label values msat msat_l
forvalues i=1 (1) 3 {
	label values chrel`i'_r chrel_l
}
forvalues i=1 (1) 4 {
	label values klang`i'_r klang_l
}
forvalues i=1 (1) 3 {
	label values mlang`i'_r chrel_l
}

keep if mstat==2 & chrel1<6
drop if mlang1==0|mlang2==0|mlang3==0

save practice_230428



*** 데이터 분석 ***
use practice_230428, clear

*** 모형 적합도 평가 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Chrel -> chrel1_r chrel2_r chrel3_r) (Klang -> msat_r) (Klang msat_r -> Chrel), standardized
estat summarize
estat gof, st(chi2 rmsea indices)
estat gof, st(all)

*** 모형 수정 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Chrel -> chrel1_r chrel2_r chrel3_r) (Klang -> msat_r) (Klang msat_r -> Chrel), standardized
estat gof, st(all)
estat mindices

sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Chrel -> chrel1_r chrel2_r chrel3_r) (Klang -> msat_r) (Klang msat_r -> Chrel), standardized cov(e.klang1_r*e.klang2_r)
estat gof, st(all)

*** 효과 해석 ***
qui sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Chrel -> chrel1_r chrel2_r chrel3_r) (Klang -> msat_r) (Klang msat_r -> Chrel), standardized cov(e.klang1_r*e.klang2_r)
estat teffects

*** 다집단분석 ***
*** 형태 동일성 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1 Mlang@1 Chrel@1) mean(Klang@0 Mlang@0 Chrel@0) group(work) ginvariant(none) cov(Klang*Mlang Klang*Chrel Mlang*Chrel e.klang1_r*e.klang2_r)
estat gof, st(chi2 rmsea indices)

*** 측정 동일성 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1 Mlang@1 Chrel@1) mean(Klang@0 Mlang@0 Chrel@0) group(work) ginvariant(mcoef) cov(Klang*Mlang Klang*Chrel Mlang*Chrel e.klang1_r*e.klang2_r)
estat gof, st(chi2 rmsea indices)

*** 절편 동일성 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1 Mlang@1 Chrel@1) mean(Klang@0 Mlang@0 Chrel@0) group(work) ginvariant(mcoef mcons) cov(Klang*Mlang Klang*Chrel Mlang*Chrel e.klang1_r*e.klang2_r)
estat gof, st(chi2 rmsea indices)

*** 분산 동일성 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1 Mlang@1 Chrel@1) mean(Klang@0 Mlang@0 Chrel@0) group(work) ginvariant(mcoef mcons merrvar) cov(Klang*Mlang Klang*Chrel Mlang*Chrel e.klang1_r*e.klang2_r)
estat gof, st(chi2 rmsea indices)

*** 잠재평균분석 ***
sem (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) group(work) ginvariant(mcoef mcons merrvar) cov(Klang*Mlang Klang*Chrel Mlang*Chrel e.klang1_r*e.klang2_r)
estat gof, st(chi2 rmsea indices)



*** 다집단분석: 집단 간 비교 ***
use practice_230505, clear

*** 측정 동일성 제약 ***
sem (Klang -> Mlang) (Klang -> Chrel) (Mlang -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)

*** 집단 간 등가제약 ***
sem (Klang -> Mlang) (Klang -> Chrel) (Mlang -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)
estimates store M1

sem (Klang@b1 -> Mlang) (Klang -> Chrel) (Mlang -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)
estimates store M2

sem (Klang -> Mlang) (Klang@b2 -> Chrel) (Mlang -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)
estimates store M3

sem (Klang -> Mlang) (Klang -> Chrel) (Mlang@b3 -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)
estimates store M4

sem (Klang@b1 -> Mlang) (Klang@b2 -> Chrel) (Mlang@b3 -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat gof, st(chi2 rmsea indices)
estimates store M5

lrtest M1 M2
lrtest M1 M3
lrtest M1 M4
lrtest M1 M5

*** 다집단 분석 ***
sem (Klang -> Mlang) (Klang -> Chrel) (Mlang -> Chrel) (Klang -> klang1_r klang2_r klang3_r klang4_r) (Mlang -> mlang1_r mlang2_r mlang3_r) (Chrel -> chrel1_r chrel2_r chrel3_r), standardized latent(Klang Mlang Chrel) variance(Klang@1) mean(Klang@0) group(work) showginvariant ginvariant(mcoef) cov(e.klang1_r*e.klang2_r e.klang3_r*e.klang4_r)
estat ggof
estat ginvariant



*** 자기회귀교차지연모형) ***
*** 10~12차년도 데이터 병합 ***
forvalues i=10 (1) 12 {
	local j=`i'+2007
	local k=`i'+7
	use pskc_w`i'_`j', clear
	keep N_ID EMt`k'shs001-EMt`k'shs004 FFt`k'shs001-FFt`k'shs004
	forvalues l=1 (1) 4 {
		rename EMt`k'shs00`l' mhpy`i'_`l'
		rename FFt`k'shs00`l' fhpy`i'_`l'
		label var mhpy`i'_`l' "모의 행복감(`i'차년도)_`l'"
		label var fhpy`i'_`l' "부의 행복감(`i'차년도)_`l'"
		drop if mhpy`i'_`l'>8
		drop if fhpy`i'_`l'>8
	}
	save pskc_w`i'
}

use pskc_w10, clear
merge 1:1 N_ID using pskc_w11
drop _merge
merge 1:1 N_ID using pskc_w12
drop _merge
save pskc_merge

*** 기본 모형 ***
sem (Mhpy10 -> Mhpy11 Fhpy11) (Fhpy10 -> Fhpy11 Mhpy11) (Mhpy11 -> Mhpy12 Fhpy12) (Fhpy11 -> Fhpy12 Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10 -> mhpy10_2) (Mhpy10 -> mhpy10_3) (Mhpy10 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11 -> mhpy11_2) (Mhpy11 -> mhpy11_3) (Mhpy11 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12 -> mhpy12_2) (Mhpy12 -> mhpy12_3) (Mhpy12 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M1

*** 측정 동일성 ***
sem (Mhpy10 -> Mhpy11 Fhpy11) (Fhpy10 -> Fhpy11 Mhpy11) (Mhpy11 -> Mhpy12 Fhpy12) (Fhpy11 -> Fhpy12 Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M2

lrtest M1 M2

sem (Mhpy10 -> Mhpy11 Fhpy11) (Fhpy10 -> Fhpy11 Mhpy11) (Mhpy11 -> Mhpy12 Fhpy12) (Fhpy11 -> Fhpy12 Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10@b1 -> fhpy10_2) (Fhpy10@b2 -> fhpy10_3) (Fhpy10@b3 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11@b1 -> fhpy11_2) (Fhpy11@b2 -> fhpy11_3) (Fhpy11@b3 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12@b1 -> fhpy12_2) (Fhpy12@b2 -> fhpy12_3) (Fhpy12@b3 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M3

lrtest M2 M3

*** 경로 동일성 ***
sem (Mhpy10@A -> Mhpy11) (Mhpy11@A -> Mhpy12) (Mhpy10 -> Fhpy11) (Mhpy11 -> Fhpy12) ///
(Fhpy10 -> Fhpy11) (Fhpy11 -> Fhpy12) (Fhpy10 -> Mhpy11) (Fhpy11 -> Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M4

lrtest M3 M4

sem (Mhpy10@A -> Mhpy11) (Mhpy11@A -> Mhpy12) (Mhpy10 -> Fhpy11) (Mhpy11 -> Fhpy12) ///
(Fhpy10@B -> Fhpy11) (Fhpy11@B -> Fhpy12) (Fhpy10 -> Mhpy11) (Fhpy11 -> Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M5

lrtest M4 M5

sem (Mhpy10@A -> Mhpy11) (Mhpy11@A -> Mhpy12) (Mhpy10@C -> Fhpy11) (Mhpy11@C -> Fhpy12) ///
(Fhpy10@B -> Fhpy11) (Fhpy11@B -> Fhpy12) (Fhpy10 -> Mhpy11) (Fhpy11 -> Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M6

lrtest M5 M6

sem (Mhpy10@A -> Mhpy11) (Mhpy11@A -> Mhpy12) (Mhpy10@C -> Fhpy11) (Mhpy11@C -> Fhpy12) ///
(Fhpy10@B -> Fhpy11) (Fhpy11@B -> Fhpy12) (Fhpy10@D -> Mhpy11) (Fhpy11@D -> Mhpy12) ///
(Mhpy10@1 -> mhpy10_1) (Mhpy10@a1 -> mhpy10_2) (Mhpy10@a2 -> mhpy10_3) (Mhpy10@a3 -> mhpy10_4) ///
(Mhpy11@1 -> mhpy11_1) (Mhpy11@a1 -> mhpy11_2) (Mhpy11@a2 -> mhpy11_3) (Mhpy11@a3 -> mhpy11_4) ///
(Mhpy12@1 -> mhpy12_1) (Mhpy12@a1 -> mhpy12_2) (Mhpy12@a2 -> mhpy12_3) (Mhpy12@a3 -> mhpy12_4) ///
(Fhpy10@1 -> fhpy10_1) (Fhpy10 -> fhpy10_2) (Fhpy10 -> fhpy10_3) (Fhpy10 -> fhpy10_4) ///
(Fhpy11@1 -> fhpy11_1) (Fhpy11 -> fhpy11_2) (Fhpy11 -> fhpy11_3) (Fhpy11 -> fhpy11_4) ///
(Fhpy12@1 -> fhpy12_1) (Fhpy12 -> fhpy12_2) (Fhpy12 -> fhpy12_3) (Fhpy12 -> fhpy12_4) ///
, standardized latent(Mhpy10 Mhpy11 Mhpy12 Fhpy10 Fhpy11 Fhpy12) cov(e.Mhpy11*e.Fhpy11 e.Mhpy12*e.Fhpy12 Fhpy10*Mhpy10)
estat gof, st(chi2 rmsea indices)
estimates store M7

lrtest M6 M7

*** 오차 분산 동일성 ***