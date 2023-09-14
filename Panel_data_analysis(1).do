*** 데이터 병합 ***
*** 케이스 추가: append ***
use pskc_w1_2008, clear
keep N_ID EMt08prs001-EMt08prs010 EMt08dpr013-EMt08dpr018
keep in 1/10
save practice_1

use pskc_w1_2008, clear
keep N_ID EMt08prs001-EMt08prs010 EMt08dpr013-EMt08dpr018
keep in 11/20
save practice_2

use practice_1, clear
append using practice_2

*** 변수 추가: merge ***
use pskc_w1_2008, clear
keep N_ID EMt08prs001-EMt08prs010 EMt08dpr013-EMt08dpr018
tab EMt08prs001
tab EMt08prs001, nol
forvalues i=1 (1) 9 {
	keep if EMt08prs00`i' <= 5
}
keep if EMt08prs010 <= 5
tab EMt08dpr013
tab EMt08dpr013, nol
forvalues i=13 (1) 18 {
	keep if EMt08dpr0`i' <= 5
}
egen prs_w1=rowmean(EMt08prs001-EMt08prs010)
egen depr_w1=rowmean(EMt08dpr013-EMt08dpr018)
label var prs_w1 "모의 양육 스트레스(평균)_1차년도"
label var depr_w1 "모의 우울(평균)_1차년도"
drop EMt08prs001-EMt08dpr018
save practice_w1

use pskc_w2_2009, clear
keep N_ID EMt09prs001-EMt09prs010 EMt09dpr013-EMt09dpr018
tab EMt09prs001
tab EMt09prs001, nol
forvalues i=1 (1) 9 {
	keep if EMt09prs00`i' <= 5
}
keep if EMt09prs010 <= 5
tab EMt09dpr013
tab EMt09dpr013, nol
forvalues i=13 (1) 18 {
	keep if EMt09dpr0`i' <= 5
}
egen prs_w2=rowmean(EMt09prs001-EMt09prs010)
egen depr_w2=rowmean(EMt09dpr013-EMt09dpr018)
label var prs_w2 "모의 양육 스트레스(평균)_2차년도"
label var depr_w2 "모의 우울(평균)_2차년도"
drop EMt09prs001-EMt09dpr018
save practice_w2

use practice_w1, clear
merge 1:1 N_ID using practice_w2
drop _merge
save practice_merge

*** 1~6차년도 데이터 병합 ***
use pskc_w3_2010, clear
keep N_ID EMt10prs001-EMt10prs011 EMt10dpr013-EMt10dpr018
tab EMt10prs001
tab EMt10prs001, nol
forvalues i=1 (1) 9 {
	keep if EMt10prs00`i' <= 5
}
keep if EMt10prs010 <= 5
keep if EMt10prs011 <= 5
tab EMt10dpr013
tab EMt10dpr013, nol
forvalues i=13 (1) 18 {
	keep if EMt10dpr0`i' <= 5
}
egen prs_w3=rowmean(EMt10prs001-EMt10prs011)
egen depr_w3=rowmean(EMt10dpr013-EMt10dpr018)
label var prs_w3 "모의 양육 스트레스(평균)_3차년도"
label var depr_w3 "모의 우울(평균)_3차년도"
drop EMt10prs001-EMt10dpr018
save practice_w3

forvalues i=4 (1) 6 {
	local j=`i'+2007
	local k=`i'-3
	use pskc_w`i'_`j', clear
	keep N_ID EMt1`k'prs001-EMt1`k'prs011 EMt1`k'dpr013-EMt1`k'dpr018
	forvalues l=1 (1) 9 {
		keep if EMt1`k'prs00`l' <= 5
	}
	keep if EMt1`k'prs010 <= 5
	keep if EMt1`k'prs011 <= 5
	forvalues l=13 (1) 18 {
		keep if EMt1`k'dpr0`l' <= 5
	}
	egen prs_w`i'=rowmean(EMt1`k'prs001-EMt1`k'prs011)
	egen depr_w`i'=rowmean(EMt1`k'dpr013-EMt1`k'dpr018)
	label var prs_w`i' "모의 양육 스트레스(평균)_`i'차년도"
	label var depr_w`i' "모의 우울(평균)_`i'차년도"
	drop EMt1`k'prs001-EMt1`k'dpr018
	save practice_w`i'
}

use practice_merge, clear
forvalues i=3 (1) 6 {
	merge 1:1 N_ID using practice_w`i'
	drop _merge
}
save practice_merge, replace



*** 패널 데이터 관리 ***
*** 변수 정렬 ***
use practice_merge, clear
move prs_w2 depr_w1
order prs_w1 prs_w2 prs_w3 prs_w4 prs_w5 prs_w6
order prs_w1 prs_w2 prs_w3 prs_w4 prs_w5 prs_w6, last
order prs_w1 prs_w2 prs_w3 prs_w4 prs_w5 prs_w6, before(depr_w1)
order prs_w1 prs_w2 prs_w3 prs_w4 prs_w5 prs_w6, after(depr_w6)
save practice_merge, replace

*** wide-form → long-form ***
use practice_merge, clear
reshape long depr_w prs_w, i(N_ID) j(wave)
save practice_long

*** long-form → wide-form ***
use practice_long, clear
reshape wide depr_w prs_w, i(N_ID) j(wave)
save practice_wide

*** 변수명 변경 ***
use practice_merge, clear
rename N_ID id
rename depr_w(#) depr(#)
rename prs_w(#) prs(#)



*** 패널 데이터를 활용한 분석 ***
*** 대응표본 t-검정 ***
use practice_merge, clear
ttest prs1 = prs2
ttest prs1 = prs6