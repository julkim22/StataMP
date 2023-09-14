*** ANOVA ***
use KCYPS2018e4Yw1, clear
keep HID PID ARA2Aw1 YGENDERw1 YFAM1A01w1 YFAM1A02w1 YEDU1A00w1 YPHY1A00w1 YFAM2A01w1-YFAM2C03w1

*** 변수 설정 ***
rename ARA2Aw1 region
rename YGENDERw1 gender
rename YEDU1A00w1 schsat
rename YFAM1A01w1 withpar1
rename YFAM1A02w1 withpar2
forvalues i=1 (1) 4 {
	rename YFAM2A0`i'w1 patt1_`i'
	rename YFAM2B0`i'w1 patt2_`i'
}
forvalues i=1 (1) 3 {
	rename YFAM2C0`i'w1 patt3_`i'
}
rename YPHY1A00w1 health

label var schsat "학교생활 만족도"
label var withpar1 "(평일)부모와 함께 보내는 시간: 시 단위"
label var withpar2 "(평일)부모와 함께 보내는 시간: 분 단위"
forvalues i=1 (1) 4 {
	label var patt1_`i' "부모 양육태도: 따스함`i'"
	label var patt2_`i' "부모 양육태도: 거부`i'"
}
forvalues i=1 (1) 3 {
	label var patt3_`i' "부모 양육태도: 자율성 지지`i'"
}
label var health "주관적 건강상태"

gen withpar=withpar1*60+withpar2
egen patt1=rowmean(patt1_1-patt1_4)
egen patt2=rowmean(patt2_1-patt2_4)
egen patt3=rowmean(patt3_1-patt3_3)

label var withpar "(평일)부모와 함께 보내는 시간"
label var patt1 "부모 양육태도: 따스함"
label var patt2 "부모 양육태도: 거부"
label var patt3 "부모 양육태도: 자율성 지지"

drop withpar1-patt3_3

recode schsat (6=.)

*** 일원 분산분석 ***
oneway withpar region
oneway withpar region, tab

oneway health region, tab bonferroni
oneway health region, tab scheffe

*** 분산분석 ***
anova health region##gender, partial
table region gender, stat(mean health) nf(%9.2f)

*** 공분산분석 ***
anova health region c.schsat, partial
anova health region schsat, partial
