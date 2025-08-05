*
*ADAPTED FROM MAROKEY SAWO WORK
*Portion of COWS TA Request
*Median wages in Wisconsin by race and sex, 1979-2020
*July 2022

/*
LAURA J DRESSER <ldresser@cows.org>: 
The first I think we’ve asked before. I’m pretty sure you wipe out data (“NA”) when sample size falls too far for specific subgroups. Unfortunately WI Black population is small enough that it often is withheld. And I do understand the danger and volatility of small sample size data. Living in a state with such huge Black/white disparity though, we generally included median wage by race and gender in order to attend to the important questions around racial disparity. We always present the series (not just a single number) going back to 1979 so there is some contextualization of the data, and some way of talking about volatility and sample but also showing the racial wage disparity. 
*/


*Utilizing JW's wage.do (https://github.com/Economic/swx_data/blob/master/code/wages.do) and sowo.do (\projects\jwolfe\ta_requests\sowo_2021\sowo.do)

clear all
set more off

local startyear 1979
local endyear 2023
local cpibase 2023

*pulling in CPI-U-RS series, and also saving a copy to share
* this pulls in a CPI file that is saved on an EPI server
sysuse cpi_annual, clear
keep year cpiurs
tempfile cpiurs
save `cpiurs'
export excel using wage_racesex_wi, sheet("cpiurs") sheetreplace firstrow(var)

*Loading CPS ORG data
load_epiextracts, begin(`startyear'm1) end(`endyear'm12) sample(org) keep(year orgwgt wage region division statefips female wbho educ union age selfemp selfinc)


*age, selfemp, and missing values restrictions
drop if age < 16
drop if selfemp == 1
drop if selfinc == 1


*Restricting to only Wisconsin
*statefips code: 55	WI
label list statefips
keep if statefips == 55


* inflation adjust wages (line 28 above anchors to 2020 dollars)
merge m:1 year using `cpiurs'
drop _merge
sum cpiurs if year == `cpibase'
local cpi_base = `r(mean)'
gen realwage = wage * `cpi_base'/cpiurs

*save /tmp/statewages_wi.dta, replace
tempfile statewages_wi
save `statewages_wi'

*by sex
 use `statewages_wi', clear
 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year female)
 gen wbho = .
 tempfile part1
 save `part1'

*by race/ethnicity
 use `statewages_wi', clear
 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year wbho)
 gen female = .
 tempfile part2
 save `part2'

*by sex & race/ethnicity
use `statewages_wi', clear
 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year female wbho)
 tempfile part3
 save `part3'

use `part1', clear
forvalues i = 2/3{
  append using `part`i''
}

*single demographic variable
label list wbho
label list female

gen demo = "."
replace demo = "male" if female == 0
replace demo = "female" if female == 1

replace demo = "white" if wbho == 1
replace demo = "black" if wbho == 2
replace demo = "hispanic" if wbho == 3
replace demo = "other" if wbho == 4

replace demo = "white male" if wbho == 1 & female == 0
replace demo = "white female" if wbho == 1 & female == 1
replace demo = "black male" if wbho == 2 & female == 0
replace demo = "black female" if wbho == 2 & female == 1
replace demo = "hispanic male" if wbho == 3 & female == 0
replace demo = "hispanic female" if wbho == 3 & female == 1
replace demo = "other male" if wbho == 4 & female == 0
replace demo = "other female" if wbho == 4 & female == 1

*drop percentile
rename realwage_binned median_wage
rename female gender
rename wbho race
drop if gender == . & race == .

drop gender race


tempfile statewages_wi_reshape
save `statewages_wi_reshape'

use `statewages_wi_reshape', clear
reshape wide median_wage, i(demo) j(year) //string
export excel using wage_racesex_wi, sheet("median_wages_wi") sheetreplace firstrow(var)



********Sample sizes*******************
use `statewages_wi', clear
des
misstable sum realwage wage
*32 obs=.

use `statewages_wi', clear

*Dropping "other"

collapse (count) sample = realwage, by(year female)
 tempfile parta
 save `parta'

 use `statewages_wi', clear
collapse (count) sample = realwage, by(year wbho)
tempfile partb
 save `partb'

 use `statewages_wi', clear
collapse (count) sample = realwage, by(year female wbho)
tempfile partc
 save `partc'

 use `parta', clear
 append using `partb'
 append using `partc'

 gen demo = "."
replace demo = "male" if female == 0
replace demo = "female" if female == 1

replace demo = "white" if wbho == 1
replace demo = "black" if wbho == 2
replace demo = "hispanic" if wbho == 3
replace demo = "other" if wbho == 4

replace demo = "white male" if wbho == 1 & female == 0
replace demo = "white female" if wbho == 1 & female == 1
replace demo = "black male" if wbho == 2 & female == 0
replace demo = "black female" if wbho == 2 & female == 1
replace demo = "hispanic male" if wbho == 3 & female == 0
replace demo = "hispanic female" if wbho == 3 & female == 1
replace demo = "other male" if wbho == 4 & female == 0
replace demo = "other female" if wbho == 4 & female == 1

drop female wbho

drop if year <1979

tempfile samples
save `samples'

use `samples', clear
reshape wide sample, i(demo) j(year) //string

export excel using wage_racesex_wi, sheet("sample_sizes2") sheetreplace firstrow(var)


