*Wisconsin median wages by 5 education categories: no hs, HS, some college, AA
* (both types?), Bachelors plus
*2024 update

clear all
set more off


local startyear 2019
local endyear 2023
local cpibase 2023

load_epiextracts, begin(`startyear'm1) end(`endyear'm12) sample(org) keep(year orgwgt wage ///
statefips female wbhao educ gradeatn union age selfemp selfinc)
*age and selfemp restrictions
keep if statefips == 55 //Wisconsin
drop if age < 16
drop if selfemp == 1
drop if selfinc == 1
gen pop = 1
*create 5 (6) education categories
gen educat = ""
replace educat = "No HS diploma" if (inlist(gradeatn,1,2,3,4,5,6,7,8)==1) 
replace educat = "HS graduate" if gradeatn == 9 
replace educat = "Some college but no degree" if gradeatn == 10
*replace educat = "Associate degree-all" if gradeatn == 11 | gradeatn == 12
replace educat = "Associate degree-occupational/vocational" if gradeatn == 11
replace educat = "Associate degree-academic program" if gradeatn == 12 
replace educat = "Bachelor's degree or more" if (inlist(gradeatn,13,14,15,16)==1)

tempfile data
save `data'

* inflate to real dollars
sysuse cpi_annual, clear
keep year cpiurs
tempfile cpiurs
save `cpiurs'

use `data', clear
merge m:1 year using `cpiurs'
drop _merge

sum cpiurs if year == `cpibase'
local cpi_base = `r(mean)'
gen rwage = wage * `cpi_base'/cpiurs

tempfile statewages
save `statewages'

 *by education
 use `statewages', clear
 binipolate rwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year educat)
 /*tempfile part1
 save `part1'
 use `statewages', clear
 binipolate rwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year educat female)
 tempfile part2
 save `part2'
 use `statewages', clear
 binipolate rwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year educat wbhao)
 tempfile part3
 save `part3'

use `part1', clear
append using `part2'
append using `part3'*/

tempfile educwages
save `educwages'

*counts and raw counts
use `statewages', clear
gcollapse ///
(sum) wsamp = pop (rawsum) samp = pop ///
[pw = orgwgt], by(year educat)
/*tempfile parta
save `parta'

use `statewages', clear
gcollapse ///
(sum) wsamp = pop (rawsum) samp = pop ///
[pw = orgwgt], by(year educat female)
tempfile partb
save `partb'

use `statewages', clear
gcollapse ///
(sum) wsamp = pop (rawsum) samp = pop ///
[pw = orgwgt], by(year educat wbhao)
tempfile partc
save `partc'

use `parta', clear
append using `partb'
append using `partc'*/
tempfile educounts
save `educounts'

* merge counts with wages
use `educwages', clear
merge 1:1 year educat using `educounts' //female wbhao
drop _merge

gen demo = ""
/*replace demo = "male" if female == 0
replace demo = "female" if female == 1
replace demo = "white" if wbhao == 1
replace demo = "black" if wbhao == 2
replace demo = "hispanic" if wbhao == 3
replace demo = "aapi" if wbhao == 4
replace demo = "other" if wbhao == 5*/
replace demo = "all" //if female == . & wbhao == .

*drop female wbhao
order year educat demo 

tempfile finaldata
save `finaldata'

*export
export excel using wi_edu_wages2.xlsx, sheet(educ) sheetreplace firstrow(var)

