library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)

# load CPI data from realtalk
# pulling in CPI-U-RS series, and also saving a copy to share
cpi_data <- realtalk::cpi_u_rs_annual

wb <- wb_workbook()

wb$add_worksheet(sheet = "cpi_data") $
  add_data(x = cpi_data)

###### go back and change to 2024 after benchmarking #####

# set base year to 2024
cpi2023 <- cpi_data$cpi_u_rs[cpi_data$year==2023]

#load in CPS ORG data
org_data <- load_cps("org", 1979:2024, year, orgwgt, wage, statefips, female, wbho, age, selfemp, selfinc) %>%
  # standard labor force and age restrictions, restrict to only Wisconsin (statefips code = 55)
  filter(age>=16, selfemp !=1, selfinc !=1, statefips == 55) %>% 
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages to 2024$
  mutate(realwage = wage * (cpi2023/cpi_u_rs))

wages_sex

wages_race

wages_sex_race


*by sex

 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year female)
 gen wbho = .


*by race/ethnicity
 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year wbho)
 gen female = .


*by sex & race/ethnicity
 binipolate realwage [pw=orgwgt], wide binsize(.25) p(50) collapsefun(gcollapse) by(year female wbho)

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


