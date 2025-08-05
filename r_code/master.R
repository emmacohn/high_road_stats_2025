library(tidyverse)
library(epiextractr)
library(epidatatools)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)

#set up workbook
wb <- wb_workbook()

# load CPI data from realtalk
# pulling in CPI-U-RS series, and also saving a copy to share
cpi_data <- realtalk::cpi_u_rs_annual

# set base year to 2024
cpi2024 <- cpi_data$cpi_u_rs[cpi_data$year==2024]

#run wages code
source("r_code/wage_wi_racesex.R")

#run educational attainment code
source("r_code/educ_wi.R")

#save workbook
wb_save(wb, "output/wisconsin_wages_2025.xlsx")