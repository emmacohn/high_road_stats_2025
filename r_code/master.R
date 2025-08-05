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

source("r_code/wage_wi_racesex.R")

wb_save(wb, "output/wisconsin_wages_2025.xlsx")