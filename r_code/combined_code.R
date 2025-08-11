library(tidyverse)
library(epiextractr)
library(epidatatools)
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

#load in CPS ORG data
cps_org <- load_cps("org", 1979:2024, year, orgwgt, wage, statefips, female, wbho, age, 
                                      selfemp, emp, selfinc, cow1, educ, gradeatn) %>%
  # standard labor force and age restrictions, restrict to only Wisconsin (statefips code = 55)
  filter(age >= 16, emp == 1, statefips == 55,
         case_when(year < 1989 ~ selfemp == 0,
                   year >= 1989 & year < 1994 ~ selfemp == 0 | selfinc == 0,
                   TRUE ~ cow1 <= 5)) %>% 
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages to 2024$
  mutate(realwage = wage * (cpi2024/cpi_u_rs))

## Wages

#find median wages by sex
wages_sex <- cps_org |> 
  mutate(female = to_factor(female)) |>
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(female, year)) |>
  mutate(sex = case_when(female == "Female" ~ "Women", female == "Male" ~ "Men")) |>
  pivot_wider(id_cols = year, names_from = sex, values_from = wage_median)

#find median wages by race
wages_race <- cps_org |> 
  mutate(wbho = to_factor(wbho)) |>
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(wbho, year)) |>
  pivot_wider(id_cols = year, names_from = wbho, values_from = wage_median)

#find median wages by race and sex
wages_race_sex <- cps_org |> 
  mutate(wbho = to_factor(wbho)) |>
  mutate(female = to_factor(female)) |>
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(wbho, female, year)) |>
  unite(col = "demographic", c(wbho, female), na.rm = TRUE) |> 
  mutate(demographic = case_when(demographic == "White_Female" ~ "White Women", 
                                demographic == "White_Male" ~ "White Men",
                                demographic == "Black_Female" ~ "Black Women", 
                                demographic == "Black_Male" ~ "Black Men",
                                demographic == "Hispanic_Female" ~ "Hispanic Women", 
                                demographic == "Hispanic_Male" ~ "Hispanic Men",
                                demographic == "Other_Female" ~ "Other Women", 
                                demographic == "Other_Male" ~ "Other Men")) |>
  pivot_wider(id_cols = year, names_from = demographic, values_from = wage_median)

wage_demos <-  left_join(wages_sex, wages_race, by='year') |>
  left_join(wages_race_sex) |> 
  select(year, starts_with("White"), starts_with("Black"), starts_with("Hispanic"), Women, Men)

wb$add_worksheet(sheet = "Race and Sex median wages") $
  add_data(x = wage_demos)

## Sample sizes

#by sex
sample_sex <- cps_org |> 
  mutate(female = to_factor(female)) |>
  summarise(n=n(),
        .by=c(female, year)) |>
  mutate(sex = case_when(female == "Female" ~ "Women", female == "Male" ~ "Men")) |>
  pivot_wider(id_cols = year, names_from = sex, values_from = n)

#by race
sample_race <- cps_org |> 
  mutate(wbho = to_factor(wbho)) |>
  summarize(n=n(),
            .by = c(wbho, year)) |> 
  pivot_wider(id_cols = year, names_from = wbho, values_from = n)

#by race & sex
sample_race_sex <- cps_org |> 
  mutate(wbho = to_factor(wbho)) |>
  mutate(female = to_factor(female)) |>
  summarise(n=n(),
        .by=c(wbho, female, year)) |>
  unite(col = "demographic", c(wbho, female), na.rm = TRUE) |> 
  mutate(demographic = case_when(demographic == "White_Female" ~ "White Women", 
                                demographic == "White_Male" ~ "White Men",
                                demographic == "Black_Female" ~ "Black Women", 
                                demographic == "Black_Male" ~ "Black Men",
                                demographic == "Hispanic_Female" ~ "Hispanic Women", 
                                demographic == "Hispanic_Male" ~ "Hispanic Men",
                                demographic == "Other_Female" ~ "Other Women", 
                                demographic == "Other_Male" ~ "Other Men")) |>
  pivot_wider(id_cols = year, names_from = demographic, values_from = n)

sample_sizes <-  left_join(sample_sex, sample_race, by='year') |>
  left_join(sample_race_sex) |> 
  select(year, starts_with("White"), starts_with("Black"), starts_with("Hispanic"), Women, Men)
 

wb$add_worksheet(sheet = "Sample sizes") $
  add_data(x = sample_sizes)

#run educational attainment code
# the gradeatn variable is only available from 1992, so first we'll filter our data to exclude irrelevant years. 
new_cps_org <- filter(cps_org, year >= 1992)

#find median wages by single category Associate degree
wages_sing_assoc <- new_cps_org |> 
  mutate(educat = case_when(gradeatn %in% c(1,2,3,4,5,6,7,8) ~ "No HS diploma",
                          gradeatn == 9 ~ "HS graduate",
                          gradeatn == 10 ~ "Some college but no degree",
                          gradeatn == 11 | gradeatn == 12 ~ "Associate degree-all",
                          gradeatn %in% c(13,14,15,16) ~ "Bachelor's degree or more")) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(educat, year))

#find median wages by two category Associate degree
wages_two_assoc <- new_cps_org |> 
  mutate(educat = case_when(gradeatn %in% c(1,2,3,4,5,6,7,8) ~ "No HS diploma",
                          gradeatn == 9 ~ "HS graduate",
                          gradeatn == 10 ~ "Some college but no degree",
                          gradeatn == 11 ~ "Associate degree-occupational/vocational",
                          gradeatn == 12 ~ "Associate degree-academic program",
                          gradeatn %in% c(13,14,15,16) ~ "Bachelor's degree or more")) |> 
  summarise(
      wage_median = averaged_median(
        x = realwage, 
        w = orgwgt/12,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(educat, year))

wb$add_worksheet(sheet = "Education median wages") $
  add_data(x = wages_sing_assoc, start_col = 1) $
  add_data(x = wages_two_assoc, start_col = 8)

#save workbook
wb_save(wb, "output/wisconsin_wages_2025.xlsx")