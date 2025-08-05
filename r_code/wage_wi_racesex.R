# load CPI data from realtalk
# pulling in CPI-U-RS series, and also saving a copy to share
cpi_data <- realtalk::cpi_u_rs_annual

# set base year to 2024
cpi2024 <- cpi_data$cpi_u_rs[cpi_data$year==2024]

#load in CPS ORG data
cps_org <- load_cps("org", 1979:2024, year, orgwgt, wage, statefips, female, wbho, age, selfemp, emp, selfinc, cow1) %>%
  # standard labor force and age restrictions, restrict to only Wisconsin (statefips code = 55)
  filter(age >= 16, emp == 1, 
         case_when(year < 1989 ~ selfemp == 0,
                   year >= 1989 & year < 1994 ~ selfemp == 0 | selfinc == 0,
                   TRUE ~ cow1 <= 5)) %>% 
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages to 2024$
  mutate(realwage = wage * (cpi2024/cpi_u_rs))

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

wb$add_worksheet(sheet = "wages_sex") $
  add_data(x = wages_sex)

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

wb$add_worksheet(sheet = "wages_race") $
  add_data(x = wages_race)

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

wb$add_worksheet(sheet = "wages_race_sex") $
  add_data(x = wages_race_sex)
