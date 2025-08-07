cps_org <- load_cps("org", 2019:2024, year, orgwgt, wage, statefips, female, wbho, age, selfemp, emp, selfinc, cow1, educ, gradeatn) %>%
  # standard labor force and age restrictions, restrict to only Wisconsin (statefips code = 55)
  filter(age>=16, selfemp !=1, selfinc !=1, statefips == 55) %>% 
    left_join(cpi_data, by='year') %>%
  # inflation adjust wages to 2024$
  mutate(realwage = wage * (cpi2024/cpi_u_rs))

#find median wages by single category Associate degree
wages_sing_assoc <- cps_org |> 
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
wages_two_assoc <- cps_org |> 
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

