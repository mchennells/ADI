# Project:      ADI exploration
# File title:   1 -- Data setup
# Author:       Matt Chennells

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggplot2)
  library(microbenchmark)
  library(hrbrthemes)
  library(ineq)
  library(openxlsx)
  library(mice)
})

here()

# ==========  list data  ==========

rm(list=ls())

ADI_years <- list.files(here("data", "ADI_all-domains"))
  ADI_years

years_list <- trimws(ADI_years[-(length(ADI_years))] )   # 2022 doesn't have any health data
# years_list <- c("ADI_2020", "ADI_2021") 
  years_list

  
# ==========  Get ADI data; yearly and overall ==========
  
d_ADI_LSOA <- tibble()
d_ADI_AREA <- tibble()
  
for (year_file in years_list) {
  
  year <- str_sub(year_file, -4, -1)
  
  print(year)
  
  path_claimant_counts <- here("data", "ADI_all-domains", year_file, paste0("ADI_claimant_counts_", year,".csv"))
  path_crime <- here("data", "ADI_all-domains", year_file, paste0("ADI_crime_", year,".csv"))
  path_health <- here("data", "ADI_all-domains", year_file, paste0("ADI_health_", year,".csv"))
  
  print(head(path_claimant_counts))
  
  d_claims <- read_csv(file = path_claimant_counts)
  d_crime <- read_csv(file = path_crime)
  d_health <- read_csv(file = path_health)
  
  print(dim(d_claims))
  
  # ==============================  ADI dataset: LSOA ============================================================
  
  ## Get total population
  pop_total <- sum(d_claims$pop)
  print(pop_total)
  
  ## ---------- Claimants
  # For employment, the domain score is the number of individuals claiming benefits principally for the reason of being unemployed... 
  # This is the sum of the number of people claiming Jobseeker’s Allowance plus those who claim Universal Credit and are required to seek work and be available for work
  
  d_claims_select <- d_claims |> 
    select(
      area_code, area_name, pop,
      cases_claims = claimant_count,
      rate_claims = claimant_rate
    )
  
  ## ---------- Crime
  # For crime, the domain score is the sum of criminal offences in the relevant year... In turn, these can be broken down into 
  # anti-social behaviour offences, bicycle thefts, burglaries, criminal damage and arson offences, 
  # drug offences, theft, possession of weapons, public order offences, robbery, shoplifting, vehicle crime, 
  # and violence and sexual offences 
  
  d_crime_select <- d_crime |>
    rowwise(area_code, area_name, pop) |>
    summarise(
      cases_crime = sum(c_across(!ends_with("_rate")), na.rm = TRUE),
      rate_crime = sum(c_across(ends_with("_rate")), na.rm = TRUE),
      # cases_crime_2 = `anti-social_behaviour` + bicycle_theft + burglary + criminal_damage_and_arson + drugs + other_crime + other_theft + possession_of_weapons + public_order + 
      #   robbery + shoplifting + theft_from_the_person + vehicle_crime + violence_and_sexual_offences
    ) |>
      select(area_code, area_name, pop, cases_crime, rate_crime)
  # >> use microbenchmark to run tests on what's quicker: rowwise or using base R (see https://dplyr.tidyverse.org/articles/rowwise.html)
  
  ## ---------- Health
  # And for health, the domain score is the sum of registered cases of depression and other mental health illnesses
  
  glimpse(d_health)   # 32,844 rows
  
  d_health_select <- d_health |>
    select(
      area_code, area_name, pop,
      DEP_afflicted, DEP_prevalence_rate,   # depression
      MH_afflicted, MH_prevalence_rate   # mental health
    ) |>
    rowwise(area_code, area_name, pop) |>
    summarise(
      cases_health = round(DEP_afflicted + MH_afflicted, digits = 0),   # Rounded
      rate_health = DEP_prevalence_rate + MH_prevalence_rate
    ) |>
    select(area_code, area_name, pop, cases_health, rate_health)
  
  ## ------------------------------ join 3 datasets to get yearly dataset
  
  d_ADI_1 <- d_claims_select |> 
    full_join(d_crime_select, by = c("area_code", "area_name", "pop")) |>
    full_join(d_health_select,  by = c("area_code", "area_name", "pop"))

  d_ADI_2 <- d_ADI_1 |>
    mutate(
      cases_ADI_LSOA = cases_claims + cases_crime + cases_health,   # issue here with NA! doesn't add up, so ignores the cases in claims etc. but where health/crime is missing
      rate_ADI_LSOA = rate_claims + rate_crime + rate_health,   # can add rates as population is same for each denominator (i.e., pop of the district)
      year = year
    )
  
    sum(d_ADI_2$cases_ADI_LSOA)
  
  # To get district, remove the last 4 numbers (and space) from the end of district name (first check that the 4 letters uniquely identifies districts) 
  # and then group by district name
  
  d_ADI_LSOA <- bind_rows(d_ADI_LSOA, d_ADI_2)
  
  # Impute missing values
  d_ADI_LSOA_mice <- mice(d_ADI_LSOA)
  d_ADI_LSOA <- complete(d_ADI_LSOA_mice)
  
  
  # ==============================  ADI dataset: District ============================================================
  
  d_ADI_AREA <- d_ADI_LSOA |> 
    
    # identify district name
    mutate(
      district = str_sub(area_name, start = 1, end = -5),
    ) |>
    
    # aggregate areas BUT with ADI rate per area weighted by different LSOA pop x rates
    
    group_by(district, year) |>   # for each area, in each year ... 
    mutate(
      pop = sum(pop, na.rm = TRUE),   # total population of the AREA
      cases_ADI = sum(cases_ADI_LSOA, na.rm = TRUE),   # total cases by AREA
      rate_ADI = cases_ADI / pop,

      # # alternative method: get local population-weighted average of LSOA ADI rates (tiniest bit different to simpler approach above)
      # pop_weight_LSOA_of_AREA = pop / pop_AREA,   # weight of LSOA population of AREA population
      # ADI_rate_weight_of_LSOA = ADI_rate * pop_weight_LSOA_of_AREA,   # get LSOA ADI rate weighted by LSOA population weight of area
      # ADI_rate_AREA = sum(ADI_rate_weight_of_LSOA, na.rm = TRUE),   # get AREA ADI rate; some areas missing information on some categories; not many, but make sure to check
      # # area_ADI_rate_dot = ADI_rate %*% pop_area_weight, # dot product calculation leaves a strange column heading
      
      # claims
      cases_claims = sum(cases_claims, na.rm = TRUE),
      rate_claims = cases_claims / pop,
      
      # crime
      cases_crime = sum(cases_crime, na.rm = TRUE),
      rate_crime = cases_crime / pop,
      
      # health
      cases_health = sum(cases_health, na.rm = TRUE),
      rate_health = cases_health / pop,
      
    ) |>
    ungroup() |>
    select(
      district, pop, 
      cases_ADI, rate_ADI,
      cases_claims, rate_claims,
      cases_crime, rate_crime,
      cases_health, rate_health,
      year
    ) |>
    distinct()
    
}

  glimpse(d_ADI_LSOA)
  glimpse(d_ADI_AREA)

# Few enough districts to make them short enough  

d_ADI_AREA_long <- d_ADI_AREA |>
  pivot_longer(cols = -c(district, pop, year), 
               names_to = c(".value", "group"), 
               names_pattern ="(cases|rate)_(.*)"
  )

# ==========  write data output  ==========
  
write_csv(d_ADI_LSOA, here("output", "d_ADI_LSOA.csv"))
write_csv(d_ADI_AREA, here("output", "d_ADI_DISTRICT.csv"))
write_csv(d_ADI_AREA_long, here("output", "d_ADI_DISTRICT_long.csv"))

write.xlsx(d_ADI_LSOA, here("output", "d_ADI_LSOA.xlsx"))
write.xlsx(d_ADI_AREA, here("output", "d_ADI_DISTRICT_long.xlsx"))
write.xlsx(d_ADI_AREA_long, here("output", "d_ADI_DISTRICT_long.xlsx"))

# ==============================  END  ======================================================================