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
})

here()

# ==========  list data  ==========

rm(list=ls())

ADI_years <- list.files(here("data", "ADI_all-domains"))
  ADI_years

years_list <- trimws(ADI_years[-(length(ADI_years))] )   # 2022 doesn't have any health data
#years_list <- c("ADI_2020","ADI_2021")
  years_list

d_all <- tibble()
d_ADI_all <- tibble()

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
  
  
  # ==========  set up ADI ==========
  
  ## Get total population
  pop_total <- sum(d_claims$pop)
  print(pop_total)
  
  ## ---------- Claimants
  # For employment, the domain score is the number of individuals claiming benefits principally for the reason of being unemployed... 
  # This is the sum of the number of people claiming Jobseeker’s Allowance plus those who claim Universal Credit and are required to seek work and be available for work
  
  d_claims_select <- d_claims |> 
    select(
      area_code, area_name, pop,
      rate_claims = claimant_rate,
      cases_claims = claimant_count
    )
  
  ## ---------- Crime
  # For crime, the domain score is the sum of criminal offences in the relevant year... In turn, these can be broken down into 
  # anti-social behaviour offences, bicycle thefts, burglaries, criminal damage and arson offences, 
  # drug offences, theft, possession of weapons, public order offences, robbery, shoplifting, vehicle crime, 
  # and violence and sexual offences 
  
  d_crime_select <- d_crime |>
    rowwise(area_code, area_name, pop) |>
    summarise(
      rate_crime = sum(c_across(ends_with("_rate"))),
      cases_crime = sum(c_across(!ends_with("_rate"))),
    ) |>
    select(area_code, area_name, pop, rate_crime, cases_crime)
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
      rate_health = DEP_prevalence_rate + MH_prevalence_rate,
      cases_health = DEP_afflicted + MH_afflicted
    ) |>
    select(area_code, area_name, pop, rate_health, cases_health)
  
  ## ---------- ADI
  
  d_ADI <- d_claims_select |> 
    full_join(d_crime_select, by = c("area_code", "area_name", "pop")) |>
    full_join(d_health_select,  by = c("area_code", "area_name", "pop"))
  
  d_ADI <- d_ADI |>
    mutate(
      ADI_cases = cases_claims + cases_crime + cases_health,    # issue here with NA! doesn't add up, so ignores the cases in claims etc. but where health/crime is missing
      ADI_rate = rate_claims + rate_crime + rate_health,   # can add rates as population is same for each denominator (i.e., pop of the district)
      ADI_cases_tot = sum(ADI_cases),
      weight_pop_lsoa = pop/pop_total,
      weight_cases_lsoa = ADI_cases/ADI_cases_tot, 
      year = year
    )
  
  glimpse(d_ADI)
  
  # To get district, remove the last 4 numbers (and space) from the end of district name (first check that the 4 letters uniquely identifies districts) 
  # and then group by district name
  
  d_ADI_all <- bind_rows(d_ADI_all, d_ADI)
  
  # ==========  Gini ==========
  
  pop_total <- sum(d_ADI$pop)
  pop_total
  ADI_cases_total <- sum(d_ADI$ADI_cases)
  ADI_cases_total

  range(d_ADI$ADI_rate)
  
  d_ADI <- d_ADI |> mutate(ADI_rate_inverse = 1/ADI_rate)
  
  # ----------  Gini coefficient
  
  # Number of bins to break Lorenz curve into
  n_bins <- 10
  
  # x-axis: rank LSOAs by ADI rate: cumulative (adding up to total number of people), normalised (by byson) rank of ADI rates (i.e., rank of numer of cases by byson)
  d_gini <- d_ADI |> 
    arrange(desc(ADI_rate)) |>
    mutate(
      # Break up into chunks of population
      pop_cumsum = cumsum(pop),
      pop_bins = cut_interval(pop_cumsum, n = n_bins, labels = FALSE),   # Break into bins
      # OR: cut(pop_cumsum, breaks = 100, labels = FALSE)
    )
  
  d_gini |> count(pop_bins)
  
  #plot(d_ADI$ADI_rate, d_ADI$ADI_cases)

  # Get overall factors (case load and rates) by group / bin
  d_gini_sum <- d_gini |> 
    group_by(pop_bins) |> 
    summarise(       
      cases_group_claims = sum(cases_claims, na.rm = TRUE),
      cases_group_crime = sum(cases_crime, na.rm = TRUE),
      cases_group_health = sum(cases_health, na.rm = TRUE),
      
      cases_group = cases_group_claims + cases_group_crime + cases_group_health,
      #cases_group = sum(ADI_cases, na.rm = TRUE),
      
      pop_group = sum(pop, na.rm = TRUE)
      ) |>
    ungroup()
  
  sum(d_gini_sum$cases_group)
  ADI_cases_total
  
  #plot(d_gini_sum$pop_bins, d_gini_sum$cases_group)
  
  # Get Lorenz curve values in dataframe (note cumulative values)
    ineq::Lc(d_gini_sum$cases_group)
  
  d_lc <-   tibble(
    lc_p = ineq::Lc(d_gini_sum$cases_group)$p,   # bycentages (of population by group)
    lc_L = ineq::Lc(d_gini_sum$cases_group)$L,   # values of ordindary Lorenz curve: normalised (divided by total cases), cumulative sum of cases
    lc_g = ineq::Lc(d_gini_sum$cases_group)$L.general*10,   # values of generalised Lorenz curve: non-normalized, cumulative sum of cases
    
    # claims only
    lc_p_claims = ineq::Lc(d_gini_sum$cases_group_claims)$p,   
    lc_L_claims = ineq::Lc(d_gini_sum$cases_group_claims)$L,   
    lc_g_claims = ineq::Lc(d_gini_sum$cases_group_claims)$L.general*10,
    
    # crime only 
    lc_p_crime = ineq::Lc(d_gini_sum$cases_group_crime)$p,   
    lc_L_crime = ineq::Lc(d_gini_sum$cases_group_crime)$L,   
    lc_g_crime = ineq::Lc(d_gini_sum$cases_group_crime)$L.general*10,
    
    # health only
    lc_p_health = ineq::Lc(d_gini_sum$cases_group_health)$p,   
    lc_L_health = ineq::Lc(d_gini_sum$cases_group_health)$L,   
    lc_g_health = ineq::Lc(d_gini_sum$cases_group_health)$L.general*10,
    
  )
  
  d_lc <- bind_cols(d_lc,
                    d_gini_sum |> 
                      add_row(.before = 1) |>   
                      mutate( 
                        across(everything(), ~replace_na(.x, 0)),
                        pop_bins = as.factor(pop_bins),
                      )
  )
    max(d_lc$lc_g)
    sum(d_lc$cases_group)
    ADI_cases_total
  
  # Add data identifier: year
  
  d_lc <- d_lc |> mutate(year = year)
  
  d_all <- bind_rows(d_all, d_lc)
  
} 

  glimpse(d_all)
  glimpse(d_ADI_all)
  # View(d_all)

# ==========  write data output  ==========
write_csv(d_all, here("output", "d_deciles.csv"))
write_csv(d_ADI_all, here("output", "d_full.csv"))


# ==========  END  ==========