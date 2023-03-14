# Project:      ADI exploration
# File title:   Data setup
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

# ==========  read data  ==========

rm(list=ls())

list.files(here("data", "ADI_all-domains"))
list.files(here("data", "ADI_all-domains", "ADI_2021"))

path_claimant_counts_2021 <- here("data", "ADI_all-domains", "ADI_2021", "ADI_claimant_counts_2021.csv")
path_crime_2021 <- here("data", "ADI_all-domains", "ADI_2021", "ADI_crime_2021.csv")
path_health_2021 <- here("data", "ADI_all-domains", "ADI_2021", "ADI_health_2021.csv")

d_claims_21 <- read_csv(file = path_claimant_counts_2021)
d_crime_21 <- read_csv(file = path_crime_2021)
d_health_21 <- read_csv(file = path_health_2021)

# 1. Inequality measure across all LSOAs; then at higher level, to see whether within vs between inequality is greatest driver (e.g. is it within or between race inequality that's the biggeset driver?')
# 2. Changes in inequality over time
# 3. Absolute vs predictive change: which are getting worse than expected? (predict using residuals)

# ==========  set up ADI ==========

## Get total population

  # Check consistent across datasets
  sum(d_claims_21$pop)
  sum(d_crime_21$pop)
  sum(d_health_21$pop)

pop_total <- 45697898

## ---------- Claimants
# For employment, the domain score is the number of individuals claiming benefits principally for the reason of being unemployed... 
# This is the sum of the number of people claiming Jobseeker’s Allowance plus those who claim Universal Credit and are required to seek work and be available for work

  glimpse(d_claims_21)   # 32,844 rows

d_claims_21_select <- d_claims_21 |> 
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

  glimpse(d_crime_21)   # 32,844 rows

d_crime_21_select <- d_crime_21 |>
  rowwise(area_code, area_name, pop) |>
  summarise(
    rate_crime = sum(c_across(ends_with("_rate"))),
    cases_crime = sum(c_across(!ends_with("_rate"))),
    ) |>
  select(area_code, area_name, pop, rate_crime, cases_crime)

  # mutate( rate_crime = sum(c_across(ends_with("_rate"))) )

# >> use microbenchmark to run tests on what's quicker: rowwise or using base R (see https://dplyr.tidyverse.org/articles/rowwise.html)


## ---------- Health
# And for health, the domain score is the sum of registered cases of depression and other mental health illnesses

  glimpse(d_health_21)   # 32,844 rows

d_health_21_select <- d_health_21 |>
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

d_ADI <- d_claims_21_select |> 
  full_join(d_crime_21_select, by = c("area_code", "area_name", "pop")) |>
  full_join(d_health_21_select,  by = c("area_code", "area_name", "pop"))

d_ADI <- d_ADI |>
  mutate(
    ADI_cases = cases_claims + cases_crime + cases_health,
    ADI_rate = rate_claims + rate_crime + rate_health,
    ADI_cases_tot = sum(ADI_cases),
    weight_pop_lsoa = pop/pop_total,
    weight_cases_lsoa = ADI_cases/ADI_cases_tot
    )

  glimpse(d_ADI)
  

# To get district, remove the last 4 numbers (and space) from the end of district name (first check that the 4 letters uniquely identifies districts) 
  # and then group by district name

  
# ==========  inequality measures ==========

# Gini coefficient
  
d_ineq <- d_ADI |> 
  # x-axis: rank LSOAs by ADI rate: cumulative (adding up to total number of people), normalised (per person) rank of ADI rates (i.e., rank of numer of cases per person)
  arrange(ADI_rate) |>
  mutate(
    
  # Break up into chunks of population
  pop_cumsum = cumsum(pop),
  pop_bins = cut_interval(pop_cumsum, n = 10, labels = FALSE),   # Break into groups
  # OR: cut(pop_cumsum, breaks = 100, labels = FALSE)
  
  # For each LSOA, calculate their cumulative proportion of all cases
  cases_cumsum = cumsum(ADI_cases),
  cases_cumsum_norm = cases_cumsum / ADI_cases_tot
  )
  
  glimpse(d_ineq)
  d_ineq |> count(pop_bins)
  quantile(d_ineq$pop_cumsum, probs = seq(0.1,1,0.1))  

# Get total cases per group
d_ineq_sum <- d_ineq |> 
  group_by(pop_bins) |> 
  summarise( cases_total_sum = sum(ADI_cases) ) |>
  ungroup()
  
  ineq::Lc(d_ineq_sum$cases_total_sum)

# Get Lorenz curve values in dataframe
d_lc <-   tibble(
    lc_p = ineq::Lc(d_ineq_sum$cases_total_sum)$p,   # percentages (of population by group)
    lc_L = ineq::Lc(d_ineq_sum$cases_total_sum)$L,   # values of ordindary Lorenz curve: normalised (divided by total cases), cumulative sum of cases
    lc_g = ineq::Lc(d_ineq_sum$cases_total_sum)$L.general*10   # values of generalised Lorenz curve: non-normalized, cumulative sum of cases
  ) 

d_lc <- bind_cols(d_lc,
    d_ineq_sum |> 
      add_row(.before = 1) |>   
      mutate( 
        across(everything(), ~replace_na(.x, 0)),
        pop_bins = as.factor(pop_bins),
        cases_group = cases_total_sum
        )
    )

          # # OLD: ORIGINAL METHOD, THEN COMPARED TO LORENZ CURVE
          # 
          # #For plotting, get cumulative total per bin
          # d_ineq_sum_old <- d_ineq |>
          #   group_by(pop_bins) |>
          #   summarise(
          #     cases_total_sum = sum(ADI_cases),
          #     cases_total_cumsum = max(cases_cumsum),
          #     cases_total_cumsum_norm = max(cases_cumsum_norm)
          #   ) |>
          #   ungroup() |>
          #   mutate(
          #     lorenz_curve = Lc(cases_total_sum)$L[-1],
          #     lorenz_curve_g = Lc(cases_total_sum)$L.general[-1]*10   # strange, seems to divide by 10
          #   ) |>
          #   add_row(.before = 1) |>
          #   mutate( 
          #     across(everything(), ~replace_na(.x, 0)),
          #     pop_bins = as.factor(pop_bins)
          #     )
          # 
          #   sum(d_ineq_sum$cases_total_sum)  
          #   max(d_ineq_sum$cases_total_cumsum)
          #   max(d_ineq_sum$cases_total_cumsum_norm)
          #   
          #   # check cumulative measures
          #   d_ineq_sum$cases_total_cumsum
          #   d_ineq_sum$cases_total_cumsum_norm
          #   ineq::Lc(d_ineq_sum$cases_total_sum)    # compare package vs own calculations (Lc adds zeros in already)


  # compute Gini coefficient
  ineq::Gini(d_lc$cases_group[-1]) # remove zeros rows
  ineq::Gini(d_ineq_sum$cases_total_sum) 

  
  # plot(d_ineq$ADI_rate)

f_gini <- ggplot(
  data = d_lc, 
  aes(y = lc_L, x = lc_p, group = 1))  +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1) +
  scale_y_continuous(name = "Cumulative normalised ADI cases", breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(name = "Cumulative normalised rank of ADI cases", breaks = seq(0, 1, 0.1)) +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_minimal()
f_gini

#>> add Gini coefficient as label 
  




