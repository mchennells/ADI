# Project:      ADI exploration
# File title:   2 -- Figures
# Author:       Matt Chennells

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggplot2)
  library(gt)
  library(plotly)
  library(janitor)
  library(microbenchmark)
  library(hrbrthemes)
  library(ineq)
  library(openxlsx)
})

here()

# ==========  list data  ==========

rm(list=ls())

d_LSOA <- read_csv(here("output", "d_ADI_LSOA.csv"), 
                   col_types = c(year = "f"))

d_DIST <- read_csv(here("output", "d_ADI_DISTRICT.csv"),
                   col_types = c(year = "f"))

d_DIST_l <- read_csv(here("output", "d_ADI_DISTRICT_long.csv"),
                     col_types = c(year = "f"))

d_UK_ptile <- read.xlsx(here("data", "wiidcountry_2.xlsx"),
                        sheet = "UK",
                        colNames = TRUE)

d_UK_quint <- read.xlsx(here("data", "hdiifye2022correction.xlsx"),
                       sheet = "Table 2",
                       startRow = 6, 
                       colNames = TRUE) |> slice(1 : (n()-2))

d_UK_gini <- read.xlsx(here("data", "hdiifye2022correction.xlsx"),
                       sheet = "Table 9",
                       startRow = 7, 
                       cols = c(1:4),
                       colNames = TRUE) |> slice(1 : (n()-2)) |> clean_names()

d_UK_gini_recent <-   read_csv(here("data", "Figure_1_Gini_20_to_22.csv")) |> 
  clean_names() |>
  mutate(across(-year, ~ .x / 100))

# as.factor(year, pop_bins)

mainColor <- "#2876af"
yearColor <- "#69b3a2"
lcColor <- "#b31307"

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


  # print(d_LSOA |> mutate(area_name_sub = str_sub(area_name, start = 1, end = -5)) |> count(area_name_sub), n = 400)   # big variation in numbers of areas; not clear that aggregating by area is a good ideas


# ====================  Gini ==========


# ----------  UK income curve

d_UK_quint_long <- d_UK_quint |>
  filter(Year == "2021/22") |>
  mutate(
    across(-Year, ~ as.numeric(.x) / 1000)
    ) |>
  pivot_longer(cols = -Year, names_to = "quintile", values_to = "median_income") |>
  select(-Year)

d_UK_quint_long$quintile <- fct_relevel(d_UK_quint_long$quintile, "Bottom", "2nd", "3rd", "4th", "Top")

# UK median income
f_UK_median <- ggplot(
  data = d_UK_quint_long |> filter(quintile != "All.Individuals"),
  aes(x = quintile, y = median_income, group = 1)
) +
  geom_point() +
  geom_line() +
  geom_hline(
    yintercept = (d_UK_quint_long |> filter(quintile == "All.Individuals"))$median_income, 
    col = lcColor, size = 1, linetype = "dotted"
    ) +
  theme_ipsum() +
  scale_y_continuous(name = "median income (£000s)", breaks = seq(0, 100, 10)) +
  coord_cartesian(ylim = c(0, 80)) +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
  ) +
  ggtitle("UK median income by income quartile, 2021/22")
f_UK_median


# Table 2: Timeseries of median equivalised disposable household income of individuals by income quintile, 1977-2021/22, UK (2021/22 prices)

# ----------  UK Lorenz curve
d_UK_ptile_long <- d_UK_ptile |>
  select(starts_with("p"), -population, -palma) |>
  pivot_longer(cols = everything(), names_to = "pctile", values_to = "pctile_income_share", 
               names_prefix = "p", names_transform = list(pctile = as.numeric)) |>
  arrange(pctile) |>
  mutate(
    cum_income_share = cumsum(pctile_income_share)
  )

f_UK_pctile <- ggplot(
  data = d_UK_ptile_long,
  aes(x = pctile, y = cum_income_share, group = 1)
  ) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, col = lcColor, lwd = 1) +
  theme_ipsum() +
  scale_y_continuous(name = "cumulative share of income", breaks = seq(0, 100, 10)) +
  scale_x_continuous(name = "income percentile", breaks = seq(0, 100, 10)) +
  coord_cartesian(ylim = c(0, 100), xlim = c(0,100)) +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
  ) +
  ggtitle("UK income Lorenz curve, 2019")
f_UK_pctile

# ----------  UK gini coefficient over time

d_UK_gini_long <- d_UK_gini |>
  mutate(across(-year, ~ as.numeric(.x))) |>
  pivot_longer(cols = -year, names_to = "income_type", values_to = "gini") |>
  mutate(
    gini = gini / 100,
    year_b = str_sub(year, start = 1, end = 2),
    year_e = str_sub(year, start = -2, end = -1),
    year = as.numeric(paste0(year_b, year_e)), 
    year = ifelse(year == "1900", "2000", year)
    ) |> 
  arrange(year) |> select(-year_b, -year_e)


# gini over time
f_UK_gini <- ggplot(
    data = d_UK_gini_long, 
    aes(x = year, y = gini, group = income_type, color = income_type)
  ) +
  geom_line() +
  theme_ipsum() +
  scale_y_continuous(name = "Gini coefficient", breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(name = "year", breaks = seq(0, 3000, 10)) +
  scale_color_discrete(name = "Income type") +
  coord_cartesian(ylim = c(0.1, 0.6)) +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.9, 0.2),
    panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(reverse = TRUE)) +
  ggtitle("Gini coefficient for UK income")
f_UK_gini

# recent, with error bars
f_UK_gini_recent <- ggplot(
  data = d_UK_gini_recent, 
  aes(x = year, y = gini_disposable_income)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.5) +
  theme_ipsum() +
  scale_y_continuous(name = "Gini coefficient", breaks = seq(0, 1, 0.01)) +
  coord_cartesian(ylim = c(0.32, 0.38)) +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.9, 0.25) 
  ) +
  ggtitle("Gini coefficient in UK for disposable income")
f_UK_gini_recent
# source: Figure 1 @ https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/householdincomeinequalityfinancial/financialyearending2022
# >> plotly label for % lagged change
# note: caveat early on that the Gini might not be very sensitive, can't confidently claim that it's changed year on year given error bars; 
#... could be a better measure of income inequality more suited to needs.


# ----------  ADI Gini preparation
n_bins <- 100

d_gini_LSOA <- d_LSOA |> 
  arrange(year) |>
  group_by(year) |>
  arrange(desc(rate_ADI_LSOA)) |>   # 
  mutate(
      
    # x-axis: rank LSOAs by ADI rate (i.e., LSOAs are normalised by person), get cumulative number and proportion of population
    pop_TOTAL = sum(pop, na.rm = TRUE),
    pop_LSOA_cumsum = cumsum(pop),
    pop_LSOA_cumprop = pop_LSOA_cumsum / pop_TOTAL,
    
    # percentiles for each of interpretation (but less accuracy) and Gini calculation (requires equally weighted bins)
    pop_bins = cut_interval(pop_LSOA_cumsum, n = n_bins, labels = FALSE),   # Break into bins
    pop_bins_p = pop_bins / n_bins,
    ## OR: cut(pop_cumsum, breaks = 100, labels = FALSE)
    
    cases_ADI_TOTAL = sum(cases_ADI_LSOA, na.rm = TRUE),
    cases_ADI_prop = cases_ADI_LSOA / cases_ADI_TOTAL,
    cases_ADI_cumprop = cumsum(cases_ADI_prop)
  ) |> 
  ungroup()

  # d_gini |> count(pop_bins)

  # plot(d_gini_LSOA$pop_LSOA_cumprop, d_gini_LSOA$cases_ADI_cumprop)

# ----------  Lorenz curve calculations

d_gini_bins <- d_gini_LSOA |>
  group_by(year, pop_bins, pop_bins_p) |>
  summarise(
    pop_bins_sum = sum(pop, na.rm = TRUE),
    cases_ADI_bins = sum(cases_ADI_LSOA, na.rm = TRUE),
    cases_claims_bins = sum(cases_claims, na.rm = TRUE),
    cases_crime_bins = sum(cases_crime, na.rm = TRUE),
    cases_health_bins = sum(cases_health, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(year) |>
  mutate(   # cumulative sum of cases by bins, for plotting Lc
    cases_ADI_bins_cumsum = cumsum(cases_ADI_bins),
    cases_ADI_bins_cumsum_norm = cases_ADI_bins_cumsum / max(cases_ADI_bins_cumsum),
    
    cases_claims_bins_cumsum = cumsum(cases_claims_bins),
    cases_claims_bins_cumsum_norm = cases_claims_bins_cumsum / max(cases_claims_bins_cumsum),
    
    cases_crime_bins_cumsum = cumsum(cases_crime_bins),
    cases_crime_bins_cumsum_norm = cases_crime_bins_cumsum / max(cases_crime_bins_cumsum),
    
    cases_health_bins_cumsum = cumsum(cases_health_bins),
    cases_health_bins_cumsum_norm = cases_health_bins_cumsum / max(cases_health_bins_cumsum),
  ) |>
  ungroup()
  
  # plot(d_gini_bins$pop_bins_100, d_gini_bins$cases_ADI_bins_cumsum_norm)
  # ineq::Lc(d_gini_bins$cases_ADI_bins)


# ====================  plot Lorenz curve ==========

# ----------  Lc simple pivot for plot
d_gini_bins_long <- d_gini_bins |>
    select(year, pop_bins_p, cases_ADI_bins_cumsum_norm, cases_claims_bins_cumsum_norm, cases_crime_bins_cumsum_norm, cases_health_bins_cumsum_norm) |>
    rename_with(cols = everything(), ~ str_remove(.x, "_bins_cumsum_norm")) |>
    pivot_longer(cols = starts_with("cases_"), names_to = "Type", values_to = "Lc", names_prefix = "cases_")

    # add_row(.before = 1) |>   
    # mutate( 
    #   across(everything(), ~replace_na(.x, 0)),
    #   pop_bins = as.factor(pop_bins),
    # )  

write_csv(d_gini_bins_long, here("output", "d_gini_bins_long.csv"))

## ----- Plot Lorenz curve

range_years <- unique(d_gini_bins_long$year)   #>> select range of years
  range_years
  
# Lc: ADI by category, most recent year
most_recent_year <- c("2021")   # >> year select

# add zero row
new_row <- tibble(year = "2021", pop_bins_p = 0, Type = "ADI", Lc = 0.00)
# new_row <- data.frame(lapply(d_gini_bins_long, function(x) 0))
d_gini_bins_long_recent <- rbind( 
  new_row, 
  d_gini_bins_long |> filter(year %in% most_recent_year)
) |> select(-year)

f_lc_recent <- ggplot(
  data = d_gini_bins_long_recent |> filter(Type == "ADI"), 
  aes(y = Lc, x = pop_bins_p, group = 1))  +
  geom_line(color = mainColor) +
  geom_point(color = mainColor) +
  geom_abline(intercept = 0, slope = 1, col = lcColor, lwd = 1) +
  scale_y_continuous(name = "cumulative normalised ADI cases", breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(name = "cumulative normalised rank of ADI cases", breaks = seq(0, 1, 0.1)) +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.8, 0.2) 
  ) +
  ggtitle(paste0("Lorenz curve of ADI cases in ", most_recent_year, ", by category"))
f_lc_recent
# >> plotly here

f_lc_recent_cat <- ggplot(
  data = d_gini_bins_long_recent |> filter(Type == "ADI"), 
  aes(y = Lc, x = pop_bins_p, group = 1))  +
  geom_line(color = mainColor) +
  geom_point(color = mainColor) +
  geom_line(data = d_gini_bins_long_recent |> filter(Type != "ADI"),
            aes(group = Type, color = Type), 
            linetype = 3, size = 2) +
  geom_abline(intercept = 0, slope = 1, col = lcColor, lwd = 1) +
  scale_y_continuous(name = "cumulative normalised ADI cases", breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(name = "cumulative normalised rank of ADI cases", breaks = seq(0, 1, 0.1)) +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.8, 0.2) 
  ) +
  ggtitle(paste0("Lorenz curve of ADI cases in ", most_recent_year, ", by category"))
f_lc_recent_cat
# \\ health inequality is low; note that it's ranked by ADI rate (rather than health rate)

# Lc: ADI by year
f_lc_all <- ggplot(
  data = d_gini_bins_long |> 
    filter(
      Type == "ADI",
      year %in% range_years
    ), 
  aes(y = Lc, x = pop_bins_p, group = year, color = year))  +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = lcColor, lwd = 1) +
  scale_y_continuous(name = "cumulative normalised ADI cases", breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(name = "cumulative normalised rank of ADI cases", breaks = seq(0, 1, 0.1)) +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.9, 0.25) 
  ) +
  ggtitle("Lorenz curve of ADI cases, by year")
f_lc_all

# But can have similar Gini coefficient but due to either poor getting better off or rich getting worse off

# ====================  plot Gini coefficient ==========

# ----------  Gini coefficient (by year) calculations
d_gini_bins_summary <-
  d_gini_bins |>
  arrange(year, pop_bins) |>
  group_by(year) |>
  summarise(
    gini_ADI = ineq::Gini(cases_ADI_bins),   # so this is per person within LSOA, but it doesn't weigh how different LSOAs are wrt to total contribution to all rates, which I think must happen
    gini_claims = ineq::Gini(cases_claims_bins),
    gini_crime = ineq::Gini(cases_crime_bins),
    gini_health = ineq::Gini(cases_health_bins)
  ) |>
  ungroup()

d_gini_bins_summary_long <- d_gini_bins_summary |>
  pivot_longer(cols = c(-year), names_to = "Type", values_to = "gini", names_prefix = "gini_")

write_csv(d_gini_bins_summary_long, here("output", "d_gini_bins_summary_long.csv"))

# ----------  plot Gini coefficient over time
f_gini_year <- ggplot(
  data = d_gini_bins_summary_long |> filter(Type == "ADI"), 
  aes(y = gini, x = year, group = 1)) +
  geom_line(size = 2, color = mainColor) +
  coord_cartesian(ylim = c(0,0.6)) +
  scale_y_continuous(name = "Gini coefficient") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20)
  ) +
  ggtitle("Gini coefficient of ADI cases, by year")
f_gini_year

# > plotly: add labels: % change each year; overall % change



## ... and by category   >>> add in category to click
f_gini_year_cat <- ggplot(
  data = d_gini_bins_summary_long |> filter(Type == "ADI"), 
  aes(y = gini, x = year, group = 1)) +
  geom_line(size = 2, color = mainColor) +
  geom_line(data = d_gini_bins_summary_long |> filter(Type != "ADI"), 
            aes(group = Type, color = Type), 
            linetype = 3, size = 2) +
  coord_cartesian(ylim = c(0,0.6)) +
  scale_y_continuous(name = "Gini coefficient") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20), 
    legend.position = c(0.9, 0.8)
  ) +
  ggtitle("Gini coefficient of ADI cases by category, by year")
f_gini_year_cat

# >>> can add graph selection option to look at difference between selected points


# ---------- gini income vs ADI 

g_adi <- d_gini_bins_summary_long |> filter(Type == "ADI")

g_inc <- d_UK_gini_long |> mutate(year = as.numeric(year)) |> 
  filter(year >= 2013, income_type == "disposable") |> 
  rename(Type = income_type) |> slice(1:(n()-1)) |>
  mutate(Type = ifelse(Type == "disposable", "Income", Type))

d_gini_joint <- rbind(g_adi, g_inc) |> arrange(year)

f_gini_joint <- ggplot(
  data = d_gini_joint, 
  aes(y = gini, x = year, group = Type, color = Type)) +
  geom_line(size = 2) +
  coord_cartesian(ylim = c(0.2,0.4)) +
  scale_y_continuous(name = "Gini coefficient") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    panel.grid.minor.y = element_blank(),
    legend.position = c(0.2, 0.2)
  ) +
  ggtitle("Gini coefficient of UK Income vs ADI cases, over time")
f_gini_joint

d_gini_joint_diff <- d_gini_joint |> 
  pivot_wider(names_from = Type, values_from = gini) |>
  mutate(diff = ADI - Income)

f_gini_joint_diff <- ggplot(
  data = d_gini_joint_diff, 
  aes(y = diff, x = year)) +
  # geom_col() +
  geom_point(size = 2) +
  geom_segment(aes(xend = year, y = 0, yend = diff)) +
  coord_cartesian(ylim = c(-0.075, 0.025)) +
  scale_y_continuous(name = "Gini ADI minus Income") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )
f_gini_joint_diff


# ==============================================================================================================
# ====================  Theil measure ==========

df <- d_LSOA |>
  # filter(year %in% c("2020", "2021")) |>
  mutate(
    district = str_sub(area_name, start = 1, end = -5),     # identify district name
  ) |>
  select(area_name, district, pop, cases_ADI_LSOA, year) |>
  rename(
    neighbourhood_name = area_name,
    district_name = district,
    number_of_job_claims = cases_ADI_LSOA,
    neighbourhood_population = pop,
  )
  

# overall Theil measure

# total_job_claims <- df |>
#   group_by(year) |>
#   summarise(
#     total_job_claims = sum(number_of_job_claims, na.rm = TRUE)
#   )
# 
# overall_neighbourhood_mean <- df |>
#   group_by(year) |>
#   summarise(
#     overall_neighbourhood_mean = mean(number_of_job_claims, na.rm = TRUE)
#   )

theil <- df |>
  group_by(year) |>
  mutate(
    overall_neighbourhood_mean = mean(number_of_job_claims, na.rm = TRUE),
    claims_share = number_of_job_claims / overall_neighbourhood_mean,
    theil = claims_share * log(claims_share)
  ) |>
  summarise(
    theil = sum(theil, na.rm = TRUE) / length(neighbourhood_name)
  )
theil

# total: 0.1796

# calculate between-district Theil index

between_district_theil <- df %>%
  group_by(year) |>
  mutate(
    total_job_claims = sum(number_of_job_claims, na.rm = TRUE),
    overall_neighbourhood_mean = mean(number_of_job_claims, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(year, district_name) |>
  summarise(
    district_job_claims = sum(number_of_job_claims, na.rm = TRUE),
    district_job_claims_share = district_job_claims / mean(total_job_claims, na.rm = TRUE),
    district_job_claims_mean = mean(number_of_job_claims, na.rm = TRUE),
    between_district_theil = district_job_claims_share * log(district_job_claims_mean / mean(overall_neighbourhood_mean, na.rm = TRUE))
  )

between_district_theil_sum <- between_district_theil |>
  group_by(year) |>
  summarise(
    between_district_theil = sum(between_district_theil, na.rm= TRUE)
  )

between_district_theil_sum
# 0.052 (cases)
# 0.491 (rate)


# calculate within-district Theil index
within_district_theil <- df |>
  left_join(between_district_theil |> select(district_name, year, district_job_claims_mean)) |>
  group_by(year) |>
  mutate(
    total_job_claims = sum(number_of_job_claims, na.rm = TRUE),
    overall_neighbourhood_mean = mean(number_of_job_claims, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(year, district_name) %>% 
  mutate(
    job_claims_share = number_of_job_claims / sum(number_of_job_claims, na.rm = TRUE),
    neighbourhood_theil = job_claims_share * log(number_of_job_claims / district_job_claims_mean)
  ) |>
  ungroup() |> 
  group_by(year, district_name) |>
  summarise(
    neighbourhood_theil_sum = sum(neighbourhood_theil, na.rm = TRUE)
  ) |> 
  ungroup() |>
  left_join(between_district_theil |> select(district_name, district_job_claims_share)) |>
  mutate(
    neighbourhood_theil_times_district_share = neighbourhood_theil_sum * district_job_claims_share
  )

within_district_theil_sum <- within_district_theil |>
  group_by(year) |>
  summarise(
    within_district_theil = sum(neighbourhood_theil_times_district_share)
  )
within_district_theil_sum

# 0.127 (using cases)
# 0.099 (using rates)

# combine datasets
theil_total <- left_join(theil, between_district_theil_sum) |> left_join(within_district_theil_sum) |>
  mutate(
    T_between_contribution = between_district_theil / theil
  ) |>
  rename(
    T_total = theil,
    T_between = between_district_theil,
    T_within = within_district_theil
  )

theil_total_long <- theil_total |>
  pivot_longer(cols = -year, names_to = "Theil_component", values_to = "Theil_value")

head(theil_total_long)

theil_total_long$Theil_component <- fct_relevel(theil_total_long$Theil_component, "T_total", "T_within", "T_between")

f_theil <- ggplot(
  data = theil_total_long |> filter(Theil_component != "T_between_contribution"), 
  aes(y = Theil_value, x = year, group = Theil_component, color = Theil_component)) +
  geom_line(size = 2) +
  coord_cartesian(ylim = c(0,0.3)) +
  scale_y_continuous(name = "Theil value") +
  scale_x_discrete(name = "year") +
  scale_color_discrete(name = "Theil Component") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.9, 0.85)
  ) +
  ggtitle("Theil measure of ADI cases over time, between and within districts")
f_theil
  



# ==============================================================================================================
# ====================  ABSOLUTE cases over time ==========

d_year_totals <- d_gini_bins |>
  # filter(cases_group != 0) |>
  group_by(year) |>
  summarise(
    ADI = max(cases_ADI_bins_cumsum),
    # ADI_sum = sum(cases_ADI_bins, na.rm = TRUE),    # should give same answer
    Claims = max(cases_claims_bins_cumsum),
    Crime = max(cases_crime_bins_cumsum),
    Health = max(cases_health_bins_cumsum)
  ) |>
  mutate(across(-year, ~ .x /1000000))

  # d_year_totals_check <- d_year_totals |> mutate(cases_check = (Claims + Crime + Health ) - ADI)

###
d_year_totals_pivot <- d_year_totals |> 
  pivot_longer(cols = -year, names_to = "Type", values_to = "N")

# ADI total and by category, over time
f_cases_year <- ggplot(
  data = d_year_totals_pivot |> filter(Type == "ADI"), 
  aes(y = N, x = year, group = 1)) +
  geom_line(size = 2, color = mainColor) +
  geom_line(data = d_year_totals_pivot |> filter(Type != "ADI"),    # Can use d_abs_pivot to include ADI as well
            aes(y = N, x = year, group = Type, color = Type), 
            linetype = 3, size = 2) +
  scale_y_continuous(name = "cases, in millions") +
  scale_x_discrete(name = "year") +
  coord_cartesian(ylim = c(0, 40)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.12,0.8) 
  ) +
  ggtitle("ADI total cases by year, and by category")
f_cases_year

# Proportions shown in stacked graph
f_cases_stack <- ggplot(
  data = d_year_totals_pivot |> filter(Type != "ADI"),  
  aes(y = N, x = year, fill = Type)) +
  geom_area(
    aes(x = as.numeric(year)),
    alpha = 0.4
  ) +
  geom_line(aes(group = Type, color = Type), linetype = 3, size = 2) +
  # geom_line(data = d_year_totals_pivot |> filter(Type == "ADI"),
  #           aes(x = year, group = 1, fill = NULL, color = NULL), color = mainColor, size = 2) +
  scale_x_discrete(name = "year") +
  scale_y_continuous(name = "cases, in millions") +
  coord_cartesian(ylim = c(0, 40)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.12,0.8) 
  ) +
  ggtitle("ADI total cases by year, and by category (shaded = proportion)")
#geom_line( color = mainColor), size = 2)
f_cases_stack

# https://r-graph-gallery.com/stacked-area-graph.html (see also for plotly and dygaph interactive code)



# ====================  plot cases by deciles ==========

# d_gini_bins_check <- d_gini_bins |>
#   select(year, pop_bins, cases_ADI_bins, cases_claims_bins, cases_crime_bins, cases_health_bins) |>
#   mutate(
#     checksie = cases_claims_bins + cases_crime_bins + cases_health_bins,
#     minus = cases_ADI_bins - checksie
#     
#   )

# create deciles using previous bins
d_gini_deciles_base <- d_gini_bins |>
  arrange(year, pop_bins) |>
  group_by(year) |>
  mutate(
    pop_deciles = as.factor(cut_interval(pop_bins, n = 10, labels = FALSE)),
  ) |> 
  ungroup()

  # print(d_gini_deciles |> count(year, pop_deciles), n = 200)

d_gini_deciles <- d_gini_deciles_base |>
  group_by(year, pop_deciles) |>
  summarise(
    pop_decile = sum(pop_bins_sum),
    
    cases_ADI_decile = sum(cases_ADI_bins, na.rm = TRUE), # deciles are equal sized, so no need to weight according to pop size
    rate_ADI_decile = cases_ADI_decile / pop_decile,

    cases_claims_decile = sum(cases_claims_bins, na.rm = TRUE),
    rate_claims_decile = cases_claims_decile / pop_decile,

    cases_crime_decile = sum(cases_crime_bins, na.rm = TRUE),
    rate_crime_decile = cases_crime_decile / pop_decile,

    cases_health_decile = sum(cases_health_bins, na.rm = TRUE),
    rate_health_decile = cases_health_decile / pop_decile
  )

write_csv(d_gini_deciles, here("output", "d_gini_deciles.csv"))

# d_bins_facet <- d_bins |> 
#   select(starts_with("cases_"), year, pop_bins, -cases_group) |>
#   pivot_longer(cols = c(-year, -pop_bins), names_to = "Type", values_to = "N", names_prefix = "cases_") |>
#   mutate(Type = str_remove(Type ,"group_"))
# 
# # ADI category for most deprived groups
# f_cases_bin_facet <- ggplot(
#   data = d_bins_facet, 
#   aes(y = N, x = year, group = Type, color = Type)) +
#   geom_line(size = 2) +
#   scale_y_continuous(name = "cases, in millions") +
#   scale_x_discrete(name = "year") +
#   coord_cartesian(ylim = c(0, 6)) +
#   theme_ipsum() +
#   theme(
#     axis.title.y = element_text(color = mainColor, size = 20),
#     axis.title.x = element_text(color = yearColor, size = 20),
#     legend.position = c(0.95,0.15) 
#   ) +
#   ggtitle("ADI cases by ADI decile (1=most deprived), by category and year") +
#   facet_wrap(vars(pop_bins))
# f_cases_bin_facet



## ----------  plot rates by decile over time
# see how the ranges change over time

f_area_rate_by_year <- ggplot(
  data = d_gini_deciles,
  aes(y = rate_ADI_decile, x = year, group = pop_deciles, color = pop_deciles)
  ) +
  geom_line(size = 2) +
  scale_y_continuous(name = "ADI rate (average cases/person)") +
  scale_x_discrete(name = "years") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_ipsum() +
  labs(color = "ADI rate decile\n(1=most deprived)") +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "right"
  ) +
  ggtitle("ADI rate by LSOA decile rank, by year")
f_area_rate_by_year


## ----------- annual rate change by decile

# rate changes by band
year_input_1 <- "2020"
year_input_2 <- "2021"

# d_prog_AREA_band_sum_change <- d_prog_AREA_band_sum |>
#   filter(year %in% c(year_1, year_2)) |>
#   pivot_wider(names_from = year, values_from = decile_rate_mean, names_prefix = "rate_") |>
#   mutate(rate_change = rate_2021 - rate_2020)

# rate change by decile

d_gini_deciles_change <- d_gini_deciles |>
  filter(year == year_input_1 | year == year_input_2) |>
  select(year, pop_deciles, starts_with("rate_")) |>
  rename_with(cols = everything(), ~ str_remove_all(.x, "rate_|_decile")) |> 
  arrange(pops, year) |>
  group_by(pops) |>
  mutate(
    change_ADI = ADI - lag(ADI), 
    change_claims = ADI - lag(claims), 
    change_crime = ADI - lag(crime), 
    change_health = ADI - lag(health), 
    
    change_color = as.factor(ifelse(change_ADI >= 0, "pos", "neg"))
  ) |>
  ungroup()

d_gini_deciles_change_base <- d_gini_deciles_change |>
  select(pops, change_color, starts_with("change_")) |>
  filter(!is.na(change_ADI))
  
# graph rate changes by decile
f_rate_change_base <- 
  ggplot(
    data = d_gini_deciles_change_base, 
    aes(x = pops, y = change_ADI, group = change_color, color = change_color)
  ) +
  geom_point(size = 3) +
  geom_segment(aes(xend = pops, y = 0, yend = change_ADI), size = 1.5) +
  # geom_errorbar() +
  geom_hline(yintercept = 0, size = 0.25, color = "grey") +
  coord_cartesian(ylim = c(-0.05, 0.15)) +
  scale_y_continuous(name = "change in ADI rate") +
  scale_x_discrete(name = "LSOA ADI rate decile rank (1=most deprived)") +
  scale_color_manual(values=c(lcColor, yearColor)) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "none"
  ) +
  ggtitle(paste0("change in ADI rate between ", year_input_1," and ", year_input_2,", by LSOA ADI decile rank"))
f_rate_change_base

# graph rate changes by decile, from previous year level
d_gini_deciles_change_orig <- d_gini_deciles_change |>
  select(pops, year, change_color, ADI, change_ADI) |>
  arrange(pops, year) |>
  group_by(pops) |>
  mutate(
    ADI_1 = lag(ADI), 
    ADI_2 = ADI
  ) |>
  ungroup() |>
  filter(!is.na(change_ADI)) |> 
  select(-year)

f_rate_change_orig <- 
  ggplot(
    data = d_gini_deciles_change_orig, 
    aes(x = pops, y = ADI_1, group = change_color, color = change_color)
  ) +
  geom_point(size = 1.5) +
  geom_point(aes(y=ADI_2), size = 1.5) +
  geom_segment(
    aes(xend = pops, y = ADI_1, yend = ADI_2), size = 1.5,
  ) +
  geom_segment(
    aes(xend = pops, y = ADI_1, yend = ADI_2),
    arrow = arrow(length = unit(0.015, "npc")), 
    position = position_nudge(x = -0.3)
    ) +
  # geom_errorbar() +
  # geom_hline(yintercept = 0, size = 0.25, color = "grey") +
  # coord_cartesian(ylim = c(-0.05, 0.15)) +
  scale_y_continuous(name = "change in ADI rate") +
  scale_x_discrete(name = "LSOA ADI rate decile rank (1=most deprived)") +
  scale_color_manual(values=c(lcColor, yearColor)) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "none"
  ) +
  ggtitle(paste0("change in ADI rate between ", year_input_1," and ", year_input_2,", by LSOA ADI decile rank"))
f_rate_change_orig
#

# 
# ## ----------- annual rate change by LSOA (identifying biggest changes)
# 
# d_gini_deciles_limits <- d_gini_deciles |>
#   filter(year == year_input_1 | year == year_input_2) |>
#   select(year, pop_deciles, starts_with("rate_")) |>
#   rename_with(cols = everything(), ~ str_remove_all(.x, "rate_|_decile")) |>
#   arrange(pops, year) |>
#   group_by(pops) |>
#   mutate(
#     change_ADI = ADI - lag(ADI), 
#     change_claims = ADI - lag(claims), 
#     change_crime = ADI - lag(crime), 
#     change_health = ADI - lag(health), 
#     
#     change_color = as.factor(ifelse(change_ADI >= 0, "pos", "neg"))
#   ) |>
#   ungroup()
# 
# 
# # get upper limits of ADI rate decile bands
# d_deciles_limits <- d_prog_AREA_ranks |>
#   select(year, area_ADI_rate, area_rank_by_year_decile) |> 
#   group_by(year, area_rank_by_year_decile) |>
#   summarise(
#     rank_mean = mean(area_ADI_rate),
#     rank_min = min(area_ADI_rate),
#     rank_max = max(area_ADI_rate)
#   ) |> 
#   ungroup()
# 
# f_decile_change <- ggplot(
#   data = d_prog_AREA_ranks_lag_recent |> filter(change_group_max == 2),
#   aes(x = year, y = rate, group = area_name_sub)
# ) +
#   geom_point() +
#   geom_line() +
#   # geom_point(
#   #   data = d_deciles_limits,
#   #   aes(y = rank_max, group = NULL),
#   #   color = "black", size = 4
#   # ) +
#   geom_errorbar(
#     data = d_deciles_limits |> filter(year == "2020"),
#     aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
#     size = 2, width = 0.1, alpha = 0.4, position = position_nudge(x = -0.05)
#   ) +
#   geom_errorbar(
#     data = d_deciles_limits |> filter(year == "2021"),
#     aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
#     size = 1, width = 0.1, alpha = 0.4, position = position_nudge(x = +0.05)
#   ) +
#   coord_cartesian(ylim = c(0.2, 1.5)) +
#   scale_y_continuous(name = "ADI rate", breaks = seq(0, 25, 0.25)) +
#   scale_x_discrete(name = "year") +
#   theme_ipsum() +
#   theme(
#     axis.title.y = element_text(color = mainColor, size = 20),
#     axis.title.x = element_text(color = yearColor, size = 20),
#     legend.position = "none",
#     panel.grid.major.x = element_blank()
#   ) +
#   ggtitle("ADI rate in 2020 and 2021 for 40 areas seeing worsening of decile rank (with ADI decile bands shown)")
# 
# f_decile_change


## >> Add: ability to lookup specific LSOAs to see changes over time



# ====================  plot district changes over two periods ==========

# hard to look directly at specific LSOA's (indicated by 4-digit code), as some are missing years of data and might be larger variation
# rather, look at districts and then dive into those experiencing more decline

d_district <- d_DIST

  glimpse(d_district)

# Looking at individual district progress over time, ideas: 
# > look at position changes, either by rank or pop bin change each year
# > likelihood of moving up or down

  
## ----------- average district-level ADI rate over time
  
fun_type <- "mean"

f_rate_by_year_dist <- 
  ggplot(
    data = d_district,
    aes(x = year, y = rate_ADI)
  ) +
  geom_violin() +
  stat_summary(
    geom = "point", 
    fun = fun_type, 
    color = mainColor, size = 2
  ) +
  stat_summary(
    group = 1,
    geom = "line", 
    fun = fun_type, 
    color = mainColor, size = 1, linetype = 3
  ) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_y_continuous(name = "average ADI rate") +
  scale_x_discrete(name = "year") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  ggtitle("distribution of district-level ADI rate, by year")
f_rate_by_year_dist


## ----------- worst and best performing performing districts

year_input_district_1 <- "2020"
year_input_district_2 <- "2021"
  

d_district_lag <- d_district |>
  arrange(district, year) |>
  group_by(district) |>
  mutate (
    rate_change = round(rate_ADI - lag(rate_ADI), digits = 5),
  ) |> 
  ungroup()

d_district_lag_select<- d_district_lag |> 
  filter(year %in% c(year_input_district_1, year_input_district_2))

# Is there a natural cutoff?

d_select_cutoff <- d_district_lag_select |> 
  filter(year == year_input_district_2) |> 
  arrange(desc(rate_change)) |>
  mutate(
    index = row_number(),
    qlnorm = qlnorm(rate_change+0.1) - 0.1
         )

plot(d_select_cutoff$qlnorm)

f_ADI_cutoff <- ggplot(
  data = d_select_cutoff,
  aes(x= index, y=rate_change, text = district)
) +
  geom_point() +
  geom_hline(yintercept = 0, size = 0.5, color = "grey") +
  geom_line(aes(y = qlnorm - 0.2, group = 1), color = lcColor) +
  scale_y_continuous(name = "ADI rate change", breaks = seq(-0.2,0.3, 0.05)) +
  scale_x_continuous(name = "District") +
  coord_cartesian(ylim = c(-0.1, 0.25)) +
  theme_ipsum() +
  theme(  
    axis.text.x = element_blank(),
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
  )
f_ADI_cutoff
# ggplotly(f_ADI_cutoff, tooltip = c("area_name_sub"))

# f_ADI_cutoff_qlnorm <- ggplot(
#   data = d_select_cutoff,
#   aes(x= index, y=rate_change)
# ) +
#   geom_point() +
#   geom_line(aes(y = qlnorm - 0.2, group = 1), color = lcColor) +
#   # geom_line(aes(y=qlnorm, group = 1)) +
#   # geom_hline(yintercept = 0, size = 0.5, color = "grey") +
#   scale_y_continuous(name = "ADI rate change") +
# 
#   # coord_cartesian(ylim = c(-0.1, 0.25)) +
#   theme_ipsum() +
#   theme(  
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.y = element_text(color = mainColor, size = 20),
#     )
# f_ADI_cutoff_qlnorm


# Table of performers
d_district_select <- d_district_lag_select |> 
  filter(year == year_input_district_2) |>
  select(district, pop, rate_change, cases_ADI, rate_ADI, everything(), -year)
  

# head(d_district_select, n = 20) |> 

d_district_select |> 
  arrange(desc(rate_change)) |>
  gt() |>
  tab_header(title = "District-level ADI info and sub-categories") |>
  tab_options(
    ihtml.active = TRUE,
    ihtml.use_pagination = TRUE,
    ihtml.use_highlight = TRUE,
    ihtml.page_size_default = 20
    # ihtml.use_filters = TRUE,
    # ihtml.use_search = TRUE
    ) |>
  fmt_number(
    columns = starts_with("rate_"),
    ) |>
  fmt_number(
    columns = c(pop, starts_with("cases")),
    decimals = 0
  ) |>
  data_color(columns = rate_change,
             colors = "#e66000"
             ) |>
  cols_label(
    district = "District",
    pop = "Population",
    rate_change = "ADI rate change",
    cases_ADI = "ADI cases",
    rate_ADI = "ADI rate",
    
    cases_claims = "Claims cases",
    rate_claims = "Claims rate",
    
    cases_crime = "Crime cases",
    rate_crime = "Crime rate",
    
    cases_health = "Health cases",
    rate_health = "Health rate",
  )

# Get worst and best performers
n_slice <- 20

d_select_worst <- d_district_lag_select |> 
  filter(year == year_input_district_2) |>
  arrange(desc(rate_change)) |>  
  slice_head(n=n_slice)

d_select_best <- d_district_lag_select |> 
  filter(year == year_input_district_2) |>
  arrange(rate_change) |>
  slice_head(n=n_slice)


d_district_lag_worst <- d_district_lag_select |>
  filter(district %in% d_select_worst$district)

d_district_lag_best <- d_district_lag_select |>
  filter(district %in% d_select_best$district)


d_district_lag_main <- d_district_lag_select |>
  filter(!(district %in% d_select_worst$district) & !(district %in% d_select_best$district))


f_rate_worst <- 
  ggplot(
    data = d_district_lag_main,
    aes(x = year, y = rate_ADI, label = district)
  ) +
  geom_point(color = "grey", alpha = 0.3, position = position_jitter()) +
  geom_boxplot(alpha = 0.2) +
  
  # biggest rate increases (worst performers)
  geom_point(data = d_district_lag_worst, color = lcColor) +
  geom_line(data = d_district_lag_worst, aes(group = district), color = lcColor) +
  
  coord_cartesian(ylim = c(0.25, 1.5)) +
  scale_y_continuous(name = " average ADI rate", breaks = seq(0, 2, 0.25)) +
  scale_x_discrete(name = "LSOA ADI rate decile rank (1=most deprived)") +
  scale_color_manual(values=c(lcColor, yearColor)) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    # legend.position = "none"
  ) +
  ggtitle(paste0("worst-performing districts (ADI rate between ", year_input_1," and ", year_input_2, ")"))
f_rate_worst
# ggplotly(f_rate_worst)

f_rate_best <- 
  ggplot(
    data = d_district_lag_main,
    aes(x = year, y = rate_ADI, label = district)
  ) +
  geom_point(color = "grey", alpha = 0.3, position = position_jitter()) +
  geom_boxplot(alpha = 0.2) +
  
  # biggest rate decreases (best performers)
  geom_point(data = d_district_lag_best, color = mainColor) +
  geom_line(data = d_district_lag_best, aes(group = district), color = mainColor) +
  
  coord_cartesian(ylim = c(0.25, 1.5)) +
  scale_y_continuous(name = " average ADI rate", breaks = seq(0, 2, 0.25)) +
  scale_x_discrete(name = "LSOA ADI rate decile rank (1=most deprived)") +
  scale_color_manual(values=c(lcColor, yearColor)) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    # legend.position = "none"
  ) +
  ggtitle(paste0("best-performing districts (ADI rate between ", year_input_1," and ", year_input_2, ")"))
f_rate_best
# ggplotly(f_rate_best)

f_rate_both <- 
  ggplot(
    data = d_district_lag_main,
    aes(x = year, y = rate_ADI, label = district)
  ) +
  geom_point(color = "grey", alpha = 0.3, position = position_jitter()) +
  geom_boxplot(alpha = 0.2) +
  
  # biggest rate decreases (best performers)
  geom_point(data = d_district_lag_best, color = mainColor) +
  geom_line(data = d_district_lag_best, aes(group = district), color = mainColor) +
  
  # biggest rate increases (worst performers)
  geom_point(data = d_district_lag_worst, color = lcColor) +
  geom_line(data = d_district_lag_worst, aes(group = district), color = lcColor) +
  
  coord_cartesian(ylim = c(0.25, 1.5)) +
  scale_y_continuous(name = " average ADI rate", breaks = seq(0, 2, 0.25)) +
  scale_x_discrete(name = "LSOA ADI rate decile rank (1=most deprived)") +
  scale_color_manual(values=c(lcColor, yearColor)) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    # legend.position = "none"
  ) +
  ggtitle(paste0("district-level ADI rate between ", year_input_1," and ", year_input_2))
f_rate_both


# ## print tables
# 
# d_worst_table <-  d_district_lag_worst |> 
#   select(area_name_sub, year, rate_ADI_AREA) |>
#   rename(district = area_name_sub, rate = rate_ADI_AREA) |>
#   mutate(rate = round(rate, digits=2)) |>
#   group_by(district) |>
#   pivot_wider(names_from = year, values_from = rate, names_prefix = "rate_") |>
#   mutate(rate_change = rate_2021 - rate_2020) |>
#   ungroup() |>
#   arrange(desc(rate_change))
# d_worst_table
# 
# d_best_table <-  d_district_lag_best |> 
#   select(area_name_sub, year, rate_ADI_AREA) |>
#   rename(district = area_name_sub, rate = rate_ADI_AREA) |>
#   mutate(rate = round(rate, digits=2)) |>
#   group_by(district) |>
#   pivot_wider(names_from = year, values_from = rate, names_prefix = "rate_") |>
#   mutate(rate_change = rate_2021 - rate_2020) |>
#   ungroup() |>
#   arrange(rate_change)
# d_best_table









# # ==============================================================================================================
# # ====================  chloropleth map ==========
# 
# 
# library(rgeos)
# library(rgdal)
# library(maptools)
# 
# shp <- readOGR(here("data", "maps", "LAD_DEC_2021_GB_BUC.shp"))
# 
# # glimpse(shp)
# 
# shp_1 <- fortify(shp, region = 'LAD21CD') |> rename(area_code = id)
# shp_2 <- fortify(shp, region = 'LAD21NM') |> rename(area_name_sub = id)
# 
# # test <-  shp_2 |> left_join(d_DIST) |> filter(!is.na(year))
# 
# test <-  shp_1 |> left_join(d_LSOA) |> filter(!is.na(year))
# 
# test <- arrange(test, order)
# 
# p <- ggplot(data = test, aes(x = long, y = lat, 
#                             group = group, fill = rate)) + 
#   geom_polygon() + coord_equal() + theme_void()
# p
# 
# describe(test$rate_ADI_AREA)