# Project:      ADI exploration
# File title:   2 -- Figures
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

d_dec <- read_csv(here("output", "d_deciles.csv"), 
                  col_types = c(year = "f", pop_bins = "f"))

d_full <- read_csv(here("output", "d_full.csv"),
                   col_types = c(year = "f"))
  

# as.factor(year, pop_bins)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

mainColor <- "#2876af"
yearColor <- "#69b3a2"
lcColor <- "#b31307"


# ==========  Gini  ==========

d_dec_lc <- d_dec |> 
  select(year, pop_bins, lc_p, starts_with("lc_L")) |>
  rename(lc_L_ADI = lc_L) |>
  pivot_longer(cols = c(-year, -pop_bins,-lc_p), names_to = "Type", values_to = "Lc", names_prefix = "lc_L_")

d_dec_lc |> count(Type)

## ----- Plot Lorenz curve

# Lc: ADI by year
f_lc <- ggplot(
  data = d_dec_lc |> filter(Type == "ADI"), 
  aes(y = Lc, x = lc_p, group = year, color = year))  +
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
    legend.position = c(0.8, 0.2) 
  ) +
  ggtitle("Lorenz curve of ADI cases, over time")
f_lc

# But can have similar Gini coefficient but due to either poor getting better off or rich getting worse off

# Lc: ADI by category, most recent year
most_recent_year <- "2021"
d_dec_lc_mry <- d_dec_lc |> filter(year == most_recent_year)

f_lc_cat <- ggplot(
  data = d_dec_lc_mry |> filter(Type == "ADI"), 
  aes(y = Lc, x = lc_p, group = 1))  +
  geom_line(color = mainColor) +
  geom_point(color = mainColor) +
  geom_line(data = d_dec_lc_mry |> filter(Type != "ADI"), 
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
f_lc_cat

# \\ health inequality is low; gini is driven by claims (and then crime) inequality

# ----- compute Gini coefficient
year_list_gini <- unique(d_dec$year)
year_list_gini

d_dec_gini <- tibble()

for (y in year_list_gini) {
  
  gini_ADI <- ineq::Gini( (d_dec |> filter(year == y))$cases_group[-1] )
  gini_claims <- ineq::Gini( (d_dec |> filter(year == y))$cases_group_claims[-1] )
  gini_crime <- ineq::Gini( (d_dec |> filter(year == y))$cases_group_crime[-1] )
  gini_health <- ineq::Gini( (d_dec |> filter(year == y))$cases_group_health[-1] )
  
  z_gini <- tibble(
    year = y, gini_ADI = gini_ADI, gini_claims = gini_claims, gini_crime = gini_crime, gini_health = gini_health
  )
  
  d_dec_gini <- bind_rows(d_dec_gini, z_gini)
  
}

d_dec_gini_long <- d_dec_gini |>
  pivot_longer(cols = c(-year), names_to = "Type", values_to = "gini", names_prefix = "gini_")

## Gini coefficient over time
f_gini_year <- ggplot(
  data = d_dec_gini_long |> filter(Type == "ADI"), 
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

# >> add labels: % change each year; overall % change

## Gini coefficient over time, by category too
f_gini_year_cat <- ggplot(
  data = d_dec_gini_long |> filter(Type == "ADI"), 
  aes(y = gini, x = year, group = 1)) +
  geom_line(size = 2, color = mainColor) +
  geom_line(data = d_dec_gini_long |> filter(Type != "ADI"), 
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


# ----- add absolute values to the coefficient graph

d_dec_check <- d_dec |> mutate(cases_check = (cases_group_claims + cases_group_crime + cases_group_health ) - cases_group)
# Tiniest discrepancy due to rounding

d_abs <- d_dec |>
  filter(cases_group != 0) |>
  group_by(year) |>
  summarise(
    ADI = sum(cases_group),
    Claims = sum(cases_group_claims),
    Crime = sum(cases_group_crime),
    Health = sum(cases_group_health)
  ) |>
  mutate(across(-year, ~ .x /1000000))

d_abs_check <- d_abs |> mutate(cases_check = (Claims + Crime + Health ) - ADI)


###
d_abs_pivot <- d_abs |> pivot_longer(cols = -year, names_to = "Type", values_to = "N")
d_abs_ADI <- d_abs |> mutate(ADI = Claims + Crime + Health) |> select(year, ADI)
d_abs_CAT <- d_abs |> select(-ADI) |> pivot_longer(cols = -year, names_to = "Type", values_to = "N")

# ADI total and by category, over time
f_cases_year <- ggplot(
  data = d_abs_ADI, 
  aes(y = ADI, x = year, group = 1)) +
  geom_line(size = 2, color = mainColor) +
  geom_line(data = d_abs_CAT,    # Can use d_abs_pivot to include ADI as well
            aes(y = N, x = year, group = Type, color = Type), 
            linetype = 3, size = 2) +
  scale_y_continuous(name = "cases, in millions") +
  scale_x_discrete(name = "year") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.1,0.8) 
  ) +
  ggtitle("ADI total cases by year, and by category")
f_cases_year


# Proportions shown in stacked graph
f_cases_stack <- ggplot(
  data = d_abs_ADI,  aes(y = ADI, x = as.numeric(year)), group = 1) +
  geom_line(color = mainColor, size = 2) +
  geom_area(
    data = d_abs_CAT,
    aes(y = N, x = as.numeric(year), fill = Type),
    alpha = 0.4
  ) +
  geom_line(data = d_abs_CAT,  aes(y = N, x = as.numeric(year), group = Type, color = Type), linetype = 3, size = 2) +
  scale_x_continuous(name = "year", breaks = seq(2013, 2021, 1)) +
  scale_y_continuous(name = "cases, in millions") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.1,0.8) 
  ) +
  ggtitle("ADI total cases by year, and by category (shaded = proportion)")
#geom_line( color = mainColor), size = 2)
f_cases_stack

# https://r-graph-gallery.com/stacked-area-graph.html (see also for plotly and dygaph interactive code)

# ----------- By deciles

d_bins <- d_dec |> filter(cases_group != 0) |> 
  mutate(across(starts_with("cases_"), ~ .x / 1000000))

# ADI total and by category, over time
f_cases_bins <- ggplot(
  data = d_bins, 
  aes(y = cases_group, x = year, group = pop_bins, color = pop_bins)) +
  geom_line(size = 2) +
  # geom_line(data = d_abs_CAT,    # Can use d_abs_pivot to include ADI as well
  #           aes(y = N, x = year, group = Type, color = Type), 
  #           linetype = 3, size = 2)
  scale_y_continuous(name = "cases, in millions") +
  scale_x_discrete(name = "year") +
  labs(color = "ADI decile (1=most deprived)") +
  coord_cartesian(ylim = c(0, 10)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.4,0.7)
  ) +
  ggtitle("ADI total cases by ADI decile, by year")
f_cases_bins

# focus on most vulnerable group

d_bins_facet <- d_bins |> 
  select(starts_with("cases_"), year, pop_bins, -cases_group) |>
  pivot_longer(cols = c(-year, -pop_bins), names_to = "Type", values_to = "N", names_prefix = "cases_") |>
  mutate(Type = str_remove(Type ,"group_"))

# ADI category for most deprived groups
f_cases_bin_facet <- ggplot(
  data = d_bins_facet, 
  aes(y = N, x = year, group = Type, color = Type)) +
  geom_line(size = 2) +
  scale_y_continuous(name = "cases, in millions") +
  scale_x_discrete(name = "year") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.95,0.15) 
  ) +
  ggtitle("ADI cases by ADI decile (1=most deprived), by category and year") +
  facet_wrap(vars(pop_bins))
f_cases_bin_facet


# -----------
f_lc
f_lc_cat
f_gini_year
f_gini_year_cat
f_cases_year
f_cases_bins
f_cases_stack
f_cases_bin_facet

## What about just general rates over time? Should be similar to graph, just moderated by population changes too

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Looking at individual district progress over time, ideas: 
# > look at position changes, either by rank or pop bin change each year
# > likelihood of moving up or down

## can't rank by LSOA as not the case that each LSOA (indicated by 4-digit code) has data for each year

## Looking at area ADI rate rank change by year
d_prog_AREA <- d_full |>
  select(area_code, area_name, pop, ADI_rate, year) |> 
  arrange(area_name) |>
  
  # AREA RATE: collapse into areas, but with ADI rate per area weighted by different LSOA sizes
  mutate(
    area_name_sub = str_sub(area_name, start = 1, end = -5),
  ) |>
  group_by(area_name_sub, year) |>
  mutate(
    pop_area = sum(pop),
    pop_area_weight = pop / pop_area,
    ADI_rate_weight = ADI_rate * pop_area_weight,
    area_ADI_rate = sum(ADI_rate_weight, na.rm = TRUE)   # Some areas missing information on some categories; not many, but make sure to check
  ) |>
  ungroup() |>
  
  # get area-wide rate
  select(area_name_sub, pop, area_ADI_rate, year) |>
  group_by(area_name_sub, year) |>
  summarise(
    area_ADI_rate = max(area_ADI_rate),   # could slice, but just to be safe
    area_pop_total = sum(pop)
  ) |>
  ungroup() |>
  group_by(year) |>
  mutate(
    pop_total = sum(area_pop_total),
    area_pop_total_weight = area_pop_total / pop_total,
    area_ADI_rate_popweighted = area_ADI_rate * area_pop_total_weight
  ) |>
  ungroup()

# >> issues: Coventry has rank = 1 for 2017 but that's because it's missing information there, so more cleaning in future
# microbenchmark if row_number is quicker than: rank; and arrange by year and row number
# >area_rank = rank(area_ADI_rate),
# >arrange(year, area_ADI_rate) |>
#   group_by(year) |>
#   mutate(
#     area_rank_by_year = row_number(),
#     minus_2 = area_rank - area_rank_by_year
#   ) |>
#   ungroup()


## ---------- Rate by ranks over time
# This is different from total counts for equivalently sized population groups; no guarantee that ranked groups will be the same size

d_prog_AREA |> count(year) 

# Average area rate over time (area rates weighted either equally or by pop weight)
d_prog_AREA_calcs <- d_prog_AREA |>
  group_by(year) |>
  summarise(
    ADI_mean = mean(area_ADI_rate),
    ADI_mean_popweighted = sum(area_ADI_rate_popweighted),
    
    ADI_median = median(area_ADI_rate),
    ADI_sd = sd(area_ADI_rate),
  ) |>
  ungroup()
# Can get weighted average if needed

d_prog_AREA_calcs_long <- d_prog_AREA_calcs |> 
  select(-ADI_sd) |> 
  pivot_longer(cols = -year, names_to = "Measure", values_to = "Rate", names_prefix = "ADI_")

f_area_rate_by_year <- ggplot(
  data = d_prog_AREA_calcs_long,
  aes(y = Rate, x = year, group = Measure, color = Measure)
) +
  geom_line(size = 2) +
  scale_y_continuous(name = "ADI rate (average cases/person)") +
  scale_x_discrete(name = "years") +
  coord_cartesian(ylim = c(0.2, 0.8)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = c(0.2,0.8)
  ) +
  ggtitle("ADI rate, by year")
f_area_rate_by_year

# Check rank change over time, e.g. 2013-2021, or 2015-2021 (for future ref: some missing data could make this wonky) 
# This is relative: so we might see rank change, but then ask what's happening in absolute terms (e.g. those getting lower rank have greater increase in rate than the decrease in rate experienced by those gaining rank)

#!!! and can check average rate change by those with highest rank changes

# check <- d_prog_AREA |>
#   mutate(across
#          
#          , ~ str_remove(.x, "area_")))

## calculate area ADI_rate rank, by year
prog_bins <- 10

d_prog_AREA_ranks <- d_prog_AREA |>
  group_by(year) |>
  mutate(
    area_rank_by_year = row_number(area_ADI_rate),
    area_rank_by_year_decile = as.factor(cut_interval(area_rank_by_year, n = prog_bins, labels = FALSE))
  ) |>
  ungroup() |>
  mutate(
    area_rank_by_year_decile_band = ifelse(area_rank_by_year_decile %in% c(1,2,3), 1, 
                                           ifelse(area_rank_by_year_decile %in% c(4,5,6,7), 2, 3))
  )

d_prog_AREA_ranks |> count(area_rank_by_year_decile)
d_prog_AREA_ranks |> count(area_rank_by_year_decile_band)

d_prog_AREA_ranks_sum <- d_prog_AREA_ranks |>
  group_by(year, area_rank_by_year_decile) |>
  summarise(
    decile_rate_mean = mean(area_ADI_rate),
    decile_rate_upper = max(area_ADI_rate),
    decile_rate_lower = min(area_ADI_rate),
  )

d_prog_AREA_band_sum <- d_prog_AREA_ranks |>
  group_by(year, area_rank_by_year_decile_band) |>
  summarise(
    decile_rate_mean = mean(area_ADI_rate),
    decile_rate_upper = max(area_ADI_rate),
    decile_rate_lower = min(area_ADI_rate)
  )

f_area_rate_by_year_ranks <- 
  ggplot(
    data = d_prog_AREA_ranks_sum, 
    aes(x = year, y = decile_rate_mean, group = area_rank_by_year_decile, color = area_rank_by_year_decile )
  ) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_line(
    data = d_prog_AREA_ranks,
    aes(y = area_ADI_rate, x = year, group = area_name_sub),
    color = "grey", alpha = 0.2
  ) +
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_y_continuous(name = "average ADI rate") +
  scale_x_discrete(name = "year") +
  labs(color = "ADI rate decile rank") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "right"
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  ggtitle("ADI rate by decile rank, by year")

# theme(legend.position = c(0.2, 0.8))
f_area_rate_by_year_ranks

## ----------- rate changes by rank

# rate changes by band
year_1 <- "2020"
year_2 <- "2021"

# d_prog_AREA_band_sum_change <- d_prog_AREA_band_sum |>
#   filter(year %in% c(year_1, year_2)) |>
#   pivot_wider(names_from = year, values_from = decile_rate_mean, names_prefix = "rate_") |>
#   mutate(rate_change = rate_2021 - rate_2020)

# rate change by decile
d_prog_AREA_ranks_sum_change <- d_prog_AREA_ranks_sum |>
  rename_with(~ str_remove(.x, "decile_rate_"), cols = starts_with("decile_rate_")) |>
  pivot_wider(names_from = year, values_from = c(mean, upper, lower)) |>
  mutate(
    rate_change = mean_2021 - mean_2020,
    rate_color = ifelse(rate_change >= 0, "#69b3a2", "#b31307")
  )

# graph rate changes by rank
f_area_rate_change <- 
  ggplot(
    data = d_prog_AREA_ranks_sum_change, 
    aes(x = area_rank_by_year_decile, y = rate_change, group = rate_color, color = rate_color)
  ) +
  geom_point(size = 2) +
  geom_segment(aes(xend = area_rank_by_year_decile, y = 0, yend = rate_change)) +
  # geom_errorbar() +
  geom_hline(yintercept = 0, size = 0.25, color = "grey") +
  coord_cartesian(ylim = c(-0.02, 0.08)) +
  scale_y_continuous(name = "change in ADI rate") +
  scale_x_discrete(name = "area ADI rate decile rank") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "none"
  ) +
  ggtitle(paste0("change in ADI rate between ", year_1," and ", year_2,", by ADI decile rank"))
f_area_rate_change



# Table of rank change

d_prog_AREA_ranks_lag <- d_prog_AREA_ranks |>
  select(area_name_sub, year, area_ADI_rate, area_rank_by_year, area_rank_by_year_decile) |>
  arrange(area_name_sub, year) |>
  group_by(area_name_sub) |>
  mutate (
    rate_change = round(area_ADI_rate - lag(area_ADI_rate), digits = 5),
    rank_change = area_rank_by_year - lag(area_rank_by_year),
    rank_decile_change = as.numeric(area_rank_by_year_decile) - as.numeric(lag(area_rank_by_year_decile))
  ) |>
  ungroup() |>
  rename(
    rate = area_ADI_rate,
    rank = area_rank_by_year,
    rank_decile = area_rank_by_year_decile,
  )

d_prog_AREA_ranks_lag |> count(rank_change)
d_prog_AREA_ranks_lag |> count(rank_decile, rank_decile_change)

# make data wide
d_prog_AREA_ranks_lag_wide <- d_prog_AREA_ranks_lag |>
  pivot_wider(names_from = c(year), values_from = c(rate, rate_change), names_sep = "_")

#  can just focus on the 40 that got better and worse


d_prog_AREA_ranks_lag |> filter(year == "2021") |> count(rank_change)
d_prog_AREA_ranks_lag |> filter(year == "2021") |> count(rank_decile_change)


View(d_prog_AREA_ranks_lag |> filter(year == "2021", rank_decile_change %in% c(1,-1)))

area_id_rank_change <- d_prog_AREA_ranks_lag |> filter(year == "2021", rank_decile_change %in% c(1,-1))
  area_id_rank_change

d_prog_AREA_ranks_lag_recent <- d_prog_AREA_ranks_lag |> 
  filter(year %in% c("2020","2021"), area_name_sub %in% area_id_rank_change$area_name_sub) |>
  mutate(
    change_group = ifelse(year == "2021" & rank_decile_change == -1, 1, 
                          ifelse(year == "2021" & rank_decile_change == 1, 2,0)),
    rate_color = ifelse(rank_decile_change == 1, "#69b3a2", "#b31307")
  ) |> 
  group_by(area_name_sub) |>
  mutate(
    change_group_max = as.factor(max(change_group))
  ) |>
  ungroup()

# get upper limits of ADI rate decile bands
d_deciles_limits <- d_prog_AREA_ranks |>
  select(year, area_ADI_rate, area_rank_by_year_decile) |> 
  group_by(year, area_rank_by_year_decile) |>
  summarise(
    rank_mean = mean(area_ADI_rate),
    rank_min = min(area_ADI_rate),
    rank_max = max(area_ADI_rate)
  ) |> 
  ungroup()

f_decile_change <- ggplot(
  data = d_prog_AREA_ranks_lag_recent |> filter(change_group_max == 2),
  aes(x = year, y = rate, group = area_name_sub)
  ) +
  geom_point() +
  geom_line() +
  # geom_point(
  #   data = d_deciles_limits,
  #   aes(y = rank_max, group = NULL),
  #   color = "black", size = 4
  # ) +
  geom_errorbar(
    data = d_deciles_limits |> filter(year == "2020"),
    aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
    size = 2, width = 0.1, alpha = 0.4, position = position_nudge(x = -0.05)
  ) +
  geom_errorbar(
    data = d_deciles_limits |> filter(year == "2021"),
    aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
    size = 1, width = 0.1, alpha = 0.4, position = position_nudge(x = +0.05)
  ) +
  coord_cartesian(ylim = c(0.2, 1.5)) +
  scale_y_continuous(name = "ADI rate", breaks = seq(0, 25, 0.25)) +
  scale_x_discrete(name = "year") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("ADI rate in 2020 and 2021 for 40 areas seeing worsening of decile rank (with ADI decile bands shown)")

f_decile_change

d_select_worst <- d_prog_AREA_ranks_lag_recent |> 
  filter(year == "2021") |>
  arrange(desc(rate_change)) |>
  slice_head(n=5)

# select biggest changes only
f_decile_change_worst <- ggplot(
  data = d_prog_AREA_ranks_lag_recent |> filter(area_name_sub %in% d_select_worst$area_name_sub),
  aes(x = year, y = rate, group = area_name_sub)
  ) +
  geom_point() +
  geom_line() +
  annotate("text", x = 2, y = d_select_worst$rate, 
           label = ( paste0(d_select_worst$area_name_sub," (+",round(d_select_worst$rate_change, digits=3),")")), 
           hjust = -1) +
  # geom_point(
  #   data = d_deciles_limits,
  #   aes(y = rank_max, group = NULL),
  #   color = "black", size = 4
  # ) +
  geom_errorbar(
    data = d_deciles_limits |> filter(year == "2020"),
    aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
    size = 2, width = 0.1, alpha = 0.4, position = position_nudge(x = -0.05)
  ) +
  geom_errorbar(
    data = d_deciles_limits |> filter(year == "2021"),
    aes(y = NULL, x = year, color = area_rank_by_year_decile, ymin = rank_min, ymax = rank_max, group = NULL),
    size = 1, width = 0.1, alpha = 0.4, position = position_nudge(x = +0.05)
  ) +
  coord_cartesian(ylim = c(0.75, 1.25)) +
  scale_y_continuous(name = "ADI rate", breaks = seq(0, 25, 0.25)) +
  scale_x_discrete(name = "year") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = mainColor, size = 20),
    axis.title.x = element_text(color = yearColor, size = 20),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("ADI rate in 2020 and 2021 for 5 areas seeing worst change (with ADI decile bands shown)")

f_decile_change_worst




# print figures

f_area_rate_by_year_ranks
f_area_rate_change

f_decile_change
f_decile_change_worst

# legend.justification in theme?
