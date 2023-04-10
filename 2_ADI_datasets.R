# Project:      ADI exploration
# File title:   2 -- Additional datasets
# Author:       Matt Chennells

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggplot2)
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



# ==========  additional datasets  ==========












