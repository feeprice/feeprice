#load packages
library(tidyverse)
library(ggplot2)
library(here)

#load data
eno_outflows <- read.csv(
  file = here("./Data Raw/Eno_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)

flat_outflows <- read.csv(
  file = here("./Data Raw/Flat_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)

neuse_inflows <- read.csv(
  file = here("./Data Raw/Falls_Lake_outlet_Neuse_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)


