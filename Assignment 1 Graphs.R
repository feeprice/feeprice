#load packages
library(tidyverse)
library(ggplot2)
library(here)
library(dplyr)

#load data
eno_outflows <- read.csv(
  file = here("./Data Raw/Eno_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)
eno_average_median <- eno_outflows %>% 
  group_by(year) %>% 
  summarise(avg_median_flow = mean(median_flow, na.rm = TRUE))
eno_average_mean <- eno_outflows %>% 
  group_by(year) %>% 
  summarise(avg_mean_flow = mean(mean_flow, na.rm = TRUE))
eno_volume_mean <- eno_outflows %>% 
  group_by(year) %>% 
  summarise(avg_vol = mean(total_volume, na.rm = TRUE))

flat_outflows <- read.csv(
  file = here("./Data Raw/Flat_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)
flat_average_median <- flat_outflows %>% 
  group_by(year) %>% 
  summarise(avg_median_flow = mean(median_flow, na.rm = TRUE))
flat_average_mean <- flat_outflows %>% 
  group_by(year) %>% 
  summarise(avg_mean_flow = mean(mean_flow, na.rm = TRUE))
flat_volume_mean <- flat_outflows %>% 
  group_by(year) %>% 
  summarise(avg_vol = mean(total_volume, na.rm = TRUE))

neuse_inflows <- read.csv(
  file = here("./Data Raw/Falls_Lake_outlet_Neuse_River_monthly_flow_data_through-2022.csv"),
  stringsAsFactors = TRUE)
neuse_average_median <- neuse_inflows %>% 
  group_by(year) %>% 
  summarise(avg_median_flow = mean(median_flow, na.rm = TRUE))
neuse_average_mean <- neuse_inflows %>% 
  group_by(year) %>% 
  summarise(avg_mean_flow = mean(mean_flow, na.rm = TRUE))
neuse_volume_mean <- neuse_inflows %>% 
  group_by(year) %>% 
  summarise(avg_vol = mean(total_volume, na.rm = TRUE))

#Eno graphs - 1985 and beyond
eno_filtered <- eno_outflows %>% 
  filter(year >= 1985)
eno_avemean_filtered <- eno_average_mean %>% 
  filter(year >= 1985)
eno_avemedian_filtered <- eno_average_median %>% 
  filter(year >= 1985)
eno_vol_filtered <- eno_volume_mean %>% 
  filter(year >= 1985)

ggplot(data = (eno_filtered)) +
  geom_col(data = eno_filtered, aes(x = year, y = median_flow)) +
  labs(title = "Median Eno Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = eno_avemedian_filtered, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Eno Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")

ggplot(data = eno_filtered) +
  geom_col(data = eno_filtered, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Eno Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")
ggplot(data = eno_avemean_filtered, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Eno Outflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")

ggplot(data = eno_vol_filtered) +
  geom_col(data = eno_vol_filtered, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = eno_vol_filtered, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Eno graphs - all years
ggplot(data = (eno_outflows)) +
  geom_col(data = eno_outflows, aes(x = year, y = median_flow)) +
  labs(title = "Median Eno Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = eno_average_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Eno Outflow", x = "Year", y = "Median Inflow (in Acre-Feet)")

ggplot(data = eno_outflows) +
  geom_col(data = eno_outflows, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Eno Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")
ggplot(data = eno_average_mean, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Eno Outflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")

ggplot(data = eno_volume_mean) +
  geom_col(data = eno_volume_mean, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = eno_volume_mean, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Eno graphs - 2000
eno_2000 <- eno_outflows %>% 
  filter(year >= 2000)
eno_2000_median <- eno_average_median %>% 
  filter(year >= 2000)
eno_2000_mean <- eno_average_mean %>% 
  filter(year >= 2000)
eno_2000_volume <- eno_volume_mean %>% 
  filter(year >= 2000)

ggplot(data = eno_2000) +
  geom_col(data = eno_2000, aes(x = year, y = median_flow)) +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = eno_2000_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")

ggplot(data = eno_2000) +
  geom_col(data = eno_2000, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")
ggplot(data = eno_2000_mean, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")

ggplot(data = eno_2000_volume) +
  geom_col(data = eno_2000_volume, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = eno_2000_volume, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Eno Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")



#Flat graphs - 1985 and beyond
flat_filtered <- flat_outflows %>% 
  filter(year >= 1985)
flat_avemedian_filtered <- flat_average_median %>% 
  filter(year >= 1985)
flat_avemean_filtered <- flat_average_mean %>% 
  filter(year >= 1985)
flat_vol_filtered <- flat_volume_mean %>% 
  filter(year >= 1985)

ggplot(data = (flat_filtered)) +
  geom_col(data = flat_filtered, aes(x = year, y = median_flow)) +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = flat_avemedian_filtered, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")

ggplot(data = flat_filtered) +
  geom_col(data = flat_filtered, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")
ggplot(data = flat_avemean_filtered, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")

ggplot(data = flat_vol_filtered) +
  geom_col(data = flat_vol_filtered, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = flat_vol_filtered, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Flat graphs - all years
ggplot(data = (flat_outflows)) +
  geom_col(data = flat_outflows, aes(x = year, y = median_flow)) +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = flat_average_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")

ggplot(data = flat_volume_mean) +
  geom_col(data = flat_volume_mean, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = flat_volume_mean, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Flat graphs - 2000
flat_2000 <- flat_outflows %>% 
  filter(year >= 2000)
flat_2000_median <- flat_average_median %>% 
  filter(year >= 2000)
flat_2000_mean <- flat_average_mean %>% 
  filter(year >= 2000)
flat_2000_volume <- flat_volume_mean %>% 
  filter(year >= 2000)

ggplot(data = flat_2000) +
  geom_col(data = flat_2000, aes(x = year, y = median_flow)) +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")
ggplot(data = flat_2000_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Flat Outflow", x = "Year", y = "Median Outflow (in Acre-Feet)")

ggplot(data = flat_2000) +
  geom_col(data = flat_2000, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")
ggplot(data = flat_2000_mean, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Outflow", x = "Year", y = "Mean Outflow (in Acre-Feet)")

ggplot(data = flat_2000_volume) +
  geom_col(data = flat_2000_volume, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = flat_2000_volume, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Flat Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Neuse graphs - all years
ggplot(data = (neuse_inflows)) +
  geom_col(data = neuse_inflows, aes(x = year, y = median_flow)) +
  labs(title = "Median Neuse Inflow", x = "Year", y = "Median Inflow (in Acre-Feet)")
ggplot(data = neuse_average_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Neuse Inflow", x = "Year", y = "Median Inflow (in Acre-Feet)")

ggplot(data = neuse_inflows) +
  geom_col(data = neuse_inflows, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Neuse Inflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")
ggplot(data = neuse_average_mean, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Neuse Inflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")

ggplot(data = neuse_volume_mean) +
  geom_col(data = neuse_volume_mean, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Neuse Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = neuse_volume_mean, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Neuse Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

#Neuse graphs - 2000 and beyond
neuse_2000 <- neuse_inflows %>% 
  filter(year >= 2000)
neuse_2000_median <- neuse_average_median %>% 
  filter(year >= 2000)
neuse_2000_mean <- neuse_average_mean %>% 
  filter(year >= 2000)
neuse_2000_volume <- neuse_volume_mean %>% 
  filter(year >= 2000)

ggplot(data = (neuse_2000)) +
  geom_col(data = neuse_2000, aes(x = year, y = median_flow)) +
  labs(title = "Median Neuse Inflow", x = "Year", y = "Median Inflow (in Acre-Feet)")
ggplot(data = neuse_2000_median, aes(x = year, y = avg_median_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Median Neuse Inflow", x = "Year", y = "Median Inflow (in Acre-Feet)")

ggplot(data = neuse_2000) +
  geom_col(data = neuse_2000, aes(x = year, y = mean_flow)) +
  labs(title = "Mean Neuse Inflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")
ggplot(data = neuse_2000_mean, aes(x = year, y = avg_mean_flow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Neuse Inflow", x = "Year", y = "Mean Inflow (in Acre-Feet)")

ggplot(data = neuse_2000_volume) +
  geom_col(data = neuse_2000_volume, aes(x = year, y = avg_vol)) +
  labs(title = "Mean Neuse Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")
ggplot(data = neuse_2000_volume, aes(x = year, y = avg_vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Mean Neuse Volume", x = "Year", y = "Mean Volume (in Acre-Feet)")

  

