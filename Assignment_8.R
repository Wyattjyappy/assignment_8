## Assignment 8

library(tidyverse)
library(dplyr)

mass



# 1.2 ---------------------------------------------------------------------


Length2 <- dinosaur_lengths^b_values

Mass2 <-Length2 * a_values

a <- 0.73
b <- 3.63

dino_mass <- a_values * dinosaur_lengths^b_values
  
total_mass_dinos <- for (dino_mass in dinosaur_lengths) {
  
  dino_mass <- a_values * dinosaur_lengths^b_values
}

# Define the vectors of dinosaur lengths, a values, and b values

# Create an empty vector to store the calculated masses
masses <- vector()

# Use a for loop to calculate the mass for each dinosaur length
for (i in 1:length(dinosaur_lengths)) {
  mass <- a_values[i] * dinosaur_lengths[i]^b_values[i]
  masses <- c(masses, mass)
}

# Print the calculated masses
masses

system.time(dino_mass)



# 2.1 ---------------------------------------------------------------------

buoy_1987 

buoy_1988

buoy_1989

buoy_1990


# 2.2 ---------------------------------------------------------------------

start <- 1987
end <- 1992
for (year in start:end){
  path <- str_c("https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_", year, ".csv")
  print(path)
}


# 2.3 ---------------------------------------------------------------------

start <- 1987
end <- 1992
df_combined <- NULL
for (year in start:end){
  path <- str_c("https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_", year, ".csv")
  df <- read_csv(path)
  df_combined <- bind_rows(df_combined, df)
}

dim(df_combined)


# 2.4 ---------------------------------------------------------------------


df_combined

path


for (year in start:end) {
  
  ## filter the country to plot
  gap_to_plot <- df_combined %>%
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdp_per_cap)) + 
    geom_point() +
    ## add title and save
    labs(title = str_c(cntry, "GDP per capita", sep = " "))
}

start <- 1987
end <- 1992
df_combined <- NULL
 for (year in start:end){
    path <- str_c("https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_", year, ".csv")
    df <- read_csv(path)
    df_combined <- bind_rows(df_combined, df)
    
    my_plot <- ggplot(data = df_combined, aes(x = YY, y = BAR))

 }



for (year in start:end){
  path <- str_c("https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_", year, ".csv")
  df <- read_csv(path)
  df_combined <- bind_rows(df_combined, df)
}

df_combined

  
df_combined %>% 
  select(YY, MM, WVHT, WTMP) %>% 
  rename(year = "YY", month = "MM", wave_heights = "WVHT", temperatures = "WTMP")




library(dplyr)

yearly_summary <- df_combined %>% 
  select(YY, MM, WVHT, WTMP) %>% 
  rename(year = "YY", month = "MM", wave_heights = "WVHT", temperatures = "WTMP") %>% 
  group_by(year) %>% 
  summarize(avg_wave_height = mean(wave_heights, na.rm = TRUE),
            avg_temperature = mean(temperatures, na.rm = TRUE))

library(ggplot2)

combined_summary <- bind_rows(yearly_summary) %>% 
  mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>% 
  select(date, avg_wave_height, avg_temperature) 

ggplot(combined_summary,aes(x = date)) +
  geom_line(aes(y = avg_wave_height, color = "Wave Height")) +
  geom_line(aes(y = avg_temperature, color = "Temperature")) +
  labs(x = "Year", y = "Averages", color = "Variable") +
  theme(legend.position = "top")

# 2.4 ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Read in the data for 1987-1990
buoy_1987 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1987.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1988 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1988.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1989 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1989.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1990 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1990.csv', na = c("99", "999", "99.00", "999.0"))

# Combine the data
df_combined <- bind_rows(buoy_1987, buoy_1988, buoy_1989, buoy_1990)

# Select necessary columns and rename them
df_combined <- df_combined %>% 
  select(YY, MM, WVHT, WTMP) %>% 
  rename(year = YY, month = MM, wave_heights = WVHT, temperatures = WTMP)

# Summarize monthly averaged wave heights and temperatures throughout each year
yearly_summary <- df_combined %>% 
  group_by(year, month) %>% 
  summarize(avg_wave_height = mean(wave_heights, na.rm = TRUE),
            avg_temperature = mean(temperatures, na.rm = TRUE))

# Plot the variation of the monthly averages through time
ggplot(yearly_summary, aes(x = paste(year, month, "01", sep = "-"))) +
  geom_line(aes(y = avg_wave_height, color = "Wave Height")) +
  geom_line(aes(y = avg_temperature, color = "Temperature")) +
  labs(x = "Year-Month", y = "Averages", color = "Variable") +
  theme(legend.position = "top")


# NEW ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Read in the data for 1987-1990
buoy_1987 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1987.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1988 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1988.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1989 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1989.csv', na = c("99", "999", "99.00", "999.0"))
buoy_1990 <- read_csv('https://raw.githubusercontent.com/nt246/NTRES-6100-data-science/master/datasets/buoydata/44013_1990.csv', na = c("99", "999", "99.00", "999.0"))

# Combine the data
df_combined2 <- bind_rows(buoy_1987, buoy_1988, buoy_1989, buoy_1990)


df_combined3 <- df_combined2 %>% 
  select(YY, MM, WVHT, WTMP) %>% 
  rename(year = YY, month = MM, wave_heights = WVHT, temperatures = WTMP)


yearly_summary <- df_combined3 %>% 
  group_by(year, month) %>% 
  summarize(avg_wave_height = mean(wave_heights, na.rm = TRUE),
            avg_temperature = mean(temperatures, na.rm = TRUE))

yearly_summary %>% 
  ggplot(mapping = aes(x = month, y = avg_temperature, color = year)) +
  geom_line() + 
  geom_point()

yearly_summary %>% 
  ggplot(mapping = aes(x = month, y = avg_wave_height, color = year)) +
  geom_line() + 
  geom_point()
          
        









