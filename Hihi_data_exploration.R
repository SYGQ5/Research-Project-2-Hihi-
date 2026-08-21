---------------------------------------------------------
#  MRes Research Project 2  #
#  Hihi bird project        #
---------------------------------------------------------
# Data exploration - incubation behaviour: investigating the variation within hihi incubation
# behaviour during the 2019-2020 breeding season on Tiritiri Matangi Island, NZ.
  
# 95 nests were monitored during this breeding season - 95 rows and 15 columns

  -------------------------------------
# Setting up environment
  rm(list = ls()) # Clean environment
  setwd("~/UCL 25-26/Research Project 2/Hihi_RStudio/Data")
  getwd() # To check working directory is the R project file 
  -------------------------------------
# Load incubation behaviour data     
# Load data, 'Zoe_Summary_incubation_data.csv' file 
  library(readr)
Zoe_Summary_incubation_data <- read.csv(file.choose())
View(Zoe_Summary_incubation_data)

# Load all incubation observations csv file 
Zoe_all_inc_obs <- read.csv(file.choose())
# Select Zoe_all_inc_obs_csv

# Load on bout incubation observations csv file
Zoe_on_bout_obs <- read.csv(file.choose())
# Select Zoe_on_bout_inc_obs_csv

# Load off bout incubation observations csv file
Zoe_off_bout_obs <- read.csv(file.choose())
# Select Zoe_off_bout_inc_obs_csv

---------------------------------------------------------
# Visual data exploration of incubation data using basic Tidyverse bar plots for discrete data
---------------------------------------------------------
library(tidyverse)

# First investigating variation within each of the variables 
# Used bar plots as each variable consists of integer, discrete numerical values 

# Number of eggs laid 
ggplot(data = Zoe_Summary_incubation_data) + geom_bar(mapping = aes(x = Number_eggs_laid))
# Normal distribution 

# Laying time (days)
ggplot(data = Zoe_Summary_incubation_data) + geom_bar(mapping = aes(x = First_egg_to_incub_days))
# Normal distribution

# Incubation period (days)
ggplot(data = Zoe_Summary_incubation_data) + geom_bar(mapping = aes(x = Incubation_to_hatch_length_days))
# Skew towards 14 days 

# Laying gap - presence/absence
ggplot(data = Zoe_Summary_incubation_data) + geom_bar(mapping = aes(x = Laying_gap_presence))
# Skew towards 0 laying gaps present 

---------------------------------------------------------
# Visual data exploration of incubation data using basic Tidyverse scatterplots for continuous data
---------------------------------------------------------
# Investigating variation of Total Observation length in Early incubation period

# Import Early incubation behaviour data set 
library(readr)
Zoe_early_incubation_observations <- read_csv("Data/Zoe_early_incubation_observations.csv")
library(tidyverse)

install.packages("hrbrthemes")
library(hrbrthemes)

# Plotting Total observation lengths per nest (Early incubation period)
ggplot(Zoe_early_incubation_observations, aes(x=day_of_inc, y=Total_obs_length_minutes, color=nest)) + geom_point() +  theme_ipsum() + labs(x = " Day of Incubation", y = " Total Observation Length (minutes)")
# **** Refine plot: add title adjust axis label position, y-axis limits, make x-axis labels integers

---------------------------------------------------------
# Visual data exploration of incubation data using basic Tidyverse histograms for continuous data
---------------------------------------------------------
# Histogram for all on/off incubation bout times 
ggplot(data = Zoe_all_inc_obs) + geom_histogram(mapping = aes(x = duration_seconds))
# Extremely left-skewed distribution, one clear mode

# Histogram for on bout incubation duration 
ggplot(data = Zoe_on_bout_obs) + geom_histogram(mapping = aes(x = duration_seconds))
# Left-skewed distribution, clear singular mode

# Histogram for off bout incubation duration 
ggplot(data = Zoe_off_bout_obs) + geom_histogram(mapping = aes(x = duration_seconds))
# Extremely left-skewed, bimodal

# Bar plot for total observation lengths (mins) for all observations 
ggplot(data = Zoe_all_inc_obs) + geom_bar(mapping = aes(x = total_obs_length_minutes))
# Normal distribution? 

# Bar plot for day of incubation of observations 
ggplot(data = Zoe_all_inc_obs) + geom_bar(mapping = aes(x = day_of_inc))

# Scatter plot of on/off bout duration (y) and day of incubation (x), legend denotes on or off bout 
ggplot(Zoe_all_inc_obs, aes(x = day_of_inc, y = duration_seconds, colour = incubation)) + geom_point() + theme_ipsum()  
---------------------------------------------------------
# Statistical data exploration of incubation behaviour - descriptive statistics
---------------------------------------------------------
# Distribution and frequency: Frequency distributions, visual representations (above).

# Measures of central tendency: mean, median, mode
  
# Number of eggs laid 
mean(Zoe_Summary_incubation_data$Number_eggs_laid)
#[1] 3.821053
median(Zoe_Summary_incubation_data$Number_eggs_laid)
#[1] 4
# Mode = 4

# Laying time (days)
mean(Zoe_Summary_incubation_data$First_egg_to_incub_days, na.rm = TRUE)
#[1] 2.946809
median(Zoe_Summary_incubation_data$First_egg_to_incub_days, na.rm = TRUE)
[1] 3
# Mode = 3

# Incubation period (days)
# Make sure data is in numeric format
Incub_period_days <- as.numeric(Zoe_Summary_incubation_data$Incubation_to_hatch_length_days)
mean(Incub_period_days, na.rm = TRUE)
# [1] 14.43182
median(Incub_period_days, na.rm = TRUE)
#[1] 14
# Mode = 14

# Laying gap presence 
# Mode = 0 

# Measures of variability: range, standard deviation, variance, interquartile range

# Using var() to measure variance

# Number of eggs laid
  var(Zoe_Summary_incubation_data$Number_eggs_laid, na.rm = TRUE)
[1] 0.4463606

# Laying time 
var(Zoe_Summary_incubation_data$First_egg_to_incub_days, na.rm = TRUE)
[1] 0.5455273

#Incubation period 
var(Incub_period_days, na.rm = TRUE)
[1] 0.3401254

# Using sd() to measure standard deviation

# Number of eggs laid
sd(Zoe_Summary_incubation_data$Number_eggs_laid)
#[1] 0.6681022

# Laying time 
sd(Zoe_Summary_incubation_data$First_egg_to_incub_days, na.rm = TRUE)
#[1] 0.7385982

#Incubation period
sd(Zoe_Summary_incubation_data$Incubation_to_hatch_length_days, na.rm = TRUE)
#[1] 0.5832027


-----------------------------------------------------
# Replicate Early incubation period behaviour data #
------------------------------------------------------
# Due to faulty camera equipment, observations on some nests during the early incubation period 
# were repeated on the same or consecutive day to the original, short observation.
# Before deciding whether to include this data, I will explore the replicated/ spliced observation data.
------------------------------------------------------
  # Import Early incubation behaviour data set 
  library(readr)
  Zoe_early_incubation_observations <- read_csv("Data/Zoe_early_incubation_observations.csv")
  library(tidyverse)
  
  install.packages("hrbrthemes")
  library(hrbrthemes)

---------------------------------------------
# Comparing on/off bout duration between early incubation observations of the same nest
# I.e. replicates/ spliced observations
---------------------------------------------
  # Filter nests with replicates 
  # Nests: B1/13B, B1/27B, B22/25, B23/8, SV/2

  # First filtered the data pertaining to Nest B1/13B, then plotted a scatter plot
  # of the two early incubation observations to compare the on/off bout duration
  # **** Refine plot: y-axis limits, make x-axis labels integers/ reduce gaps between x-axis labels

Zoe_early_incubation_observations %>% filter(nest == "B1/13B") %>% ggplot(aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() + labs(x="Day of Incubation", y="Bout Duration (seconds", title = "Nest B1/13B") 

Zoe_early_incubation_observations %>% filter(nest == "B1/27B") %>% ggplot(aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() + labs(x="Day of Incubation", y="Bout Duration (seconds", title = "Nest B1/27B") 

Zoe_early_incubation_observations %>% filter(nest == "B22/25") %>% ggplot(aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() + labs(x="Day of Incubation", y="Bout Duration (seconds", title = "Nest B22/25")

Zoe_early_incubation_observations %>% filter(nest == "B23/8") %>% ggplot(aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() + labs(x="Day of Incubation", y="Bout Duration (seconds", title = "Nest B23/8")

Zoe_early_incubation_observations %>% filter(nest == "SV/2") %>% ggplot(aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() + labs(x="Day of Incubation", y="Bout Duration (seconds", title = "Nest SV/2")  

----------------------------------
# Run Welch's Two Sample t-test #
----------------------------------

  Zoe_early_incubation_observations %>% 
  filter(nest == "B1/13B") %>% 
  t.test(duration_seconds ~ day_of_inc, data = .)

  Zoe_early_incubation_observations %>% 
  filter(nest == "B1/27B") %>% 
  t.test(duration_seconds ~ day_of_inc, data = .)
  
  Zoe_early_incubation_observations %>% 
  filter(nest == "B22/25") %>% 
  t.test(duration_seconds ~ day_of_inc, data = .)
  
  Zoe_early_incubation_observations %>% 
  filter(nest == "B23/8") %>% 
  t.test(duration_seconds ~ day_of_inc, data = .)
  
  Zoe_early_incubation_observations %>% 
  filter(nest == "SV/2") %>% 
  t.test(duration_seconds ~ day_of_inc, data = .)
  
# Nest B1/13B Results
# t-value: 0.28678
# p-value = 0.7906
# Day 2 mean = 367 seconds, Day 3 mean = 290 seconds
# df = 3.4703 
# Small sample size, therefore lacks statistical power
# 95% confidence interval:  -715.5626 to +869.5626
  
# Nest B1/27B Results
# t-value: t = 1.1738
# p-value = 0.3706
# Day 1 mean = 1405.5000, Day 2 mean = 904.3333
# df = 1.8314
# 95% confidence interval: -1507.790 to +2510.123
  
# Nest B22/25 Results
# t-value = 0.29777
# p-value = 0.7752
# Day 1 mean = 878.25, Day 3 mean = 720.00
# df = 6.4743
# 95% confidence interval: -1119.414 to 1435.914
  
# Nest B23/8 Results 
# t-value: -0.78302
# p-value = 0.44
# Day 1 mean = 435.3529, Day 2 mean = 527.4118
# df = 29.046
# 95% confidence interval: -332.4987 to 148.3810
  
# Nest SV/2 Results
# t-value = 0.68421
# p-value = 0.5201
# Day 1 mean = 1405.0, Day 2 mean = 915.5
# df = 5.8226
# 95% confidence interval: -1274.083 to +2253.083

  
#--------------------------------------------------------------------------------------  
#---------------------------------------------------------
# Data exploration - Climate data
# Investigating the variation of ambient temperature collected by NIWA at Tiri 
# lighthouse weather station on TMI during the 2019/2020 breeding season 
---------------------------------------------------------    
# Read in csv file of NIWA data 
    niwa_data <- read.csv(file.choose())
# Select the 'tiri_niwa_data_2019_2020.csv'

#---------------------------------------------------------
 # Visual data exploration using basic Tidyverse histograms 
 # for continuous ambient temperature data
#---------------------------------------------------------
# Histograms - looking at distribution of daily minimum, maximum and average temperature (celcius)
# ------------------------------  
  ggplot(data = niwa_data) + geom_histogram(mapping = aes(x = tmin))
  ggplot(data = niwa_data) + geom_histogram(mapping = aes(x = tmax))
  ggplot(data = niwa_data) + geom_histogram(mapping = aes(x = av_temp))

#---------------------------------------------------------
# Statistical data exploration - descriptive statistics
#---------------------------------------------------------
# Minimum daily temperature - tmin
------
# mean
mean(niwa_data$tmin)
# [1] 13.18889
# median
median(niwa_data$tmin)
# [1] 13.55
# mode
# ~15
# range 
range(niwa_data$tmin)
#[1]  7.2 19.3 
# = 12.1
# sd 
sd(niwa_data$tmin)
#[1] 2.937713
# variance 
var(niwa_data$tmin)
#[1] 8.630155

------
# Maximum daily temperature - tmax
------
# mean
mean(niwa_data$tmax)
# [1] 20.385
# median
median(niwa_data$tmax)
# [1] 20.85
# mode
# ~16, ~21
# range 
range(niwa_data$tmax)
#[1] 14.3 28.5
# = 14.2
# sd 
sd(niwa_data$tmax)
#[1] 3.518244
# variance 
var(niwa_data$tmax)
#[1] 12.37804

---------
# Average daily temperature - av_temp
---------
# mean
mean(niwa_data$av_temp)
# [1] 16.36672
# median
median(niwa_data$av_temp)
# [1] 16.735
# mode
# ~14, ~18
# range 
range(niwa_data$av_temp)
#[1] 10.67 22.77
# = 12.10
# sd 
sd(niwa_data$av_temp)
#[1] 3.140243
# variance 
var(niwa_data$av_temp)
#[1] 9.861125
  
# ------------------------------
# Line chart?? - geom_line

#-----------------------------------------------------------------------------------
#---------------------------------------------------------
  # Data exploration - Ibutton data
  # Investigating the variation of nest box temperature, date, time 
  # and geographical location on TMI during the 2019/2020 breeding season 
#---------------------------------------------------------

# Read in csv file of Ibutton temperature data 
library(readr)
ibutton_temp_data <- read_csv("Data/Zoe_hihi_nest microclimate_data_ibuttons.csv", 
                                                     col_types = cols(Date_time = col_datetime(format = "%d/%m/%Y %H:%M")))

# This code reads in the date time column in the date time format

# Read in csv file of Ibutton date and time data 
library(readr)
ibuttons_date_time_nest_data <- read_csv("Data/ibuttons_date_time_nest_data.csv", 
                                         col_types = cols(Start_Date = col_date(format = "%d/%m/%Y"), 
                                                          End_Date = col_date(format = "%d/%m/%Y"), 
                                                          Start_Time = col_time(format = "%H:%M"), 
                                                          End_Time = col_time(format = "%H:%M")))
View(ibuttons_date_time_nest_data)
# This code sets the date and time columns in the 'date' and 'time' format  

#---------------------------------------------------------
# Visual data exploration - ibutton meta data 
#---------------------------------------------------------
# Looking at distribution of the ibutton temperature logger data collection (date, time),
# collected every 2 minutes at 23 nest boxes
# ------------------------------  
# Temperature - continuous data so using histogram to plot distribution
ggplot(data = ibutton_temp_data) + geom_histogram(mapping = aes(x = Temperature_unit))
# Slightly left-skewed distribtion, symmetrical 

  
# Start date - discrete data so using bar plot to plot distribution 
ggplot(ibuttons_date_time_nest_data, aes(x = Start_Date)) + geom_bar()
# Left-skewed, U-shaped distribution, bimodal 

# End date - discrete data so using bar plot to plot distribution
ggplot(ibuttons_date_time_nest_data, aes(x = End_Date)) + geom_bar()
# Right-skewed distribution

# Start time - discrete data so using bar plot to plot distribution 
ggplot(ibuttons_date_time_nest_data, aes(x = Start_Time)) + geom_bar()
# Flat count, no mode 

# End Time - discrete data so using bar plot to plot distribution
ggplot(ibuttons_date_time_nest_data, aes(x = End_Time)) + geom_bar()
#  U-shaped distribution, no mode

#---------------------------------------------------------
# Visual data exploration of date and time variation of ibutton  
# temperature data logging
#---------------------------------------------------------

  ggplot(data = ibutton_date_time_data) + geom_point(mapping = aes(x = Start_Date, y = Start_Time, colour = Nest_Box_ID))
  
  ggplot(data = ibutton_date_time_data) + geom_point(mapping = aes(x = End_Date, y = End_Time, colour = Nest_Box_ID))

  ggplot(data = ibutton_date_time_data) + geom_point(mapping = aes(x = Start_Date, y = Start_Time, colour = Nest_Box_ID)) + geom_point(mapping = aes(x = End_Date, y = End_Time, colour = Nest_Box_ID))

#---------------------------------------------------------
# Statistical data exploration - descriptive statistics of ibutton meta data
#---------------------------------------------------------  
# 
# Start date/ End date/ Start time/ End time
#----------------  
  range(ibuttons_date_time_nest_data$Start_Date)
  #[1] "2019-11-11" "2020-01-02"
  
  range(ibuttons_date_time_nest_data$End_Date)
  #[1] "2019-01-12" "2020-01-16"
  
  range(ibuttons_date_time_nest_data$Start_Time)
  #Time differences in secs
  #[1] 13560 86340
  
  range(ibuttons_date_time_nest_data$End_Time)
  #Time differences in secs
  #[1]  3420 82200  

#---------------------------------------------------------
# Statistical data exploration - descriptive statistics of ibutton temperature data
#---------------------------------------------------------
#temperature_unit
#------
# mean
mean(ibutton_temp_data$Temperature_unit)
# [1] 17.76731
# median
median(ibutton_temp_data$Temperature_unit)
# [1] 17.596
# mode
# ~16
# range 
range(ibutton_temp_data$Temperature_unit)
#[1]  8.113 43.083
#= 34.97
# sd 
sd(ibutton_temp_data$Temperature_unit)
#[1] 3.151104
# variance 
var(ibutton_temp_data$Temperature_unit)
#[1] 9.929459


#---------------------------------------------------------
# Visual data exploration - ibutton temperature data 
#---------------------------------------------------------
#
# Line chart showing overall trends in ibutton data across date/time - geom_line 
ggplot(data = ibutton_temp_data, aes( x = Date_time, y = Temperature_unit)) + geom_line() + geom_point()

ggplot(data = ibutton_temp_data, aes( x = Date_time, y = Temperature_unit, group = Nest_Box_ID, colour = Nest_Box_ID)) + geom_line() + geom_point()

# Individual nest level e.g. Nest B1/13B
ggplot(subset(ibutton_temp_data, Nest_Box_ID %in% "b1_13b"),  aes( x = Date_time, y = Temperature_unit)) + geom_line() + geom_point()

#---------
# Compare ibutton data for nests with and without observation data 
#--------  
# ****need to update csv 
ggplot(subset(ibutton_temp_data, Obs_data_yes_no %in% "Yes"),  aes( x = Date_time, y = Temperature_unit)) + geom_line() + geom_point() + labs( title = "Nests with observation data")

ggplot(subset(ibutton_temp_data, Obs_data_yes_no %in% "No"),  aes( x = Date_time, y = Temperature_unit)) + geom_line() + geom_point() + labs( title = "Nests without observation data")
