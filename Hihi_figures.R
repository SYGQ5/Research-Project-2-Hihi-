#---------------------------------------------------------
  #  MRes Research Project 2  #
  #  Hihi project #
#---------------------------------------------------------
# Creating a map of the nest box coordinates 
#------------------------------#
# Figure : nest box coordinates
ggplot(X2019_2020_Nestbox_coordinates, aes(x=longitude, y=Latitude, colour = nest)) + geom_point() + theme_ipsum() + labs(x = "Longitude", y = "Latitude")

# Refined nest box fig - nests with incubation data:
library(ggplot2)
library(sf)
library(ggspatial)
# Load geojson file for TMI, NZ
map_data <- st_read(file.choose())
# Choose geojson file for Tiritiri Matangi Island 
# Transform file so scale is the same as the coordinates 
map_data <- st_transform(map_data, crs = 4326)

#Load csv file for nest box coordinates 
csv_data <- read.csv(file.choose())
# Choose csv file containing the 2019/2020 nest coordinates

# Remove NAs in longitude and latitude columns 

cleaned_csv_data <- csv_data[!is.na(csv_data$Longitude) & !is.na(csv_data$Latitude), ]

# Transform file so scale is the same as the map polygon & into a spatial object
points_sf <- st_as_sf(
  cleaned_csv_data, 
  coords = c("Longitude", "Latitude"), 
  crs = 4326
)
special_point_sf <- st_as_sf(data.frame(long = 174.897444, lat = -36.605667), coords = c("long", "lat"), crs = 4326)

# To plot the map and then the nestbox coordinates  
ggplot() +
  # First layer: TMI Map
  geom_sf(data = map_data, fill = "honeydew", color = "darkgrey", size = 0.5) +
  
  #Second layer: CSV nest box Coordinates
  geom_sf(data = points_sf, aes(color = "Nest box"), size = 3, alpha = 0.8) +
  # Third layer: Weather station coordinate
  geom_sf(data = special_point_sf, aes(color = "Weather station"), size = 3) +
  scale_color_manual(values = c("Nest box" = "red", "Weather station" = "black")) +
  labs(color = "")+
  theme_void() + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15) 
  ) +   
  annotation_scale(
    location = "bl", 
    width_hint = 0.2, 
    text_size = 8,
    pad_x = unit(0.3, "in"), 
    pad_y = unit(0.3, "in")
  ) +  
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",        
    style = north_arrow_minimal(text_size = 8),
    pad_x = unit(0.3, "in"), 
    pad_y = unit(0.5, "in") 
  )

#------------------------------#
# On/off incubation duration plots vs day of incubation
#------------------------------#
# Load packages
library(tidyverse)
library(hrbrthemes)
setwd("~/UCL 25-26/Research Project 2/Hihi_RStudio/Data")
# Load all incubation observation data 
library(readr)
Zoe_all_inc_obs_csv <- read_csv("Zoe_all_inc_obs_csv.csv", 
                                col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                                 date_of_obsv = col_date(format = "%d/%m/%Y"), 
                                                 incubation = col_character(), time = col_time(format = "%H:%M:%S")))
View(Zoe_all_inc_obs_csv)


ggplot(Zoe_all_inc_obs_csv, aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum() 
ggplot(Model_draft_inc_temp_data, aes(x = Inc_day, y = Bout_duration_seconds, colour = Bout_type_on_off)) + geom_point() + theme_ipsum()

# On bout only
ggplot(subset(Zoe_all_inc_obs_csv, incubation %in% "on"), aes(x=day_of_inc, y=duration_seconds)) + geom_point() + theme_ipsum() + labs(title = "On bout")
ggplot(subset(Model_draft_inc_temp_data, Bout_type_on_off %in% "on"), aes(x = Inc_day, y = Bout_duration_seconds)) + geom_point() + labs(title = "On bout")

# Off bout only
ggplot(subset(Zoe_all_inc_obs_csv, incubation %in% "off"), aes(x=day_of_inc, y=duration_seconds)) + geom_point() + theme_ipsum() + labs(title = "Off bout")
ggplot(subset(Model_draft_inc_temp_data, Bout_type_on_off %in% "off"), aes(x = Inc_day, y = Bout_duration_seconds)) + geom_point() + labs(title = "Off bout")


par(mfrow=c(2,1))
ggplot(subset(Zoe_all_inc_obs_csv, incubation %in% "on"), aes(x=day_of_inc, y=duration_seconds)) + geom_point(colour = "orange", alpha = 0.3) + theme_ipsum() + labs(title = "(a) On bout", y = " Bout duration (seconds)", caption = "Day of Incubation")
ggplot(subset(Zoe_all_inc_obs_csv, incubation %in% "off"), aes(x=day_of_inc, y=duration_seconds)) + geom_point(colour = "blue", alpha = 0.3) + theme_ipsum() + labs(title = "Off bout")

# Ensure plots are in the order 'on' then 'off', left to right 
Zoe_all_inc_obs_csv$incubation <- factor(Zoe_all_inc_obs_csv$incubation, levels = c("on", "off"))
#

a <- ggplot(subset(Zoe_all_inc_obs_csv, !is.na(incubation)), aes(x=day_of_inc, y=duration_seconds, colour = incubation)) + geom_point(alpha=0.4) + scale_colour_manual(values = c("off" = "blue", "on" = "orange"))+ theme_ipsum() + labs(y = "Bout duration (seconds)", x = "Day of incubation") + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank())  
# can add + geom_smooth(method = "lm", se = FALSE, size = 1.2, color = "black") for linear trend line, remove method argument for non-linear
a + 
  facet_grid(. ~ incubation, labeller = as_labeller(c(
    "on"  = "(a) On-Bout",
    "off" = "(b) Off-Bout"
  ))) + 
  scale_x_continuous(
    limits = c(1, 16), 
    breaks = seq(1, 16, by = 1)
  ) + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# test lm model for both plots before plotting trend line?
# 1. Filter out the missing data just like you did for your plot
clean_data <- subset(Zoe_all_inc_obs_csv, !is.na(incubation))

# 2. Fit the linear model with an interaction term
model <- lm(duration_seconds ~ day_of_inc * incubation, data = clean_data)

# 3. View the full statistical summary table
summary(model)
#------------------------------#
# On/off incubation duration plots vs time of day of incubation bout
#------------------------------#
ggplot(Zoe_all_inc_obs_csv, aes(x=time, y=duration_seconds, colour = incubation)) + geom_point() + theme_ipsum()
ggplot(Model_draft_inc_temp_data, aes(x = Obs_bout_time, y = Bout_duration_seconds, colour = Bout_type_on_off)) + geom_point() + theme_ipsum()


b <- ggplot(subset(Zoe_all_inc_obs_csv, !is.na(incubation)), aes(x=time, y=duration_seconds, colour = incubation)) + geom_point(alpha = 0.4) + theme_ipsum() + labs(y = "Bout duration (seconds)", x = " Time of observation") +
     scale_colour_manual(values = c("off" = "blue", "on" = "orange")) + theme(legend.position = "none", panel.grid.minor.x = element_blank()) +
     
# can add + geom_smooth(method = "lm", se = FALSE, size = 1.2, color = "black") for linear trend line, remove method argument for non-linear
b + 
  facet_grid(. ~ incubation, labeller = as_labeller(c(
    "on"  = "(a) On-Bout",
    "off" = "(b) Off-Bout"
  )))+ 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# test lm model for both plots before plotting trend line?
# 1. Filter out the missing data just like you did for your plot
clean_data <- subset(Zoe_all_inc_obs_csv, !is.na(incubation))

# 2. Fit the linear model with an interaction term
model <- lm(duration_seconds ~ Obs_bout_time * incubation, data = clean_data)

# 3. View the full statistical summary table
summary(model)

# Example of a 24 hour cycle for a given individual 

#===============================#
# On bout duration final plot #
#===============================#
library(readr)
Model_draft_inc_temp_data <- read_csv("Model_draft_inc_temp_data.csv", 
                                      col_types = cols(First_egg_lay_date = col_date(format = "%d/%m/%Y"), 
                                                       Inc_start_date = col_date(format = "%d/%m/%Y"), 
                                                       Laying_gap = col_logical(), Hatch_date = col_date(format = "%d/%m/%Y"), 
                                                       Inc_period_length_days = col_double(), 
                                                       Obs_date = col_date(format = "%d/%m/%Y"), 
                                                       Obs_bout_time = col_time(format = "%H:%M:%S"), 
                                                       Bout_duration_seconds = col_number()))

# Ensure plots are in the order 'on' then 'off', left to right 
Model_draft_inc_temp_data$Bout_type_on_off <- factor(Model_draft_inc_temp_data$Bout_type_on_off, levels = c("on", "off"))

# Bout duration vs Female age
ggplot(on_bouts_filtered, aes(x=Female_age, y= Bout_duration_seconds)) + geom_point() + theme_ipsum() + labs(x = "Female age (years)", y = "Individual on bout duration (seconds)") + geom_smooth(se = FALSE, size = 1.2, color = "red")

a <- ggplot(subset(Model_draft_inc_temp_data, !is.na(Bout_duration_seconds) & !is.na(Inc_period_length_days)), aes(x=Female_age, y=Bout_duration_seconds, colour = Bout_type_on_off)) + geom_point(alpha=0.4) + scale_colour_manual(values = c("off" = "blue", "on" = "orange"))+ theme_ipsum() + labs(y = " Log bout duration (seconds)", x = "Female age (years)") + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank()) + scale_y_log10() + geom_smooth(method = "lm", se = FALSE, size = 1.2, color = "black") 
a + 
  facet_grid(. ~ Bout_type_on_off, labeller = as_labeller(c(
    "on"  = "(a) On-bout",
    "off" = "(b) Off-bout"
  ))) + 
  scale_x_continuous(
    limits = c(1, 8), 
    breaks = seq(1, 8, by = 1)
  ) + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#########################
# Bout proportion vs Temp 
Model_draft_inc_temp_data$Lay_date_centered <- as.numeric(scale(Model_draft_inc_temp_data$First_egg_lay_date, scale = FALSE))


ggplot(subset(Model_draft_inc_temp_data, !is.na(Bout_duration_seconds) & !is.na(Inc_period_length_days)& Bout_type_on_off %in% "on"), aes(x=First_egg_lay_date, y=Bout_duration_seconds)) + geom_point(alpha=0.4, colour="orange") + theme_ipsum() + labs(y = "Log On Bout duration (seconds)", x = "Lay date") + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank())  + scale_y_log10() + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
# On bout proportion versus 
Model_draft_prop_data <- read_csv("Model_draft_prop_data.csv", 
                                  col_types = cols(First_egg_lay_date = col_date(format = "%d/%m/%Y"), 
                                                   Inc_start_date = col_date(format = "%d/%m/%Y"), 
                                                   Obs_date = col_date(format = "%d/%m/%Y"), 
                                                   Obs_start_time = col_time(format = "%H:%M:%S"), 
                                                   Inc_period_length_days = col_double()))
View(Model_draft_prop_data)
# Subset to remove rows with N/A proportion values 
prop_filtered <- subset(Model_draft_prop_data, !is.na(On_bout_proportion) & !is.na(Inc_period_length_days) )

# Lay date centring
library(lubridate)
prop_filtered$Lay_date_centered <- as.numeric(scale(prop_filtered$First_egg_lay_date, scale = FALSE))

# Time of day scaled 
# Convert HH:MM:SS text time to decimal hours past midnight (e.g. "08:30:00" -> 8.5)
prop_filtered$Hours_Past_Midnight <- hour(hms(prop_filtered$Obs_start_time)) + 
  (minute(hms(prop_filtered$Obs_start_time)) / 60)

# Scale Time of Day (Z-score standardisation: sets mean to 0, SD to 1)
prop_filtered$Time_of_day_scaled <- as.numeric(scale(prop_filtered$Hours_Past_Midnight))

# Check that means are 0 - they are 
mean(prop_filtered$Lay_date_centered, na.rm = TRUE)
mean(prop_filtered$Time_of_day_scaled, na.rm = TRUE)


ggplot(data = prop_filtered, aes(x= Obs_Av_daily_temp_celcius, y= On_bout_prop_transformed)) + geom_point(alpha=0.4) + theme_ipsum() + labs(y = "On bout proportion", x = "Daily average ambient temperature (degrees celcius)") + 
  theme(panel.grid.minor.x = element_blank())  + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + geom_smooth(se = FALSE, size = 1.2, color = "red") + 
  scale_x_continuous(
    limits = c(10, 20), 
    breaks = seq(10, 20, by = 2)
  )
###################
# Bout proportion versus time of day 
ggplot(data = prop_filtered, aes(x= Obs_start_time, y= On_bout_prop_transformed)) + geom_point(alpha=0.4, colour = "darkred") + theme_ipsum() + labs(y = "On bout proportion", x = "Observation time of day") + 
  theme(panel.grid.minor.x = element_blank())  + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )+ geom_smooth (se = FALSE, size = 1.2, color = "black") 
###################
# Bout proportion versus IPL
ggplot(data = prop_filtered, aes(x= Inc_period_length_days, y= On_bout_prop_transformed)) + geom_point(alpha=0.4, colour = "darkgreen") + theme_ipsum() + labs(y = "On bout proportion", x = "Incubation period length (days)") + 
  theme(panel.grid.minor.x = element_blank())  + 
  theme(
    axis.title.x = element_text(hjust = 0.5, margin = margin(t = 10)), 
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
    plot.title   = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )+ geom_smooth (se = FALSE, size = 1.2, color = "black") 
