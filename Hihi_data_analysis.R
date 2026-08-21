#---------------------------------------------------------
#  MRes Research Project 2  #
#  Hihi bird project        #
#---------------------------------------------------------
# Statistical analysis #
# Using linear mixed models from the 'lme4' package

#-------------------------------------
# Setting up environment
rm(list = ls()) # Clean environment
setwd("~/UCL 25-26/Research Project 2/Hihi_RStudio/Data")
getwd() # To check working directory is the R project file > Data

install.packages("lme4")
remotes::install_github("timnewbold/StatisticalModels")

library(ggplot2)
library(lme4)
library(lmerTest)
library(dplyr)
library(hrbrthemes)

#-------------------------------------
# Testing for correlation between fixed effect variables - as multicollinearity 
# can impact later linear mixed model outputs

# Import data for model: dataset combining incubation and climate variables

library(readr)
Model_draft_inc_temp_data <- read_csv("Model_draft_inc_temp_data.csv", 
                                      col_types = cols(First_egg_lay_date = col_date(format = "%d/%m/%Y"), 
                                                       Inc_start_date = col_date(format = "%d/%m/%Y"), 
                                                       Laying_gap = col_logical(), Hatch_date = col_date(format = "%d/%m/%Y"), 
                                                       Inc_period_length_days = col_double(), 
                                                       Obs_date = col_date(format = "%d/%m/%Y"), 
                                                       Obs_bout_time = col_time(format = "%H:%M:%S"), 
                                                       Bout_duration_seconds = col_number()))
View(Model_draft_inc_temp_data)

#---------------------------------------------
# Daily ambient temperature at the observation dates 
ggplot(Model_draft_inc_temp_data, aes(x=Obs_date, y= Obs_Niwa_daily_av_ambient_temp_celcius)) + geom_point()

#Tested correlation:
# Convert date to Julian day 
Model_draft_inc_temp_data$julian_day <- as.numeric(format(Model_draft_inc_temp_data$Obs_date, "%j"))
# Run pearson's correlation test 
cor.test(Model_draft_inc_temp_data$Obs_Niwa_daily_av_ambient_temp_celcius, Model_draft_inc_temp_data$julian_day, method = "pearson")

#-----------------------------------------------------
# Daily ambient temperature (lay and obs days) at the first egg lay dates
ggplot(Model_draft_inc_temp_data, aes(x=First_egg_lay_date, y= First_egg_Niwa_daily_av_ambient_temp_celcius)) + geom_point()
#/
ggplot(Model_draft_inc_temp_data, aes(x=First_egg_lay_date, y= Lay_av_temp_celcius)) + geom_point()
# Tested correlation:
# Converted lay date to Julian day (1 to 365)
Model_draft_inc_temp_data$julian_day <- as.numeric(format(Model_draft_inc_temp_data$First_egg_lay_date, "%j"))
# Then ran cor.test()
cor.test(Model_draft_inc_temp_data$Lay_av_temp_celcius, Model_draft_inc_temp_data$julian_day, method = "spearman")

cor.test(Model_draft_inc_temp_data$Obs_Niwa_daily_av_ambient_temp_celcius, Model_draft_inc_temp_data$julian_day, method = "spearman")

#-----------------------------------------------------
# Daily ambient temp at first egg lay date over clutch size 
ggplot(Model_draft_inc_temp_data, aes(x=Clutch_size, y= First_egg_Niwa_daily_av_ambient_temp_celcius)) + geom_point()
Model_draft_inc_temp_data$julian_day <- as.numeric(format(Model_draft_inc_temp_data$First_egg_lay_date, "%j"))
cor.test(Model_draft_inc_temp_data$Clutch_size, Model_draft_inc_temp_data$julian_day, method = "pearson")

# Daily ambient temperature at the observation dates plot against bout durations, with on and off bouts colour coded
ggplot(Model_draft_inc_temp_data, aes(x=Bout_duration, y= Obs_Niwa_daily_av_ambient_temp_celcius, colour = Bout_type_on_off)) + geom_point() +  theme_ipsum()

#Daily ambient temperature at the observation dates plot against bout durations, with day of incubation colour coded
ggplot(Model_draft_inc_temp_data, aes(x=Bout_duration_seconds, y=Obs_Niwa_daily_av_ambient_temp_celcius, colour = Inc_day)) + geom_point() + theme_ipsum() + labs(x = "Bout duration (seconds)", y = " Daily average ambient temperature (celcius)")
# ???separate by incubation period/ bout type ???
# Early incubation
# Middle incubation 
# Late incubation

# on bout 
ggplot(subset(Model_draft_inc_temp_data, Bout_type_on_off %in% "on"), aes(x=Bout_duration_seconds, y=Obs_Niwa_daily_av_ambient_temp_celcius, colour = Inc_day)) + geom_point() + theme_ipsum() + labs(x = "On-bout duration (seconds)", y = " Daily average ambient temperature (celcius)")
# off bout
ggplot(subset(Model_draft_inc_temp_data, Bout_type_on_off %in% "off"), aes(x=Bout_duration_seconds, y=Obs_Niwa_daily_av_ambient_temp_celcius, colour = Inc_day)) + geom_point() + theme_ipsum() + labs(x = "Off-bout duration (seconds)", y = " Daily average ambient temperature (celcius)")

#---------------------------------------
# Clutch size vs ambient temp
# lay date
cor.test(Model_draft_inc_temp_data$Clutch_size, Model_draft_inc_temp_data$Lay_av_temp_celcius, method = "pearson")
# obs date
cor.test(Model_draft_inc_temp_data$Clutch_size, Model_draft_inc_temp_data$Obs_Niwa_daily_av_ambient_temp_celcius, method = "pearson")

#----------------------------------------
# female age vs lay date
cor.test(Model_draft_inc_temp_data$Female_age, 
         Model_draft_inc_temp_data$julian_day, 
         method = "spearman")

#----------------------------------------
# Time of day - correlation test with ambient temp and lay date 
# Scale time of day 
library(lubridate)

# Convert HH:MM:SS to decimal hours (e.g., 08:30:00 becomes 8.5)
on_bouts$Hours_Past_Midnight <- hour(hms(on_bouts$Obs_bout_time)) + 
  (minute(hms(on_bouts$Obs_bout_time)) / 60)

# Scale the decimal hours (Z-score standardisation)
on_bouts$Time_of_day_scaled <- as.numeric(scale(on_bouts$Hours_Past_Midnight))

# Check that the mean is 0 and standard deviation should be 1
summary(on_bouts$Time_of_day_scaled)
# Mean = 0, sd =1 means this has worked 

#=====================================================================================  
#--------------
# LMER 
  library(lubridate)
  library(car)
#===================
# On bout LMER
#===================
# Make sure truncated bouts are excluded, any rows with NA for incubation period length and only on bout durations are selected 
  on_bouts_filtered <- subset(Model_draft_inc_temp_data, Bout_type_on_off == "on"& !is.na(Bout_duration_seconds) & !is.na(Inc_period_length_days))

# Centre lay date
  on_bouts_filtered$Lay_date_centered <- as.numeric(scale(on_bouts_filtered$First_egg_lay_date, scale = FALSE))
  

# Scale time for model
  on_bouts_filtered$Hours_Past_Midnight <- hour(hms(on_bouts_filtered$Obs_bout_time)) + 
    (minute(hms(on_bouts_filtered$Obs_bout_time)) / 60)
  
  on_bouts_filtered$Time_of_day_scaled <- as.numeric(scale(on_bouts_filtered$Hours_Past_Midnight))
  
# Run LMER 
  on_bout_revised_model <- lmer(Bout_duration_seconds ~ 
                          Obs_Niwa_daily_av_ambient_temp_celcius + 
                          Female_age + 
                          Clutch_size + 
                          Lay_date_centered +  
                          Time_of_day_scaled + 
                          Total_obs_duration_minutes +
                            Inc_period_length_days + 
                          (1 | Female_ID), 
                        data = on_bouts_filtered)
  
# Run VIF of model to check for multicollinearity 
  vif(on_bout_revised_model)
# Results: All under 3.0 threshold 
  Obs_Niwa_daily_av_ambient_temp_celcius                             Female_age 
  1.212415                               1.364614 
  Clutch_size                      Lay_date_centered 
  1.215662                               1.432433 
  Time_of_day_scaled             Total_obs_duration_minutes 
  1.022307                               1.087109 
  Inc_period_length_days 
  1.270241                           1.060139 

# Run summary of revised model for on bout 
  summary(on_bout_revised_model)
# Results
# P Values
# Female age p=0.00888
# Observation length p=0.04154

# Correlation
# No high correlation values (greater than 0.7)
  
# Scaled residuals 
# max = 6.6319 so extremely right-skewed and response variable needs to be 
# log transformed 
  on_bout_revised_model1 <- lmer(log(Bout_duration_seconds) ~ 
                                   Obs_Niwa_daily_av_ambient_temp_celcius + 
                                   Female_age + 
                                   Clutch_size + 
                                   Lay_date_centered +  
                                   Time_of_day_scaled + 
                                   Total_obs_duration_minutes +
                                   Inc_period_length_days +
                                   (1 | Female_ID), 
                                 data = on_bouts_filtered)
vif(on_bout_revised_model1)
# VIF scores are all less than 1.5

summary(on_bout_revised_model1)
# Results 
# residual scale down from max = 6.6319 to max = 2.4712

# P values #
# intercept - significant p = 0.00874
# Temperature - not significant p = 0.68770
# Female age - significant p = 0.00455
# Clutch size - not significant p = 0.51732
# Lay date centred - significant  p = 0.08225
# Time of day scaled - not significant p = 0.79714
# Total observation duration - not significant p = 0.22260
# Incubation period length - not significant p = 0.46583

# Correlation
# No high values 

# AIC comparison
AIC(on_bout_revised_model, on_bout_revised_model1)

#df       AIC
#on_bout_revised_model  10 3436.758
#on_bout_revised_model1 10  635.267

# Checking model assumptions and diagnostics 

install.packages("performance")
library(performance)
check_model(on_bout_revised_model1)
check_normality(on_bout_revised_model1) #Warning: Non-normality of residuals detected (p < .001)
check_heteroscedasticity(on_bout_revised_model1) #OK: Error variance appears to be homoscedastic (p = 0.237)
check_outliers(on_bout_revised_model1) #OK: No outliers detected.- Based on the following method and threshold: cook (0.921).- For variable: (Whole model)
check_singularity(on_bout_revised_model1) # False result
check_collinearity(on_bout_revised_model1) # Low correlation, all VIF < 1.5

# Run Q-Q plot to check reported non-normality of residuals 
qqnorm(resid(on_bout_revised_model1))
qqline(resid(on_bout_revised_model1))
# Plot follows expected residuals, but points at either tail aren't straight 
# This is okay to continue

#--------------
# Comparison of model performance with addition and removal of variables  
# Stepwise model comparison method
#--------------
# Centre temperature variable to run the quadratic model
on_bouts_filtered$Temp_centered <- scale(on_bouts_filtered$Obs_Niwa_daily_av_ambient_temp_celcius, scale = FALSE)

# Global: Full model with all variables with REML = FALSE - enables comparison of AIC scores between models with different fixed effects 
on_global  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Test removing random effect before continuing 
on_global_lm  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = on_bouts_filtered)

AIC(on_global, on_global_lm)
df      AIC
on_global    10 598.3736 # continue with lmer w random effect 
on_global_lm  9 600.2123

# Round 1
# m1: Remove Temperature
on_m1  <- lmer(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m2: Remove Female Age 
on_m2  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m3: Remove Clutch size 
on_m3  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m4: Remove Lay date
on_m4  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m5: Remove Time of day 
on_m5  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m6: Remove Total observation duration 
on_m6  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# m7: Remove Incubation period length
on_m7  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

AIC(on_global, on_m1, on_m2, on_m3, on_m4, on_m5, on_m6, on_m7)

#df      AIC
on_global 10 598.3736
on_m1      9 596.5187 #2 Temp
on_m2      9 605.8180 # Largest delta AIC indicates female age is a strong variable
on_m3      9 596.8651 #3 Clutch size
on_m4      9 599.8550
on_m5      9 596.4720 #1 Time of day
on_m6      9 597.3946
on_m7      9 597.0982

# Removing Time of day variable reduced the AIC most 

#Round 2
# For the next round of model comparisons, I will start with "on_m5" as the base
# on_m5  <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

on_m5_m1 <- on_m5 # Model with Time of day variable removed
#Model with Time of day and Av Temp removed 
on_m5_m2  <- lmer(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Model with Time of day and Female age removed
on_m5_m3 <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Model with Time of day and Clutch size removed
on_m5_m4 <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Model with Time of day and Lay date centered removed
on_m5_m6 <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Model with Time of day and Total observation duration removed
on_m5_m7 <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# Model with Time of day and Incubation period length removed
on_m5_m8 <- lmer(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes+ (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

AIC(on_m5_m1, on_m5_m2, on_m5_m3, on_m5_m4, on_m5_m6, on_m5_m7, on_m5_m8)
#          df      AIC
on_m5_m1  9 596.4720
on_m5_m2  8 594.6256 #1 Temp: Lowest AIC - largest drop in AIC from starting model in this round
on_m5_m3  8 604.1501 # Biggest increase in AIC with Female age removal again - indicates a strong variable
on_m5_m4  8 595.0073 #2 Clutch size 
on_m5_m6  8 597.9006
on_m5_m7  8 595.5285
on_m5_m8  8 595.1917 #3 Incubation period length

# Round 3
# on_m5_m2 - Model with Time of day and Temp removed becomes starting model for this round
# on_m5_m2  <- lmer(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
on_final_1 <- on_m5_m2
# This time the #2 & #3 variables are removed, followed by both to create the final round of comparisons

# - Time of day, Temp and Clutch size removed 
on_final_2 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# - Time of day, Temp and Incubation period length removed 
on_final_3 <- lmer(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# - Time of day, Temp, Clutch size, and Incubation period length removed
on_final_4 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# Round 3 Model comparison
AIC(on_final_1, on_final_2, on_final_3, on_final_4)

#           df      AIC
on_final_1  8 594.6256
on_final_2  7 593.1320 #2 delta AIC <2 of on_final_4
on_final_3  7 593.3241 #3 delta AIC <2 of on_final_4
on_final_4  6 591.5884 #1 

#Round 4
# On_final_4 becomes On_final_4_1 - Time of day, Temp, Clutch size, and Incubation period length removed
on_final_4_1 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# on_final_4_2 - Time of day, Temp, Clutch size, Incubation period length and Female Age removed
on_final_4_2 <- lmer(log(Bout_duration_seconds) ~ Lay_date_centered + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# on_final_4_3 - Time of day, Temp, Clutch size, Incubation period length and Lay date removed
on_final_4_3 <- lmer(log(Bout_duration_seconds) ~ Female_age + Total_obs_duration_minutes + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
# on_final_4_4 - Time of day, Temp, Clutch size, Incubation period length and Total Obs duration removed
on_final_4_4 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

AIC(on_final_4_1, on_final_4_2, on_final_4_3, on_final_4_4)
df      AIC
on_final_4_1  6 591.5884 #2
on_final_4_2  5 600.9914
on_final_4_3  5 592.2361 #3
on_final_4_4  5 590.8466 #1

#Round 5 
# on_final_4_4_1 <- on_final_4_4 - Time of day, Temp, Clutch size, Incubation period length and Total Obs duration removed
on_final_4_4_1 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
#on_final_4_4_2  - Time of day, Temp, Clutch size, Incubation period length, Total Obs duration and Female age removed
on_final_4_4_2 <- lmer(log(Bout_duration_seconds) ~ Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)
#on_final_4_4_3 - Time of day, Temp, Clutch size, Incubation period length, Total Obs duration and Lay date removed
on_final_4_4_3 <- lmer(log(Bout_duration_seconds) ~ Female_age +(1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

AIC(on_final_4_4_1, on_final_4_4_2, on_final_4_4_3)
df      AIC
on_final_4_4_1  5 590.8466 #1 Female age and Lay date centred produced the lowest AIC score
on_final_4_4_2  4 601.3149
on_final_4_4_3  4 592.5330 #2

# on_final_4_4_1 <- on_final_4_4 - Time of day, Temp, Clutch size, Incubation period length and Total Obs duration removed
on_final_4_4_1 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# Test lm of on_final_4
on_final_4_4_1_lm <- lm(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered, data = on_bouts_filtered)
# Test quadratic of on_final_4 - age as quadratic term 
on_final_4_4_1_quad_age <- lmer(log(Bout_duration_seconds) ~ Female_age + I(Female_age^2) + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = FALSE)

# Compare AIC 
AIC(on_final_4_4_1, on_final_4_4_1_lm, on_final_4_4_1_quad_age)

                     df      AIC
on_final_4_4_1       5 590.8466 #1 Lowest AIC score 
on_final_4_lm        4 593.7223
on_final_4_quad_age  6 592.3075

anova(on_final_4_4_1, on_final_4__4_1_quad_age) # Chi squared test value was insignifcant, p = 0.5391

##===================
# Final model is on_final_4_4_1
# on_final_4_4_1 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = TRUE)
#====================
# Rerun final model with REML = TRUE then run summary(on_final_4_4_1)
 on_final_4_4_1 <- lmer(log(Bout_duration_seconds) ~ Female_age + Lay_date_centered + (1 | Female_ID), data = on_bouts_filtered, REML = TRUE)

summary(on_final_4_4_1)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: log(Bout_duration_seconds) ~ Female_age + Lay_date_centered +      (1 | Female_ID)
Data: on_bouts_filtered

REML criterion at convergence: 596.2

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-5.3672 -0.5029  0.1019  0.6448  2.6683 

Random effects:
  Groups    Name        Variance Std.Dev.
Female_ID (Intercept) 0.08546  0.2923  
Residual              0.76698  0.8758  
Number of obs: 220, groups:  Female_ID, 45

Fixed effects:
  Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)        5.864249   0.156130 35.241247  37.560  < 2e-16 ***
  Female_age         0.211862   0.056644 43.139144   3.740 0.000538 ***
  Lay_date_centered -0.013727   0.007246 58.057110  -1.894 0.063145 .  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
  (Intr) Feml_g
Female_age  -0.863       
Ly_dt_cntrd -0.330  0.321

# Get R2 values 
library(performance)
r2_nakagawa(on_final_4_4_1)
# R2 for Mixed Models

Conditional R2: 0.230
Marginal R2: 0.145
#===================
# Off bout LMER
#===================
# Make sure truncated bouts are excluded and only on bout durations are selected, also removes any rows with NAs for incubation period length
  off_bouts_filtered <- subset(Model_draft_inc_temp_data, Bout_type_on_off == "off"& !is.na(Bout_duration_seconds)& !is.na(Inc_period_length_days) )
  
# Centre lay date
  off_bouts_filtered$Lay_date_centered <- as.numeric(scale(off_bouts_filtered$First_egg_lay_date, scale = FALSE))
  
  
# Scale time for model
  off_bouts_filtered$Hours_Past_Midnight <- hour(hms(off_bouts_filtered$Obs_bout_time)) + 
    (minute(hms(off_bouts_filtered$Obs_bout_time)) / 60)
  
  off_bouts_filtered$Time_of_day_scaled <- as.numeric(scale(off_bouts_filtered$Hours_Past_Midnight))
  
# Run LMER 
  off_bout_revised_model <- lmer(Bout_duration_seconds ~ 
                                  Obs_Niwa_daily_av_ambient_temp_celcius + 
                                  Female_age + 
                                  Clutch_size + 
                                  Lay_date_centered +  
                                  Time_of_day_scaled + 
                                  Total_obs_duration_minutes + Inc_period_length_days
                                 + (1 | Female_ID), 
                                data = off_bouts_filtered)
vif(off_bout_revised_model)
# Results: all scores are below 3.0 point threshold for correlation 
  
summary(off_bout_revised_model)
  # Scaled residuals: max = 12.9732 - extreme right skew, so response variable needs to be log transformed
  
  off_bout_revised_model1 <- lmer(log(Bout_duration_seconds) ~ 
                                   Obs_Niwa_daily_av_ambient_temp_celcius + 
                                   Female_age + 
                                   Clutch_size + 
                                   Lay_date_centered +  
                                   Time_of_day_scaled + 
                                   Total_obs_duration_minutes + 
                                    Inc_period_length_days +
                                   (1 | Female_ID), 
                                 data = off_bouts_filtered) 
 
# Compare off_bout_revised to log transformed off_bout_revised_model1
  AIC(off_bout_revised_model, off_bout_revised_model1)
# Results 
  df      AIC
  off_bout_revised_model  10 3978.649
  off_bout_revised_model1 10  650.526 # Much improved AIC by well over 2 points

#Check scaled residuals of log transformed model  
  summary(off_bout_revised_model1) 
  # Singular fit warning 
  # Scaled residuals: max = 4.0029 - log transformation is a suitable fix for right skew
  # Error reported: singular fit so zero variance between groups is reported
  # Random effects: variance of Female ID is zero 
  
#Check all model assumptions are met 
library(performance)
check_model(off_bout_revised_model1)
check_normality(off_bout_revised_model1) #Warning: Non-normality of residuals detected (p < .001)
check_heteroscedasticity(off_bout_revised_model1) #OK: Error variance appears to be homoscedastic (p = 0.675).
check_outliers(off_bout_revised_model1) #OK: No outliers detected.- Based on the following method and threshold: cook (0.92).- For variable: (Whole model)
check_singularity(off_bout_revised_model1) # True result
check_collinearity(off_bout_revised_model1) # Low correlation, all VIF < 1.5

# Run Q-Q plot to check reported non-normality of residuals 
qqnorm(resid(off_bout_revised_model1))
qqline(resid(off_bout_revised_model1)) 
# Plot follows expected residuals, but points at either tail aren't straight 
# This is okay to continue

# As singularity is present in my model,  my model is overfitted and 
# too complex
# Check VIF for collinearity
vif(off_bout_revised_model1)
# Results - all scored < 1.5
# As in the earlier summary of the model  # Random effects: variance of Female ID is zero
# Female ID holds no explanatory power in this model, so i will remove 
# it and use a simpler lm model instead

off_bout_revised_model1_lm <- lm(log(Bout_duration_seconds) ~ 
                                  Obs_Niwa_daily_av_ambient_temp_celcius + 
                                  Female_age + 
                                  Clutch_size + 
                                  Lay_date_centered +  
                                  Time_of_day_scaled + 
                                  Total_obs_duration_minutes + 
                                  Inc_period_length_days, 
                                data = off_bouts_filtered) 
# Retest VIF
# VIF<1.5

# Retest summary - scaled residuals: Min-3.7665 to Max2.9055

#Check all model assumptions are met 
library(performance)
check_model(off_bout_revised_model1_lm)
check_normality(off_bout_revised_model1_lm) #Warning: Non-normality of residuals detected (p < .001)
check_heteroscedasticity(off_bout_revised_model1_lm) #OK: Error variance appears to be homoscedastic (p = 0.078).
check_outliers(off_bout_revised_model1_lm) #OK: No outliers detected.- Based on the following method and threshold: cook (0.92).- For variable: (Whole model)
check_singularity(off_bout_revised_model1_lm) # False result - improvement 
check_collinearity(off_bout_revised_model1_lm) # Low correlation, all VIF < 1.5

# Run Q-Q plot to check reported non-normality of residuals 
qqnorm(resid(off_bout_revised_model1_lm))
qqline(resid(off_bout_revised_model1_lm)) 
# Plot follows expected residuals, but points at either tail aren't straight 
# This is okay to continue
#-----------------------------
# Model Comparisons - addition and removal of varibles to compare performance
# Using stepwise comparison method 
#-----------------------------
# Centre temperature variable to run the quadratic model
  off_bouts_filtered$Temp_centered <- scale(off_bouts_filtered$Obs_Niwa_daily_av_ambient_temp_celcius, scale = FALSE)
  
# Global: Full model with all variables  
# Same as off_bout_revised_model1, but with REML = FALSE for comparability
off_global <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered +  Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered) 
# Round 1
# m1: Remove Temperature
off_m1  <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered)

# m2: Remove Female Age 
off_m2  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered)

# m3: Remove Clutch size 
off_m3  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered)

# m4: Remove Lay date
off_m4  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Time_of_day_scaled + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered)

# m5: Remove Time of day 
off_m5  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes + Inc_period_length_days, data = off_bouts_filtered)

# m6: Remove Total observation duration 
off_m6  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days, data = off_bouts_filtered)

# m7: Remove Incubation period length
off_m7  <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)

AIC(off_global, off_m1, off_m2, off_m3, off_m4, off_m5, off_m6, off_m7)
df      AIC
off_global  9 603.0864 #7 Within AIC<2 of #1
off_m1      8 601.2820 #4
off_m2      8 606.9358 # Biggest increase in AIC, indicates Female age as strongest influence
off_m3      8 601.8550 #6
off_m4      8 601.2056 #3
off_m5      8 601.4122 #5
off_m6      8 601.1975 #2
off_m7      8 601.1127 #1 Lowest AIc when Incubation period length removed
# All models but off_m2 have AIC scores within AIC<2 of #1

#Round 2
# m7_m1: Remove Incubation period length
# off_m7_m1 <- off_m7
off_m7_m1 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m2: Remove Incubation period length and Temp
off_m7_m2 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m3: Remove Incubation period length and Female age 
off_m7_m3 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Clutch_size + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m4: Remove Incubation period length and Clutch size 
off_m7_m4 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m5: Remove Incubation period length and Lay date
off_m7_m5 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Time_of_day_scaled + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m6: Remove Incubation period length and Time of day obs
off_m7_m6 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Total_obs_duration_minutes, data = off_bouts_filtered)
#m7_m7: Remove Incubation period length and Total obs duration
off_m7_m7 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled, data = off_bouts_filtered)

AIC(off_m7_m1,off_m7_m2, off_m7_m3, off_m7_m4, off_m7_m5, off_m7_m6, off_m7_m7)
           df      AIC
off_m7_m1  8 601.1127 #6
off_m7_m2  7 599.3011 #3 Next when temp also removed 
off_m7_m3  7 605.8941 # Largest increase in AIC due to Female age removal again
off_m7_m4  7 599.8550 #5
off_m7_m5  7 599.2153 #2 Next lowest when lay date also removed
off_m7_m6  7 599.4285 #4
off_m7_m7  7 599.2142 #1 Lowest AIC when Incubation period length and Total observation duration removed

#Round 3 - test removal of Incubation period length, Total observation duration, Lay date and/or Temp
# Off_final_1 - Removal of Incubation period length and Total obs duration
# Off_final_1 <- off_m7_m7
Off_final_1 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled, data = off_bouts_filtered)
# Off_final_2 - Removal of Incubation period length, Total obs duration and Lay date
Off_final_2 <- lm(log(Bout_duration_seconds) ~ Obs_Niwa_daily_av_ambient_temp_celcius + Female_age + Clutch_size + Time_of_day_scaled, data = off_bouts_filtered)
# Off_final_3 - Removal of Incubation period length, Total obs duration and Temp
Off_final_3 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled, data = off_bouts_filtered)
# Off_final_4 - Removal of Incubation period length, Total obs duration, Lay date and Temp
Off_final_4 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Time_of_day_scaled, data = off_bouts_filtered)

AIC(Off_final_1, Off_final_2, Off_final_3, Off_final_4)
#Results
#df      AIC
Off_final_1  7 599.2142
Off_final_2  6 597.3994 #2 - within AIC<2 OF #1
Off_final_3  6 597.4622 #3 - within AIC<2 OF #1
Off_final_4  5 595.5216 #1 Lowest AIC 

#off_final_4_1 - Removal of Incubation period length, Total obs duration, Lay date and Temp
#off_final_4_1 <- Off_final_4
off_final_4_1 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size + Time_of_day_scaled, data = off_bouts_filtered)
#off_final_4_2 - Removal of Incubation period length, Total obs duration, Lay date, Temp and Female Age
off_final_4_2 <- lm(log(Bout_duration_seconds) ~ Clutch_size + Time_of_day_scaled, data = off_bouts_filtered)
#off_final_4_3 - Removal of Incubation period length, Total obs duration, Lay date, Temp and Clutch size 
off_final_4_3 <- lm(log(Bout_duration_seconds) ~ Female_age + Time_of_day_scaled, data = off_bouts_filtered)
#off_final_4_4 - Removal of Incubation period length, Total obs duration, Lay date, Temp and Time of day
off_final_4_4 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size, data = off_bouts_filtered)

AIC(off_final_4_1, off_final_4_2, off_final_4_3, off_final_4_4)
df      AIC
off_final_4_1  5 595.5216 #3
off_final_4_2  4 600.9878 #4
off_final_4_3  4 594.2455 #2
off_final_4_4  4 593.8432 #1


#off_final_4_4 = off_final_4_4_1- Removal of Incubation period length, Total obs duration, Lay date, Temp and Time of day
off_final_4_4_1 <- lm(log(Bout_duration_seconds) ~ Female_age + Clutch_size, data = off_bouts_filtered)
# off_final_4_4_2 - Removal of Incubation period length, Total obs duration, Lay date, Temp, Time of day and Female age
off_final_4_4_2 <- lm(log(Bout_duration_seconds) ~ Clutch_size, data = off_bouts_filtered)
# off_final_4_4_3 - Removal of Incubation period length, Total obs duration, Lay date, Temp, Time of day and clutch size 
off_final_4_4_3 <- lm(log(Bout_duration_seconds) ~ Female_age, data = off_bouts_filtered)

AIC(off_final_4_4_1, off_final_4_4_2, off_final_4_4_3)
                 df      AIC
off_final_4_4_1  4 593.8432 #2
off_final_4_4_2  3 598.9888
off_final_4_4_3  3 592.4461 #1 Lowest AIC score 

# Final comparison with a model with the quadratic term of Female age - testing a non-linear relationship
off_final_4_4_3_quad <- lm(log(Bout_duration_seconds) ~ Female_age + I(Female_age^2), data = off_bouts_filtered)

AIC(off_final_4_4_3, off_final_4_4_3_quad)
df      AIC
off_final_4_4_3       3 592.4461 #1 lmer performed better 
off_final_4_4_3_quad  4 594.0337

anova(off_final_4_4_3, off_final_4_4_3_quad)

#==============================
### Resulting final model: 
### off_final_4_4_3 <- lm(log(Bout_duration_seconds) ~ + Female_age, data = off_bouts_filtered)
#==============================
summary(off_final_4_4_3)
#Call:
#  lm(formula = log(Bout_duration_seconds) ~ Female_age, data = off_bouts_filtered)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.8834 -0.2296  0.0862  0.3591  2.9275 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.45114    0.08311  65.591   <2e-16 ***
  Female_age   0.07467    0.02903   2.572   0.0106 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7195 on 268 degrees of freedom
Multiple R-squared:  0.02409,	Adjusted R-squared:  0.02045 
F-statistic: 6.616 on 1 and 268 DF,  p-value: 0.01065

#===============================================================#
# Proportional model #
#===============================================================#
# LMER not appropriate as proprtional data is limited betwen 0 and 1 
# So a beta-mixed effects model is more appropriate, it will also be 
# weighted as to account for differences in observation period length
  install.packages("glmmTMB")
  library(glmmTMB)
# Load data - redo once added time of day column
  setwd("~/UCL 25-26/Research Project 2/Hihi_RStudio/Data")
  library(readr)
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

#Run model  
  prop_model_weighted <- glmmTMB(On_bout_proportion ~ 
                                   Obs_Av_daily_temp_celcius +  
                                   Female_age + 
                                   Clutch_size + 
                                   Lay_date_centered + 
                                   Time_of_day_scaled + 
                                   Inc_period_length_days +
                                   (1 | Female_ID),            
                                 weights = Total_obs_length_minutes, # <-- Weighted here 
                                 data = prop_filtered,
                                 family = beta_family(link = "logit"))
  
  summary(prop_model_weighted)
  # Some proportion values are exactly equal to 0 and 1, so transformation is needed
  # This will make the values just between the bouds to run the model
    # Get the total number of observations in your dataset
  n_obs <- nrow(prop_filtered)
  # Apply the Smithson & Verkuilen transformation to eliminate exact 0s and 1s
  prop_filtered$On_bout_prop_transformed <- (prop_filtered$On_bout_proportion * (n_obs - 1) + 0.5) / n_obs
  # Quick sanity check: the Min should now be > 0 and Max should be < 1 - the max is still over 1 
  summary(prop_filtered$On_bout_prop_transformed)
  
  #So will cap any impossible value at exactly 1.0 (100%)
  prop_filtered$On_bout_proportion[prop_filtered$On_bout_proportion > 1] <- 1.0
  # Get the number of rows
  n_obs <- nrow(prop_filtered)
  # Apply the transformation to handle the 0s and the newly capped 1s
  prop_filtered$On_bout_prop_transformed <- (prop_filtered$On_bout_proportion * (n_obs - 1) + 0.5) / n_obs
  # Double check the summary (The Max MUST now be strictly less than 1)
  summary(prop_filtered$On_bout_prop_transformed)
  
  # Rerun your final weighted model using the TRANSFORMED column
  prop_model_final <- glmmTMB(On_bout_prop_transformed ~ 
                                Obs_Av_daily_temp_celcius +  
                                Female_age + 
                                Clutch_size + 
                                Lay_date_centered + 
                                Time_of_day_scaled + 
                                Inc_period_length_days +
                                (1 | Female_ID),            
                              weights = Total_obs_length_minutes, 
                              data = prop_filtered,
                              family = beta_family(link = "logit"))

# To check model assumptions are met 

  install.packages("DHARMa")
  library(DHARMa)
  
  # 1. Simulate the residuals
  sim_final <- simulateResiduals(fittedModel = prop_model_final, n = 500)
  
  # 2. Main diagnostic plot (Check QQ plot & Residual vs Predicted)
  plot(sim_final)
  
# Assumptions test - results
# Q-Q plot: p = 0.016, significant KS test result and not expected residual distribution 
# Residual vs. predicted plot: significant quantile deviations (lower quartile)
  
# Fix this with dispformula to make dispersion dynamic instead of fixed - this fixes the overdispersion causing assumption violation

  prop_model_adjusted <- glmmTMB(On_bout_prop_transformed ~ 
                                   Obs_Av_daily_temp_celcius + Female_age + 
                                   Clutch_size + Lay_date_centered + 
                                   Time_of_day_scaled + Inc_period_length_days +
                                   (1 | Female_ID),            
                                 dispformula = ~ Time_of_day_scaled, 
                                 weights = Total_obs_length_minutes, 
                                 data = prop_filtered,
                                 family = beta_family(link = "logit"))
  
  sim_res_adj <- simulateResiduals(fittedModel = prop_model_adjusted, n = 1000)
  
  plot(sim_res_adj)
  
  
# Assumptions re-test - results
# Q-Q: non significant result
# Residual vs. predicted plot: Improved, no significant result  
# Okay to continue as model assumptions are met 
  
# Compare AIC to previous model 
  
  AIC(prop_model_final, prop_model_adjusted)
  df       AIC
  prop_model_final     9 -6452.356
  prop_model_adjusted 10 -8828.601
  
 
###===============
# Model comparison 
# Using stepwise method 
#=================
# Global model with all variables 
#on_prop_global <- prop_model_adjusted
  
on_prop_global <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID),            
dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
 
# Test 1 without random effect
on_prop_global_1 <-glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days,            
dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

anova(on_prop_global, on_prop_global_1)
# p = 0 so supports keeping the random effect

# Test 2 dispersion covariate - whether dispformula = ~ Time_of_day_scaled is necesary 
# dispformula = ~1
on_prop_global_2 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID),            
                          dispformula = ~ 1, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
anova(on_prop_global, on_prop_global_2)
# p = 0 so supports keeping the dispersion covariate 

# Compare using AIC scores 
AIC(on_prop_global, on_prop_global_1, on_prop_global_2)
df       AIC
on_prop_global   10 -8828.601 #1 Lowest AIC by >2 AIC points
on_prop_global_1  9 -3891.653
on_prop_global_2  9 -6452.356

# Round 1 
# Global model
on_prop_global <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m1: Remove Temp 
on_prop_m1 <- glmmTMB(On_bout_prop_transformed ~ Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m2: Remove Female age 
on_prop_m2 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Clutch_size + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m3: Remove Clutch size 
on_prop_m3 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m4: Remove Lay date centered 
on_prop_m4 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m5: Remove Time of day 
on_prop_m5 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m6: Remove Incubation period length
on_prop_m6 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Clutch_size + Lay_date_centered + Time_of_day_scaled + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

#Compare AIC scores across models from Round 1 
AIC(on_prop_global, on_prop_m1, on_prop_m2, on_prop_m3, on_prop_m4, on_prop_m5, on_prop_m6)
               df       AIC
on_prop_global 10 -8794.449 #4
on_prop_m1      9 -7642.568 
on_prop_m2      9 -8795.789 #2
on_prop_m3      9 -8796.156 #1
on_prop_m4      9 -8794.759 #3
on_prop_m5      9 -8527.757 
on_prop_m6      9 -8791.528

# Round 2 
# m3_m1: Removes Clutch size
#on_prop_m3_m1 <- on_prop_m3
on_prop_m3_m1 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m2: Removes Clutch size & Temp  
on_prop_m3_m2 <- glmmTMB(On_bout_prop_transformed ~ Female_age + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m3: Removes Clutch size & Female age
on_prop_m3_m3 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m4: Removes Clutch size and Lay date 
on_prop_m3_m4 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m5: Removes Clutch size and Time of day 
on_prop_m3_m5 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Lay_date_centered + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m6: Removes Clutch size and Incubation period length 
on_prop_m3_m6 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Female_age + Lay_date_centered + Time_of_day_scaled + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

# Compare models 
AIC(on_prop_m3_m1, on_prop_m3_m2, on_prop_m3_m3, on_prop_m3_m4, on_prop_m3_m5, on_prop_m3_m6)
# Results 
              df       AIC
on_prop_m3_m1  9 -8796.156 #3
on_prop_m3_m2  8 -7644.557 # AIC increases the most with the removal of Temp variable - strong impact 
on_prop_m3_m3  8 -8797.716 #1
on_prop_m3_m4  8 -8796.357 #2
on_prop_m3_m5  8 -8529.659
on_prop_m3_m6  8 -8791.937

# Round 3
# On_prop_m3_m3: Removes Clutch size and Female age 
# on_prop_m3_m3_m1 <- on_prop_m3_m3
on_prop_m3_m3_m1 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m2: Removes Clutch size, Female age and Temp
on_prop_m3_m3_m2 <- glmmTMB(On_bout_prop_transformed ~ Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m3: Removes Clutch size, Female age and Lay date 
on_prop_m3_m3_m3 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m4: Removes Clutch size, Female age and Time of day
on_prop_m3_m3_m4 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
# m5: Removes Clutch size, Female age and Incubation period
on_prop_m3_m3_m5 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

#Compare model performance 
AIC(on_prop_m3_m3_m1, on_prop_m3_m3_m2, on_prop_m3_m3_m3, on_prop_m3_m3_m4, on_prop_m3_m3_m5)
# Results 
                df       AIC
on_prop_m3_m3_m1  8 -8797.716 #1 Removes Clutch size and Female age
on_prop_m3_m3_m2  7 -7646.555
on_prop_m3_m3_m3  7 -8797.303 #2 Removes Clutch size, Female age and Lay date 
on_prop_m3_m3_m4  7 -8531.465
on_prop_m3_m3_m5  7 -8793.929 # Incomparable as >2 delta AIC 

#1:Removes Clutch size and Female age
# on_prop_final_1  <- on_prop_m3_m3_m1
on_prop_final_1 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
#2:Removes Clutch size, Female age and Lay date
#on_prop_final_2 <- on_prop_m3_m3_m3
on_prop_final_2 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
#3:Removes Clutch size, Female age and Incubation period
##on_prop_final_3 <- on_prop_m3_m3_m5 
on_prop_final_3 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  
#4:Removes Clutch size, Female age, Lay date and Incubation period
on_prop_final_4 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Time_of_day_scaled + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

# Compare performance 
AIC(on_prop_final_1, on_prop_final_2, on_prop_final_3, on_prop_final_4)
#Results 
                 df       AIC
on_prop_final_1  7 -8797.716 #1 
on_prop_final_2  7 -8797.303 #2 Comparable as <1 delta AIC 
on_prop_final_3  7 -8793.929
on_prop_final_4  6 -8792.893 # Combined removal does not improve, and reduce, AIC 

#=================
#Final model #
#on_prop_final_1 - lowest AIC
#=================
# on_prop_final_1 <- glmmTMB(On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered + Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID), dispformula = ~ Time_of_day_scaled, weights = Total_obs_length_minutes, data = prop_filtered, family = beta_family(link = "logit"))  

summary(on_prop_final_1)

Family: beta  ( logit )
Formula:          
  On_bout_prop_transformed ~ Obs_Av_daily_temp_celcius + Lay_date_centered +  
  Time_of_day_scaled + Inc_period_length_days + (1 | Female_ID)
Dispersion:                                ~Time_of_day_scaled
Data: prop_filtered
Weights: Total_obs_length_minutes

AIC       BIC    logLik -2*log(L)  df.resid 
-8831.9   -8811.5    4423.9   -8847.9        86 

Random effects:
  
  Conditional model:
  Groups    Name        Variance Std.Dev.
Female_ID (Intercept) 1.833    1.354   
Number of obs: 94, groups:  Female_ID, 47

Conditional model:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)               -19.145992   5.027805   -3.81  0.00014 ***
  Obs_Av_daily_temp_celcius   0.340176   0.007352   46.27  < 2e-16 ***
  Lay_date_centered          -0.026970   0.017143   -1.57  0.11566    
Time_of_day_scaled         -0.295284   0.017284  -17.08  < 2e-16 ***
  Inc_period_length_days      0.856179   0.345145    2.48  0.01311 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Dispersion model:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)         1.75921    0.01944   90.49   <2e-16 ***
  Time_of_day_scaled -1.34775    0.02092  -64.42   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 