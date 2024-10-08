
library(grid) # combining plots
library(gridExtra) # combining plots
library(ggpubr) # combining plots
library(patchwork) # combining plots
library(ggfortify) # nice extension for ggplot
library(tidyverse)
library(data.table)
library(timeplyr)
library(lubridate)
library(zoo)
library(slider)
library(writexl)
library(readxl)
library(RFlux)
library(REddyProc)
library(hrbrthemes)
library(lubridate)
library(anytime)
library(tidyr)
library(splines)
library(hms)
library(mgcv)
library(ggplot2)
library(viridis)
library(scales)
library(outliers)
library(changepoint)
library(FNN)


# Manual analysis

## before inputed to R, the data was checked to its time consystency --> resulting several gaps (time-jumping) at daytime
## the gaps on environmental variables were filled using "Mean Diurnal Variation" method, computing using 14 adjacent data, according to 
## Falge et al. 2001 https://doi.org/10.1016/S0168-1923(00)00225-2 

library(readxl)
R_eddy <- read_excel("D:/Licor7500/03092024/input.xlsx", 
    sheet = "R", col_types = c("text", "text", 
        "date", "date", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"))
View(R_eddy)


str(R_eddy)
		tibble [4,075 × 247] (S3: tbl_df/tbl/data.frame)
		 $ DATAH                         : chr [1:4075] "DATA" "DATA" "DATA" "DATA" ...
		 $ filename                      : chr [1:4075] "2024-06-10T133000_smart3-00273.ghg" "2024-06-10T143000_smart3-00273.ghg" "2024-06-10T150000_smart3-00273.ghg" "2024-06-10T153000_smart3-00273.ghg" ...
		 $ date                          : POSIXct[1:4075], format: "2024-06-10" ...
		 $ time                          : POSIXct[1:4075], format: "1899-12-31 14:00:00" ...
		 $ DOY                           : num [1:4075] 163 163 163 163 163 ...
		 $ daytime                       : num [1:4075] 1 1 1 1 1 1 1 0 0 0 ...
		 $ file_records                  : num [1:4075] 18000 18000 18000 18000 18000 18000 18000 18000 18000 18000 ...
		 $ used_records                  : num [1:4075] 18000 18000 18000 18000 18000 18000 18000 18000 18000 18000 ...
		 $ Tau                           : num [1:4075] -0.0319 -0.4274 -0.325 -0.0721 -0.0403 ...
		 $ qc_Tau                        : num [1:4075] 1 1 1 0 2 1 0 1 0 2 ...
		 $ rand_err_Tau                  : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ H                             : num [1:4075] -24.243 -12.121 -5.355 -0.681 -8.666 ...
		 $ qc_H                          : num [1:4075] 1 1 1 0 1 0 0 0 0 0 ...
		 $ rand_err_H                    : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ LE                            : num [1:4075] 77 105 104.9 35.2 73.7 ...
		 $ qc_LE                         : num [1:4075] 1 1 1 0 1 2 0 0 0 0 ...
		 $ rand_err_LE                   : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ co2_flux                      : num [1:4075] 12.02 17.24 6.77 1.69 16.5 ...
		 $ qc_co2_flux                   : num [1:4075] 1 1 1 2 1 0 1 0 0 0 ...
		 $ rand_err_co2_flux             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ h2o_flux                      : num [1:4075] 1.753 2.39 2.387 0.801 1.676 ...
		 $ qc_h2o_flux                   : num [1:4075] 1 1 1 0 1 2 0 0 0 0 ...
		 $ rand_err_h2o_flux             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_flux                      : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ qc_ch4_flux                   : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ rand_err_ch4_flux             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_flux                     : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ qc_none_flux                  : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ rand_err_none_flux            : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ H_strg                        : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ LE_strg                       : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ co2_strg                      : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ h2o_strg                      : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_strg                      : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_strg                     : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ co2_v-adv                     : num [1:4075] -5.72e-11 -6.25e-11 1.13e-10 -1.46e-10 -2.15e-10 ...
		 $ h2o_v-adv                     : num [1:4075] -4.35e-12 -4.48e-12 8.12e-12 -1.04e-11 -1.52e-11 ...
		 $ ch4_v-adv                     : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_v-adv                    : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ co2_molar_density             : num [1:4075] 19.6 19.6 19.4 19.1 18.4 ...
		 $ co2_mole_fraction             : num [1:4075] 483 483 477 470 452 ...
		 $ co2_mixing_ratio              : num [1:4075] 502 501 494 486 467 ...
		 $ co2_time_lag                  : num [1:4075] 0.2 0 0 0.1 0 -0.1 -0.1 0 -0.2 -0.1 ...
		 $ co2_def_timelag               : num [1:4075] 0 0 0 0 0 0 0 0 0 0 ...
		 $ h2o_molar_density             : num [1:4075] 1488 1409 1397 1361 1301 ...
		 $ h2o_mole_fraction             : num [1:4075] 36.8 34.7 34.4 33.5 31.9 ...
		 $ h2o_mixing_ratio              : num [1:4075] 38.2 35.9 35.6 34.6 33 ...
		 $ h2o_time_lag                  : num [1:4075] 0.1 0 0 0.1 0 -0.1 -0.1 0 -0.3 -0.2 ...
		 $ h2o_def_timelag               : num [1:4075] 0 0 0 0 0 0 0 0 0 0 ...
		 $ ch4_molar_density             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_mole_fraction             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_mixing_ratio              : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_time_lag                  : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ ch4_def_timelag               : num [1:4075] 9 9 9 9 9 9 9 9 9 9 ...
		 $ none_molar_density            : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_mole_fraction            : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_mixing_ratio             : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_time_lag                 : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ none_def_timelag              : num [1:4075] 9 9 9 9 9 9 9 9 9 9 ...
		 $ sonic_temperature             : num [1:4075] 305 304 304 303 303 ...
		 $ air_temperature               : num [1:4075] 300 299 299 299 299 ...
		 $ air_pressure                  : num [1:4075] 100932 100984 101005 101059 101132 ...
		 $ air_density                   : num [1:4075] 1.16 1.16 1.16 1.16 1.17 ...
		 $ air_heat_capacity             : num [1:4075] 1026 1025 1025 1024 1024 ...
		 $ air_molar_volume              : num [1:4075] 0.0247 0.0246 0.0246 0.0246 0.0245 ...
		 $ ET                            : num [1:4075] 0.1136 0.1549 0.1547 0.0519 0.1086 ...
		 $ water_vapor_density           : num [1:4075] 0.0268 0.0254 0.0252 0.0245 0.0234 ...
		 $ e                             : num [1:4075] 3710 3505 3473 3382 3229 ...
		 $ es                            : num [1:4075] 3500 3346 3313 3275 3229 ...
		 $ specific_humidity             : num [1:4075] 0.0232 0.0219 0.0217 0.0211 0.0201 ...
		 $ RH                            : num [1:4075] 100 100 100 100 100 ...
		 $ VPD                           : num [1:4075] 0 0 0 0 0.245 ...
		 $ Tdew                          : num [1:4075] 301 300 300 299 298 ...
		 $ u_unrot                       : num [1:4075] 0.282 -0.34 0.417 0.642 -0.443 ...
		 $ v_unrot                       : num [1:4075] 0.6272 2.7782 2.3032 0.0701 -0.8054 ...
		 $ w_unrot                       : num [1:4075] 0.0459 0.038 0.0542 0.0133 0.0136 ...
		 $ u_rot                         : num [1:4075] 0.689 2.799 2.341 0.646 0.919 ...
		 $ v_rot                         : num [1:4075] 9.29e-14 1.39e-14 8.23e-14 -1.08e-13 5.21e-14 ...
		 $ w_rot                         : num [1:4075] -2.92e-15 -3.18e-15 5.81e-15 -7.63e-15 -1.17e-14 ...
		 $ wind_speed                    : num [1:4075] 0.689 2.799 2.341 0.646 0.919 ...
		 $ max_wind_speed                : num [1:4075] 2.52 8.42 7.87 3.56 5.14 ...
		 $ wind_dir                      : num [1:4075] 98.7 171.7 190.1 271.7 44.6 ...
		 $ yaw                           : num [1:4075] 65.83 96.97 79.73 6.23 241.17 ...
		 $ pitch                         : num [1:4075] 3.816 0.778 1.326 1.177 0.845 ...
		 $ roll                          : num [1:4075] NA NA NA NA NA NA NA NA NA NA ...
		 $ u*                            : num [1:4075] 0.166 0.607 0.529 0.249 0.186 ...
		 $ TKE                           : num [1:4075] 0.332 1.742 1.46 0.417 0.797 ...
		 $ L                             : num [1:4075] 16.7 1626.6 2438.2 1999.2 65.4 ...
		 $ (z-d)/L                       : num [1:4075] 0.81012 0.00553 0.00369 0.0045 0.13767 ...
		 $ bowen_ratio                   : num [1:4075] -0.315 -0.1154 -0.051 -0.0193 -0.1176 ...
		 $ T*                            : num [1:4075] 0.123 0.0168 0.0085 0.0023 0.0391 ...
		 $ model                         : num [1:4075] 1 0 0 0 1 1 0 1 1 1 ...
		 $ x_peak                        : num [1:4075] 16.6 61.2 62.4 71.7 20.1 ...
		 $ x_offset                      : num [1:4075] 7 -9.29 -9.47 -10.88 9 ...
		 $ x_10%                         : num [1:4075] 15 21 21.4 24.6 18 ...
		 $ x_30%                         : num [1:4075] 31 52.3 53.3 61.3 38 ...
		 $ x_50%                         : num [1:4075] 57 79.8 81.3 93.5 71 ...
		 $ x_70%                         : num [1:4075] 122 112 114 131 152 ...
		 $ x_90%                         : num [1:4075] 503 168 171 196 641 ...
		  [list output truncated]

		  
## handling date-time problem
## the dataframe contains false time information in "date" column , while "time" column posessses false date information
# manipulating column and datetime 

eddy_test <- R_eddy %>%
				mutate(
						day = day(date),
						month = month(date), 
						year = year(date), # separate date to day, month, and year. Just for backup
						Date = format(date, format = "%Y/%m/%d"), # remove false time in "date" column by creating other "Date" column
						Time = format(time, format = "%H:%M:%S")) # remove false date in "time" column by creating other "Time" column





eddy_test$Datetime <- ymd_hms(paste0(eddy_test$Date, eddy_test$Time)) # merging "Date" and "Time" to "Datetime" for comfortable usage

write_xlsx(eddy_test, "D:/Licor7500/03092024/eddy_test.xlsx") # test if the process run well. Result = OK !! (assuming the time in UTC wkwkw)
														## trying to get rid from nonsense timezone adjustment wkwkwkwkw lol

# Quality Control and Cell Manipulation 

## replacing cell with N.A. if the flux' and H' qc flags above 1 (2, 3, ..) = indicating bad fluxes
## also replacing cell with N.A. if the flux has negative values on nightime (daytime=0), no photosynthesis at night, resulting positive fluxes
## also averaging soil temperature (TS) and soil water content (SWC) for NEE' gapfill inputs
## also convert TS unit (Kelvin) to Celcius

eddy_testqc <- eddy_test %>% 
				mutate(
						co2_flux = ifelse(qc_co2_flux == 2, NA, co2_flux)) %>% # discard co2_flux if it have qc = 2									
				mutate(
						H = ifelse(qc_H == 2, NA, H)) %>% 	# discard H if it have qc = 2										
				mutate(
						LE = ifelse(qc_LE == 2, NA, H)) %>%  # discard LE if it have qc = 2		
				mutate(
						co2_flux = ifelse(daytime == 0 & co2_flux < 0, NA , co2_flux)) %>% # discard negative co2_flux on nighttime 	
				mutate(
						TS = rowMeans(cbind(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm = TRUE))%>%  # average soil temperature
				mutate(
						TS = TS - 273.15) %>% 	# convert soil temperature to celcius							
				mutate(
						TA = air_temperature - 273.15) %>% 		# convert air temperature to celcius	
				mutate(
						SWC = rowMeans(select(.,starts_with("SWC")), na.rm = TRUE))%>% #change SWC_1_1_1 to SWC
				mutate(
						co2_flux2 = co2_flux * 0.00158436)%>%  # change the units from umol/m2,s to Mg/ha, hour
				mutate(
						PPFD_1_1_1 = ifelse(PPFD_1_1_1 < 0, 0, PPFD_1_1_1))%>% # set PPFD to zero if it contains negative value (no sunlight)
				mutate(
						ET = ifelse(ET < 0, 0, ET))%>% # set ST to zero if it contains negative value 
				mutate(
						PPFD_1_1_1 = ifelse(Time >= "19:00:00" & Time <= "04:00:00", 0, PPFD_1_1_1))%>%  # set PPFD to zero at 19:00-04:00
				rename(
						ustar = "u*" ) %>% # rename u* to ustar for convencience
				rename(
						PPFD = PPFD_1_1_1) 					
									
write_xlsx(eddy_testqc, "D:/Licor7500/03092024/eddy_testqc.xlsx") # test if the process run well. Result = OK !! 

# identify time gaps (consistency) by create correct datetime and binned it to the original flux data

## Generate correct and consistent half-hourly DateTime column

half_hourly_dt <- seq(ymd_hms("2024-06-10 14:00:00"), ymd_hms("2024-09-03 17:00:00"), by = "30 min")
half_hourly_df <- data.frame(Datetime = half_hourly_dt)

## Bin  original dataframe to the half-hourly grid

binned_eddy_testqc <- eddy_testqc %>% 
							mutate(Datetime = floor_date(Datetime, "30 minutes")) %>% 
							right_join(half_hourly_df, by = "Datetime") %>% 
							group_by(Datetime) %>% 
							summarise(across(everything(), ~ coalesce(mean(., na.rm = TRUE), NA)))%>% 
							select(-c(day, month, year, Date, Time)) %>% #drop time-related columns containing gaps
							mutate(
									day = day(Datetime),
									month = month(Datetime), 
									year = year(Datetime),
									Date = format(Datetime, format = "%Y/%m/%d"), 
									Time = format(Datetime, format = "%H:%M:%S"))%>% #retaining all correct time-related columns
							mutate(
									daytime = ifelse(Time >= "06:00:00" & Time <= "17:30:00", 1, 0))   #fill the daytime gaps
				
write_xlsx(binned_eddy_testqc, "D:/Licor7500/03092024/binned_eddy_testqc.xlsx") # test if the process run well. Result = OK !!

# de-spiking/extreme outlier elimination based on the averaged moving window (=MDV) of 14 day. 
## value is bad if it beyond 1.5*SD (Modified from Papale = 3*SD)


eddy_testqc2 <-  binned_eddy_testqc %>%
  arrange(Datetime) %>%
  group_by(daytime) %>% 
  mutate(
    ma = slide_dbl(co2_flux2, ~ mean(.x, na.rm = TRUE),.before = 7,.after = 7),  # moving average 14 day -7 & +7 days
    msd = slide_dbl(co2_flux2, ~ sd(.x, na.rm = TRUE),.before = 7,.after = 7) # moving std 14 day -7 & +7 days
  ) %>%
  ungroup() %>%
  mutate(
    qc_rm = case_when(
      co2_flux2 > ma + 1.5 * msd ~ 0, 
      co2_flux2 < ma - 1.5 * msd ~ 0, 
      TRUE ~ 1  
    )
  ) %>%
  mutate(
    co2_flux2 = ifelse(qc_rm == 0, NA, co2_flux2)
  ) %>%
 # group_by(daytime) %>%
  # mutate( #apply moving window of Smirnoff–Grubbs rejection test (α = 0.05) # in development
 #  sg_outlier = slide_dbl(co2_flux2, ~ ifelse(grubbs.test(.x)$p.value < 0.05, 1, 0), .before = 7, .after = 7)) %>% 
 # mutate(
  #  co2_flux2 = ifelse(sg_outlier, NA, co2_flux2)
 # )%>%
  ungroup()
		 
write_xlsx(eddy_testqc2, "D:/Licor7500/03092024/eddy_testqc2.xlsx") # test if the process run well. Result = OK !! 



# compute and add the storage term for calculating NEE
# Weights for the weighted sum
weights <- tibble(
  height = c(10, 20, 30, 40),
  weight = c(0.1, 0.3, 0.4, 0.2)
)

# Compute the weighted sum for each profile and time
data_weighted <- data %>%
  inner_join(weights, by = "height") %>%
  group_by(profile, time) %>%
  mutate(weighted_concentration = concentration * weight) %>%
  summarise(weighted_sum = sum(weighted_concentration))

# Compute the finite difference for each profile
data_fd <- data_weighted %>%
  group_by(profile) %>%
  arrange(time) %>%
  mutate(dc = weighted_sum - lag(weighted_sum),
         dt = time - lag(time)) %>%
  summarise(dcdt = mean(dc / dt))
  
## not yet due to Li8100 and Li8150 failures!!!!!!




# CO2 removal on night time based the U* (Ustar) or friction velocity 
## the method is modified from change point detection implemented by Hirano et al. 2024  https://doi.org/10.1038/s43247-024-01387-7


## Calculate quartiles for TA
quartiles <- quantile(eddy_testqc2$TA[eddy_testqc2$daytime == 0], 
                      probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Create TA_quartile column
eddy_testqc3 <- eddy_testqc2 %>%
  mutate(
    TA_quartile = case_when(
      daytime == 0 ~ cut(TA, breaks = quartiles, 
                         include.lowest = TRUE, 
                         labels = 1:4),
      TRUE ~ NA
    )
  )

write_xlsx(eddy_testqc3, "D:/Licor7500/03092024/eddy_testqc3.xlsx") # test if the process run well. Result = OK !! 

# Create ustar_binned column and calculate changepoints
# Calculate max_ustar (=max changepoint on each quartile) and annual_threshold (=mean(max_ustar(Q1:Q4)))

thresholds <- eddy_testqc3 %>%
  filter(!is.na(TA_quartile)) %>%  # select only non-NA rows of TA_quartile
  group_by(TA_quartile) %>%
  mutate(
    ustar_binned = cut(ustar, breaks = quantile(ustar, probs = seq(0, 1, by = 0.05), na.rm = TRUE), 
                        include.lowest = TRUE, labels = 1:20)
  ) %>%
  group_by(TA_quartile, ustar_binned) %>%
  mutate(
    changepoints = cpt.mean(ustar, method = "PELT")@cpts,  # extract the cpts slot from the S4 object
    max_chgpoint_idx = which.max(changepoints),  # get the index of the max changepoint
    max_chgpoint_ustar = ustar[ max_chgpoint_idx ]  # extract the ustar value at the max changepoint
  ) %>%
  group_by(TA_quartile) %>%
  summarise(
    max_ustar = max(max_chgpoint_ustar)  # extract the maximum ustar in each quarter
  )

write_xlsx(thresholds, "D:/Licor7500/03092024/thresholds.xlsx") # test if the process run well. Result = OK !! 

# Calculate annual_threshold
annual_threshold <- thresholds %>%
  pull(max_ustar) %>%  # extract the max_ustar values
  mean(na.rm = TRUE)  # calculate the mean of the max_ustar values

# Add the annual_threshold to the original data
eddy_testqc4 <- eddy_testqc3 %>%
  mutate(
    flags_ustar = case_when(
      daytime == 0 & ustar < annual_threshold ~ 0,  # bad
      daytime == 0 & ustar >= annual_threshold ~ 1,  # good
      daytime == 1 ~ NA_integer_  # NA when daytime == 1
    )
  ) %>%
  mutate(
    co2_flux2 = ifelse(daytime == 0 & flags_ustar == 0, NA_real_, co2_flux2) # set co2_flux2 to NA when flags_ustar == 0 and daytime == 0
  ) %>%
  arrange(Datetime)

write_xlsx(eddy_testqc4, "D:/Licor7500/03092024/eddy_testqc4.xlsx") # test if the process run well. Result = OK !! 


# Fill nighttime CO2 flux using look up table

# Create the look-up table
lookup_table <- eddy_testqc4 %>%
  filter(daytime == 0, flags_ustar == 1, !is.na(TA), !is.na(ustar), !is.na(co2_flux2)) %>%
  select(TA, ustar, co2_flux2) %>%
  mutate(group = kmeans(cbind(TA, ustar), centers = 9)$cluster) %>%  #using kmeans clustering to fill bins
  group_by(group) %>%
  summarise(
    mean_co2_flux2 = mean(co2_flux2, na.rm = TRUE),
    mean_Tair = mean(TA, na.rm = TRUE),
    mean_ustar = mean(ustar, na.rm = TRUE),
    count = n()  #store the amount of calculated row
  )
  
write_xlsx(lookup_table, "D:/Licor7500/03092024/lookup_table.xlsx") # test if the process run well. Result = OK !! 
  

# Fill empty values of co2_flux2 column at daytime == 0 
fill_co2_flux2 <- function(data, lookup_table) {
  data %>%
    rowwise() %>%
    mutate(
      co2_flux2 = ifelse(is.na(co2_flux2) & daytime == 0,
                         lookup_table$mean_co2_flux2[which.min(abs(lookup_table$mean_Tair - TA) + abs(lookup_table$mean_ustar - ustar))],
                         co2_flux2)
    )
}

eddy_testqc5 <- fill_co2_flux2(eddy_testqc4, lookup_table)

write_xlsx(eddy_testqc5, "D:/Licor7500/03092024/eddy_testqc5.xlsx") # test if the process run well. Result = OK !! 



## select cell for computing daytime gap filling

gp_NEE <- eddy_testqc5 %>% select(day, month, year, Date, Time, Datetime, daytime, 
					co2_flux2, H, LE, ET, P_RAIN_1_1_1, RG_1_1_1, TA, PPFD, VPD, ustar, RH_1_1_1, TS, SWC) 
					
gp_NEE<- as.data.frame(gp_NEE)

str(gp_NEE )
			'data.frame':	4087 obs. of  20 variables:
		 $ day         : int  10 10 10 10 10 10 10 10 10 10 ...
		 $ month       : int  6 6 6 6 6 6 6 6 6 6 ...
		 $ year        : int  2024 2024 2024 2024 2024 2024 2024 2024 2024 2024 ...
		 $ Date        : chr  "2024/06/10" "2024/06/10" "2024/06/10" "2024/06/10" ...
		 $ Time        : chr  "14:00:00" "14:30:00" "15:00:00" "15:30:00" ...
		 $ Datetime    : POSIXct, format:  ...
		 $ daytime     : num  1 1 1 1 1 1 1 1 0 0 ...
		 $ co2_flux2   : num  0.019 NA 0.0273 0.0107 NA ...
		 $ H           : num  -24.243 NA -12.121 -5.355 -0.681 ...
		 $ LE          : num  -24.243 NA -12.121 -5.355 -0.681 ...
		 $ ET          : num  0.1136 NA 0.1549 0.1547 0.0519 ...
		 $ P_RAIN_1_1_1: num  1e-04 NA 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 ...
		 $ RG_1_1_1    : num  205.3 NA 61.9 69.4 46.8 ...
		 $ TA          : num  26.7 NA 26 25.8 25.6 ...
		 $ PPFD        : num  465 NA 151 158 104 ...
		 $ VPD         : num  0 NA 0 0 0 ...
		 $ ustar       : num  0.166 NA 0.607 0.529 0.249 ...
		 $ RH_1_1_1    : num  95.4 NA 92.5 93.9 93.5 ...
		 $ TS          : num  29.4 NA 29.6 29.7 29.6 ...
		 $ SWC         : num  0.435 NA 0.432 0.431 0.429 ...


# change the column name to Biogeoscience/Europa Flux community convention

gp_NEE  <- gp_NEE %>% rename(
							DateTime = Datetime,
							NEE = co2_flux2,
							Rg = RG_1_1_1, 
							Tair = TA,
							rH = RH_1_1_1,
							Precpt = P_RAIN_1_1_1,
							Tsoil = TS ) %>% 
						relocate(
							DateTime)

str(gp_NEE  )
		'data.frame':	4087 obs. of  20 variables:
		 $ DateTime: POSIXct, format:  ...
		 $ day     : int  10 10 10 10 10 10 10 10 10 10 ...
		 $ month   : int  6 6 6 6 6 6 6 6 6 6 ...
		 $ year    : int  2024 2024 2024 2024 2024 2024 2024 2024 2024 2024 ...
		 $ Date    : chr  "2024/06/10" "2024/06/10" "2024/06/10" "2024/06/10" ...
		 $ Time    : chr  "14:00:00" "14:30:00" "15:00:00" "15:30:00" ...
		 $ daytime : num  1 1 1 1 1 1 1 1 0 0 ...
		 $ NEE     : num  0.019 NA 0.0273 0.0107 NA ...
		 $ H       : num  -24.243 NA -12.121 -5.355 -0.681 ...
		 $ LE      : num  -24.243 NA -12.121 -5.355 -0.681 ...
		 $ ET      : num  0.1136 NA 0.1549 0.1547 0.0519 ...
		 $ Precpt  : num  1e-04 NA 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 0e+00 ...
		 $ Rg      : num  205.3 NA 61.9 69.4 46.8 ...
		 $ Tair    : num  26.7 NA 26 25.8 25.6 ...
		 $ PPFD    : num  465 NA 151 158 104 ...
		 $ VPD     : num  0 NA 0 0 0 ...
		 $ ustar   : num  0.166 NA 0.607 0.529 0.249 ...
		 $ rH      : num  95.4 NA 92.5 93.9 93.5 ...
		 $ Tsoil   : num  29.4 NA 29.6 29.7 29.6 ...
		 $ SWC     : num  0.435 NA 0.432 0.431 0.429 ...
		 
		 
write_xlsx(gp_NEE, "D:/Licor7500/03092024/gp_NEE.xlsx") # test if the process run well. Result = OK !! 

# gap-fill All variables using MDV
## Simple approach: using 7-day centered rolling window MDV (14 days total)

EFgp_NEE_mdv <- gp_NEE  %>%
  group_by(Time) %>%
  mutate(across(c(NEE, H, LE, ET, Precpt, Rg, Tair, PPFD, VPD, ustar, rH, Tsoil, SWC), 
                ~ ifelse(is.na(.), 
                         mean(.[which(DateTime >= DateTime - 604800 & DateTime <= DateTime + 604800)], na.rm = TRUE), 
                        .))) %>%
  ungroup()
  

## more sophisticated approach: combining 2-day, 4-day and 7-day centered rolling window MDV. (4, 8, & 14 days total)
## lowest variance' input is selected

 EFgp_mdv <- gp_NEE %>%
  group_by(Time) %>%
  mutate(
    across(
      c(NEE, H, LE, ET, Precpt, Rg, Tair, PPFD, VPD, ustar, rH, Tsoil, SWC),
      list(
        mean_2day = ~ ifelse(is.na(.), mean(.[which(DateTime >= DateTime - 172800 & DateTime <= DateTime + 172800)], na.rm = TRUE),.),
        var_2day = ~ ifelse(is.na(.), var(.[which(DateTime >= DateTime - 172800 & DateTime <= DateTime + 172800)], na.rm = TRUE), 0),
        mean_4day = ~ ifelse(is.na(.), mean(.[which(DateTime >= DateTime - 345600 & DateTime <= DateTime + 345600)], na.rm = TRUE),.),
        var_4day = ~ ifelse(is.na(.), var(.[which(DateTime >= DateTime - 345600 & DateTime <= DateTime + 345600)], na.rm = TRUE), 0),
        mean_7day = ~ ifelse(is.na(.), mean(.[which(DateTime >= DateTime - 604800 & DateTime <= DateTime + 604800)], na.rm = TRUE),.),
        var_7day = ~ ifelse(is.na(.), var(.[which(DateTime >= DateTime - 604800 & DateTime <= DateTime + 604800)], na.rm = TRUE), 0)
      )
    ),
    across(
      c(NEE, H, LE, ET, Precpt, Rg, Tair, PPFD, VPD, ustar, rH, Tsoil, SWC),
      ~ ifelse(is.na(.), 
               ifelse(get(paste0(cur_column(), "_var_2day")) < get(paste0(cur_column(), "_var_4day")), 
                      get(paste0(cur_column(), "_mean_2day")), 
                      ifelse(get(paste0(cur_column(), "_var_4day")) < get(paste0(cur_column(), "_var_7day")), 
                             get(paste0(cur_column(), "_mean_4day")), 
                             get(paste0(cur_column(), "_mean_7day")))), 
              .)
    )
  ) %>%
  select(-ends_with("_2day"), -ends_with("_4day"), -ends_with("_7day"))

write_xlsx(EFgp_NEE_mdv, "D:/Licor7500/03092024/EFgp_NEE_mdv.xlsx") # test if the process run well. Result = OK !! 
write_xlsx(EFgp_mdv, "D:/Licor7500/03092024/EFgp_mdv.xlsx") # test if the process run well. Result = OK !! 





