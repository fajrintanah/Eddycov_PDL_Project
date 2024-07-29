

# plot NEE to Time

### ---- Binning Net emission to times. Averaging half-hourly to Hourly Data 

gp_NEE_hour <- EFgp_mdv %>% 
		mutate(hourlyDate = floor_date(DateTime,unit='hour')) %>%
		select(-c(DateTime, Date, Time)) %>%  # removing Date and Time columns: cannot be averaged
		group_by(hourlyDate) %>% 
		summarize(
				across(everything(),mean)) %>%
		mutate(
				Date = format(hourlyDate, format = "%Y/%m/%d"), #summoning Date again from hourlyDate
				Time = format(hourlyDate, format = "%H:%M:%S"))%>% # summoning Time again from hourlyDate
      ungroup()
	  
#gp_NEE_hour$Time <- as.POSIXct(strptime(gp_NEE_hour$Time, format = "%H:%M:%S"))	

str(gp_NEE_hour)
		tibble [867 × 18] (S3: tbl_df/tbl/data.frame)
		 $ hourlyDate: POSIXct[1:867], format: "2024-06-10 14:00:00" "2024-06-10 15:00:00" "2024-06-10 16:00:00" "2024-06-10 17:00:00" ...
		 $ day       : num [1:867] 10 10 10 10 10 10 10 10 10 10 ...
		 $ month     : num [1:867] 6 6 6 6 6 6 6 6 6 6 ...
		 $ year      : num [1:867] 2024 2024 2024 2024 2024 ...
		 $ daytime   : num [1:867] 1 1 1 1 0 0 0 0 0 0 ...
		 $ NEE       : num [1:867] 0.00636 0.01902 0.01506 0.01101 0.00804 ...
		 $ H         : num [1:867] 4.13 -8.74 -4.67 -8.56 -5.94 ...
		 $ LE        : num [1:867] 4.13 -8.74 -4.67 -8.29 -5.94 ...
		 $ Rg        : num [1:867] 285.5 65.6 33.2 -85.8 -480.7 ...
		 $ Tair      : num [1:867] 28.6 25.9 25.5 25.1 24.8 ...
		 $ PPFD      : num [1:867] 618.52 154.74 74.76 5.03 0 ...
		 $ VPD       : num [1:867] 744.048 0 0.123 24.081 35.768 ...
		 $ Ustar     : num [1:867] 0.216 0.568 0.217 0.194 0.142 ...
		 $ rH        : num [1:867] 85.6 93.2 93.7 95.1 96.3 ...
		 $ Tsoil     : num [1:867] 29.5 29.7 29.6 29.5 29.4 ...
		 $ SWC       : num [1:867] 0.395 0.432 0.429 0.426 0.424 ...
		 $ Date      : chr [1:867] "2024/06/10" "2024/06/10" "2024/06/10" "2024/06/10" ...
		 $ Time      : chr [1:867] "14:00:00" "15:00:00" "16:00:00" "17:00:00" ...
		 
write_xlsx(gp_NEE_hour, "D:/Licor7500/gp_NEE_hour.xlsx") # test if the process run well. Result = OK !! 
 

# make summary of the flux data for reporting

summary(gp_NEE_hour) #summary all
		   hourlyDate                       day           month            year         daytime            NEE           
		 Min.   :2024-06-10 14:00:00   Min.   : 1.0   Min.   :6.000   Min.   :2024   Min.   :0.0000   Min.   :-0.046661  
		 1st Qu.:2024-06-19 14:30:00   1st Qu.:10.0   1st Qu.:6.000   1st Qu.:2024   1st Qu.:0.0000   1st Qu.:-0.008394  
		 Median :2024-06-28 15:00:00   Median :14.0   Median :6.000   Median :2024   Median :1.0000   Median : 0.012435  
		 Mean   :2024-06-28 15:00:00   Mean   :15.1   Mean   :6.435   Mean   :2024   Mean   :0.5017   Mean   : 0.006189  
		 3rd Qu.:2024-07-07 15:30:00   3rd Qu.:21.0   3rd Qu.:7.000   3rd Qu.:2024   3rd Qu.:1.0000   3rd Qu.: 0.017877  
		 Max.   :2024-07-16 16:00:00   Max.   :30.0   Max.   :7.000   Max.   :2024   Max.   :1.0000   Max.   : 0.054070  
			   H                 LE                Rg                Tair            PPFD             VPD         
		 Min.   :-65.715   Min.   :-65.715   Min.   :-551.165   Min.   :21.56   Min.   :   0.0   Min.   :   0.00  
		 1st Qu.: -4.029   1st Qu.: -4.153   1st Qu.:-186.309   1st Qu.:23.85   1st Qu.:   0.0   1st Qu.:  58.22  
		 Median : -1.362   Median : -1.672   Median :  -2.864   Median :25.34   Median :   0.0   Median : 228.94  
		 Mean   : 17.808   Mean   : 17.758   Mean   :  61.326   Mean   :26.47   Mean   : 338.6   Mean   : 586.41  
		 3rd Qu.: 27.364   3rd Qu.: 27.364   3rd Qu.: 296.899   3rd Qu.:29.09   3rd Qu.: 605.7   3rd Qu.:1029.95  
		 Max.   :212.248   Max.   :212.248   Max.   : 881.588   Max.   :34.52   Max.   :1790.4   Max.   :2739.26  
			 Ustar               rH              Tsoil            SWC             Date               Time          
		 Min.   :0.02676   Min.   : 0.5908   Min.   :26.87   Min.   :0.3123   Length:867         Length:867        
		 1st Qu.:0.08824   1st Qu.:79.6951   1st Qu.:28.14   1st Qu.:0.3310   Class :character   Class :character  
		 Median :0.14271   Median :99.7827   Median :28.70   Median :0.3413   Mode  :character   Mode  :character  
		 Mean   :0.16862   Mean   :86.5694   Mean   :28.80   Mean   :0.3568                                        
		 3rd Qu.:0.23177   3rd Qu.:99.9381   3rd Qu.:29.48   3rd Qu.:0.3896                                        
		 Max.   :0.84288   Max.   :99.9493   Max.   :31.07   Max.   :0.4822     
 
 
 library(dplyr)
 
summary_daily_NG <- gp_NEE_hour %>% 
 summarise(
    NEE_mean = mean(NEE), NEE_sd = sd(NEE), NEE_min = min(NEE), NEE_max = max(NEE),
    PPFD_mean = mean(PPFD), PPFD_sd = sd(PPFD), PPFD_min = min(PPFD), PPFD_max = max(PPFD),
    VPD_mean = mean(VPD), VPD_sd = sd(VPD), VPD_min = min(VPD), VPD_max = max(VPD),
	SWC_mean = mean(SWC), SWC_sd = sd(SWC), SWC_min = min(SWC),SWC_max = max(SWC),
    Tsoil_mean = mean(Tsoil), Tsoil_sd = sd(Tsoil), Tsoil_min = min(Tsoil), Tsoil_max = max(Tsoil),
    Tair_mean = mean(Tair), Tair_sd = sd(Tair), Tair_min = min(Tair), Tair_max = max(Tair)
  )%>% 
  pivot_longer(
    cols = everything(),
    names_to = c("parameter", ".value"),
    names_pattern = "(.*)_(.*)"
  )

summary_daily_NG 
		# A tibble: 6 × 5
		  parameter      mean       sd     min       max
		  <chr>         <dbl>    <dbl>   <dbl>     <dbl>
		1 NEE         0.00619   0.0159 -0.0467    0.0541
		2 PPFD      339.      489.      0      1790.    
		3 VPD       586.      681.      0      2739.    
		4 SWC         0.357     0.0363  0.312     0.482 
		5 Tsoil      28.8       0.890  26.9      31.1   
		6 Tair       26.5       3.16   21.6      34.5    


  
library(tidyr)

summary_daily <- gp_NEE_hour %>% 
  group_by(daytime) %>% #summary grouped by daytime
  summarise(
    NEE_mean = mean(NEE), NEE_sd = sd(NEE), NEE_min = min(NEE), NEE_max = max(NEE),
    PPFD_mean = mean(PPFD), PPFD_sd = sd(PPFD), PPFD_min = min(PPFD), PPFD_max = max(PPFD),
    VPD_mean = mean(VPD), VPD_sd = sd(VPD), VPD_min = min(VPD), VPD_max = max(VPD),
	SWC_mean = mean(SWC), SWC_sd = sd(SWC), SWC_min = min(SWC),SWC_max = max(SWC),
    Tsoil_mean = mean(Tsoil), Tsoil_sd = sd(Tsoil), Tsoil_min = min(Tsoil), Tsoil_max = max(Tsoil),
    Tair_mean = mean(Tair), Tair_sd = sd(Tair), Tair_min = min(Tair), Tair_max = max(Tair)
  ) %>% 
  pivot_longer(
    cols = -daytime,
    names_to = c("parameter", ".value"),
    names_pattern = "(.*)_(.*)"
  )

summary_daily 
		# A tibble: 12 × 6
		   daytime parameter       mean        sd      min       max
			 <dbl> <chr>          <dbl>     <dbl>    <dbl>     <dbl>
		 1       0 NEE          0.0172    0.00586  0.00348    0.0541
		 2       0 PPFD         0         0        0          0     
		 3       0 VPD        153.      192.       0       1202.    
		 4       0 SWC          0.357     0.0365   0.313      0.455 
		 5       0 Tsoil       28.9       0.808   27.0       31.0   
		 6       0 Tair        24.4       1.30    21.6       28.5   
		 7       1 NEE         -0.00472   0.0151  -0.0467     0.0330
		 8       1 PPFD       675.      499.       0       1790.    
		 9       1 VPD       1016.      719.       0       2739.    
		10       1 SWC          0.356     0.0361   0.312      0.482 
		11       1 Tsoil       28.7       0.961   26.9       31.1   
		12       1 Tair        28.6       3.08    21.6       34.5  






## make diurnal plot	
		 
plot_jam_NEE <- ggplot(gp_NEE_hour, aes(Time, NEE,colour = Time)) +
	geom_jitter(alpha = 0.40)+ 
	geom_boxplot(col = "grey40", alpha = 0.40) +
	geom_smooth(aes(group = 1, col = "yellow"), method="lm", formula =  y ~ poly(x, 6))+
	scale_size_manual(values = 3, guide = FALSE)+
	 ylim(-0.05, 0.055)+
	theme_bw()+
	theme(legend.position="none", 
		text = element_text(size=15), 
		axis.title.x=element_blank(), 
		axis.text.x = element_text(angle = 90))+
	labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")"))
plot_jam_NEE	

library(viridis)
plot_jam_PPFD <- ggplot(gp_NEE_hour, aes(Time, PPFD, colour = PPFD)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
  ylim(0, 2000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(PPFD~~"("~umol~m^{-2}~~s^{-1}~")"))

plot_jam_VPD <- ggplot(gp_NEE_hour, aes(Time, VPD, colour = VPD)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
  ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="VPD (Pa)")
  
plot_jam_rH <- ggplot(gp_NEE_hour, aes(Time, rH, colour = rH)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
  ylim(60, 100)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="rH (%)")

plot_jam_SWC <- ggplot(gp_NEE_hour, aes(Time, SWC, colour = SWC)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
#  ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(SWC~~"("~m^{3}~~m^{-3}~~")")) 
 
plot_jam_Tsoil <- ggplot(gp_NEE_hour, aes(Time, Tsoil, colour = Tsoil)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
  #ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(Tsoil~~"("^{o}~~C~")"))

plot_jam_Tair <- ggplot(gp_NEE_hour, aes(Time, Tair, colour = Tair)) + 
  geom_jitter(size = 2, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
  ylim(20, 35)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90)) + 
  labs(y=expression(Tair~~"("^{o}~~C~")"))

plot_jam_PPFD+plot_jam_VPD+plot_jam_rH+plot_jam_SWC+plot_jam_Tsoil+plot_jam_Tair+plot_layout(ncol=1)


## make daily plot	

plot_hari_NEE1 <- ggplot(gp_NEE_hour, aes(x = Date, y = NEE, fill = Date)) +
  geom_boxplot(aes(group = Date), alpha = 0.80) +
  geom_jitter(alpha = 0.50) + 
  facet_grid(daytime ~., scales = "free_y", space = "free", 
             labeller = labeller(daytime = c("0" = "Malam", "1" = "Siang"))) +
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  # scale_fill_magma_d(name = "Date") +  # for magma palette
   ylim(-0.05, 0.055)+ theme_bw() +
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) +
  labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")"))

	 
plot_hari_NEE2 <- ggplot(gp_NEE_hour, aes(Date, NEE, fill = Date)) +	
	geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
	scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
	 ylim(-0.05, 0.055)+
	theme_bw()+
	theme(legend.position="none", 
		text = element_text(size=15), 
		axis.title.x=element_blank(), 
		axis.text.x = element_text(angle = 90))+
	labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")"))
	


plot_hari_NEE1
plot_hari_NEE2

library(viridis)

plot_hari_PPFD <- ggplot(gp_NEE_hour, aes(Date, PPFD, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
  #ylim(0, 2000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(PPFD~~"("~umol~m^{-2}~~s^{-1}~")"))
plot_hari_PPFD



plot_hari_VPD <- ggplot(gp_NEE_hour, aes(Date, VPD, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
 # ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="VPD (Pa)")
  
plot_hari_VPD

plot_hari_rH <- ggplot(gp_NEE_hour, aes(Date, rH, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
  ylim(60, 100)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="rH (%)")

plot_hari_SWC <- ggplot(gp_NEE_hour, aes(Date, SWC, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
 # ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
   labs(y=expression(SWC~~"("~m^{3}~~m^{-3}~~")")) 
  
plot_hari_Tsoil <- ggplot(gp_NEE_hour, aes(Date, Tsoil, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
 # ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(Tsoil~~"("^{o}~~C~")"))

plot_hari_Tair <- ggplot(gp_NEE_hour, aes(Date, Tair, fill = Date)) + 
  geom_boxplot(alpha = 0.80) +
	geom_jitter(alpha = 0.50)+ 
  scale_size_manual(values = 3, guide = FALSE)+
	  scale_fill_viridis_d(name = "Date") +  # for viridis palette
  theme_bw() + 
 # ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90)) + 
  labs(y=expression(Tair~~"("^{o}~~C~")"))

plot_hari_PPFD+plot_hari_VPD+plot_hari_rH+plot_hari_SWC+plot_hari_Tsoil+plot_hari_Tair+plot_layout(ncol=1)

## make cumulative Flux data 

gp_NEE_hour_cum <-EFgp_NEE_mdv %>% 
					mutate(NEE_Mgha = NEE ) %>% 
					select(DateTime, NEE_Mgha) %>% 
					mutate(hourlyDate = floor_date(DateTime,unit='hour')) %>%
					group_by(hourlyDate) %>% 
					summarise(
							meanH_NEE = mean(NEE_Mgha, na.rm = TRUE), 
							sd_NEE = sd(NEE_Mgha, na.rm = TRUE), 
							n = sum(!is.na(NEE_Mgha))
								) %>% 
					mutate(se_NEE = sd_NEE / sqrt(n)) %>% 
					mutate(
							cum_NEE = cumsum(replace_na(meanH_NEE, 0)), 
							cum_se_NEE = sqrt(cumsum(se_NEE^2))
							)


write_xlsx(gp_NEE_hour_cum, "D:/Licor7500/gp_NEE_hour_cum.xlsx") # test if the process run well. Result = OK !! 
					
str(gp_NEE_hour_cum)
		tibble [867 × 7] (S3: tbl_df/tbl/data.frame)
		 $ hourlyDate: POSIXct[1:867], format:  ...
		 $ meanH_NEE : num [1:867] 0.00636 0.01902 0.01506 0.01101 0.00804 ...
		 $ sd_NEE    : num [1:867] 0.01794 0.01173 0.01567 0.00128 0.0056 ...
		 $ n         : int [1:867] 2 2 2 2 2 2 2 2 2 2 ...
		 $ se_NEE    : num [1:867] 0.012682 0.008292 0.011084 0.000902 0.003963 ...
		 $ cum_NEE   : num [1:867] 0.00636 0.02538 0.04043 0.05144 0.05948 ...
		 $ cum_se_NEE: num [1:867] 0.0127 0.0152 0.0188 0.0188 0.0192 ...
		 
		 
plot_NEE2 <- ggplot(data = gp_NEE_hour_cum, aes(x = hourlyDate, y = cum_NEE)) + 
  geom_line(aes(group = 1), size = 1.5, color = "black", stroke = 2) + 
  geom_ribbon(aes(y = cum_NEE, ymin = cum_NEE - 1.96 * cum_se_NEE, ymax = cum_NEE + 1.96 * cum_se_NEE), 
              alpha = 0.5, fill = "darkseagreen3") + 
  # geom_point(aes(y = mean), color = "aquamarine4") + 
  theme_bw() + 
  ylim(0,6)+
  theme(text = element_text(size = 15)) + 
  labs(y = expression(CO[2] ~ Flux ~ Netto ~~ " (" ~ Mg ~ ha^{-1} ~ ")"), 
       x = "")+ 
  scale_x_datetime(breaks = seq(min(gp_NEE_hour_cum$hourlyDate), max(gp_NEE_hour_cum$hourlyDate), by = "1 days"), 
                   labels = function(x) format(x, "%Y-%m-%d %H:%M"))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))

plot_NEE2
 
 
plot_NEE1 <- ggplot(gp_NEE_hour_cum, aes(x = hourlyDate, y = meanH_NEE)) +					
					geom_line(color = "aquamarine4", lwd = .7) + 
					geom_ribbon(aes(ymin = meanH_NEE - sd_NEE, ymax = meanH_NEE + sd_NEE), alpha = .5,
						fill = "darkseagreen3", color = "transparent") +
					labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")"), x = "")+
					theme_bw()+
					ylim(-0.05, 0.075)
					
plot_NEE1	 
