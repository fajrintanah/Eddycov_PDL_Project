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
library(GGally) # displaying pairs panel
library(caret)
library(caTools) # split dataset


# plot NEE to Time

load("E:/Fajrin/Licor7500/25022025/olaheddycov25022025.RData") # save the workspace for backup

### ---- Binning Net emission to times. Averaging half-hourly to Hourly Data 
str(gp_NEE_hour)


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

write_xlsx(gp_NEE_hour, "E:/Fajrin/Licor7500/25022025/gp_NEE_hour.xlsx") # test if the process run well. Result = OK !! 
 

# make summary of the flux data for reporting

summary(gp_NEE_hour) #summary all

 
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
  #  parameter     mean       sd     min       max
  #  <chr>        <dbl>    <dbl>   <dbl>     <dbl>
  #1 NEE         0.0126   0.0204 -0.0876    0.0776
  #2 PPFD      336.     492.      0      1971.
  #3 VPD       641.     718.      0      3477.
  #4 SWC         0.419    0.250   0.200     0.95  
  #5 Tsoil      29.6      1.33   26.3      34.8
  #6 Tair       26.6      3.26   19.5      36.6


  
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
  #  daytime parameter       mean        sd     min       max   
  #     <dbl> <chr>          <dbl>     <dbl>   <dbl>     <dbl>
  # 1       0 NEE          0.0294    0.00410  0.0161    0.0522
  # 2       0 PPFD         0.0203    0.439    0        14.8      
  # 3       0 VPD        176.      208.       0      1446.       
  # 4       0 SWC          0.418     0.249    0.200     0.95  
  # 5       0 Tsoil       29.5       1.09    26.3      33.6      
  # 6       0 Tair        24.3       1.40    19.5      29.8      
  # 7       1 NEE         -0.00423   0.0157  -0.0876    0.0776   
  # 8       1 PPFD       672.      509.       0      1971.    
  # 9       1 VPD       1106.      746.       0      3477.       
  #10       1 SWC          0.420     0.250    0.201     0.95     
  #11       1 Tsoil       29.7       1.53    26.3      34.8      
  #12       1 Tair        28.8       3.07    20.6      36.6   



save.image("E:/Fajrin/Licor7500/25022025/olaheddycov25022025_1.RData") # save the workspace for backup




## make diurnal plot	
		 
plot_jam_NEE <- ggplot(gp_NEE_hour, aes(Time, NEE,colour = Time)) +
	geom_jitter(alpha = 0.40)+ 
	geom_boxplot(col = "grey40", alpha = 0.40) +
	geom_smooth(aes(group = 1, col = "yellow"), method="lm", formula =  y ~ poly(x, 6))+
	scale_size_manual(values = 3, guide = FALSE)+
	 ylim(-0.055, 0.055)+
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
 # ylim(0, 2000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y=expression(PPFD~~"("~umol~m^{-2}~~s^{-1}~")"))

plot_jam_VPD <- ggplot(gp_NEE_hour, aes(Time, VPD, colour = VPD)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
 # ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="VPD (Pa)")


plot_jam_ET <- ggplot(gp_NEE_hour, aes(Time, ET, colour = ET)) + 
  geom_jitter(size = 2.5, alpha = 0.60) + 
  scale_color_viridis_c(option = "viridis") + 
  theme_bw() + 
#  ylim(0, 3000)+
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x= element_blank(), 
        axis.text.x = element_blank()) + 
  labs(y="ET (mm/jam)")

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

plot_jam_PPFD+plot_jam_VPD+plot_jam_ET+plot_jam_rH+plot_jam_SWC+plot_jam_Tsoil+plot_jam_Tair+plot_layout(ncol=1)

plot_jam_ET+plot_jam_NEE+plot_layout(ncol=1)
## make daily plot	

plot_hari_NEE1 <- ggplot(gp_NEE_hour, aes(x = Date, y = NEE, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  facet_grid(daytime ~., scales = "free_y", space = "free", 
             labeller = labeller(daytime = c("0" = "Malam", "1" = "Siang"))) +
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  ylim(-0.05, 0.055)+ theme_bw() +
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank()) +
  labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())


plot_hari_NEE2 <- ggplot(gp_NEE_hour, aes(x = Date, y = NEE, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
   geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  ylim(-0.05, 0.055) +
  theme_bw() +
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d"))


plot_hari_NEE1
plot_hari_NEE2


library(viridis)
plot_hari_PPFD <- ggplot(gp_NEE_hour, aes(x = Date, y = PPFD, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y=expression(PPFD~~"("~umol~m^{-2}~~s^{-1}~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())



plot_hari_VPD <- ggplot(gp_NEE_hour, aes(x = Date, y = VPD, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y="VPD (Pa)") +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())

plot_hari_ET <- ggplot(gp_NEE_hour, aes(x = Date, y = ET, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y="ET (mm/jam)") +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())

plot_hari_Precipt <- ggplot(gp_NEE_hour, aes(x = Date, y = Precpt, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y="Curah Hujan (mm)") +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())


plot_hari_rH <- ggplot(gp_NEE_hour, aes(x = Date, y = rH, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  ylim(60, 100) +
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y="rH (%)") +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())


plot_hari_SWC <- ggplot(gp_NEE_hour, aes(x = Date, y = SWC, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y=expression(SWC~~"("~m^{3}~~m^{-3}~~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())


plot_hari_Tsoil <- ggplot(gp_NEE_hour, aes(x = Date, y = Tsoil, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y=expression(Tsoil~~"("^{o}~~C~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d")) +
  theme(axis.text.x = element_blank())


plot_hari_Tair <- ggplot(gp_NEE_hour, aes(x = Date, y = Tair, fill = as.factor(Date))) +
  geom_boxplot(alpha = 0.80, aes(color = "black"), color = "black") +
  geom_jitter(alpha = 0.10) + 
  scale_size_manual(values = 3, guide = FALSE) +
  scale_fill_viridis_d(name = "Date") +  
  theme_bw() + 
  theme(legend.position="none", 
        text = element_text(size=15), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
  labs(y=expression(Tair~~"("^{o}~~C~")")) +
  scale_x_date(breaks = seq(min(gp_NEE_hour$Date, na.rm = TRUE), max(gp_NEE_hour$Date, na.rm = TRUE), by = "7 days"), 
               labels = function(x) format(x, "%Y-%m-%d"))


plot_hari_PPFD+plot_hari_VPD+plot_hari_ET+plot_hari_Precipt+plot_hari_rH+
plot_hari_SWC+plot_hari_Tsoil+plot_hari_Tair+plot_layout(ncol=1)

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
