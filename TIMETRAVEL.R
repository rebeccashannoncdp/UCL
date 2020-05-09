# STDM Coursework - Spatio-Temporal Data Summary Statistics
# Rebecca Shannon

rm(list=ls())
load("C:/Users/rebec/Downloads/UJTWorkSpace.RData")

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(lattice)
library(matrixStats)
library(tmap)
#library(spdep)

LCAPShp_WB <- LCAPShp[LCAPShp$BOROUGH == "Westminster",] # Shapefile for WB
#plot(LCAPShp_WB, col = "black", main = "Westminster Borough")
  
LCAPShp_WB_Data <- LCAPShp_WB@data                    # Dataframe for WB containing spatial info
WB_ID <- LCAPShp_WB_Data$LCAP_ID %>% unique()
UJT_ID <- as.integer(colnames(UJT))
WestminsterBorough <- intersect(WB_ID, UJT_ID)
LCAPShp_WBid_Data <- LCAPShp_WB_Data %>% filter(LCAPShp_WB_Data$LCAP_ID %in% WestminsterBorough)

LCAPShp_WB_UJT <- LCAPShp_WB[LCAPShp_WB$LCAP_ID %in% WestminsterBorough,] # Shapefile for WB
#plot(LCAPShp_WB_UJT, col = "blue", add = TRUE)

# MAP PLOT
sample_map <- tm_shape(LCAPShp, bbox = c(-0.25, 51.48, -0.1, 51.55))+tm_lines(col = "grey", alpha = 0.5)+
  tm_shape(LCAPShp_WB)+tm_lines(lwd = 2)+tm_scale_bar()+tm_grid(labels.size =0.6)+
  tm_shape(LCAPShp_WB_UJT)+tm_lines(col = "blue", lwd = 3)+
  tm_text("LCAP_ID",size=0.7, auto.placement = TRUE, bg.color = "white")
sample_map

LCAAdj_WB       <- LCAPAdj[, c(as.character(WestminsterBorough))]   # Adjacency matrix for WB_id containing links
UJT_WB          <- UJT[, c(as.character(WestminsterBorough))]       # Data matrix for WB_id containing travel time (s/m)

# SUMMARY STATISTICS
WB_Mean         <- mean(UJT_WB)   # January 6am - 9pm Mean       
WB_Sdev         <- sd(UJT_WB)     # January 6am - 9pm STDEV
hist(UJT_WB)                      # Histogram - skewed LHS
abline(v=WB_Mean, col="red")      # Mean is affected by potential high outliers
qqnorm(UJT_WB)                    # Distribution plot - skews very high upper bound
qqline(UJT_WB, col="red")         # Normal distribution line

paste0("The average January travel time in the Westminster Borough between 6am and 9pm was ", WB_Mean,
       "and the standard deviation was ", WB_Sdev)

UJT_WB <- cbind(dates, UJT_WB) # Added dates

avg_travel_time_L           <- data.frame(Link=colnames(UJT_WB[,3:27]), Mean=colMeans(UJT_WB[,3:27]))
avg_travel_time_date_L      <- aggregate(UJT_WB[,c(1,3:27)], by=list(UJT_WB$Date), FUN=mean) %>% select(-c(Group.1))
avg_travel_time_date_L_long <- melt(avg_travel_time_date_L, id.vars = 1)
avg_travel_time_date        <- data.frame(Date=avg_travel_time_date_L[,1], Mean=rowMeans(avg_travel_time_date_L[,-1]),
                                          STDEV=rowSds(as.matrix(avg_travel_time_date_L[,-1])))
avg_travel_time_date$Day    <- weekdays(as.Date(avg_travel_time_date$Date))
avg_travel_time_date$W      <- ifelse(avg_travel_time_date$Day == "Saturday" | avg_travel_time_date$Day == "Sunday",
                                    "Weekend", "Weekday")


avg_travel_time_time_L      <- aggregate(UJT_WB[,c(2,3:27)], by=list(UJT_WB$Time), FUN=mean) %>% select(-c(Group.1))
avg_travel_time_time_L_long <- melt(avg_travel_time_time_L, id.vars = 1)
avg_travel_time_time        <- data.frame(Time=avg_travel_time_time_L[,1], Mean=rowMeans(avg_travel_time_time_L[,-1]),
                                          STDEV=rowSds(as.matrix(avg_travel_time_time_L[,-1])))

date_start <- as.POSIXct(paste0("06:00:00"), format= "%H:%M:%S")
date_end <- as.POSIXct(paste0("20:55:00"), format= "%H:%M:%S")
date_range <- seq.POSIXt(date_start, date_end, by = "5 min")
avg_travel_time_time$Time_Conv <- date_range

date_format <- function(format = "%H:%M") {function(x) format(x, format)}

#PLOTS
ggplot(avg_travel_time_date_L_long, aes(x=Date, y=value)) + geom_point(alpha=0.3, size=0.5) +
  geom_line(aes(color=variable)) + labs(x = "Date", y = "Mean Travel Time (sec/m)")

ggplot(avg_travel_time_date, aes(x=Date, y=Mean)) + geom_point(aes(colour = factor(W))) +
  geom_ribbon(aes(ymin=Mean-STDEV, ymax=Mean+STDEV), linetype=2, alpha=0.1)+
  ylim(0.05, 0.45)+
  labs(x = "Date", y = "Mean Travel Time (sec/m)") + theme(legend.title = element_blank())

ggplot(avg_travel_time_time_L_long, aes(x=Time, y=value)) + geom_point(alpha=0.3, size=0.5) +
  geom_line(aes(color=variable)) + labs(x = "Time of Day", y = "Mean Travel Time (sec/m)")

ggplot(avg_travel_time_time, aes(x=Time_Conv, y=Mean)) + geom_point() +
  geom_ribbon(aes(ymin=Mean-STDEV, ymax=Mean+STDEV), linetype=2, alpha=0.1)+
  ylim(0.05, 0.4)+
  scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) +
  labs(x = "Time of Day", y = "Mean Travel Time (sec/m)")



TEST <- melt(UJT_WB, id.vars = 1:2)
TEST$variable <- as.numeric(as.character(TEST$variable))
TEST_time <- aggregate(TEST[, c(2:4)], by=list(TEST$variable, TEST$Time), FUN=mean) %>% select(-c(Group.1, Group.2))
TEST_date <- aggregate(TEST[, c(1, 3:4)], by=list(TEST$variable, TEST$Date), FUN=mean) %>% select(-c(Group.1, Group.2))

#xyplots of each site - unnecessary

#xyplot(value ~ Date | variable, xlab = "Date", ylab = "Travel Time (sec/m)", type = "l",
#       data=TEST_date,
#       layout=c(5,5),
#       main = "Travel Time Across 25 Links in Westminster Borough")

#xyplot(value ~ Time | variable, xlab = "Time", ylab = "Travel Time (sec/m)", type = "l",
#       data=TEST_time,
#       layout=c(5,5),
#       main = "Travel Time Across 25 Links in Westminster Borough")




###################### Pearson’s Product Moment Correlation Coefficient (PMCC) ######################

# This assumes date is LINEAR - our data is NOT LINEAR. Not good test.

ATT_time_mean <- avg_travel_time_time$Mean
ATT_time_lag <- data.frame(Time = 1:179, ATT = ATT_time_mean[2:(length(ATT_time_mean))],
                           ATT_minus_1 = ATT_time_mean[1:(length(ATT_time_mean)-1)])
ATT_date_mean <- avg_travel_time_date$Mean
ATT_date_lag <- data.frame(Date = avg_travel_time_date[1:29,1], ATT = ATT_date_mean[2:(length(ATT_date_mean))],
                           ATT_minus_1 = ATT_date_mean[1:(length(ATT_date_mean)-1)])

# Calculated the autocorrelation between xt and xt-1 (where xt was the 5 minute interval ATT across all links
# avg over Jan 2011 and xt-1 was the time interval before that). For the length of the series we show the
# temporal autocorrelation in Average Travel Time. The value of the autocorrelation is nearly 1, showing that
# ATT in subsequent time periods are strongly correlated.
ggplot(ATT_time_lag, aes(x=ATT, y=ATT_minus_1)) + geom_point() + labs(y="ATT-1") +
  geom_smooth(method="lm") +
  annotate("text", 0.16, 0.25, label=paste("r =", round(cor(ATT_time_lag$ATT, ATT_time_lag$ATT_minus_1), 3)))

# Calculated the autocorrelation between xt and xt-1 (where xt was the daily ATT across all links avg over Jan 11
# and xt-1 was the day before that). For the length of the series we show the temporal autocorrelation in Average
# Travel Time. The value of the autocorrelation is 0.633, showing that ATT in subsequent days are partially
# positively correlated.

ggplot(ATT_date_lag, aes(x=ATT, y=ATT_minus_1)) + geom_point() + labs(y="ATT-1") +
  geom_smooth(method="lm") +
  annotate("text", 0.2, 0.27, label=paste("r =", round(cor(ATT_date_lag$ATT, ATT_date_lag$ATT_minus_1), 3)))

###################### Autocorrelation Function ######################

acf(ATT_date_mean, lag.max = 28)
pacf(ATT_date_mean, lag.max = 28)


