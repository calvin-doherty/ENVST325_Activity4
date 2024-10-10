install.packages(c("lubridate","dplyr","ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings="#N/A")
weather$dateF <- mdy_hm(weather$Date)

interval <- weather$dateF [-length(weather$dateF)] %--% weather$dateF [-1]
interval
# set up time intervals in a vector of dates
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}
timeInterval(weather$dateF)

for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1, 4, 6)
for(i in seqEx){
  print(paste("example", i))
}

chEx <- character()
for (i in 1:6) {
  chEx[i] <- paste('example', i)
}

numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}

numEx2 <- numeric()
for(i in 2:6){
  numEx2[i] <- 6*i
}

# Prompt 1
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)
Jan22 <- weather %>%
  filter(month==1 & year==2022)

mean(Jan22$AirTemp[1:8])

rollAveTemp <- numeric()
for(i in 8:nrow(Jan22)){
  rollAveTemp[i] <- mean(Jan22$AirTemp[(i-7):i])
}

Jan22$rollAveTemp <- rollAveTemp

ggplot(Jan22, aes(x = dateF)) +
  geom_line(aes(y = AirTemp, color = "Air Temp")) +  
  geom_line(aes(y = rollAveTemp, color = "Rolling Avg Temp")) +  
  labs(x = "Date", y = "Air Temperature (°C)")

# Prompt 2
MayJune21 <- weather %>%
  filter(month==5 & year==2021|month==6 & year==2021)

ggplot()+
  aes(x=MayJune21$dateF, y=MayJune21$SolRad)+
  geom_line()+
  labs(x = "Date", y = "Solar Radiation")

# Question 1
weatherClean <- weather %>%
  filter(Precip>=0 & XLevel<2 & YLevel<2)

# Question 2
weather$VoltageFlag <- ifelse(weather$BatVolt < 8.5, 
                             1,
                             0)

# Question 3
weather$TempRadFlag <- ifelse(weather$AirTemp > 38 | weather$AirTemp < -28 |
                                weather$SolRad > 1500, 
                              1,
                              0)

# Question 4

Jan_Mar21 <- weather %>%
  filter(year==2021 & month==1 | year==2021 & month==2 | year==2021 & month==3)

ggplot() +
  aes(x=Jan_Mar21$dateF, y=Jan_Mar21$AirTemp)+
  geom_line(col="blue")+
  labs(x = "Date", y = "Air Temperature (°C)")
  

# Question 5
MarApr_21 <- weather %>% filter(year == 2021 & month %in% c(3,4))
MarApr_21 <- MarApr_21 %>%
  mutate(AirTempF = (AirTemp*(9/5)) + 32)

for (i in (2: length(MarApr_21$AirTemp))) {
  MarApr_21$Precip[i] <- ifelse(MarApr_21$AirTempF[i] < 35 | MarApr_21$AirTempF[i-1] < 35, NA, MarApr_21$Precip[i])
}

sum(as.integer(!is.na(MarApr_21$Precip)))


