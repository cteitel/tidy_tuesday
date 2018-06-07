library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

bikeRaw <- read_csv("week10_biketown.csv")

#http://w2.weather.gov/climate/local_data.php?wfo=pqr
weather <- read_csv("Portland_dailyclimatedata.csv" , skip = 6)

bike <- mutate(bikeRaw , bearing = atan((EndLongitude-StartLongitude)/(EndLatitude-StartLatitude)),
               StartDate = mdy(StartDate ))
bike <- mutate(bike , dow = wday(StartDate , label = T),
               durMins = minute(Duration),
               monthYear = format(as.Date(StartDate), "%Y-%m"))

bikeDates <- group_by(bike , StartDate)
dateSummary <- summarize(bikeDates ,
                         StartTime = mean(StartTime , na.rm = T) ,
                         Distance = mean(Distance_Miles , na.rm = T),
                         count = n(),
                         Duration = median(Duration , na.rm = T))
dateSummary <- mutate(dateSummary , Duration = as.numeric(Duration)/60)

precip <- filter(weather , X3 == "PR")
weatherLong <- gather(precip , key = "day" , value = "precip" , "1":"31")
weatherLong <- filter(weatherLong ,
                      YR %in% c(2016:2018))
weatherLong <- mutate(weatherLong , date = ymd(str_c(YR,MO,day,sep="-")),
                      precip = as.numeric(precip))

dateSummary <- left_join(dateSummary , weatherLong , c("StartDate" = "date"))

p <- ggplot(dateSummary , aes(x = StartDate , y = count)) +
  geom_col(aes(fill = Duration)) +
  theme_classic(base_size = 14) +
  scale_fill_gradient2("Median ride time (mins)",low =  brewer.pal(8,"PiYG")[8], mid = brewer.pal(8,"PiYG")[4],high = brewer.pal(11,"PiYG")[1],
                       midpoint = 15) +
  ylab("Number of rides (per day)") + xlab("Date") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  # geom_col(aes(x = StartDate , y = -1 * precip*1000) , color = rgb(.2,.3,1)) +
  # scale_y_continuous(breaks = seq(0,3000,1000),
  #   sec.axis = sec_axis(~ . / (-1000) ,name = "Precipitation (in)",
  #          breaks = seq(0,2.2,.5)))+
  # geom_line(y=0) +
  theme(legend.position = c(0.95,0.95) , legend.justification = c("right","top") ,
        legend.direction = "horizontal" , legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        plot.title = element_text(hjust = 0.5,size = 20,face="bold"),
        #legend.box.background = element_rect(color = "black"),
        #axis.text.y = element_text(color = brewer.pal(8,"PiYG")[1]),
        axis.text.y.right = element_text(color = rgb(.2,.3,1))) +
  ggtitle("Bike when it's dry!")



p2 <- ggplot(dateSummary , aes(StartDate , y = precip))+
  geom_col( color = rgb(.2,.3,1)) +
  theme_classic(base_size = 14) +
  ylab("Daily precipitation (in)") + xlab("Date")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") 
  
  
grid.arrange(p,p2,nrow = 2)


