
#import DataFrame libraries
library(dplyr)
library(zoo)
library(pointdensityP)
tinytex:::is_tinytex()


#import visualization libraries
library(reshape2)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#set working directory
setwd('~/Saman/TDI')


#import gun violence data
lines = length(readLines("Data_NYPD.csv"))
input=read.csv("Data_NYPD.csv", header = TRUE, fill = TRUE, sep=',')
nyc_gun_data= input[c(4:6,8:12,17:18)]; nyc_gun_data$id = input


#import weather data
weather_data=read.csv('weather.csv')
weather_dates=as.numeric(as.Date(weather_data$DATE))

#Structuring the dates
dates = input$OCCUR_DATE
dates = sapply(1:length(dates), 
               function(i) as.numeric(unlist(strsplit(gsub(" 0:00","",dates[i]),'/'))))

nyc_gun_data$MONTH = dates[1,]; nyc_gun_data$DAY = dates[2,]; nyc_gun_data$YEAR = dates[3,]

times = as.character(input$OCCUR_TIME); 
times = sapply(1:length(times),function(i) as.numeric(unlist(strsplit(times[i],":"))))
nyc_gun_data$HOUR = times[1,]
nyc_gun_data$monyear <- as.yearmon(paste(nyc_gun_data$YEAR, nyc_gun_data$MONTH), "%Y %m")
nyc_gun_data$dates = as.Date(paste(dates[3,],'-',dates[1,],'-',dates[2,],sep=''))


#insights from data aggregation
structured_nyc_gun_data<- aggregate(STATISTICAL_MURDER_FLAG~BORO+monyear, as.data.frame(nyc_gun_data),length)
sample_of_data=nyc_gun_data[sample(100,100),c(1,2,4,5,6,9,10,17)]
colnames(sample_of_data) <- c("Borough","Precinct","Murder","Age Group","Sex","Lat","Lon","Dates")


# EDA plots on the general trends in each location
nyc_gun_data_plot <- structured_nyc_gun_data[which(structured_nyc_gun_data[,1]=='BROOKLYN'),]
nyc_gun_data_plot <- melt(nyc_gun_data_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")
p1=ggplot(nyc_gun_data_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("BROOKLYN")+
		labs(x = "Date", y='Shooting')

nyc_gun_data_plot <- structured_nyc_gun_data[which(structured_nyc_gun_data[,1]=='BRONX'),]
nyc_gun_data_plot <- melt(nyc_gun_data_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")


p2=ggplot(nyc_gun_data_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("BRONX")+
  labs(x = "Date", y='Shooting')

nyc_gun_data_plot <- structured_nyc_gun_data[which(structured_nyc_gun_data[,1]=='MANHATTAN'),]
nyc_gun_data_plot <- melt(nyc_gun_data_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")


p3=ggplot(nyc_gun_data_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("MANHATTAN")+
  labs(x = "Date", y='Shooting')

nyc_gun_data_plot <- structured_nyc_gun_data[which(structured_nyc_gun_data[,1]=='QUEENS'),]
nyc_gun_data_plot <- melt(nyc_gun_data_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")


p4=ggplot(nyc_gun_data_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("QUEENS")+
  labs(x = "Date", y='Shooting')

grid.arrange(p1, p2, p3, p4, ncol = 2)


# EDA plots to visualize seasonality 
nyc_gun_data_plot <- aggregate(STATISTICAL_MURDER_FLAG~MONTH,as.data.frame(nyc_gun_data),length)
barplot(sapply(1:12,function(x) length(nyc_gun_data$STATISTICAL_MURDER_FLAG[which(nyc_gun_data$MONTH==x)])),
        col=rainbow(12),names=month.abb[1:12],ylab='Number of Shootings',
        font=2,cex.axis = 0.7,cex.names=0.7,cex.lab=0.8)



structured_nyc_gun_data<- aggregate(STATISTICAL_MURDER_FLAG~dates, as.data.frame(nyc_gun_data),length)
id_dates_shooting=as.numeric(as.Date(structured_nyc_gun_data$dates))

id_dates=as.numeric(as.Date(nyc_gun_data$dates))

ranks=apply(weather_data[,c(4,7:9)],2,function(x) round(rank(x)/length(x),2))

weather_shooting=sapply(1:length(id_dates_shooting), 
                        function(x) ranks[which(weather_dates==id_dates_shooting[x]),])


thresh=quantile(structured_nyc_gun_data$STATISTICAL_MURDER_FLAG,seq(0,1,0.2))
thresh=as.numeric(thresh)+0.5
labels_ins <- c('0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1')

classified_shootings=cut(structured_nyc_gun_data$STATISTICAL_MURDER_FLAG,
                         breaks = as.numeric(thresh), 
                         labels = as.character(1:(length(thresh)-1)))


boxplot(sapply(1:(length(thresh)-1), function(x) weather_shooting[3,which(classified_shootings==x)]),col=rainbow(5),
        staplecol=rainbow(5),whiskcol=rainbow(5),
        boxcol=rainbow(5),medcol='darkgrey', 
        xlab='Quantile Ranges of Shoorting Rates',
        whisklty=1,ylab='Max Temperature Quantile',
        outline=FALSE,names=labels_ins,boxwex = 0.4,
        font=2,cex.axis = 0.7,cex.names=0.7,cex.lab=0.8)

boxplot(sapply(1:(length(thresh)-1), function(x) weather_shooting[1,which(classified_shootings==x)]),
        staplecol=rainbow(5),whiskcol=rainbow(5),
        boxcol=rainbow(5),medcol=rainbow(5),
        xlab='Quantile Ranges of Shoorting Rates',
        whisklty=1,ylab='Precipitation Quantile',
        outline=FALSE,names=labels_ins,boxwex = 0.4,
        font=2,cex.axis = 0.7,cex.names=0.7,cex.lab=0.8)



#EDA plots for Spatial kernel density
register_google(key = OS.ENV.KEY)
has_google_key()

#Show the density map using KDE
density_shot = NULL

for (i in 1:12){
  
  yr_ind = 2006:2018
  
  dp=pointdensity(nyc_gun_data[which(nyc_gun_data$YEAR==yr_ind[i]),], lat_col="Latitude", lon_col="Longitude",date_col = "dates", 1, 2)
  density_shot = c(density_shot,max(dp$count))
  nyc_map <- get_map(location = c(lon = -73.95, lat = 40.73), maptype = "terrain", zoom = 11)
  p=ggmap(nyc_map)
  p=p + geom_point(data=dp[c((nrow(dp)-40),nrow(dp)),], 
	aes(x=lon, y=lat),size=8,color='darkred')+ 
	theme(axis.text=element_text(size=6),
	axis.title=element_text(size=10,face="bold"))+ggtitle(yr_ind[i])

  assign(paste("P", i, sep = ""), p)  
}

pcomb=grid.arrange(P3, P6, P9, P12, ncol = 2)

