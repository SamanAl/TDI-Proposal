
library(dplyr)
library(zoo)
tinytex:::is_tinytex()


setwd('D:/Saman/TDI')

lines = length(readLines("Data_NYPD.csv"))

input=read.csv("Data_NYPD.csv", header = TRUE, fill = TRUE, sep=',')
df = input[c(4:6,8:12,17:18)]; df$id = input

dates = input$OCCUR_DATE
dates = sapply(1:length(dates), 
               function(i) as.numeric(unlist(strsplit(gsub(" 0:00","",dates[i]),'/'))))

df$MONTH = dates[1,]; df$DAY = dates[2,]; df$YEAR = dates[3,]

times = as.character(input$OCCUR_TIME); 
times = sapply(1:length(times),function(i) as.numeric(unlist(strsplit(times[i],":"))))
df$HOUR = times[1,]
df$monyear <- as.yearmon(paste(df$YEAR, df$MONTH), "%Y %m")
df$dates = as.Date(paste(dates[3,],'-',dates[1,],'-',dates[2,],sep=''))

structured_df <- aggregate(STATISTICAL_MURDER_FLAG~BORO+monyear, as.data.frame(df),length)
sample_of_data=df[sample(100,100),c(1,2,4,5,6,9,10,17)]
colnames(sample_of_data) <- c("Borough","Precinct","Murder","Age Group","Sex","Lat","Lon","Dates")

library(reshape2)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

df_plot <- structured_df[which(structured_df[,1]=='BROOKLYN'),]
df_plot <- melt(df_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")
p1=ggplot(df_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("BROOKLYN")+
  labs(x = "Date", y='Shooting')

df_plot <- structured_df[which(structured_df[,1]=='BRONX'),]
df_plot <- melt(df_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")

p2=ggplot(df_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("BRONX")+
  labs(x = "Date", y='Shooting')

df_plot <- structured_df[which(structured_df[,1]=='MANHATTAN'),]
df_plot <- melt(df_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")

p3=ggplot(df_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("MANHATTAN")+
  labs(x = "Date", y='Shooting')

df_plot <- structured_df[which(structured_df[,1]=='QUEENS'),]
df_plot <- melt(df_plot[, c("monyear", "STATISTICAL_MURDER_FLAG")], id="monyear")

p4=ggplot(df_plot) + geom_line(aes(x=monyear, y=value),color='black',size = 1.2)+ 
  stat_smooth(aes(x = monyear, y = value),color='red', method = "lm",
              formula = y ~ poly(x, 1), se = TRUE)+ ggtitle("QUEENS")+
  labs(x = "Date", y='Shooting')

grid.arrange(p1, p2, p3, p4, ncol = 2)

df_plot <- aggregate(STATISTICAL_MURDER_FLAG~MONTH,as.data.frame(df),length)
barplot(sapply(1:12,function(x) length(df$STATISTICAL_MURDER_FLAG[which(df$MONTH==x)])),
        col=rainbow(12),names=month.abb[1:12],ylab='Number of Shootings',
        font=2,cex.axis = 0.7,cex.names=0.7,cex.lab=0.8)


weather_data=read.csv('D:/Saman/TDI/weather.csv')
weather_dates=as.numeric(as.Date(weather_data$DATE))

structured_df <- aggregate(STATISTICAL_MURDER_FLAG~dates, as.data.frame(df),length)
id_dates_shooting=as.numeric(as.Date(structured_df$dates))

id_dates=as.numeric(as.Date(df$dates))

ranks=apply(weather_data[,c(4,7:9)],2,function(x) round(rank(x)/length(x),2))

weather_shooting=sapply(1:length(id_dates_shooting), 
                        function(x) ranks[which(weather_dates==id_dates_shooting[x]),])


thresh=quantile(structured_df$STATISTICAL_MURDER_FLAG,seq(0,1,0.2))
thresh=as.numeric(thresh)+0.5
labels_ins <- c('0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1')

classified_shootings=cut(structured_df$STATISTICAL_MURDER_FLAG,
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


library(pointdensityP)
library(ggmap)

register_google(key = "")
has_google_key()

density_shot = NULL

for (i in 1:12){
  
  yr_ind = 2006:2018
  
  dp=pointdensity(df[which(df$YEAR==yr_ind[i]),], lat_col="Latitude", lon_col="Longitude",date_col = "dates", 1, 2)
  density_shot = c(density_shot,max(dp$count))
  nyc_map <- get_map(location = c(lon = -73.95, lat = 40.73), maptype = "terrain", zoom = 11)
  p=ggmap(nyc_map)
  p=p + geom_point(data=dp[c((nrow(dp)-40),nrow(dp)),], aes(x=lon, y=lat),size=8,color='darkred')+ theme(axis.text=element_text(size=6),
                                                                                                         axis.title=element_text(size=10,face="bold"))+ggtitle(yr_ind[i])
  assign(paste("P", i, sep = ""), p)  
}

pcomb=grid.arrange(P3, P6, P9, P12, ncol = 2)


df_plot = list(); df_plot$Density = density_shot; df_plot$Year=c(2005.5:2016.5)
df_plot = as.data.frame(df_plot)
ggplot(df_plot) + geom_line(aes(x=Year, y=Density),color='black',size = 1.2)+ggtitle("Density")+scale_x_continuous(labels=scaleFUN)



