rm(list = ls(all = TRUE)) 

library(dplyr)
library(zoo)

setwd('D:/Saman/TDI/Q1')

input=read.csv("NYPD_Motor_Vehicle_Collisions (1).csv", header = TRUE)

input$DATE <- as.Date(input$DATE,format = "%m/%d/20%y") 
input$Year <- as.numeric(format(input$DATE,'%Y'))
input$Months <- as.numeric(format(input$DATE,'%m'))

Answer_Q1_a=sum(input[which(input$Year<2019),"NUMBER.OF.PERSONS.INJURED"],na.rm=TRUE)


df = input[which(input$Year==2016),]
brook_inj=nrow(df[which(df$BOROUGH=='BROOKLYN'),] )
Answer_Q1_b=format(brook_inj/(nrow(df)-nrow(df[which(df$BOROUGH==''),] )),nsmall = 10)


structured_df = df[,c("BOROUGH","Year","NUMBER.OF.CYCLIST.INJURED",
                      "NUMBER.OF.CYCLIST.KILLED")]
index_cycle = which((structured_df[,3]+structured_df[,4])>0 & structured_df$Year==2016)
Answer_Q1_c = length(index_cycle)/length(which(structured_df$Year==2016))



library(rvest)    
scotusURL <- "https://en.wikipedia.org/wiki/Demographics_of_New_York_City"
temp <- scotusURL %>% 
  html %>%
  html_nodes("table")
table=(html_table(temp[1])); population=table[[1]][3:7,c(1,3)]


structured_df = input[which(input$Year==2017),]
text_indicator <- structured_df[,18:22]
alcohol_ind <- apply(text_indicator,2,function(x) regexpr('Alcohol',x))
alcohol_ind[alcohol_ind<0]=0; alcohol_ind=which(rowSums(alcohol_ind)>0)
structured_df <- aggregate(UNIQUE.KEY~BOROUGH, as.data.frame(structured_df[alcohol_ind,]),length)
Answer_Q1_d <- max(structured_df[2:6,2]/as.numeric(gsub(",","",population[,2])))



num_veh_alpha <- function(x){
  if(as.character(x)=="") {y=0}
  else
  {y=length(as.character(x))}
}


structured_df = input[which(input$Year==2016),]
vehicle = structured_df[,25:29]
vehicle_ind = sapply(1:nrow(vehicle),function(i) sum(apply(vehicle[i,],2,num_veh_alpha)))
structured_df$vehicle_ind = vehicle_ind
structured_df <- aggregate(vehicle_ind~ZIP.CODE, as.data.frame(structured_df),sum)
Answer_Q1_e <- max(structured_df[,2])


structured_df <- aggregate(BOROUGH~Year, as.data.frame(input),length)
Answer_Q1_f <- format(summary(lm(structured_df[2:7,2]~c(1:6)))$coefficients[2,1],nsmall = 10)


num_veh_mod <- function(x){
  x=as.character(x)
  x=gsub("bike","",x)
  if(x=="") {y=0}
  else
  {y=length(unlist(strsplit(x,'/')))}
}

structured_df = input[which(input$Year==2017),]
vehicle = structured_df[,25:29]
vehicle_ind = sapply(1:nrow(vehicle),function(i) sum(apply(vehicle[i,],2,num_veh_alpha)))
structured_df$vehicle_ind = vehicle_ind
ratio_month <- aggregate(vehicle_ind~Months, 
                         as.data.frame(structured_df),function(x) c(sum(x),sum(x[x>=3])/sum(x)))
number_month <- ratio_month$vehicle_ind[,1]; ratio_month <- ratio_month$vehicle_ind[,2]
Answer_Q1_g = dchisq(((ratio_month[1]/ratio_month[5])-3)^2/3, df=1)


df=structured_df
crash_zip <- aggregate(vehicle_ind~ZIP.CODE,as.data.frame(structured_df),sum)
crash_zip_major <- crash_zip[which(crash_zip$vehicle_ind>1000),]

area_per_zip <- function(df,zipcode){
  
  d=df[which(df$ZIP.CODE==zipcode),5:6]*111.699
  lats = as.numeric(d[,2]); lons = as.numeric(d[,1]); d=cbind(lons,lats)
  
  library(cluster)
  r <- ellipsoidhull(d)
  plot(lons,lats)
  lines( predict(r))
  e <- sqrt(eigen(r$cov)$values)
  a <- sqrt(r$d2) * e[1]  # semi-major axis
  b <- sqrt(r$d2) * e[2]  # semi-minor axis
  area = 3.1426*a*b
}

areas = sapply(1:nrow(crash_zip_major),function(i) area_per_zip(structured_df,crash_zip_major[i,1]))
Answer_Q1_h = format(max(crash_zip_major[,2]/areas),nsmall=10)
