library(DBI)
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)

###################DATABASE CREATION########################
#Check if database exists
setwd("E:/ST2195/Coursework/")
if (file.exists("airline2.db")) {
  file.remove("airline2.db")
  cat("File deleted.")
} else {
  print("File does not exist.")
}

#Create database and connect
conn <- dbConnect(RSQLite::SQLite(), "airline2.db")

#Create data frames
setwd("E:/ST2195/Coursework/datasets/ontime/")
csv_ontime <- dir("E:/ST2195/Coursework/datasets/ontime/")
ontime <- do.call(rbind,lapply(csv_ontime,read.csv))

setwd("E:/ST2195/Coursework/datasets/")
airports <- read.csv("airports.csv", header=TRUE)
carriers <- read.csv("carriers.csv", header=TRUE)
planes <- read.csv("plane-data.csv", header=TRUE)

#Copy data frames and put in database as tables
dbWriteTable(conn, "ONTIME", ontime)
dbWriteTable(conn, "AIRPORTS", airports)
dbWriteTable(conn, "CARRIERS", carriers)
dbWriteTable(conn, "PLANES", planes)


######################SUB-SETTING###########################
#Check for missing values
summary(ontime)
summary(airports)
summary(carriers)
summary(planes)

#Sub-setting relevant variables
ontime2 <- ontime[c(1:11,15:19,22:24,29)]

#Create new variable and column named FlightDate 
date <- paste(ontime2$Month, ontime2$DayofMonth, ontime2$Year, sep="/")
date2 <- as.Date(date, format="%m/%d/%Y")
ontime2$FlightDate <- date2
#Add Day variable
ontime2$Day <- as.factor(weekdays(ontime2$FlightDate))


#######################################QUESTION ONE###########################################
#Q1. What is the best time of day, day of the week and time of year to fly to minimise delays?

#Check frequency of flights on certain days
table(ontime2$Day)


#Drawing a stratified sample of roughly 200,000 with proportional size for each day
table(ontime2$Day)/nrow(ontime2) * 200000
set.seed(7)
o <- ontime2 %>%
  group_by(Day, Month) %>%
  sample_frac(size=.0144)
#Checking if proportions are similar to population as well as being an accurate representation of it
nrow(o)
table(o$Day)
summary(o)
summary(ontime2)
var(o$DepDelay, na.rm=T)
var(ontime2$DepDelay, na.rm=T)
var(o$ArrDelay, na.rm=T)
var(ontime2$ArrDelay, na.rm=T)

boxplot(DepDelay~Day, ontime2)
#############What is the best time of the day to fly?##################
#Create copy of sample
o2 <- o

#Changing integer values of time-based variables to readable time format
o2[5:8] <- o2[5:8]%>%
  lapply(sprintf, fmt="%04d")%>%
  lapply(strptime, format= "%H%M")%>%
  lapply(as.POSIXct, format= "%H:%M")

#Making a graph to determine best time to fly
classes <- cut(as.POSIXlt(o2$CRSDepTime)$hour, breaks=c(0:24), 
               c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"),
               right=F, include.lowest = T)
DelayTimes <-data.frame(timeclass=classes,
                    o2$DepDelay,
                    o2$ArrDelay)


avg_time_delay <- DelayTimes %>%
  group_by(timeclass)%>%
  summarise(ArrDelay=mean(o2.ArrDelay, na.rm=T),
            DepDelay=mean(o2.DepDelay, na.rm=T),
            NumOfFlights=n())%>%
  arrange(timeclass)

avg_time_delay$logFlights <- log(avg_time_delay$NumOfFlights)
avg_time_delay


melttime <- melt(avg_time_delay[1:3])
ggplot(melttime, mapping= aes(x=timeclass, y=value, fill=variable))+
  geom_bar(stat="identity", position=position_stack())+
  ggtitle("Average delay by time (hour)")+
  xlab("Time(Hour 00:00 - 24:00)")+ylab("Average Delay (in minutes)")

ggplot(avg_time_delay, aes(x=logFlights, y=DepDelay))+
  geom_point(size=2)+
  geom_text(hjust=-.2, vjust=0,label=avg_time_delay$timeclass)

  

barplot(t(as.matrix(avg_time_delay[, 2:3])),
        names.arg=avg_time_delay$timeclass,
        legend.text=T,
        col=c("red","blue"),
        beside=T,
        main="Total Delay (in minutes) by Time",
        ylim=c(-2,40),
        ylab="Total Delay (in minutes)",
        xlab="Time")


##############What is the best day of the week to fly?#################

#Finding the average and sum delay for each day
avg_daily_delay <- o2 %>%
  group_by(Day)%>%
  summarize(DepDelay=mean(DepDelay, na.rm=T),
            ArrDelay=mean(ArrDelay, na.rm=T),
            NumOfFlights=n())%>%
  arrange(DepDelay)

#Plot results for better visualization

barplot(t(as.matrix(avg_daily_delay[, 2:3])),
        beside=T,
        names.arg=avg_daily_delay$Day,
        legend.text=T,
        col=c("red","blue"),
        main="Average Delay (in minutes) by Day",
        ylim=c(0,13),
        ylab="Average Delay (in minutes)",
        xlab="Day")

meltday <- melt(avg_daily_delay[1:3])
meltday[4] <- avg_daily_delay[4]
ggplot(meltday, aes(x=NumOfFlights, y=value, color=variable))+
  geom_line(size=2)+
  geom_text(hjust=-.1, vjust=0,label=meltday$Day)+
  xlab("Flights")+ylab("Average Delay (in minutes)")

############What is the best time of the year to fly?############
#Adding name of Month to sample
o2$NameOfMonth <- as.factor(months(o2$FlightDate))

avg_monthly_delay <- o2 %>%
  group_by(NameOfMonth)%>%
  summarize(DepDelay=mean(DepDelay,na.rm=T),
            ArrDelay=mean(ArrDelay, na.rm=T),
            NumOfFlights=n())%>%
  arrange(DepDelay)

#Plot results for better visualization
meltmonth <- melt(avg_monthly_delay[1:3])
ggplot(meltmonth, aes(x=reorder(NameOfMonth, +value), y=value, fill=variable))+
  geom_bar(stat = "identity", position = 'dodge')+
  xlab("Month")+ylab("Average delay (in minutes)")

meltmonth[4] <- avg_monthly_delay[4]
ggplot(meltmonth, aes(x=NumOfFlights, y=value, color=variable))+
  geom_line()+
  geom_point()+
  geom_text(hjust=-.15, vjust=0,label=meltmonth$NameOfMonth)+
  xlab("Flights")+ylab("Average Delay (in minutes")


#barplot(t(as.matrix(cancelled_monthly[,2])),
#        names.arg=cancelled_monthly$NameOfMonth,
#        col=c("red"),
#        main="Cancelled Flights",
#        ylim=c(0,600),
#        ylab="Cancelled Flights",
#        xlab="Day")

######################################QUESTION TWO########################################
#Clean up 'planes' dataset by dropping rows with invalid/empty issue date 
planes2 <- planes%>%
  filter(issue_date!="None", 
         issue_date!="")

planes2$issue_date <- as.Date(planes2$issue_date, format="%m/%d/%Y")

#############Do older planes suffer more delays?################
#Subset
q2 <- o2 %>%
  inner_join(planes2, by=c("TailNum"="tailnum"))%>%
  mutate(YearIssued=format(issue_date, "%Y"))%>%
  group_by(YearIssued)%>%
  summarise(DepDelay=mean(DepDelay, na.rm=T), 
            ArrDelay=mean(ArrDelay, na.rm=T), 
            Flights=n())
q2 <- subset(q2, YearIssued<=2005)

#Normalize by log transformation
q2$logFlights <- log(q2$Flights)


#Plot for visual
ggplot(q2, aes(x=YearIssued, y=DepDelay, group=1))+
  geom_line(color="steelblue")+
  geom_point()

ggplot(q2, aes(x=YearIssued, y=Flights))+
  geom_bar(stat="identity", position=position_stack(), fill="steelblue")


q2<-q2[c(-3)]
ggplot(q2, aes(x=logFlights, y=DepDelay))+
  geom_line(col="steelblue")+
  geom_point()+
  geom_text(hjust=-.12, vjust=-.1,label=q2$YearIssued)

#####################################QUESTION THREE######################################
####How does the number of people flying between different locations change over time?####
#Create another copy of sample
o3 <- o2

#Removing cancelled and diverted flights from the table due to association with NA values
o3 <- subset(o3, Cancelled==0 & Diverted==0)
#Subset again by removing Cancelled and Diverted variables
o3 <- o3[c(-17:-19)]

#Subset through query
q3 <- o3 %>%
  mutate(Date=format(FlightDate, "%Y/%m"))%>%
  group_by(Date)%>%
  summarise(Flights=n())%>%
  arrange(Date)

q3$Date <- as.Date(paste(q3$Date,"/01", sep=""), format="%Y/%m/%d")

#Plot for visualisation of data
#Limit time frame
ggplot(q3, aes(x=Date, y=Flights))+
  geom_line(color="steelblue")+
  geom_point()+
  scale_x_date(date_breaks = "1 month", date_labels="%b")+
  scale_y_continuous(breaks=seq(7500,9000,250),limits=c(7500,9000))+
  ggtitle("Number of flights from 2004-2006")+
  xlab("Months(2004-2005)")

####################QUESTION FOUR#########################
####Can you detect cascading failures as delays in one airport create delays in others?####

#Subset and transform for readability
o4 <- o
o4[5:8] <- o4[5:8] %>%
  lapply(sprintf, fmt="%04d")%>%
  lapply(strptime, format="%H%M")%>%
  lapply(format, format="%H:%M")

#Subset rows with positive LateAircraftDelay values
o4 <- subset(o4, LateAircraftDelay > 0)

#Query and explore data to find connections
mostdelays <- o4%>%
  inner_join(airports, by=c("Origin"="iata"))%>%
  group_by(Origin, airport)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  filter(Flights>=30)%>%
  arrange(desc(SumDelay))


mostdelays2 <- o4%>%
  inner_join(airports, by=c("Origin"="iata"))%>%
  group_by(Origin, lat, long, Dest)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  rename(olat=lat, olong=long)%>%
  filter(SumDelay>=1500)%>%
  arrange(desc(SumDelay))

mostdelays3<- o4%>%
  inner_join(airports, by=c("Dest"="iata"))%>%
  group_by(Origin, Dest, lat, long)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  rename(dlat=lat, dlong=long)%>%
  filter(SumDelay>=1500)%>%
  arrange(desc(SumDelay))

mostdelays3[7:8]<-mostdelays2[2:3]

  
map <- borders("usa", fill='white')

ggplot() + map +
  geom_curve(mostdelays3,
             mapping=aes(x=olong, y=olat, xend=dlong, yend=dlat),
             col="#00008b",
             size=.5,
             curvature=.2)+
  geom_point(mostdelays3,
             mapping=aes(x=olong, y=olat),
             color="blue",
             size=1.5)+
  geom_point(mostdelays3,
             mapping=aes(x=dlong, y=dlat),
             color="red")+
  geom_text(mostdelays3, mapping=aes(x=olong, y=olat, label=Origin), hjust=1.2, vjust=-.7)+
  geom_text(mostdelays3, mapping=aes(x=dlong, y=dlat, label=Dest), hjust=1.2, vjust=-.7)

ORDdelays <- o4%>%
  group_by(Dest, Origin)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  filter(Dest=="ORD")%>%
  arrange(desc(SumDelay))%>%
  top_n(9)

EWRdelays <- o4%>%
  group_by(Dest, Origin)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  filter(Dest=="EWR")%>%
  arrange(desc(SumDelay))%>%
  top_n(9)

MSPdelays <- o4%>%
  group_by(Dest, Origin)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  filter(Dest=="MSP")%>%
  arrange(desc(SumDelay))%>%
  top_n(9)

RDUdelays <- o4%>%
  group_by(Dest, Origin)%>%
  summarise(SumDelay=sum(LateAircraftDelay), Flights=n())%>%
  filter(Dest=="RDU")%>%
  arrange(desc(SumDelay))%>%
  top_n(9)
##############################QUESTION FIVE##################################
####Use the available variables to construct a model that predicts delays####
library(skimr)
library(caret)

o5<-o[c(2:19)]
skim(o5)

#Removing cancelled and diverted flights from the table due to association with NA values
o5 <- subset(o5, Cancelled==0 & Diverted==0)

#Remove variables for DepDelay prediction
dep_o5<-o5[c(1, 3, 4, 5, 7, 12, 15)]
skim(dep_o5)

#Remove variables for ArrDelay prediction
arr_o5<-o5[c(1, 3, 4, 5, 11)]
skim(arr_o5)

#Making a simple multilinear regression model
dep_lr_model <- train(DepDelay~.,
               data = dep_o5,
               method = "lm")

arr_lr_model <- train(ArrDelay~.,
               data = arr_o5,
               method = "lm")


dep_lr_model
arr_lr_model
summary(dep_lr_model)
summary(arr_lr_model)

predict(dep_lr_model, head(dep_o5))
predict(arr_lr_model, head(arr_o5))

#Adding another variable and making new model
dep_o5<-o5[c(2, 4, 5, 7, 12)]
skim(dep_o5)

dep_lr_model <- train(DepDelay~.,
                      data = dep_o5,
                      method = "lm")
dep_lr_model
summary(dep_lr_model)

predict(dep_lr_model, head(dep_o5))

#Splitting data into training and testings sets
set.seed(5)
samp_dep <- createDataPartition(dep_o5$DepDelay, p=0.7, list=F)
train_dep <- dep_o5[samp_dep,]
test_dep <- dep_o5[-samp_dep,]

set.seed(6)
samp_arr <- createDataPartition(arr_o5$ArrDelay, p=0.7, list=F)
train_arr <- arr_o5[samp_arr,]
test_arr <- arr_o5[-samp_arr,]


#Making a ridge regression model
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeat ten times



dep_rr_model<- train(DepDelay~.,
               data = train_dep,
               method = "ridge",
               trControl = fitControl,
               verbose = F)

dep_rr_model

predict(dep_rr_model, head(test_dep))

arr_rr_model <- train(ArrDelay~.,
                data = train_arr,
                method = "ridge",
                trControl = fitControl,
                verbose = F)

arr_rr_model
predict(arr_rr_model,head(test_arr))

