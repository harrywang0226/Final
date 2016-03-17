##################################################################
######################## Final Exam ##############################
##################################################################

# Weiyang Wang

# load library
library(nycflights13)
library(dplyr)
library(RSQLite)
library(ggplot2)

# load data and create data frames
db <- src_sqlite("db.sqlite3", create = T)
flights_sqlite <- copy_to(
  db, flights, temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum")
)

airlines_sqlite <- copy_to(
  db, airlines, temporary = FALSE, 
  indexes = list("carrier")
)

airports_sqlite <- copy_to(
  db, airports, temporary = FALSE, 
  indexes = list("faa")
)

planes_sqlite <- copy_to(
  db, planes, temporary = FALSE, 
  indexes = list("tailnum")
)

weather_sqlite <- copy_to(
  db, weather, temporary = FALSE, 
  indexes = list(
    c("year", "month","day","hour"),
    "origin")
)

######################################################################################
df.flight<-tbl(db, sql("SELECT * FROM flights")) %>% 
  collect() %>%
  mutate(canceled=is.na(arr_time))
df.plane<-tbl(db, sql("SELECT * FROM planes")) %>% 
  collect()
df.airport<-tbl(db,sql("SELECT * FROM airports"))%>% 
  collect()
df.airport<-airports %>% mutate(dest = faa)
df.weather<-tbl(db,sql("SELECT * FROM weather")) %>%
  collect()
######################################################################################
df.FP<- tbl(db, sql("SELECT *
                           FROM flights join planes 
                           ON flights.tailnum = planes.tailnum")
                       ) %>% 
  collect()
# panel data flights and airport
df.FA<-inner_join(
  df.flight, df.airport,by="dest")
# panel data flights and plane
df.FP<-inner_join(
  df.flight,df.plane,by="tailnum")
# panel data flights and weather
df.FW<-inner_join(
  df.flight,df.weather)
# panel data flights and departure time
df.FT<-df.flight
df.FT$time_of_day<-NA
df.FT$time_of_day<-df.FT$hour+(df.FT$minute/60)
df.FT$date<-paste(df.FT$year,df.FT$month,df.FT$day)
df.FT$date<-as.Date(df.FT$date,"%Y%m%d")
df.FT$weekdays<-weekdays(df.FT$date)
######################################################################################
# Feature Engineering
df.FW$canceled<-ifelse(df.FW$canceled==FALSE,0,1)
df.FA$canceled<-ifelse(df.FA$canceled==FALSE,0,1)
df.FP$canceled<-ifelse(df.FP$canceled==FALSE,0,1)
df.FT$canceled<-ifelse(df.FT$canceled==FALSE,0,1)

######################################################################################
# Model

# Weather
FWreg<-lm(dep_delay~temp+humid+wind_speed+precip+pressure+visib,df.FW)
summary(FWreg)
FWlgreg<-glm(canceled~temp+humid+wind_speed+precip+pressure+visib,df.FW,family=binomial(link="probit"))
summary(FWlgreg)
# Airport Destination
FAreg<-lm(dep_delay~name+lat+lon+alt,df.FA)
summary(FAreg)
FAlgreg<-glm(canceled~name+lat+lon+alt,df.FA,family=binomial(link="probit"))
summary(FAlgreg)
# Plane characteristics
#unique(df.FP$seats[which(df.FP$model=="different models")])
FPreg<-lm(dep_delay~model,df.FP)
summary(FPreg)
FPlgreg<-glm(canceled~model,df.FP,family=binomial(link="probit"))
summary(FPlgreg)
# Depature time
DFreg<-lm(dep_delay~time_of_day,df.FT)
summary(DFreg)
DFlgreg<-glm(canceled~as.factor(hour),df.FT,family=binomial(link="probit"))
summary(DFlgreg)
MFreg<-lm(dep_delay~as.factor(month),df.FT)
summary(MFreg)
MFlgreg<-glm(canceled~as.factor(month),df.FT,family=binomial(link="probit"))
summary(MFlgreg)
WFreg<-lm(dep_delay~weekdays,df.FT)
summary(WFreg)
WFlgreg<-glm(canceled~weekdays,df.FT,family=binomial(link="probit"))
summary(WFlgreg)

######################################################################################
# plot
p<-ggplot(data=df.FT,aes(x=weekdays,y=canceled))
p+geom_point()
