## ----For detailed comments on code snippets , please refer to reading material----------------------------------------------------------


## setwd(" Here/Goes/Path/To/Your/Data/Folder/")
## windows users will need to replace "\" with "/" or "\\" 

getwd()

setwd("/Users/lalitsachan/Desktop/March onwards/CBAP with R/Data/")

getwd()

#xlsx : java, openxlsx : c.... read.xlsx 

## ------------------------------------------------------------------------
# read.csv is same read.table just that default set value of option "sep" is ","

bd=read.csv("bank-full.csv")


head(bd,2)



## ------------------------------------------------------------------------
bd=read.csv("bank-full.csv",sep=";")

head(bd,2)



## -----------------------------------------------------------
library(dplyr)
glimpse(bd)

#sql : sqldf | xls : xlsx , openxlsx | sas : sas7bdat

## ------------------------------------------------------------------------
bd=read.csv("bank-full.csv",sep=";",stringsAsFactors = F)


glimpse(bd)

## ------------------------------------------------------------------------

table(bd$job)

## ------------------------------------------------------------------------
bd=read.csv("bank-full.csv",sep=";",stringsAsFactors = FALSE,na.strings = "unknown")

sum(is.na(bd$job))

### openxlsx : read.xlsx : use this to import data from excel files 
#### sas7bdat : read.sas7bdat : use this to import data from sas7bdat files



## ------------------------------------------------------------------------
for(i in 1:ncol(mtcars)){
  
  print(mean(mtcars[,i]))

  }

## -----------------------------------------------------------

lapply( mtcars , mean )

mydata=data.frame(v1=c(1,2,3,4,NA,5,6,7),v2=sample(1:10,8))

lapply(mydata, mean, na.rm=T )


## --------------------------------------------------------------
## # Before running these codes , you'll have to set your working directory to the folder "namesbystate".
## # You will find this folder inside "Data" folder which you downloaded from LMS
getwd()
setwd("/Users/lalitsachan/Desktop/March onwards/CBAP with R/Data/namesbystate/")



file_names=list.files(getwd(),pattern="*.TXT")

files=lapply( file_names , read.csv , header=F, stringsAsFactors = F)

file=do.call(rbind,files)


names(file)=c("state","gender","year","name","count")
## ----

x=sample(1:100,20)

lapply(x,log10)

sapply(mtcars, mean)

## ------------------------------------------------------------------------

apply(mtcars,1,mean)



## ----
temps=data.frame(days=1:30,T1=sample(20:30,30,replace = T),T2=sample(20:30,30,replace = T),T3=sample(20:30,30,replace = T))
head(temps)

## ------------------------------------------------------------------------
k=apply(temps[,-1],1,max)

temps$max_temp = k

head(temps)

## ------------------------------------------------------------------------
outlier_upper=function(x){
  m=mean(x);
  s=sd(x);
  return(m+3*s);
}

apply(mtcars,2,outlier_upper)



## ------------------------------------------------------------------------
other=function(x){
  upper.lim=outlier_upper(x)
  num_out=sum(x>upper.lim)
  return(num_out)
}

apply(mtcars,2,function(x) sum(x>outlier_upper(x)))


## ------------------------------------------------------------------------


tapply( mtcars$mpg , mtcars$am, mean)



tapply( mtcars$mpg , mtcars$cyl, outlier_upper)


## ------------------------------------------------------------------------




apply(mtcars[,-9],2,function(x) tapply(x,mtcars$am,mean))



tapply(mtcars$mpg,mtcars[,c("vs","am")],mean)






## ----
library(vcd)

##adding data to an existing dataframe

row1=c(NA,NA,NA,mean(Arthritis$Age),"Maybe")
a=rbind(Arthritis,row1)

data1=data.frame(ID=c(12,13),Treatment=c("New","New1"),
                 Sex=c("Male","F"),Age=c(-10,-100),
                 Improved=c("Maybe","Yes"))
a=rbind(Arthritis,data1)
###


Arthritis$log_age=log(Arthritis$Age)

newcol=paste0(Arthritis$Improved,Arthritis$ID)

Arthritis=cbind(Arthritis,newcol)


head(Arthritis)



## ------------------------------------------------------------------------
Arthritis$age_40_indicator=as.numeric(Arthritis$Age<40)


head(Arthritis)

## ------------------------------------------------------------------------
x=sample(40,10)

x


y=ifelse(x>20,20,x)

y

## ------------------------------------------------------------------------
Arthritis$cond_var=ifelse(Arthritis$Treatment=="Placebo" & 
                            Arthritis$Improved=="None",100,Arthritis$Age**2)



head(Arthritis)


## ----
library(dplyr)
library(hflights)

data(hflights)
head(hflights)

hflights
## ------------------------------------------------------------------------
flights=tbl_df(hflights)
flights

## ------------------------------------------------------------------------
d1=flights[flights$Month==1 & flights$DayofMonth==1, ]

## ------------------------------------------------------------------------
#note: you can use comma or ampersand to represent AND condition

d1=filter(flights, Month==1, DayofMonth==1)

# use pipe for OR condition

filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA" )



# you can also use %in% operator
filter(flights, UniqueCarrier %in% c("AA", "UA"))

## ------------------------------------------------------------------------
# base R approach to select DepTime, ArrTime, and FlightNum columns

flights[, c("DepTime", "ArrTime", "FlightNum")]



# dplyr approach

select(flights, DepTime, ArrTime, FlightNum)

select(flights, -DepTime, -ArrTime, -FlightNum)


## ------------------------------------------------------------------------
select(flights, Year:DepTime, contains("Taxi"), contains("Delay"))



## ------------------------------------------------------------------------
# nesting method to select UniqueCarrier and DepDelay columns 
# and filter for delays over 60 minutes





filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

## ------------------------------------------------------------------------
x=sample(10,6)

sum(log(x))

x %>% 
  log() %>%
  sum()

## ------------------------------------------------------------------------
# chaining method
flights %>% 
  select(UniqueCarrier, DepDelay) %>% 
  filter(DepDelay > 60)
 


## ------------------------------------------------------------------------
# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay

flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]





# dplyr approach
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(UniqueCarrier,desc(DepDelay))

# how to sort in descending order using function arrange : using -ve sign in front of
# the variable namess




## ------------------------------------------------------------------------
# base R approach to create a new variable Speed (in mph)

flights$Speed = flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]



# dplyr approach 
d2=flights %>%
  select(Distance, AirTime,DepDelay) %>%
  mutate(Speed = Distance/AirTime,
         Speed=Speed*60,
         Speed=round(Speed,2),
         faraway=ifelse(Distance>300,1,0),
         Airtime_flag60=as.numeric(AirTime==60),
         DepDelay=ifelse(DepDelay>100,100,DepDelay)
         )





## ------------------------------------------------------------------------
flight_sub=flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

flight_sub=flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = ifelse(AirTime>40,Distance/AirTime,200))


## ------------------------------------------------------------------------
# dplyr approach: create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay,na.rm=T))







## ------------------------------------------------------------------------
# for each day of the year, count the total number of flights and sort in 
# descending order
# hint : use the function n()


flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(-flight_count)

# n() gives count of observations

# rewrite more simply with the 'tally' function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort=TRUE) 

# for each destination, count the total number of flights and 
# the number of distinct planes 
# that flew there


flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(),
            plane_count = n_distinct(TailNum))




# for each destination, show the number of cancelled and not cancelled flights

select(flights,Dest,Cancelled)

flights %>%
  group_by(Dest) %>%
  summarize(c1=sum(Cancelled),total=n(),c2=total-c1) %>%
  select(-total)


flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()


# for each month, calculate the number of flights and the 
# change from the previous month

flights %>%
  group_by(Year,Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))





# rewrite more simply with the `tally` function
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))


# base R approach to view the structure of an object
str(flights)

# dplyr approach: better formatting, and adapts to your screen width
glimpse(flights)


View(mtcars)

mtcars1=mtcars[1:15,]
mtcars2=mtcars[16:32,]
mtcars3=mtcars[c(1,3,13,17),]

s=sample(1:32,10)
mtcars4=mtcars[s,]
mtcars5=mtcars[-s,]


## ------------------------------------------------------------------------
set.seed(1)
k=nrow(mtcars)
s=sample(1:k,0.7*k)


# we are using set.seed for our random sample to be reproducible

## ------------------------------------------------------------------------
mtcars_sample=mtcars[s,]

## ------------------------------------------------------------------------
mtcars_remaining=mtcars[-s,]

## ------------------------------------------------------------------------
set.seed(1)
s=sample(1:nrow(mtcars),100,replace = TRUE)

mtcars_bootstrapped=mtcars[s,]

# what happens if group_by is not used
library(dplyr)
mtcars %>%
  summarize(mean_mpg=mean(mpg))

###
z=flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table()
z=as.data.frame(z)

z %>%
  select(-Dest)

## tidyr and lubridate
## ----

messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50),
  c=c(10,20,30),
  d=c(0,1,2),
  e=c(-3,5,6)
)
messy



## ----

library(tidyr)

messy %>% 
  gather(drug,heartrate,a:e)



## ----
set.seed(10)
messy <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)

messy

## ------------------------------------------------------------------------
tidier = messy %>%
  gather(key,time,work.T1:home.T2)
tidier

## ------------------------------------------------------------------------
tidier=tidier %>% 
  separate(key,into=c("location","shift"),sep="\\.")

tidier

## ------------------------------------------------------------------------
step1= tidier %>%
  unite(lalit,location,shift,sep=".")

step1
step2=step1 %>%
  spread(lalit,time)
step2

## ----
library(lubridate)

ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")

class(dmy("04/06/2011"))


#dplyr, ggplot2, tidyr , ggvis, lubridate : Hadley Wickham

### POSIXct 

## ------------------------------------------------------------------------


arrive = ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")

leave = ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")







## please find out documentation for list of time zones , what all options i can give to tz

## ------------------------------------------------------------------------
second(arrive)

second(arrive) = 25
arrive

second(arrive) = 0
arrive

wday(arrive)

wday(arrive, label = TRUE)

quarter(arrive)

day(arrive)

month(arrive)

## ------------------------------------------------------------------------
meeting = ymd_hms("2011-07-01 09:00:00", tz = "Pacific/Auckland")

## ------------------------------------------------------------------------
z=with_tz(meeting, "America/Chicago")

## ------------------------------------------------------------------------
leap_year(2011)

ymd(20110101) + dyears(1)

ymd(20110101) + years(1)

## ------------------------------------------------------------------------
 
leap_year(2012)

ymd(20110101) + dyears(12) #adding exactly 365 days 

ymd(20110101) + years(12) # this explicitly add one year

ymd(20120101) + months(3)

## ------------------------------------------------------------------------
# d= two digit date
# y = two digit year
# b = abbreviated month name
# Y= 4 digit year
# B = complete month name
# p = for  am pm
# m = month in numbers
# %H:%M = time is in 24 hrs format
# %I:%M = time is in 12 hr format , this needs to be accompanied by p

parse_date_time("01-12-Jan","%d-%y-%b")

z=parse_date_time("2012-01-January 10:05 PM","%Y-%d-%B %I:%M %p")

# extracting date time in specfic format from POSIXt object 

format(z,"%Y/%m/%d")



#Function can be used seamlessely for vectors as well

x = c("09-01-01", "09-01-02", "09-01-03")
parse_date_time(x, "ymd")
parse_date_time(x, "%y%m%d")
parse_date_time(x, "%y %m %d")


## ** heterogenuous formats **




x = c("09-01-01", "090102", "09-01 03", "09-01-03 12:02")
parse_date_time(x, c("%y%m%d", "%y%m%d %H%M"))

# for excel : xlsx , openxlsx
getwd()

d=read.xlsx("exercise.xls",sheetName="Sheet3")
# reading from a sas datafile

library(sas7bdat)
d=read.sas7bdat("gaming1.sas7bdat")






