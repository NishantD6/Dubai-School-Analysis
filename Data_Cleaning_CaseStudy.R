#Seeing the Data
mtcars
class(mtcars$mpg)
names(mtcars)
glimpse(mtcars)
summary(mtcars)
data(mtcars)

##Tidy Data (Reshape2 of TidyR) (ggplot for economics)
str(economics)

#TidyR
em1 <- gather(economics, variable123, numbervalue123, 2:6)
glimpse(em1)

em1%>%
  ggplot(aes(date,numbervalue123, col=variable123))+
  geom_line()

head(em1)
head(economics)

reversed<- spread(em1, variable123, numbervalue123)
glimpse(reversed)

#Reshape2
em <- melt(economics, id='date')
glimpse(em)


#Detect & Replace - using stringr
Names <- c("James" ,"John", "Jake")
str_detect(Names, "James")
str_replace(Names, "James", "David")

#Cleaning Data within Columns using stringr

str_trim("    asdajsndka       ")
str_pad("1912312", width=10, side="left", pad="N")



#Type Conversion
str(mtcars1)

mtcars1<- mtcars
mtcars1$cyl <- as.factor(mtcars1$cyl)
mtcars1$gear <- as.factor(mtcars1$gear)
str(mtcars1)

#Fixing Dates using lubridate
data <- data.frame(initialDiagnose = c("14.01.2009", 
                                       "9/22/2005", 
                                       "4/21/2010", 
                                       "28.01.2010", 
                                       "09.01.2009", 
                                       "3/28/2005", 
                                       "04.01.2005", 
                                       "04.01.2005", 
                                       "Created on 9/17/2010", 
                                       "03 01 2010",
                                       "March 03 2010",
                                       "6 September 2014",
                                       "9th Oct 2012",
                                       "2015 April 8th"))

data

mdy <- mdy(data$initialDiagnose) #Take all dates which are in mdy format and name it mdy.
dmy <- dmy(data$initialDiagnose) #Take all dates which are in dmy format and name it dmy.
ymd <- ymd(data$initialDiagnose) #Take all dates which are in ymd format and name it ymd.
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # convert all those that is dmy into mdy
mdy[is.na(mdy)] <- ymd[is.na(mdy)] # convert all those that is ymd into mdy
data$initialDiagnose <- mdy        # permanently change the column based on mdy column
data


#Renaming factors example
glimpse(hflights)
summary(as.factor(hflights$CancellationCode))

# Hard Encoding - takes time!
renameVector <- c("A"="CarrierFault", "B"="Weather", "C"="FFA","D"="Security")
hflights$CancellationCodeFull <- renameVector[hflights$CancellationCode]
levels(as.factor(hflights$CancellationCodeFull))
summary(as.factor(hflights$CancellationCodeFull))
summary(as.factor(hflights$CancellationCode))

# Using recode from Dplyr
x = factor(c('a','a','a','b','b','b','c','c','c'))
recode(x, a = "Apple", .default = levels(x))

# Using replace from Plyr
x = factor(c('a','a','a','b','b','b','c','c','c'))
revalue(x, c("a" = "Apple"))


#Creating the Right Factor Order
data(mtcars)
mtcars$gear <- factor(mtcars$gear, levels=c("5","3","4"))
summary(factor(mtcars$gear))

#Box Plot
set.seed(10)
x<-c(rnorm(30, mean=15, sd=15), -5, 29,35, 100, -30)
boxplot(x,horizontal=F)


#Case Study with tidyr & lubridate- Draw a timeseries graph of the number of flights over the year
str(hflights)

str(hflights_date)

hflights_date<- unite(hflights, date1, "Year", "Month", "DayofMonth")
hflights_date$date1<- ymd(hflights_date$date1)
str(hflights_date)
hflights_date%>%
  group_by(date1)%>%
  summarise(DailyNumFlights=n())%>%
  ggplot(aes(date1, DailyNumFlights))+
  geom_point()+
  geom_smooth(stat = 'identity')

#Case Study with Renaming Carrier names in Hflights and converting to factors. 

data(hflights) #Fresh copy of Hflights

glimpse(hflights)
hflights$UniqueCarrier <- as.factor(hflights$UniqueCarrier)
summary(as.factor(hflights$UniqueCarrier))

hflights$UniqueCarrier[hflights$UniqueCarrier=="AA"] <- "American Airlines"

AirlineCodes <- c("AA" = "American", 
                  "AS" = "Alaska", 
                  "B6" = "JetBlue", 
                  "CO" = "Continental",   
                  "DL" = "Delta", 
                  "OO" = "SkyWest", 
                  "UA" = "United", 
                  "US" = "US_Airways",
                  "WN" = "Southwest", 
                  "EV" = "Atlantic_Southeast",
                  "F9" = "Frontier",
                  "FL" = "AirTran",
                  "MQ" = "American_Eagle",
                  "XE" = "ExpressJet",
                  "YV" = "Mesa")

# Hard Encoding - Takes a lot of time! You can use recode from Dplyr or revalue from Plyr
hflights$Carrier <- AirlineCodes[hflights$UniqueCarrier]
levels(factor(hflights$Carrier))
str(hflights)
hflights$Carrier <- as.factor(hflights$Carrier)


# Case study - using TidyR (gather and separate)
glimpse(iris)
new_iris<-gather(iris,type, dimension,1:4)
glimpse(new_iris)
new_iris1 <- separate(new_iris, type, c("SoP", "Dim"))
glimpse(new_iris1)

new_iris1%>%
  ggplot(aes(Dim, dimension, col=SoP))+
  geom_jitter()+
  facet_wrap(~Species)

# Case study - Tidy Data with School Dataset
str(school)
school_long <- gather(school, key="Grade", value="Fees", "FS1":"Grade12")
glimpse(school_long)

# Making factors into ordered factors
school_long$Grade <- factor(school_long$Grade, levels= c('FS1', 'KG1', 'KG2', 'Grade1','Grade2','Grade3','Grade4','Grade5','Grade6','Grade7','Grade8','Grade9','Grade10','Grade11','Grade12'))

write.csv(school_long, file="showtidyr.csv")


# stringR refer to this later - once KHDA case study is completed
school1
summary(str_detect(school1$Slaybus, "MOE"))
str_replace(school1$Slaybus, "MOE", "UAE")

#Save a Data Frame Version onto Excel
library(openxlsx)
write.xlsx(DataFrameName, 'name-of-your-file.xlsx')


