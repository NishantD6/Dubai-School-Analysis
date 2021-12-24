#Readr Package
Year_Established <- read_csv("/Users/Home/Desktop/Big_Data_Analytics/R/KHDA_Case_Study_timeseriers/KHDA_Training_Examples/Merging_Cleaning_Datasets/Private Schools_Database_2016.csv")
main <- read_csv("/Users/Home/Desktop/Big_Data_Analytics/R/KHDA_Case_Study_timeseriers/KHDA_Training_Examples/Merging_Cleaning_Datasets/Private Schools_Database_main.csv")

glimpse(Year_Established)
glimpse(main)

school<-inner_join(Year_Established, main, by="School Name") #TidyR Package
glimpse(school)

write.csv(school, file= "School_Dataset.csv")

school <- tbl_df(school)

glimpse(school) #Dyplr
summary(school)

school <- school%>%
  select(School='School Name',
         Slaybus,
         Rating= 'Rating 2016/17', 
         Enrollment='2016/17 Current Capacity', 
         Location, Year_Established='Year Established in Dubai', 
         Type='Type of School', 
         FS1, KG1, KG2, 
         Grade1='Grade 1',
         Grade2='Grade 2',
         Grade3='Grade 3',
         Grade4='Grade 4',
         Grade5='Grade 5',
         Grade6='Grade 6',
         Grade7='Grade 7',
         Grade8='Grade 8',
         Grade9='Grade 9',Grade10='Grade 10',Grade11='Grade 11',Grade12='Grade 12')%>%
  mutate(RatingS=ifelse(Rating=="Unsatisfactory"|Rating=="Weak", 0,
                        ifelse(Rating=="Acceptable",1,
                               ifelse(Rating=="Good",2, 
                                      ifelse(Rating=="Very Good"|Rating=="Very good",3, 
                                             ifelse(Rating=="Outstanding", 4, "No Rating Available"))))))


school$Slaybus <- factor(school$Slaybus)
school$Location <- as.factor(school$Location)
school$Rating<-factor(school$Rating, levels=c("Outstanding",
                                              "Acceptable",
                                              "Very Good",
                                              "Good", 
                                              "Unsatisfactory", 
                                              "Weak"))
school$Type <- as.factor(school$Type)

levels(school$Rating)
summary(school$Rating)

levels(school$Slaybus)
summary(school$Slaybus)

# Use recode from dplyr
school$Slaybus <- recode(school$Slaybus,
                         "Canadian" = "Other Western",
                         "French"= "Other Western",
                         "French/ IB" = "Other Western",
                         "German"= "Other Western",
                         "Indian(CBSE)"="Indian",
                         "Indian/IB"="Indian",
                         "Indian/French"="Indian",
                         "Iranian"="Other Asian",
                         "Japanese"= "Other Asian",
                         "MOE/US"="MOE",
                         "Pakistani"="Other Asian",
                         "Phillipini"= "Other Asian",
                         "Phillippine"="Other Asian",
                         "Phillipine"="Other Asian",
                         "Russian"="Other Western",
                         "SABIS (UK/US)"="US",
                         "UK/IB"="IB")

levels(school$Location)

school$Location <- recode(school$Location,
                          "Al Nahda 2"= "Al Nahda",
                          "Al Nahda 2"= "Al Nahda",
                          "Al Nahda 1"= "Al Nahda",
                          "Al Barsha 1"= "Al Barsha",
                          "Al Barsha 2"= "Al Barsha",
                          "Al Khawaneej 1"= "Al Khawaneej",
                          "Al Mizhar 1"= "Al Mizhar",
                          "Al Muhaisnah 1"= "Al Muhaisnah",
                          "Al Muhaisnah 4"= "Al Muhaisnah",
                          "Al Quoz 1"= "Al Quoz",
                          "Al Quoz 4"= "Al Quoz",
                          "Al Safa 1"= "Al Safa",
                          "Al Sufouh 1"="Al Sufouh",
                          "Al Twar 1"= "Al Twar",
                          "Al Twar 2"="Al Twar",
                          "Jumeirah 1"= "Jumeirah",
                          "Jumeirah 3"= "Jumeirah",
                          "Nad Al Sheba 1"= "Nad Al Sheba",
                          "Nad Al Sheba 3"= "Nad Al Sheba",
                          "Umm Suqueim 1"= "Umm Suqueim",
                          "Umm Suqueim 3"= "Umm Suqueim")

levels(school$Location)

glimpse(school)

#Avearge Fees
school$Fees <- rowMeans(school[ ,8:22], na.rm=T)
summary(school$Fees)
school <- school%>%
  filter(!is.na(Fees))

boxplot(school$Fees, horizontal = T)

#Understanding the NA's

summary(school$Rating)
#Depends on the Data! In KHDA DATA, KHDA rating is available only after 3 years of opeartions.
# Hence many new schools that opened in the last 3 years do not have any ratings!
# We cannot omit these schools because they give us latest trends of fees being charged by new schools. 

 
