install.packages("plotly")
install.packages("dplyr")
install.packages("tidyR")
install.packages("ggplot2")
install.packages("ggvis")

#Read the data table and rename it to schools
Private_Schools_Database_4 <- read_csv("/Users/Home/Desktop/Big_Data/R/School_KHDA/Private Schools_Database_4.csv")
schools <- tbl_df(Private_Schools_Database_4)
glimpse(schools)

glimpse(hflights)
#Reshape table into tidy format
x<- schools%>%
  gather(Year, Rating, "2008":"2016")%>%
  gather(Year_E,Enrollments, "2008_9_Enrolments" :"2016_17_Enrolments")%>%
  separate(Year_E, c("Year_E", "xm", "E"),sep="_")%>%
  filter(Year_E==Year)%>%
  gather(Year_Grade, Fees,"Fees_FS1_2016":"Fees_Grade13_2008")%>%
  separate(Year_Grade, c("Typ","Grade", "Year_Fees"), sep="_")%>%
  filter(Year_Fees==Year)%>%
  mutate(RatingS=ifelse(Rating=="Unsatisfactory"|Rating=="Weak", 0,ifelse(Rating=="Acceptable",1,ifelse(Rating=="Good",2, ifelse(Rating=="Very Good"|Rating=="Very good", 3, ifelse(Rating=="Outstanding", 4, -1))))))%>%
  select(-Year_E,-Year_Fees, -xm, -E, -Typ, -Grades_2015_16)

#Save a DataFrame Version onto Excel

library(openxlsx)
write.xlsx(x, 'ncheck.xlsx')



#Fix the data types for each column
x$Enrollments<-as.numeric(x$Enrollments)
x$Year<-as.numeric(x$Year)
x$Location_Grouped<-as.factor(x$Location_Grouped)
x$Rating<-factor(x$Rating, levels=c("Outstanding","Very Good","Good", "Acceptable", "Unsatisfactory", "Weak" ))
x$Fees<-as.numeric(x$Fees)
x$Grade<-factor(x$Grade, levels=c("FS1", "KG1","KG2", "Grade1", "Grade2",  "Grade3",  "Grade4"  ,"Grade5", "Grade6","Grade7","Grade8" ,"Grade9","Grade10", "Grade11" ,"Grade12", "Grade13"))

glimpse(x)
levels(x$Grade)


#Enrollment by Syllabus
E_S<- x%>%
  select(Enrollments, Year, Syllabus)%>%
  group_by(Year,Syllabus)%>%
  na.omit%>%
  summarize(Mean_Enrollment=mean(Enrollments))%>%
  #filter(Syllabus=="IB")%>%
  ggplot(aes(x=Year, y=Mean_Enrollment, col=Syllabus))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  labs(caption="(based on data from KHDA")
  ggplotly(E_S,tooltip=c("Syllabus", "Mean_Enrollment"), 
         selectize = TRUE, persistent = TRUE)

E_S<- x%>%
  select(Enrollments, Year, Syllabus_Grouped)%>%
  group_by(Year,Syllabus_Grouped)%>%
  na.omit%>%
  #filter(Syllabus=="IB")%>%
  ggplot(aes(x=Year, y=Mean_Enrollment, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)
E_by_SG<- ggplotly(E_S,tooltip=c("Syllabus_Grouped", "Mean_Enrollment"), 
         selectize = TRUE, persistent = TRUE)%>%
  layout(margin=list(l = 100), yaxis=list(tickprefix=" "))


#Sys.setenv("plotly_username"="NishantDas")
#Sys.setenv("plotly_api_key"="QIFieIwjwHnKTfIpcyrD")

#chart_link = api_create(E_by_SG, filename = "Enrollment by Sllaybus Grouped_1")
#chart_link


#Enrollment by Location
glimpse(x)
test <- x%>%
  select(Enrollments, Year, Location_Grouped)%>%
  group_by(Year,Location_Grouped)%>%
  na.omit%>%
  summarize(Mean_Enrollment=mean(Enrollments))%>%
  #filter(Syllabus=="IB")%>%
  ggplot(aes(x=Year, y=Mean_Enrollment, col=Location_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)
ggplotly(test,tooltip=c("Location_Grouped"), 
         selectize = TRUE, persistent = TRUE)

#Enrollment by Location for major enrollment diff
glimpse(x)
test <- x%>%
  select(Enrollments, Year, Location)%>%
  group_by(Year,Location)%>%
  na.omit%>%
  summarize(Mean_Enrollment=mean(Enrollments))%>%
  #filter(Syllabus=="IB")%>%
  ggplot(aes(x=Year, y=Mean_Enrollment, col=Location))+
  geom_point()+
  geom_smooth(method="lm", se=F)
ggplotly(test,tooltip=c("Location"), 
         selectize = TRUE, persistent = TRUE)

#Rating by Location for 2016
glimpse(x)
test1<- x%>%
  filter(Year=="2015")%>%
  select(School, Location_Grouped, Syllabus_Grouped, Rating)%>%
  group_by(Syllabus_Grouped, Location_Grouped, Rating)%>%
  distinct(School, keep_all=F)%>%
  filter(Rating!="NA")%>%
  ggplot(aes(x=Rating, fill=factor(Syllabus_Grouped)))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~Location_Grouped)
ggplotly(test1)




#Avg. Fees across GRades 
glimpse(x)
x%>%
  select(School, Syllabus_Grouped, Grade, Fees, Rating)%>%
  na.omit()%>%
  group_by(Grade)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=as.numeric(Grade), y=avg_Fees))+
  geom_point()+
  geom_smooth(se=F)


#Avg. Fees across GRades by Syllabus for 2016
glimpse(x)
test3<-x%>%
  filter(Year=="2016")%>%
  select(School, Syllabus_Grouped, Grade, Fees, Rating)%>%
  na.omit()%>%
  group_by(Grade, Syllabus_Grouped)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=as.numeric(Grade), y=avg_Fees, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(se=F)
ggplotly(test3)

#Does Syllabus mean poor rating
glimpse(x)
test4<-x%>%
  filter(Year=="2016")%>%
  select(School, Syllabus_Grouped, Grade, Fees, RatingS)%>%
  na.omit()%>%
  group_by(School, Syllabus_Grouped, RatingS)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=jitter(RatingS), y=avg_Fees, col=Syllabus_Grouped))+
  geom_point()+
  #geom_line()+
  #geom_smooth( se=F)+
  scale_x_continuous(name= "KHDA Ratings", breaks = c(0,1,2,3,4), labels=c("Weak", "Acceptable", "Good", "Very Good", "Outstanding"))
ggplotly(test4, tooltip=c("avg_Fees", "Syllabus_Grouped"), 
         selectize = TRUE, persistent = TRUE)


#YearEstablished, Avg.Fees & Enrollment 3D Model for 2016
glimpse(x)
test1<- x%>%
  filter(Year=="2016")%>%
  select(School, Year_Established, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Year_Established, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))

p<-plot_ly(test1, x=~Year_Established, y=~Enrollments, z=~avg_Fees, color=~Syllabus_Grouped)
p

#Avg.Fees vs. Year Established
test1<- x%>%
  filter(Year=="2016")%>%
  select(School, Year_Established, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Year_Established, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=Year_Established, y=avg_Fees, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)

#Avg. Fees vs. Enrollment of Students
test1<- x%>%
  filter(Year=="2016")%>%
  select(School, Year_Established, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Year_Established, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=Enrollments, y=avg_Fees, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)

#Enrollment by year Established
test1<- x%>%
  filter(Year=="2016")%>%
  select(School, Year_Established, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Year_Established, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=Year_Established, y=Enrollments, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)

#Rating by Enrollment 
test1<- x%>%
  filter(Year=="2016")%>%
  select(School, Rating, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Rating, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=jitter(as.numeric(Rating)), y=Enrollments, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)

#Number of Students Vs. Avergae School Fees by Rating & Syllabus
x1<-x%>%
  filter(Year=="2016")%>%
  select(School, Rating, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Rating, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggvis(~avg_Fees, ~Enrollments,fill=~Syllabus_Grouped ,  size=~Rating,  opacity:=0.7)%>%
  layer_points()%>%
  scale_nominal("size", range= c("5", "10", "20", "70", "130", "160"), c("Unsatisfactory", "Weak", "Acceptable", "Good", "Very Good","Outstanding"))%>%
 # add_legend(c("shape"), title = "Type", properties = legend_props(legend = list(y = 100)))%>%
  add_legend(c("size"), title = "Rating", properties = legend_props(legend = list(y = 150)))%>%
  add_axis("y", title = 'Number of Students Enrolled in 2016-2017', properties=axis_props(title=list(dy=-20)))%>%
  add_axis("x", title = 'Average Fees the School (AED)', properties=axis_props(title=list(dy=+10)))

test6<-x%>%
  filter(Year=="2016")%>%
  select(School, Rating, Syllabus_Grouped, Enrollments, Fees)%>%
  na.omit()%>%
  group_by(School, Rating, Syllabus_Grouped, Enrollments)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=avg_Fees, y=Enrollments, col=Syllabus_Grouped, shape=Rating, Rating=Rating, School=School ))+
  geom_point()
ggplotly(test6, tooltip=c("School", "Rating"), 
         selectize = TRUE, persistent = TRUE)


#Enrollment by year Established
test1<- x%>%
  #%filter(Year=="2016")
  select(Syllabus_Grouped, Fees, Year, Grade)%>%
  na.omit()%>%
  group_by(Grade, Year, Syllabus_Grouped)%>%
  summarise(avg_Fees=mean(Fees))%>%
  ggplot(aes(x=Grade, y=avg_Fees, col=Syllabus_Grouped))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(~Grade)

#Number of students over the years
glimpse(x)
ns<-x%>%
  select(School, Year, Enrollments, Grade)%>%
  na.omit()%>%
  group_by(School,Year)%>%
  summarise(Real=mean(Enrollments))

ns%>%
  select(Year, Real)%>%
  group_by(Year)%>%
  summarise(Total_Number_Students=sum(Real))%>%
  ggplot(aes(x=Year, y=Total_Number_Students))+
  geom_point()+
  geom_smooth()

  