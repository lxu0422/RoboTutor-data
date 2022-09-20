library('knitr')
library('dplyr')
library('ggplot2')
library('ggpmisc')
library('scales')
data_1 <- read.csv("Activity_Table_CD2_131_19June2022.csv", header = TRUE)

View(data_1)

#data cleaning
#1 winsorize the data, filter out 3 sd values above
# Calculate 3 quantiles
Q3 <- quantile(data_1$Last_attemptHiatus,0.75)
# Calculate 1 quantile
Q1 <- quantile(data_1$Last_attemptHiatus,0.25)
# Get the sd
sd(data_1$Last_attemptHiatus)
# Calculate the outlier upper bound
max_ <- Q3 + 3 * sd(data_1$Last_attemptHiatus)
# remove outliers
data_new <- data_1[data_1$Last_attemptHiatus < max_,]
View(data_new)


#
data_new$same <- ifelse(gsub(":","_",data_new$ActivityName) == gsub(" ","",data_new$TutorID), "same", "not same")

#hist(data_new$same, freq = TRUE, labels=TRUE,ylim = c(0,2000))
data_new %>% 
  group_by(same) %>%
  summarise(
    n=n(),
    percentage = n*100 / nrow(data_new)) %>%
  knitr::kable()

# Create “Completion” column = how activity ended; classify each value as completed or not.
data_new$Completion_2 <- ifelse(data_new$completion == "BACKBUTTON"|data_new$CRASHED == 1, "not completed", "completed")
data_new<-filter(data_new, completion!="")
View(data_new)

#
df4<- filter(data_new, Area_AN != "DNFT")
df4<- filter(df4, same!= "not same")

#
df5<- filter(data_new, Area_AN != "DNFT")
df5<- filter(df5, same!= "not same")
df5<-filter(df5, Completion_2!="not completed")
View(df5)


#backbutton
data_1 %>% 
  count(completion) %>% 
  mutate(perc = n / nrow(data_new)) -> df8
View(df8)
View(data_new)

ggplot(df8, aes(x = completion, y = perc)) + geom_bar(stat = "identity")+theme_bw()+
  geom_text(aes(label=paste0(sprintf("%.2f", perc*100),"%")),position = position_dodge(width = .9),vjust = -0.5,size = 5)+
  scale_y_continuous(labels = scales::percent) + ggtitle("Bar chart, <completion, Percentage>")+ylab("Percentage")+geom_bar(stat = "identity", fill = "lightblue", colour = "black")





#progress through Area_AN
#data_d <- filter(data_1, Area_AN != "DNFT")
#ggplot(data_d, aes(x = Percentage_Completion, y = Percentage_Correct, colour = Area_AN))+
#  stat_poly_eq(formula = y ~ x, 
#               aes(label = paste(..eq.label..,
#                                 ..rr.label..,
#                                 sep = "~~~")), 
#               parse = TRUE)+
#  xlim(0,100)+ylim(0,100)+geom_point()+ggtitle("Scatter plot, <Percentage_Completion, Percentage_Correct>")

ggplot(df5, aes(x = First_Attempt_Percentage_Correct, y = Percentage_Correct, colour = Area_AN))+
  theme_bw()+
  xlim(0,100)+ylim(0,100)+geom_point()+ggtitle("Scatter plot, <First_Attempt_Percentage_Correct, Percentage_Correct>")


#ggplot(data_d, aes(x = Percentage_Completion, y = First_Attempt_Percentage_Correct, colour = Area_AN))+
#  stat_poly_eq(formula = y ~ x, 
#               aes(label = paste(..eq.label..,
#                                 ..rr.label..,
#                                 sep = "~~~")), 
#               parse = TRUE)+
#  xlim(0,100)+ylim(0,100)+geom_point()+ggtitle("Scatter plot, <Percentage_Completion, First_Attempt_Percentage_Correct>")





#2
#time that kids do the activities
datetime <- as.POSIXct(df5$DateTime_logName, format = '%m/%d/%Y, %H:%M:%S')
hour <- as.numeric(format(datetime, format = "%H"))
hour_tab <- table(hour)
prop_tab <- prop.table(hour_tab)
res1 <- cbind(Hour = as.numeric(names(hour_tab)), count = hour_tab, percentage = 100*round(prop_tab, 4))
print(res1)

#datetime <- as.POSIXct(data_new$DateTime_logName, format = '%m/%d/%Y, %H:%M:%S')
#hour <- as.numeric(format(datetime, format = "%H"))
#hour_tab <- table(hour)
#prop_tab <- prop.table(hour_tab)
#res1 <- cbind(Hour = as.numeric(names(hour_tab)), count = hour_tab, percentage = round(prop_tab, 4))
#print(res1)


time <- vector(length = length(hour))
for(i in 1:length(time)){
  if(hour[i] < 6){
    time[i] <- "night"
  }else if(hour[i] < 12){
    time[i] <- "morning"
  }else if(hour[i] < 18){
    time[i] <- "afternoon"
  }else{
    time[i] <- 'evening'
  }
}
time_tab <- table(time)
prop_tab <- prop.table(time_tab)
res2 <- data.frame(Time = names(time_tab), count = as.vector(time_tab), percentage = 100*round(as.vector(prop_tab), 4))
res2 <- res2[c(3,1,2,4),]
print(res2)

# Calculate the proportion of the number of 0s
sum(df5$activity_duration == 0)*100 / length(data_new$activity_duration)
# Distribution of the rest
hist(df5[df5$activity_duration != 0,]$activity_duration, xlab = "activity_duration(ms)", main = "Bar chart, <activity_duration, Frequency>")
# histogram in log scale
#hist(log(df5[df5$activity_duration != 0,]$activity_duration,3), xlab = "log_3(activity_duration(ms))", main = "Bar chart, <log_3(activity_duration), Frequency>")
hist(log(df5[df5$activity_duration != 0,]$activity_duration,5), xlab = "log_5(activity_duration(ms))", main = "Bar chart, <log_5(activity_duration), Frequency>")


#3
#Select recurring data
df3 <- df5%>%group_by(Area_AN,Percentage_Completion) %>% summarise(count = n())
View(df3)
ggplot(df3,aes(x=Area_AN,y=Percentage_Completion, size = count))+ ggtitle("Scatter plot, <Area_AN, Percentage_Completion>")+
  geom_point() +
  theme_bw()+
  ylim(0,100)


#df3<-data_new
#sum(df3$Percentage_Completion>=0 & df3$Percentage_Completion<=100)*100/length(df3$Percentage_Completion)
#sum(df3$Percentage_Completion>100)*100/length(df3$Percentage_Completion)

#4
## completed activities / total # activities = 48.04%
df5 %>% 
  group_by(Completion_2) %>%
  summarise(
    n=n(),
    percentage = n*100 / nrow(data_new)) %>%
  knitr::kable()

#ggplot(data_new, aes(x=Completion_2, fill=as.factor(Completion_2))) + 
#  geom_bar() +
#  xlab("Completion_2") +
#  ylab("Count") +
#  theme_bw()+ggtitle("Bar chart,<Completion_2,Count>")+
#  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)


aggregate(df4$activity_duration, by=list(TabletID=df4$TabletID),sum)
#% time
aggregate(activity_duration~Completion_2+TabletID, data=df4, FUN=sum)
#5A27001459:spent 88.10%=4464273/(4464273+603233) of their time performing activities to completion. 
#5B20001313:spent 70.58%=5686950/(5686950+2370191) of their time performing activities to completion.
#6116001855:spent 63.59%=43431604/(43431604+24870136) of their time performing activities to completion.

ggplot(df4, aes(fill=Completion_2, y=activity_duration, x=TabletID)) + 
  geom_bar(position="fill", stat="identity")+ ggtitle("Stacked bar chart, <TabletID, Completion_2>")+
  theme_bw()+
  scale_y_continuous(labels =percent_format())+coord_flip()

#5

#percentage completion and frequency
#hist(df5$Percentage_Completion, xlab = "Percentage_Completion", main="Bar chart, <Percentage_Completion, Frequency>")

## 100% completed activities / total # activities

df5 %>% 
  summarise(
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / nrow(df5)) %>%
  knitr::kable()

## 100% completed activities by area / total # activities
df5 %>%
  group_by(Area_AN) %>%
  summarise(
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / nrow(df5)) %>%
  knitr::kable()

## 100% completed in an activity / total # activities
df5 %>%
  count(Area_AN)
## 100% completed activities by area / total # completed activities by area
df5 %>%
  group_by(Area_AN) %>%
  summarise(
    n=n(),
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / n) %>%
  knitr::kable()


#6
df5$complete <- ifelse(df5$Percentage_Completion == 100, "100% completed", "not 100% completed")
View(df5)
df5 %>%
  group_by(complete) %>%
  summarise(
    average_duration = mean(activity_duration, na.rm = T), 
    median_duration = median(activity_duration, na.rm = T))%>%
  knitr::kable()

ggplot(df5,aes(x=activity_duration,y=X.attempts, col=Area_AN))+ geom_count() +
  geom_point() +
  theme_bw()+
  ylim(0,100)+ggtitle("Scatter plot, <activity_duration, X.attempts>")

#1 attempt 0 activity_duration percentage 
#df6 <- data_new[data_new$activity_duration == 0 & data_new$X.attempts == 1,]
#df6 %>% 
#  summarise(
#    count = nrow(df6), 
#    percentage = count*100 / nrow(data_new)) %>%
#  knitr::kable()




ggplot(data_new, aes(x = completion, fill = Area_AN)) + geom_bar() + 
  ggtitle("Stacked bar chart, <completion, Area_AN>")+
  theme_bw()+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat = 'count',
            position = position_stack(vjust = 0.5),
            size = 4, color = "white")

#9
#Boxplot: activity duration on Area_AN
#boxplot(df5$activity_duration~df5$Area_AN, xlab = "Area_AN", ylab = "activity_duration(ms)", main = "Box plot, <Area_AN, completed activity_duration>")


#percentage completion of different areas
#boxplot(df5$Percentage_Completion~df5$Area_AN, xlab = "Area_AN", ylab = "Percentage_Completion", main = "Box plot, <Area_AN, completed Percentage_Completion>")
#ggplot(data_new,aes(x=Area_AN,y=Percentage_Completion))+ ylim(0,100)+
#  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
#  geom_point()


#pie plot
#Pie plot with legend
df<-df5 %>% group_by(Area_AN) %>% summarise(count = n())
df$percent <- paste(round(100*df$count/sum(df$count), 2), "%")
pie(df$count, df$percent, main = "Pie plot, <completed activities percentage, Area_AN>", col=rainbow(length(df$percent)))
legend("topright", c("literacy", "math", "story"), cex = 1,
       fill = rainbow(length(df$percent)))
#pie plot with labels
View(df)
labels <- c("literacy", "math", "story")
pie(df$count, labels, main = "Pie plot, <completed activities percentage, Area_AN>", col=rainbow(length(df$count)))

#density plot
# density plot of all percent_Completion
ggplot(data_new, aes(x = Percentage_Completion))+ geom_density(color = 'black', fill = 'gray') + 
  ggtitle("density plot of all percent_Completion")
# density plot of the completed percent_completion
ggplot(df5, aes(x = Percentage_Completion))+ geom_density(color = 'black', fill = 'gray')+
  ggtitle("density plot of the completed percent_completion")

