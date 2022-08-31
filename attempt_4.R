library('knitr')
library('dplyr')
library('ggplot2')
install.packages("ggpmisc")
library('ggpmisc')
data_1 <- read.csv("Activity_Table_CD2_131_19June2022.csv", header = TRUE)

View(data_1)

ggplot(data_1,aes(x=Upload_date,y=activity_duration))+geom_point()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  geom_smooth()

#% activities completed
data_1 %>%
  group_by(Area_AN) %>%
  summarise(
    completed_count = sum(Percentage_Completion >= 100, na.rm=TRUE), 
    completed_percentage = completed_count / nrow(data_1)) %>%
  knitr::kable()

#percentage completion of different areas
ggplot(data_new,aes(x=Area_AN,y=Percentage_Completion))+ ylim(0,100)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  geom_point()

#session duration
ggplot(data_new,aes(x=Area_AN,y=activity_duration))+geom_point()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")


#progress through Area_AN
ggplot(data_new, aes(x = Percentage_Completion, y = Percentage_Correct, colour = Area_AN))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()
ggplot(data_new, aes(x = Percentage_Completion, y = First_Attempt_Percentage_Correct, colour = Area_AN))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()




#1 winsorize the data, filter out 3 sd values above
# Calculate 3 quantiles
Q3 <- quantile(data_1$activity_duration,0.75)
# Calculate 1 quantile
Q1 <- quantile(data_1$activity_duration,0.25)
# Calculate the outlier upper bound
max_ <- Q3 + 3 * (Q3 - Q1)
# remove outliers
data_new <- data_1[data_1$activity_duration < max_,]

#2
#start_timestamp and activity_duration
ggplot(data_new,aes(x=jitter(start_timestamp),y=activity_duration,col = TabletID))+geom_point()+xlim(9.465e+11,9.480e+11)
  ylab("activity_duration(ms)")+xlab("start_ms")
  
ggplot(data_new,aes(x=jitter(start_timestamp),y=activity_duration,col = TabletID))+geom_point()+xlim(1.5425e+12,1.5440e+12)
  ylab("activity_duration(ms)")+xlab("start_ms")
# Calculate the proportion of the number of 0s
sum(data_new$activity_duration == 0)*100 / length(data_new$activity_duration)
# Distribution of the rest
hist(data_new[data_new$activity_duration != 0,]$activity_duration, xlab = "activity_duration", main = "Distribution of activity_duration")
# histogram in log scale
hist(log(data_new[data_new$activity_duration != 0,]$activity_duration), xlab = "activity_duration", main = "Distribution of activity_duration in log_scale")


#3
#Select recurring data
data_new$color <- duplicated(paste(data_new$Area_AN, data_new$Percentage_Completion))
df3 <- data_new%>%group_by(Area_AN,Percentage_Completion) %>% summarise(count = n())
ggplot(df3,aes(x=Area_AN,y=Percentage_Completion, size = count))+
  geom_point() +
  ylim(0,100)

ggplot(data_new,aes(x=Area_AN,y=Percentage_Completion, col = color, size = Percentage_Completion))+
  geom_point() +
  ylim(0,100)

hist(data_new$Percentage_Completion, xlab = "Percentage_Completion", main="Histgorm of Percentage_Completion")

df3<-data_new
sum(df3$Percentage_Completion>=0 & df3$Percentage_Completion<=100)*100/length(df3$Percentage_Completion)
sum(df3$Percentage_Completion>100)*100/length(df3$Percentage_Completion)#bailout rate

#4
# Create “Completion” column = how activity ended; classify each value as completed or not.
data_new$Completion_2 <- ifelse(data_new$completion == "BACKBUTTON"|data_new$CRASHED == 1, FALSE, TRUE)
View(data_new)

## completed activities / total # activities = 48.04%
data_new %>% 
  group_by(Completion_2) %>%
  summarise(
    n=n(),
    percentage = n*100 / nrow(data_new)) %>%
  knitr::kable()

aggregate(data_new$activity_duration, by=list(TabletID=data_new$TabletID),sum)
aggregate(activity_duration~Completion_2+TabletID, data=data_new, FUN=sum)
#5A27001459:spent 93.04% of his time performing activities to completion. 
#5B20001313:spent 66.66% of his time performing activities to completion.
#6116001855:spent 70.33% of his time performing activities to completion.

#5
## 100% completed activities / total # activities
df5 <- data_new[data_new$Area_AN != "DNFT"& data_new$CRASHED==0 & data_new$Completion_2==TRUE, ]
df5 %>% 
  summarise(
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / nrow(df5)) %>%
  knitr::kable()

## 100% completed items in an activity / total # activities
df5 %>%
  group_by(Area_AN) %>%
  summarise(
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / nrow(df5)) %>%
  knitr::kable()

## 100% completed items in an activity / total # items in the activity
df5 <- data_new[data_new$Area_AN != "DNFT"& data_new$CRASHED==0 & data_new$Completion_2==TRUE, ]
df5 %>%
  count(Area_AN)
## 100% completed items in an activity / total # items in the activity
df5 %>%
  group_by(Area_AN) %>%
  summarise(
    n=n(),
    completed_count = sum(Percentage_Completion == 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / n) %>%
  knitr::kable()

#6
df5$complete <- ifelse(df5$Percentage_Completion >= 100, TRUE, FALSE)
df5 %>%
  group_by(complete) %>%
  summarise(
    average_duration = mean(activity_duration, na.rm = T), 
    median_duration = median(activity_duration, na.rm = T))%>%
  knitr::kable()

df6 <- data_new%>%group_by(activity_duration,X.attempts) %>% summarise(count = n())
ggplot(df5,aes(x=activity_duration,y=X.attempts))+ geom_count() +
  geom_point() +
  ylim(0,100)

#1 attempt 0 activity_duration percentage 
df6 <- data_new[data_new$activity_duration == 0 & data_new$X.attempts == 1,]
df6 %>% 
  summarise(
    count = nrow(df6), 
    percentage = count*100 / nrow(data_new)) %>%
  knitr::kable()

#7
data_new$same <- ifelse(gsub(":","_",data_new$ActivityName) == gsub(" ","",data_new$TutorID), TRUE, FALSE)
View(data_new)
#hist(data_new$same, freq = TRUE, labels=TRUE,ylim = c(0,2000))
data_new %>% 
  group_by(same) %>%
  summarise(
    n=n(),
    percentage = n*100 / nrow(data_new)) %>%
  knitr::kable()

#8 % backbutton rate/bailout rate
data_new %>% 
  count(completion) %>% 
  mutate(perc = n / nrow(data_new)) -> df8
View(df8)
View(data_new)

ggplot(df8, aes(x = completion, y = perc)) + geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%.2f", perc*100),"%")),position = position_dodge(width = .9),vjust = -0.5,size = 5)+
  scale_y_continuous(labels = scales::percent)

ggplot(data_new, aes(x = completion, fill = Area_AN)) + geom_bar() 

#9
#Boxplot: activity duration on Area_AN
boxplot(df5$activity_duration~df5$Area_AN, xlab = "Area_AN", ylab = "activity_duration", main = "Pie plot of completed activity_duration and Area_AN")

#pie plot
#Pie plot with legend
df<-df5 %>% group_by(Area_AN) %>% summarise(count = n())
df$percent <- paste(round(100*df$count/sum(df$count), 2), "%")
pie(df$count, df$percent, main = "Pie plot of completed activities grouped by Area_AN", col=rainbow(length(df$percent)))
legend("topright", c("literacy", "math", "story"), cex = 1,
       fill = rainbow(length(df$percent)))
#pie plot with labels
View(df)
labels <- c("literacy", "math", "story")
pie(df$count, labels, main = "Pie plot of completed activities grouped by Area_AN", col=rainbow(length(df$count)))

#density plot
# density plot of all percent_Completion
ggplot(data_new, aes(x = Percentage_Completion))+ geom_density(color = 'black', fill = 'gray') + 
  ggtitle("density plot of all percent_Completion")
# density plot of the completed percent_completion
ggplot(df5, aes(x = Percentage_Completion))+ geom_density(color = 'black', fill = 'gray')+
  ggtitle("density plot of the completed percent_completion")

