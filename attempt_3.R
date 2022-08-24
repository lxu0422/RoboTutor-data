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
ggplot(data_new,aes(x=jitter(start_timestamp),y=activity_duration))+geom_point()
# Calculate the proportion of the number of 0s
sum(data_new$activity_duration == 0)*100 / length(data_new$activity_duration)
# 
hist(data_new[data_new$activity_duration != 0,]$activity_duration, xlab = "activity_duration", main = "Distribution of activity_duration")


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
#Time divided by the completion rate, which represents the time each child completed in full
x <- data_new$activity_duration / data_new$Percentage_Completion
mean(x[!is.na(x) & !is.infinite(x)])

#5
#% activities completed
df5 <- data_new[data_new$Area_AN != "DNFT", ]
df5 %>%
  group_by(Area_AN) %>%
  summarise(
    completed_count = sum(Percentage_Completion >= 100, na.rm=TRUE), 
    completed_percentage = completed_count*100 / nrow(df5)) %>%
  knitr::kable()

#6
df5$complet <- ifelse(df5$Percentage_Completion >= 1, TRUE, FALSE)
df5 %>%
  group_by(complet) %>%
  summarise(
    average_duration = mean(activity_duration, na.rm = T)) %>%
  knitr::kable()

#7
data_new$same <- ifelse(gsub(":","_",data_new$ActivityName) == gsub(" ","",data_new$TutorID), 1, 0)
View(data_new)
hist(data_new$same, freq = TRUE, labels=TRUE,ylim = c(0,2000))

#8
ggplot(data_new,aes(x=completion)) + geom_bar()

data_new %>% 
  count(completion) %>% 
  mutate(perc = n / nrow(data_new)) -> df8

ggplot(df8, aes(x = completion, y = perc)) + geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%.2f", perc*100),"%")),position = position_dodge(width = .9),vjust = -0.5,size = 5)+
  scale_y_continuous(labels = scales::percent)
  

#9
#Boxplot: activity duration on Area_AN
boxplot(data_new$activity_duration~data_new$Area_AN)
#Pie plot
df<-data_new %>% group_by(Area_AN) %>% summarise(count = n())
df$percent <- paste(round(100*df$count/sum(df$count), 2), "%")
pie(df$count, df$percent, main = "Pie plot of Area_AN", col=rainbow(length(df$percent)))
legend("topright", c("DNFT", "literacy", "math", "story"), cex = 1,
       fill = rainbow(length(df$percent)))
# density plot
ggplot(data_new, aes(x = Percentage_Completion))+ geom_density(color = 'black', fill = 'gray')

