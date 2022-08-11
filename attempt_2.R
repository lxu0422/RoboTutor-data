library('knitr')
library('dplyr')
library('ggplot2')
library('ggpmisc')
data_1 <- read.csv("Activity_Table_CD2_131_19June2022.csv", header = TRUE)

View(data_1)
###nrows=1000000
ggplot(data_1,aes(x=Upload_date,y=activity_duration))+geom_point()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  geom_smooth()

#% activities completed
data_1 |>
  group_by(Area_AN) |>
  summarise(
    completed_count = sum(Percentage_Completion >= 100, na.rm=TRUE), 
    completed_percentage = completed_count / nrow(data_1)) |>
  knitr::kable()
data_1 |>
  group_by(MatrixName) |>
  summarise(
    completed_count = sum(Percentage_Completion >= 100, na.rm=TRUE), 
    completed_percentage = completed_count / nrow(data_1)) |>
  knitr::kable()

#percentage completion of different areas
ggplot(data_1,aes(x=Area_AN,y=Percentage_Completion))+ ylim(0,100)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  geom_point()
ggplot(data_1,aes(x=MatrixName,y=Percentage_Completion))+ ylim(0,100)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  geom_point()

#session duration
ggplot(data_1,aes(x=Area_AN,y=activity_duration))+geom_point()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")
ggplot(data_1,aes(x=MatrixName,y=activity_duration))+geom_point()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")


#progress through Area_AN
ggplot(data_1, aes(x = Percentage_Completion, y = Percentage_Correct, colour = Area_AN))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()+geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
ggplot(data_1, aes(x = Percentage_Completion, y = First_Attempt_Percentage_Correct, colour = Area_AN))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()+geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)

#progress through MatrixName
ggplot(data_1, aes(x = Percentage_Completion, y = Percentage_Correct, colour = MatrixName))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()+geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
ggplot(data_1, aes(x = Percentage_Completion, y = First_Attempt_Percentage_Correct, colour = MatrixName))+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)+
  xlim(0,100)+ylim(0,100)+geom_point()+geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)

