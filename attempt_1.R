getwd()
data <- read.csv("Activity_Table_CD2_131_19June2022.csv",nrows=100000, header = TRUE)
View(data)
df <- read.csv("Activity_Table_CD2_131_19June2022.csv", header = TRUE)
View(df)
par(mfrow=c(3,3))
hist(data$Percentage_Completion, col = 'orange', freq = TRUE, labels=TRUE,xlim = c(0,100))
hist(data$First_Attempt_Percentage_Correct, col = 'pink', freq = TRUE, labels=TRUE,xlim = c(0,100))
hist(data$Percentage_Correct, col = 'gray', freq = TRUE, labels=TRUE,xlim = c(0,100))
hist(data$X.items_completed, col = 'blue', freq = TRUE, labels=TRUE)
hist(data$X.items_started, col = 'yellow', freq = TRUE, labels=TRUE)
#hiatus
hist(data$Last_attemptHiatus, col = 'green', freq = TRUE, labels=TRUE)
#crash rate
hist(data$CRASHED, col = 'purple', freq = TRUE, labels=TRUE)

