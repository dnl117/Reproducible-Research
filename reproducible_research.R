library(ggplot2)
library(dplyr)
library(gridExtra)
#read in data
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

#calculate the total steps per day plus mean and median and plot
stepsbyday <- tapply(activity$steps, activity$date, FUN=sum, na.rm = TRUE)
mean_steps <- mean(stepsbyday, na.rm = TRUE)
#mean steps = 9354.23
med_steps <- median(stepsbyday, na.rm = TRUE)
#median steps = 10395
qplot(stepsbyday, geom =  "histogram", binwidth=2500, main = "Histogram of Total Steps per Day", xlab = "Total Steps", ylab = "Frequency", fill = I("blue"), col = I("black"))

#calculate average number of steps at each interval and plot
activity_sub <- aggregate(steps ~ interval, activity, mean)
ggplot(data=activity_sub, aes(x=interval, y=steps), xlab = "Interval", ylab = "Mean Steps") + ggtitle("Time Series Plot of Mean Steps by Interval") + geom_line(color = "black", size = 1)
activity_sub[which.max(activity_sub$steps),c("interval")]
#The interval is 835, average steps = 206.1698

#Find the number of missing values
missingVal <- sum(is.na(activity$steps))
#There are 2304 NA values

#impute the missing values
library(mice)
set.seed(1234)
activity_sub2 <- subset(activity, select = c(steps, interval))
activity_imp <- complete(mice(activity_sub2))
activity_imp$date <- activity$date


stepsbyday_imp <- tapply(activity_imp$steps, activity_imp$date, sum, na.rm = TRUE)
mean_steps_imp <- mean(stepsbyday_imp)
#mean steps = 10659.21
med_steps_imp <- median(stepsbyday_imp)
#median_steps = 10600
qplot(stepsbyday_imp, geom =  "histogram", binwidth=2500, main = "Histogram of Total Steps per Day with Imputation", xlab = "Total Steps", ylab = "Frequency", fill = I("red"), col = I("black"))

activity_imp$date <- as.Date(activity_imp$date, "%Y-%m-%d")
activity_imp$day <- weekdays(activity_imp$date)
activity_imp$weekday <- ifelse(activity_imp$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 1, 0)
imp_wkday <- filter(activity_imp, weekday == 1)
imp_wkend <- filter(activity_imp, weekday == 0)
imp_wkday_steps <- aggregate(steps ~ interval, imp_wkday, mean)
imp_wkend_steps <- aggregate(steps ~ interval, imp_wkend, mean)
p1 <- ggplot(data=imp_wkday_steps, aes(x=interval, y=steps), xlab = "Interval", ylab = "Mean Steps") + ggtitle("Time Series Plot of Mean Steps on Weekdays") + geom_line(color = "red", size = 1)
p2 <- ggplot(data=imp_wkend_steps, aes(x=interval, y=steps), xlab = "Interval", ylab = "Mean Steps") + ggtitle("Time Series Plot of Mean Steps on Weekends") + geom_line(color = "blue", size = 1)
grid.arrange(p1, p2)
