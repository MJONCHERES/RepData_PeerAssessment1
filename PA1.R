unzip("activity.zip")
activity<-read.csv("activity.csv")
head(activity)

library(ggplot2)
library(dplyr)

dailysteps<-aggregate(steps~date, activity, sum)
hist(dailysteps$steps, xlab="Number of steps per day", 
     ylab="Frequency", main="Total number of steps taken each day")

mean(dailysteps$steps, na.rm=TRUE)
median(dailysteps$steps, na.rm=TRUE)

IntervalSteps<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=IntervalSteps, type="l", xlab="5-minute intervals", ylab=("Average number of steps taken"))

IntervalSteps[which.max(IntervalSteps$steps), ]$interval

Missing <- sum(is.na(activity$steps))
print(Missing)

m<-mean(IntervalSteps$steps)
activitynona<- transform(activity, steps = ifelse(is.na(activity$steps), m, activity$steps))
dailystepsnona<-aggregate(steps~date, activitynona, sum)
hist(dailystepsnona$steps, xlab="Number of steps per day", 
     ylab="Frequency", main="Total number of steps taken each day")
mean(dailystepsnona$steps)
median(dailystepsnona$steps)

activitynona$date <- as.Date(strptime(activitynona$date, format="%Y-%m-%d"))
activitynona1 <- mutate(activitynona, "day_type"= ifelse(weekdays(activitynona$date)=="samedi" | weekdays(activitynona$date)=="dimanche", "Weekend", "Weekday"))
head(activitynona1,20)


IntervalStepsnona<-aggregate(steps~interval+day_type, data=activitynona1, mean)
plot <- ggplot(IntervalStepsnona, aes(x = interval , y = steps, color=day_type)) + facet_wrap(~day_type, ncol = 1, nrow=2) +
  geom_line()+labs(title="Average steps per 5-minutes intervals", x="Intervals", y="Number of steps")
print(plot)