##
## Peer Assessmen 1 
## Working Code
##

setwd("~/Dave/Coursera/DataScientistsToolKit/RepData_PeerAssessment1")

## Loading and preprocessing the data

activity_data <- read.csv (unz("activity.zip","activity.csv"))

activity_data$date <- as.Date(as.character(activity_data$date), "%Y-%m-%d")

#1. What is mean total number of steps taken per day

library(plyr)
steps_summary <- ddply (activity_data, .(date), summarize, totalsteps=sum(steps), mediansteps=median(steps),meansteps=mean(steps))

library(ggplot2)
qplot (date, totalsteps, data=steps_summary, geom="histogram", stat="identity", main="Total Steps per Day", ylab="TOTAL Steps")

library(xtable)

!!!! mean and median of total steps per day

mean(steps_summary$totalsteps, na.rm=TRUE)
median(steps_summary$totalsteps, na.rm=TRUE)

The mean total number of steps per day is 'r mean_steps' and the median total number of steps per day is 'r median_steps'

## 2. What is the average daily activity pattern?

daily_activity <- ddply ( activity_data, .(interval), summarize, meansteps = mean(steps,na.rm=TRUE))

plot(daily_activity$interval,daily_activity$meansteps, xlab="Five Minutes Interval", ylab="Average Steps",type="l")

daily_activity[which.max(daily_activity$meansteps),]


