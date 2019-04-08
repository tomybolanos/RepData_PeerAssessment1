## libraries 

#install.packages("plyr")
#install.packages('ggplot2', dep = TRUE)

library(knitr)
library("plyr")
library("ggplot2")


opts_chunk$set(echo = TRUE)


#load data

data <- read.csv("activity.csv")

tail(data)
head(data)


## change date format

data$date = as.Date(x = data$date,format = "%Y-%m-%d")

#check data 

tail(data)
head(data)
str(data)

 # 1 - Calculate the total number of steps taken per day


total_Steps<-aggregate(steps~date,data=data,sum,na.rm=TRUE)
hist(total_Steps$steps,main = "Steps per day", xlab = "Steps", col = "green", breaks = 8)

# Calculate and report the mean and median of the total number of steps taken per day

meansteps <- mean(total_Steps$steps)
print(sprintf("Mean total steps taken per day: %f", meansteps))

mediansteps <- median(total_Steps$steps)
print(sprintf("Median total steps taken per day: %f", mediansteps))


# What is the average daily activity pattern?
# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



avgactivity <- aggregate(steps~interval,data,mean,na.rm = TRUE)
plot(x = avgactivity$interval,y = avgactivity$steps,type = "l",xlab = "5 Min Interval", ylab = "Avg Number of Steps",main = "Avg Daily Activity Planner")




maxSteps = max(avgactivity$steps)
max.avgactivity <- avgactivity$interval[avgactivity$steps == maxSteps]
print(sprintf("5-min time Interval with maximum average steps taken per day: %i",max.avgactivity))


# Imputing missing values

#find the NAs




na_st <- is.na(data$steps)


avg_int <- tapply(data$steps, data$interval, mean, na.rm=TRUE, simplify=TRUE)


data$steps[na_st] <- avg_int[as.character(data$interval[na_st])]


# Calculate the average steps in the 5-minute interval and use ggplot for making the time series of the 5-minute interval for weekday and weekend, and compare the average steps:

# Add the Weekday/weekend identifier

data$week <- ifelse(weekdays(data$date) == "Saturday" | weekdays(data$date) == "Sunday" ,"weekend","weekday")

#df of the mean and median number of steps taken, averaged across all days (y-axis)
int_steps2 <- aggregate(data$steps, by = list(data$week, data$interval), mean, na.rm=TRUE)
int_stepsmed2 <- aggregate(data$steps, by = list(data$week, data$interval), median, na.rm=TRUE)

int_steps2 <- cbind(int_steps2[], int_stepsmed2$x)

#Tidy the df names and round the numbers
names(int_steps2) = c("weekday", "interval","mean.steps", "median.steps")
int_steps2$mean.steps <- round(int_steps2$mean.steps)
int_steps2$median.steps <- round(int_steps2$median.steps)

ggplot(int_steps2, aes(x = interval, y = mean.steps)) + ylab("Number of Steps") + geom_line() + facet_grid(weekday~.)


