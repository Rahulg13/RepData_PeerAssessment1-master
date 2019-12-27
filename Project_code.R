unzip("activity.zip")
# Reading the data 
d <- read.csv("activity.csv")

# processing data
d$date <- as.Date(d$date)

# Q1. Mean number of total steps taken per day
datesum <- tapply(d$steps, d$date, sum)

hist(datesum, breaks = 8, xlab = "steps count", main = "Steps taken per day")

stepmean <- mean(datesum, na.rm = TRUE)
stepmedian <- median(datesum, na.rm = TRUE)

# Q2. Average daily activity pattern 
#Time series plot for step count in intervals
intervalcount <- tapply(d$steps, d$interval, mean, na.rm = TRUE)
intervals <- levels(as.factor(d$interval))
q2 <- cbind(intervalcount, intervals)
q2 <- as.data.frame(q2)
q2$intervals <- as.numeric(q2$intervals)
q2$intervalcount <- as.numeric(as.character(q2$intervalcount))
library(ggplot2)
g <- ggplot(q2, aes(y = intervalcount, x = intervals)) + geom_point()
g <- g + geom_smooth()
g <- g + ggtitle("Step Count  vs  Time interval")
g

#Finding maximum
maxindex <- which.max(q2$intervalcount)
maxinterval <- q2$intervals[maxindex]

# Q3. Handling NAs
naindex <- which(is.na(d$steps))
d2 <- d
d2$steps[naindex] <- intervalcount[naindex]

datesum2 <- tapply(d2$steps, d2$date, sum)

hist(datesum2, breaks = 8, xlab = "steps count", main = "Steps taken per day")
stepmean2 <- mean(datesum, na.rm = TRUE)
stepmedian2 <- median(datesum, na.rm = TRUE)

# Q4.Differences in weekdays and weekends 

library(lubridate)
library(dplyr)
d <- mutate(d, weekend = ((wday(date) == 1) | (wday(date) == 7)))
d$weekend <- as.factor(d$weekend)
levels(d$weekend) <- c("Weekday", "Weekend")
divided <- with(d, tapply(steps, list(interval, weekend), mean, na.rm =  TRUE))
divided <- as.data.frame(divided)
l <- length(divided[,1])
divided2 <- divided
divided <- unlist(divided)
daytype <- c(rep("Weekday", l), rep("Weekend", l))
intervaltype <- c(intervals, intervals)
divided <- cbind(divided, daytype, intervaltype)
divided <- as.data.frame(divided)
names(divided) <- c("steps", "day_type", "interval")
divided$interval <- as.numeric(as.factor(divided$interval))
divided$steps <- as.numeric(as.character(divided$steps))

g2 <- ggplot(divided, aes(x = interval, y = steps))
g2 <- g2 + geom_line(col = "blue") + facet_grid(day_type ~ .)
g2