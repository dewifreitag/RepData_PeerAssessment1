# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

1. Load the data


```r
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

The 5-minute interval was presented as a combination of hour and time (eg, 0 means 00:00 or the 1st 5 minutes of the 1st hour, 1115 means 11:15 or the 4th 5 minutes of the 12th hour). The interval is therefore changed into the number of minutes elapsed since the time 0.


```r
minute <- activity$interval %% 100
hour <- (activity$interval-minute)/100
activity$time <- hour*60 + minute
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
steps.per.day <- tapply(activity$steps, activity$date, sum, na.rm = FALSE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(steps.per.day, breaks = 25, col = "orange", 
     main = "Histogram of the number of steps taken per day",
     xlab = "Number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median for the total number of steps taken per day

The mean is

```r
mean(steps.per.day, na.rm = TRUE)
```

```
## [1] 10766.19
```
and the median is

```r
median(steps.per.day, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity.summary <- summarise(group_by(activity,time), 
                              steps=mean(steps, na.rm=TRUE))
library(ggplot2)
ggplot(data=activity.summary, aes(x=time, y=steps)) + 
    geom_line() + 
    labs(x="Time since midnight (in minutes)") + 
    labs(y="Average number of steps taken") + 
    labs(title="Average number of steps taken in 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute interval with the maximum number of steps is the interval

```r
steps.per.5minutes <- tapply(activity$steps, activity$time, mean, na.rm = TRUE)
max.steps <- max(steps.per.5minutes)
max.interval <- unique(activity$interval[steps.per.5minutes==max.steps])
max.interval
```

```
## [1] 835
```

that is, the one that starts at

```r
(max.interval-max.interval %% 100)/100
```

```
## [1] 8
```
o'clock and

```r
max.interval %% 100
```

```
## [1] 35
```
minutes.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset

There are

```r
sum(is.na(activity))
```

```
## [1] 2304
```
cases with missing values.

2. Devise a strategy for filling in all of the missing values in the dataset

A missing value from a 5-minute interval was replaced by the mean of the same 5-minute interval (averaged over all the days in the dataset).


```r
new.steps <- rep(0, times=nrow(activity))
mean.steps <- rep(steps.per.5minutes, times=61)
for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i]) == TRUE) {
        new.steps[i] <- mean.steps[i]
    }
    else {
        new.steps[i] <- activity$steps[i]
    }
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
new.activity <- data.frame(steps=new.steps, date=as.Date(activity$date), 
                      interval=activity$interval)
head(new.activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. 

- Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 


```r
new.steps.per.day <- tapply(new.activity$steps, new.activity$date, sum, 
                            na.rm=FALSE)
hist(new.steps.per.day, breaks = 25, col = "orange", 
     main = "Histogram of the number of steps taken per day",
     xlab = "Number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

The new mean is

```r
mean(new.steps.per.day, na.rm=TRUE)
```

```
## [1] 10766.19
```
and the new median is

```r
median(new.steps.per.day, na.rm=TRUE)
```

```
## [1] 10766.19
```

- Do these values differ from the estimates from the first part of the assignment?

The mean values are exactly the same.

```r
t.test(steps.per.day, new.steps.per.day)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  steps.per.day and new.steps.per.day
## t = 0, df = 107.145, p-value = 1
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1539.144  1539.144
## sample estimates:
## mean of x mean of y 
##  10766.19  10766.19
```


- What is the impact of imputing missing data on the estimates of the total daily number of steps?

This way of imputing missing data does not have an impact on the estimates of the total daily number of steps (p-value = 1). 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day <- rep("", times=nrow(new.activity))
for (i in 1:nrow(new.activity)) {
    weekday <- weekdays(new.activity$date[i])
    if (weekday == "Saturday"| weekday == "Sunday") {
        day[i] = "weekend"
    }
    else {
        day[i] = "weekday"
    }
}
new.activity <- cbind(new.activity, time=activity$time, day=day)
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays days or weekend days (y-axis).


```r
new.activity.summary <- summarise(group_by(new.activity,day,time), 
                                  steps=mean(steps))
library(ggplot2)
ggplot(data=new.activity.summary, aes(x=time, y=steps)) + 
    geom_line() + facet_grid(. ~ day) + 
    labs(x="Time since midnight (in minutes)") + 
    labs(y="Average number of steps taken") + 
    labs(title="Average number of steps taken in 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png) 
