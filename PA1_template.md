# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
---{r}
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)
---

---
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
---


Exploring the basics of this data.
---{r}
names(activityData)

---
## [1] "steps"    "date"     "interval"
---

head(activityData)

---
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
---

pairs(activityData)
---


## What is mean total number of steps taken per day?
---{r}
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
hist(stepsPerDay$steps)
---

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)


The mean and median of the total number of steps taken per day.
--{r}
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay

---
## [1] 10766.19
---

medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay

---
## [1] 10765
---


## What is the average daily activity pattern?
---{r}
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
---

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)


The 5-minute interval accross all the days containing the maximum number of steps.
---{r}
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
---

---
## [1] 835
---


## Imputing missing values
The total number of missing values in the dataset.
---{r}
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissing
---

---
## [1] 2304
---

getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
---

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


The mean and median of the total number of steps taken each day with no missing values.
---{r}
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
---

## Are there differences in activity patterns between weekdays and weekends?
---{r}
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
---

Making a panel plot containing a time series plot.
---{r}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
---


![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

