#Reproducible Research Course Project

##Loading and Preprocessing the Data

###Set the working directory

```r
setwd("W:/Coursera/Reproducible Research/week2/activitydata")
```
###Load the data

```r
readData <- read.csv("activity.csv")
```
###Let's summarize our data

```r
head(readData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(readData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
#What is mean total number of steps taken per day?

##1.Calculate the total number of steps taken per day.

```r
sumsteps <- aggregate(steps ~ date,data = readData,sum)
head(sumsteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
##2.Make a histogram of the total number of steps taken each day.

```r
hist(sumsteps$steps,col=8,main = "Histogram of the total number of steps taken each day",
     xlab = "Total number of steps in a day",ylab = "Frequency")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)





##3.Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(sumsteps$steps)  
```

```
## [1] 10766.19
```

```r
median(sumsteps$steps)
```

```
## [1] 10765
```
#What is the average daily activity pattern?

##1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
averageSteps <- aggregate(steps ~ interval,data = readData,mean)

plot(averageSteps$interval,averageSteps$steps,type = "l",main="Average number of steps taken",
     xlab = "Interval",ylab = "steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)




##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepInterval <- averageSteps[which.max(averageSteps$steps),]
maxStepInterval
```

```
##     interval    steps
## 104      835 206.1698
```
#Imputing missing values

##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's.

```r
sum(is.na(readData))
```

```
## [1] 2304
```
##2.Devise a strategy for filling in all of the missing values in the dataset.
###I am using the mean strategy to fill the missing values.

```r
imputeData <- readData
imputeData$steps[which(is.na(readData$steps))] <- mean(imputeData$steps,na.rm = TRUE)
```
##3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newDataset <- as.data.frame(imputeData)
```


##4a.Make a histogram of the total number of steps taken each day.
###Here we have selected the data with no missing values.

```r
StepsTotal <- aggregate(steps ~ date, data = newDataset, sum, na.rm = TRUE)

hist(StepsTotal$steps, main = "Total number of steps taken each day", xlab = "day",col = 8)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)



##4b.Calculate and report the mean and median total number of steps taken per day.

```r
oldMean <- mean(sumsteps$steps)
oldMean
```

```
## [1] 10766.19
```

```r
oldMedian <- median(sumsteps$steps)
oldMedian
```

```
## [1] 10765
```
###New Mean and Median

```r
newMean <- mean(StepsTotal$steps)
newMean
```

```
## [1] 10766.19
```

```r
newMedian <- median(StepsTotal$steps)
newMedian
```

```
## [1] 10766.19
```
##4c.Do these values differ from the estimates from the first part of the assignment? 
###Yes, the mean is the same but the median is increased by 1.19 steps.


#5.Are there differences in activity patterns between weekdays and weekends?
##5a.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dateData <- as.POSIXct(newDataset$date)
newDataset$weekday <- ifelse(weekdays(dateData) %in% c("Saturday", "Sunday"), 
                           "weekend", "weekday")
```
##5b.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
intervalData <- aggregate(steps ~ interval + weekday,data = newDataset,mean)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```
## Stackoverflow is a great place to get help:
## http://stackoverflow.com/tags/ggplot2.
```

```r
ggplot(intervalData, aes(x=interval, y=steps)) + geom_line()+
  facet_grid(weekday ~.) + xlab("Interval") + ylab("Steps") +
  ggtitle("Average Number of Steps in Each Interval")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)



