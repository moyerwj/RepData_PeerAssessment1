---
title: "Week 2 Project"
output:
  html_document:
    keep_md: true
  
---

- William "Josh"" Moyer
- 12/10/2017

Set the working directory for the project:


```r
setwd("~/Josh Data Science/DataScienceWorkingDir/repdata%2Fdata%2Factivity")
```

1.) Read in the data for the project and load packages for analysis


```r
dataset<- read.csv("activity.csv")
head(dataset)
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
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

2.) Histogram of the number of steps taken each day.

Calculate total steps


```r
totalsteps<- aggregate(dataset$steps, by= list(dataset$date), sum)
names(totalsteps)[1]= "date"
names(totalsteps) [2]= "TotalSteps"
head(totalsteps)                       
```

```
##         date TotalSteps
## 1 2012-10-01         NA
## 2 2012-10-02        126
## 3 2012-10-03      11352
## 4 2012-10-04      12116
## 5 2012-10-05      13294
## 6 2012-10-06      15420
```

Create Histogram

```r
histogram <- ggplot(data = na.omit(totalsteps), aes(TotalSteps)) + 
    geom_histogram(binwidth = 1000, colour = "blue") +
    xlab("Total Steps per Day") +
    ylab("Count") +
    ggtitle("#2 Make a histogram of the total number of steps taken each day")
print(histogram)
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3.) Calculate the mean and median number of steps taken each day


```r
mean(na.omit(totalsteps$TotalSteps)) 
```

```
## [1] 10766.19
```

```r
median(na.omit(totalsteps$TotalSteps))
```

```
## [1] 10765
```

4.) Time series plot of the average number of steps taken

Making a time series plot


```r
avgintervals<- aggregate(steps ~ interval, data = dataset, mean)
TSPlot<- ggplot(avgintervals, aes(x= interval, y= steps))+
  geom_line()+
  xlab("Time Intervals (5 Min)")+
  ylab("Avg Steps")
print(TSPlot)
```

![](PA1_Template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

5.) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgintervals[avgintervals$steps==max(avgintervals$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

6.) Code to describe and show a strategy for imputing missing data

Total number of NAs


```r
sum(is.na(dataset$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean interval steps from the mean steps of that interval and create a new data set with the NAs filled in with the avg.  I will add in a check for the count of NAs.  


```r
newdataset <- dataset
NAdata <- is.na(newdataset$steps)
intervalavg <- tapply(dataset$steps, dataset$interval, mean, na.rm=TRUE, simplify=T)
newdataset$steps[NAdata] <- intervalavg[as.character(newdataset$interval[NAdata])]
sum(is.na(newdataset))
```

```
## [1] 0
```

7.) Histogram of the total number of steps taken each day after missing values are imputed

Calculate total steps


```r
totalsteps2<- aggregate(newdataset$steps, by= list(newdataset$date), sum)
names(totalsteps2)[1]= "date"
names(totalsteps2) [2]= "TotalSteps"
head(totalsteps2)                       
```

```
##         date TotalSteps
## 1 2012-10-01   10766.19
## 2 2012-10-02     126.00
## 3 2012-10-03   11352.00
## 4 2012-10-04   12116.00
## 5 2012-10-05   13294.00
## 6 2012-10-06   15420.00
```

Create Histogram

```r
histogram2 <- ggplot(data = totalsteps2, aes(TotalSteps)) + 
    geom_histogram(binwidth = 1000, colour = "blue") +
    xlab("Total Steps per Day") +
    ylab("Count") +
    ggtitle("New histogram of the total number of steps taken each day NAs populated")
print(histogram2)
```

![](PA1_Template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean(totalsteps2$TotalSteps)
```

```
## [1] 10766.19
```

```r
median(totalsteps2$TotalSteps)
```

```
## [1] 10766.19
```

The mean did not change but the median is now reflective of the mean.  So by replacing the NAs with the mean interval of steps we have moved the median now to be the average.  

8.) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

I will begin by changeing the date format of the file so the weekdays function can be applied.  


```r
newdataset$date<- as.Date(as.character(newdataset$date))
newdataset$DayofWeek<- weekdays(newdataset$date)
newdataset$weektype<- ifelse(newdataset$DayofWeek=="Saturday"| newdataset$DayofWeek=="Sunday", "Weekend", "Weekday")
head(newdataset)
```

```
##       steps       date interval DayofWeek weektype
## 1 1.7169811 2012-10-01        0    Monday  Weekday
## 2 0.3396226 2012-10-01        5    Monday  Weekday
## 3 0.1320755 2012-10-01       10    Monday  Weekday
## 4 0.1509434 2012-10-01       15    Monday  Weekday
## 5 0.0754717 2012-10-01       20    Monday  Weekday
## 6 2.0943396 2012-10-01       25    Monday  Weekday
```

Make a Time series plot for both the weekday and weekend data simultaneously.


```r
avgWeekdayWeekend<- aggregate(newdataset$steps, by = list(newdataset$weektype,newdataset$interval), mean)
names(avgWeekdayWeekend)[1]= "weektype"
names(avgWeekdayWeekend)[2]= "interval"
names(avgWeekdayWeekend)[3]= "steps"
TSPlot2<- ggplot(avgWeekdayWeekend, aes(x= interval, y= steps))+
  geom_line()+
  facet_grid(weektype~.)+
  xlab("Time Intervals (5 Min)")+
  ylab("Avg Steps")
print(TSPlot2)
```

![](PA1_Template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



