---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(readr)

movRaw <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

```r
library(ggplot2)
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
totalSteps <- aggregate(steps~date, data = movRaw, FUN = sum, na.rm = TRUE)

totalSteps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

### 2. Calculate the total number of steps taken per day

```r
histSteps <- hist(totalSteps$steps, main = "Steps per day", xlab = "Steps")
abline(v = mean(totalSteps$steps), col = "red")
abline(v = median(totalSteps$steps), col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanSteps <- movRaw %>% group_by(interval) %>% summarise(mean = mean(steps, na.rm = TRUE))

ggplot(meanSteps, aes(interval, mean)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanSteps[grep(max(meanSteps$mean), meanSteps$mean),]
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```
## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
sum(is.na(movRaw))
```

```
## [1] 2304
```
### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will fill in the missing data using the mean steps per 5-minute interval.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
movFill <- movRaw
i <- 1
while(i <= length(movFill$steps))
{
  if(!is.na(movFill$steps[i]))
  {
    i <- i + 1
    next()
  }
  movFill$steps[i] <- meanSteps$mean[which(movFill$interval[i] == meanSteps$interval)]
  i <- i + 1
}
movFill
```

```
## # A tibble: 17,568 x 3
##     steps date       interval
##     <dbl> <date>        <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # ... with 17,558 more rows
```
### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
 fillSum <-  movFill %>% group_by(date) %>% summarise(total = sum(steps))

hist(fillSum$total, xlab = "Total steps taken per day", main = "")
abline(v = mean(fillSum$total), col = "red")
abline(v = median(fillSum$total), col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(fillSum$total)
```

```
## [1] 10766.19
```

```r
median(fillSum$total)
```

```
## [1] 10766.19
```

There would appear to be no impact made from imputing the missing data, at least when using the method described above. The mean remained exactly the same and the median increased by only 1.19.

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
day <- weekdays(movFill$date)
day[which(day == "Saturday" | day == "Sunday")] <- "weekend"
day[which(day != "weekend")] <- "weekday"
day <- factor(day)
movFill <- mutate(movFill, day = day)
```

### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
meanFill <- movFill %>% group_by(interval, day) %>% summarise(mean = mean(steps, na.rm = TRUE))

ggplot(meanFill, aes(interval, mean)) + geom_line() + facet_grid(day~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
