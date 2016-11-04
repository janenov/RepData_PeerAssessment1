# Peer Assessment 1 for Reproducible Research



## Loading and preprocessing the data


```r
library(data.table)
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.3.2
```

```r
library(ggplot2)

activitydt <- fread("activity.csv")
```


## What is mean total number of steps taken per day?


```r
actmean <- mean(activitydt$steps, na.rm = "TRUE")
totalperday <- aggregate(steps~date, activitydt, sum)
mean(totalperday$steps)
```

```
## [1] 10766.19
```

```r
median(totalperday$steps)
```

```
## [1] 10765
```

```r
qplot(totalperday$steps, geom = "histogram", bins = 60)
```

![](PA1_template_files/figure-html/MeanTotalNumberOfSteps-1.png)<!-- -->



## What is the average daily activity pattern?


```r
totalperinterval <- aggregate(steps~interval, activitydt, mean)

plot(totalperinterval$interval, totalperinterval$steps, type = "l", xlab = "5-minute interval", ylab = "average number of steps")
```

![](PA1_template_files/figure-html/AverageDailyActivityPattern-1.png)<!-- -->

```r
subset(totalperinterval, steps == max(steps)) ##interval 835  steps 10927
```

```
##     interval    steps
## 104      835 206.1698
```

```r
summary(activitydt) #NA's   :2304 
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```



## Imputing missing values


```r
# perform the imputation using mean for that 5-minute interval which is already calculated in previous question - totalperinterval
for (i in 1:nrow(activitydt)){
  if (is.na(activitydt$steps[i])){
    interval_val <- activitydt$interval[i]
    row_id <- which(totalperinterval$interval == interval_val)
    steps_val <- totalperinterval$steps[row_id]
    activitydt$steps[i] <- steps_val
  }
}

nonatotalperday <- aggregate(steps~date, activitydt, sum)

qplot(nonatotalperday$steps, geom = "histogram", bins = 60)
```

![](PA1_template_files/figure-html/ImputingMissingValues-1.png)<!-- -->

```r
mean(nonatotalperday$steps)
```

```
## [1] 10766.19
```

```r
median(nonatotalperday$steps)
```

```
## [1] 10766.19
```

```r
## Mean and Median have not changed much at all
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activitydt$date <- as.Date(activitydt$date, "%Y-%m-%d")

activitydt$day <- weekdays(activitydt$date)

activitydt$day_type <- c("weekday")


for (i in 1:nrow(activitydt)){
    if (activitydt$day[i] == "Saturday" || activitydt$day[i] == "Sunday" )
        { activitydt$day_type[i] <- "weekend" } }



qplot(interval, steps, data=activitydt, geom=c("line"), xlab="Interval", 
      ylab="Number of steps") + facet_wrap(~ day_type, ncol=1)
```

![](PA1_template_files/figure-html/DifferencesInActivityPatterns-1.png)<!-- -->

```r
## looks like people exercise more often and more intensely during the week
```




