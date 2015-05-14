---
title: "RepData Reproducible Research / Peer Assessment 1"
author: "wh"
date: "May 13, 2015"
output: html_document
---
# Reproducible Research / Peer Assessment 1


```r
library(dplyr)
library(lattice)
library(knitr)
```

### Loading and preprocessing the data
### 1. Load csv data 

```r
      steps_data <- read.csv("activity.csv", as.is = TRUE, header = TRUE)
      head(steps_data)
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

### 2. Process/transform the data (if necessary) into a format suitable for your analysis
Remove NA from dataset

```r
      remove_na <- steps_data %>% filter(!is.na(steps))
      head(remove_na)
```

```
##   steps       date interval
## 1     0 2012-10-02        0
## 2     0 2012-10-02        5
## 3     0 2012-10-02       10
## 4     0 2012-10-02       15
## 5     0 2012-10-02       20
## 6     0 2012-10-02       25
```

### 3. Calculate the total number of steps taken per day 
group by days - total = sum steps by the group days

```r
       total_day_steps <- tapply(remove_na$steps, remove_na$date, sum)
```

### 4. Make a histogram of the total number of steps taken each day

```r
hist(total_day_steps, 
                          breaks="FD",
                          main="Total Number of Steps Taken Each Day",
                          xlab="Number of Total Steps", 
                          ylab="Frequency", 
                          col=4)
```

![plot of chunk histogramsteps](figure/histogramsteps-1.png) 

### 5. Calculate and report the mean and median of the total number of steps taken per day


```r
            mean(total_day_steps) ## Mean of the total number of steps taken per day
```

```
## [1] 10766.19
```

```r
            median(total_day_steps) ## Median of the total number of steps taken per day
```

```
## [1] 10765
```

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
            average_steps <- tapply(remove_na$steps, remove_na$interval, mean)
            plot(average_steps, 
                          type = "l", 
                          main="Time Series Plot", 
                          ylab = "Average Number of Steps", 
                          xlab = "5-minute Interval")
```

![plot of chunk plot](figure/plot-1.png) 

### 7. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
            average_steps[average_steps == max(average_steps)]
```

```
##      835 
## 206.1698
```

```r
            which.max(average_steps)
```

```
## 835 
## 104
```

## 8. Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
            filter_na <- steps_data %>% filter(is.na(steps))
            NA_count <- sum(is.na(filter_na)) 
```

## 9. Devise a strategy for filling in all of the missing values in the dataset. 
### Strategy is to take the mean average_steps(5-minute interval)

```r
            mean_steps <- mean(average_steps) ## mean of the 5-minute interval
            steps_data$steps[is.na(steps_data$steps)] <- mean_steps ## Replace NA with mean
            head(steps_data)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

### 10. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
            new_data <- cbind(steps_data$steps, steps_data$date, steps_data$interval)
            colnames(new_data) <- c("new_steps", "new_date", "new_interval")
```

### 11. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
            steps_new <- as.numeric(new_data[,1])
            new_date <- new_data[,2]
            new_average_steps <- tapply(steps_new, new_date, sum)
```

### Histogram

```r
            hist(new_average_steps, 
                              main="Total Number of Steps Taken Each Day",
                              xlab="Number of Total Steps", 
                              breaks="FD",
                              ylab="Frequency", 
                              col=2)
```

![plot of chunk histogramplot](figure/histogramplot-1.png) 
 

### 12. Mean abd Median

```r
            mean(new_average_steps) ## Mean of the total number of steps taken per day
```

```
## [1] 10766.19
```

```r
            median(new_average_steps) ## Median of the total number of steps taken per day
```

```
## [1] 10766.19
```

### 13. Are there differences in activity patterns between weekdays and weekends? Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
            new_data2 <- data.frame(cbind(steps_data$steps, steps_data$date, steps_data$interval))
            colnames(new_data2) <- c("steps", "date", "interval")

            date_weekdays <- as.POSIXct(new_data2$date)
            new_data2$day <- weekdays(date_weekdays, abbreviate=FALSE) 

            cutdata  <- new_data2 %>% filter(day == "Sunday" )  
            cutdata2  <- new_data2 %>% filter(day == "Saturday") 
            weekend <- rbind(cutdata, cutdata2)
            weekend$day <- c("Weekend") 

            cutdata3  <- new_data2 %>% filter(day == "Monday") 
            cutdata4  <- new_data2 %>% filter(day == "Tuesday") 
            cutdata5  <- new_data2 %>% filter(day == "Wednesday") 
            cutdata6  <- new_data2 %>% filter(day == "Thursday") 
            cutdata7  <- new_data2 %>% filter(day == "Friday") 
            weekdays <- rbind(cutdata3, cutdata4, cutdata5, cutdata6, cutdata7)
            weekdays$day <- c("Weekday") 

            weekdays_date <- rbind(weekdays, weekend)
            head(weekdays_date)
```

```
##              steps       date interval     day
## 1 37.3825995807128 2012-10-01        0 Weekday
## 2 37.3825995807128 2012-10-01        5 Weekday
## 3 37.3825995807128 2012-10-01       10 Weekday
## 4 37.3825995807128 2012-10-01       15 Weekday
## 5 37.3825995807128 2012-10-01       20 Weekday
## 6 37.3825995807128 2012-10-01       25 Weekday
```

### 14. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
            weekdays_date$day <- factor(weekdays_date$day, levels = c("Weekday", "Weekend"))
            stepsdays <- aggregate(steps ~ interval + weekdays_date$day, data = steps_data, mean)
            names(stepsdays) <- c("interval", "days", "steps")
            head(stepsdays)
```

```
##   interval    days    steps
## 1        0 Weekday 7.837293
## 2        5 Weekday 6.215071
## 3       10 Weekday 5.970627
## 4       15 Weekday 5.992849
## 5       20 Weekday 5.903960
## 6       25 Weekday 8.281738
```
   
### Lattice panel plot

```r
            xyplot(steps ~ interval | days, stepsdays, type = "l", layout = c(1, 2), 
                       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk Lattice panel plot](figure/Lattice panel plot-1.png) 
