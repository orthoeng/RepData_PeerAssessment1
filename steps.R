#########################################################################################

rm(list=ls(all=TRUE))
library(dplyr)
library(lattice)
library(knitr)

##########################################################################################
## Loading and preprocessing the data
## 1. Load csv data 

            steps_data <- read.csv("activity.csv", as.is = TRUE, header = TRUE)

##########################################################################################
## 2. Process/transform the data (if necessary) into a format suitable for your analysis
## Remove NA from dataset

            remove_na <- steps_data %>% filter(!is.na(steps))

##########################################################################################
## 3. Calculate the total number of steps taken per day 
## group by days - total = sum steps by the group days

            total_day_steps <- tapply(remove_na$steps, remove_na$date, sum)

##########################################################################################
## 4. Make a histogram of the total number of steps taken each day

            hist(total_day_steps, 
                          breaks="FD",
                          main="Total Number of Steps Taken Each Day",
                          xlab="Number of Total Steps", 
                          ylab="Frequency", 
                          col=4)

##########################################################################################
## 5. Calculate and report the mean and median of the total number of steps taken per day

            mean_total_steps <- mean(total_day_steps) ## Mean of the total number of steps taken per day
            median_total_steps <- median(total_day_steps) ## Median of the total number of steps taken per day

##########################################################################################
## 6. What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of 
## steps taken, averaged across all days (y-axis)

            average_steps <- tapply(remove_na$steps, remove_na$interval, mean)
            plot(average_steps, 
                          type = "l", 
                          main="Time Series Plot", 
                          ylab = "Average Number of Steps", 
                          xlab = "5-minute Interval")

##########################################################################################
## 7. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
   
            maximum_num_steps <- average_steps[average_steps == max(average_steps)]
            index_steps <- which.max(average_steps)

##########################################################################################
## 8. Imputing missing values
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

            filter_na <- steps_data %>% filter(is.na(steps))
            NA_count <- sum(is.na(filter_na))  

##########################################################################################       
## 9. Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. Could use mean for that 5-minute interval

            mean_steps <- mean(average_steps) ## mean of the 5-minute interval
            steps_data$steps[is.na(steps_data$steps)] <- mean_steps ## Replace NA with mean

##########################################################################################
## 10. Create a new dataset that is equal to the original dataset but with the missing data filled in.

            new_data <- cbind(steps_data$steps, steps_data$date, steps_data$interval)
            colnames(new_data) <- c("new_steps", "new_date", "new_interval")
           
#########################################################################################
## 11. Make a histogram of the total number of steps taken each day and Calculate and report the mean 
## and median total number of steps taken per day. 

            steps_new <- as.numeric(new_data[,1])
            new_date <- new_data[,2]
            new_average_steps <- tapply(steps_new, new_date, sum)
          
            hist(new_average_steps, 
                              main="Total Number of Steps Taken Each Day",
                              xlab="Number of Total Steps", 
                              breaks="FD",
                              ylab="Frequency", 
                              col=2)

##########################################################################################
## 12. Mean abd Median
        
            mean_new_total_steps <- mean(new_average_steps) ## Mean of the total number of steps taken per day
            median_new_total_steps <- median(new_average_steps) ## Median of the total number of steps taken per day

###########################################################################################
## 13. Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels – “weekday” and 
## “weekend” indicating whether a given date is a weekday or weekend day.

            new_data2 <- data.frame(cbind(steps_data$steps, steps_data$date, steps_data$interval))
            colnames(new_data2) <- c("steps", "date", "interval")

            date_weekdays <- as.POSIXct(new_data2$date)
            new_data2$day <- weekdays(date_weekdays, abbreviate=FALSE) 

##########################################################################################

            cutdata  <- new_data2 %>% filter(day == "Sunday" )  
            cutdata2  <- new_data2 %>% filter(day == "Saturday") 
            weekend <- rbind(cutdata, cutdata2)
            weekend$day <- c("Weekend") 

#########################################################################################

            cutdata3  <- new_data2 %>% filter(day == "Monday") 
            cutdata4  <- new_data2 %>% filter(day == "Tuesday") 
            cutdata5  <- new_data2 %>% filter(day == "Wednesday") 
            cutdata6  <- new_data2 %>% filter(day == "Thursday") 
            cutdata7  <- new_data2 %>% filter(day == "Friday") 
            weekdays <- rbind(cutdata3, cutdata4, cutdata5, cutdata6, cutdata7)
            weekdays$day <- c("Weekday") 

            weekdays_date <- rbind(weekdays, weekend)

############################################################################################
## 14. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
## interval (x-axis) and the average number of steps taken, averaged across all weekday 
## days or weekend days (y-axis). 

            weekdays_date$day <- factor(weekdays_date$day, levels = c("Weekday", "Weekend"))
            stepsdays <- aggregate(steps ~ interval + weekdays_date$day, data = steps_data, mean)
            names(stepsdays) <- c("interval", "days", "steps")

            xyplot(steps ~ interval | days, stepsdays, type = "l", layout = c(1, 2), 
                       xlab = "Interval", ylab = "Number of steps")

############################################################################################

            setwd("/Users/WestYani/Documents/Classes/John\ Hopkins/Data\ Science\ /Reproducible\ Research")
            knit2html("PA1_template.rmd", "PA1_template.html")
            opts_knit$set(base.dir = 'figure')
           



