---
title: 'Reproducible Research: Peer Assessment 1'
output:
  word_document: default
  html_document: default
keep_md: yes
---

```{r setup, include=TRUE}
# Load required packages
library(knitr)
library(dplyr)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)
```

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
# set the destination file path
f <- file.path(getwd(), "activity.zip")
# downoad file to given destination
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",f)
# unzip acitivity data file
uzf <- unzip(f)
# load data
raw_data <- read.csv(uzf)
# omit NAs
data <- na.omit(raw_data)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3.Calculate and report the mean and median of the total number of steps taken per day

```{r}
steps_per_day <- tapply(data$steps, data$date, sum, na.rm=TRUE)
## plot historgram
hist(steps_per_day, main="Histogram of total number of steps per day", xlab="Steps Per Day",  border=1)
## Mean of the total steps takes per day?
mean(steps_per_day, na.rm=TRUE)
median(steps_per_day, na.rm=TRUE)
```

**Mean of the total number of steps per day is `r round(mean(steps_per_day, na.rm=TRUE),2)` ,and median is `r median(steps_per_day, na.rm=TRUE)` **

## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
interval_steps <- aggregate(steps~interval, data, mean)
plot(interval_steps$interval, interval_steps$steps,  col=1,main="Avg steps across all days", xlab="Interval", ylab="Average number of steps", type='l')
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` {r}
# find row id of max avg number of steps in the interval
max_row_id = which.max(interval_steps$steps)
## find the interval for the given max avg number of steps in the interval
interval_steps[max_row_id,]
```

**The interval `r interval_steps[max_row_id,][1] ` has the maximum average value of steps `r round(interval_steps[max_row_id,][2],2) ` **

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(raw_data))
```
**Total number of rows with NA's is  `r sum(is.na(raw_data)) ` **

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc

**My strategy is to fill the missing values with the mean for that 5 minute interval**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

## Imputing missing values

```{r}
fill_data <- raw_data
interval_steps <- aggregate(steps~interval, data, mean)
for ( i in 1:nrow(raw_data)) { 
  if(is.na(raw_data$steps[i])) { 
     row_id <- which(interval_steps$interval==raw_data$interval[i])
     fill_data$steps[i] <- interval_steps$steps[row_id]
    }
  }
fill_steps_per_day <- tapply(fill_data$steps, fill_data$date, sum, na.rm=TRUE)
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```{r}
hist(fill_steps_per_day, main="Histogram of total number of steps per day", xlab="Steps Per Day",  border=1)
# Mean of the total steps takes per day after imputing missing values
mean(fill_steps_per_day)
median(fill_steps_per_day)
```

**Mean values remain same, but there is a slight difference in medians**

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels– “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```{r}
fill_data['day_type'] <- weekdays(as.Date(fill_data$date))
fill_data$day_type[fill_data$day_type %in% c('Saturday','Sunday')] <- "weekend"
fill_data$day_type[fill_data$day_type != "weekend"] <- "weekday"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
# convert day type from character to factor
fill_data$day_type <- as.factor(fill_data$day_type)
steps_by_day_type <- aggregate(steps~interval+day_type, fill_data, mean )
ggplot(steps_by_day_type, aes(interval, steps))+ geom_line()+facet_grid(day_type~.)+xlab("5 minute interval")+ ylab("Number of Steps")
```



**Relatively more foot steps in the range of 1000-2000 over the weekends**
