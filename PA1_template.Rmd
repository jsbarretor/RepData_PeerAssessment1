---
title: "Course Project 1"
author: "Sebastian Barreto"
date: "8/27/2020"
output: html_document
---

## Load libraries

```{r , echo=TRUE}
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

Read the csv file with the data and delete NA

```{r , echo=TRUE}
data.raw <- read.csv(file = "D:/Coursera/Reproducible Research/Semana2/Proyecto/activity.csv", header = T)
data <- na.omit(data.raw)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```{r , echo=TRUE}
by.day <- group_by(data,date)
steps.by.day <- summarise(by.day, total=sum(steps))
steps.by.day
```

2. Make a histogram of the total number of steps taken each day

```{r , echo=TRUE}
hist1 <- hist(steps.by.day$total,
              main="Histogram: Total number of steps by day",
              xlab = "Day (date)",
              ylab = "Frequency",
              col = "blue")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r , echo=TRUE}
mean.1 <- mean(steps.by.day$total)
median.1 <- median(steps.by.day$total)
```

The mean of the total number of steps taken per day is `r mean.1` an the median is `r median.1`

## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)}

```{r , echo=TRUE}
by.interval <- group_by(data,interval)
steps.by.interval <- summarise(by.interval, meann=mean(steps))
ggplot(steps.by.interval, aes(x=interval, y=meann)) + 
        geom_line(color="steelblue") +
        ggtitle("Average number of steps by interval") +
        xlab("Interval") +
        ylab("Average number of steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r , echo=TRUE}
max.step <- which.max(steps.by.interval$meann)
num.max.steps <- steps.by.interval[max.step,]
```

The intervale with most steps is `r num.max.steps` steps

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```{r , echo=TRUE}
total.na <- sum(!complete.cases(data.raw))
```

The dataset have `r total.na` rows with NA

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r , echo=TRUE, warning=FALSE }
complete.na <- data.raw
for (i in 1:nrow(complete.na)){
        if (is.na(complete.na$steps[i])){
                value.interval <- complete.na$interval[i]
                steps.value <- by.interval[by.interval$interval == value.interval,]
                complete.na$steps[i] <- steps.value$steps}
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r , echo=TRUE, warning=FALSE }
no.na <- complete.na
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r , echo=TRUE}
by.day.na <- group_by(no.na,date)
steps.by.day.na <- summarise(by.day.na, total=sum(steps))
steps.by.day.na
hist2 <- hist(steps.by.day.na$total,
              main="Histogram: Total number of steps by day",
              xlab = "Day (date)",
              ylab = "Frequency",
              col = "blue")
mean.2 <- mean(steps.by.day.na$total)
median.2 <- median(steps.by.day.na$total)
```

The mean of the total number of steps taken per day (in the dataset withour NA) is `r mean.2` an the median is `r median.2`

Imputing missing data INCREASE the total daily number of steps, cause no values now are a value

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r , echo=TRUE}
no.na['Type_Day'] <- weekdays(as.Date(no.na$date))
no.na$Type_Day[no.na$Type_Day %in% c('Saturday','Sunday')] <- "Weekend"
no.na$Type_Day[no.na$Type_Day != "Weekend"] <- "Weekday"
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r , echo=TRUE}
no.na$Type_Day <- as.factor(no.na$Type_Day)
diff.steps.by.interval <- aggregate(steps ~ interval + Type_Day, no.na, mean)
ggplot(diff.steps.by.interval, aes(interval, steps))+
        geom_line(stat = "identity", aes(colour=Type_Day))+
        facet_grid(Type_Day ~ ., scales = "fixed", space = "fixed") +
        labs(x= "Intervals", y="Number of steps")
```