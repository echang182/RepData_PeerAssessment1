---
title: "Project 1 Reproducible research"
author: "Eloy Chang"
date: "8 de mayo de 2016"
output: html_document
---

# loading and preprocessing the data

* Load the data
```{r}
datos<- read.csv("activity.csv")
```

# What is mean total number of steps taken per day?

* Make a histogram of the total number of steps taken each day
```{r}
steps.day<- tapply(datos$steps, datos$date, sum)
hist(steps.day, main = "Number of steps by day", xlab = "Steps")
```

* Calculate and report the mean and median total number of steps taken per day
```{r}
steps.mean<- mean(steps.day, na.rm = TRUE)
steps.median<- median(steps.day, na.rm = TRUE)
```
The mean of steps by day is: `r steps.mean`   

The median of steps by day is: `r steps.median`

# What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps.interval<- tapply(datos$steps, datos$interval, mean, na.rm = TRUE)
intervals<- unique(datos$interval)
plot(intervals, steps.interval, main = "Steps by intervals", ylab = "mean(Steps)", type = "l")
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
interval.max<- which.max(steps.interval)
```
The interval with maximum number of steps is: `r interval.max`

# Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
number.NA<- sum(is.na(datos))
```
The total number of NA is: `r number.NA`

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc
```{r}
steps.NA <- datos$steps 
number.NA<- 0
for (i in 1:length(steps.NA)) {
    if (is.na(steps.NA[i])) {
        steps.NA[i]=steps.interval[which(intervals==datos$interval[i])]
        number.NA<- number.NA + 1
    }
}
```
The total number of NA processed is: `r number.NA`

* Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
datos2<- data.frame(steps.NA, datos$date, datos$interval)
names(datos2)<- names(datos)
number.NA<- sum(is.na(datos2))
```
The total number of NA of the processed data is: `r number.NA`

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
steps.day2<- tapply(datos2$steps, datos2$date, sum)
hist(steps.day, col=rgb(0.1,0.1,0.1,0.5), main = "Number of steps by day", xlab = "Steps")
hist(steps.day2, main = "Number of steps by day", xlab = "Steps",col=rgb(0.8,0.8,0.8,0.5), add=T)
steps.mean2<- mean(steps.day2, na.rm = TRUE)
steps.median2<- median(steps.day2, na.rm = TRUE)
dif.mean<- abs(steps.mean - steps.mean2)
dif.median<- abs(steps.median - steps.median2)
```
Both histograms are almost the same.

The difference between the means are: `r dif.mean`

The difference between the medians are: `r dif.median`

# Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
datos2$date<- as.Date(as.character(datos2$date))
category<- weekdays(datos2$date)
p<- ((category=="lunes")|(category=="martes")|(category=="miercoles")|
              (category=="Jueves")|(category=="viernes"))
category[p]<- "weekday"
category[!p]<- "weekend"
category<- as.factor(category)
table(category)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
datos.split<- split(datos2, category)
datos.weekday<- datos.split[[1]]
datos.weekend<- datos.split[[2]]
steps.weekday<- tapply(datos.weekday$steps, datos.weekday$interval, mean)
steps.weekend<- tapply(datos.weekend$steps, datos.weekend$interval, mean)
par(mfrow = c(1,2))
plot(intervals, steps.weekday, main = "Steps by intervals (Weekday)", ylab = "mean(Steps)", type = "l")
plot(intervals, steps.weekend, main = "Steps by intervals (Weekend)", ylab = "mean(Steps)", type = "l")
```
