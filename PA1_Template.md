---
title: "Reproducible Research Project 1"
author: "Kamlesh Mishra"
date: "6/04/2018"
---

Loading and preprocessing the data
----------------------------------

Load Require Packages

```{r}
library(ggplot2)
library(scales)
library(data.table)
```

Download the file

```{r}
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/rep_data_activity.zip")
```

Unzip dataSet to /data directory

```{r}
unzip(zipfile="./data/rep_data_activity.zip",exdir="./data")
```

Read txt file in a variable applying filters.

```{r}
rawdata <- read.csv("./data/activity.csv", header=T, sep=',')
```

What is mean total number of steps taken per day?

1. Mean total number of steps taken per day

```{r}
stepperday <- aggregate(rawdata$steps, by =list(rawdata$date), sum)
names(stepperday)[1] ="Date"
names(stepperday)[2] ="Total_Steps"
head(stepperday,5)
```
  
         Date             Total_Steps
     1:  2012-10-01          NA
     2:  2012-10-02         126
     3:  2012-10-03       11352
     4:  2012-10-04       12116
     5:  2012-10-05       13294


1.  If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```{r}
ggplot(stepperday, aes(x = Total_Steps)) +
  geom_histogram(fill = "red", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```
![](https://raw.githubusercontent.com/kamleshmishra18/Reproducible-Reserach-2/master/Figures/Total_Daily_Steps.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(stepperday$Total_Steps, na.rm = TRUE)
median(stepperday$Total_Steps, na.rm = TRUE)
```
    [1] 10766.19 (Mean)
    [1] 10765 (Median)
    
What is the average daily activity pattern?
-------------------------------------------
1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```{r}
rawdataNA <- is.na(rawdata$steps)
cleandata <- rawdata[!rawdataNA,]
MeanDataByInterval <- aggregate(cleandata$steps, by=list(cleandata$interval), mean)
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="blue") 
```
![](https://raw.githubusercontent.com/kamleshmishra18/Reproducible-Reserach-2/master/Figures/Sum%20of%20Steps%20by%20Interval.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval
```
         interval    steps
     104 835         206.1698
    
Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```{r}
missingVals <- sum(rawdataNA)
missingVals
```
      [1] 2304
1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
# NA's in dataset
# dataset minus NA's for Mean calculation
rawdata1 <- rawdata
rawdata1NA <- rawdata1[is.na(rawdata1$steps),]
cleandata1 <- rawdata1[!is.na(rawdata1$steps),]

# generate Mean Data2 by interval
MeanData1ByInterval <- aggregate(cleandata1$steps, by=list(cleandata1$interval), sum)

names(MeanData1ByInterval)[1] ="interval"
names(MeanData1ByInterval)[2] ="steps"

# IMPUT METHOD

rawdata1 <- rawdata
missingData <- is.na(rawdata1$steps)
meanVals <- tapply(cleandata$steps, cleandata$interval, mean, na.rm=TRUE, simplify=TRUE)
rawdata1$steps[missingData] <- meanVals[as.character(rawdata1$interval[missingData])]
sum(missingData)
```

1.  Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r}
# count of NA values
sum(is.na(rawdata1$steps))
```

histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```{r}
FullSummedDataByDay <- aggregate(rawdata1$steps, by=list(rawdata1$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,10)

# Plot using ggplot
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

# Mean on New Data
mean(FullSummedDataByDay$totalsteps)

# Median on New Data
median(FullSummedDataByDay$totalsteps)
```
        ##          date totalsteps
        ## 1  2012-10-01   10766.19
        ## 2  2012-10-02     126.00
        ## 3  2012-10-03   11352.00
        ## 4  2012-10-04   12116.00
        ## 5  2012-10-05   13294.00
        ## 6  2012-10-06   15420.00
        ## 7  2012-10-07   11015.00
        ## 8  2012-10-08   10766.19
        ## 9  2012-10-09   12811.00
        ## 10 2012-10-10    9900.00

![](https://raw.githubusercontent.com/kamleshmishra18/Reproducible-Reserach-2/master/Figures/Total%20Daily%20Steps.png)
Do these values differ from the estimates from the first part of the assignment

Yes, he mean is the same but the median has risen 1.19 steps. 

Old Mean & Median
```{r}
mean(stepperday$Total_Steps, na.rm = TRUE)
median(stepperday$Total_Steps, na.rm = TRUE)
```
      ## [1] 10766.19
      ## [1] 10765
New Mean and Median
```{r}
mean(FullSummedDataByDay$totalsteps)
median(FullSummedDataByDay$totalsteps)
```
      ## [1] 10766.19
      ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
rawdata1$weekday <- weekdays(as.Date(rawdata1$date))
rawdata1$weekend <- ifelse (rawdata1$weekday == "Saturday" | rawdata1$weekday == "Sunday",  "Weekend", "Weekday")
#baseData2$weekend <- as.factor(baseData2$weekend)
head(rawdata1,5)
      ##       steps       date interval weekday weekend
      ## 1 1.7169811 2012-10-01        0  Monday Weekday
      ## 2 0.3396226 2012-10-01        5  Monday Weekday
      ## 3 0.1320755 2012-10-01       10  Monday Weekday
      ## 4 0.1509434 2012-10-01       15  Monday Weekday
      ## 5 0.0754717 2012-10-01       20  Monday Weekday
      
Meandataweek <- aggregate(rawdata1$steps, by=list(rawdata1$weekend, rawdata1$interval), mean)
names(Meandataweek)[1] = "weekend"
names(Meandataweek)[2] = "interval"
names(Meandataweek)[3] = "steps"

ggplot(Meandataweek, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```
![](https://raw.githubusercontent.com/kamleshmishra18/Reproducible-Reserach-2/master/Figures/Mean%20of%20Steps%20by%20Interval.png)
