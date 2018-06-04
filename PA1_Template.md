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

2. Histogram of the total number of steps taken each day

```{r}
ggplot(stepperday, aes(x = Total_Steps)) +
  geom_histogram(fill = "red", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

3. Mean and median of the total number of steps taken per day

```{r}
mean(stepperday$Total_Steps, na.rm = TRUE)
median(stepperday$Total_Steps, na.rm = TRUE)
```

## Average daily activity pattern

### time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

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

### Maximum Number of steps in 5-minute interval, on average across all the days in the dataset

```{r}
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval
```

##  Imputing missing values

### Total number of missing values in the dataset

```{r}
missingVals <- sum(rawdataNA)
missingVals
```

### Strategy for filling in all of the missing values in the dataset.
### Use mean interval steps from Mean Steps for that interval.
### new dataset that is equal to the original dataset but with the missing data filled in

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

```{r}
# count of NA values
sum(is.na(rawdata1$steps))
```

### histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

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

###  Do these values differ from the estimates from the first part of the assignment

#### Yes, he mean is the same but the median has risen 1.19 steps. 

#### Old Mean & Median
```{r}
mean(stepperday$Total_Steps, na.rm = TRUE)
median(stepperday$Total_Steps, na.rm = TRUE)
```
#### New Mean and Median
```{r}
mean(FullSummedDataByDay$totalsteps)
median(FullSummedDataByDay$totalsteps)
```

### Impact of imputing missing data on the estimates of the total daily number of steps

#### The effect of using mean data per interval as a data impute method for missing values seems to push overall data towards the mean.

```{r}
rawdata1$weekday <- weekdays(as.Date(rawdata1$date))
rawdata1$weekend <- ifelse (rawdata1$weekday == "Saturday" | rawdata1$weekday == "Sunday",  "Weekend", "Weekday")
#baseData2$weekend <- as.factor(baseData2$weekend)
head(rawdata1,5)

Meandataweek <- aggregate(rawdata1$steps, by=list(rawdata1$weekend, rawdata1$interval), mean)
names(Meandataweek)[1] = "weekend"
names(Meandataweek)[2] = "interval"
names(Meandataweek)[3] = "steps"

ggplot(Meandataweek, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```
