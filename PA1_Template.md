
Reproducible Research Assignment
Load Require Packages

library(ggplot2)
library(scales)
library(data.table)

Load and Process Data
Download the file

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/rep_data_activity.zip")

Unzip dataSet to /data directory

unzip(zipfile="./data/rep_data_activity.zip",exdir="./data")

Read txt file in a variable applying filters.

rawdata <- read.csv("./data/activity.csv", header=T, sep=',')

mean total number of steps taken per day
Mean total number of steps taken per day

stepperday <- aggregate(rawdata$steps, by =list(rawdata$date), sum)
names(stepperday)[1] ="Date"
names(stepperday)[2] ="Total_Steps"
head(stepperday,5)

##         Date Total_Steps
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294

Histogram of the total number of steps taken each day

ggplot(stepperday, aes(x = Total_Steps)) +
  geom_histogram(fill = "red", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

## Warning: Removed 8 rows containing non-finite values (stat_bin).

Mean and median of the total number of steps taken per day

mean(stepperday$Total_Steps, na.rm = TRUE)

## [1] 10766.19

median(stepperday$Total_Steps, na.rm = TRUE)

## [1] 10765

Average daily activity pattern
time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

rawdataNA <- is.na(rawdata$steps)
cleandata <- rawdata[!rawdataNA,]
MeanDataByInterval <- aggregate(cleandata$steps, by=list(cleandata$interval), mean)
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="blue") 

Maximum Number of steps in 5-minute interval, on average across all the days in the dataset

maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval

##     interval    steps
## 104      835 206.1698

Imputing missing values
Total number of missing values in the dataset

missingVals <- sum(rawdataNA)
missingVals

## [1] 2304

Strategy for filling in all of the missing values in the dataset.
Use mean interval steps from Mean Steps for that interval.
new dataset that is equal to the original dataset but with the missing data filled in

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

## [1] 2304

# count of NA values
sum(is.na(rawdata1$steps))

## [1] 0

histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

FullSummedDataByDay <- aggregate(rawdata1$steps, by=list(rawdata1$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,10)

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

# Plot using ggplot
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

# Mean on New Data
mean(FullSummedDataByDay$totalsteps)

## [1] 10766.19

# Median on New Data
median(FullSummedDataByDay$totalsteps)

## [1] 10766.19

Do these values differ from the estimates from the first part of the assignment
Yes, he mean is the same but the median has risen 1.19 steps.
Old Mean & Median

mean(stepperday$Total_Steps, na.rm = TRUE)

## [1] 10766.19

median(stepperday$Total_Steps, na.rm = TRUE)

## [1] 10765

New Mean and Median

mean(FullSummedDataByDay$totalsteps)

## [1] 10766.19

median(FullSummedDataByDay$totalsteps)

## [1] 10766.19

Impact of imputing missing data on the estimates of the total daily number of steps
The effect of using mean data per interval as a data impute method for missing values seems to push overall data towards the mean.

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


