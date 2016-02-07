# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Project data was supplied as a aip file `activity.zip` which was unzipped
manually to yield `activity.csv`.


```r
require(ggplot2)
data <- read.csv("activity.csv")

#  Just a simple comment.
#` A backticked comment
#' An SQUOTEd comment
```

## What is mean total number of steps taken per day?


```r
stepsPerDay      <- aggregate(data=data, steps ~ date, FUN = sum)
stepsPerDay$date <- as.Date(stepsPerDay$date)


ggplot(data=stepsPerDay, aes(steps)) 	           + 
    xlab('Steps per Day') 		           + 
    ylab('Number of Days with That Number of Steps')	   +
    ggtitle('Steps per Day in Period of Interest') + 
    geom_histogram(bins=100)
```

![](PA1_template_files/figure-html/histogram-1.png)
The figure above shows how many days in the period of interest have step 
counts in various buckets.


```r
meanStepsPerDay  <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
```

We can see from the above that the mean number of steps per day is
10766 and the median is 10765.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
