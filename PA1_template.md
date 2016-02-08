# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Project data was supplied as a aip file `activity.zip` which was unzipped
manually to yield `activity.csv`.


```r
require(dplyr)
require(ggplot2)

data <- read.csv("activity.csv")

# hour is zero-based, slot it one-based :-(
data2          <- mutate(data, 
                   hour = as.integer((0.001 + interval) / 100),
                   slot = 1 + (interval %% 100) / 5 )

data3          <- select(data2, date, hour, slot, steps)
data3$date     <- as.Date(data3$date)

avgBySlot      <- select(data3, slot, steps) %>%
                  group_by(slot)             %>% 
                  summarize(meanSteps=mean(steps, na.rm=TRUE))

avgByHour      <- select(data3, hour, steps) %>%
                  group_by(hour)             %>% 
                  summarize(meanSteps=mean(steps, na.rm=TRUE))
```

## What is mean total number of steps taken per day?



```r
perDay         <- select(data3, date, steps) %>% 
                  group_by(as.factor(date))  %>%
                  summarize(totalSteps=sum(steps, na.rm=TRUE))

ggplot(data=perDay, aes(totalSteps)) 	           +
    xlab('Steps per Day') 		           +
    ylab('Number of Days with That Number of Steps')	   +
    ggtitle('Steps per Day in Period of Interest') +
    geom_histogram(bins=100)
```

![](PA1_template_files/figure-html/histogram-1.png)

```r
meanSteps   <- sprintf("%.0f", mean(perDay$totalSteps))
medianSteps <- sprintf("%.0f", median(perDay$totalSteps))
```

The figure above shows how many days in the period of interest have step
counts in various ranges (buckets).

The mean number of steps per day is 9354 and the median is 
10395.

## What is the average daily activity pattern?

This is understood to be the average across all 60 or so days in the period
for which we have data.  There are 288 = 12x24 5-minute intervals in each
day; this is the number on the x-axis in the graph.


```r
# Add a column converting hour and slot to 'daySlot', the [1..288] index 
# of this particular 5-minute slot in the day

data4  <- mutate(data3, daySlot= (hour * 12) + slot)

data5  <- select(data4, daySlot, steps) %>% 
          group_by(daySlot)             %>%
          summarize(meanSteps = mean(steps, na.rm=TRUE))

plot(data5$daySlot, data5$meanSteps, type='l', 
       main='Typical Daily Activity (Steps)',
       xlab='5-minute interval [1:288]',
       ylab='Average Number of Steps in Interval')
```

![](PA1_template_files/figure-html/dailyActivity-1.png)

```r
maxMeanSteps <- max(data5$meanSteps)
```

The maximum averagenumber of steps in any 5-minute interval was 
206.17.

## Imputing missing values

### Calculate and Report Total Number of Missing Values


### Devise a Strategy for Filling in Missing Values

### Create a New Dataset With Missing Values Replaced

### Make a Histogram of Total Steps Taken Each Day

### Calculate and Report Mean and Median

### Effect of Imputing on Estimates of Total Daily Steps



## Are there differences in activity patterns between weekdays and weekends?

