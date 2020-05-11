---
output:
  html_document: 
    keep_md: yes
  pdf_document: default
  word_document: default
---
---
# Reproducible Research - Project 1

## Read the dataset

```r
library(tidyverse)
library(ggplot2)
df <- read_csv("./activity.csv")
df$weekday <- weekdays(df$date)
```

## Histogram of the total number of steps taken each day

```r
steps_by_day <- aggregate(df$steps, list(df$date), sum)
steps_by_day <- steps_by_day %>%
    rename(dates = "Group.1", num_steps = "x")
steps_by_day <- na.omit(steps_by_day)
ggplot(steps_by_day, aes(x = dates, y = num_steps, fill = dates)) + geom_col() + labs(x = "Date", y = "Number of Steps", 
        title =  "Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Mean and median number of steps taken each day

```r
mean(steps_by_day$num_steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_day$num_steps)
```

```
## [1] 10765
```

## Time series plot of the average number of steps taken

```r
avg_steps_by_interval <- aggregate(df$steps, list(df$interval), mean, na.rm = TRUE)
avg_steps_by_interval <- avg_steps_by_interval %>%
    rename(interval = "Group.1", mean = "x")

ggplot(avg_steps_by_interval, aes(x = interval, y = mean)) + geom_line() + labs(title = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 5-Minute Interval with the Maximum Average Number of Steps

```r
avg <- avg_steps_by_interval$interval[which.max(avg_steps_by_interval$mean)]
```
The 5-minute interval with the maximum average number of steps is 835.
## Imputing Missing Values

* Total number of missing values is:

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

* To fill NAs, using the average steps by interval

```r
df_new <- df # df_new would be the new date frame with filled NAs
for (i in avg_steps_by_interval$interval) {
    df_new[df_new$interval == i & is.na(df_new$steps), ]$steps <- 
        avg_steps_by_interval[avg_steps_by_interval$interval == i, ]$mean
    }
head(df_new)
```

```
## # A tibble: 6 x 4
##    steps date       interval weekday
##    <dbl> <date>        <dbl> <chr>  
## 1 1.72   2012-10-01        0 Monday 
## 2 0.340  2012-10-01        5 Monday 
## 3 0.132  2012-10-01       10 Monday 
## 4 0.151  2012-10-01       15 Monday 
## 5 0.0755 2012-10-01       20 Monday 
## 6 2.09   2012-10-01       25 Monday
```

The dataset with all NAs filled in is now called df_new

* Total number of steps taken ecah day, and mean and median

```r
num_of_steps_new <- aggregate(df_new$steps, list(df_new$date), sum)
num_of_steps_new <- num_of_steps_new %>%
    rename(date = "Group.1", num_of_steps = "x")
mean_new <- mean(num_of_steps_new$num_of_steps)
median_new <- median(num_of_steps_new$num_of_steps)
```

The mean of the new dataset is now 1.0766189\times 10^{4} and the median is now 1.0766189\times 10^{4}

* Histogram of the Total Number of Steps Taken Each Day 

```r
ggplot(num_of_steps_new, aes(x = date, y = num_of_steps, fill = date)) + geom_col()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

* Creating the New Factor Vector to Indicate Weekdays and Weekends

```r
working_week <- c()
for (i in 1:nrow(df_new)) {
    if (df_new[i, 4] == "Saturday" | df_new[i, 4] == "Sunday") {
        working_week[i] <- "Weekend"
    }
    else {
        working_week[i] <- "Weekday"
    }
}
working_week <- as.factor(working_week)
df_new <- cbind(df_new, working_week)
```

* Preview of the Nwe Data Frame

```r
head(df_new)
```

```
##       steps       date interval weekday working_week
## 1 1.7169811 2012-10-01        0  Monday      Weekday
## 2 0.3396226 2012-10-01        5  Monday      Weekday
## 3 0.1320755 2012-10-01       10  Monday      Weekday
## 4 0.1509434 2012-10-01       15  Monday      Weekday
## 5 0.0754717 2012-10-01       20  Monday      Weekday
## 6 2.0943396 2012-10-01       25  Monday      Weekday
```

* Panel PLot

```r
steps_by_interval_and_working_week <- aggregate(steps ~ interval + working_week, data = df_new, sum)
ggplot(steps_by_interval_and_working_week, aes(x = interval, y = steps)) + geom_line() + facet_grid(working_week ~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
