
Activity Monitoring
========================================================
### Reproducible Research Assignment One
The purpose of this assignment is to assess monitoring device activity and to produce a graphical presentation.   

### Libraries used for data processing.

```r
library(stats)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, desc, failwith, id, mutate, summarise, summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
library(zoo)
```

```
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

### Data is read into R.

```r
act <- read.csv("repActivity.csv")
```

### Data processing for histogram of steps taken per day and table with mean and median steps per day.
The following processing steps were used:   
1.  Number of NAs determined to be **2304**.    
2.  NAs were removed from data set.    
3.  Data was selected, grouped, and summarized in a tidy format.   
## Histogram I.  Total Steps Per Day After NA Removal.                     
#### Prepare data for histogram and create histogram.

```r
length(which(is.na(act)))
```

```
## [1] 2304
```

```r
act1 <- na.omit(act)
actHistogram <- act1 %.% select(steps, date) %.% group_by(date) %.% summarise(round(mean(steps), 
    2), sum(steps), median(steps))
names(actHistogram)[1] <- "Date"
names(actHistogram)[2] <- "Mean"
names(actHistogram)[3] <- "Sum"
names(actHistogram)[4] <- "Median"
hist(actHistogram$Sum, xlab = "Number of Steps per Day", main = "Total Steps Per Day", 
    col = "gray")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## Table
#### Prepare and print table with date, mean and median.

```r
actTable <- act1 %.% select(steps, date) %.% group_by(date) %.% summarise(round(mean(steps), 
    2), median(steps))
names(actTable)[1] <- "Date"
names(actTable)[2] <- "Mean"
names(actTable)[3] <- "Median"
print(actTable)
```

```
## Source: local data frame [53 x 3]
## 
##          Date  Mean Median
## 1  2012-10-02  0.44      0
## 2  2012-10-03 39.42      0
## 3  2012-10-04 42.07      0
## 4  2012-10-05 46.16      0
## 5  2012-10-06 53.54      0
## 6  2012-10-07 38.25      0
## 7  2012-10-09 44.48      0
## 8  2012-10-10 34.38      0
## 9  2012-10-11 35.78      0
## 10 2012-10-12 60.35      0
## 11 2012-10-13 43.15      0
## 12 2012-10-14 52.42      0
## 13 2012-10-15 35.20      0
## 14 2012-10-16 52.38      0
## 15 2012-10-17 46.71      0
## 16 2012-10-18 34.92      0
## 17 2012-10-19 41.07      0
## 18 2012-10-20 36.09      0
## 19 2012-10-21 30.63      0
## 20 2012-10-22 46.74      0
## 21 2012-10-23 30.97      0
## 22 2012-10-24 29.01      0
## 23 2012-10-25  8.65      0
## 24 2012-10-26 23.53      0
## 25 2012-10-27 35.14      0
## 26 2012-10-28 39.78      0
## 27 2012-10-29 17.42      0
## 28 2012-10-30 34.09      0
## 29 2012-10-31 53.52      0
## 30 2012-11-02 36.81      0
## 31 2012-11-03 36.70      0
## 32 2012-11-05 36.25      0
## 33 2012-11-06 28.94      0
## 34 2012-11-07 44.73      0
## 35 2012-11-08 11.18      0
## 36 2012-11-11 43.78      0
## 37 2012-11-12 37.38      0
## 38 2012-11-13 25.47      0
## 39 2012-11-15  0.14      0
## 40 2012-11-16 18.89      0
## 41 2012-11-17 49.79      0
## 42 2012-11-18 52.47      0
## 43 2012-11-19 30.70      0
## 44 2012-11-20 15.53      0
## 45 2012-11-21 44.40      0
## 46 2012-11-22 70.93      0
## 47 2012-11-23 73.59      0
## 48 2012-11-24 50.27      0
## 49 2012-11-25 41.09      0
## 50 2012-11-26 38.76      0
## 51 2012-11-27 47.38      0
## 52 2012-11-28 35.36      0
## 53 2012-11-29 24.47      0
```

### Data processing for plot of average steps taken per day averaged across all days.
Data was selected, grouped by interval, and summarized in a tidy format.
## Plot I.  Average Daily Activity Pattern After NA Removal.                     
#### Prepare data for plot and create plot.

```r
actPlot <- act1 %.% select(steps, interval) %.% group_by(interval) %.% summarise(mean(steps), 
    sum(steps), median(steps), max(steps))
names(actPlot)[2] <- "meanSteps"
names(actPlot)[3] <- "sumSteps"
names(actPlot)[4] <- "medianSteps"
plot(actPlot$meanSteps ~ actPlot$interval, type = "l", lwd = 2, col = "steelblue", 
    xlab = "Time Interval (min)", ylab = "Average Number of Steps Taken", main = "Average Daily Activity Pattern")
abline(v = 835, col = "red", lwd = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

#### As shown in Plot 1 and in the below chunk, the maximum number of steps is **206.17** at interval **835** (shown in red).

```r
maX <- actPlot %.% select(interval, meanSteps) %.% filter(meanSteps == max(meanSteps))
round(maX, 2)
```

```
## Source: local data frame [1 x 2]
## 
##   interval meanSteps
## 1      835     206.2
```


### Data processing of the activity data to incorporate values for missing values.                 
As shown above for generating a histogram, there are **2304** NAs in the data set.                    
### Strategy for filling in the missing values.                 
1.  Local values were used to fill in all NAs except for the first 288 from October 1, 2012.      
2.  The first 288 values were filled in with data from October 15, 2012, which appeared to have typical Monday activity.    
3.  The appropriate data was extracted, and combined.    
4.  Data was transformed into numbers and day of the week levels.   
5.  Finally, data was grouped, summarized and plotted.   
## Imputted missing values.  

```r
b <- na.locf(act)
c <- b[289:17568, ]
p <- act[4033:4320, ]
p1 <- select(p, steps)
q <- b[1:288, ]
r <- select(q, date, interval)
s <- cbind(p1, r)
t <- rbind(s, c)
t1 <- transform(t, steps = as.numeric(steps), interval = as.numeric(interval), 
    date = as.factor(as.Date(date)))
u <- transform(t, steps = as.numeric(steps), interval = as.numeric(interval), 
    date = as.factor(weekdays(as.Date(date))))
```

## Histogram II.  Total Steps Per Day After Imputing 2304 Missing Values.
#### Prepare data for histogram II and create histogram II.   
Overall, histogram II is similar to histogram I, except there is greater activity between 1-5000 steps.  This may be a result of the how the na.locf function uses local values to fill in missing values.  This scheme of filling in data was chosen because it seems that it might be more representative of the data.  However, other schemes may give different results.  Without further analysis it is unclear what the optimal procedure is.

```r
actHistogramII <- t1 %.% select(steps, date) %.% group_by(date) %.% summarise(round(mean(steps), 
    2), sum(steps), median(steps))
names(actHistogramII)[1] <- "Date"
names(actHistogramII)[2] <- "Mean"
names(actHistogramII)[3] <- "Sum"
names(actHistogramII)[4] <- "Median"
hist(actHistogramII$Sum, xlab = "Number of Steps Per Day", main = "Total Steps Per Day", 
    col = "gray")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

## Plot II.  Average Daily Activity Pattern After Imputing 2304 Missing Values.

```r
v <- u %.% group_by(interval) %.% summarise(round(mean(steps), 2), sum(steps))
names(v)[2] <- "meanSteps"
names(v)[3] <- "sumSteps"
plot(v$meanSteps ~ v$interval, type = "l", lwd = 2, col = "blue", xlab = "Time Interval (min)", 
    ylab = "Average Number of Steps Taken", main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


### Data processing for plotting weekday and weekend activity data.      
1.  The Monday-Friday levels were converted to weekday.        
2.  The Saturday and Sunday levels were converted to weekend.             
3.  Two plots were generated comparing weekend and weekday activities.                                              

## Plot III.  Comparison of Weekend and Weekday Average Daily Activity Pattern After Imputing 2304 Missing Values.    
As shown in plot III, there is much greater activity on weekdays than weekends.

```r
levels(u$date) <- c("weekday", "weekday", "weekend", "weekend", "weekday", "weekday", 
    "weekday")
v <- u %.% group_by(interval, date) %.% summarise(round(mean(steps), 2), sum(steps))
names(v)[3] <- "meanSteps"
names(v)[4] <- "sumSteps"
xyplot(sumSteps ~ interval | date, type = "l", layout = c(1, 2), data = v, xlab = "Interval", 
    ylab = "Number of steps", lwd = 2)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



