Module 5 Project1
================

Load and preprocess data
------------------------

``` r
activity<-read.csv(file="activity.csv",header=TRUE)
```

What is the mean total number of steps taken per day
----------------------------------------------------

``` r
#calculate the total number of steps taken per day
totalSteps<-aggregate(steps~date,activity,FUN=sum)
#Histogram of total number of steps
hist(totalSteps$steps,main="Total steps per day",xlab="Number of steps")
```

![](PA1_template_files/figure-markdown_github/steps%20mean-1.png)

``` r
#Calculate the mean and median of total steps per day
meanSteps<-mean(totalSteps$steps,na.rm=TRUE)
medsteps<-median(totalSteps$steps,na.rm=TRUE)
meanSteps
```

    ## [1] 10766.19

``` r
medsteps
```

    ## [1] 10765

What is the average daily activity pattern
------------------------------------------

``` r
averageintervaldata<-aggregate(steps~interval,activity,mean)
plot(x= averageintervaldata$interval, y= averageintervaldata$steps, type = "l")
```

![](PA1_template_files/figure-markdown_github/daily%20pattern-1.png)

``` r
#Calculate the interval which contains maximum number of steps
maxInt<-averageintervaldata[which.max(averageintervaldata$steps),]
maxInt$interval
```

    ## [1] 835

Inputting missing values
------------------------

``` r
missingvalues<-is.na(activity$steps)

#Create a new dataset that has missing values

imp_activity<-transform(activity,steps=ifelse(is.na(activity$steps),
averageintervaldata$steps[match(activity$interval,averageintervaldata$interval)],
activity$steps))

#Create a histogram with filled data values

impSteps<-aggregate(steps~date,imp_activity,FUN=sum)
hist(impSteps$steps,main="Imputed number of steps per day",xlab="number of steps")
```

![](PA1_template_files/figure-markdown_github/missing%20values-1.png)

``` r
#mean and median with filled in data
newmean<-mean(impSteps$steps)
newmedian<-median(impSteps$steps)
newmean
```

    ## [1] 10766.19

``` r
newmedian
```

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` r
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")
}
imp_activity$date<-as.Date(imp_activity$date)
imp_activity$day<-sapply(imp_activity$date,FUN=DayType)

meanstepsbyDay<-aggregate(steps~interval+day,imp_activity,mean)
library(ggplot2)
ggplot(data=meanstepsbyDay,aes(x=interval,y=steps))+
  geom_line()+
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps")
```

![](PA1_template_files/figure-markdown_github/activity%20pattern-1.png)
