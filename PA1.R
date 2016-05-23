#Load the data (i.e. read.csv())
#Process/transform the data (if necessary) into a format suitable for your analysis
activity <- read.csv("./data/activity.csv")


#Calculate the total number of steps taken per day
library(dplyr)
z <- group_by(activity, date)
stepsdate <- summarise(z, steps=sum(as.integer(steps), na.rm=TRUE))

#If you do not understand the difference between a histogram and a barplot, 
#research the difference between them. 
#Make a histogram of the total number of steps taken each day
hist(stepsdate$steps)

#Calculate and report the mean and median of the total number of steps 
#taken per day

mean(stepsdate$steps)
median(stepsdate$steps)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)

y <- group_by(activity, interval)
intval <- summarise(y, meanofsteps=mean(as.numeric(steps), na.rm=TRUE))
head(intval)
library(ggplot2)
ggplot(intval, aes(interval,meanofsteps)) + geom_line() +
  xlab("5-minute interval") + ylab("Mean of steps")

#Which 5-minute interval, on average across all the days in the dataset, contains the 
#maximum number of steps?
intval[which.max(intval$meanofsteps),]$interval

#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)

NAs <- subset(activity, is.na(activity$steps)==TRUE)
count(NAs)

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use 
#the mean/median for that day,or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the 
#missing data filled in.


FilledDS <- activity

stepsfunc <- function(interval){
  intval[intval$interval==interval,]$meanofsteps
}

#stepsfunc(0)

i=0
for(i in 1:nrow(FilledDS)){
  if(is.na(FilledDS[i,]$steps)){
    FilledDS[i,]$steps <- stepsfunc(FilledDS[i,]$interval)
  }
  i=i+1
}
head(FilledDS)


#Make a histogram of the total number of steps taken each day and Calculate 
#and report the mean and median total number of steps taken per day. Do 
#these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?

v <- group_by(FilledDS, date)
stepsday <- summarise(v, sumofsteps=sum(as.numeric(steps), na.rm=TRUE))
head(stepsday)
hist(stepsday$sumofsteps)
mean(stepsday$sumofsteps, na.rm=TRUE)
median(stepsday$sumofsteps, na.rm=TRUE)


#Create a new factor variable in the dataset with two levels - "weekday" and 
#"weekend" indicating whether a given date is a weekday or weekend day.
FilledDS$date <- as.Date(FilledDS$date)
FilledDS$day=ifelse(as.POSIXlt(FilledDS$date)$wday%%6==0,"weekend","weekday")
FilledDS$day=factor(FilledDS$day,levels=c("weekday","weekend"))
head(FilledDS)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average
#number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub 
#repository to see an example of what this plot should look like using simulated data.

library(lattice)
xyplot(steps~interval|factor(day),data=FilledDS,type="l")


