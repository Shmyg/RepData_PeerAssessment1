## ----loadstats------------------------------------------------------------
Sys.setlocale('LC_ALL', 'en_US.UTF8')
url <- 'https://d396qusza40orc.cloudfront.net/repstats%2Fstats%2Factivity.zip'
zipfile <- 'activity.zip'

# Reaging and parsing the file
# Checking if the file has already been downloaded
if (!file.exists(zipfile)) {
	# Not yet
	download.file (url, zipfile, method = 'curl')
}

unzip(zipfile)
stats <- read.csv("activity.csv")


## Part 1
library('dplyr')
stats <- transform(stats, date = factor(date))
days <- group_by (stats, date)
df <- summarize(days, steps = sum(steps))
hist(df$steps,
	main = 'Histogram of total number of steps per day',
	xlab = 'Number of steps')

mean(df$steps)
median(df$steps)

## Part 2
library('sqldf')
avgPerInterval <- sqldf('select interval, avg(steps) as steps from stats group by interval')
plot (avgPerInterval, type = 'h')
avgPerInterval[which.max(avgPerInterval$steps),]


## Part 3
sum(is.na(stats$steps))

completeData <- stats
for (i in 1:nrow(completeData)) { 
	if ( is.na(completeData[i,]$steps )) {
		completeData[i,]$steps <- avgPerInterval[avgPerInterval$interval == completeData[i,]$interval, "steps"]
	}
}
days <- group_by (completeData, date)
result <- summarize(days, steps = sum(steps))

hist(result$steps,
	main = 'Histogram of total number of steps per day',
	xlab = 'Number of steps')

mean(df$steps)
median(df$steps)


## Part 4
library('ggplot2')
for (i in 1:nrow(completeData)) { 
	day <- weekdays(as.Date(completeData[i, "date"]))
	if (day %in% c('Saturday', 'Sunday')) {
		completeData[i, "day"] <- 'Weekend'
	}
	else {
		completeData[i, "day"] <- 'Weekday'
	}
}

completeData <- transform(completeData, day = factor(day))
avgPerInterval5 <- sqldf('select interval, day, avg(steps) as steps from completeData group by interval, day')
ggplot(avgPerInterval5, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
