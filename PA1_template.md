Load libraries

    library(plyr)

Code to Read the data

    activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

Code that formats the data into an analyzable table

    activity$date <- as.factor(activity$date)

    #create a new set that has dates formated into a date object
    activity_d <- activity
    activity_d$date <- strptime(activity$date, format = "%Y-%m-%d")

Total Steps taken per day

    step_sum <- aggregate(activity$steps, by = list(activity$date), sum, na.rm = TRUE)
    hist(as.vector(rep(strptime(step_sum$Group.1, format = "%Y-%m-%d"), step_sum$x)), 
         breaks = strptime(step_sum$Group.1, format = "%Y-%m-%d"), 
         freq = TRUE, 
         xlab = "Date", 
         ylab = "Total Number of Steps", 
         main = "Total Steps Taken per Day")

![](PA1_template_files/figure-markdown_strict/total%20steps-1.png)

Mean and median of total number of steps taken per day

    activity_rm <- activity[is.na(activity$steps) == FALSE, ]
    activity_sums <- aggregate(activity_rm$steps, by = list(activity_rm$date), sum)
    summary(activity_sums$x)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    8841   10760   10770   13290   21190

Time series of Number of steps taken in each interval. Each interval is
the average of all days in that interval.

    time_series <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm = TRUE)
    plot(time_series,
         type = "l",
         xlab = "Time (min)",
         ylab = "Average steps taken")

![](PA1_template_files/figure-markdown_strict/time%20series-1.png)

Interval that contains the maximum number of steps

    time_series[which.max(time_series$x), ]

    ##     Group.1        x
    ## 104     835 206.1698

Percentage of NA's

    sum(is.na(activity$steps))/nrow(activity)

    ## [1] 0.1311475

Use mean imputation to impute missing values

    activity_noNA <- activity[is.na(activity$steps) == FALSE, ]
    activity_NA <- activity[is.na(activity$steps) == TRUE, ]
    names(time_series) <- c("interval", "x")
    imputed <- join(time_series, activity_NA, by = 'interval', type = 'right', match = 'all')
    imputed <- data.frame(cbind(imputed$x, as.character(imputed$date), imputed$interval))
    names(imputed) <- names(activity_NA)
    activity_imputed <- data.frame(rbind(activity_noNA, imputed))
    activity_imputed$steps <- as.numeric(activity_imputed$steps)

Total Steps taken per day using imputed data

    step_sum2 <- aggregate(activity_imputed$steps, by = list(activity_imputed$date), sum, na.rm = TRUE)
    hist(as.vector(rep(strptime(step_sum2$Group.1, format = "%Y-%m-%d"), step_sum2$x)), 
         breaks = strptime(step_sum2$Group.1, format = "%Y-%m-%d"), 
         freq = TRUE, 
         xlab = "Date", 
         ylab = "Total Number of Steps", 
         main = "Total Steps Taken per Day with Mean Imputation")

![](PA1_template_files/figure-markdown_strict/total%20steps%20imputed-1.png)

Mean and median of total number of steps taken per day using imputed
data

    activity_sums2 <- aggregate(activity_imputed$steps, by = list(activity_imputed$date), sum)
    summary(activity_sums2$x)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10770   10770   12810   21190

Make a new column indicating whether the day is a weekday or weekend

    day_type <- character()
    activity_imputed$date <- strptime(activity_imputed$date, format = "%Y-%m-%d")
    date_vector <- activity_imputed$date
    nrows <- nrow(activity_imputed)
    for (i in 1:17568) {
      if (weekdays(date_vector)[i] %in% c("Saturday", "Sunday")) {
        day_type[i] <- "weekend"
      } else {
        day_type[i] <- "weekday"
      } 
    }
    activity_imputed$day_type <- day_type
    activity_imputed$day_type <- as.factor(activity_imputed$day_type)

Panel plot for weekday vs weekends

    activity_imputed$steps <- as.numeric(activity_imputed$steps)
    activity_imputed$interval <- as.numeric(activity_imputed$interval)
    par(mfrow = c(2:1))
    weekdays <- subset(activity_imputed, activity_imputed$day_type == "weekday")
    weekends <- subset(activity_imputed, activity_imputed$day_type == "weekend")
    weekdays_agg <- aggregate(weekdays$steps, by = list(weekdays$interval), mean)
    weekends_agg <- aggregate(weekends$steps, by = list(weekends$interval), mean)
    plot(weekdays_agg, main = "Weekdays", type = "l", xlab = "Interval", ylab = "Number of steps")
    plot(weekends_agg, main = "Weekends", type = "l", xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/weekday%20vs%20weekends-1.png)
