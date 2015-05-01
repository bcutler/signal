#' Cohort
#'
#' Groups users into cohorts of n days, based on the day they signed up. Also calculates
#' the number of users who performed an event for the first time in each cohort.
#'
#' @param df a dataframe containing user ids, cohort dates (e.g. signup date), and events dates
#' @param days number of days in each period (typically 7, 14, or 28)
#' @param periods the number of periods in each cohort
#'
#' @import plyr reshape
#'
#' @keywords cohort
#' @export
#' @examples
#' cohort(df, days=7, periods=4)
cohort <- function(df, days=7, periods=4) {
	df <- df[, 1:3]
	names(df) <- c('id', 'cohort.date', 'event.date')
	
	# Create cohorts
	end.date <- max(df$event.date, na.rm=TRUE)
	df$cohort <- floor(as.numeric(end.date - df$cohort.date)/days)
	df$cohort <- end.date - (days * df$cohort) - 6

	# Calculate cohort size and number of events per cohort
	dat.cohort.size <- ddply(df, .(cohort), function(df) {
		return(data.frame(
			cohort.size=length(unique(df$id))
		))
	}, .progress="text")
	
	# Calculate the number of events for each user in each cohort
	dat <- ddply(subset(df, !is.na(event.date)), .(id), function(df) {
		total.events <- 0
		first.event.period <- 0

		res <- matrix(ncol=periods, nrow=1)
		for (i in 1:periods) {
			start.date <- df$cohort.date + days * (i-1)
			end.date <- df$cohort.date + (days * i)

			num.events <- nrow(subset(df, event.date >= start.date & event.date < end.date))
			res[, i] <- num.events

			if (total.events == 0 & num.events > 0){
				first.event.period <- i
			}
			total.events = total.events + num.events
		}

		res <- data.frame(res)
		res$cohort = df$cohort[1]
		res$first.event.period <- first.event.period
		return (res)
	}, .progress="text")
	
	# Flatten into panel data format
	dat <- melt(dat, id=c('id', 'cohort', 'first.event.period'))
	dat$variable <- as.numeric(substr(dat$variable, 2, 3))
	names(dat) <- c('id', 'cohort', 'first.event.period', 'period', 'events')

	# Add binary variable for whether the first event is in the cohort
	dat$first.event <- (dat$first.event.period == dat$period)
	dat <- subset(dat, select=c('cohort', 'period', 'id', 'events', 'first.event'))

	# Calculate the number of unique user with events in each cohort
	dat <- ddply(dat, .(cohort, period), function(df) {
		return(data.frame(
			users=nrow(subset(df, events>0)), 
			first.time.users=nrow(subset(df, first.event==TRUE))
		))
	}, .progress="text")

	# Merge cohort data with cohort size data
	dat <- merge(dat, dat.cohort.size, by='cohort')
	dat <- subset(dat, select=c('cohort', 'period', 'cohort.size', 'users', 'first.time.users'))

	# Remove data from incomplete cohorts
	idx <- 1
	max.date <- max(dat$cohort)
	for (i in idx:periods) {
		dat$users[dat$cohort==max.date][i:periods] <- NA
		dat$first.time.users[dat$cohort==max.date][i:periods] <- NA
		idx <- idx + 1
		max.date <- max(dat$cohort[dat$cohort < max.date])
	}

	# Calculate percent in each cohort
	dat$perc <- round(dat$users/dat$cohort.size, 4)
	dat$perc.first.time <- round(dat$first.time.users/dat$cohort.size, 4)

	return(dat)
}
