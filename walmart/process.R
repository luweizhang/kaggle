
library(forecast)

setwd("/home/willie/data_science/kaggle/walmart")
training_data <- read.csv("raw_data/train.csv")

sub <- read.csv("raw_data/sampleSubmission.csv",stringsAsFactors=FALSE)

#function to determine optimal frequency
optimal_freq <- function(x) {
	x2 <- x/2
	if (x2 >= 52) {answer <- 52}
	if (x2 >= 26 && x2 < 52) {answer <- 26}
	if (x2 < 26 && x >= 13 ) {answer <- 13}
	if (x2 < 13) {answer <- 2}
	if (x2 < 2) {answer <- 1}
	answer
}

######make sure it works for one
x <- 34 #store
y <- 39 #dept

forecast.length <- dim(subset(sub, store == x & dept == y))[1] #find the number of days to forecast

mydata <- subset(training_data, Store == x & Dept == y) #grab the training dat for store = 1 and department = 1 

opt_freq <- optimal_freq(dim(mydata)[1])

tryCatch({

mydata.ts <- ts(mydata$Weekly_Sales, start = c(2010,1), frequency = opt_freq) #convert to time series
mydata.forecast <- HoltWinters(mydata.ts) #apply HW
mydata.forecast <- forecast.HoltWinters(mydata.forecast, h = forecast.length) #apply HW forecast
#plot(mydata.forecast)
outputdata <- as.matrix(as.data.frame(mydata.forecast[4])) #convert to matrix format

}, error = function(e) {
    	outputdata <<- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
	"dead"
}, warning = function(w) { 
	outputdata <<- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
	"warn"
}
)

#####now do it for a whole department
finaldf <- c()
for (y in unique(subset(sub, store == 2)$dept)) { #all departments for a store
	forecast.length <- dim(subset(sub, store == 2 & dept == y))[1] #find the number of days to forecast
	mydata <- subset(training_data, Store == 2 & Dept == y) #grab the training dat for store = 1 and department = 1 
	opt_freq <- optimal_freq(dim(mydata)[1])
	if (opt_freq == 1) {outputdata <- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))}
	else {
	tryCatch({
		mydata.ts <- ts(mydata$Weekly_Sales, start = c(2010,1), frequency = opt_freq) #convert to time series
		mydata.forecast <- HoltWinters(mydata.ts) #apply HW
		mydata.forecast <- forecast.HoltWinters(mydata.forecast, h = forecast.length) #apply HW forecast
		#plot(mydata.forecast)
		outputdata <- as.matrix(as.data.frame(mydata.forecast[4])) #convert to matrix format

		}, error = function(e) {
		    outputdata <- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
		"dead"
		}, warning = function(w) { 
		outputdata <- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
		"warn"
		}
		)
	}
	finaldf <- rbind(finaldf, cbind(outputdata,x,y))
}

###now do it for all the stores
superfinaldf <- c()

for (x in 1:45) {
	finaldf <- c()
		for (y in unique(subset(sub, store == x)$dept)) { #all departments for a store
			forecast.length <- dim(subset(sub, store == x & dept == y))[1] #find the number of days to forecast
			mydata <- subset(training_data, Store == x & Dept == y) #grab the training dat for store = 1 and department = 1 
			opt_freq <- optimal_freq(dim(mydata)[1])
		if (opt_freq == 1) {outputdata <- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
	} else {
	tryCatch({
		mydata.ts <- ts(mydata$Weekly_Sales, start = c(2010,1), frequency = opt_freq) #convert to time series
		mydata.forecast <- HoltWinters(mydata.ts) #apply HW
		mydata.forecast <- forecast.HoltWinters(mydata.forecast, h = forecast.length) #apply HW forecast
		#plot(mydata.forecast)
		outputdata <- as.matrix(as.data.frame(mydata.forecast[4])) #convert to matrix format
	}, error = function(e) {
	    	outputdata <<- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
		"dead"
	}, warning = function(w) { 
		outputdata <<- cbind(rep(mean(mydata$Weekly_Sales), forecast.length))
		"warn"
	}
	)
	}
	finaldf <- rbind(finaldf, cbind(outputdata,x,y))

	}
	superfinaldf <- rbind(superfinaldf, finaldf)
}
strsplit(sample_sub$Id[60],"_")[[1]][2]

###extra stuff
unique(sub$store) #list of stores
unique(subset(sub, store == 1)$dept) #list of departments given a store

#number of days to forcast
length(unique(subset(training_data, Store == 1)$Dept))
length(unique(subset(sub, store == 1)$dept))



