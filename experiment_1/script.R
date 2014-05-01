setwd("..")

library("neuralnet")

stores <- read.csv("data/stores.csv", header=TRUE)
features <- read.csv("data/features.csv", header=TRUE)
train <- read.csv("data/train.csv", header=TRUE)
merged <- merge(stores, features)
merged <- merge(merged, train)


net <- neuralnet(Weekly_Sales~Store+Date+isHoliday,  train_data,  hidden = 17, threshold = 0.005,
	stepmax = 1000000, rep = 1, startweights = NULL,
	learningrate.limit = NULL, learningrate.factor = NULL, learningrate=0.1,
	lifesign = "full", lifesign.step = 1000,
	algorithm = "rprop+",
	err.fct = "sse", act.fct = "logistic",
	linear.output = TRUE, exclude = NULL,
	constant.weights = NULL, likelihood = FALSE)

test <- read.csv("data/test.csv", header=TRUE)