library("neuralnet")

stores <- read.csv("data/stores.csv", header=TRUE)
features <- read.csv("data/features.csv", header=TRUE)
train <- read.csv("data/train.csv", header=TRUE)
merged <- merge(stores, features)
merged <- merge(merged, train)

# turn the qualitative columns into binary quantitative columns
# e.g. Type: {A, B, A, B, B}
# becomes: TypeA: {1, 0, 1, 0, 0}, TypeB: {0, 1, 0, 1, 1}
net_data <- cbind(model.matrix(~-1 + Type + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHoliday + Weekly_Sales, data = merged)

net <- neuralnet(Weekly_Sales ~ TypeA + TypeB + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHolidayTRUE, net_data,  hidden = 17, threshold = 0.005,
	stepmax = 1000000, rep = 1, startweights = NULL,
	learningrate.limit = NULL, learningrate.factor = NULL, learningrate=0.1,
	lifesign = "full", lifesign.step = 1000,
	algorithm = "rprop+",
	err.fct = "sse", act.fct = "logistic",
	linear.output = TRUE, exclude = NULL,
	constant.weights = NULL, likelihood = FALSE)

# test <- read.csv("data/test.csv", header=TRUE)