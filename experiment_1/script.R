library("neuralnet")

stores <- read.csv("data/stores.csv", header=TRUE)
features <- read.csv("data/features.csv", header=TRUE)
train <- read.csv("data/train.csv", header=TRUE)
merged <- merge(stores, features)
merged <- merge(merged, train)

# add an id column as a combination of store and department
store_dep <- paste(merged[, 'Store'], merged[, 'Dept'], sep = "-")
merged <- cbind(store_dep, merged)

# turn the qualitative columns into binary quantitative columns
# e.g. Type: {A, B, A, B, B}
# becomes: TypeA: {1, 0, 1, 0, 0}, TypeB: {0, 1, 0, 1, 1}
bin_store_dep <- model.matrix(~-1 + store_dep, data = merged)
bin_date <- model.matrix(~-1 + Date, data = merged)
bin_is_holiday <- model.matrix(~-1 + IsHoliday, data = merged)
bin_type <- model.matrix(~-1 + Type, data = merged)


net <- neuralnet(Weekly_Sales~Store+Date+isHoliday,  train_data,  hidden = 17, threshold = 0.005,
	stepmax = 1000000, rep = 1, startweights = NULL,
	learningrate.limit = NULL, learningrate.factor = NULL, learningrate=0.1,
	lifesign = "full", lifesign.step = 1000,
	algorithm = "rprop+",
	err.fct = "sse", act.fct = "logistic",
	linear.output = TRUE, exclude = NULL,
	constant.weights = NULL, likelihood = FALSE)

test <- read.csv("data/test.csv", header=TRUE)