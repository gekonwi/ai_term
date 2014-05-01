library("neuralnet")

# passing through missing data
options(na.action = "na.pass")

stores <- read.csv("../data/stores.csv", header=TRUE)
features <- read.csv("../data/features.csv", header=TRUE)
train <- read.csv("../data/train.csv", header=TRUE)
train_merged <- merge(stores, features)
train_merged <- merge(train_merged, train)

# turn the qualitative columns into binary quantitative columns
# e.g. Type: {A, B, A, B, B}
# becomes: TypeA: {1, 0, 1, 0, 0}, TypeB: {0, 1, 0, 1, 1}
train_net_data <- model.matrix(~-1 + Type + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHoliday + Weekly_Sales, 
	data = train_merged)

net <- neuralnet(Weekly_Sales ~ TypeA + TypeB + TypeC + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHolidayTRUE, 
	train_net_data,  hidden = 5, threshold = 0.005,
	stepmax = 1000000, rep = 1, startweights = NULL,
	learningrate.limit = NULL, learningrate.factor = NULL, learningrate=0.1,
	lifesign = "full", lifesign.step = 1000,
	algorithm = "rprop+",
	err.fct = "sse", act.fct = "logistic",
	linear.output = TRUE, exclude = NULL,
	constant.weights = NULL, likelihood = FALSE)

test <- read.csv("../data/test.csv", header=TRUE)

test_merged <- merge(stores, test)


# the network was trained knowing features like Fuel_Price and MarkDown2
# the test data does not provide these features so we handle them as missing data
missing_col_names <- colnames(train_net_data)
missing_col_names <- missing_col_names[missing_col_names != "Weekly_Sales"]
for (col_name in colnames(test_merged))
	missing_col_names <- missing_col_names[missing_col_names != col_name]
for (col_name in missing_col_names)
	test_merged[, col_name] <- NA

# turn the qualitative columns into binary quantitative columns
# e.g. Type: {A, B, A, B, B}
# becomes: TypeA: {1, 0, 1, 0, 0}, TypeB: {0, 1, 0, 1, 1}
test_net_data <- model.matrix(~-1 + Type + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHoliday, 
	data = test_merged)

Id <- paste(test[, 'Store'], test[, 'Dept'], test[, 'Date'], sep = "_")
prediction <- compute(net, test_net_data, rep = 1)
Weekly_Sales <- prediction$net.result

result <- cbind(Id, Weekly_Sales)
write.csv(result, file ="result.csv", row.names=FALSE)

# log in to diadem
# ssh gekonwi@129.64.2.200

# on UNix run this:
# cd ~/Documents/ai_term
# export R_LIBS="~/myRdir"