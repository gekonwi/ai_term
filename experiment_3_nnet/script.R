#install.packages("nnet")
library("nnet")

# passing through missing data
options(na.action = "na.pass")

# functions -------------------------------------------------

# normalizing 
normalize <- function(x) {
	x <- sweep(x, 2, apply(x, 2, min))
	sweep(x, 2, apply(x, 2, max), "/") 
}

dateFun <- function(x) {
	z <- paste(as.numeric(format(as.Date(x), "%Y")),"-01-01", sep="")
	as.numeric(difftime(as.Date(x, "%Y-%m-%d"), as.Date(z)))+1
}

# /functions -------------------------------------------------

# read in and merge all the available training data
stores <- read.csv("../data/stores.csv", header=TRUE)
features <- read.csv("../data/features.csv", header=TRUE)
train <- read.csv("../data/train.csv", header=TRUE)
train_merged <- merge(stores, features)
train_merged <- merge(train_merged, train)

# transform the Date values into numeric values between 0 (representing "01/01/XXXX") and 1 (representing "12/31/XXXX") - ignoring the year, assuming similar behavior every year.
train_merged[c('Date')] <- lapply(train_merged[c('Date')], dateFun)

# create a unique ID for each store-department pair
#train_merged <- transform(train_merged, Store_Dep = paste(Store * max(Dept) + Dept, "_", sep=""))
train_merged <- transform(train_merged, Store_Dep = Store * max(Dept) + Dept)


# normalize all numeric numbers columns
num_col_names <- c('Size', 'Temperature', 'Fuel_Price', 'MarkDown1', 'MarkDown2', 'MarkDown3', 'MarkDown4', 'MarkDown5', 'CPI', 'Unemployment')
train_merged[num_col_names] <- normalize(train_merged[num_col_names])

# replace all NA values by zeros (nnet can only omit whole records with NA values)
train_merged[is.na(train_merged)] <- 0

#net <- nnet(Weekly_Sales ~ Date + IsHoliday + Type + Size + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + Store_Dep, 
#	data = train_merged,  size = 40, decay = 5e-4, maxit = 50)
net <- nnet(Weekly_Sales ~ Date + IsHoliday + Type + Size + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + Store_Dep, 
	data = train_merged,  size = 40, abstol = 5e-4, maxit = 5000)


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
test_net_data <- model.matrix(~Type + Size + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + IsHoliday, 
	data = test_merged)
a <- train_net_data[, -1] # delete the generated "(Intercept)" column containing only "1"

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