library("neuralnet")

# passing through missing data
options(na.action = "na.pass")

# functions -------------------------------------------------

# normalizing 
normalize <- function(x) {
	x <- x - min(x, na.rm = TRUE)
	x <- x / max(x, na.rm = TRUE)
}

dateFun <- function(x) {
	z <- paste(as.numeric(format(as.Date(x), "%Y")),"-01-01", sep="")
	as.numeric(difftime(as.Date(x, "%Y-%m-%d"), as.Date(z)))+1
}

# filling in missing vals
Fill <- function(col){
	if (is.na(col[1])) {
		col[1] <- NextNotNA(1, col)
		lastVal <- col[1]
	}
	
	rowCount <- length(col)
	for(i in 2:(rowCount - 1)){		
		if (i %% 10000 == 1)
			print(paste("filled", i, "of", rowCount))
		
		if(!is.na(col[i])) {
			lastVal <- col[i]
			next			
		}
		
		nextVal <- NextNotNA(i, col)

		col[i] <- (lastVal + nextVal)/2
		lastVal <- col[i]
	}
	
	if (is.na(col[rowCount]))
		col[rowCount] <- lastVal
	
	col
}

NextNotNA <- function(i,col){
	for(j in i+1:length(col)){
		if(!is.na(col[j]))
			return(col[j])
	}
	return(mean(col, na.rm = TRUE))
}

# temp <- cbind(train[1:2],lapply(train[3],DateFun))


# /functions -------------------------------------------------

# read in and merge all the available training data
stores <- read.csv("../data/stores.csv", header=TRUE)
features <- read.csv("../data/features.csv", header=TRUE)
train <- read.csv("../data/train.csv", header=TRUE)
features_merged <- merge(stores,features)

# transform the Date values into numeric values between 0 (representing "01/01/XXXX") and 1 (representing "12/31/XXXX") - ignoring the year, assuming similar behavior every year.
features_merged[c('Date')] <- lapply(features_merged[c('Date')], dateFun)

features_temp <- features_merged[with(features_merged,order(IsHoliday, Type, Size, Store, Date, Unemployment, Fuel_Price, CPI, Temperature)),]

features_merged <- features_temp
features_merged$MarkDown2 <- Fill(features_merged$MarkDown2)
features_merged$MarkDown3 <- Fill(features_merged$MarkDown3)
features_merged$MarkDown4 <- Fill(features_merged$MarkDown4)
features_merged$MarkDown5 <- Fill(features_merged$MarkDown5)
features_merged$MarkDown1 <- Fill(features_merged$MarkDown1)

train_merged <- merge(stores, features_merged)

# transform the Date values in train as well (for merging)
train[c('Date')] <- lapply(train[c('Date')], dateFun)
train_merged <- merge(train_merged, train)

train_merged <- subset(train_merged, Store <= 5)


# create a unique ID for each store-department pair
train_merged <- transform(train_merged, Store_Dep = Store * max(Dept) + Dept)

# Holiday TRUE/FALSE to 1/0
Holiday <- function(x) {
	replace(x, x == TRUE, 1)
	replace(x, x == FALSE, 0)
}

train_merged[c('IsHoliday')] <- lapply(train_merged[c('IsHoliday')], Holiday)

# Type A/B/C to 1/2/3

levels(train_merged$Type) <- c(levels(train_merged$Type), "1")
train_merged$Type[train_merged$Type == 'A'] <- '1'

levels(train_merged$Type) <- c(levels(train_merged$Type), "2")
train_merged$Type[train_merged$Type == 'B'] <- '2'

levels(train_merged$Type) <- c(levels(train_merged$Type), "3")
train_merged$Type[train_merged$Type == 'C'] <- '3'

train_merged[c('Type')] <- as.numeric(as.character(train_merged$Type))

# normalize all numeric numbers columns
num_col_names <- c('Size', 'Temperature', 'Fuel_Price', 'MarkDown1', 'MarkDown2', 'MarkDown3', 'MarkDown4', 'MarkDown5', 'CPI', 'Unemployment')
train_merged[num_col_names] <- normalize(train_merged[num_col_names])

net <- neuralnet(Weekly_Sales ~ Date + IsHoliday + Type + Size + Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + Store_Dep, 
	train_merged,  hidden = 10, threshold = 0.005,
	stepmax = 1000000, rep = 1, startweights = NULL,
	learningrate.limit = NULL, learningrate.factor = NULL, learningrate=0.1,
	lifesign = "full", lifesign.step = 100,
	algorithm = "rprop+",
	err.fct = "sse", act.fct = "logistic",
	linear.output = TRUE, exclude = NULL,
	constant.weights = NULL, likelihood = FALSE)

test <- read.csv("../data/test.csv", header=TRUE)

test_merged <- merge(stores, test)
test_merged <- subset(test_merged, Store <= 5)

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

# Store: 45
# Date 143
# IsHoliday: 2
# Type: 3
# Size: 40
# Temperature: 3528
# Fuel_Price: 892
# MarkDown1: 2278
# MarkDown2: 1500
# MarkDown3: 1663
# MarkDown4: 1945
# MarkDown5: 2294
# CPI: 2145
# Unemployment: 349