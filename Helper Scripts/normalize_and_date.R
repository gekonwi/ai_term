# 
A <- function(x) as.numeric(difftime(as.Date(x, "%Y-%m-%d"),as.Date("1/1/1904", "%m/%d/%Y")))

train2 <- cbind(train[1:2], lapply(train[3], A),  train[4:5])



# normalizing 
normalize <- function(x) { 
   x <- sweep(x, 2, apply(x, 2, min)) 
   sweep(x, 2, apply(x, 2, max), "/") 
}
# This is just for temp and fuel price, select more if needed (we need it)
temp <- cbind(features[1:2],normalize(features[3:4]))