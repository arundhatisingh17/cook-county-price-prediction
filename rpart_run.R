
library(rpart)
library(rpart.plot)

train <- read.table("cook_county_train_v3.txt", 
                    header = TRUE, na.strings = "NA", quote = "\"")
test  <- read.table("cook_county_test_v3.txt",  
                    header = TRUE, na.strings = "NA", quote = "\"")

for (col in names(train)) {
  if (is.logical(train[[col]])) train[[col]] <- as.numeric(train[[col]])
}
for (col in names(test)) {
  if (is.logical(test[[col]])) test[[col]] <- as.numeric(test[[col]])
}

to_drop <- c("Sale.Date")
train <- train[, !(names(train) %in% to_drop)]
test  <- test[,  !(names(test)  %in% to_drop)]

cat("Train:", nrow(train), "rows x", ncol(train), "columns\n\n")
fit <- rpart(log.Sale.Price ~ ., 
             data = train, 
             method = "anova",
             control = rpart.control(cp = 0.001, xval = 5, minsplit = 20))

cat("Tree leaves:", sum(fit$frame$var == "<leaf>"), "\n\n")
printcp(fit)

# Plot the tree
png("rpart_tree.png", width = 2000, height = 1200, res = 130)
rpart.plot(fit, type = 2, extra = 101, fallen.leaves = TRUE,
           main = "rpart Regression Tree (unpruned)")
dev.off()

pred <- predict(fit, newdata = test)
y    <- test$log.Sale.Price
mse  <- mean((pred - y)^2)

cat("\nLog MSE: ",  round(mse, 4), "\n")
cat("Log RMSE:", round(sqrt(mse), 4), "\n")
cat("Log R²:  ", round(1 - mse / var(y), 4), "\n")
