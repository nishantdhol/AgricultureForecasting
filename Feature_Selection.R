#Generating Formula for linear regression equation
View(scaled_data)
model <- lm(Yield_Generated ~ .-crop_name,data = scaled_data)
allvifs <- car::vif(model)
car::vif(model)

#Checking statistical significance and multicollinearity
all_vars <- names(model[[1]])[-1]  # names of all X variables
# Get the non-significant vars
summ <- summary(model)  # model summary
pvals <- summ[[4]][, 4]  # get all p values
not_significant <-
  character()  # init variables that aren't statsitically significant
not_significant <- names(which(pvals > 0.1))
not_significant <-
  not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!

# If there are any non-significant variables,
while (length(not_significant) > 0) {
  all_vars <- all_vars[!all_vars %in% not_significant[1]]
  myForm <-
    as.formula(paste("Yield_Generated ~ ", paste (all_vars, collapse = " + "), sep =
                       ""))  # new formula
  model <-
    lm(myForm, data = scaled_data)  # re-build model with new formula
  
  # Get the non-significant vars.
  summ <- summary(model)
  pvals <- summ[[4]][, 4]
  not_significant <- character()
  not_significant <- names(which(pvals > 0.1))
  not_significant <-
    not_significant[!not_significant %in% "(Intercept)"]
}
summary(model)

#Correleation Between Attributes
library(corrplot)
df_correlation <-as.data.frame(scaled_data)
df_correlation <- df_correlation[, sapply(df_correlation, is.numeric)]
View(df_correlation)
corrplot(
  cor(df_correlation),
  method = "pie",
  type = "upper",
  details = FALSE
)


#Creating the Train & Test Data
testmodel <- scaled_data[sample(nrow(scaled_data),1), ]
check <-testmodel$Yield_Generated
testmodel$Yield_Generated <- NULL
View(testmodel)
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <-
  sample(1:nrow(scaled_data), 0.8 * nrow(scaled_data))  # row indices for training data
trainingData <-
  scaled_data1[trainingRowIndex,]  # model training data
testData  <- scaled_data1[-trainingRowIndex,]
# describe(trainingData)
# colnames(trainingData)

#Linear Regression
fit1 <- lm(Yield_Generated ~ ., data = scaled_data)
#Stepwise Regression
selectedfit1 <- step(fit1)
# trainingData1 <- filter(trainingData,crop_name == "Cotton")
# dim(traingData1)
View(trainingData)
summary(trainingData)
# Build the model on training data -
fit1 <- lm(Yield_Generated ~ . - crop_name, data = trainingData)
Pred <- predict(fit1, testData)
par(mar = c(1, 1, 1, 1))
plot(Pred)
summary(fit1)
AIC(fit1)

#Finding Correlation Between Actual Data & Predicted Data
actualacc <-
  data.frame(cbind(actuals = testData$Yield_Generated, Predicteds = Pred))
correlation_accuracy <- cor(actualacc)
correlation_accuracy

# selectedfit <- step(fit1)
# summary(selectedfit)

predictions <-  fit1 %>% predict(testData)
# Model performance
data.frame(
  RMSE = rmse(predictions, testData$Yield_Generated),
  R2 = R2(predictions, testData$Yield_Generated)
)

