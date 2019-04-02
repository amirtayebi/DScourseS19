
#Loading required packages
library(mlr)
library(glmnet)


#loading data 
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

#checking data names 
names(housing)

#replacing data names 
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

#checking data names again to verify changes 
names(housing)

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    % lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

#Question 5
housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula<- as.formula(lmedv ~ .^3 +
  poly(crim,6) +
  poly(zn,6) +
  poly(indus,6) +
  poly(nox,6) +
  poly(rm,6) +
  poly(age,6) +
  poly(dis,6) +
  poly(rad,6) +
  poly(tax,6) +
  poly(ptratio,6) +
  poly(b,6) +
  poly(lstat,6))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix <- data.frame(model.matrix(formula,housing))

mod_matrix [,1] = housing$lmedv

#Question 6 pt. 1 
#In-sample test 
#L1 LASSO Regression 

#rename so MLR will find it 
colnames(mod_matrix)[1] = "lmedv"

#check output 
head(mod_matrix) 

#Breaking up the data:
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
train.a<-as.array(train)
dim(train.a)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train,]
housing.test <- mod_matrix[test,]


# Define the task:
theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

#Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

#Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

#Mean RMSE across the 6 folds
print(sampleResults$aggr)

#Question 6 Pt. 2 
#Out-of-sample
#L1 LASSO Regression 

# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

#Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

#Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         # RMSE performance measure, this can be changed to one or many
                         measures = rmse,      
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

#Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

#Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

#Train the final model
finalModel <- train(learner = predAlg, task = theTask)

#Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

#RMSE 
performance(prediction, measures = list(rmse))

print(head(prediction$data))





#Question 7 
#L2 Ridge Regression 

# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

#RMSE 
performance(prediction, measures = list(rmse))

print(prediction)

#Question 8

# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

#RMSE 
performance(prediction, measures = list(rmse))

print(prediction)
