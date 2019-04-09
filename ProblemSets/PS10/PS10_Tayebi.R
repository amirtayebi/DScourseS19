#Loading required packages

library("rpart")
library("e1071")
library("kknn")
library("nnet")
library("mlr")
library("glmnet")

#Importing the data
set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")


#Cleaning the dataset
# Drop unnecessary columns

income$native.country <- NULL
income$fnlwgt <- NULL

# Make sure continuous variables are coded as such

income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels

levels(income$education)      <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race)           <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass)      <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation)     <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:

n            <- nrow(income)
train        <- sample(n, size = .8*n)
test         <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#Creating objects
#Definning the task

the.task <- makeClassifTask(data = income.train, target = "high.earner")

# Setting resampling strategy 

resample.strat <- makeResampleDesc(method = "CV", iters = 3)

# Taking 10 ransom guesses at lambda within the interval specified above

tune.method <- makeTuneControlRandom(maxit = 10L)

# Creating each of the six learners

alg.tree  <- makeLearner("classif.rpart", predict.type = "response")
alg.lreg  <- makeLearner("classif.glmnet", predict.type = "response")
alg.nn    <- makeLearner("classif.nnet", predict.type = "response")
alg.bayes <- makeLearner("classif.naiveBayes", predict.type = "response")
alg.knn   <- makeLearner("classif.kknn", predict.type = "response")
alg.svm   <- makeLearner("classif.svm", predict.type = "response")


#Setting up hyperparameters


paramstree <- makeParamSet(makeIntegerParam("minsplit",lower = 10, upper = 50), 
                            makeIntegerParam("minbucket", lower = 5, upper = 50), 
                            makeNumericParam("cp", lower = 0.001, upper = 0.2))
paramslreg <- makeParamSet(makeNumericParam("lambda", lower = 0, upper = 3), 
                           makeNumericParam("alpha", lower = 0, upper = 1))
paramsnn   <- makeParamSet(makeIntegerParam("size", lower = 1, upper = 10), 
                            makeNumericParam("decay", lower = 0.1, upper = 0.5), 
                            makeIntegerParam("maxit", lower = 1000, upper = 1000))
paramsknn  <- makeParamSet(makeIntegerParam("k", lower = 1, upper = 30))
paramssvm  <- makeParamSet(makeDiscreteParam("cost", values = 2^c(2^-2, -1, 0, 1, 2, 10)), 
                            makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))


Tunning the models


tuned.tree    <- tuneParams(learner = alg.tree,
                            task = the.task,
                            resampling = resample.strat,
                            measures = list(f1, gmean),
                            par.set = paramstree,
                            control = tune.method,
                            show.info = TRUE)
tuned.lreg    <- tuneParams(learner = alg.lreg,
                            task = the.task,
                            resampling = resample.strat,
                            measures = list(f1, gmean),
                            par.set = paramslreg,
                            control = tune.method,
                            show.info = TRUE)
tuned.nn      <- tuneParams(learner = alg.nn,
                            task = the.task,
                            resampling = resample.strat,
                            measures = list(f1, gmean),
                            par.set = paramsnn,
                            control = tune.method,
                            show.info = TRUE)
tuned.knn     <- tuneParams(learner = alg.knn,
                            task = the.task,
                            resampling = resample.strat,
                            measures = list(f1, gmean),
                            par.set = paramsknn,
                            control = tune.method,
                            show.info = TRUE)
tuned.svm     <- tuneParams(learner = alg.svm,
                            task = the.task,
                            resampling = resample.strat,
                            measures = list(f1, gmean),
                            par.set = paramssvm,
                            control = tune.method,
                            show.info = TRUE)



#Getting the results



# Applying the optimal algorithm parameters to the model

alg.tree <- setHyperPars(learner = alg.tree, par.vals = tuned.tree$x)
alg.lreg <- setHyperPars(learner = alg.lreg, par.vals = tuned.lreg$x)
alg.nn   <- setHyperPars(learner = alg.nn, par.vals = tuned.nn$x)
alg.knn  <- setHyperPars(learner = alg.knn, par.vals = tuned.knn$x)
alg.svm <- setHyperPars(learner = alg.svm, par.vals = tuned.svm$x)


# Resampling each learner

resample.tree  <- resample(learner = alg.tree, task = the.task, resampling = resample.strat, measures=list(f1, gmean))
resample.lreg  <- resample(learner = alg.lreg, task = the.task, resampling = resample.strat, measures=list(f1, gmean))
resample.nn    <- resample(learner = alg.nn, task = the.task, resampling = resample.strat, measures=list(f1, gmean))
resample.bayes <- resample(learner = alg.bayes, task = the.task, resampling = resample.strat, measures=list(f1, gmean))
resample.knn   <- resample(learner = alg.knn, task = the.task, resampling = resample.strat, measures=list(f1, gmean))
resample.svm   <- resample(learner = alg.svm, task = the.task, resampling = resample.strat, measures=list(f1, gmean))


# Training the final model 

final.tree  <- train(learner = alg.tree, task = the.task)
final.lreg  <- train(learner = alg.lreg, task = the.task)
final.nn    <- train(learner = alg.nn, task = the.task)
final.bayes <- train(learner = alg.bayes, task = the.task)
final.knn   <- train(learner = alg.knn, task = the.task)
final.svm   <- train(learner = alg.svm, task = the.task)


# Predicting in test set

prediction.tree  <- predict(final.tree, newdata = income.test)
prediction.lreg  <- predict(final.lreg, newdata = income.test)
prediction.nn    <- predict(final.nn, newdata = income.test)
prediction.bayes <- predict(final.bayes, newdata = income.test)
prediction.knn   <- predict(final.knn, newdata = income.test)
prediction.svm   <- predict(final.svm, newdata = income.test)


# Out of sample f1 and gmean

print(performance(prediction.tree, measures = list(f1, gmean)))
print(performance(prediction.lreg, measures = list(f1, gmean)))
print(performance(prediction.nn, measures = list(f1, gmean)))
print(performance(prediction.bayes, measures = list(f1, gmean)))
print(performance(prediction.knn, measures = list(f1, gmean)))
print(performance(prediction.svm, measures = list(f1, gmean)))
