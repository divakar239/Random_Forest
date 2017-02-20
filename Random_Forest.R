# Random Forest Classification
#install.packages("mice")
library(mice)
library(caret)
# Importing the dataset
dataset<-read.csv("/Users/DK/Desktop/U of T hackathon/training_data.csv", header=TRUE)
testset<-read.csv("/Users/DK/Desktop/U of T hackathon/test_data.csv", header=TRUE)
#dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$num = factor(dataset$num, levels = c(0,1,2,3,4))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)

#Encoding the dataset
dataset$fbs<-as.numeric(as.character(dataset$fbs))
dataset$fbs[(dataset$fbs<0) | (dataset$fbs>1)]<-NA

dataset$sex<-as.numeric(as.character(dataset$sex))
dataset$sex[(dataset$sex<0) | (dataset$sex>1)]<-NA

dataset$cp<-as.numeric(as.character(dataset$cp))
dataset$cp[(dataset$cp<1) | (dataset$cp>4)]<-NA

dataset$exang<-as.numeric(as.character(dataset$exang))
dataset$exang[(dataset$exang<0) | (dataset$exang>1)]<-NA


dataset$slope<-as.numeric(as.character(dataset$slope))
dataset$slope[(dataset$slope<1) | (dataset$slope>3)]<-NA


dataset$thal<-as.numeric(as.character(dataset$thal))
dataset$thal[(dataset$thal!=3) & (dataset$thal!=7) & (dataset$thal!=6) ]<-NA


#Proper Naming
dataset$trestbps<-as.numeric(dataset$trestbps)
dataset$chol<-as.numeric(dataset$chol)
dataset$thalach<-as.numeric(dataset$thalach)
dataset$oldpeak<-as.numeric(dataset$oldpeak)

dataset$sex <- as.numeric(factor(dataset$sex))
levels(dataset$sex) <- c("female", "male",NA)

dataset$cp <- as.numeric(factor(dataset$cp))
levels(dataset$cp) <- c("typical","atypical","non-anginal","asymptomatic",NA)

dataset$fbs <- as.numeric(factor(dataset$fbs))
levels(dataset$fbs)<-c("false","true",NA)

dataset$restecg <- as.numeric(factor(dataset$restecg))
levels(dataset$restecg) <- c("normal","stt","hypertrophy",NA)

dataset$exang <- as.numeric(factor(dataset$exang))
levels(dataset$exang) <- c("no","yes",NA)

dataset$slope<-as.numeric(factor(dataset$slope))
levels(dataset$slope) <- c("upsloping","flat","downsloping",NA)

dataset$ca <- as.numeric(factor(dataset$ca)) # not doing level conversion because its not necessary)

dataset$thal <- as.numeric(factor(dataset$thal))
levels(dataset$thal) <- c("normal","fixed","reversable",NA)

#dataset$num <- as.numeric(factor(dataset$num)) # not doing level conversion because its not necessary

#Filling all the NA
for(i in 1:ncol(dataset)){
  dataset[is.na(dataset[,i]),i]<- mean(dataset[,i], na.rm = TRUE)
}


#Splitting dataset
split = sample.split(dataset$num, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set$trestbps = scale(training_set$trestbps)
test_set$trestbps = scale(test_set$trestbps)

training_set$chol = scale(training_set$chol)
test_set$chol = scale(test_set$chol)

training_set$thalach = scale(training_set$thalach)
test_set$thalach = scale(test_set$thalach)



# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')

#lmtry=floor(sqrt(ncol(training_set[,2:14])))
#set.seed(123)
#mtry <- tuneRF(training_set[,2:14], training_set[,15], stepFactor=1.5, improve=1e-5, ntree=500)

library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[,2:14], #training_set['sex']+training_set['age']+training_set['cp']+training_set['exang']+training_set['oldpeak'],
                          y = training_set$num,
                          ntree = 3500,
                          mtry=19,
                          nodesize=30
                           )

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,2:14])

# Making the Confusion Matrix
cm = table(test_set[, 15], y_pred)

#install.packages("caret")

#install.packages("e1071")
library("e1071")
postResample(y_pred,test_set[,15])

