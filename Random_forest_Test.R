# Random Forest Classification
library(caret)
#install.packages("readr")

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
  dataset[is.na(dataset[,i]),i]<- 0 #mean(dataset[,i], na.rm = TRUE)
}

testset$fbs<-as.numeric(as.character(testset$fbs))
testset$fbs[(testset$fbs<0) | (testset$fbs>1)]<-NA

testset$sex<-as.numeric(as.character(testset$sex))
testset$sex[(testset$sex<0) | (testset$sex>1)]<-NA

testset$cp<-as.numeric(as.character(testset$cp))
testset$cp[(testset$cp<1) | (testset$cp>4)]<-NA

testset$exang<-as.numeric(as.character(testset$exang))
testset$exang[(testset$exang<0) | (testset$exang>1)]<-NA


testset$slope<-as.numeric(as.character(testset$slope))
testset$slope[(testset$slope<1) | (testset$slope>3)]<-NA


testset$thal<-as.numeric(as.character(testset$thal))
testset$thal[(testset$thal!=3) & (testset$thal!=7) & (testset$thal!=6) ]<-NA


#Proper Naming
testset$trestbps<-as.numeric(testset$trestbps)
testset$chol<-as.numeric(testset$chol)
testset$thalach<-as.numeric(testset$thalach)
testset$oldpeak<-as.numeric(testset$oldpeak)

testset$sex <- as.numeric(factor(testset$sex))
levels(testset$sex) <- c("female", "male",NA)

testset$cp <- as.numeric(factor(testset$cp))
levels(testset$cp) <- c("typical","atypical","non-anginal","asymptomatic",NA)

testset$fbs <- as.numeric(factor(testset$fbs))
levels(testset$fbs)<-c("false","true",NA)

testset$restecg <- as.numeric(factor(testset$restecg))
levels(testset$restecg) <- c("normal","stt","hypertrophy",NA)

testset$exang <- as.numeric(factor(testset$exang))
levels(testset$exang) <- c("no","yes",NA)

testset$slope<-as.numeric(factor(testset$slope))
levels(testset$slope) <- c("upsloping","flat","downsloping",NA)

testset$ca <- as.numeric(factor(testset$ca)) # not doing level conversion because its not necessary)

testset$thal <- as.numeric(factor(testset$thal))
levels(testset$thal) <- c("normal","fixed","reversable",NA)

#testset$num <- as.numeric(factor(testset$num)) # not doing level conversion because its not necessary

#Filling all the NA
for(i in 1:ncol(testset)){
  testset[is.na(testset[,i]),i]<- 0 #mean(testset[,i], na.rm = TRUE)
}

training_set$trestbps = scale(training_set$trestbps)
test_set$trestbps = scale(test_set$trestbps)

training_set$chol = scale(training_set$chol)
test_set$chol = scale(test_set$chol)

training_set$thalach = scale(training_set$thalach)
test_set$thalach = scale(test_set$thalach)
#Splitting dataset
split = sample.split(dataset$num, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[,2:14], #training_set['sex']+training_set['age']+training_set['cp']+training_set['exang']+training_set['oldpeak'],
                          y = training_set$num,
                          ntree = 3500,
                          mtry=19,
                          nodesize=30
                          )

# Predicting the Test set results
#y_pred = predict(classifier, newdata = test_set[,2:14])
new_dataset<-data.frame(testset[,2:14])
y_pred = predict(classifier, newdata =new_dataset)
summary(y_pred)
# Making the Confusion Matrix
#cm = table(testset[, 15], y_pred)
final_dataframe<-data.frame(testset$id,y_pred)

write.csv(final_dataframe,file="/Users/DK/Desktop/U of T hackathon/sample_sol10.csv",row.names = FALSE)
#install.packages("caret")

#install.packages("e1071")
#library("e1071")
#postResample(y_pred,testset[,15])

