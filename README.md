# project_week4
 
Load the necessary packages in R : 

`library(caret)
library(ggplot2)
library(rattle)
library(rpart)
library(randomForest)`


###LOAD DATA

The Data are the one provided by coursera (link). One large test sample and one very small test sample to predict when the model is completed.  

`pml<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = T , sep= ",") ##load data train `


`test<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=T , sep=",") ## load 20 cases test data` 

###CLEAN DATA 

 First task is to clean the data to be able to see clearly the variable that can be used for the model. 

First,  let's remove the not so relevant columns : time stamps, ids and names

`pml_clean<-pml[,-c(1,2,3,4,5,6,7)]`

###DEAL WITH NA's VALUE

Secondly, we need to get rid of the variable wiht too many NAs. Here's hte threshold is 10,000 per column (around 50%)  

`na<-is.na(pml_clean) ## count na `

`col_na<-colSums(na) ##  count na for each columns`

`col_remove<-which(Col_na>10000)`

`pml_clean2<-pml_clean[,-col_remove]`

###CREATE TRAIN AND TEST SAMPLE FROM DATA  

The idea is to be able to train and test the model before predicting the 20 cases. 

`set.seed(25930) ## can be reproduce `

`intrain<-createDataPartition(pml_clean2$classe, p=0.6, list=F) ## create a train//test in full set 60%//40% `

`train_pml<-pml_clean2[intrain,] #train`

`test_pml<-pml_clean2[-intrain,] # test //validation before prediction on 20 case for exam `

###CHOOSE PREDICTOR 
Let's remove the predictor with near zero variance (very little variance will not help a lot in  classification)

`nearZero<-nearZeroVar(train_pml, saveMetrics = T) ## give the variable with near zero variance so wecan exclude them`

 53 variables don't have near zero variance and can be used in the project 

###CREATION OF THE NEW TRAIN AND TEST  SAMPLE 
At this point, we need to remove the irrelevant columns in  the train and testing set. 

`mytrain<-train_pml[,c('roll_belt'
                      ,'pitch_belt'
                      ,'yaw_belt'
                      ,'total_accel_belt'
                      ,'gyros_belt_x'
                      ,'gyros_belt_y'
                      ,'gyros_belt_z'
                      ,'accel_belt_x'
                      ,'accel_belt_y'
                      ,'accel_belt_z'
                      ,'magnet_belt_x'
                      ,'magnet_belt_y'
                      ,'magnet_belt_z'
                      ,'roll_arm'
                      ,'pitch_arm'
                      ,'yaw_arm'
                      ,'total_accel_arm'
                      ,'gyros_arm_x'
                      ,'gyros_arm_y'
                      ,'gyros_arm_z'
                      ,'accel_arm_x'
                      ,'accel_arm_y'
                      ,'accel_arm_z'
                      ,'magnet_arm_x'
                      ,'magnet_arm_y'
                      ,'magnet_arm_z'
                      ,'roll_dumbbell'
                      ,'pitch_dumbbell'
                      ,'yaw_dumbbell'
                      ,'total_accel_dumbbell'
                      ,'gyros_dumbbell_x'
                      ,'gyros_dumbbell_y'
                      ,'gyros_dumbbell_z'
                      ,'accel_dumbbell_x'
                      ,'accel_dumbbell_y'
                      ,'accel_dumbbell_z'
                      ,'magnet_dumbbell_x'
                      ,'magnet_dumbbell_y'
                      ,'magnet_dumbbell_z'
                      ,'roll_forearm'
                      ,'pitch_forearm'
                      ,'yaw_forearm'
                      ,'total_accel_forearm'
                      ,'gyros_forearm_x'
                      ,'gyros_forearm_y'
                      ,'gyros_forearm_z'
                      ,'accel_forearm_x'
                      ,'accel_forearm_y'
                      ,'accel_forearm_z'
                      ,'magnet_forearm_x'
                      ,'magnet_forearm_y'
                      ,'magnet_forearm_z'
                      ,'classe')]`
                      
`mytest<-test_pml[,c('roll_belt' ,'pitch_belt' ,'yaw_belt' ,'total_accel_belt' ,'gyros_belt_x'  ,'gyros_belt_y','gyros_belt_z' ,'accel_belt_x' ,'accel_belt_y'
                      ,'accel_belt_z'
                      ,'magnet_belt_x'
                      ,'magnet_belt_y'
                      ,'magnet_belt_z'
                      ,'roll_arm'
                      ,'pitch_arm'
                      ,'yaw_arm'
                      ,'total_accel_arm'
                      ,'gyros_arm_x'
                      ,'gyros_arm_y'
                      ,'gyros_arm_z'
                      ,'accel_arm_x'
                      ,'accel_arm_y'
                      ,'accel_arm_z'
                      ,'magnet_arm_x'
                      ,'magnet_arm_y'
                      ,'magnet_arm_z'
                      ,'roll_dumbbell'
                      ,'pitch_dumbbell'
                      ,'yaw_dumbbell'
                      ,'total_accel_dumbbell'
                      ,'gyros_dumbbell_x'
                      ,'gyros_dumbbell_y'
                      ,'gyros_dumbbell_z'
                      ,'accel_dumbbell_x'
                      ,'accel_dumbbell_y'
                      ,'accel_dumbbell_z'
                      ,'magnet_dumbbell_x'
                      ,'magnet_dumbbell_y'
                      ,'magnet_dumbbell_z'
                      ,'roll_forearm'
                      ,'pitch_forearm'
                      ,'yaw_forearm'
                      ,'total_accel_forearm'
                      ,'gyros_forearm_x'
                      ,'gyros_forearm_y'
                      ,'gyros_forearm_z'
                      ,'accel_forearm_x'
                      ,'accel_forearm_y'
                      ,'accel_forearm_z'
                      ,'magnet_forearm_x'
                      ,'magnet_forearm_y'
                      ,'magnet_forearm_z'
                      ,'classe' )]  `              


###FIT MODEL -- RANDOM FOREST 

Using random forest algorithm to predict classe (best accuracy compared to tree). 

`fit_forest<-train(classe ~ . , data = mytrain, method="rf") ## fit a random forest `

### PREDICTION AND ACCURACY 
Now that the model is fitted we can use the test set to estimate the accurary of the model 

`preds<-predict(fit_forest$finalModel,newdata= mytest)`


`confusionMatrix(preds,mytest$classe)`




