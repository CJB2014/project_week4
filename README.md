# project_week4
Coursera course project 

###Final Project 


load packages 


`library(caret)
library(ggplot2)
library(rattle)
library(rpart)
library(randomForest)`


##LOAD DATA

`pml<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = T , sep= ",") ##load data train 
test<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header=T , sep=",") ## load 20 cases test data `

##CLEAN DATA 

`pml_clean<-pml[,-c(1,2,3,4,5,6,7)] ## clean first column id, time stamps `

##DEAL WITH NA's VALUE

`na<-is.na(pml_clean) ## count na 
col_na<-colSums(na) ##  count na for each columns
col_remove<-which(Col_na>10000)
pml_clean2<-pml_clean[,-col_remove]`

##CREATE TRAIN AND TEST SAMPLE FROM DATA  

`set.seed(25930) ## can be reproduce 
intrain<-createDataPartition(pml_clean2$classe, p=0.6, list=F) ## create a train//test in full set 60%//40% 
train_pml<-pml_clean2[intrain,] #train
test_pml<-pml_clean2[-intrain,] # test //validation before prediction on 20 case for exam `

##CHOOSE PREDICTOR 

`nearZero<-nearZeroVar(train_pml, saveMetrics = T) ## give the variable with near zero variance so wecan exclude them`

-- 53 variables don't have near zero variance and can be used in the project 

##CREATION OF THE NEW TRAIN AND TEST  SAMPLE 

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


##FIT MODEL -- RANDOM FOREST 

`fit_forest<-train(classe ~ . , data = mytrain, method="rf") ## fit a random forest `
