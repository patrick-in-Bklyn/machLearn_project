---
title: "JHU/Coursera Practical Machine Learning Project"
author: "Patrick_in_Bklyn"
date: "September 15, 2014"
output: html_document
---
The data used in this project was kindly provided by: 
                Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

Please read the licensing conditions on the site below as i want to comply with them .

Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3DQI0RW23

##Synopsis and Conclusion
The dataset contains various measurements taken from sensors located on a set of dumbells and on a subject's forearms, arms and waist during the execution of "bicep curls". The assignment is to use this data to predict whether the subject is performing the operation properly. If the subject is executing the exercise incorrectly, the prediction model should identify which of 4 common errors the subject is making. 

By graphically exploring the data I concluded that each user had a very different method of executing the bicep curl. Also, the data is "polluted" to some extent by including sensor readings for periods where the dumbells appear to be at rest. Also, to generate an accurate model, I reformatted the training file substantially to capture the range of motion across time windows. When I used a model from this data set, I achieved reasonably impressive accuracy on a partitioned test set. Unfortunately, it was not possible to run this model on the test set provided as this set had only 20 rows and there did not seem to be partitioned on the basis of time windows. 

Despite all of these shortcomings, I settled on three methods of machine learning.

The first was reformat the training file substantially. Adding a range of movement parameter between each new window = 'yes' parameter. I ran a hdda simulation on this reformatted file.  
        This was a very efficient method, providong 97% accuracy. 
The second was to use a rpart method on the reformatted file with preprocessing to center and scale the variables. This produced 97% accuracy and was efficient in terms of processing time. 

Finally I ran the hdda model on all variables using just the original training file. This provided poor results with accuracy at 

To complete the assignment, however I also ran an rpart model on the accel_arm, accel_bel components of the file given. This method produced a model with 71% acuracy. 




##The Human Activity Recognition Experiment
This project attempts to find an accurate model that will regocnize the qualitative component of human activity. Subjects were asked to perform "bicep curls" while holding relatively light dumbells. Signals from both the subject and the dumbells were measured periodically to capture the movement of the weights throughout each exercise and the "quality" of each repetition was assigned a letter from A to E depending on the execution of the bicep curl.  

The description of this experiment is quoted from the website as follows (I re-formatted the text for easier understanding):
_______________________________________________________________________________________________________________

Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: 

1. Class A      --      Exactly according to the specification
2. Class B      --      Throwing the elbows to the front 
3. Class C      --      Lifting the dumbbell only halfway
4. Class D      --      Lowering the dumbbell only halfway
5. Class E      --      Throwing the hips to the front

Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3DQJi9fXq

_____________________________________________________________________________________________________________

## Getting and Preparing the Data
The data comes in two parts and carries a large number of empty fields. To begin I extracted the columns with the instrument readings (gyro, magnet and accel) and the fields that described movement (roll, pitch and yaw). I then used cbind to add these back to first seven columns. FInally I added the classe column to the data. 



```
## [1]  20 160
```

I then ran a series of plots on this data in an attempt to find a combination of readings that would produce a clean separation of the outcome classes. I include some of these below.

I found that there were some areas where there is a reasonably clear relationship between the given data inputs and the quality classifier. For instance, the E classifier is reasonably easy to identify from the acceleration along the z plane of the belt monitor. Also, the y and z acceleration of the arm monitor show different patterns of curves depending on the quality of the exercise performed. 

Each subject carried out the exercise in a different manner. As a result, there seems to be litlle or no consistent means to identify an activity across users. To address this, I scaled the range parameters 





```
## [1] "pml-testing.csv"  "pml-training.csv"
```

```
## [1] 19622   160
```

Graphically splitting the data offered some promise for finding a set of drivers for this model. For instance the E classifier is seems reasonably identifiable by the belt acceleration metric. Also, the A classifier seems sufficiently different to most of the other classifiers to make it identifiable in a generatized linear model. 

There were difficulties in differentiating between categories C and D and sometimes between B and C. The B and C errors (which capture either lifting or lowewring the dumbells only half way) both measures should exhibit lower overall acelleration along the Y axis over a repetition, however total acceleration
between the parameters should be similar.

Finally, the classification of classe A appears to be polluted to some extent as it seems to include periods when the dumbells are at rest (see the )

```r
qplot(X,  accel_arm_y+accel_arm_z, data = Dumbells, colour = classe, facets = user_name~., ylab="Arm Acceleration in Y and Z planes - all Subjects")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```r
qplot(raw_timestamp_part_1,  accel_arm_y, data = Dumbells[Dumbells$user_name == "carlitos",], colour = classe, facets = user_name~., ylab="Y Acceleration of arm Monitors", main = "Arm accel in Y Plane _ Carlito")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

```r
qplot(raw_timestamp_part_1,  accel_arm_z, data = Dumbells[Dumbells$user_name == "carlitos",], colour = classe, facets = user_name~., ylab="Z Acceleration of arm Monitors", main = "Arm Accel in Z plane - Carlito")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) 

```r
qplot(raw_timestamp_part_1,  accel_belt_z, data = Dumbells[Dumbells$user_name == "carlitos",], colour = classe, facets = user_name~., ylab="Z Acceleration of belt Monitors", main = "Belt Accel in Z plane -Carlito")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-34.png) 



#Splitting the Data and Selecting prediction Models

In an effort to maximize the effectiveness of the prediction model, I decided to prep the data files as follows;

1.Extract the accel, roll, pitch and yaw columns from the original file. Add these to the classe column and the first 7 cols of the original data. 
2.Make a parallel set of data frames including only the rows in which a "new window" begins. I calculated the max, min and average of the accel and pitch data to add to this frame. 





```r
prep_file <- function(file){
        grep_terms <- c("gyros", "magnet", "accel", "total_accel")
        search_terms <- paste("^", grep_terms, sep = "", collapse = ",")
        search_terms <- unlist(strsplit(search_terms, ","))
        wanted_cols <- unlist(lapply(search_terms, function(x) c(colnames(file)[grep(x, cols)])))
        new_file<- subset(file, select = wanted_cols)
        new_file <- cbind(file[,1:7], new_file)
        return(new_file)
}

new_dumbells <- prep_file(Dumbells)

new_dumbells <- cbind(new_dumbells,Dumbells$classe)
colnames(new_dumbells)[ncol(new_dumbells)] = "classe"
```



```r
## Using the New_window column to separate out segments of each column. Find the max and min within each segment


add_cols <- function(file)
                {
                accel_belt_z_range <- merge(aggregate(accel_belt_z~num_window, file, max),aggregate(accel_belt_z~num_window, file, min), by = "num_window")

                colnames(accel_belt_z_range) <- c("num_window", "accel_belt_z_max", "accel_belt_z_min")
                accel_belt_z_range <- cbind(accel_belt_z_range, (accel_belt_z_range$accel_belt_z_max-accel_belt_z_range$accel_belt_z_min))
                colnames(accel_belt_z_range) <- c("num_window", "accel_belt_z_max", "accel_belt_z_min", "accel_belt_z_range")

                accel_arm_y_range <- merge(aggregate(accel_arm_y~num_window, file, max),aggregate(accel_arm_y~num_window, new_dumbells, min), by = "num_window")
                colnames(accel_arm_y_range) <- c("num_window", "accel_arm_y_max", "accel_arm_y_min")
                accel_arm_y_range <- cbind(accel_arm_y_range, (accel_arm_y_range$accel_arm_y_max-accel_arm_y_range$accel_arm_y_min))
                colnames(accel_arm_y_range) <- c("num_window", "accel_arm_y_max", "accel_arm_y_min", "accel_arm_y_range")

                accel_arm_x_range <- merge(aggregate(accel_arm_x~num_window, file, max),aggregate(accel_arm_x~num_window, new_dumbells, min), by = "num_window")
                colnames(accel_arm_x_range) <- c("num_window", "accel_arm_x_max", "accel_arm_x_min")
                accel_arm_x_range <- cbind(accel_arm_x_range, (accel_arm_x_range$accel_arm_x_max-accel_arm_x_range$accel_arm_x_min))
                colnames(accel_arm_x_range) <- c("num_window", "accel_arm_x_max", "accel_arm_x_min", "accel_arm_x_range")

                new_file <- merge(file, accel_belt_z_range, by = "num_window")
                new_file <- merge(new_file, accel_arm_y_range, by = "num_window")
                new_file <- merge(new_file, accel_arm_x_range, by = "num_window")
                return(new_file)
        }
new_dumbells <- add_cols(new_dumbells)
colnames(new_dumbells)
```

```
##  [1] "num_window"           "X"                    "user_name"           
##  [4] "raw_timestamp_part_1" "raw_timestamp_part_2" "cvtd_timestamp"      
##  [7] "new_window"           "gyros_belt_x"         "gyros_belt_y"        
## [10] "gyros_belt_z"         "gyros_arm_x"          "gyros_arm_y"         
## [13] "gyros_arm_z"          "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [16] "gyros_dumbbell_z"     "gyros_forearm_x"      "gyros_forearm_y"     
## [19] "gyros_forearm_z"      "magnet_belt_x"        "magnet_belt_y"       
## [22] "magnet_belt_z"        "magnet_arm_x"         "magnet_arm_y"        
## [25] "magnet_arm_z"         "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [28] "magnet_dumbbell_z"    "magnet_forearm_x"     "magnet_forearm_y"    
## [31] "magnet_forearm_z"     "accel_belt_x"         "accel_belt_y"        
## [34] "accel_belt_z"         "accel_arm_x"          "accel_arm_y"         
## [37] "accel_arm_z"          "accel_dumbbell_x"     "accel_dumbbell_y"    
## [40] "accel_dumbbell_z"     "accel_forearm_x"      "accel_forearm_y"     
## [43] "accel_forearm_z"      "total_accel_belt"     "total_accel_arm"     
## [46] "total_accel_dumbbell" "total_accel_forearm"  "classe"              
## [49] "accel_belt_z_max"     "accel_belt_z_min"     "accel_belt_z_range"  
## [52] "accel_arm_y_max"      "accel_arm_y_min"      "accel_arm_y_range"   
## [55] "accel_arm_x_max"      "accel_arm_x_min"      "accel_arm_x_range"
```

##The Prediction Model
I took the following approach to finding a predictive model. 

1. Use a brute force method. 
        I ran a naive bayes model on the the range predictors. I preProcessed the data with Center and scale commands.
        This produced a model with poor 59% accuracy results. 
        
2. Borrow a more sophisticated model. 
        I used a high Dimensional Discriminant analysis to run on the data. This method produced better results, however it apparently suffers from 
        overfitting problems. This produced 97% accuracy, so I kept this model 
        
3. I tried to use a randon forest, but in every case it took too long to process. 



Having created a training and test set from the training data provided, I decided to take a sledgehammer approach first. I began by running a generalized linear model on the data, with all of the predictive parameters as a possibility. This failed because there were too many predictors. 

I next used a random forest option("rf") and although this produced a result, the processing time was excessive so I discard this model. 

```
## Loading required package: HDclassif
## Loading required package: MASS
```


```r
confusionMatrix(Predictions, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1351    0    0    0    0
##          B   44  939    2    0    0
##          C    0   10  816    0    0
##          D    0    0   37  775    8
##          E    0    0    0   29  893
## 
## Overall Statistics
##                                         
##                Accuracy : 0.973         
##                  95% CI : (0.969, 0.978)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.967         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.968    0.989    0.954    0.964    0.991
## Specificity             1.000    0.988    0.998    0.989    0.993
## Pos Pred Value          1.000    0.953    0.988    0.945    0.969
## Neg Pred Value          0.988    0.997    0.990    0.993    0.998
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.275    0.191    0.166    0.158    0.182
## Detection Prevalence    0.275    0.201    0.168    0.167    0.188
## Balanced Accuracy       0.984    0.989    0.976    0.976    0.992
```

```r
FitRf <- train(classe ~accel_arm_x_range+accel_arm_y_range+accel_belt_x+accel_belt_y+accel_belt_z, method = "rpart", preProcess = c("center", "scale"), data = training)
```

```
## Loading required package: rpart
```

```r
Predictions.Rf <- predict(FitRf, newdata = testing)

confusionMatrix(Predictions.Rf, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   A   B   C   D   E
##          A 568  63  34  38  54
##          B  71 226  51  29  83
##          C 348 321 450 186  79
##          D 397 243 231 463 217
##          E  11  96  89  88 468
## 
## Overall Statistics
##                                        
##                Accuracy : 0.444        
##                  95% CI : (0.43, 0.458)
##     No Information Rate : 0.284        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.312        
##  Mcnemar's Test P-Value : <2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.407   0.2381   0.5263   0.5759   0.5194
## Specificity             0.946   0.9408   0.7693   0.7346   0.9291
## Pos Pred Value          0.750   0.4913   0.3251   0.2985   0.6223
## Neg Pred Value          0.801   0.8373   0.8849   0.8983   0.8957
## Prevalence              0.284   0.1935   0.1743   0.1639   0.1837
## Detection Rate          0.116   0.0461   0.0918   0.0944   0.0954
## Detection Prevalence    0.154   0.0938   0.2822   0.3163   0.1533
## Balanced Accuracy       0.677   0.5895   0.6478   0.6553   0.7242
```

The hdda approach appears to produce a very accurate result with 97% accuracy. So I tested this against the given test data. This however did not work because there is no column for "classe" in the test file. 

##Feeding the test files. 
These methods did not work for submitting results. To address this I accepted a lower accuracy and used a rpart model on the most relevant metrick in the frame(at least according to my view). On that basis, I used arm acceleration and belt acceleration and dumbell acceleration to determine the best method to 


```r
set.seed(1234)
inTrain <- createDataPartition(y = new_dumbells$classe, p = 0.75, list = FALSE)
training1 <- Dumbells[inTrain,]
testing1 <- Dumbells[-inTrain,]

Fit <- train(classe ~ accel_arm_x+accel_arm_y+accel_arm_z+accel_belt_x+accel_belt_y+accel_belt_z+accel_forearm_x+accel_forearm_y+accel_forearm_z, method = "hdda", data = training1)
##Fit <- train(classe ~ ., method = 'rf', data = training)
```


```r
Predictions_for_test <- predict(Fit, newdata = testing1)

confusionMatrix(Predictions_for_test, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   A   B   C   D   E
##          A 591 275 230 266 381
##          B 246 216 134 137  80
##          C 218 153 178 131 138
##          D 171 157 130 166 183
##          E 169 148 183 104 119
## 
## Overall Statistics
##                                         
##                Accuracy : 0.259         
##                  95% CI : (0.247, 0.271)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : 1             
##                                         
##                   Kappa : 0.054         
##  Mcnemar's Test P-Value : <2e-16        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.424    0.228   0.2082   0.2065   0.1321
## Specificity             0.672    0.849   0.8419   0.8437   0.8491
## Pos Pred Value          0.339    0.266   0.2176   0.2057   0.1646
## Neg Pred Value          0.746    0.821   0.8343   0.8443   0.8130
## Prevalence              0.284    0.194   0.1743   0.1639   0.1837
## Detection Rate          0.121    0.044   0.0363   0.0338   0.0243
## Detection Prevalence    0.355    0.166   0.1668   0.1646   0.1474
## Balanced Accuracy       0.548    0.538   0.5251   0.5251   0.4906
```




