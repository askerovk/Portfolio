# Codebook for tidy data set. 

This codebook describes the final tidy dataset. It contains the following variables:

* subject
* activity
* variable
* statistic
* vector
* meanvalue

The "subject" column lists the 30 people who took part in the study.

The "activity" column list the 6 activities which the participants took part in:

 *   walking
 *   walking upstairs
  * walking downstairs
   * sitting
    * standing
    * laying

The "variable" column list all the original variables, which were measured with phone gyroscopes in the original study:

*    tBodyAcc
 *   tGravityAcc
  *  tBodyAccJerk
   * tBodyGyro
*    tBodyGyroJerk
 *   tBodyAccMag
  *  tGravityAccMag
   * tBodyAccJerkMag
*    tBodyGyroMag
 *   tBodyGyroJerkMag
  *  fBodyAcc
   * fBodyAccJerk
*    fBodyGyro
 *   fBodyAccMag
  *  fBodyAccJerkMag
   * fBodyGyroMag
   * fBodyGyroJerkMag

The "statistic" column list two of the required measurements of mean and std (standard deviation)

The "vector" column list the x, y, z directional vector components of the following variables:

*    tBodyAcc
 *   tGravityAcc
  *  tBodyAccJerk
   * tBodyGyro
*    tBodyGyroJerk
 *   fBodyAcc
  *  fBodyAccJerk
   * fBodyGyro

The "meanvalue" column displays the mean of all observations for a given participant, activity, 
variable, statistic and vector (if any).
