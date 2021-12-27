# Jet2TT - Data Science Use Case 1
## _Predict the Energy Star Score of Buildings_

At first glance the problem statement seems to be more about data cleaning and preprocessing. We can take following 2 approaches

 1. *Clustering Approach*

  This approach will take effect of similar building demography as well as similar climatic conditions to possibly reduce variance in the model
  We can consider longitude and latitude to create clusters of data based on location, which will ensure similar subsets of data based on proximity of buildings and for uniform climatic conditions
  Based on the clusters (or subsets of data), we can find relation between target variable (Energy score) and input variables (energy consumption etc.) by creating a categorical variable for the clusters

2. *Regression*

  This approach takes into account all of the variables and takes the usual regression model path
  
 Conclusion:
  MAE of the rpart(decision tree) regression model
  for train - 13
  for test - 19
  The train MAE can be reduced and the gap between train and test MAE can be bridged by:
  1. Ommitting less important number of columns with more missing data than 40%. As the missing values are replaced with median values, large proportion of dataset with a constant value can impactMAe for train and test
  2. Hyperparameter tuning for xGboost - not performed currently due to resource limitations on personal computer. Overfitting can be concluded based on train MAE vs test MAE.
  3. Random forest and KNN algorithms - These algorithms can be run to assess any improvement 
