# HR Attrition Analysis
In this project, exploratory data analysis was used to identify types of employees who have a likelihood of quitting. Random forest and GBM were used to create a model to predict employee attrition. Due to the class imbalance of the attrition variable, upsampling and downsampling were utilized to create a more balanced data set. Since false negatives are more costly in an employee attrition model, recall was used to assess the model. GBM with downsampling performed the best (Recall = 0.787). Based on the variable importance of this model, monthly income and overtime are key variables in predicting attrition. <br />

Results: https://rpubs.com/mary18/929056
