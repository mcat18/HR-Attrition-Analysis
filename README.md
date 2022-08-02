# HR Attrition Analysis
In this project, exploratory data analysis was used to identify types of employees that will quit. Random forest and GBM models were used to create a model to predict employee attrition. Upsampling and downsampling were used because of the class imbalance for the attrition variable. Recall was used to assess the model instead of accuracy because false negatives are more costly. GBM with downsampling performed the best (Recall = 0.78720). Based on the variable importance plot for this model, monthly income and overtime are the most important variables. <br />

Results: https://rpubs.com/mary18/929056
