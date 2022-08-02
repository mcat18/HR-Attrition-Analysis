# Objective 
  
  
#Development of retention strategies is important to the health of a company. 
#Employee turnover has many consequences such as low morale, increased recruitment costs, and decreased productivity. 
# The goal of this analysis is to discover why employees leave, 
# and create a model that predicts employee attrition to help alleviate attrition. 
  
  # Data Cleaning
  
  #Variable types were changed to allow proper analysis and missing values were checked. 
  
  #Loading packages
  # Package names
  packages <- c("tidyverse", "ggcorrplot", "randomForest", "here", "knitr", "gbm", 
                "caret", "car", "readr", "DescTools", "MASS", "rstatix", "viridis", 
                "pdp","rmarkdown")
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  
  here()
  
  hr_data = read.csv(here("Input", "hr_ibm_data_2.csv"))
  
  
  #Checking the structure of the data 
  
  str(hr_data)
  
  #Age column name has weird formatting in the title so the column name will be renmaed. 
  
  hr_data = 
    hr_data %>% 
    dplyr::rename(Age = ï..Age)
  
  #categorical variables are currently character and need to be converted to factor variables 
  #some variables are integer type but should be factor
  
  # Education
  # 1 'Below College'
  # 2 'College'
  # 3 'Bachelor'
  # 4 'Master'
  # 5 'Doctor'
  # 
  # EnvironmentSatisfaction
  # 1 'Low'
  # 2 'Medium'
  # 3 'High'
  # 4 'Very High'
  # 
  # JobInvolvement
  # 1 'Low'
  # 2 'Medium'
  # 3 'High'
  # 4 'Very High'
  # 
  # JobSatisfaction
  # 1 'Low'
  # 2 'Medium'
  # 3 'High'
  # 4 'Very High'
  # 
  # PerformanceRating
  # 1 'Low'
  # 2 'Good'
  # 3 'Excellent'
  # 4 'Outstanding'
  # 
  # RelationshipSatisfaction
  # 1 'Low'
  # 2 'Medium'
  # 3 'High'
  # 4 'Very High'
  # 
  # WorkLifeBalance
  # 1 'Bad'
  # 2 'Good'
  # 3 'Better'
  # 4 'Best'
  
  hr_data$Attrition = as.factor(hr_data$Attrition)
  hr_data$BusinessTravel = as.factor(hr_data$BusinessTravel)
  hr_data$Department = as.factor(hr_data$Department)
  hr_data$Gender = as.factor(hr_data$Gender)
  hr_data$OverTime = as.factor(hr_data$OverTime)
  hr_data$JobRole = as.factor(hr_data$JobRole)
  hr_data$Over18 = as.factor(hr_data$Over18)
  hr_data$MaritalStatus = as.factor(hr_data$MaritalStatus)
  hr_data$EducationField = as.factor(hr_data$EducationField)
  
  #Education 
  hr_data$Education[hr_data$Education == 1] = "Below College"
  hr_data$Education[hr_data$Education == 2] = "College"
  hr_data$Education[hr_data$Education == 3] = "Bachelor"
  hr_data$Education[hr_data$Education == 4] = "Master"
  hr_data$Education[hr_data$Education == 5] = "Doctor"
  
  hr_data$Education = as.factor(hr_data$Education)
  
  #EnvironmentStatisfaction 
  hr_data$EnvironmentSatisfaction[hr_data$EnvironmentSatisfaction == 1] = "Low"
  hr_data$EnvironmentSatisfaction[hr_data$EnvironmentSatisfaction == 2] = "Medium"
  hr_data$EnvironmentSatisfaction[hr_data$EnvironmentSatisfaction == 3] = "High"
  hr_data$EnvironmentSatisfaction[hr_data$EnvironmentSatisfaction == 4] = "Very High"
  
  hr_data$EnvironmentSatisfaction = as.factor(hr_data$EnvironmentSatisfaction)
  
  #JobInvolvement 
  hr_data$JobInvolvement[hr_data$JobInvolvement == 1] = "Low"
  hr_data$JobInvolvement[hr_data$JobInvolvement == 2] = "Medium"
  hr_data$JobInvolvement[hr_data$JobInvolvement == 3] = "High"
  hr_data$JobInvolvement[hr_data$JobInvolvement == 4] = "Very High"
  
  hr_data$JobInvolvement = as.factor(hr_data$JobInvolvement)
  
  #JobSatisfaction
  hr_data$JobSatisfaction[hr_data$JobSatisfaction == 1] = "Low"
  hr_data$JobSatisfaction[hr_data$JobSatisfaction == 2] = "Medium"
  hr_data$JobSatisfaction[hr_data$JobSatisfaction == 3] = "High"
  hr_data$JobSatisfaction[hr_data$JobSatisfaction == 4] = "Very High"
  
  hr_data$JobSatisfaction = as.factor(hr_data$JobSatisfaction)
  
  #PerformanceRating
  hr_data$PerformanceRating[hr_data$PerformanceRating == 1] = "Low"
  hr_data$PerformanceRating[hr_data$PerformanceRating == 2] = "Good"
  hr_data$PerformanceRating[hr_data$PerformanceRating == 3] = "Excellent"
  hr_data$PerformanceRating[hr_data$PerformanceRating == 4] = "Outstanding"
  
  hr_data$PerformanceRating = as.factor(hr_data$PerformanceRating)
  
  #RelationshipSatisfaction 
  hr_data$RelationshipSatisfaction[hr_data$RelationshipSatisfaction == 1] = "Low"
  hr_data$RelationshipSatisfaction[hr_data$RelationshipSatisfaction == 2] = "Medium"
  hr_data$RelationshipSatisfaction[hr_data$RelationshipSatisfaction == 3] = "High"
  hr_data$RelationshipSatisfaction[hr_data$RelationshipSatisfaction == 4] = "Very High"
  
  hr_data$RelationshipSatisfaction = as.factor(hr_data$RelationshipSatisfaction)
  
  #WorkLifeBalance
  hr_data$WorkLifeBalance[hr_data$WorkLifeBalance == 1] = "Bad"
  hr_data$WorkLifeBalance[hr_data$WorkLifeBalance == 2] = "Good"
  hr_data$WorkLifeBalance[hr_data$WorkLifeBalance == 3] = "Better"
  hr_data$WorkLifeBalance[hr_data$WorkLifeBalance == 4] = "Best"
  
  hr_data$WorkLifeBalance = as.factor(hr_data$WorkLifeBalance)
  
  
  #Checking to see if the changes were implemented correctly 
  str(hr_data)
  
  #Checking for missing values
  
  sum(is.na(hr_data))
  
  #There are no missing data points 
  
  # Data Exploration and Visualization 
  
  
  ## Attrition
  
  #The company has a 16% attrition rate. 
  
  
  hr_data %>% 
    dplyr::group_by(Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = Attrition, y = freq, fill = Attrition)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Attrition", x = "Attrition", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  ## Business Travel and Attrition
  
  #Employees who travel frequently have the highest employee turnover. 
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data %>% 
    dplyr::group_by(BusinessTravel, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = BusinessTravel, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Business Travel and Attrition", x = "Business Travel", y = "Percentage") +
    scale_x_discrete(breaks = c("Travel_Rarely", "Travel_Frequently", "Non-Travel"),
                     labels = c("Travel Rarely","Travel Frequently", "Non Travel")) +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  
  ## Department and Attrition 
  
  #Employees in the Sales Department have the highest turnover. 
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data %>% 
    dplyr::group_by(Department, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = Department, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Department and Attrition", x = "Department", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  
  ## Gender and Attrition 
  
  #The attrition rates between men and women is very similar. Men have a slighly higher turnover rate. 
  
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data %>% 
    dplyr::group_by(Gender, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = Gender, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Gender and Attrition", x = "Gender", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  ## Overtime and Attrition  
  
  #Employees who work overtime have the highest turnover rate. 
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data %>% 
    dplyr::group_by(OverTime, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = OverTime, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Over Time and Attrition", x = "Over Time", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  ## Job Role and Attrition 
  
  #Sales representatives have the highest attrition rate. 
  
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data %>% 
    dplyr::group_by(JobRole, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = JobRole, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Job Role and Attrition", x = "Job Role", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c")) +
    theme(axis.text.x = element_text(angle = 20, hjust = 0.5))

  ## Environement Satisfaction and Attrition 
  
  #Employees with low environment satisfaction leave more than employees with medium, high, and very high enviroment satisfaction. 
  
  
  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  
  hr_data$EnvironmentSatisfaction = factor(hr_data$EnvironmentSatisfaction, levels = c("Low",
                                                                                       "Medium",
                                                                                       "High",
                                                                                       "Very High"))
  
  
  hr_data %>% 
    dplyr::group_by(EnvironmentSatisfaction, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = EnvironmentSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Environment Satisfaction and Attrition", x = "Environment Satisfaction", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  ## Job Satisfaction and Attrition 
  
  #Employees with low job satisfaction leave more than employees with medium, high, and very high job satisfaction.
  
  

  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  hr_data$JobSatisfaction = factor(hr_data$JobSatisfaction, levels = c("Low",
                                                                       "Medium",
                                                                       "High",
                                                                       "Very High"))
  
  hr_data %>% 
    dplyr::group_by(JobSatisfaction, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = JobSatisfaction, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Job Satisfaction and Attrition", x = "Job Satisfaction", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  
  ## Work Life Balance and Attrition 
  
  #Employees with bad work life balance have the highest amount of turnover. 
  
  
  hr_data$Attrition = factor(hr_data$Attrition, levels = c("Yes", "No"))
  hr_data$WorkLifeBalance = factor(hr_data$WorkLifeBalance, levels = c("Bad",
                                                                       "Good",
                                                                       "Better",
                                                                       "Best"))
  
  hr_data %>% 
    dplyr::group_by(WorkLifeBalance, Attrition) %>% 
    dplyr::summarise(cnt = n()) %>% 
    dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
    ggplot(aes(x = WorkLifeBalance, y = freq, fill = Attrition)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = paste0(round(freq,0), "%")), 
              position = position_stack(vjust = 0.5), size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Work Life Balance and Attrition", x = "Work Life Balance", y = "Percentage") +
    scale_fill_manual(values = c("#fde725",  "#21918c"))

  ## Distribution of Monthly Income 
  
  #Employees who left had a lower monthly income. 
  
  ggplot(hr_data, aes(x=MonthlyIncome, fill=Attrition, color=Attrition)) +
    geom_histogram(position="identity", alpha=0.5) +
    labs(title = "Distribution of Monthly Income") +
    scale_color_manual(values=c("#fde725", "#21918c")) +
    scale_fill_manual(values=c("#fde725", "#21918c"))

  
  ## Distribution of Years at Company 
  
  #Employees who left were not at the company for a long time.
  
  ggplot(hr_data, aes(x=YearsAtCompany, fill=Attrition, color=Attrition)) +
    geom_histogram(position="identity", alpha=0.5) +
    labs(title = "Distribution of Years at Company") +
    scale_color_manual(values=c("#fde725", "#21918c")) +
    scale_fill_manual(values=c("#fde725", "#21918c"))

  
  ## Correlations
  
  #Total working years and years at company have a high correlation (r = .63), total working year and age have a high correlation (r = .68),
  # total working year and monthly income have a high correlation (r = .77),
  # years in current role and year working with current manager have a high correlation (r = .71), 
  # years with current manager and years at company have a high correlation (r = .77), 
  # and years at company and years in current role have a high correlation (r = .76). 
  
  
  cordata <- hr_data %>% 
    dplyr::select(c("DistanceFromHome", "MonthlyIncome",
                    "NumCompaniesWorked", "TotalWorkingYears", 
                    "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager", "Age"))
  cormatrix <- cor(cordata)
  round(cormatrix, 2)

  ggcorrplot(cormatrix, hc.order = TRUE,outline.color = "white", lab = TRUE, colors = c("#52c569", "white", "#fde725"), lab_size = 2.5) +
    labs(title="Correlation of Numeric Variables") 

  
  
  
  
  # Chi-Square Test for Feature Selection 
  
  #To decide which categorical variables should be kept in the attrition model, Chi-Square will be used to test
  # whether there is a relationship between the categorical variables and attrition. 
  # The null hypothesis for this test is the two variables are independent, 
  # and the alternative hypothesis is the variables are not independent. 
  # In order to reject the null hypothesis and keep variables in the model, the p-value of this test must have a p-value below .05. 
  
  
  #The variables we will leave out of the model are education, gender, performance rating, and relationship satisfaction. 
  #These variables all have a p-value above .05 so they are independent from attrition. 
  
  
  chisq.test(hr_data$BusinessTravel, hr_data$Attrition)
  chisq.test(hr_data$Department, hr_data$Attrition)
  chisq.test(hr_data$Education, hr_data$Attrition)
  chisq.test(hr_data$EducationField, hr_data$Attrition)
  chisq.test(hr_data$EnvironmentSatisfaction, hr_data$Attrition)
  chisq.test(hr_data$Gender, hr_data$Attrition)
  chisq.test(hr_data$JobInvolvement, hr_data$Attrition)
  chisq.test(hr_data$JobRole, hr_data$Attrition)
  chisq.test(hr_data$JobSatisfaction, hr_data$Attrition)
  chisq.test(hr_data$MaritalStatus, hr_data$Attrition)
  chisq.test(hr_data$PerformanceRating, hr_data$Attrition)
  chisq.test(hr_data$OverTime, hr_data$Attrition)
  chisq.test(hr_data$RelationshipSatisfaction, hr_data$Attrition)
  chisq.test(hr_data$StockOptionLevel, hr_data$Attrition)
  chisq.test(hr_data$WorkLifeBalance, hr_data$Attrition)

  
  chisq_results = 
    data.frame(Variable = c("Business Travel",
                            "Department", 
                            "Education",
                            "Education Field",
                            "Environment Satisfaction",
                            "Gender",
                            "Job Involvement", 
                            "Job Role",
                            "Job Satisfaction",
                            "Marital Status",
                            "Over Time",
                            "Performance Rating",
                            "Relationship Satisfaction",
                            "Stock Option Level",
                            "Work Life Balance"),
               Chi_Sq_Stat = c(24.18,
                               10.80, 
                               3.07, 
                               16.03,
                               22.50,
                               1.13,
                               28.49,
                               86.19,
                               17.51,
                               46.16,
                               87.54,
                               0.00,
                               5.241,
                               60.51,
                               16.33),
               P_value = c(0.00,
                           0.00,
                           0.55, 
                           0.01,
                           0.00,
                           0.29,
                           0.00,
                           0.00,
                           0.00,
                           0.00,
                           0.00,
                           0.99,
                           0.16,
                           0.00,
                           0.00),
               Stat_Sig = c("Yes",
                            "Yes",
                            "No",
                            "Yes",
                            "Yes",
                            "No",
                            "Yes",
                            "Yes",
                            "Yes",
                            "Yes",
                            "Yes",
                            "No",
                            "No",
                            "Yes",
                            "Yes"))
  
  kable(chisq_results, 
        col.names = c("Variable","Chi-Square Statistic", "p-value", "Statistically Significant"))

  
# ANOVA for Feature Selection 
  
#To decide if any numerical variables should be kept in the attrition model, 
#I am using ANOVA to test if there is a significant difference between attrition and the numerical variables. 
#The null hypothesis of ANOVA is there is no difference between means. 
#The alternative hypothesis states there is a difference. 
#If the p-value of the ANOVA is greater than .05, we can reject the null hypothesis and keep the variables in the model. 
#Distance from home and monthly income will be tested. The other numeric variables are counts, so they cannot be evaluated with ANOVA. 
  
# One of the assumptions in ANOVA is equal variances, 
#so I'm using the Levene's Test to determine whether or not that assumption has been violated. 
#The null hypothesis for the Levene's Test is the variances are equal across groups. 
#The alternative hypothesis for the Levene's Test is the variances are not equal. 
#If the p-value of the Levene's Test is greater than .05, we can accept the null hypothesis and the equal variance assumption of ANOVA will be met. 


#Distance from home and monthly income are both statistically significant, so they will be kept in the attrition model. 


# ANOVA: Distance From Home and Attrition 
aov.res = aov(DistanceFromHome~Attrition, data = hr_data)
summary(aov.res)
leveneTest(aov.res)
oneway.test(DistanceFromHome~Attrition, data = hr_data, var.equal = FALSE)


aov.res = aov(MonthlyIncome~Attrition, data = hr_data)
summary(aov.res)
leveneTest(aov.res)
oneway.test(MonthlyIncome~Attrition, data = hr_data, var.equal = FALSE)


anova_results = 
  data.frame(Variable = c("Distance From Home",
                          "Monthly Income"),
             F_Stat = c(8.34,
                        55.99),
             P_value = c(0.00,
                         0.00),
             Levene_Test = c(0.048,
                             0.00),
             Stat_Sig = c("Yes",
                          "Yes"))

kable(anova_results , 
      col.names = c("Variable","F Statistic", "p-value", "Levene's Test p-value","Statistically Significant"))


## Splitting Into Test and Train Data 

#Creating a test and a train dataset to evaluate and test the model.  


set.seed(123)

index = createDataPartition(y=hr_data$Attrition, 
                                p = .8,list = FALSE )


train = hr_data[index,]
test  = hr_data[-index,]

# 
# ## Create the Model Control for Different Sampling Methods 
# 
# Since there is class imbalance in the target variable attrition, upsampling and downsampling will be used.
# Upsampling randomly replicate instances in the minority class, and downsampling randomly remove instances in the majority class. 
# Models will be trained with cross-validation only, upsampling, and downsampling to see how these methods will affect model performance. 


up.model.control = 
  trainControl(method = "repeatedcv", 
               repeats = 3, 
               number = 10, 
               classProbs = TRUE,
               sampling = "up", 
               summaryFunction = prSummary)


down.model.control = 
  trainControl(method = "repeatedcv", 
               repeats = 3, 
               number = 10, 
               classProbs = TRUE,
               sampling = "down", 
               summaryFunction = prSummary)

control = trainControl(method = "repeatedcv", 
               repeats = 3, 
               number = 10, 
               classProbs = TRUE,
               summaryFunction = prSummary)


# Random Forest Models 



## Random Forest with Cross-Validation Only 


set.seed(123)



rf.model = 
   train(Attrition ~ 
        BusinessTravel +
        EnvironmentSatisfaction +
        JobInvolvement +
        MonthlyIncome +
        JobSatisfaction +
        NumCompaniesWorked +
        OverTime +
        TotalWorkingYears +
        WorkLifeBalance +
        YearsAtCompany +
        YearsInCurrentRole +
        YearsSinceLastPromotion +
        Department +
        DistanceFromHome +
        JobRole +
        YearsWithCurrManager+ 
        PercentSalaryHike +
        TrainingTimesLastYear,
        data = train,
        method = "rf", 
        metric = "Recall",
        trControl = control)

rf.model


p1 = predict(rf.model, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p1), test$Attrition, positive = 'Yes', mode = 'everything')



## Random Forest with Upsampling 


set.seed(123)

rf.model.up = 
  train(Attrition ~ 
        BusinessTravel +
        EnvironmentSatisfaction +
        JobInvolvement +
        MonthlyIncome +
        JobSatisfaction +
        NumCompaniesWorked +
        OverTime +
        TotalWorkingYears +
        WorkLifeBalance +
        YearsAtCompany +
        YearsInCurrentRole +
        YearsSinceLastPromotion +
        Department +
        DistanceFromHome +
        JobRole +
        YearsWithCurrManager+ 
        PercentSalaryHike +
        TrainingTimesLastYear,
        data = train,
        method = "rf", 
        metric = "Recall",
        trControl = up.model.control)

rf.model.up

p2 = predict(rf.model.up, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p2), test$Attrition, positive = 'Yes', mode = 'everything')



## Random Forest with Downsampling 


set.seed(123)

rf.model.down = 
  train(Attrition ~ 
                  BusinessTravel +
                  EnvironmentSatisfaction +
                  JobInvolvement +
                  MonthlyIncome +
                  JobSatisfaction +
                  NumCompaniesWorked +
                  OverTime +
                  TotalWorkingYears +
                  WorkLifeBalance +
                  YearsAtCompany +
                  YearsInCurrentRole +
                  YearsSinceLastPromotion +
                  Department +
                  DistanceFromHome +
                  JobRole +
                  YearsWithCurrManager+ 
                  PercentSalaryHike +
                  TrainingTimesLastYear,
        data = train,
        method = "rf", 
        metric = "Recall",
        trControl = down.model.control)

rf.model.down

p3 = predict(rf.model.down, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p3), test$Attrition, positive = 'Yes', mode = 'everything')



# GBM Models 

## GBM with Cross-Validation Only 


set.seed(123)


gbm_fit <- train(Attrition ~ 
                  BusinessTravel +
                  EnvironmentSatisfaction +
                  JobInvolvement +
                  MonthlyIncome +
                  JobSatisfaction +
                  NumCompaniesWorked +
                  OverTime +
                  TotalWorkingYears +
                  WorkLifeBalance +
                  YearsAtCompany +
                  YearsInCurrentRole +
                  YearsSinceLastPromotion +
                  Department +
                  DistanceFromHome +
                  JobRole +
                  YearsWithCurrManager+ 
                  PercentSalaryHike +
                  TrainingTimesLastYear,
                data = train,
                method = "gbm",
                verbose = FALSE,
                metric = "Recall",
                trControl = control)

print(gbm_fit)

p4 = predict(gbm_fit, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p4), test$Attrition, positive = 'Yes', mode = 'everything')



## GBM with Upsampling 


set.seed(123)



up_fit <- train(Attrition ~ 
                  BusinessTravel +
                  EnvironmentSatisfaction +
                  JobInvolvement +
                  MonthlyIncome +
                  JobSatisfaction +
                  NumCompaniesWorked +
                  OverTime +
                  TotalWorkingYears +
                  WorkLifeBalance +
                  YearsAtCompany +
                  YearsInCurrentRole +
                  YearsSinceLastPromotion +
                  Department +
                  DistanceFromHome +
                  JobRole +
                  YearsWithCurrManager+ 
                  PercentSalaryHike +
                  TrainingTimesLastYear,
                data = train,
                method = "gbm",
                verbose = FALSE,
                metric = "Recall",
                trControl = up.model.control)

print(up_fit)

p5 = predict(up_fit, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p5), test$Attrition, positive = 'Yes', mode = 'everything')


## GBM with Downsampling 


set.seed(123)


down_fit <- train(Attrition ~ 
                  BusinessTravel +
                  EnvironmentSatisfaction +
                  JobInvolvement +
                  MonthlyIncome +
                  JobSatisfaction +
                  NumCompaniesWorked +
                  OverTime +
                  TotalWorkingYears +
                  WorkLifeBalance +
                  YearsAtCompany +
                  YearsInCurrentRole +
                  YearsSinceLastPromotion +
                  Department +
                  DistanceFromHome +
                  JobRole +
                  YearsWithCurrManager+ 
                  PercentSalaryHike +
                  TrainingTimesLastYear,
                data = train,
                method = "gbm",
                verbose = FALSE,
                metric = "Recall",
                trControl = down.model.control)

print(down_fit)


p6 = predict(down_fit, newdata=test, type="raw")
caret::confusionMatrix(as.factor(p6), test$Attrition, positive = 'Yes', mode = 'everything')


# # Model Performance Summary
# 
# Recall is used to assess model performance instead of accuracy because in this model it is important to minimize false negatives.
# If we falsely label too many employee as "no attrition", but they actually do leave ("yes attrition"), 
# we run the risk of omitting employees who will actually leave. This will be very costly, 
# and reducing false negatives will negate that risk. The gbm with downsampling model is the best performing model.
# It has a train recall of 0.7295 and a test recall of 0.7872. The other models fail to improve on the training recall score. 


model_summary = 
  data.frame(Model = c("Random Forest",
                       "Random Forest Upsampling",
                       "Random Forest Downsampling", 
                       "GBM",
                       "GBM Upsampling", 
                       "GBM Downsampling"),
             recall_training_score = c(.9986, 
                                       .9443,
                                       .7423,
                                       .9960,
                                       .8267,
                                       .7295),
             recall_test_score = c(.06383, 
                                   .34043,
                                   .7021,
                                   .1064,
                                   .6809,
                                   .7872))

kable(model_summary, 
      col.names = c("Model","Train Recall", "Test Recall"))


# Variable Importance of GBM Downsampling Model

#Monthly income and over time are the most important variables in the model. 
# Other important variables are related to work history and distance from the office. 


gbm.varimp = varImp(down_fit, scale = FALSE)
imp <- data.frame(importance = gbm.varimp$importance) %>%
  tibble::rownames_to_column(var = "variable")

imp %>% ggplot(aes(x = reorder(variable,Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#21918c", color = "black")+
  coord_flip() +
  labs(x = "Variables", y = "Variable importance")

  # Conclusion 
  

# 
# Sixteen percent of employees left the company. 
# 
# In the stacked bar charts, we saw employees who left were:
#   
#   - In Sales 
# - Traveled frequently 
# - Worked over time
# - Had low job satisfaction 
# - Had low environment satisfaction
# - Had bad work life balance 
# 
# Chi-square results revealed gender, education, and performance rating did not have a significant role in employee attrition. 
# 
# From Chi-square tests and ANOVA, statistically significant variables that affected an employee's decision to leave include:
# 
# - Monthly income
# - Distance from home
# - Business travel
# - Environment satisfaction
# - Job involvement
# - Job role
# - Job satisfaction
# - Over time
# - Stock option level
# - Work life balance  
# 
# GBM with downsampling performed the best in minimizing false negatives, 
# which will prevent us from overlooking employees that will actually leave. 
# According to the variable importance plot, monthly income and over time are critical in attrition.
# Other important variables are related to work history, and distance from the office.
# 
# To prevent attrition, the company could consider raising wages, foster a company culture that promotes work life balance, 
# and allow remote work so employees don't have long commutes, and in turn, permit flexible schedules that will aid in work life balance issues. 
# 
#   
  
  
  
  
  
  
  
  
  
  
  
  