##########################################################
## Code written for McKinsey Analytics online hackathon ##
## Started on Jan 19, 2018 ended Jan 20, 2018           ##
## Public leaderboard: 77(84.7% AUC)                    ##
## Private Leaderboard: 92(83.5% AUC)                   ##
## Author: Samrudh Keshava Kumar                        ##
##########################################################

link_comp <- "https://datahack.analyticsvidhya.com/contest/mckinsey-analytics-online-hackathon-ii/pvt_lb"

setwd("C:/Users/Sam/Desktop/MCKinsey_sales_leads")

library(readr)
library(lubridate)
library(dplyr)
###Read in data 


trx_data <- function(data_df){
  ####Variable modification /creation 
  ###Change to date format
  data_df$DOB <- as.Date(data_df$DOB, format = "%d/%m/%y")
  data_df$age <- round(as.numeric(abs(data_df$DOB - Sys.Date()))/365, 0)
  #data_df$age_grp <- ifelse(data_df$age <= 25, 1, ifelse((data_df$age> 25 & data_df$age <=40), 2,3))
  data_df$birth_month <- lubridate::month(data_df$DOB)
  data_df$Lead_Creation_month <- lubridate::month(as.POSIXlt(data_df$Lead_Creation_Date, format = "%d/%m/%y"))
  data_df$Lead_Creation_week <- lubridate::week(as.POSIXlt(data_df$Lead_Creation_Date, format = "%d/%m/%y"))
  
  ###Variable for income surplass after existing EMI deduction
  data_df$Existing_EMI[is.na(data_df$Existing_EMI)] <- 0
  data_df$Income_surp <- data_df$Monthly_Income - data_df$Existing_EMI
  data_df$Income_surp[is.na(data_df$Income_surp)] <- data_df$Monthly_Income
  data_df$Income_surp_loan <- data_df$Income_surp - data_df$EMI 
  data_df$Income_emi_ratio <- data_df$Income_surp_loan/data_df$Income_surp
  data_df$Income_emi_ratio[is.na(data_df$Income_emi_ratio)] <- 0
  data_df$Income_surp_loan[is.na(data_df$Income_surp_loan)] <- data_df$Income_surp[is.na(data_df$Income_surp_loan)]
  ##Create log age variable 
  data_df$log.age <- log(data_df$age)
  data_df$Monthly_Income.log <- log(data_df$Monthly_Income)
  data_df$Monthly_Income.log[!is.finite(data_df$Monthly_Income.log)] <- 0
  ##No income
  data_df$NoIncome <- as.integer(data_df$Monthly_Income == 0)
  data_df$NoIncome[is.na(data_df$NoIncome)] <- 0
  
  data_df$Income_surp.log <- log(data_df$Income_surp)
  data_df$Income_surp.log[!is.finite(data_df$Income_surp.log)] <- 0
  
  data_df$Income_surp_loan.log <- log(data_df$Income_surp_loan)
  data_df$Income_surp_loan.log[!is.finite(data_df$Income_surp_loan.log)] <- 0
  
  #data_df$IncomeDivBy10 <- as.integer(data_df$Monthly_Income %% 10 == 0)
  #data_df$IncomeDivBy100 <- as.integer(data_df$Monthly_Income %% 100 == 0)
  data_df$IncomeDivBy1000 <- as.integer(data_df$Monthly_Income %% 1000 == 0)
  data_df$IncomeDivBy5000 <- as.integer(data_df$Monthly_Income %% 5000 == 0)
  
  
  ###Few EDA plots
  #plot(data_df$Monthly_Income, data_df$age)
  #plot(data_df$Loan_Amount, data_df$age)
  #plot(data_df$Loan_Amount, data_df$Lead_Creation_week)
  
  ##replace NA emp category with 5
  data_df$Employer_Category2_NA <- ifelse(sapply(data_df$Employer_Category2, is.na) ==    TRUE,1,0)
  data_df$Employer_Category2[is.na(data_df$Employer_Category2)] <- 5
  
  ##replace city_code NA with C999
  data_df$City_Code_NA <- ifelse(sapply(data_df$City_Code, is.na) ==    TRUE,1,0)
  data_df$City_Code[is.na(data_df$City_Code)] <- "C999"
  ##replace city category NA with D
  data_df$City_Category_NA <- ifelse(sapply(data_df$City_Category, is.na) ==    TRUE,1,0)
  data_df$City_Category[is.na(data_df$City_Category)] <- "D"
  ###replace employer_code NA with COM999
  data_df$Employer_Code_NA <- ifelse(sapply(data_df$Employer_Code, is.na) ==    TRUE,1,0)
  data_df$Employer_Code[is.na(data_df$Employer_Code)] <- "COM999"
  ###Replace Emp cat 1 N with D
  data_df$Employer_Category1_NA <- ifelse(sapply(data_df$Employer_Category1, is.na) ==    TRUE,1,0)
  data_df$Employer_Category1[is.na(data_df$Employer_Category1)] <- "D"
  ###Replace Customer Primary Bank Code with B999
  data_df$Customer_Existing_Primary_Bank_Code_NA <- ifelse(sapply(data_df$Customer_Existing_Primary_Bank_Code, is.na) ==    TRUE,1,0)
  data_df$Customer_Existing_Primary_Bank_Code[is.na(data_df$Customer_Existing_Primary_Bank_Code)] <- "B999"
  ###Replace Primary Bank type as Z
  data_df$Primary_Bank_Type_NA <- ifelse(sapply(data_df$Primary_Bank_Type, is.na) ==    TRUE,1,0)
  data_df$Primary_Bank_Type[is.na(data_df$Primary_Bank_Type)] <- "Z"
  
  ###Impute values for Loan_amount
  data_df$Loan_Amount[is.na(data_df$Loan_Amount)] <- -999
  data_df$Loan_Period[is.na(data_df$Loan_Period)] <- -10
  data_df$Interest_Rate[is.na(data_df$Interest_Rate)] <- -9.9
  data_df$EMI[is.na(data_df$EMI)] <- -999
  ###converting codes with char to just numeric
  data_df$City_Code <- as.numeric(substring(data_df$City_Code, 2))
  data_df$City_Category <- ifelse(data_df$City_Category == "A", 4, 
                                   ifelse(data_df$City_Category == "B", 3, 
                                          ifelse(data_df$City_Category == "C", 2,1)))
  
  
  data_df$Employer_Code <- as.numeric(substring(data_df$Employer_Code, 4))
  
  data_df$Employer_Category1 <- ifelse(data_df$Employer_Category1 == "A", 4, 
                                        ifelse(data_df$Employer_Category1 == "B", 3, 
                                               ifelse(data_df$Employer_Category1 == "C", 2,1)))
  
  
  
  data_df$Customer_Existing_Primary_Bank_Code <- as.numeric(substring(data_df$Customer_Existing_Primary_Bank_Code, 2))
  data_df$Primary_Bank_Type <- ifelse(data_df$Primary_Bank_Type == "P", 3, ifelse(data_df$Primary_Bank_Type == "G", 2, 1))
  
  data_df$Contacted <- ifelse(data_df$Contacted == "Y", 1,0)
  data_df$Source <- as.numeric(substring(data_df$Source, 2))
  data_df$Source_Category <- ifelse(data_df$Source_Category == "A", 7, 
                                     ifelse(data_df$Source_Category == "B", 6, 
                                            ifelse(data_df$Source_Category == "C", 5,
                                                   ifelse(data_df$Source_Category == "D", 4,
                                                          ifelse(data_df$Source_Category == "E", 3, 
                                                                 ifelse(data_df$Source_Category == "F", 2,1))))))
  data_df$Gender <- ifelse(data_df$Gender == "Female", 0,1)
  data_df$profit_bank <- ((data_df$Loan_Period * data_df$EMI*12)- data_df$Loan_Amount)/data_df$Loan_Period
  data_df$profit_bank[data_df$Loan_Amount == -999] <- -999
  data_df$profit_bank[data_df$EMI == -9] <- -9
  library(dummies)
  library(data.table)
  setDT(data_df)
  ##Create encoded variables
  cat_variables <- c("City_Category", "Gender", "Employer_Category1",
                     "Employer_Category2", "Primary_Bank_Type", "Contacted","Source_Category", "Var1")
  
  data_df <- dummy.data.frame(data_df, names = cat_variables, sep = "_")
  return (data_df)
}

#############################
## Reading in taining data ##
#############################

train_df <- read_csv("train.csv", na = c("", "NA"))
##Transforming train.csv and storing in train_df
train_df <- trx_data(train_df)

###Drop columns
drop_cols <- c("ID", "Lead_Creation_Date", "DOB")
train_df <- train_df[,!names(train_df) %in% drop_cols]

train_df<-na.omit(train_df)
summary(train_df)

###Move the Predicted variable to first position 
col_idx <- grep("Approved", names(train_df))
train_df <- train_df[, c(col_idx, (1:ncol(train_df))[-col_idx])]
names(train_df)

###########################################
## Reading in testing data and transform ##
###########################################

###Load test data 
test_df <- read_csv("test.csv")
###Transforming
test_df <- trx_data(test_df)
###Drop columns
drop_cols <- c("Lead_Creation_Date", "DOB")
test_df <- test_df[,!names(test_df) %in% drop_cols]

library(dummies)
library(data.table)
setDT(test_df)
cat_variables <- c("City_Category", "Gender", "Employer_Category1", 
                   "Employer_Category2", "Primary_Bank_Type", "Contacted","Source_Category", "Var1")

test_df <- dummy.data.frame(test_df, names = cat_variables, sep = "_")

summary(test_df)


####################################################END OF TEST DATA CREATION########################

######################################################Model Building#################################

###Random forest using H2O
install.packages("h2o")
library(h2o)

localH2O <- h2o.init(nthreads = -1) ## initialize h20 local instance to use all CPU cores

train.h2o <- as.h2o(train_df)
test.h2o <- as.h2o(test_df)
train.h2o[["Approved"]] <- as.factor(train.h2o[["Approved"]])

colnames(train.h2o)
colnames(test.h2o)

splits <- h2o.splitFrame(
  data = train.h2o, 
  ratios = c(0.6,0.2),
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 12345
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

#dependent variable (Response)
y.dep <- 1

#independent variables (dropping ID variables)
x.indep <- c(2:dim(train.h2o)[2])

gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = h2o.rbind(train, valid), 
                     ntrees = 100, learn_rate = 0.01, seed = 12345, max_depth = 10,
                     learn_rate_annealing = 0.99, nfolds = 5, sample_rate = 0.8,
                     col_sample_rate = 0.8, score_tree_interval = 10)

cv_prediction <- h2o.cross_validation_holdout_predictions(gbm.model)
cv_prediction_vect <- as.vector(cv_prediction)
## Show a detailed summary of the cross validation metrics gives an 
#idea of the variance between the folds
gbm.model@model$cross_validation_metrics_summary

## Get the AUC on the validation set
h2o.performance(gbm.model, test)
###
library(caTools)
test_pred <- predict(gbm.model, test)
colAUC(as.vector(test_pred[[1]]), as.vector(test[[1]])) ## out of sample accuracy of 84.34% to 85.45%

##################
## Deeplearning ##
##################
dlearning.model <- h2o.deeplearning(y = y.dep, 
                                    x = x.indep, 
                                    training_frame = h2o.rbind(train, valid), 
                                    epoch = 60, 
                                    hidden = c(10,10), 
                                    activation = "Rectifier", 
                                    seed = 1122) 
##Checking the performance of the deeplearning model
h2o.performance(dlearning.model) 

h2o.performance(dlearning.model, test) 

###############################################################################


###Make a prediction and write for submission 
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

out_gbm <- data.frame(ID = test_df$ID, Approved = predict.gbm$predict)

write.csv(out_gbm, file = "out_sub.csv", row.names = F)

df<-as.data.frame(h2o.varimp(gbm.model))

#################################END of predictions ####################


######################
## Parameter Tuning ##
######################

train.valid <- h2o.rbind(train,valid)

## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = c(4,5,6,7,8,9,10) ) ##faster for larger datasets

grid <- h2o.grid(
  hyper_params = hyper_params,
  
  search_criteria = list(strategy = "Cartesian"),  algorithm="gbm",  grid_id="depth_grid",
  
  ## standard model parameters
  x = x.indep, 
  y = y.dep, 
  training_frame = train.valid,  ntrees = 100,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.01,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths)) ##Best depth parameter for best AUC: 10
