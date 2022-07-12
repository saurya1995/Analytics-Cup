# Business Analytics WS 2019/2020
# Generalized Linear Mode"ls - Exercise
#install.packages("caTools")
require(caTools)

# Load the library tidyverse
library(tidyverse)
library(dplyr)
library(summarytools)
library(lubridate)
# Load for logistic regression model
library(tidymodels)
# Load for variable importance
#library(vip)

#install.packages("caTools")
library(yardstick)

set.seed(2021)
# Load the data from the file companies.csv
company_df = read_csv("C:/Users/saury/Desktop/BA Analytics Cup/companies.csv")
payments_df = read_csv("C:/Users/saury/Desktop/BA Analytics Cup/payments.csv")
physicians_df = read.csv("C:/Users/saury/Desktop/BA Analytics Cup/physicians.csv")


#Remove columns from df
payments_df <- subset( payments_df, select = -c(City_of_Travel,State_of_Travel,Country_of_Travel,Third_Party_Covered,Contextual_Information
                                                        ,Product_Code_2,Product_Code_3,Product_Type_2,Product_Type_3,Product_Name_2,Product_Name_3,
                                                        Product_Category_1,Product_Category_2,Product_Category_3) )
physicians_df <- subset( physicians_df, select = -c(First_Name, Province,Name_Suffix,License_State_3,License_State_4,License_State_5,Country,Middle_Name,
                                                    Last_Name,City,State,Zipcode))


#Added Ownership column in Physician df
Phywith_owner<- payments_df %>%
  select(Physician_ID,Ownership_Indicator) %>%
  filter(Ownership_Indicator=="Yes") %>% 
  group_by(Physician_ID) %>%
  distinct(Physician_ID)

Phywith_owner$Ownership_Indicator="Yes"

physicians_df<-merge(physicians_df, Phywith_owner, by.x = "id",  by.y = "Physician_ID", all.x=TRUE)
physicians_df <- physicians_df %>%
  mutate(Ownership_Indicator = if_else(is.na(Ownership_Indicator), "No", Ownership_Indicator))

# #Split physician to train and test   ----Execute it in Last stages
# physicians_df_train <- physicians_df %>%
#   select(everything())%>%
#   filter(set=="train")
# 
# physicians_df_test <- physicians_df %>%
#   select(everything())%>%
#   filter(set=="test")
# 
# #Split training data to physician_train/physician_test  ----Execute it at last
# set.seed(101)
# sample = sample.split(physicians_df[,1], SplitRatio = .80)
# physician_train = subset(physicians_df, sample == TRUE)
# physician_test  = subset(physicians_df, sample == FALSE)


#Features:

# Total Amount
physicians_df_totalDollar <- payments_df %>% group_by(Physician_ID) %>% summarise_at(vars(Total_Amount_of_Payment_USDollars), sum)
physicians_df <- merge(physicians_df, physicians_df_totalDollar, by.x = "id",  by.y = "Physician_ID")

#Charity
payments_df<-payments_df %>%
  mutate(
    Charity=factor(Charity, labels = c(0,1))
  )

charity_0<-payments_df %>% 
  select(Physician_ID,Charity) %>%
  filter(Charity=="0") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(No_Charity=count)

physicians_df <- merge(physicians_df, charity_0, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
remove(charity_0)
remove(charity_1)
#TODO : NA values with 0
#physicians_df %>%
#  mutate(No_Charity = if_else(is.na(No_Charity), '0', No_Charity))

charity_1<-payments_df %>% 
  select(Physician_ID,Charity) %>%
  filter(Charity=="1") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(Yes_Charity=count)

physicians_df <- merge(physicians_df, charity_1, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

head(physicians_df)

#Date 
payments_df$temp <- rep(1,nrow(payments_df))
head(payments_df$temp)
Year <-format(as.Date(payments_df$Date, format="%m/%d/%Y"),"%Y")
nr_py <- aggregate(temp ~ Physician_ID + Year, data=payments_df, FUN=sum)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2013") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2013=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2014") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2014=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2015") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2015=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2016") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2016=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2017") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2017=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2018") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2018=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2019") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2019=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

head(physicians_df)

#Primary Speciality
physicians_df$Primary_Specialty <- as.factor(physicians_df$Primary_Specialty)

#No of Payments
physicians_df_totalNrPayments <- payments_df %>% group_by(Physician_ID) %>% summarise_at(vars(Number_of_Payments), sum)
physicians_df <- merge(physicians_df, physicians_df_totalNrPayments, by.x = "id",  by.y = "Physician_ID")

#Third_Party_Recipient
Third_Party_Payment_No<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="No Third Party Payment") %>%
  group_by(Physician_ID) %>%
  summarise(No_Third_PPayment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_No, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)


Third_Party_Payment_Individual<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="Individual") %>%
  group_by(Physician_ID) %>%
  summarise(Individual_Payment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_Individual, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

Third_Party_Payment_Entity<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="Entity") %>%
  group_by(Physician_ID) %>%
  summarise(Entity_Payment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_Entity, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

#Total no of Transactions
nr_py <- aggregate(temp ~ Physician_ID, data=payments_df, FUN=sum) %>% rename(Total_No_Transactions=temp)
physicians_df <- merge(physicians_df, nr_py, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

#Remove unnecessary variables from the memory
remove(charity_0)
remove(charity_1)
remove(nr_py)
remove(rest)
remove(Third_Party_Payment_No)
remove(Third_Party_Payment_Individual)
remove(Third_Party_Payment_Entity)
remove(physicians_df_totalNrPayments)
remove(physicians_df_totalNrPayments)
remove(Phywith_owner)
#########################################################Related Product Indicator##################################################################
#Recode to Yes/No/Combined
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "None"] <- "No"
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "Non-Covered"] <- "Yes"
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "Covered"] <- "Yes"

#factorize to 1 = Combination, 2 = No, 3 = Yes
payments_df$Related_Product_Indicator <- factor(payments_df$Related_Product_Indicator)

payments_df$Related_Product_Indicator <- as.numeric(payments_df$Related_Product_Indicator)

#payments_df$Related_Product_Indicator <-recode(payments_df$Related_Product_Indicator, 'No'=0, 'Yes'=1, 'Combined'=2)
payments_df$Related_Product_Indicator <- as.numeric(payments_df$Related_Product_Indicator)

#add Combination column to physician dataframe
payments_relatedPI_Combination<-payments_df %>%
  select(Physician_ID,Related_Product_Indicator) %>%
  filter(Related_Product_Indicator=="1") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(RelatedPI_Combined=count)

physicians_df <- merge(physicians_df, payments_relatedPI_Combination, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$RelatedPI_Combined <- physicians_df$RelatedPI_Combined %>% replace(is.na(.), 0)

#add No column to physician dataframe
payments_relatedPI_No<-payments_df %>%
  select(Physician_ID,Related_Product_Indicator) %>%
  filter(Related_Product_Indicator=="2") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(RelatedPI_No=count)

physicians_df <- merge(physicians_df, payments_relatedPI_No, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$RelatedPI_No <- physicians_df$RelatedPI_No %>% replace(is.na(.), 0)

#add Yes column to physician dataframe
payments_relatedPI_Yes<-payments_df %>%
  select(Physician_ID,Related_Product_Indicator) %>%
  filter(Related_Product_Indicator=="3") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(RelatedPI_Yes=count)

physicians_df <- merge(physicians_df, payments_relatedPI_Yes, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$RelatedPI_Yes <- physicians_df$RelatedPI_Yes %>% replace(is.na(.), 0)


#cash
payments_form_cash<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value=="Cash or cash equivalent") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_cash=count)
physicians_df <- merge(physicians_df, payments_form_cash, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_cash[is.na(physicians_df$payments_form_cash)] <- 0

#In-kind items
payments_form_inkindItems<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value=="In-kind items and services") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_inkindItems=count)
physicians_df <- merge(physicians_df, payments_form_inkindItems, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_inkindItems[is.na(physicians_df$payments_form_inkindItems)] <- 0


#other payment type
payments_form_other<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value!="In-kind items and services" & Form_of_Payment_or_Transfer_of_Value!= "Cash or cash equivalent") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_other=count)
physicians_df <- merge(physicians_df, payments_form_other, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_other[is.na(physicians_df$payments_form_other)] <- 0

#114
company_id_114<-payments_df %>%
  select(Physician_ID,Company_ID) %>%
  filter(Company_ID=="114") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_id_114=count)
physicians_df <- merge(physicians_df, company_id_114, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_id_114[is.na(physicians_df$company_id_114)] <- 0

#772
company_id_772<-payments_df %>%
  select(Physician_ID,Company_ID) %>%
  filter(Company_ID=="772") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_id_772=count)
physicians_df <- merge(physicians_df, company_id_772, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_id_772[is.na(physicians_df$company_id_772)] <- 0


#other id
company_id_other<-payments_df %>%
  select(Physician_ID,Company_ID) %>%
  filter(Company_ID!="772" & Company_ID!="114") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_id_other=count)
physicians_df <- merge(physicians_df, company_id_other, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_id_other[is.na(physicians_df$company_id_other2)] <- 0



####################################################################################################################################################
#Remove unnecessary variables from the memory
remove(charity_0)
remove(charity_1)
remove(nr_py)
remove(rest)
remove(Third_Party_Payment_No)
remove(Third_Party_Payment_Individual)
remove(Third_Party_Payment_Entity)
remove(physicians_df_totalNrPayments)
remove(physicians_df_totalNrPayments)
remove(Phywith_owner)
remove(payments_relatedPI_Yes)
remove(payments_relatedPI_No)
remove(payments_relatedPI_Combination)
remove(physicians_df_totalDollar)
remove(payments_form_cash)
remove(payments_form_inkindItems)
remove(payments_form_other)
remove(company_id_other)
remove(company_id_772)
remove(company_id_114)

physicians_df$Primary_Specialty <- as.factor(physicians_df$Primary_Specialty)
physicians_df$License_State_1 <- as.factor(physicians_df$License_State_1)
physicians_df$License_State_2 <- as.factor(physicians_df$License_State_2)

colnames(physicians_df)
physicians_df <- physicians_df %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
#physicians_df <- addNA(physicians_df$Primary_Specialty)


#Separate test and train set
physicians_train <- physicians_df %>%
  select(everything())%>%
  filter(set=="train")

physicians_test <- physicians_df %>%
  select(everything())%>%
  filter(set=="test")

physicians_train$Ownership_Indicator <- as.factor(physicians_train$Ownership_Indicator)
physicians_test$Ownership_Indicator <- as.factor(physicians_test$Ownership_Indicator)

physicians_test <- subset( physicians_test, select = -c(set))
physicians_train <- subset( physicians_train, select = -c(set))


splits <-physicians_train %>% initial_split(prop = 0.80)

splits

# <Analysis/Assess/Total>
#  <4001/999/5000>
# 4001 record for train
# 999 record for test

#Logistic Regression Model Version 1
# "glm" is the type of engine that comes defautl with R without loading extra packages.
# Primary_Specialty + License_State_1 + License_State_2

# summary(physicians_train)

#These factor columns removed: Primary_Specialty + License_State_1 + License_State_2
# model_fit_glm <- logistic_reg() %>% set_engine("glm") %>%
#   fit(as.factor(Ownership_Indicator) ~  Total_Amount_of_Payment_USDollars 
#       + No_Charity + Yes_Charity + Year_2013 + Year_2014 + Year_2015 + Year_2016 + Year_2017 + Year_2018 + Year_2019
#       + Number_of_Payments + No_Third_PPayment + Individual_Payment + Entity_Payment + Total_No_Transactions  , data = physicians_train )
# 
# model_fit_glm
# 
# #this line predicts our class probability for the testing data set
# prediction_class_test <- predict(model_fit_glm, new_data = testing(splits), type = "class")
# #View(prediction_class_test)
# 
# prediction_prob_test <- predict(model_fit_glm, new_data = testing(splits), type = "prob")
# #View(prediction_prob_test)
# 
# results_tbl <- bind_cols(prediction_class_test, prediction_prob_test, testing(splits))
# results_tbl
# 
# results_tbl %>% roc_auc(factor(Ownership_Indicator), .pred_No)
# results_tbl %>% roc_curve(factor(Ownership_Indicator), .pred_No) %>% autoplot(
#   options = list(
#     smooth = TRUE
#   )
# )

#Version 2 (Tidyverse Models, Recipes, etc.)

#Prepare recipe for preprocessing 
#Primary_Specialty  --> Error: Missing data in columns: Primary_Specialty
#License_State_2
#+ License_State_1
rec <- recipe(
  Ownership_Indicator ~  Total_Amount_of_Payment_USDollars  + 
  + Number_of_Payments + No_Third_PPayment + Individual_Payment + Entity_Payment + Total_No_Transactions + RelatedPI_Combined
  + RelatedPI_No + RelatedPI_Yes + payments_form_cash + company_id_114 + company_id_772 + company_id_other
  , data = physicians_train) %>% 
  #update_role(id, new_role = "ID") %>% 
# step_mutate_at(where(is.Date), fn=decimal_date) %>%    
  step_mutate_at (where(is.integer),
                 fn= ~replace_na(.,0)) %>%
  # impute all other numeric columns with their mean
  step_meanimpute(all_numeric()) %>% 
  
  # determine what happens when a new nominal value is encountered in test data (which was missing from the trianing set)
  #step_novel(all_nominal(), -has_role("ID"), new_level="new") %>% 
  
  # impute all other nominal (character + factor) columns with the value "none"
  #step_unknown(all_nominal(), new_level = "none") %>% 
  
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # remove constant columns
  step_zv(all_predictors())

rec
#Preprocessed data
physicians_train_preprocessed <- rec %>% prep() %>% juice()

rec %>% prep() %>% bake(testing(splits))

folds <- physicians_train_preprocessed %>% vfold_cv(v=5)


#Random forest Model Start
rf_model <- 
  rand_forest() %>%
  set_args(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_model

rf_workflow <- 
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf_model)

rf_workflow

#tuning layer (we specify the range of mtry values we want to try)
rf_grid <- expand.grid(mtry = c(3, 4, 5))

# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = folds, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(yardstick::accuracy, yardstick::roc_auc, yardstick::bal_accuracy) # metrics we care about
  )

rf_tune_results %>%
  collect_metrics()

param_final <- rf_tune_results %>%
  select_best(metric = "bal_accuracy")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(splits)

rf_fit

rf_test_performance <- rf_fit %>% collect_metrics()
rf_test_performance

rf_test_predictions <- rf_fit %>% collect_predictions()
rf_test_predictions

rf_test_predictions %>% 
  conf_mat(truth = Ownership_Indicator, estimate = .pred_class)

rf_test_predictions <- rf_fit %>% pull(.predictions)
rf_test_predictions

rf_final_model <- fit(rf_workflow, physicians_train_preprocessed)
rf_final_model

#predict our new test data
rf_predicted <- predict(rf_final_model, new_data = physicians_test)

#variable importance
rf_ranger_obj <- pull_workflow_fit(rf_final_model)$fit
rf_ranger_obj
rf_ranger_obj$variable.importance

rf_test_results <- physicians_test %>% select(id) 

rf_test_results$Ownership_pred <- rf_predicted
#View(rf_test_results)
#rf_test_results

rf_test_results$Ownership_pred <- ifelse(rf_test_results$Ownership_pred=="Yes", 1, 0)


#summary(rf_test_results)

names(rf_test_results)[1] <- "id"
names(rf_test_results)[2] <- "prediction"
#write.csv(rf_test_results,"C:/Users/Baþak/Desktop/TUM/Business Analytics/AC/rf_test_results.csv", row.names= FALSE)

colnames(physicians_df)

#Random forest Model End


# 
# #Features completed:
# 1. Ownership
# 2. Charity
# 3. Date
# 4. Primary Speciality
# 5. No of Payments
# 6. Related Product Indicator
# 7. Third_Party_Recipient
# 8. Total no of Transactions

#TODO: Fill all the NA Values




