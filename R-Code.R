# clear workspace
rm(list = ls())

wd <- "/Users/tracie/Desktop/academic/goethe/2-SoSe25/Seminar-PMIM/[SUBMISSION]"
setwd(wd)

library(dplyr)
library(ggplot2)
library(xtable)
library(gmodels)
library(psych)
library(woe)
library(ggcorrplot)
library(corrplot)
library(ROSE)
library(caret)
library(pROC)
library(stargazer)
library(Matrix)
library(randomForest)
library(gbm)
library(xgboost)
library(doParallel)
library(gridExtra)
library(patchwork)



# read the data set-------------------------------------------------------------------------------



xsell <- get(load("customer_data.RData"))



# first glance at the dataset (initial univariate analyses)-----------------------------------------------------------------



head(xsell)
names(xsell)
summary(xsell)



# analysis of first variables-------------------------------------------------------------------------------



table(xsell$age)
table(xsell$xsell)

barplot(table(xsell$age))

table(xsell$age) # checking the distribution of age:xsell
table(xsell$xsell) # checking the distribution of 0:1 in xsell
table(xsell$age, xsell$xsell) # checking the distribution of age in 0,1 of xsell

# histogram of age-xsell
barplot(table(xsell$age)) #distribution of age 

#distribution of purchase by age  
ggplot(xsell, aes(x=factor(age), fill=factor(xsell))) +
  geom_bar(position="dodge") +
  labs(x="Age", fill="Purchase", y="Count", title="Distribution of Cross-buying Likelihood by Age") +
  scale_fill_manual(values = c("grey", "blue"), labels = c("Not Purchase", "Purchase")) +
  scale_y_continuous(breaks = seq(0, max(table(xsell$age)), by = 250)) +
  theme_minimal()


# data preprocessing & feature engineering ----------------------------------------------------------------------------------------------------



# check and modify missing values ----------------------------------------------------------------------



# check which variables have missing values
colSums(is.na(xsell))


# for 'vol_eur_inflows' (489 NA) & 'vol_eur_outflows' (489 NA), missing values are considered as no transaction
# therefore, replace missing values with 0
xsell$vol_eur_inflows[is.na(xsell$vol_eur_inflows)] <- 0
xsell$vol_eur_outflows[is.na(xsell$vol_eur_outflows)] <- 0

# for 'age_at_boarding' (3 NA), replace missing values with mean value
xsell$age_at_onboarding[is.na(xsell$age_at_onboarding)] <- round(mean(xsell$age_at_onboarding, na.rm = TRUE))


# for 'ext_house_size' (419 NA), 'ext_purchase_power' (464 NA), replace missing values with mean value
xsell$ext_house_size[is.na(xsell$ext_house_size)] <- round(mean(xsell$ext_house_size, na.rm = TRUE))
xsell$ext_purchase_power[is.na(xsell$ext_purchase_power)] <- round(mean(xsell$ext_purchase_power, na.rm = TRUE))


# check for missing values in the dataset again
colSums(is.na(xsell))

# there is 0 missing values 



# bivariate analyses with 'xsell' as dependent variable---------------------------------------------------------------------------------------------------- 


# 'xsell' vs 'age'
xsell_agg <- aggregate(xsell ~ age, data=xsell, FUN="mean")
# qplot(x=xsell_agg$age,y=xsell_agg$xsell,main="XSELL Likelihood; split by Customers' Age",
#       xlab="Age (years)", ylab="xsell", color=I("blue"))  + theme_gray(base_size = 18)
ggplot(xsell_agg, aes(x = age, y = xsell)) +
  geom_point(color = "blue", size = 1) +
  labs(
    x = "Customer Age",
    y = "Cross-buy",
    title = "Cross-buy Likelihood by Customer Age"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1),
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# 1. xsell vs number of app logins
xsell_app_agg <- aggregate(xsell ~ logins_app, data=xsell, FUN="mean")
p1 <- ggplot(xsell_app_agg, aes(x = logins_app, y = xsell)) +
  geom_point(color = "blue", size = 0.5) +
  labs(
    x = "Number of App logins",
    y = "Cross-buying",
    title = "Cross-buying Likelihood by Number of App Logins"
  ) +
  xlim(0, 100) +
  ylim(0, 0.2) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1),
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# 2. xsell vs number of total logins
xsell_total_agg <- aggregate(xsell ~ logins_total, data=xsell, FUN="mean")
p2 <- ggplot(xsell_total_agg, aes(x = logins_total, y = xsell)) +
  geom_point(color = "blue", size = 0.5) +
  labs(
    x = "Total Number of Logins",
    y = "Cross-buying",
    title = "Cross-buying Likelihood by Total Number of Logins"
  ) +
  xlim(0, 50) +
  ylim(0, 0.2) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1),
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# 3. xsell vs customer tenure
xsell_tenure_agg <- aggregate(xsell ~ customer_tenure_months, data=xsell, FUN="mean")
p3 <- ggplot(xsell_tenure_agg, aes(x = customer_tenure_months, y = xsell)) +
  geom_point(color = "blue", size = 0.5) +
  labs(
    x = "Customer Tenure in Months",
    y = "Cross-buying",
    title = "Cross-buying Likelihood by Customer Tenure"
  ) +
  xlim(0, 200) +
  ylim(0, 0.2) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1),
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# 4. xsell vs girocard transaction
xsell_giro_agg <- aggregate(xsell ~ nr_girocard_trx_90d, data=xsell, FUN="mean")
p4 <- ggplot(xsell_giro_agg, aes(x = nr_girocard_trx_90d, y = xsell)) +
  geom_point(color = "blue", size = 0.5) +
  labs(
    x = "Girocard Transactions",
    y = "Cross-buying",
    title = "Cross-buying Likelihood by Girocard Transactions"
  ) +
  xlim(1, 150) +
  ylim(0, 0.5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1),
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# Combine all four plots in a 2x2 grid
(p1 | p2) / (p3 | p4)





# create DummyVariable to handle categorical variable (marital status)
table(xsell$marital_status)
xsell$married <- ifelse(xsell$marital_status=="VH", 1, 0)
table(xsell$married)


# create an "overdraft" variable
# if the client has used the overdraft within 90 days
xsell$overdraft <- ifelse(xsell$nr_days_when_giro_below_0 >0, 1, 0)
table(xsell$overdraft)
# check how important that variable is
aggregate(xsell ~ overdraft, data=xsell, FUN="mean")


# create marital status dummies
#install.packages("psych")
status_dummies <- dummy.code(xsell$marital_status)

# transform to a factor variable 
status_dummies <-apply(status_dummies,FUN=as.factor,MARGIN=2)
xsell <- data.frame(xsell,status_dummies) 


# save the new variables
save(object = xsell, file = "xsell_cleaned.Rdata")

# load the clean dataset
xsell <- get(load("xsell_cleaned.RData"))

# check for missing values
colSums(is.na(xsell))

# there is no missing values



# descriptive statistics & correlations----------------------------------------------------------------------------------------------------------------------



# check correlations with all numerical variables

correlations <- round(cor(xsell[, c("acad_title","age", "complaints" , "customer_tenure_months",
                                    "vol_eur_inflows" ,   "logins_app" ,  "logins_total"     ,
                                    "member_get_member_recommender", "member_get_member_recommended",
                                    "nr_products" ,  "vol_eur_outflows"    ,
                                    "prod_mortgages"  ,"prod_brokerage","prod_savings" , "income",
                                    "nr_relocations" ,  "vol_eur_debit" , "vol_eur_credit",
                                    "nr_girocard_trx_90d",   "nr_visacard_trx_90d" ,
                                    "nr_days_when_giro_above_0" ,    "nr_days_when_giro_below_0" , "ext_city_size",
                                    "ext_house_size" , "ext_purchase_power","xsell")]),2)
print(correlations)

#ggcorrplot(correlations, hc.order=TRUE, type="full", outline.col="white")


# correlation plot
xsell_numeric <- xsell[sapply(xsell, is.numeric)]
xsell_numeric <- xsell_numeric[, !(names(xsell_numeric) %in% "nr_days_with_App_login_90d")]
correl_matrix<-cor(xsell_numeric,use="pairwise.complete.obs") # correlation matrix
corrplot(correl_matrix, method = "color", type = "upper", tl.cex = 0.8, number.cex = 0.7, tl.col = "black") 

# descriptive statistics
stargazer(xsell_numeric, type = "text", summary = TRUE)


# split and shuffle the data----------------------------------------------------------------------------------------------------



set.seed(12345) # fix random number generator seed for reproducibility
xsell_random <- xsell[order(runif(100000)),] #sort the data set randomly
xsell_valid <- xsell_random[1:20000, ]       # 20% / 2000 observations in the validation dataset
xsell_train <- xsell_random[20001:100000, ]   # 80% / 8000 in the training data set

# check for missing values in 2 datasets
colSums(is.na(xsell_train))
colSums(is.na(xsell_valid))

# there is no missing values in 2 datasets



# data sampling----------------------------------------------------------------------------------------------------



# checking for class imbalance in the target variable of training dataset
table(xsell_train$xsell)

# sampling 
set.seed(12345)
xsell_train_balanced <- ovun.sample(
  xsell ~ .,
  data = xsell_train,
  method = "both",
  p = 0.5,
  N = nrow(xsell_train),
  seed = 12345
)$data

# assign back to xsell_train
xsell_train <- xsell_train_balanced

# check new class distribution
table(xsell_train_balanced$xsell)

# there is no class imbalance in the training dataset now

# no attempt to address class imbalance in the test dataset to avoid overly optimistic model performance

# check for missing values in preprocessed training dataset
colSums(is.na(xsell_train))



# 10-fold cross validation settings------------------------------------------------------------------------------------------



ctrl <- trainControl(
  method = "cv",
  number = 10,
  search = "random",
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)



# evaluation metrics------------------------------------------------------------------------------------------



# function for evaluation metrics
get_metrics <- function(pred, pred_class, reference) {
  cm <- confusionMatrix(
    factor(pred_class, levels = c("0", "1")),
    factor(reference, levels = c("0", "1")),
    positive = "1"
  )
  tdl <- function(truth, prob) {
    n <- length(prob)
    top_10pct <- order(prob, decreasing = TRUE)[1:ceiling(n * 0.1)]
    mean(truth[top_10pct] == "1") / mean(truth == "1")
  }
  gini <- function(truth, prob) {
    roc_obj <- roc(truth, prob, levels = c("0", "1"), direction = "<")
    auc_value <- auc(roc_obj)
    gini <- 2 * auc_value - 1
    return(as.numeric(gini))
  }
  tdl_value <- tdl(reference, pred)
  gini_value <- gini(reference, pred)
  auc_value <- (gini_value + 1) / 2
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  accuracy <- as.numeric(cm$overall["Accuracy"])
  metrics <- c(
    TDL = tdl_value,
    GINI = gini_value,
    AUC = auc_value,
    F1 = f1 * 100,
    Precision = precision * 100,
    Recall = recall * 100,
    Accuracy = accuracy * 100,
    Specificity = specificity * 100
  )
  names(metrics) <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall",
                      "Accuracy", "Specificity")
  return(metrics)
}



# model training----------------------------------------------------------------------------------------------------



# create a model
model <- xsell ~  acad_title + age + gender + nr_calls_90d + complaints +
                  customer_tenure_months + #vol_eur_inflows + vol_eur_outflows +
                  #vol_eur_debit + vol_eur_credit +
                  logins_app + logins_total +
                  member_get_member_recommender + member_get_member_recommended +
                  nr_products + nr_relocations +
                  nr_girocard_trx_90d + nr_visacard_trx_90d + income +
                  overdraft + #nr_days_when_giro_above_0 +
                  #prod_mortgages + prod_brokerage + prod_savings +
                  ext_city_size + ext_house_size + ext_purchase_power +
                  married




# 1) RANDOM FOREST----------------------------------------------------------------------------------------------------



xsell_train$xsell <- as.factor(xsell_train$xsell)
xsell_train$xsell <- factor(xsell_train$xsell, levels = c(0, 1), labels = c("No", "Yes"))

# MODEL TRAINING

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123) 

rf_grid <- expand.grid(mtry=c(5, 10, 20, 30))

rf_model <- train(
  model,                 
  data = xsell_train,
  method = "rf",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = rf_grid,
  ntree = 50
)

stopCluster(cl)  

# RESULTS

xsell_valid$rf_prob <- predict(rf_model, newdata = xsell_valid, type = "prob")[, "Yes"]
xsell_valid$rf_pred <- ifelse(xsell_valid$rf_prob > 0.5, "1", "0")

rf_pred_factor <- factor(xsell_valid$rf_pred, levels = c("0", "1"))
rf_reference_factor <- factor(as.character(xsell_valid$xsell), levels = c("0", "1"))

cm_rf <- confusionMatrix(
  data = rf_pred_factor,
  reference = rf_reference_factor,
  positive = "1"
)

print(cm_rf)


rf_metrics <- get_metrics(
  pred = xsell_valid$rf_prob,                
  pred_class = xsell_valid$rf_pred,          
  reference = as.character(xsell_valid$xsell) 
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity")

rf_metrics_df <- data.frame(
  Metric = metrics,
  Value = round(as.numeric(rf_metrics), 3)
)

stargazer(rf_metrics_df, 
          summary = FALSE, rownames = FALSE,
          type = "text", digits = 3,
          title = "Random Forest Model Performance")



# 2) GRADIENT BOOSTING----------------------------------------------------------------------------------------------------



# check for missing values in preprocessed training dataset
colSums(is.na(xsell_train))


# MODEL TRAINING

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123) 

xgb_grid <- expand.grid(
  nrounds = 150,
  eta = 0.07,
  max_depth = 3,
  min_child_weight = 3,
  colsample_bytree = 1,
  subsample = 1,
  gamma = 0
)

xgb_model <- train(
  model, 
  data = xsell_train,
  method = "xgbTree",
  metric = "ROC",  
  trControl = ctrl,
  tuneGrid = xgb_grid
)

stopCluster(cl)  

# RESULTS

xsell_valid$xgb_prob <- predict(xgb_model, newdata = xsell_valid, type = "prob")[, "Yes"]
xsell_valid$xgb_pred <- ifelse(xsell_valid$xgb_prob > 0.5, "1", "0")

xgb_pred_factor <- factor(xsell_valid$xgb_pred, levels = c("0", "1"))
xgb_reference_factor <- factor(as.character(xsell_valid$xsell), levels = c("0", "1"))

cm_xgb <- confusionMatrix(
  data = xgb_reference_factor,
  reference = xgb_reference_factor,
  positive = "1"
)

print(cm_xgb)

xgb_metrics <- get_metrics(
  pred = xsell_valid$xgb_prob,
  pred_class = xsell_valid$xgb_pred,
  reference = as.character(xsell_valid$xsell)
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity")

xgb_metrics_df <- data.frame(
  Metric = metrics,
  Value = round(as.numeric(xgb_metrics), 3)
)

stargazer(xgb_metrics_df, 
          summary = FALSE, rownames = FALSE,
          type = "text", digits = 3,
          title = "XGBoost Model Performance")



# model evaluation----------------------------------------------------------------------------------------------------



# performance metrics

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity")

combined_metrics_df <- data.frame(
  Metric     = metrics,
  RandomForest = round(as.numeric(rf_metrics), 3),
  XGBoost      = round(as.numeric(xgb_metrics), 3)
)

stargazer(combined_metrics_df, 
          summary = FALSE, rownames = FALSE,
          type = "text", digits = 3,
          title = "Model Performance Evaluation")

stargazer(combined_metrics_df, 
          summary = FALSE, rownames = FALSE,
          type = "html", digits = 3,
          title = "Model Performance Evaluation")


# roc/auc

xsell_valid$xsell <- factor(xsell_valid$xsell, levels = c(0, 1), labels = c("No", "Yes"))

roc_rf  <- roc(response = xsell_valid$xsell, predictor = xsell_valid$rf_prob, levels = c("No", "Yes"), direction = "<")
roc_xgb <- roc(response = xsell_valid$xsell, predictor = xsell_valid$xgb_prob, levels = c("No", "Yes"), direction = "<")

auc_rf  <- round(auc(roc_rf), 3)
auc_xgb <- round(auc(roc_xgb), 3)

df_rf <- data.frame(
  tpr = rev(roc_rf$sensitivities),
  fpr = rev(1 - roc_rf$specificities),
  model = paste0("Random Forest (AUC = ", auc_rf, ")")
)

df_xgb <- data.frame(
  tpr = rev(roc_xgb$sensitivities),
  fpr = rev(1 - roc_xgb$specificities),
  model = paste0("XGBoost (AUC = ", auc_xgb, ")")
)

df_all <- rbind(df_rf, df_xgb)

ggplot(df_all, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves: Random Forest vs XGBoost",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# lift chart

rf_lift <- lift(xsell_valid$xsell ~ xsell_valid$rf_prob, data = xsell_valid, class = "Yes", cuts = 10)
xgb_lift <- lift(xsell_valid$xsell ~ xsell_valid$xgb_prob, data = xsell_valid, class = "Yes", cuts = 10)

rf_lift_df  <- as.data.frame(rf_lift$data)
rf_lift_df$model <- "Random Forest"

xgb_lift_df <- as.data.frame(xgb_lift$data)
xgb_lift_df$model <- "XGBoost"

lift_df <- rbind(rf_lift_df, xgb_lift_df)
str(lift_df)

ggplot(lift_df, aes(x = cuts, y = lift, color = model, group = model)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Lift Chart: Random Forest vs XGBoost",
    x = "Decile",
    y = "Lift",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# gain chart

xsell_valid <- xsell_valid %>%
  mutate(
    rf_decile = ntile(desc(rf_prob), 10),   
    xgb_decile = ntile(desc(xgb_prob), 10)
  )

gain_rf <- xsell_valid %>%
  arrange(desc(rf_prob)) %>%
  mutate(cum_positives = cumsum(xsell == "Yes")) %>%
  group_by(rf_decile) %>%
  summarise(gain = max(cum_positives) / sum(xsell == "Yes")) %>%
  mutate(model = "Random Forest")

gain_xgb <- xsell_valid %>%
  arrange(desc(xgb_prob)) %>%
  mutate(cum_positives = cumsum(xsell == "Yes")) %>%
  group_by(xgb_decile) %>%
  summarise(gain = max(cum_positives) / sum(xsell == "Yes")) %>%
  mutate(model = "XGBoost")

colnames(gain_rf)[1] <- "decile"
colnames(gain_xgb)[1] <- "decile"
gain_df <- bind_rows(gain_rf, gain_xgb)

ggplot(gain_df, aes(x = decile, y = gain, color = model, group = model)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Cumulative Gain Chart: Random Forest vs XGBoost",
    x = "Decile",
    y = "Cumulative Gain",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# model interpretation----------------------------------------------------------------------------------------------------



varImp(rf_model)
plot(varImp(rf_model), top = 20, main = "Random Forest - Variable Importance Measures")

varImp(xgb_model)
plot(varImp(rf_model), top = 20, main = "XGBoost - Variable Importance Measures")









