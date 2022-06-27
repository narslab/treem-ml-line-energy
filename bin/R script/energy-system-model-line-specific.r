library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(caTools)
library(randomForest) # regression tree
library(car)
library(glmnet)

# input data - integrated with ridership, temperature and train movement variables
YEARLIST =c("19","20")
MONTHLIST = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11","12")
COMPUTATION_FILEPATH = "F:/data/tidy/trajectory-aggregation-line-specific/"
# Data input
aggregation_data_input = function(YEARLIST,MONTHLIST,lineid){
  df = data.table()
  for (y in YEARLIST) {
    for (m in MONTHLIST) {
      assign("d",fread(paste0(COMPUTATION_FILEPATH, paste(paste("trajectory", "aggregation" , y , m , lineid, sep = "-", collapse = ""), ".csv", sep=""))))
      df = rbind(df,d)
    }
  }
  df = df %>% rename_all(.,~sub("speed_bin_*", paste("S"), names(df)))
  df = df %>% rename_all(.,~sub("accel_bin_*", paste("A"), names(df)))
  return(df)
}
time_scale_create = function(df){
  df = df[-1,]
  df$Date = as.POSIXct(df$Date, format="%Y-%m-%dT%H:%M")
  df$year = year(df$Date)
  df$month = month(df$Date)
  df$hour = hour(df$Date)
  df$day = day(df$Date)
  # remove the date column for merging the table
  df = df[,-"Date"]
  return(df)
}
# Input the precipitation data and hourly temperature data
precipitation = fread("D:/Github/treem/data/raw/precipitation.csv", header = FALSE)
setnames(precipitation,c("Date", "Precipitation"))
precipitation = time_scale_create(precipitation)
hourly_TEMP = fread("D:/Github/treem/data/raw/hourly-temperature.csv", header = FALSE)
setnames(hourly_TEMP,c("Date", "Hourly_temperature"))
hourly_TEMP = time_scale_create(hourly_TEMP)


# Simplify the variables name
rename = function(df, lineid)
{
  colnames(df)[names(df) == 'avg_hour_speed_mph'] <- paste0('avg_hour_speed_mph_', lineid)
  colnames(df)[names(df) == 'avg_interval_speed_mph'] <- paste0('avg_interval_speed_mph_', lineid)
  colnames(df)[names(df) == 'distance_mile'] <- paste0('distance_mile_', lineid)
  colnames(df)[names(df) == 'time_hr'] <- paste0('time_hr_', lineid)
  names(df)[names(df) == lineid] = paste0('num_', lineid)  
  return(df)
}


# Integrate all data set into one single table
agg_data_1 = aggregation_data_input(YEARLIST,MONTHLIST,1)
agg_data_2 = aggregation_data_input(YEARLIST,MONTHLIST,2)
agg_data_3 = aggregation_data_input(YEARLIST,MONTHLIST,3)
agg_data_4 = aggregation_data_input(YEARLIST,MONTHLIST,4)
agg_data_1 = rename(agg_data_1, 1)
agg_data_2 = rename(agg_data_2, 2)
agg_data_3 = rename(agg_data_3, 3)
agg_data_4 = rename(agg_data_4, 4)
agg_data_1 = agg_data_1[,-"V1"]
agg_data_2 = agg_data_2[,-"V1"]
agg_data_3 = agg_data_3[,-"V1"]
agg_data_4 = agg_data_4[,-"V1"]
agg_data = merge(agg_data_1, agg_data_2, by = c("year","month","day","hour","ridership","weekends",
                                                "TAVG","energy_MWh"))
agg_data = merge(agg_data, agg_data_3, by = c("year","month","day","hour","ridership","weekends",
                                              "TAVG","energy_MWh"))
agg_data = merge(agg_data, agg_data_4, by = c("year","month","day","hour","ridership","weekends",
                                              "TAVG","energy_MWh"))
merged_precipitation = merge(agg_data,precipitation,by = c("year","month","day","hour"),all=F)
agg_data = merge(merged_precipitation,hourly_TEMP,by = c("year","month","day","hour"),all=F)
agg_data$Hourly_temperature = as.numeric(agg_data$Hourly_temperature)
agg_data$Precipitation = as.numeric(agg_data$Precipitation)
agg_data = agg_data[,-c("TAVG","weekends")]
agg_data$Date = with(agg_data, ISOdatetime(year,  month, day, hour, 0, 0))
agg_data = agg_data[!duplicated(agg_data)]

# interaction-term plot
interact_table = agg_data
interact_table = interact_table[,-(1:21)]
interact_table = interact_table[,-(37:53)]
interact_table = interact_table[,-(73:89)]
# data split for model estimation
set_split = function(df){
  # df$weekends = as.factor(df$weekends)
  df$num_trains = df$num_1 + df$num_2 + df$num_3 + df$num_4
  df$month_factor = as.factor(df$month)
  # df$TAVG_squared = I(df$TAVG^2)
  # remove all 0 variable
  #df = df[,-"speed_bin_1_accel_bin_6_time_hr"]
  set.seed(1234)
  train = df[year == 2019,]
  #     df_2019 = df[year == "2019",]
  #     rate = 0.8
  #     sub = sample(1:nrow(df_2019),round(nrow(df_2019)*rate)) # the split ratio is 0.8
  #     train = df_2019[sub,] # train set
  #     validation = df_2019[-sub,] # validation set
  test = df[year ==  2020,] # test set
  train[is.na(train)] <- 0
  return(list(df_train = train,
              #df_validate = validation,
              df_test = test))
}


dfh_train = set_split(agg_data)$df_train
# dfh_validation = set_split(agg_data)$df_validate
dfh_test = set_split(agg_data)$df_test
# Save all observations in one list and we will input the list into to RF function
name_list = names(dfh_train)
x_list = name_list[-c(1, 2, 3, 4, 5, 6)]
Random_forest_energy <- randomForest(reformulate(x_list,"energy_MWh"), data = dfh_train, mtry= 120, 
                                     ntree = 1000, na.omit = TRUE)

RF_pred = predict(Random_forest_energy, newdata = dfh_test[,-"energy_MWh"], na.action = pass)
actual = dfh_test$energy_MWh
# RMSE
sqrt(mean((RF_pred - actual)^2,na.rm = TRUE))
# MAPE
mean(abs((actual - RF_pred)/actual),na.rm =TRUE) * 100


# Find the best parameters
sub = sample(1:nrow(dfh_train), size = round(0.7*nrow(dfh_train)), replace=FALSE)
dfh_train_1 = dfh_train[sub,]

ntree_list = c(200,500,800,1000)
mtry_list = c(80, 100,120,140,160,180,200)
error_df = data.frame("mtry"= rep(NA, length(mtry_list)),"ntree" = rep(NA, length(mtry_list)))
error_df_results = data.frame()

# Create a dataframe for saving the hyper-parameters index
for (NTREE in ntree_list){
  error_df$ntree = NTREE 
  error_df$mtry = mtry_list 
  error_df_results = rbind(error_df_results,error_df)
}
error_df_results$oob_errors = NA
error_df_results$predict_errors= NA

# mtry is no of Variables randomly chosen at each split
for (NTREE in ntree_list)
{  
  for(MTRY in mtry_list) 
  {
    rf = randomForest(reformulate(x_list,"energy_MWh") , data = dfh_train_1,  mtry = MTRY, ntree = NTREE) 
    error_df_results$oob_errors[error_df_results$mtry == MTRY &
                                  error_df_results$ntree == NTREE] = sqrt(rf$mse[NTREE]) #Error of all Trees fitted    
    # train errors
    # pred = predict(rf,dfh_test,na.action = pass) # Predictions on Test Set for each Tree
    # error_df_results$predict_errors[error_df_results$mtry == MTRY &
    #                                   error_df_results$ntree == NTREE] = with(dfh_test, sqrt(mean((energy_MWh - pred)^2,na.rm = TRUE)))
    print(MTRY)
    print(NTREE)
  }
}
# Plot the OOB errors from Random Forests
ggplot(error_df_results,aes(x = factor(mtry), y = oob_errors, 
                            color = as.factor(ntree))) + 
  theme_bw() +
  geom_point(size = 2, alpha =  0.5) +
  geom_line(aes(group = as.factor(ntree)),size = 1.5) + 
  labs( x = "Number of variables selected as candidates at each split ", 
        y = "RMSE (MWh)", color = "Number of estimators") +
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size = 25))


error_df_results_melt = melt(error_df_results, id.vars = c("mtry","ntree"), 
                             measure.vars = c("oob_errors","predict_errors"))


# Plot the variable importance
RF_var_table = function(RF_result){
  RF_importance = data.frame(Random_forest_energy$importance)
  View(Random_forest_energy)
  RF_importance$ID = rownames(RF_importance)
  RF_importance = RF_importance[order(RF_importance$IncNodePurity,decreasing=TRUE,na.last=FALSE),]
  RF_importance$seq = 1:nrow(RF_importance)
  # calculate the cumulative significant plot
  RF_importance = RF_importance %>%
  mutate(cumsig = cumsum(IncNodePurity))
  RF_importance$prop = RF_importance$cumsig/sum(RF_importance$IncNodePurity)
  return(RF_importance)
}


RF_importance = RF_var_table(Random_forest_energy)

# plot the variable importance
ggplot(RF_importance) +
  theme_bw() +
  geom_line(aes(x = seq, y = cumsig/1000000, colour = prop < 0.96),size = 2) +
  theme(axis.text=element_text(size = 10), axis.title = element_text(size = 15)) +
  labs(x = "Variable index", y = "Cumulative significance (million)", color = "Proportion < 0.96")

# fill all NA with 0
dfh_train[is.na(dfh_train)] <- 0
# subset table by selected variables
RF_selected_var = RF_importance[RF_importance$prop < 1,]
RF_var_list = unlist(RF_selected_var $ID)
# train
y_train = dfh_train$energy_MWh
x_train = data.matrix(dfh_train[,..RF_var_list])
# test
y_test = dfh_test$energy_MWh
x_test = data.matrix(dfh_test[,..RF_var_list])
# Ridge regression
ridge_regression = function(x, y){
  energy_ridge_cv = cv.glmnet(x, y, standardize = TRUE, alpha = 0)
  best_lamda = energy_ridge_cv$lambda.min
  energy_ridge = glmnet(x, y, standardize = TRUE, alpha = 0, lambda = best_lamda) 
  return(list("final_model" = energy_ridge, "lamda" = best_lamda))
}

m = matrix(0, ncol = 2, nrow = 6)
result_table = data.frame(m)
prop_list = c(0.8,0.85,0.9,0.95,0.99,1)
result_table$prop = prop_list
result_table$R_squared = NA
for (p in prop_list){
  RF_selected_var = RF_importance[RF_importance$prop < p,]
  RF_var_list = unlist(RF_selected_var$ID)
  # train
  y_train = dfh_train$energy_MWh
  x_train = data.matrix(dfh_train[,..RF_var_list])
  # test
  y_test = dfh_test$energy_MWh
  x_test = data.matrix(dfh_test[,..RF_var_list])
  model = ridge_regression(x_train, y_train)$final_model
  best_lamda = ridge_regression(x_train, y_train)$lamda
  # Prediction
  pred <- predict(model, s = best_lamda, newx = x_train, na.action = pass)
  actual = y_train
  # R squared
  rss <- sum((pred - actual) ^ 2, rm.na = TRUE)
  tss <- sum((actual - mean(actual,rm.na =TRUE )) ^ 2, rm.na =TRUE)
  rsq <- 1 - rss/tss
  result_table[result_table$prop == p,"R_squared"] = rsq
}

ggplot(result_table)+
  theme_bw() +
  geom_point(aes(x = prop, y = R_squared), color = "violet") +
  geom_line(aes(x = prop, y = R_squared)) +
  theme(axis.text = element_text(size=15), 
        axis.title = element_text(size = 20)) +
  labs(x = "Variables proporation threshold", y = "R Squared")
# RMSE
sqrt(mean((pred - actual)^2,rm.na = TRUE))
# MAPE
mean(abs((actual - pred)/actual),na.rm =TRUE) * 100

test_compare = data.frame("Observed" = actual, "Predicted" = pred, 
                          "Residual" =  actual - pred)
test_compare = test_compare[which(test_compare$Observed > 0),]
mean(abs((test_compare$Observed - test_compare$s1 )/test_compare$Observed),na.rm =TRUE) * 100
#linear regression
energy_linear_model = lm(reformulate(RF_var_list,"energy_MWh"), data = dfh_train)
energy_linear_table = data.frame(summary(energy_linear_model)$coefficients)
energy_linear_table$ID = rownames(energy_linear_table)
energy_linear_table = energy_linear_table[energy_linear_table$Pr...t< 0.01,]
variable_linear_list_new = energy_linear_table$ID
variable_linear_list_new = variable_linear_list_new[-1]
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), 
                               data = dfh_train)
summary(energy_linear_model_fiter)
# Predict the energy model on the test set
pred <- predict(Random_forest_energy, newdata = dfh_test,na.action = pass)


ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 1,alpha = 0.4) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 1, alpha = 0.4) +
  #geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #                  position=position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))

# Predict the energy model on the test set
pred <- predict(Random_forest_energy, newdata = dfh_test,na.action = pass)
# Calculate the metrics
print(paste("RMSE:",sqrt(mean((dfh_test$energy_MWh - pred)^2,na.rm = TRUE))))
print(paste("MAPE:",mean(abs((dfh_test$energy_MWh - pred)/dfh_test$energy_MWh),na.rm =TRUE) * 100))
test_compare = data.frame("Observed" = dfh_test$energy_MWh, "Predicted" = pred, "Residual" =  dfh_test$energy_MWh - pred,
                          "Hour" = dfh_test$hour,"Year" = dfh_test$year,"Day"  = dfh_test$day,"Month" = dfh_test$month)
test_compare$Date = with(test_compare, ISOdatetime(Year,  Month, Day, Hour, 0, 0))
test_compare_melt = melt(test_compare,id.vars=c("Hour","Year","Day","Month","Date","Residual"),
                         measure.vars = c("Predicted","Observed"),variable.name = "type",value.name = "value")
ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 1,alpha = 0.4) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 1, alpha = 0.4) +
  #geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #                  position=position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
# Plot the residuals histogram
# Estimate the energy linear regression model
energy_linear_model = lm(reformulate(variable_linear_list,"energy_MWh"), data = dfh_train)
energy_linear_table = data.frame(summary(energy_linear_model)$coefficients)
View(energy_linear_table)
energy_linear_table$ID = rownames(energy_linear_table)
energy_linear_table = energy_linear_table[energy_linear_table$Pr...t< 0.01,]
variable_linear_list_new = energy_linear_table$ID
variable_linear_list_new = variable_linear_list_new[-1]
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
fix(variable_linear_list_new)
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
energy_linear_model_fiter_table = data.frame(summary(energy_linear_model_fiter)$coefficients)
energy_linear_model_fiter_table$ID = rownames(energy_linear_model_fiter_table)
energy_linear_model_fiter_table = energy_linear_model_fiter_table[-1,]
ggplot(energy_linear_model_fiter_table,aes(Estimate, reorder(ID, Estimate))) +
  geom_bar(stat="identity",fill="red",width = 0.2) +
  theme_bw()+
  theme(axis.text=element_text(size = 10),
        axis.title.x = element_text(size = 15),axis.title.y = element_blank()) +
  #scale_color_gradient(low="blue",high = "red") +
  labs(x = "Coefficients") +
  guides(color = FALSE)
summary(energy_linear_model_fiter)


length(energy_linear_model_filter_predict)
energy_linear_model_filter_predict = predict(energy_linear_model_fiter, data = dfh_test)

energy_linear_model_filter_predict = predict(energy_linear_model_fiter, data = dfh_test,)
energy_linear_model_filter_predict = predict(energy_linear_model_fiter, newdata = dfh_test)

energy_linear_model_filter_predict = predict(energy_linear_model_fiter, newdata = dfh_test,na.action = pass)
energy_linear_model_filter_predict = predict(energy_linear_model_fiter, newdata = dfh_test,na.action = na.pass)
print(paste("RMSE:",sqrt(mean((dfh_test$energy_MWh - energy_linear_model_filter_predict)^2,na.rm = TRUE))))
print(paste("MAPE:",mean(abs((dfh_test$energy_MWh - energy_linear_model_filter_predict)/dfh_test$energy_MWh),na.rm =TRUE) * 100))
energy_linear_model_filter_predict = predict(energy_linear_model, newdata = dfh_test,na.action = na.pass)
print(paste("RMSE:",sqrt(mean((dfh_test$energy_MWh - energy_linear_model_filter_predict)^2,na.rm = TRUE))))
print(paste("MAPE:",mean(abs((dfh_test$energy_MWh - energy_linear_model_filter_predict)/dfh_test$energy_MWh),na.rm =TRUE) * 100))
variable_linear = RF_importance[1:19,]
variable_linear_list = variable_linear$ID
# Estimate the energy linear regression model
energy_linear_model = lm(reformulate(variable_linear_list,"energy_MWh"), data = dfh_train)
energy_linear_table = data.frame(summary(energy_linear_model)$coefficients)
energy_linear_table$ID = rownames(energy_linear_table)
energy_linear_table = energy_linear_table[energy_linear_table$Pr...t< 0.01,]
variable_linear_list_new = energy_linear_table$ID
variable_linear_list_new = variable_linear_list_new[-1]
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
energy_linear_model_fiter_table = data.frame(summary(energy_linear_model_fiter)$coefficients)
energy_linear_model_fiter_table$ID = rownames(energy_linear_model_fiter_table)
energy_linear_model_fiter_table = energy_linear_model_fiter_table[-1,]


test_compare = data.frame("Observed" = dfh_test$energy_MWh, "Predicted" = energy_linear_model_filter_predict$energy_linear_model_filter_predict,
                          "Hour" = dfh_test$hour, "Year" = dfh_test$year, "Day"  = dfh_test$day, "Month" = dfh_test$month,
                          "Residual" =  dfh_test$energy_MWh - energy_linear_model_filter_predict$energy_linear_model_filter_predict)
test_compare$Date = with(test_compare, ISOdatetime(Year,  Month, Day, Hour, 0, 0))
test_compare_melt = melt(test_compare,id.vars=c("Hour","Year","Day","Month","Date","Residual"),
                         measure.vars = c("Predicted","Observed"),variable.name = "type",value.name = "value")
ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 0.5,alpha = 2) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 2, alpha = 2) +
  geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 0.5,alpha = 2) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 2, alpha = 2) +
  # geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #               position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 1,alpha = 0.4) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 1, alpha = 0.4) +
  #geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #                  position=position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
# NA values were replaced by 0: The trains are not operating at that hour, so the corresponding train movement variables are 0.
dfh_test[is.na(dfh_test)] <- 0
# Estimate the energy linear regression model
energy_linear_model = lm(reformulate(variable_linear_list,"energy_MWh"), data = dfh_train)
energy_linear_table = data.frame(summary(energy_linear_model)$coefficients)
energy_linear_table$ID = rownames(energy_linear_table)
energy_linear_table = energy_linear_table[energy_linear_table$Pr...t< 0.01,]
variable_linear_list_new = energy_linear_table$ID
variable_linear_list_new = variable_linear_list_new[-1]
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
energy_linear_model_fiter_table = data.frame(summary(energy_linear_model_fiter)$coefficients)
energy_linear_model_fiter_table$ID = rownames(energy_linear_model_fiter_table)
fix(energy_linear_model_fiter)
fix(variable_linear_list_new)
energy_linear_model_fiter = lm(reformulate(variable_linear_list_new,"energy_MWh"), data = dfh_train)
energy_linear_model_fiter_table = data.frame(summary(energy_linear_model_fiter)$coefficients)
energy_linear_model_fiter_table$ID = rownames(energy_linear_model_fiter_table)
energy_linear_model_fiter_table = energy_linear_model_fiter_table[-1,]
energy_linear_model_filter_predict = predict(energy_linear_model_fiter, newdata = dfh_test,na.action = na.pass)
energy_linear_model_filter_predict = data.frame(energy_linear_model_filter_predict)
View(energy_linear_model_filter_predict)
print(paste("RMSE:",sqrt(mean((dfh_test$energy_MWh - energy_linear_model_filter_predict)^2,na.rm = TRUE))))
print(paste("RMSE:",sqrt(mean((dfh_test$energy_MWh - energy_linear_model_filter_predict$energy_linear_model_filter_predict)^2,na.rm = TRUE))))
print(paste("MAPE:",mean(abs((dfh_test$energy_MWh - energy_linear_model_filter_predict$energy_linear_model_filter_predict)/dfh_test$energy_MWh),na.rm =TRUE) * 100))
test_compare = data.frame("Observed" = dfh_test$energy_MWh, "Predicted" = energy_linear_model_filter_predict$energy_linear_model_filter_predict,
                          "Hour" = dfh_test$hour, "Year" = dfh_test$year, "Day"  = dfh_test$day, "Month" = dfh_test$month,
                          "Residual" =  dfh_test$energy_MWh - energy_linear_model_filter_predict$energy_linear_model_filter_predict)
test_compare$Date = with(test_compare, ISOdatetime(Year,  Month, Day, Hour, 0, 0))
test_compare_melt = melt(test_compare,id.vars=c("Hour","Year","Day","Month","Date","Residual"),
                         measure.vars = c("Predicted","Observed"),variable.name = "type",value.name = "value")
ggplot(test_compare_melt) +
  ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 0.5,alpha = 2) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 2, alpha = 2) +
  # geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #               position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
View(test_compare)
MSE = sqrt(mean((test_compare$Observed - test_compare$Predicted)^2,na.rm = TRUE))
sqrt(mean((test_compare$Observed - test_compare$Predicted)^2,na.rm = TRUE))
mean(abs((test_compare$Observed - test_compare$Predicted)/test_compare$Observed),na.rm =TRUE) * 100
range(test_compare$Observed - test_compare$Predicted)
range((test_compare$Observed - test_compare$Predicted)/test_compare$Observed)
test_compare$diff = (test_compare$Observed - test_compare$Predicted)/test_compare$Observed
View(test_compare)
test_compare = test_compare[!diff == "-inf",]
test_compare = test_compare[!diff == -inf,]
range(test_compare$diff)
test_compare = test_compare["Observed" != 0,]
sqrt(mean((test_compare$Observed - test_compare$Predicted)^2,na.rm = TRUE))
print(MSE)
mean(abs((test_compare$Observed - test_compare$Predicted)/test_compare$Observed),na.rm =TRUE) * 100
test_compare = test_compare["Observed" != 0]
sqrt(mean((test_compare$Observed - test_compare$Predicted)^2,na.rm = TRUE))
mean(abs((test_compare$Observed - test_compare$Predicted)/test_compare$Observed),na.rm =TRUE) * 100
View(test_compare)
test_compare = test_compare[Observed != 0]
test_compare = test_compare["Observed" >0]
test_compare = test_compare["Observed" > 0, ]
View(test_compare)
test_compare = test_compare[,"Observed" > 0]
View(test_compare)
test_compare = subset(test_compare, "Observed" > 0)
View(test_compare)
test_compare = subset(test_compare, test_compare$Observed > 0)
sqrt(mean((test_compare$Observed - test_compare$Predicted)^2,na.rm = TRUE))
mean(abs((test_compare$Observed - test_compare$Predicted)/test_compare$Observed),na.rm =TRUE) * 100
test_compare_melt = melt(test_compare,id.vars=c("Hour","Year","Day","Month","Date","Residual"),
                         measure.vars = c("Predicted","Observed"),variable.name = "type",value.name = "value")
ggplot(test_compare_melt) +
  #ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 0.5,alpha = 2) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 2, alpha = 2) +
  # geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #               position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
png("../../figures/Linear_performance_0.9.png",height=2700,width=8000,res=360)



png("../../figures/Linear_performance_0.9.png",height=2700,width=8000,res=360)
ggplot(test_compare_melt) +
  #ylim(20,70) +
  geom_line(aes(x = Date, y = value, color = type)
            ,size = 0.5,alpha = 0.5) +
  geom_point(aes(x = Date , y = value, color = type)
             ,size = 2, alpha = 0.5) +
  # geom_errorbar(aes(x = Date, y = value, ymin = CI_lower, ymax=CI_upper, color = ""), width=.2,
  #               position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text = element_text(size=30), axis.title.x = element_blank(),title = element_text(size=35),
        legend.text = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(3, 'cm'),
        panel.grid = element_line(size = 0.9)) +
  labs(#title ="Random Forests",
    y = "Energy consumption (MWh)", x = "Date") +
  scale_x_datetime(date_breaks ="1 month",date_labels = ("%m/%y")) +
  scale_color_manual(values=c("#fc8d62", "#8da0cb"))
dev.off()
