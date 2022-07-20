library(reshape2)
library(dplyr) # Reshape the data table
library(caTools) # 
library(randomForest) # Random Forests function
library(data.table) # Read data table
library(lubridate) # Generate date columns
library(caret) # Splitting data set


agg_data = fread("aggregation_data.csv")

# data split for model estimation
set_split = function(df){
  # df$weekends = as.factor(df$weekends)
  df$num_trains = df$num_1 + df$num_2 + df$num_3 + df$num_4
  df$month_factor = as.factor(df$month)
  # df$TAVG_squared = I(df$TAVG^2)
  # remove all 0 variable
  #df = df[,-"speed_bin_1_accel_bin_6_time_hr"]
  set.seed(1234)
  # Define the partition (e.g. 40% of the data for training)
  trainIndex <- createDataPartition(df$energy_MWh, p = .40, 
                                    list = FALSE, 
                                    times = 1)
  
  # Split the dataset using the defined partition
  train_data <- df[trainIndex,]
  test_plus_val_data <- df[-trainIndex, ]
  # Test set and validation set will take 50% each
  test_plus_val_index <- createDataPartition(test_plus_val_data$energy_MWh,
                                             p = .50,
                                             list = FALSE,
                                             times = 1)
  val_data <- test_plus_val_data[test_plus_val_index,]
  test_data <- test_plus_val_data[-test_plus_val_index,]
  return(list(df_train = train_data,
              df_validate = val_data,
              df_test = test_data))
}
# prepare the train set, validation set and test set for oob errors computation process
dfh_train = set_split(agg_data)$df_train
dfh_validation = set_split(agg_data)$df_validate
dfh_test = set_split(agg_data)$df_test


# Save all observations in one list and we will input the list into to RF function
name_list = names(dfh_train)
x_list = name_list[-c(1, 2, 3, 4, 5, 6)]


ntree_list = c(200, 400, 600, 800, 1000)
mtry_list = c(80, 100, 120, 140, 160, 180, 200)
error_df = data.frame("mtry"= rep(NA, length(mtry_list)),
                      "ntree" = rep(NA, length(mtry_list)))
error_df_results = data.frame()

# Create a dataframe for saving the hyper-parameters index

# mtry is no of Variables randomly chosen at each split
for (NTREE in ntree_list)
{  
  for(MTRY in mtry_list) 
  {
    rf = randomForest(reformulate(x_list,"energy_MWh") , data = dfh_train,  mtry = MTRY, ntree = NTREE) 
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

write.csv(error_df_results,"random_forests_oob_errors.csv")