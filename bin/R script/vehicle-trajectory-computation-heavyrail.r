library(data.table) # Fread
library(dplyr)
library(geosphere) # Calculate vehicle distance
library(lubridate)# Date column extraction
library(robfilter)# Smooth the data

# Read data from folder
# Suggestion: Just do one year at one time. One year's table has 94363502 rows(green)
YEARLIST = c('19',"20")
MONTHLIST = c("01", "02", "03", "05", "07", "08", "09", "10", "11","12") # FOR FULL TABLE
DISTANCE_FILEPATH = "../../data/raw/vehicle-location/"

# Add different time scale columns
add_dd_mm_yy_cols = function(df) {
    df$day = day(df$trxtime)
    df$month = month(df$trxtime)
    df$year = year(df$trxtime)
    return(df)
}
# Read heavy rail location raw data
get_heavy_rail_trajectories = function(year, month){
    assign("dh", fread(paste(DISTANCE_FILEPATH, paste("heavyrail", "trajectories", month, year, ".csv", sep = "-", collapse = ""), sep="")))
    dh = add_dd_mm_yy_cols(dh)
    return(dh)
}

# Data preprocessing
preprocess_data = function(df){
    # check the na values ratio
    # df_zero = df[df$lon == 0 | df$lat == 0,]
    # df_zero_rate = nrow(df_zero)/nrow(df)
    # print(df_zero_rate)
    # remove the rows with values of lat and lon are 0/NA
    df = df[!(is.na(df$lon)) | !(is.na(df$lat)),]
    df = df[df$lon != 0 | df$lat != 0,]
    return(df)
}

# Subset table by a given day
get_day_trajectories = function(df, dayid){
    day_df = df[day == dayid, .SD, keyby = .(lineid, trainid) ]
    print(paste("Number of observations", nrow(day_df), "on day", dayid ))
    return(day_df)
}

# Create trajectory index table 
get_unique_trajectory_indices = function(day_df) {
    trajectory_index_df = unique(day_df[, .(trainid, vehicleid, lineid, heavyrailbranchid, tripid)])
    print(paste("Number of unique trajectories extracted: ", dim(trajectory_index_df)[1]))
    return(trajectory_index_df)
}

# Subset the raw table by unique index
extract_unique_trajectory = function(day_df, traj_index_df, index){
    day_df[["heavyrailbranchid"]][is.na(day_df[["heavyrailbranchid"]])] <- 9999999
    day_df[["tripid"]][is.na(day_df[["tripid"]])] <- 9999999
    day_df[["vehicleid"]][is.na(day_df[["vehicleid"]])] <- 9999999
    traj_index_df[["heavyrailbranchid"]][is.na(traj_index_df[["heavyrailbranchid"]])] <- 9999999
    traj_index_df[["tripid"]][is.na(traj_index_df[["tripid"]])] <- 9999999
    traj_index_df[["vehicleid"]][is.na(traj_index_df[["vehicleid"]])] <- 9999999
    trajectory = day_df[trainid == traj_index_df[index, 1][[1]] & 
                            vehicleid == traj_index_df[index, 2][[1]] &
                            lineid == traj_index_df[index, 3][[1]] &
                            heavyrailbranchid == traj_index_df[index, 4][[1]] &
                            tripid == traj_index_df[index, 5][[1]],][order(trxtime)]
    clean_trajectory = trajectory[, .SD[1], by = trxtime] # takes first observation of multiple with same time
    clean_trajectory[["heavyrailbranchid"]][clean_trajectory[["heavyrailbranchid"]] == 9999999] = NA
    clean_trajectory[["tripid"]][clean_trajectory[["tripid"]] == 9999999] = NA
    clean_trajectory[["vehicleid"]][clean_trajectory[["vehicleid"]] == 9999999] = NA
    # transfer trxtime to timestamp
    clean_trajectory$time = as.POSIXct(format(clean_trajectory$trxtime),tz = "EST")
    return (clean_trajectory)
}

# compute time interval
compute_time_interval <- function(d) {
    d$interval_seconds = NA
    n <- nrow(d)
    if (n >= 2) {
        # Compute time interval
        d$interval_seconds[2:n] = as.numeric(difftime(d$trxtime[2:n], d$trxtime[1:n-1], units = "secs"))
        }
    return(d)
}
# compute vehicle distance
compute_distance <- function(d) {
    d$dist_meters = NA
     n <- nrow(d)
    if (n >= 2) {
        # Compute interval distance using Haversine function
        d$dist_meters[2:n] = distHaversine(cbind(d$lon[1:n-1],d$lat[1:n-1]),cbind(d$lon[2:n],d$lat[2:n]))
        }
    return(d)
}
# compute speed and acceleration
compute_speed_acceleration <- function(d) {
    d$speed_mps = NA
    d$speed_kph = NA
    d$accel_mps2 = NA
    n <- nrow(d)
    if (n >= 2) {
        d$speed_mps[2:n] = d$dist_meters[2:n] / d$interval_seconds[2:n]
        # Convert speed to kph
        d$speed_kph[2:n] = d$speed_mps[2:n] * 3.6
        d$accel_mps2[2:n] = (d$speed_mps[2:n] - d$speed_mps[1:n-1])/d$interval_seconds[2:n]
        }
    return(d)
} 
# Calculate the cumulative dist and time
compute_cumulative_time_distance = function(d){
    df = d
    # no rm.na argument in cumsum function,so we make distance and time with NA as 0
    df[is.na(df)] <- 0
    # Calculate the cumulative dist and time
    df = df %>%
    mutate(cumdist = cumsum(dist_meters)) %>%
    mutate(cumtime = cumsum(interval_seconds))
    d$cumdist_km = df$cumdist/1000
    d$cumtime_hrs = df$cumtime/3600
    return(d)
}

# Remove the outlier speed
case_5 = function(clean_trajectory){
    clean_trajectory = data.table(clean_trajectory)
    clean_trajectory = compute_time_interval(clean_trajectory)
    # Remove short time interval observations
    clean_trajectory = clean_trajectory[interval_seconds > 1]
    clean_trajectory = compute_time_interval(clean_trajectory)
    clean_trajectory = compute_distance(clean_trajectory)
    clean_trajectory = compute_speed_acceleration(clean_trajectory)
    clean_trajectory = compute_cumulative_time_distance(clean_trajectory)
    # Remove outlier speed observations
    clean_trajectory = clean_trajectory[speed_kph < 170]
    clean_trajectory = clean_trajectory[accel_mps2 > -5 & accel_mps2 < 5]
    return(clean_trajectory)   
}

compute_day_trajectories = function(month_df, dd) {
    df_dd = get_day_trajectories(month_df, dd)
    traj_indices_dd = get_unique_trajectory_indices(df_dd)
    # print(head(traj_indices_dd))
    num_traj = nrow(traj_indices_dd)
    for (tt in seq(num_traj)) { # ideally this should be for the whole sequence
        traj = extract_unique_trajectory(df_dd, traj_indices_dd, tt)
        traj$trajid = tt # add a new column
        traj = case_5(traj) 
        if (tt==1) {
            processed_traj_df = traj
        } else {
            processed_traj_df = rbind(processed_traj_df, traj)
        }
    }
    return (processed_traj_df)
}

process_month_trajectory = function(data){
    results_df = data.frame() # empty dataframe
    for(i in unique(data$day)) { 
        day_df = compute_day_trajectories(data, i)       
        results_df <- rbind(results_df, day_df)
        }
 }

# Generate the final table
main = function(YEARLIST, MONTHLIST) {
    for (y in YEARLIST) {
        for (m in MONTHLIST) {
            df_heavy = get_heavy_rail_trajectories(y, m)
            df_heavy = preprocess_data(df_heavy)
            df_heavy = process_month_trajectory(df_heavy) 
            write.csv(x = df_heavy, 
                      file.path("../../data/tidy", 
                                paste(paste("heavy", "trajectory", y, m, sep = "-", collapse = ""), ".csv", sep="")))
        }
    }
}

main(YEARLIST, MONTHLIST)
