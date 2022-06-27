#necessary library
library(data.table) #fread
library(lubridate)
library(reshape2)
library(dplyr)
library(scales)
library(stringr)
library(tidyr) # spread function

#memory.limit(size=900000) #Windows-specific #JO

YEARLIST =("19")
MONTHLIST = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11","12")
DISTANCE_FILEPATH = "../../data/tidy/vehicle-trajectory-computation/"
COMPUTATION_FILEPATH = "../../data/tidy/"
DISTANCE_FILEPATH = "../../data/tidy/vehicle-trajectory-computation/"
COMPUTATION_FILEPATH = "../../data/tidy/"

NUM_SAMPLE_DAYS = 10
NUM_SPEED_BINS = 6
NUM_ACCEL_BINS = 6

# Pick random days in each month and create the sample table for bins computation
sample_day_trajectories = function(year, month, num_sample_days, seed_val){
    # Read the trajectory computation data
    assign("dg",fread(paste0(DISTANCE_FILEPATH, paste(paste("green", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    assign("dh",fread(paste0(DISTANCE_FILEPATH, paste(paste("heavy", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    dg$lineid = 4
    dg = subset(dg, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    dh = subset(dh, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    # dg = dg[, c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid)]
    # dh = dh[, c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid)]
    DT = rbind(dg, dh) 
    if (any(list("04", "06", "09", "11") %like% month)) { #for efficiency filter for months with 30 days
        num_days = 30
    } else if (month == "02") { # february
        num_days = 28 
    } else {
        num_days = 31
    }
    DT <- DT[, day:=as.integer(day)]
    set.seed(seed_val)
    day_subset = sample(seq(num_days), num_sample_days)
    setkey(DT, day)
    DT = DT[day %in% day_subset]
    print(paste("Year:", year, "; Month:", month, "; Days sampled:"))
    print(unique(DT$day))
    remove(dg)
    remove(dh)
    return(DT)
}

# Unit conversion
convert_units = function(df){
    df$hour = hour(df$trxtime)
    df$speed_mph = df$speed_kph*0.621371 #kph to mph
    df$distance_mile = df$dist_meters*0.000621371 #convert from meters to mile
    df$time_hr = df$interval_seconds/3600.0 #convert from seconds to hour
    df
    return(df)
}    

# Calculate the speed bins 
get_speed_cutpoints <- function (DT, num_bins, test = FALSE) {
    print("Computing speed cutpoints")
    probabilities = seq(0, 1, 1/num_bins)
    cutpoints <- quantile(DT$speed_mph, probabilities, na.rm=TRUE) 
    df <- data.frame(cutpoints)
    print("Done")
    return(df) 
}

# Calculate the acceleration quantiles 
get_acceleration_cutpoints <- function (DT, num_bins, test = FALSE) {
    print("Computing acceleration cutpoints")
    probabilities = seq(0, 1, 1/num_bins)
    cutpoints <- quantile(DT$accel_mps2, probabilities, na.rm=TRUE) #read in the list from a saved file of cutpoints
    df <- data.frame(cutpoints)
    print("Done")
    return(df)
}

main <- function (num_speed_bins, num_accel_bins, num_days_to_sample, year_list, month_list,line_number){
    DT = data.table()
    SEEDLIST = c(111, 222, 333, 444, 555, 666, 777, 888, 999, 101010, 111111, 121212) #different seed for each month
    seed_counter = 1
    for (y in year_list) {
        for (m in month_list) {
            interval_DT = sample_day_trajectories(y, m, num_days_to_sample, SEEDLIST[seed_counter])
            DT = data.frame(DT)
            DT = rbindlist( list(DT, interval_DT) ) # obtain sampled df from all observations
            seed_counter = seed_counter + 1
            remove(interval_DT)
        }
    }
    DT = convert_units(DT) 
    DT = DT[lineid == line_number] # Subset the table by given lineid
    speed_cutpoints = get_speed_cutpoints(DT, num_speed_bins)
    acceleration_cutpoints = get_acceleration_cutpoints(DT, num_accel_bins)
    # Write the speed bin tables and acceleration tables to the computation path
    # write.csv(speed_cutpoints, file.path(paste0(COMPUTATION_FILEPATH, paste0(paste("speed", y,"cutpoints","line","specific","bins", num_speed_bins,line_number, sep = "-", collapse = ""), ".csv"))))
    write.csv(speed_cutpoints, file.path(paste0(COMPUTATION_FILEPATH, paste0(paste("speed", y,"cutpoints","bins", num_speed_bins,"line",line_number, sep = "-", collapse = ""), ".csv"))))
    write.csv(acceleration_cutpoints, file.path(paste0(COMPUTATION_FILEPATH, paste0(paste("acceleration", y, "cutpoints","line","specific","bins", num_accel_bins, line_number, sep = "-", collapse = ""), ".csv"))))
    remove(DT)
}

# Calculate the bins for each line
for (LINE_NUMBER in c(1,2,3,4)) {
    main(NUM_SPEED_BINS, NUM_ACCEL_BINS, NUM_SAMPLE_DAYS, YEARLIST, MONTHLIST, LINE_NUMBER)
}
