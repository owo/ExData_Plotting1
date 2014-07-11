# Plot 2 for Project 1 of Exploratory Data Analysis course on Coursera.
# Author: Ossama W. Obeid (@owo)


# Loads and returns houshold power consumption data from the file indicated
# by 'datapath'. Dates are converted to Date objects, times are converted
# to POSIXct values, and '?' column values are converted to NA.
loadData <- function(datapath) {
    # Read data from file
    data <- read.csv(datapath, sep = ";", na.strings = "?");
    
    # Convert Time and Date fields to POSIXct and Date classes respectively.
    # Since converting to POSIXct requires a date as well, we use the Date
    # column values.
    times <- paste(data[,"Date"], data[,"Time"], sep = " ");
    data[,"Time"] <- as.POSIXct(strptime(times, "%d/%m/%Y %H:%M:%S"));
    data[,"Date"] <- as.Date(data[,"Date"], "%d/%m/%Y");
    
    data;
}


# Generates Plot 2 using houshold power consumption data from the file
# indicated by 'datapath', saving it to a PNG file indicated by 'plotpath'.
plot2 <- function(datapath, plotpath) {
    
    # Load data
    data <- loadData(datapath);
    
    # We only care about data from 2007-02-01 to 2007-02-02
    start_date = as.Date("2007-02-01", "%Y-%m-%d");
    end_date = as.Date("2007-02-02", "%Y-%m-%d");
    filter = data[,'Date'] >= start_date & data[,'Date'] <= end_date;
    data <- data[filter,];
    
    # Initialize PNG device
    png(plotpath, width = 480, height = 480, units = "px", bg="transparent");
    
    # Plot time-series
    plot(data[,"Time"],
         data[,"Global_active_power"],
         type = "l",
         col = "black",
         main = "",
         ylab = "Global Active Power (kilowatts)",
         xlab = "");
    
    # Close PNG device
    dev.off();
}


# Create Plot 2
plot2("household_power_consumption.txt", "plot2.png");
