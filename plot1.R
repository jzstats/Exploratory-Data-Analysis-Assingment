################################################################################
# WHAT YOU NEED TO KNOW BEFORE EXECUTING THIS SCRIPT
################################################################################

# DESCRIPTION:  The script 'plot1.R', constructs and saves a bar-chart
#               of the 'Total Emissions from PM2.5 (in million of tons),
#               in the United States, over the years 1999, 2002, 2005 and 2008.

# THE QUESTION: The plot tries to answer the question:
#
#                   Have total emissions from PM2.5 decreased
#                   in the United States from 1999 to 2008?

# WORK-FLOW:    The script executes the following four STEPS:
#                 1. Downloads and extracts the data file if it doesn't exist.
#                 2. Loads the data file in R.
#                 3. Subsets the variables needed to construct plot.
#                 4. Creates the plot and saves it in the working directory.

# ABOUT THE DATA USED
# The data used to create the plot,
# comes from the 'National Emissions Inventory (NEI)' database.
# Specifically the table 'PM2.5 Emissions Data' (summarySCC_PM25.rds) was used,
# which can be downloaded from 'UC Irvine Machine Learning Repository' through this link:
#     "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
# The script will download the data file, if it doesn't exists in the working directory.
# More informations on the dataset can be found on the link:
#     https://www.epa.gov/air-emissions-inventories

# ABOUT THIS SCRIPT
# The script was created:
#   - in RStudio Version 1.1.383
#   - with R version 3.4.4
# and the data was downloaded at 24 April 2018.




# When the steps get executed informative messages, appear in console.
message("EXECUTING THE SCRIPT: 'plot1.R'")

################################################################################
# STEP 1: Downloads and extracts the data file if it doesn't exist
################################################################################

# The file 'summarySCC_PM25.rds',
# should be in the working directory for the STEP 2,
# when data will be loaded in R.
# If it doesn't exist, it is downloaded and extracted in this step.
# (informative messages explain the situation to the user)

if (!file.exists(file = "summarySCC_PM25.rds")) {
      
      # downloads the zipped data file as 'data.zip'
      message("The file summarySCC_PM25.rds ",
              "doesn't exists in the working directory. \n",
              "Trying to download the zipped data file ...")
      
      zip_data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      download.file(url = zip_data_url,
                    destfile = "data.zip")
      message("\t ... zipped data file was successfully downloaded.")
      
      # unzips the 'data.zip' file
      message("Trying to extract the data file...")
      unzip("data.zip")
      
      # removes the 'data.zip' file
      file.remove("data.zip")
      
      message("\t ... data file was successfully extracted ",
              "and the zipped file removed. \n",
              "The data file 'summarySCC_PM25.rds' ",
              "is now present in the working directory.")
}

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 1: Downloads and extracts the data file if it doesn't exist.")




################################################################################
# STEP 2: Loads the data file in R
################################################################################

#Data Table: 'PM2.5 Emissions Data'
NEI <- readRDS("summarySCC_PM25.rds")

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 2: Loads the data file in R.")




################################################################################
# STEP 3: Subsets the variables needed to construct plot
################################################################################

# Subsets the target variables.
step_to_target_data_1 <- subset(x = NEI,
                                select = c("year", "Emissions"))

# Summarizes the emissions per year.
step_to_target_data_2 <- with(data = step_to_target_data_1,
                              expr = tapply(X = Emissions,
                                            INDEX = year,
                                            FUN = sum))

# Changes the units of total emissions from tons to million of tons.
target_data <- step_to_target_data_2 / 1e6

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 3: Subsets the variable needed to construct plot.")




################################################################################
# STEP 4: Creates the plot and saves it in the working directory
################################################################################

# Opens a new png graphical device, with resolution 480x480 pixels.
png(filename = "plot1.png",
    width = 480,
    height = 480)


# Constructs the plot.

## sets the margins of the plot
par(mar = c(10, 5, 4, 5))

## creates the main body of the bar-chart
barplot(height = target_data,
        col = "brown",
        ylim = c(0, 8),
        las = 1)

## adds a title and descriptive labels to the plot
title(main = expression("Total Emissions from PM"[2.5] ~ "in the United States"),
      cex.main = 1.7,
      xlab = "Year",
      ylab = expression("Emissions from PM"[2.5] ~ " (in million of tons)"))

## adds the question of interest at the bottom of the plot
mtext(side = 1,
      line = 8,
      paste0("QUESTION 1: \n",
             "Have total emissions from PM2.5 decreased ",
             "in the United States from 1999 to 2008? \n",
             "(Using the base plotting system make a plot, ",
             "showing the total PM2.5 emission \n",
             "from all sources for each of the years",
             "1999, 2002, 2005, and 2008.)"))


# Closes the graphical device and saves the plot as 'plot1.png'.
# in the working directory.
dev.off()

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 4: Creates the plot and saves it in the working directory.")




# A message that is expected to appear, when the script had been sourced successfully.
message("SUCCESSfully created 'plot1.png' in the working directory.")
# THE END ######################################################################
