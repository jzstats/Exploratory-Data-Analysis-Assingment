################################################################################
# WHAT YOU NEED TO KNOW BEFORE EXECUTING THIS SCRIPT
################################################################################

# DESCRIPTION:  The script 'plot6.R', constructs and saves a multiplot,
#               that presents some core aspects of the changes in emissions
#               over time, only for motor vehicle sources,
#               between Baltimore City and Los Angeles County.

# THE QUESTION: The plot tries to answer the question:
#
#                   Compare emissions from motor vehicle sources
#                   in Baltimore City with emissions from motor vehicle sources
#                   in Los Angeles County, California (fips == "06037").
#                   Which city has seen greater changes over time
#                   in motor vehicle emissions?

# WORK-FLOW:    The script executes the following STEPS:
#                 0. Loads the required libraries.
#                 1. Downloads and extracts the data files if any of them doesn't exist.
#                 2. Loads the data files in R.
#                 3. Subsets the target variables and observations from the data.
#                 4: Creates the data frames needed to construct each plot.
#                 5. Creates all elementary plots that compose the multiplot.
#                 6. Combines the plots, to create the multiplot,
#                    and saves it in the working directory.

# ABOUT THE DATA USED
# The data used to create the plot,
# comes from the 'National Emissions Inventory (NEI)' database.
# Specifically the tables below were used:
#     - 'PM2.5 Emissions Data' (summarySCC_PM25.rds)
#     - 'Source Classification Code Table' (Source_Classification_Code.rds)
# which can be downloaded from 'UC Irvine Machine Learning Repository' through this link:
#     "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
# The script will download the data files, if any of them doesn't exists in the working directory.
# More informations on the dataset can be found on the link:
#     https://www.epa.gov/air-emissions-inventories

# ABOUT THIS SCRIPT
# The script was created:
#   - in RStudio Version 1.1.383
#   - with R version 3.4.4
#   -- used dplyr version 0.7.4
#   -- used tidyr version 0.8.0
#   -- used ggplot2 version 2.2.1
#   -- used gridExtra version 2.3
# and the data was downloaded at 24 April 2018.




# When the steps get executed informative messages, appear in console.
message("EXECUTING THE SCRIPT: 'plot6.R'")

################################################################################
# STEP 0: Loads the required libraries
################################################################################

# To create all data frames needed to construct the components of the multiplot.
library(dplyr)
library(tidyr)

# To create all elementary plots that compose the multiplot.
library(ggplot2)


# To combine the different plots, in one multiplot.
library(gridExtra)

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 0: Loads the required libraries.")




################################################################################
# STEP 1: Downloads and extracts the data files if any of them doesn't exist
################################################################################

# The files 'summarySCC_PM25.rds' and 'Source_Classification_Code.rds',
# should be in the working directory for the STEP 2,
# when data will be loaded in R.
# If any of them doesn't exist, both are downloaded and extracted in this step.
# (Informative messages explain the situation to the user)


if (!file.exists("summarySCC_PM25.rds") |
    !file.exists("Source_Classification_Code.rds")) {
      
      # downloads the zipped data file as 'data.zip'
      message("At least one of the files: \n",
              "  summarySCC_PM25.rds' or 'Source_Classification_Code.rds' \n",
              "doesn't exists in the working directory. \n",
              "Trying to download the zipped data file ...")
      
      zip_data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      download.file(zip_data_url, "data.zip")
      
      message("\t ... zipped data file was successfully downloaded.")
      
      
      # unzips the 'data.zip' file
      message("Trying to extract the data files...")
      unzip("data.zip")
      
      # removes the 'data.zip' file
      file.remove("data.zip")
      
      message("\t ... data files were successfully extracted ",
              "and the zipped file removed. \n",
              "The data files 'summarySCC_PM25.rds' ",
              "and 'Source_Classification_Code.rds' \n",
              "are now present in the working directory.")
}

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 1: Downloads and extracts the data files if any of them doesn't exist.")




################################################################################
# STEP 2: Loads the data files in R
################################################################################

# Data Table: 'PM2.5 Emissions Data'
NEI <- readRDS("summarySCC_PM25.rds")

# Data Table: 'Source Classification Code Table'
SCC <- readRDS("Source_Classification_Code.rds")

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 2: Loads the data files in R.")




################################################################################
# STEP 3: Subsets the target variables and observations from the data
################################################################################

# PLAN
#     1. From the SCC data table, based on the value of EI.Sector,
#        the 'SCC' codes that are related to motor vehicle sources
#        will be identified and extracted.
#     2. Based on the target SCC codes and the fips variable,
#        the observations of NEI data table will be filtered
#        and will subset only the target variables.

# Filters the motor vehicle related sources
# and selects only the respective values of the 'SCC' variable.
motor_vehicle_related_sources <- SCC %>%
      filter(EI.Sector %in% c("Mobile - On-Road Gasoline Light Duty Vehicles",
                              "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                              "Mobile - On-Road Diesel Light Duty Vehicles",
                              "Mobile - On-Road Diesel Heavy Duty Vehicles")) %>%
      # Subsets the 'SCC' variable simplified, as a vector
      `[[`("SCC") %>%
      # Changes the type of the SCC variable from factor to character,
      # because in the NEI data frame 'SCC' variable is of class character.
      as.character()


# Filters the NEI data, to get only the observations
# related to emissions from motor vehicle sources
# for Los Angeles County and Baltimore City over the years
# and subsets only the variables needed to create the different data frames,
# required to construct each components of the multiplot.
target_data <- NEI %>%
      filter(fips %in% c("06037", "24510"),
             SCC %in% motor_vehicle_related_sources) %>%
      select(fips, year, Emissions) %>%
      mutate(year = as.factor(year),
             # The value of fips changes, from a code to the name of each city
             fips = gsub("06037", "Los Angeles County", fips),
             fips = gsub("24510", "Baltimore City", fips),
             fips = factor(fips, levels = c("Los Angeles County", "Baltimore City")))

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 3: Subsets the target variables and observations from the data.")




################################################################################
# STEP 4: Creates the data frames needed to construct each plot
################################################################################

# For 'figure_top_left', the data contains
# the total emissions in PM2.5 by year and city.
data_total_emissions <- target_data %>%
      group_by(fips, year) %>%
      summarise(total_emissions = sum(Emissions)) %>%
      ungroup()


# For 'figure_top_right', the data contains
# the changes in total emissions in PM2.5 by each city,
# for the periods 1999-2002, 2002-2005 and 2005-2008.
data_change_total <- target_data %>%
      # The total emissions by each year and city gets computed.
      group_by(fips, year) %>%
      summarise(total_emissions = sum(Emissions)) %>%
      # After getting the total emissions, the grouping variables change,
      # so we need to ungroup the data, and regroup it only by the 'fips' variable.
      ungroup() %>%
      group_by(fips) %>%
      # From the total values it calculates the change that
      # happened in the periods 1999-2002, 2002-2005 and 2005-2009.
      nest() %>%
      mutate(data = lapply(data, function(df) {
            # The main (anonymous) function used, is very simplistic,
            # just takes a data frame with 2 variables/columns
            # and uses the first one as the 'step' and the second as the 'value'.
            step <- as.character(df[[1]])
            value <- as.numeric(df[[2]])
            
            last <- nrow(df)
            # Because in some of the groups there are missing values
            # for whole steps, some invalid periods can occur
            # so the valid periods are manually specified.
            valid_periods <- c("1999-2002", "2002-2005", "2005-2008")
            
            # The output is a new data frame with two columns,
            # the first is the period (between two steps)
            # and the second is the difference of the values between those steps.
            data.frame("period" = paste(step[-last], step[-1], sep = "-"),
                       "change" = value[-1] - value[-last]) %>%
                  # The invalid periods are filtered out.
                  filter(period %in% valid_periods) %>%
                  mutate(period = factor(period, levels = valid_periods))
      })) %>%
      unnest() %>%
      ungroup()


# For 'figure_bottom', the data contains
# the logarithmic transformations of emissions in PM2.5: ln(Emissions + 1)
# by year and city
data_log_emissions <- target_data %>%
      mutate(log_emissions = log(Emissions + 1),
             # The original 'Emissions' variable is no longer needed and dropped.
             Emissions = NULL)

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 4: Creates the data frames needed to construct each plot.")




################################################################################
# STEP 5: Creates all elementary plots that compose the multiplot
################################################################################

# Creates a list that contains four plots, the components of the multiplot.
components_of_multiplot <- list(
      
      # The first figure will explore how much was, the total emissions,
      # in each city, for each of the years 1999, 2002, 2005 and 2009,
      # in a linechart that presents both cities together for comparison,
      "figure_top_left" = ggplot(data_total_emissions, aes(year, total_emissions)) +
            geom_point(aes(color = fips), size = 3) +
            geom_line(aes(color = fips, group = fips), show.legend = FALSE) +
            labs(title = expression("Total Emissions from PM"[2.5])) +
            xlab(label = "")  +
            ylab(label = "Total Emissions from PM2.5 in tons") +
            theme_minimal() +
            theme(legend.position = c(0.75, 0.5)),
      
      # The second figure will explore how much the changes of total emissions was,
      # for each city, in the periods 1999-2002, 2002-2005 and 2005-2009,
      # in two distinct bar-charts, one for each city.
      "figure_top_right" = ggplot(data_change_total, aes(x = period, y = change)) +
            geom_col(aes(fill = change)) +
            scale_fill_gradient2(low = "turquoise", mid = "black", high = "indianred",
                                 midpoint = 0) +
            xlab(label = "") +
            ylab(label = "Changes in Emissions from PM2.5 in tons") +
            facet_wrap(~ fips, nrow = 2) +
            theme_linedraw(),
      
      # The third figure will present in box-plots,
      # the logarithmic transformation for Emissions:  ln(Emissions + 1),
      # that will expose in which city the bigger changes happened.
      "figure_bottom" = ggplot(data_log_emissions, aes(year, log_emissions)) +
            geom_boxplot(aes(color = fips), show.legend = FALSE) +
            labs(title = expression("Logarithm of Emissions from PM"[2.5] ~
                                          "for the Los Angeles County and Baltimore City"),
                 subtitle = paste("The figure amplifies the changes happened, ",
                                  "in Emissions over the years \n",
                                  "!! (It shouldn't be used to do conclude ",
                                  "about the magnitude of those changes)")) +
            xlab(label = "") +
            ylab(label = expression(log("Emissions from PM"[25] + 1))) +
            facet_grid(. ~ fips) +
            theme_linedraw()
)

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 5: Creates all elementary plots that compose the multiplot.")




################################################################################
# STEP 6: Combines the plots, to create the multiplot,
#         and saves it in the working directory
################################################################################

# Construct the multiplot from the four plots,
# contained in the list created in STEP 5.
multiplot <- grid.arrange(grobs = components_of_multiplot,
                          layout_matrix = matrix(c(1, 2,
                                                   3, 3),
                                                 byrow = TRUE,
                                                 ncol = 2),
                          # Adds the main question at the top of the multiplot.
                          top = paste("Question 6: \n",
                                      "Compare emissions from motor vehicle sources ",
                                      "in Baltimore City \n",
                                      "with emissions from motor vehicle sources ",
                                      "in Los Angeles County, California (fips == '06037'). \n",
                                      "Which city has seen greater changes over time ",
                                      "in motor vehicle emissions?")
)

# Opens a new png graphical device, with resolution 640x640 pixels,
# plots the multiplot and saves it as 'plot6.png' in the working directory.
png(filename = "plot6.png", width = 640, height = 640)
plot(multiplot)
dev.off()

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 6: Combines the plots, to create the multiplot, ",
        "and saves it in the working directory.")




# A message that is expected to appear, when you source the script successfully.
message("SUCCESSfully created 'plot6.png' in the working directory.")
# THE END ######################################################################




