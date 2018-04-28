################################################################################
# WHAT YOU NEED TO KNOW BEFORE EXECUTING THIS SCRIPT
################################################################################

# DESCRIPTION:  The script 'plot3.R', constructs and saves a multiplot,
#               that presents some core aspects of the changes in emissions
#               over time by the type of source, for the Baltimore City.

# THE QUESTION: The plot tries to answer the question:
#
#                   Of the four types of sources indicated by the type
#                   (point, nonpoint, onroad, nonroad) variable,
#                   which of these four sources have seen decreases in emissions
#                   from 1999–2008 for Baltimore City?
#                   Which have seen increases in emissions from 1999–2008?  

# WORK-FLOW:    The script executes the following STEPS:
#                 0. Loads the required libraries.
#                 1. Downloads and extracts the data file if it doesn't exist.
#                 2. Loads the data file in R.
#                 3. Subsets the target variables and observations from the data.
#                 4: Creates the data frames needed to construct each plot.
#                 5. Creates all elementary plots that compose the multiplot.
#                 6. Combines the plots, to create the multiplot,
#                    and saves it in the working directory.

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
#   -- used dplyr version 0.7.4
#   -- used tidyr version 0.8.0
#   -- used ggplot2 version 2.2.1
#   -- used gridExtra version 2.3
# and the data was downloaded at 24 April 2018.




# When the steps get executed informative messages, appear in console.
message("EXECUTING THE SCRIPT: 'plot3.R'")

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

# Data Table: 'PM2.5 Emissions Data'
NEI <- readRDS("summarySCC_PM25.rds")

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 2: Loads the data file in R.")




################################################################################
# STEP 3:  Subsets the target variables and observations from the data
################################################################################

# Filter the observations to get those about Baltimore City,
# subsets the 'year', the value of 'Emissions' and the the 'type' of source.
target_data <- NEI %>%
      filter(fips == "24510") %>%
      select(year, type, Emissions) %>%
      mutate(year = as.factor(year),
             type = as.factor(type))

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 3: Subsets the target variables and observations from the data.")




################################################################################
# STEP 4: Creates the data frames needed to construct each plot
################################################################################

# For 'figure_top_left' and 'figure_top_right', the data contains 
# the total emissions in PM2.5 by year and type of source.
data_total_emissions <- target_data %>%
      group_by(year, type) %>%
      summarise(total_emissions = sum(Emissions)) %>%
      ungroup()


# For 'figure_bottom_left', the data contains
# the changes in total emissions in PM2.5 by each type of source,
# for the periods 1999-2002, 2002-2005 and 2005-2008.
data_change_total <- target_data %>%
      # The total emissions by each year and gets computed.
      group_by(year, type) %>%
      summarise(total_emissions = sum(Emissions)) %>%
      # After getting the total emissions, the grouping variables change,
      # so we need to ungroup the data, and regroup it only by the 'type' variable.
      ungroup() %>%
      group_by(type) %>%
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


# For 'figure_bottom_right', the data contains
# the logarithmic transformations of emissions in PM2.5: ln(Emissions + 1)
# by year and type of source.
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
      # by the type of sources, for each of the years 1999, 2002, 2005 and 2009,
      # in a linechart that presents all types together for comparison.
      "figure_top_left" = ggplot(data_total_emissions, aes(x = year, y = total_emissions)) +
            geom_point(aes(color = type), size = 2) +
            scale_color_discrete(name = "Type of Source") +
            geom_line(aes(color = type, group = type)) +
            labs(title = paste0("Total Emissions from PM2.5",
                                "by Type of Source, in Baltimore City")) +
            xlab(label = "") +
            ylab(label = "") +
            theme_bw(base_size = 10),
      
      # The second figure will explore how much was the total emissions,
      # by the type of sources, for each of the years 1999, 2002, 2005 and 2009,
      # in four distinct bar-charts, one for each type.
      "figure_top_right "= ggplot(data_total_emissions, aes(x = year, y = total_emissions)) +
            geom_col(aes(fill = type), position = "dodge", show.legend = FALSE) +
            labs(title = paste0("Total Emissions from PM2.5 ",
                                "by Type of Source in Baltimore City")) +
            xlab(label = "") +
            ylab(label = "") +
            facet_wrap(~ type, nrow = 1) +
            theme_linedraw(base_size = 10) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)),
      
      
      
      # The third figure will explore how much the changes in total emissions was,
      # by the type of sources, for the periods 1999-2002, 2002-2005 and 2005-2009,
      # in four distinct bar-charts, one for each type.
      "figure_bottom_left" = ggplot(data_change_total, aes(x = period, y = change)) +
            geom_col(aes(fill = change)) +
            scale_fill_gradient2(name = paste0("Changes in Emissions \n",
                                               "from PM2.5 \n",
                                               "(in thousand of tons)"),
                                 low = "turquoise", mid = "black", high = "indianred",
                                 midpoint = 0) +
            labs(title = paste0("Changes in Total Emissions from PM2.5 \n",
                                "over 3-year periods by Type of Source ",
                                "in Baltimore City")) +
            xlab(label = "") +
            ylab(label = "") +
            facet_wrap(~ type, ncol = 4) +
            theme_linedraw(base_size = 9) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)),
      
      # The fourth figure will present in box-plots,
      # the logarithmic transformation for Emissions:  ln(Emissions + 1),
      # that will expose in which type of source, the bigger changes happened.
      "figure_bottom_right" = ggplot(data_log_emissions, aes(x = year, y = log_emissions)) +
            geom_boxplot(aes(fill = type), show.legend = FALSE) +
            labs(title = paste0("Logarithm of Emissions from PM2.5 ",
                                "by Type of Source in Baltimore City"),
                 subtitle = paste("The figure amplifies the changes happened ",
                                  "in Emissions over the years \n",
                                  "!! (It shouldn't be used to conclude about",
                                  "the magnitude of those changes)")) +
            xlab(label = "") +
            ylab(label = "\n Log10(Emissions + 1)") +
            facet_wrap( ~ type, ncol = 4) +
            theme_linedraw(base_size = 9) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
                          nrow = 2,
                          # Adds the main question at the top of the multiplot.
                          top = paste0("QUESTION 3: \n",
                                       "Of the four types of sources indicated by the type",
                                       "(point, nonpoint, onroad, nonroad) variable \n",
                                       "which of these four sources have seen decreases ",
                                       "in emissions from 1999–2008 for Baltimore City? \n",
                                       "Which have seen increases in emissions ",
                                       "from 1999–2008? \n",
                                       "(Use the ggplot2 plotting system to make a plot ",
                                       "answer this question.) \n"),
                          # Adds a common label for both plots,
                          # 'figure_top_left' and 'figure_bottom_right',
                          # at the left of the multiplot.
                          left = "Total Emissions from PM2.5 (in thousands of tons)"
)

# Opens a new png graphical device, with resolution 800x640 pixels,
# plots the multiplot and saves it as 'plot6.png' in the working directory.
png(filename = "plot3.png", width = 800,  height = 640)
plot(multiplot)
dev.off()

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 6: Combines the plots, to create the multiplot, ",
        "and saves it in the working directory.")




# A message that is expected to appear, when the script had been sourced successfully.
message("SUCCESSfully created 'plot3.png' in the working directory.")
# THE END ######################################################################










