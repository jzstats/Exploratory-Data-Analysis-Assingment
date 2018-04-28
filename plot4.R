################################################################################
# WHAT YOU NEED TO KNOW BEFORE EXECUTING THIS SCRIPT
################################################################################

# DESCRIPTION:  The script 'plot4.R', constructs and saves a multiplot,
#               that presents some core aspects of the changes in emissions
#               over time, only for coal combustion-related sources,
#               across the United States.

# THE QUESTION: The plot tries to answer the question:
#
#                   Across the United States, how have emissions from coal
#                   combustion-related sources changed from 1999–2008? 

# WORK-FLOW:    The script executes the following STEPS:
#                 1. Downloads and extracts the data files if any of them don't exist.
#                 2. Loads the data files in R.
#                 3. Process the data to isolate the target variables and observations.
#                 4. Creates 'utility functions' to help in data wrangling.
#                 5. Creates a list with instruction to construct all elementary plots.
#                 6. Constructs all elementary plots that compose the multiplot.
#                 7. Combines the plots, to create each part of the multiplot.
#                 8. Combines the parts, to create the multiplot
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
#   -- used rlang version 0.2.0
#   -- used dplyr version 0.7.4
#   -- used tidyr version 0.8.0
#   -- used ggplot2 version 2.2.1
#   -- used lvplot version 0.2.0
#   -- used gridExtra version 0.7.4
# and the data was downloaded at 24 April 2018.




# When the steps get executed informative messages, appear in console.
message("EXECUTING THE SCRIPT: 'plot4.R'")

################################################################################
# STEP 0: Loads required libraries
################################################################################

# To create all data frames needed to construct the components of the multiplot.
library(dplyr)
library(tidyr)

# To supply the function 'enquo()' that will be used in the utility functions.
library(rlang)

# To create all elementary plots that compose the multiplot.
library(ggplot2)

# To create 'letter-value' plots.
library(lvplot)

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

## Data Table: 'PM2.5 Emissions Data'
NEI <- readRDS("summarySCC_PM25.rds")

## Data Table: 'Source Classification Code Table'
SCC <- readRDS("Source_Classification_Code.rds")

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 2: Loads the data files in R.")




##################################################################################
# STEP 3: Process the data to isolate the target variables and observations
##################################################################################

# PLAN
#     1. From the SCC data table, based on the value of EI.Sector,
#        the 'SCC' codes that are related with the coal combastions-related sources
#        will be identified and extracted along with 'EI.Sector' variable as well as
#        'SCC.Level.One', 'SCC.Level.Two', 'SCC.Level.Three' and 'SCC.Level.Four'.
#     2. Based on the target SCC codes, the subset of SCC and NEI datasets
#        will be merged and some aspects of the variables will be refined,
#        to create a dataset that contains all needed variables and observations
#        to construct the different data frames for each elementary plot.


# Filters the coal compastion-related sources
# and selects the required variables for the target data.
coal_combastion_related_sources <- SCC %>%
      filter(EI.Sector %in% c("Fuel Comb - Electric Generation - Coal",
                              "Fuel Comb - Comm/Institutional - Coal",
                              "Fuel Comb - Industrial Boilers, ICEs - Coal")) %>%
      select(SCC, EI.Sector,
             SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four)


# Merges target variables from both data frames
# to get the emissions from the coal combastion-related sources over the years
# and all variables needed to construct the multiplot.
target_data <- NEI %>%
      select(SCC, year, Emissions) %>%
      merge(coal_combastion_related_sources, by = "SCC") %>%
      select(-SCC) %>%
      mutate(year = as.factor(year),
             # The values of EI.Sector are shortened to fit better in multiplot
             EI.Sector = gsub("Fuel Comb - ", "", x = EI.Sector),
             EI.Sector = gsub(" - Coal", "", x = EI.Sector),
             EI.Sector = as.factor(EI.Sector))

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 3: Process the data to isolate the target variables and observations.")




################################################################################
# STEP 4: Creates 'utility functions' to help in data wrangling
################################################################################

# The utility functions help to construct all different data frames,
# in some pipes of 'dplyr' verbs.

# The function summarizes the value of a variable over some grouping variables.
#     data        a data frame which contains all the needed variables
#     value       the variable of which the value will be summarized
#     ...         the names of the grouping variables
# Subsets only the given variables from the data frame
# and summarizes the specified 'value' variable by the grouping variables.
my_get_total_value_by <- function(data, value, ...) {
      # In order to be able to supply the arguments in the regular way
      # used by dplyr verbs the 'enquo()/enquos()' verbs are used
      # to look into the value of the argument and interpret it as expression.
      value <- enquo(value)
      by <- enquos(...)
      
      # !! or !!! are used to tell to the 'dplyr' verbs
      # that the arguments supplied as expression
      data %>%
            select(!!value, !!!by) %>%
            group_by(!!!by) %>%
            summarise(total = sum(!!value))
}

# The function can compute the difference in values over steps,
# over some grouping factors.
#     data        a data frame which contains all the needed variables
#     step        a variable of with the name of each step
#     value       a variable that contains the value for each step
#     ...         the names of grouping variables
# This function takes as arguments a data frame, subsets only the given variables
# and creates a new data frame with the difference of the value over each step
# by the grouping variables/columns.
my_get_change_by <- function(data, step, value, ...) {
      # In order to be able to supply the arguments in the regular way
      # used by dplyr verbs the 'enquo()/enquos()' verbs are used
      # to look into the value of the argument and interpret it as expression.
      step <- enquo(step)
      value <- enquo(value)
      by <- enquos(...)
      
      # !! or !!! are used to tell to the 'dplyr' verbs
      # that the arguments supplied as expression.
      data %>%
            # The given variables are subseted and the rest are dropped.
            select(!!step, !!value, !!!by) %>%
            group_by(!!!by) %>%
            # The data frame will be reformed to create a column
            # that will contain a data frame with only the step and value variables
            # among other columns witch will contain one grouping variable each.
            nest() %>%
            # The variable that contains the data frames
            # with the 'step' and the 'value' variables will be transform to new data frames
            # that will contain the 'period' and the 'change' in values between each step.
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
}

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 4: Creates 'utility functions' to help in data wrangling.")




################################################################################
# STEP 5: Creates a list with instruction to construct all elementary plots
################################################################################

# All the information needed to construct the plots is supplied through the
# list below which has the following structure:
#
#     instructions <- list(
#
#           "part_1" = list(
#                 function_that_creates_plots = function(arguments) {
#                       some_ggplot2_code_that_produces_a_plot
#                       },
#                 arguments_for_the_function = list(
#                       arg_1 = list(arg_1_value_for_call_1, ..., arg_1_value_for_call_last),
#                       ...,
#                       arg_last = list(arg_last_value_for_call_1, ..., arg_last_value_for_call_last)
#                       )
#           ),
#
#           "part_2" = list(
#                 function_that_creates_plots = function(arguments) {
#                       some_ggplot2_code_that_produ89ces_a_plot
#                 },
#                 arguments_for_the_function = list(
#                       arg_1 = list(arg_1_value_for_call_1, ..., arg_1_value_for_call_last),
#                       ...,
#                       arg_last = list(arg_last_value_for_call_1, ..., arg_last_value_for_call_last)
#                       )
#           ),
#
#           ...,
#
#           part_last = list(
#                 function_that_creates_plots = function(arguments) {
#                       some_ggplot2_code_that_produces_a_plot
#                 },
#                 arguments_for_the_function = list(
#                       arg_1 = list(arg_1_value_for_call_1, ..., arg_1_value_for_call_last),
#                       ...,
#                       arg_last = list(arg_last_value_for_call_1, ..., arg_last_value_for_call_last)
#                       )
#           )
#    )
#
#
# More informations about the list are supplied through the comments
# alongside the actual list used to create all elementary plots.
instructions_to_construct_components_for_each_part_of_the_multiplot <- list(
      # In the first part, all plots will explore how much the total emissions have changed
      # from 1999-2002, 2002-2005, 2005-2009 but will differ by the grouping
      # factor under which we examine them.
      "part_1" = list(
            # The first object of part_1 is the function that can construct
            # plots that would vary based on the given arguments
            "function_to_construct_the_main_body_of_each_plot_for_this_part" =
                  # The function requires 3 arguments to be supplied:
                  #     data        the data frame with the data needed
                  #                 by ggplot to construct the plot
                  #     title       a title for each plot
                  #     facet       a 'facet' as it is regularly used in ggplot2 code blocks
                  #                 that will be added to the main body of the function
                  #                 to specify the grouping factors
                  # The output is a ggplot object.                
                  function(data, title, facet) {
                        # All the plots in this part are produced by the
                        # code below that differs only though the values of
                        # the its arguments
                        ggplot(data, aes(x = period, y = change)) +
                              geom_col(aes(fill = change)) +
                              scale_fill_gradient2(name = paste0("Change in \n",
                                                                 "Total Emissions \n",
                                                                 "from PM2.5 in tons"),
                                                   low = "turquoise",
                                                   mid = "black",
                                                   high = "indianred") +
                              labs(title = title) +
                              facet +
                              xlab("3-Year Period") +
                              ylab(paste0("Emissions from PM2.5 in tons)"))
                  },
            
            # The second object of 'part_1' is a list that contains
            # all the values for the arguments needed by the function
            # to create each plot, for this part.
            # The structure of this list is:
            #
            #     list(arg_1 = list(arg_1_value_for_call_1, ..., arg_1_value_for_call_last),
            #          arg_2 = list(arg_2_value_for_call_1, ..., arg_2_value_for_call_last)
            #          ...,
            #          arg_last = list(arg_last_value_for_call_1, ..., arg_last_value_for_call_last)
            #     )
            #
            # so it easy to set in first place, modify or debug when you need to.
            # Unfortunately this structure doesn't fit what the
            # 'do.call() function needs when called from 'Map()' function,
            # so the list will be reformed before it can be used.
            # More details are supplied below when that transformation is about to happen.
            "arguments_of_the_function_to_construct_the_main_body_of_each_plot_for_this_part" = list(
                  # A distinct title will be supplied to each plot
                  # (the first part of the title remain the same,
                  #  while the ending depends on the grouping factors)
                  "title" = paste0("Change in Total Emissions from PM2.5 in tons",
                                   c(".", " by each EI.Sector.")),
                  
                  # The data that ggplot() will use to construct each plot.
                  "data" = list(
                        # The data for the plot that shows the change
                        # of total emission for every 3-year step period
                        # without any grouping factor (fig_1_left)
                        "data_change_total" = target_data %>%
                              my_get_total_value_by(Emissions, year) %>%
                              my_get_change_by(year, total) %>%
                              mutate(change = change / 1000),
                        # The data for the plot that shows the change
                        # of total emission for every 3-year step period
                        #  by EI.Sector (fig_1_right)
                        "data_change_total_by_sector" = target_data %>%
                              my_get_total_value_by(Emissions, year, EI.Sector) %>%
                              my_get_change_by(year, total, EI.Sector) %>%
                              mutate(change = change / 1000)
                  ),
                  
                  # The complete command for ggplot2 facet is added to each plot
                  # based on the grouping factors of our interest.
                  "facet" = list("left_plot" = NULL,
                                 "right_plot" = facet_wrap( ~ EI.Sector, ncol = 3)
                  )
            )
      ),
      
      # In the second part, all plots will explore the total emissions in PM2.5
      # for each year and will differ by the grouping factor
      # under which we examine them.
      "part_2" = list(
            # The first object of part_2 is similar to the one for the part_1.
            "function_to_construct_the_main_body_of_each_plot_for_this_part" = 
                  # The function is similar to the one for part_1.
                  function(data, title, facet) {
                        ggplot(data, aes(x = year, y = total)) +
                              geom_col(aes(fill = EI.Sector), show.legend = FALSE) +
                              scale_fill_manual(values = c("brown", "darkgreen", "goldenrod")) +
                              labs(title = title) +
                              facet +
                              xlab("Year") +
                              ylab("Emissions from PM2.5 in tons")
                  },
            
            # The second object of part_2 is similar to the one in part_1.
            "arguments_of_the_function_to_construct_the_main_body_of_each_plot_for_this_part" = list(
                  # The title is similar to the one for part_1.
                  "title" = paste0("Total Emissions from PM2.5 in tons",
                                   c(".", " by each EI.Sector.")),
                  
                  # The data is similar to the one for part_1
                  ## However only one data set is enough to create all plots so
                  ## in the end the same dataset is subseted 2 times to
                  ## be supplied as needed in each of the function's call.
                  "data" = list(
                        # The data contains the logarithmic transformation
                        # of Emissions: ln(Emissions + 1)
                        # over the year and EI.Sector
                        "data_log_emissions" = target_data %>%
                              my_get_total_value_by(value = Emissions, year, EI.Sector) %>%
                              mutate(total = total / 1000))[c(1, 1)],
                  
                  # The facet is similar to the one for part_1.
                  "facet" = list("left_plot" = NULL,
                                 "right_plot" = facet_wrap( ~ EI.Sector, ncol = 3)
                  )
            )
      ),
      
      # In the third part all plots present letter-value plots of the
      # the logarithmic transformation of Emissions: log(Emissions + 1)
      # over the years but differ by the grouping factor
      # under which we examine them.
      "part_3" = list(
            # The first object of part_3 is similar to the one for the part_1.
            "function_to_construct_the_main_body_of_each_plot_for_this_part" = 
                  # The function is similar to the one for part_1.
                  function(data, title, facet) {
                        ggplot(data, aes(x = year, y = log_emissions)) +
                              geom_lv(color = "black") +
                              scale_fill_manual(values = c("brown", "darkgreen", "goldenrod")) +
                              facet +
                              labs(title = title)+
                              xlab("Year") +
                              ylab("ln(Emissions + 1)")
                  },
            
            # The second object of part_3 is similar to the one in part_1.
            "arguments_of_the_function_to_construct_the_main_body_of_each_plot_for_this_part" = list(
                  # The title is similar to the one for part_1.
                  "title" = paste0("Logarithmic Transformation of Emissions from PM2.5",
                                   c(".", "by each EI.Sector.")),
                  
                  # The data is similar to the one for part_1
                  ## However only one data set is enough to create all plots so
                  ## in the end the same dataset is subseted 2 times to
                  ## be supplied as needed in each of the function's call
                  "data" = list(
                        # The data contains the total emissions
                        # over each year and EI.Sector
                        "data_total_by_year_sector" = target_data %>%
                              select(year, Emissions, EI.Sector) %>%
                              mutate(log_emissions = log(Emissions + 1),
                                     Emissions = NULL))[c(1, 1)],
                  
                  # The facet is similar to the one for part_1.
                  "facet" = list("left_plot" = NULL,
                                 "right_plot" = facet_wrap( ~ EI.Sector, ncol = 3))
            )
      ),
      
      # In the fourth part all plots will explore where was the greater changes
      # between the years, for the periods 1999-2002, 2002-2005, 2005-2008,
      # grouped by the EI.Sector (but also includes the marginal case)
      # for each of the variables:
      #     SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four
      # that classify the sources of the pollutant,
      # from more general (in level one) to more specific (in level four).
      # (so a latter analysis can be conducted in the categories that show
      # the bigger changes and try to explain them in some way).
      # The logarithmic transformation was applied to the total emissions
      # before the change in every step was computed to construct
      # a heatmap with the periods in x-axis, and the different values of
      # each level in y-axis, while the color will indicate emissions where:
      #     * increase (columns will appear red),
      #     * stayed about the same (tiles will appear black)
      #     * decreased (column will appear light blue)
      # Also it must be highlighted that because of the transformation
      # the plot is only useful to spot the categories with greater changes
      # and NOT to conclude about the magnitude of those changes!!
      "part_4" = list(
            "function_to_construct_the_main_body_of_each_plot_for_this_part" =
                  # Although the function is similar to the one in part_1,
                  # it differs because it takes as argument the
                  # the value of aes(y = this_is_supplied_as_argument)
                  # and the aes() is replaced with aes_string()
                  # Furthermore the legend should be modified
                  # to make the plot look better so it is supplied as well.   
                  function(data, scc_level, legend_modification) {
                        # The function requires 3 arguments to be supplied:
                        #     data              the data frame with the data needed
                        #                       by ggplot to construct the plot
                        #     scc_level         the variable representing
                        #                       the level of interest
                        #     legend_modification     the ggplot's verb theme()
                        #                             that will be added to
                        #                             to specify the legend
                        # The output is a ggplot object.                
                        ggplot(data, aes_string("period", y = scc_level)) +
                              geom_tile(aes(fill = change)) +
                              scale_fill_gradient2(name = paste0("Changes in \n",
                                                                 "Logarithm of \n",
                                                                 "Emissions \n",
                                                                 "from PM2.5 \n"),
                                                   low = "turquoise", mid = "black", high = "indianred",
                                                   midpoint = 0) +
                              scale_y_discrete(position = "right") +
                              # Because the argument 'scc_level' is already a string
                              # it can easily be supplied again to create a descriptive title
                              labs(title = paste0("Changes in Emissions form PM2.5, over 3-Year Periods ",
                                                  "by the ", scc_level, " categorization of Sources, ",
                                                  "for all EI.Sectors and by each EI.Sector")) +
                              xlab(label = "") +
                              facet_grid(~ EI.Sector, margins = TRUE) +
                              theme_classic() +
                              legend_modification
                        
                  },
            
            # The fourth object is similar to the one for part_1
            "arguments_of_the_function_to_construct_the_main_body_of_each_plot_for_this_part" = list(
                  # The data is similar to the one of part_1
                  ## All datasets are created by the same function with the only
                  ## difference to be the value of scc_level argument,
                  "data" = lapply(
                        # The arguments are supplied as a list that contains
                        # only the name of the variable, as expression created with quo()
                        #   * the quo() is needed here to prevent the names of the
                        #     scc_levels to get evaluated by lapply()
                        #     when the list is constructed,
                        #     because the names are not supplied at this point
                        #     with information about the data table in which they belong
                        #     which will conclude in some sort of error.
                        X = list("data_change_log_total_by_sector_lvl1" = quo(SCC.Level.One),
                                 "data_change_log_total_by_sector_lvl2" = quo(SCC.Level.Two),
                                 "data_change_log_total_by_sector_lvl3" = quo(SCC.Level.Three),
                                 "data_change_log_total_by_sector_lvl4" = quo(SCC.Level.Four)),
                        FUN = function(scc_level) {
                              # the value of the argument scc_level is captured
                              # to be passed latter in the dplyr pipe
                              # that will create each dataset
                              scc_level <- enquo(scc_level)
                              target_data %>%
                                    my_get_total_value_by(Emissions, year, EI.Sector, !!scc_level) %>%
                                    mutate(log_total = log10(total + 1), total = NULL) %>%
                                    my_get_change_by(year, log_total, EI.Sector, !!scc_level)
                        }),
                  
                  # The value of the level supplied in as character string
                  # to the function that creates the plot through 'aes_string()'
                  "scc_level" = list(
                        "lvl1" = "SCC.Level.One",
                        "lvl2" = "SCC.Level.Two",
                        "lvl3" = "SCC.Level.Three",
                        "lvl4" = "SCC.Level.Four"),
                  
                  # The legend was modified for the plots to fit better
                  # in the final multiplot so specifications
                  # are supplied for the legend of each level
                  "legend_modification" = list(
                        "lvl1" = theme(legend.direction = "horizontal",
                                       axis.text.x = element_text(size = 8)),
                        "lvl2" = theme(legend.direction = "horizontal",
                                       axis.text.x = element_text(size = 8)),
                        "lvl3" = theme(legend.direction = "horizontal",
                                       axis.text.x = element_text(size = 8)),
                        "lvl4" = theme(axis.text.x = element_text(size = 7),
                                       legend.key.height = unit(50, "point"))
                  )
            )
      )
)

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 5: Creates a list with instruction to construct all elementary plots.")




################################################################################
# STEP 6: Constructs all elementary plots that compose the multiplot
################################################################################

# The (anonymous) function below,
# with the instruction contained in the list created in STEP 5,
# constructs all elementary plots that will compose the multiplot
# inside a list.
components_for_each_part_of_the_multiplot <-
      
      # The list of instruction which contains three lists,
      # each with the instructions to construct the plots of each part
      # is supplied to lapply
      lapply(instructions_to_construct_components_for_each_part_of_the_multiplot,
             # An anonymous function which:
             #    1) Reforms the list with arguments in the form needed in step 2
             #    2) Calls the function that creates the plots in this
             #       part with the proper arguments for each plot,
             #       through the function 'do.call()',
             #       from inside of a 'Map()' loop for all plots of each part.
             function(instructions_for_this_part) {
                   
                   # Catches the two main components of each list,
                   # the function and the list with arguments.
                   ## The first element of the instructions is the function that
                   ## constructs the main body of each plot.
                   what <- instructions_for_this_part[1]
                   
                   ## The second argument of the instructions is the arguments of
                   ## the function that constructs the main body of each plot.
                   args <- instructions_for_this_part[[2]]
                   
                   # The arguments must be reformed, from a list more intuitive when setting the parameters, like this:
                   #
                   #     list("argument_1" = list(value_for_call_1, value_for_call_2, ..., value_for_call_last),
                   #          "argument_2" = list(value_for_call_1, value_for_call_2, ..., value_for_call_last),
                   #          ...,
                   #          "argument_last" = list(value_for_call_1, value_for_call_2, ..., value_for_call_last))
                   #
                   # to a list compatible with the function 'do.call()'
                   # called by 'Map()' multiple times for each part, like this:
                   #
                   #     list("call_1" = list(value_for_argument_1, value_for_argument_call_2, ..., value_for_argument_last),
                   #          "call_2" = list(value_for_argument_1, value_for_argument_call_2, ..., value_for_argument_last),
                   #          ...,
                   #          "call_last" = list(value_for_argument_1, value_for_argument_call_2, ..., value_for_argument_last))
                   number_of_calls <- max(vapply(args, length, integer(1)))
                   reformed_args_by_each_call <- lapply(seq_len(number_of_calls),
                                                        function(ith_call) lapply(args, `[[`, ith_call))
                   
                   # Finally the function is called
                   # with the reformed arguments that correspond to each call
                   # and all components of each part are constructed
                   # in a list like this:
                   #
                   #     list("part_1" = list(plot_1, plot_2, ..., plot_last),
                   #          "part_2" = list(plot_1, plot_2, ..., plot_last),
                   #          ...,
                   #          "part_last" = list(plot_1, plot_2, ..., plot_last))
                   Map(do.call,
                       "what" = what,
                       "args" = reformed_args_by_each_call
                   )
             }
      )

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 6: Constructs all elementary plots that compose the multiplot.")



################################################################################
# STEP 7: Combines the plots, to create each part of the multiplot
################################################################################

# From the list with the elementary plots created in STEP 4,
# constructs the three parts of the multiplot.
parts_of_the_multiplot <- Map(
      # The 'grid.arrange()' function is used to combine the plots of each part.
      grid.arrange,
      "grobs" = components_for_each_part_of_the_multiplot,
      # Determines the layout of the plots for each part.
      "layout_matrix" = list(
            "part_1" = matrix(c(1, 2, 2), nrow = 1),
            "part_2" = matrix(c(1, 2, 2), nrow = 1),
            "part_3" = matrix(c(1, 2, 2), nrow = 1),
            "part_4" = matrix(rep(1:4, times = c(3, 4, 4, 17)),
                              ncol = 1)
      ),
      # Adds descriptive titles are on the top of each part
      # to provide the reader with some helpful information.
      "top" = list(
            "part_1" = paste(
                  "Changes in Total Emissions from PM2.5 over 3-year Periods, ",
                  "for all EI.Sectors and by each EI.Sector across United States. \n"),
            "part_2" = paste0(
                  "Total Emissions from PM2.5 over all years, ",
                  "for all EI.Sectors and by each EI.Sector across United States. \n"),
            "part_3" = paste0(
                  "Logarithmic Transformation of Emissions from PM2.5 (in tons) ",
                  "over all years, for all EI.Sectors and by each EI.Sector. \n",
                  "The figures below, highlight the changes happened in Emissions. \n",
                  "(IMPORTANT!!!  They shouldn't be used to conclude about the magnitude ",
                  "of those changes.) \n"),
            "part_4" = paste0(
                  "INFORMATION:   ",
                  "The coal combustion-related sources are categorized in four different ways ",
                  "from more general (SCC.Level.One) to more specific (SCC.Level.Four). \n",
                  "The figures below, explore the changes happened to each category ",
                  "over the 3-year periods (for all EI.Sector and by each EI.Sector) \n",
                  "by visualizing the changes of logarithmic transformation ",
                  "of total emissions for each period, for every available category. \n",
                  "( IMPORTANT!!!  They shouldn't be used to conclude about ",
                  "the magnitude of those changes.) \n")
      ),
      "bottom" = "\n\n")

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 7: Combines the plots, to create each part of the multiplot.")




################################################################################
# STEP 8: Combines the parts, to create the multiplot
#         and saves it in the working directory
################################################################################

# Construct the multiplot from the three parts,
# contained in the list created in STEP 7.
multiplot <- grid.arrange(
      grobs = parts_of_the_multiplot,
      layout_matrix = matrix(rep(1:4, times = c(1, 1, 1, 3)), ncol = 1),
      # Adds The main question at the top of the multiplot,
      # and some helpful informations about the process
      # of isolating the target data that was used to address the question.
      top = paste0(
            "QUESTION:   ",
            "Across the United States, how have emissions ",
            "from coal combustion-related sources changed from 1999–2008? \n\n",
            "INFORMATION:   In order to isolate the the observations for ",
            "coal combustion-related sources from the NEI and SCC datasets, ",
            "the variable EI.Sector was used, \n",
            "that is the reason why it is treated as an important grouping factor ",
            "for the changes in emissions. \n\n\n")
)


# Opens a new png graphical device, with resolution 1024x2048 pixels,
# plots the multiplot and saves it as 'plot_4.png' in the working directory.
png(filename = "plot4.png", width = 1024, height = 2048)
plot(multiplot)
dev.off()

# A message that is expected to appear, when this STEP had been executed successfully.
message("  DONE --> STEP 8: Combines the parts, to create the multiplot ",
        "and saves it in the working directory.")




# A message that is expected to appear, when you source the script successfully.
message("SUCCESSfully created 'plot4.png' in the working directory.")
# THE END ######################################################################

