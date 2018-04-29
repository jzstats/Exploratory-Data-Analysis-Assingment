# Exploratory-Data-Analysis-Assingment
***

## About this repository 
 
This repository was created for the peer-graded assignment of: 

> Course 3: Exploratory Data Analysis,  
> from Data Science Specialization,  
> by Johns Hopkins University,  
> on coursera  

The course is taught by: 
 
  - Jeff Leek, Phd 
  - Roger D. Peng, Phd
  - Brian Caffo, Phd
 
As putted by the teachers of the course: 
 
> The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say   
> about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R   
> package you want to support your analysis.  
  
The assignment asked to:  
  
> You must address the following questions and tasks in your exploratory analysis.  
> For each question/task you will need to make a single plot.  
> Unless specified, you can use any plotting system in R to make your plot.  
>  
>  1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting   
>     system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005,   
>     and 2008.   
>  2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from   
>     1999 to 2008? Use the base plotting system to make a plot answering this question.  
>  3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of  
>     these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen  
>     increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.  
>  4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?  
>  5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
>  6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in  
>     Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor   
>     vehicle emissions?  
>  
> For each plot you should:  
>  
>   1. Construct the plot and save it to a PNG file.  
>   2. Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot,  
>        i.e. code in plot1.R constructs the plot1.png plot.   
>      Your code file should include code for reading the data so that the plot can be fully reproduced.   
>      You must also include the code that creates the PNG file.  
>      Only include the code for a single plot (i.e. plot1.R should only include code for producing plot1.png)   
>   3. Upload the PNG file on the Assignment submission page.     
>   4. Copy and paste the R code from the corresponding R file into the text box at the appropriate point in the   
>      peer assessment.  
  
   
## About the data used to create the plots  
 
The data used to create the plots, comes from the 'National Emissions Inventory (NEI)' database.  
Specifically the tables below were used:  
* 'PM2.5 Emissions Data' (summarySCC_PM25.rds)  
* 'Source Classification Code Table' (Source_Classification_Code.rds)  

which can be downloaded from 'UC Irvine Machine Learning Repository' through this link:  
    "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"   
The scripts will download the data, if any data file needed doesn't exists in the working directory.   
More informations on the dataset can be found on the link:  
    "https://www.epa.gov/air-emissions-inventories"  
 
