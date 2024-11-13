#Covid19 R project
# Firstly, the dataset was downloaded using Kaggle.

# To read the file:
covid19 <- read.csv("Covid19.csv")
covid19

# To know the column names
colnames(covid19)

#To show the first few rows of the dataset
head(covid19)


#DATA WRANGLING#

#To clean the dataset
complete_covid19 <- na.omit(covid19)
complete_covid19

# Showing a global view of the dataset.
install.packages("tibble")
library(tibble)
glimpse(covid19)

#Data filtering
install.packages("dplyr")
library(dplyr)
selected_data <- covid19 %>% 
  filter(Province_State == "All States") %>% #Filter the "All States" Province states
  select(-Province_State) #remove the `Province_State` column
selected_data

#To select certain columns from the dataset using data.frame
data.frame(selected_data$Country_Region, selected_data$death)

#To select certain data using data.table
library(data.table)
dt <- fread("Covid19.csv")
dt[Country_Region == "Iceland" & death > 1]


#Statistical Analysis#

# To know how big is the dataset
dim(covid19) # the outcome is 27641 observations and 12 variables
dim(complete_covid19) # the outcome is 6330 observations (number of rows) and 12 variables (number of columns)

#To calculate the mean
mean(complete_covid19$hospitalized)
mean(covid19$hospitalized, na.rm = TRUE) # ignore the N/A 

# Summary about your data
summary(complete_covid19) # the outcome is the mean, median, range (minimum and maximum value), first and third quartile.

#Standard deviation
sd(covid19$hospitalized, na.rm = TRUE)


#Data Visualization#
library(dplyr)
library(ggplot2)
ggplot(complete_covid19, aes(x = daily_positive)) + 
geom_histogram() +
xlim(0, 6500)

summary(complete_covid19$daily_positive) # Mean > Median so it's right skewed

ggplot(complete_covid19, aes(x= Province_State, y = daily_positive)) + 
geom_point() +
ylim(0, NA)


















