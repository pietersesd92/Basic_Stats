#Day_1.R
#The first day of stats class
#Purpose: To practice some of the concepts we will encounter
#12/04/2018

library(tidyverse)

# Integers ----------------------------------------------------------------
#Integer data is nominal

#To generate integer data
integer_r <- as.integer(seq(5, 14, by = 1))

#The dataset
integer_r 

#Running brief summary of data generated
summary(integer_r)

#REFER TO HELP FILE OFTEN!

# Continuous data ---------------------------------------------------------
#Has integer part and decimal part


#To generate continuous dataset of numeric values
numeric_r <- seq(23, 43, length.out = 10)


# Dates -------------------------------------------------------------------
#Arimetic functions can be applied to dates

#Performing some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-12")

#For example: generating a consecutive date sequence
seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

#Even summary can be compiled for dates
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
 summary(dates_r)


# Creating a new dataframe ------------------------------------------------

#Create a datafframe df_r
#NB: all datasets must be the same length
df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_r)

#Then upgrade to a tibble 
df_r <- as_tibble(df_r) 
summary(df_r)

 

# Categories of Qualitative Data ------------------------------------------

#Examples of factor variables of qualitative datasets
#Electronics
elec_r <- as.factor(c("laptops", "desktops", "cell phones"))

na.rm = brands_r
 
#People
people_r <- as.factor(c("funny hair", "beauty", "beanies"))
 
#Colours
colours_r <- as.factor(c("red", "blue"))

#You can summarise qualitative data categories
summary(colours_r) 
#Factor variables can be used to group objects under study 

 

# Ordinal Data ------------------------------------------------------------

#Also part of Qualitative data
#Is ranked or ordered by "increments"

colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"), 
                          levels = c("blue", "green", "yellow", "orange", "red"))
 
 
colour_qual
 
 

# Binary Data -------------------------------------------------------------

#Binary data defined as TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE) 
summary(binary_r)



# Character Data ----------------------------------------------------------

#"WORD" data
sites_r <- c("Yztervarkpunt", "Betty's Bay", "Gansbaai", "Sea Point")




# Missing Data ------------------------------------------------------------

#Biological data will contain missing data 
#"NA" data entries

#Example
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
summary(chicks_nest)

#Calculating mean for dataset
mean(chicks_nest)

#Calculating standard deviation
sd(chicks_nest)

#To view datasets use following commands
#head
#tail
#summary
#view
#Square brackets
#ChickWeight[c(1,54,61,12),2]





# Descriptive Statistics --------------------------------------------------
library(tidyverse)
#First Create Dataframe

chicks <- as_tibble(ChickWeight)

#Example of eaxtracring stats from dataset
#Data count
chicks %>% 
  summarise(chicken_count =n())
#Or
nrow(chicks)


# Measures of central tendency --------------------------------------------

#Calculating mean weight
#Use the pipe command to join commands together
chicks %>%
  summarise(mean_wt = mean(weight))

#Be specific 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight))

#Also use median as measure of central tendency
#If mean and median are similar then data is vormally distributed

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

#Visualising the density of the data
#Density plot shows the average occurence of data

ggplot(data = filter(chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)



# Skewness ----------------------------------------------------------------
#skewness infers where the bulk of the data lie
#Right-skewed data because bulk of dataset occur on the left of mean
#Calculate Numerical Value
#Load libraries
library(e1071)

#Compare the difference in mean and median against skewness

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))
#the median is middle of data
#the value of the skewness determines the mean position of data point
#positive values indicate right-skewed data values
#negative values indicate left-skewed data values



# Kurtosis ----------------------------------------------------------------
#kurtosis notes missing from my link?
#Calculate the kurtosis of a distribution

#A small tail indicates negative kurtosis

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))

#Calulating kurtosis

kurtosis(exp_r)

# Measures of Variability ------------------------------------------------------

#Below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))

#median is 50th quartile













