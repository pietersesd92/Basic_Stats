
# Day_2 Basic Stats.R -----------------------------------------------------

#13 April 2018
#Data visualisations and Distributions



#Loading Libraries
library(tidyverse)


# Manual Calculations -----------------------------------------------------

#Generating dummy data

#Random Data can be generated to represent normal or skewed datasets, see below
#click TAB to generate list of argument requirements

r_dat <- rnorm(n = 600, mean = 372, sd = 50)

r_dat

#dataframe needs to be created first... or error will result

r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                   sample = "A")

#Visualizing the data
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

#Calculating the mean
#The mean is the sum of all samples, divided by number of samples

r_dat %>% 
  summary(mean.r_dat = mean(r_dat))

#Or
#Always avoid using static numbers, will cause errors
#use function "n" to get number of samples
#Code below calculates mean, sunm and count, 
#and additionally having R check the answer

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))


#Calculating the Median
r_dat %>% 
  arrange(dat) %>% 
  summarise(r_median = slice(dat, ((n() + 1)/2)))

#Using Base R?
r_dat$dat[(length(r_dat$dat)+1)/2]

#Tidy Automagic Way?
r_dat %>% 
  summarise(r_median = median(dat))

#Using tidyverse

r_dat %>% 
  arrange(dat) %>% 
  slice(n())

#Variance
#The sum of each value, minus the mean, squared
  #Divided by the count of samples, minus 1

r_dat %>% 
  mutate(r_error = dat-mean(dat))
  
#Using mutate function to add extra coloumn into existing dataset

r_dat %>% 
  mutate(r_error = dat-mean(dat),
  r_error_square = r_error*r_error) %>% 
         summarise(r_squared_sum = sum(r_error_square),
                   r_var = r_squared_sum/(n()-1),
                   r_var_func = var(dat))


#The Standard Deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))




# Exercise 1 --------------------------------------------------------------

#Summary of Chicken Weight

#Run summary to compare answers
summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))



# Visualizations ----------------------------------------------------------

#Load libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)



# Qualitative Data --------------------------------------------------------

#Loading SA Time Data
sa_time <- read_csv("data/SA_Time_Data.CSV")

#Edit our data
#New data form rewritten, despite redundancy
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6), rep("Joburg", 2)))

View(sa_time)

#Create long data
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human, -geo)

View(sa_long)


#creating count of qualitative values
sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n / sum(n)) 

sa_count

#Creating stacked bar graphs

ggplot(data = sa.count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "Cumulative Sum",
       x = NULL, y = "Count") +
  theme_minimal()

# A stacked bar graph with the relative proportions of observations
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# A basic pie chart
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie Chart", subtitle = "but why though",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()

#Bar graph with separated bars aka side-by-side bar graph
ggplot(data = sa_long, aes(x = time_type, fill = time_type)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "SA Time Data", subtitle = "Who's time though?", y = "Time(minutes)", x = "Time Type") +
  theme_minimal()

#Work in progress
#remove geo from sa_count data

# Quantitative Data -------------------------------------------------------

#Histograms
ggplot(data = sa_long, aes(x = minutes))+
  geom_histogram()

#Only one value appears

#To remove one value
sa_clean <- sa_long %>% 
  filter(minutes < 100)

#Trying again

ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type, scales = "free_x")

#Facet wrapped histogram
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#Relative Proportion Histogram
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(y = ..density.., fill = time_type), position = "dodge", binwidth = 1)+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#X-axis is different, causing bars to look different despite the binwidth being the same

#Boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type))

#The lthick black line found inside the boxes represent the median
#the upper and lower limits of the boxes refers to the quartiles
#The vertical tails show you the data sample distributed around the mean  

#Notched Boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)
  
#Notches show areas of little to no statistical difference in data

#Calculating summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>%
  summarise(time_type_mean = mean(minutes))

#plottine the mean over the box plots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)+
  geom_point(data = sa_summary_stats, size = 3, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")

#diamonds indicate mean
#Outlier is affecting the position of the mean



# Relationships -----------------------------------------------------------

#A basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point()+
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

#Adding trend lines to the scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point(aes(colour = geo))+
  geom_smooth(aes(colour = geo), method = "lm")+
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
