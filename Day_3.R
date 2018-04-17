# Day_3.R -----------------------------------------------------------------
#Shaun Pieterse
#17/4/2018
#Discrete & Continuous Data Distributions

knitr::include_graphics("figures/paranormal_distributions.jpeg")

#Generate a Cullen & Frey Graph
#Cullen & Frey graph used to find if data is normall

#Load Libraries
library(fitdistrplus)
library(logspline)

#To generate random normal data use function "rnorm()"
r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

r_norm

hist(r_norm) +
descdist(r_norm, discrete = FALSE, boot = 100)

#uniform data
y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1, 100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# t-tests -----------------------------------------------------------------

#Libraries
library(tidyverse)
library(plotly)

#t-tests deal with two variables, whereas the ANOVA deals with multiple variables

# Creating random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# Checking Assumptions ----------------------------------------------------

#Normality
#For this, we may use the Shapiro-Wilk Test
shapiro.test(r_dat$dat)

#This test is testing all of the data together
#Be more specific when making the test
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

#Shapiro Wilk test, the square bracket notation indicates the data group required
#Remember the data are normal when p>0.05
#The data are non-normal when p<0.05


# ChecK Homoscedasticity --------------------------------------------------

#There are many ways to check for homoscedasticity
#which is the similarity of variance between sample sets
#for now we will simply say that this assumptions is met when the variance of the 
#samples are not more than 2-4 times greater than one another

#Check everything at once?
#WRONG

var(r_dat$dat)

#or do it the tidy way

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

#Check help file for arguments within function

two_assum <- function(r_dat) {
  x_var <- var(r_dat)
  x_norm <- as.numeric(shapiro.test(r_dat)[2])
  result <- c(x_var, x_norm)
  return(result)
}
two_assum

# A one-sample t-test -----------------------------------------------------

set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

r_one

#Visualizing the t-test

#Run the t-test
t.test(r_one$dat, mu = 20)

#mu = population mean


#Check if data is normally distributed
shapiro.test(r_one$dat) #p value should be more than 0.05

#Run a test we know will produce a significant result
t.test(r_one$dat, mu = 20)

#df is the sample size minus one



# Pick a side -------------------------------------------------------------

#Two sample t-test
#Are these data smaller/less than the population mean?
t.test(r_one$dat, mu = 20, alternative = "less") #To do a comparative t-test, use the function "alternative"

#Or greater

t.test(r_one$dat, mu = 20, alternative = "greater")

#But what about for the large population mean?
#Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
#What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")


# Two sample t-test -------------------------------------------------------

#Create another dataset
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

#Run a default/basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#Assumptions testing
shapiro.test(r_two$dat)

#p-value>0.05...

#Pick a side.. Running a one-sided two sample t-test
#Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
#yep...

#Alternatively
#Is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")
#nope...
