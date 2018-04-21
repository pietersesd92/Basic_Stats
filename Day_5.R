
# Day_5.R -----------------------------------------------------------------
#Shaun Pieterse
#20/4/2018
#ANOVAS

#Load libraries
library(tidyverse)
library(Rmisc)

#Load Data
snakes <- read_csv("Snakes.CSV") %>% 
  mutate(day = as.factor(day))

#Or alternatively
snakes$day <-as.factor(snakes$day)


# Summarise the data ------------------------------------------------------

snakes_summary <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))

#ANOVA assumptions being vioiated, the data is not independant
#"NA" present in data due replicate not being done
#Therefore...

snakes_summary <- snakes %>% 
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))


# Formulate a hypothesis --------------------------------------------------

#H0: There is NO difference between snakes with respect to the number of openings
#at which they habituate
#H1: There is a difference in the number of openings from day to day


# Test a hypothesis -------------------------------------------------------

#First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes, 
                             measurevar = "openings", 
                             groupvars = c("day"))

#Then visualise 
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, 
                                           y = openings - ci, 
                                           yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)


# Fit the ANOVA -----------------------------------------------------------
#To test just the days hpothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)

summary(snakes.day.aov)
#when reporting ANOVA, report degrees Degrees of freedom, report the F-value, 
#report on P value

#Test both hypothesis
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)


# Testing assumptions afterwards ------------------------------------------

#First visualize the normailty of the results
snakes.res <- residuals(snakes.all.aov)
hist(snakes.res)

#Visualise the homoscedasticity of results
plot(fitted(snakes.aov), residuals(snakes.aov))

#Use Tukey analysis to 
#Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

#Visualize the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day), y = openings, colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)


# Exercise -----------------------------------------------------------------

#Get the moth data from GitHub
#Run a two-way ANOVA on them

#Load Data
moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location)

View(moths)

#H0: There's no difference in the number of moths counted w.r.t. 
#the different trap types at each location.
#H1: There is a difference


moth_summary <- moths %>% 
  group_by(trap) %>% 
  summarise(moths_mean = mean(count),
            moths_sd = sd(count))

#Run the ANOVA and its summary
moths.aov <- aov(count ~ trap * Location, data = moths)
summary_moth<-(moths.aov)

summary_moth

#Homoscedasticity
plot(fitted(moths.aov), residuals(moths.aov))         

#Check the Tukey results
moths.tukey <- TukeyHSD(moths.aov, which = "Location")
plot(moths.tukey)

#Plotting the moth data set
ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot()

#Visualizing the data
plot1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot()+
  geom_jitter(width = 0.05, shape = 21)

plot2 <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_boxplot()+
  geom_jitter(width = 0.05, shape = 21)

plot3 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap))+
  geom_jitter(width = 0.05, shape = 21)

library(ggpubr)
ggarrange(plot1, plot2, plot3, labels="AUTO", ncol = 2, nrow = 3)

#Are the residuals normal?
moths.res <- residuals(moths.aov)
hist(moths.res)


# Regression --------------------------------------------------------------

#residual is the difference from the observed value and predicted value

#For the explanation of this statistical analysis
#We are going to use eruption data from ol' Faithful

#Look at the top of the data
head(faithful)

#plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions))+
  geom_point()


#There is a significant relationship between the duration of the eruption and 
#the the waiting time
#H0: Waiting does not have an influence on the eruption duration
#H1: Waiting time does have an influence on the eruption duration


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)


# Notes -------------------------------------------------------------------
#Estimate Std is the point at which the line of best fit intercepts the y-axis, 
#note the waiting time
#The P-value explains whether the slope of the line is significantly greater 
#than zero
#Adjusted R-squared explains the amount of variation known


# Correlations ------------------------------------------------------------

#Libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

#Load Data
ecklonia <- read_csv("data/ecklonia.csv")


# Formulate Hypothesis ----------------------------------------------------

#H0: There is no relationship between stipe length and stipe mass
#for the kelp Ecklonia maxima
#H1: There is relationship between stipe length and frond length 
#for the kelp Ecklonia maxima


# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$stipe_length, ecklonia$frond_length)


# Visualize the data ------------------------------------------------------

ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length))+
  geom_point()


# Run lots of tests at once -----------------------------------------------

#To see which factors best with one another
ecklonia_sub<-ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)

ecklonia_cor


# Spearman Rank Test ------------------------------------------------------

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), 
                                  breaks = 3))

#Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")



# Kendall Rank Test -------------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, 
         method = "kendall")


# Visualise all the things! -----------------------------------------------
ecklonia_pearson <- cor(ecklonia_sub)
corrplot(ecklonia_pearson, method = "circle")

# My Heatmap --------------------------------------------------------------

#The Correlation
cor(mtcars_sub)

cor.test(mtcars_sub$mpg, mtcars_sub$disp, method = "spearman")


mtcars_sub1 <- mtcars %>% 
  select(mpg:qsec)

mtcars_sub1 <- mtcars[, c(1,3,4,5,6,7)]
tail(mtcars_sub1)

cormat <- round(cor(mtcars_sub1),2)
tail(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

corrplot(cormat, method = "circle")

library(ggplot2)
plot1 <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "salmon", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

plot1

#The personal touch...

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
lower_tri <- get_lower_tri(cormat)

library(reshape2)
melted_cormat <- melt(lower_tri, na.rm = TRUE)

Final_plot <- plot1 +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 7,
                               title.position = "top", title.hjust = 0.5))

Final_plot
