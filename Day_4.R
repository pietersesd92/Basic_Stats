
# Dy_4.R ------------------------------------------------------------------
#Shaun Pieterse
#19/4/2018
#Today's Example Exercise


#Libraries
library(tidyverse)
library(ggpubr)


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#Code above filters data for diets number 1 & 2, only at time vector 21(day 21)

chicks_sub

#so are the chicken weights different for the different diets?

# Run the t-test ----------------------------------------------------------
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
t.test(weight~Diet, data = chicks_sub)


# 1-Way ANOVA -------------------------------------------------------------

#Research Question: Is there a difference in chicken mass attained after 
#21 days, #after the chickens having been fed four different diets?

#Null Hypothesis: There is no difference in chicken mass at 21 days after having
#been fed one of four diets.

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight~Diet, data = chicks_21)
summary(chicks.aov1)

#OR

chicks.aov1 <- aov(weight~Diet, data = filter(chicks, Time == 21))

#We therefore do no accept the null hypothesis the F-value is similar to the p-value. 
#We accept the alternate hypothesis, because the 

ggplot(data = chicks_21, aes(x = Diet, y = weight))+
  geom_boxplot(aes(fill = Diet), notch = TRUE)

#When horizontal notch bandsoverlap then there is no significant difference between diets
#the widh of the notch is 1.5x the interquartile range


# Tukey HSD test ----------------------------------------------------------
#Tukey test compares factors to one another
#"diff" refers to difference in means between the factors
#"p adj" shows whether we will accept the null hypothesis 
#if lower confidence interval is positive then there is a significant difference between factors
#
TukeyHSD(chicks.aov1)

#displaying the Tukey test grapically


# ANOVA All the things ----------------------------------------------------

#Libraries
library(tidyverse)

#Visuals

chick <- ChickWeight %>% 
  filter(Time == 21)

#ANOVA
summary(aov(weight ~ Diet, data = chicks_21))

#Tukey
TukeyHSD(aov(weight ~ Diet, data = chicks_21))

#Boxplot
ggplot(data = chicks_21, aes (x = Diet, y = weight, fill = Diet))+
  geom_boxplot(notch = TRUE, colour = "grey50")+
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

#segments showing confidence intervals
#Dataframe of segments
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <-as.factor(row.names(chicks_Tukey))


ggplot(data = chicks_Tukey) +
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr))+
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed")+
  theme_bw()+
  coord_flip()


#And then there's this... The shame!
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))



# Multiple factor ANOVA ---------------------------------------------------

#H0: There is no change in chicken mass (kg) from day 0 to day 21.

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 21))

#Visualize the data
ggplot(data = chicks_0_21, aes(x = as.factor(Time), y = weight))+
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))


#Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

#Perfom a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

#Look at the confidence intervals
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21)))

#Look only at da 0 & 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#Or simply  look at ALL of the Time
#...which is NOT the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
#Note the increase in the degrees of freedom for the time factor
#But no increase for the d.f. for Diet

#Residuals: amount of variation in factors that hasn't been explained in the ANOVA summary

#How to look at interaction BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#Let's look at the Tukey Results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

#Create a line graph to help explain this concept
#First create mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

#Then visualize it
ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet))+
  geom_line(size = 2)+
  geom_point(shape = 15, size = 5)


# Non-parametric tests ----------------------------------------------------

#But what if...
#...we don't have a normal data?

#For a t-test we rather use Wilcox rank sum test
wilcox.test()#And then one  fills this in the same as for t.test()

#And now for the Kruskall-Wallis
kruskal.test(weight ~ Diet, data = chicks_0_21)

#Attempt to summarise?
summary(kruskal.test(weight ~ Diet, data = chicks_0_21))

#Load this library for a non-parametric post-hoc test
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)
