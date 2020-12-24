# Discussion 5
# TA: Andrew Blandino
# Midterm related Questions
library(ggplot2)
library(dplyr)

load("countypop.RData")

ggplot( countypop, aes( x= abbr, y = pop_2015 )  )+
  geom_boxplot()
# box-plots are not very informative...
# very large outliers dominate the plot
# transformation that 'reduces'

# sqrt is one option
ggplot( countypop, aes( x= abbr, y = sqrt(pop_2015) )  )+
  geom_boxplot()

# log()
ggplot( countypop, aes( x= abbr, y = log(pop_2015) )  )+
  geom_boxplot()

# log10()
ggplot( countypop, aes( x= abbr, y = log10(pop_2015) )  )+
  geom_boxplot()

# Is log10() preferable to log() here?

# Exercise: 
# Order the box-plots by the median population in each group.

# How does ggplot choose the order and how can we modify that?


# Solution:
# Different ways to do this:
# 1. ggplot plots factors based on the ordering of the levels already
levels(countypop$abbr) # no levels at start, it is a character
levels( as.factor(countypop$abbr) ) # notice alphabetical ordering was the default order
# we can manually order the levels so that ggplot reflects this
new_abbr <- factor( countypop$abbr )
# here we swapped the first and second
levels(new_abbr)[ 1:51 ]
countypop$abbr <- factor(new_abbr, levels = levels(new_abbr)[ c(2, 1, 3:51 ) ] ) 

ggplot( countypop, aes( x= abbr, y = log10(pop_2015) )  )+
  geom_boxplot()
# swapping between the last plot shows the change

# we just need
 # median_ordering <- ??
countypop$abbr <- factor(new_abbr, levels = levels(new_abbr)[ median_ordering ] ) 
# to get our results

# let's get medians for each county
medians <- countypop %>% group_by(abbr) %>% summarize(median=median(pop_2015))
medians 
# notice that abbr is already in alphabetical order in our data.frame
sort(medians$median,decreasing = TRUE)

# we now need sorting / ordering on each median

# now the order() function is what we need here 
order(medians$median,decreasing = TRUE) # vector that 'sorts' the input

medians[ order(medians$median, decreasing = TRUE) , ]
median_ordering <- order(medians$median, decreasing = TRUE)
# that gets our median ordering for our factor levels

countypop$abbr <- factor(new_abbr,
                         levels = levels(new_abbr)[ median_ordering ] ) 
countypop$abbr

ggplot(countypop, aes(x=abbr, y=log10(pop_2015)))+
  geom_boxplot()

# 2. directly set the x-axis of ggplot directly 
load("countypop.RData")

# scale_x_discrete() 
?scale_x_discrete()
# directly modify the x-axis 
# here we can set limits = 
# the ordering we used previously

ggplot(countypop, aes(x=abbr, y=log10(pop_2015)))+
  geom_boxplot()+
  scale_x_discrete(limits=levels(new_abbr)[ order(medians$median, decreasing = TRUE) ] )

# Regression Example
# diamonds dataset
# interested in how price is determined by its characteristics
?ggplot2::diamonds
# what relationships show up?

# plot data
p <- ggplot(diamonds)

# response
p + geom_histogram(aes(x=price)) # incredibly skewed
summary(diamonds$price)

p + geom_histogram(aes(x=log(price))) # less-so
summary(log(diamonds$price))

p + geom_density(aes(x=log(price))) # alternative to histogram
summary(log(diamonds$price))

# Q: Why are we interested in the 'shape' of the response?
# Heavily skewed response is 'ill-posed' for doing linear regression
# We can see if there is an improvement if we use log10(price)

# response vs. predictor plots
# boxplots for qualitative 

p + geom_boxplot(aes(y=price, x=cut))
p + geom_boxplot(aes(y=log10(price), x=cut))

p + geom_boxplot(aes(y=price, x=color))
p + geom_boxplot(aes(y=log10(price), x=color))

p + geom_boxplot(aes(y=price, x=clarity))
p + geom_boxplot(aes(y=log10(price), x=clarity))

# scatter plots for quantitative predictors
p + geom_point(aes(y=log10(price),x=carat),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=carat),method='lm') 

p + geom_point(aes(y=log10(price),x=x),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=x),method='lm') 

p + geom_point(aes(y=log10(price),x=y),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=y),method='lm') 

p + geom_point(aes(y=log10(price),x=z),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=z),method='lm') 

p + geom_point(aes(y=log10(price),x=depth),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=depth),method='lm') 

p + geom_point(aes(y=log10(price),x=table),inherit.aes = TRUE,alpha=.5)+
    geom_smooth(aes(y=log10(price),x=table),method='lm') 

# linear modeling
# Comparing models with price or log(price)
diamonds
str(diamonds) # notice ordered factors
# change this before fitting lm()
diamonds$cut <- factor(diamonds$cut,ordered = FALSE)
diamonds$color <- factor(diamonds$color,ordered = FALSE)
diamonds$clarity <- factor(diamonds$clarity,ordered = FALSE)

dm_fit <- lm(price ~ ., diamonds)
dm_fit_log <- lm( log(price) ~ ., diamonds)

# comparing summaries 
# Can we deduce improvement of one model over the other?
summary(dm_fit)
summary(dm_fit_log)
# What metric could we compare here?

# Let's inspect diagnostic plots
par(mfrow=c(2,2))
plot(dm_fit,which=c(1,2,3,5))
plot(dm_fit_log,which=c(1,2,3,5))
# is there any noticeable improvement?

# res vs. fitted: less non-linearity present
# QQ plot: improvement for log(), residuals match Normality 
# scale-location : less variability 
# res vs. leverage: noticeable difference in influential points between the two

# Based on these we choose log(response)

# Q: What do previous plots suggest about Linear model assumptions?
#    Are they reasonable?
# A: Based on previous plots
# i. linearity is reasonable (first plot: fitted curve does not have strong deviations)
# ii. Normality is reasonable (second plot: reasoanble fit to normal distribution, less some outliers)
# iii. Homoskedasticity is reasonable (third plot: doesn't present obvious trends, except
#                                       some outliers)

# Questions to answer
# Q: Is there a relationship between the predictors and the response?

# A: F-test for H_0 : all \beta_i = 0
log_summary <- summary(dm_fit_log)
# can see the F-test p-value < 2.2e-16
# strongly reject null hypothesis


# Q: What predictors are useful for predicting a diamonds price?
# A: We can answer this by individually testing H_0: \beta_i = 0, for each variable
# Every predictor is highly significant, with p-value < 2e-16

# Q: What is the relationship between the log(price) and the weight (carat) of the diamond?
# A : We can answer this by the estimated regression coefficient for carat in the model.
log_summary$coefficients # -.612 

# Interpretation: the log10(price) decreases by -.612 per 1 carat increase, on average, when all
# other terms are held constant.

# Q: Does the last question's answer contradict our original plot? What is going on?
# A: No. The other terms in the model now affect the relationship of carat. 
# Trying some simpler regression models
lm(log(price)~carat,diamonds) # matches our original plot
lm(log(price)~carat+cut,diamonds) # similar
lm(log(price)~carat+cut+color,diamonds) # similar
lm(log(price)~carat+cut+color+clarity,diamonds) # similar
lm(log(price)~carat+x,diamonds) # x,y,z affect carat
lm(log(price)~carat+y,diamonds) # x,y,z affect carat
lm(log(price)~carat+z,diamonds) # x,y,z affect carat
lm(log(price)~carat+x+y+z,diamonds) # x,y,z affect carat

# Q: Try an interaction model with carat and color. How does this compare to the previous full model?

# A: First getting the fit
dm_fit_log2 <- lm( log(price) ~ (carat + color)^2, diamonds )
log_summary2 <- summary(dm_fit_log2)

# i. can see regression relationship still holds
# ii. Not all coefficients are as significant (color = 'E')

# Q: Create a plot to visualize the results of the previous model. 
#    How does this help explain the results of our model fit?
# A: Faceting or colors are helpful here
ggplot(diamonds, aes( x=carat, y=log(price), color= color ))+
  geom_point(alpha=1/5)+
  geom_smooth(method='lm',se=FALSE)
  # notice that the curve for 'E' is the closest to 'D'
  # the coefficients are in reference to the curve 'D' (by how they are coded)
  # thus: 'E' has practically the same slope and intercept, so not significantly
  #       different from 'D'.
  #       We can also we how the next 'closest', 'F' is more significant than 'E'
  #       corresponding to our lm() summary()


# Q: Write out the regression model equation to the previous fit. What interpretations can you make?
# A: Going term by term with slope and intercept
# c(intercept, slope)
# for color='D'
c(log_summary2$coefficients[1,1],log_summary2$coefficients[2,1])
# for color='E'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[3,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[9,1])
# for color='F'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[4,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[10,1])
# for color='G'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[5,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[11,1])
# for color='H'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[6,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[12,1])
# for color='I'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[7,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[13,1])
# for color='j'
c( log_summary2$coefficients[1,1] + log_summary2$coefficients[8,1],
   log_summary2$coefficients[2,1] + log_summary2$coefficients[14,1])

# Q: Based on the previous plot, is a linear model appropiate here?
# A: Not really. There is clearly a non-linear trend of log(price) against carat
#    that is not missed.
# The following plot shows this more clearly
ggplot(diamonds, aes( x=carat, y=log(price), color= color ))+
  geom_point(alpha=1/5)+
  geom_smooth(method='lm',se=FALSE)+
  facet_grid(color~.)














