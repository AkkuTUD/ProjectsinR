library(dplyr)
library('rstatix')
library(ggplot2)
library(FSA)
library(gridExtra)
library(grid)
library(xtable)

setwd("E:/Subjects/ICS/")


data <- data.frame(read.csv(file = 'ImmoDataRuhr.csv'))


# Summary of data set
summary(data)

# Group by summary 

group_by(data, regio2) %>%
  summarise(count = n(),mean = mean(sqmPrice, na.rm = TRUE),median = median(sqmPrice,na.rm = TRUE),sd = sd(sqmPrice, na.rm = TRUE), var = var(sqmPrice, na.rm=TRUE)) 


#xtable(summary.group.wise, type = "latex", file = "summary_group.tex")

                   ##################ASSUMPTIONS#################

#Independent variables
#
#Homogeneity
plot(res.aov, 1)
#
#In the plot below, there is no evident relationships between residuals and fitted values (the mean of each groups),
#which is good. So, we can assume the homogeneity of variances.

plot(res.aov, 2)
#As all the points fall approximately along this reference line, we can assume normality.
#
#Normality

#or

#Homogeneity
ggplot(data, aes(x= regio2,y = sqmPrice,fill = regio2))+
  geom_boxplot()+
  scale_x_discrete() + xlab("regio2") +
  ylab("sqmPrice")


#Normality for individual groups

ggplot(data, aes(sample = sqmPrice)) +
  stat_qq() +
  stat_qq_line(col = "red") + facet_wrap(~ regio2)  



#task1
#Comparing mean heights of players of 6 different sports.

#Null hypothesis: the means of the different sports are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

res.aov <- aov(sqmPrice~ regio2, data = data)
# Summary of the analysis
anova = summary(res.aov)
anova
#xtable(anova, type = "latex", file = "summary_group.tex")

#As the p-value (0.00351) is less than the significance level 0.05, we can conclude that 
#there are significant differences between the regions and rejecting null hypothesis

#task2
#
#A t-test a statistic method used to determine if there is a significant difference 
#between the means of two groups based on a sample of data.

#t test assumptions:
#continuous variable
#random sample
#normal distribution
#homogeneity

#H0: The  difference between 2 sports mean is 0 
#H1: The  difference between 2 sports mean is NOT 0 

two_sample_test_without_adj <- pairwise.t.test(data$sqmPrice, data$regio2,pool.sd = TRUE,
                                            p.adjust.method = 'none',var.equal =TRUE)
two_sample_test_without_adj


two_sample_test_with_adj <- pairwise.t.test(data$sqmPrice, data$regio2,pool.sd = TRUE,
                                            p.adjust.method = 'bon',var.equal =TRUE)
two_sample_test_with_adj




# xtable(two_sample_test_without_adj, type = "latex", file = "two_sample_test_without_adj.tex")
# xtable(two_sample_test_with_adj, type = "latex", file = "two_sample_test_with_adj.tex")



