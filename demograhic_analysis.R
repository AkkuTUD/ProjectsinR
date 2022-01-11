library(ggplot2)
library(modeest)
library(dplyr)
library(cowplot)
library(ggpmisc)
library(gt) 
library(ggrepel)
library(tibble)

#Read the Dataset from given file 
dataset = read.csv("~/Downloads/census_2021_2001.csv")

#Filter 2021 year data for first three objective using subset function 
census_2021 = subset(dataset, year==2021)  

head(census_2021)

#Frequecy Distribution of all 4 Variables
histogram_list<- list(
  (ggplot(census_2021, aes(x=life.expectancy.males))+
     geom_histogram(color="black", fill="whitesmoke", binwidth = 2)+
     geom_vline(aes(xintercept=mean(life.expectancy.males),color="blue"), size=1)+
     geom_vline(aes(xintercept = median(life.expectancy.males),col='red'), size=1)+
     scale_color_discrete(name = "Measures",labels = c("mean", "median"))+
     labs(y= "Count", x = "Age(years)")+
     theme_light()+
     ggtitle("A)")),

  (ggplot(census_2021, aes(x=life.expectancy.females))+
     geom_histogram(color="black", fill="whitesmoke", binwidth = 2)+
     geom_vline(aes(xintercept=mean(life.expectancy.females),color="blue"), size=1)+
     geom_vline(aes(xintercept = median(life.expectancy.females),col='red'), size=1)+
     scale_color_discrete(name = "Measures",labels = c("mean", "median"))+
     labs(y= "Count", x = "Age(years)")+
     theme_light()+
     ggtitle("B)")),
  
  (ggplot(census_2021, aes(x=life.expectancy.both.sexes))+
     geom_histogram(color="black", fill="whitesmoke", binwidth = 2)+
     geom_vline(aes(xintercept=mean(life.expectancy.both.sexes),color="blue"), size=1)+
     geom_vline(aes(xintercept = median(life.expectancy.both.sexes),col='red'), size=1)+
     scale_color_discrete(name = "Measures",labels = c("mean", "median"))+
     labs(y= "Count", x = "Age(years)")+
     theme_light()+
     ggtitle("C)")),
  
  
  (ggplot(census_2021, aes(x=total.fertility.rate))+
     geom_histogram(color="black", fill="whitesmoke", binwidth = 1)+
     geom_vline(aes(xintercept=mean(total.fertility.rate),color="blue"), size=1)+
     geom_vline(aes(xintercept = median(total.fertility.rate),col='red'), size=1)+
     scale_color_discrete(name = "Measures",labels = c("mean", "median"))+
     theme_light()+
     labs(y= "Count", x = "Childeren would be born to a woman")+
     ggtitle("D)")))

cowplot::plot_grid(plotlist = histogram_list)

mean(census_2021$total.fertility.rate)
2.436428
median(census_2021$total.fertility.rate)
1.9936

mean(census_2021$life.expectancy.males)
71.82719
median(census_2021$life.expectancy.males)
73.225

mean(census_2021$life.expectancy.females)
76.95632
median(census_2021$life.expectancy.females)
78.6

mean(census_2021$life.expectancy.both.sexes)
74.33044
median(census_2021$life.expectancy.both.sexes)
75.795


#Problem 2 : Bivariate correlation between the variables

ggplot(census_2021, aes(x=life.expectancy.males, y=life.expectancy.females))+
  geom_point(color="#49978D",size=3,shape=16)+
  labs(y= "Life expectancy females", x = "Life expectancy males")+
  theme_light()

ggplot(census_2021, aes(x=life.expectancy.both.sexes, y=total.fertility.rate))+
  geom_point(color="#49978D",size=3,shape=16)+
  labs(y= "Total fertility rate", x = "Life expectancy both sexes")+
  theme_light() 


#Scatter Plot Matrix

matrix <- census_2021[ , c(6,7:9)]
head(matrix)
upper.panel<-function(x, y){
  points(x,y, pch=19, col="#69b3a2")
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}

lower.panel<-function(x, y){
  points(x,y, pch=19, col="#69b3a2")
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(matrix,labels = c("total fertility rate", 
                        "life expectancy both sexes", 
                        "life expectancy males", 
                        "life expectancy females"), lower.panel = lower.panel, upper.panel = upper.panel, cex.labels = 1.2)

#Problem 3: Homogeneity & Heterogeneity  
#Factoring the subregion
census_2021$subregion <- factor(census_2021$subregion, levels = unique(census_2021$subregion[order(census_2021$region)]))

ggplot(census_2021, aes(x=total.fertility.rate, y=subregion, color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1,outlier.size=2, lwd=0.7)+
  labs(y= "Subregion", x = "Total fertility rate")+
  theme_light()

ggplot(census_2021, aes(x=life.expectancy.both.sexes, y=subregion, color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1,outlier.size=2, lwd=0.7)+
  labs(y= "Subregion", x = "Life expectancy both sexes")+
  theme_light()

#Extra
ggplot(census_2021, aes(x=life.expectancy.males, y=subregion, color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1,outlier.size=2, lwd=0.7)+
  labs(y= "subregion", x = "life expectancy at birth males")+
  theme_light()

ggplot(census_2021, aes(x=life.expectancy.females, y=subregion, color=region)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=1,outlier.size=2, lwd=0.7)+
  labs(y= "subregion", x = "life expectancy at birth females")+
  theme_light()

#Problem 4 : Compare 2001 and 2021 dataset

#Subset of the data for year 2001
census_2001= subset(dataset, year==2001)

#collecting the missing data 
Missing_data <- census_2001[is.na(census_2001$total.fertility.rate),]

#filter and remove the data from data set which are missing.
#dataset = dataset[!(dataset$country %in% Missing_data$country),]
census_2001 = census_2001[!(census_2001$country %in% Missing_data$country),]
census_2021 = census_2021[!(census_2021$country %in% Missing_data$country),]

ggplot(census_2021, aes(x=census_2001$total.fertility.rate, y=total.fertility.rate,color=region, label=country)) +
  geom_point(size = 3.5, alpha=0.7)+xlab("2001(year)")+ylab("2021(year)")+
  geom_abline()+
  ggtitle("Total Fertility Rate")+
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.01)

mean(census_2001$total.fertility.rate)#3.069
mean(census_2021$total.fertility.rate)#2.416

ggplot(census_2021, aes(x=census_2001$life.expectancy.both.sexes, y=life.expectancy.both.sexes,color=region, label=country)) +
  geom_point(size = 3.5, alpha=0.7) +xlab("2001(year)") +
  ylab("2021(year)")+
  geom_abline()+
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.01)+
  ggtitle("Life Expectancy Both Sexes")

mean(census_2001$life.expectancy.both.sexes)#68.41
mean(census_2021$life.expectancy.both.sexes)#74.36

#Extra
ggplot(census_2021, aes(x=census_2001$life.expectancy.males, y=life.expectancy.males,color=region, label=country)) +
  geom_point( alpha=0.7, size=3.5)+xlab("2001") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.01)+
  ylab("2021")+
  ggtitle("life expectancy at birth males")

ggplot(census_2021, aes(x=census_2001$life.expectancy.females, y=life.expectancy.females,color=region,label=country)) +
  geom_point( alpha=0.7, size=3.5)+xlab("2001") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.01)+
  ylab("2021")+
  ggtitle("life expectancy at birth females")

#Table Code
#Restting the data
census_2021 = dataset
census_2021 = subset(dataset, year==2021)  
by_subregions = census_2021 %>% group_by(census_2021$subregion) 


total_fertility_rate_Values =  by_subregions %>% summarise(
  Mean = format(round(mean(total.fertility.rate), 3), nsmall = 3),
  Median = format(round(median(total.fertility.rate), 3), nsmall = 3),
  SD = format(round(sd(total.fertility.rate), 3), nsmall = 3),
  Variance = format(round(var(total.fertility.rate), 3), nsmall = 3),
  IQR = format(round(IQR(total.fertility.rate), 3), nsmall = 3)
)

total_fertility_rate_Values %>%
  gt() %>%
  tab_header(
    title = "Table 1 : Measure of dispersion of total fertility rate by subregions"
  )%>%
  cols_label(`census_2021$subregion` = 'Subregion', SD = 'Standard Deviation')



life_expectancy_Values = by_subregions %>% summarise(
  Mean = format(round(mean(life.expectancy.both.sexes), 3), nsmall = 3),
  Median = format(round(median(life.expectancy.both.sexes), 3), nsmall = 3),
  SD = format(round(sd(life.expectancy.both.sexes), 3), nsmall = 3),
  Variance = format(round(var(life.expectancy.both.sexes), 3), nsmall = 3),
  IQR = format(round(IQR(life.expectancy.both.sexes), 3), nsmall = 3)
)

life_expectancy_Values %>%
  gt() %>%
  tab_header(
    title = "Table 2 : Mesare of dispersion of life expectancy both sexes by subregions"
  )%>%
  cols_label(`census_2021$subregion` = 'subregion', SD = 'Standard Deviation')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(census_2021$life.expectancy.females)
print(result)






