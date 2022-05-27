setwd(dir = 'C:/Users/Samuel Effiong/Desktop/new job r')
getwd()

# for number ALL please install this packages below. u will need
# strong internet connection to carry it out. 
# note remove " # " before running it.

#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("pander")
#install.packages("forcats")
#install.packages("pacman")
#install.packages("compareGroups")
#install.packages("moments")
#install.packages("ggpubr")
#install.packages("psychTools")
#install.packages("psych")
#install.packages("sparklyr")
#install.packages("corrr")
#install.packages("plyr")
#install.packages('rmarkdown')
#install.packages('tinytex')
#install.packages('yaml')


#after installations load the package. note remove " # " before running it.
library(pacman, quietly = T) 
library(tidyr , quietly = T)
library(dplyr , quietly = T)
library(ggplot2,quietly = T)
library(pander, quietly = T)
library(corrr)
library(scales)

# Plankton Dataset set for the project.
planktondata=read.csv(file.choose(),header=T)
planktondata


#QUSTION 1
# Use univariate statistics to analyse the plankton dataset

#FOR MEAN
planktondata %>%
  summarize(PLANKTONDATASET ="RESULT",Pseudonitzschia.A.Sp= mean(Pseudonitzschia.A.Sp),
            Alexandrium.Sp = mean(Alexandrium.Sp),
            Robgordia.Sp=mean(Robgordia.Sp),
            Water.Temp= mean(Water.Temp)) %>% 
  pander()


### FOR MEDIAN
planktondata %>%
  summarize(PLANKTONDATASET ="RESULT",Pseudonitzschia.A.Sp= median(Pseudonitzschia.A.Sp),
            Alexandrium.Sp = median(Alexandrium.Sp),
            Robgordia.Sp=median(Robgordia.Sp),
            Water.Temp= median(Water.Temp)) %>% 
  pander()

## FOR MODE
planktondata %>%
  summarize(PLANKTONDATASET ="RESULT",Pseudonitzschia.A.Sp= sd(Pseudonitzschia.A.Sp),
            Alexandrium.Sp = sd(Alexandrium.Sp),
            Robgordia.Sp=sd(Robgordia.Sp),
            Water.Temp= sd(Water.Temp)) %>% 
  pander()

#QUSTION 2
#Use a Boxplot to show the distribution of Pseudonitzschia and a second one 
#to show the distribution of water temperature

## FOR PSEUDONITZSCHIA
planktondata %>%
  ggplot(aes(Pseudonitzschia.A.Sp)) +
  geom_boxplot() +
  labs(title = "Boxplot to show the Distribution of Pseudonitzschia ",
       x = "PLANKTONDATASET",
       y = " RESULT") +
  theme_minimal()

## FOR WATER TEMPERATURE
planktondata %>%
  ggplot(aes( Water.Temp )) +
  geom_boxplot() +
  labs(title = "Boxplot to show the Distribution of Water Temperature ",
       x = "PLANKTONDATASET",
       y = " RESULT") +
  theme_minimal()

#coment on the  TWO plots ABOVE
#In the Pseudonitzschia.A.Sp it was observe that there  were more 
# extrem outliers than in the water Temperature

### QUSTION3
##Use Univariate statistics to compare data for Pseudonitzschia in the year 
#2021 with its data in previous years

planktondata  %>%
  group_by(year) %>%
  summarize(Pseudonitzschia.A.Sp = n()) %>%
  arrange(desc(Pseudonitzschia.A.Sp)) %>%
  pander()

ggplot(planktondata, aes(x = year)) + 
  geom_bar(fill = "cornflowerblue", 
           color="green") +
  labs(x = "Year", 
       y = "pseudonitzschia.A.Sp", 
       title = "compared data for Pseudonitzschia in the year 2021 and previous years")

#comment on solution 3
# form the univarite data above ,There was great improvement 
# in the year 2021 compared to other years

## QUSTION 4
ggplot(planktondata, aes(x = year)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 13) + 
  labs(title="histograms with a skewed distribution ", 
       y="Pseudonitzschia",
       x = "years")



ggplot(planktondata, aes(x = year)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 13,
                 binwidth = 8) + 
  labs(title="histograms an attribute with a normal
distribution", 
       x = "year",
       y="Pseudonitzschia")


##### QUSTION5
### Produce a bar plot for Species

plotdata <-planktondata %>%
  count(Species) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(Species, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "cornflowerblue", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Species", 
       y = "Percent", 
       title  = "A bar plot for Species")
###COMENT: WE 


#### QUSTION 6
## produce a pie chart for an attribute of your choice
plotdata <- planktondata %>%
  count(year) %>%
  arrange(desc(year)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 1*prop)

plotdata$label <- paste0(plotdata$year, "\n",
                         round(plotdata$prop), "%")


ggplot(plotdata, 
       aes(x = "year", 
           y = prop, 
           fill = year)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "red") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "white", ) +
  coord_polar("y", 
              start = 0, 
              direction = 1, ) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "A pie chart for an attribute year")
#COMMENT : 


#QUSTION 7
##use a plot to show values of Robgordia.SP AGAINST values of 
#Pseudonitzschia .A.SP where the species is either common mussels
#or pacific oysters.

planktondata %>% 
  subset(Species == "Common mussels") %>% 
  ggplot(aes(Robgordia.Sp, Pseudonitzschia.A.Sp)) +
  geom_line() +
  geom_line(data = , aes(Robgordia.Sp, Pseudonitzschia.A.Sp),color='red')+
  labs(title = "A plot to show values of Robgordia.SP AGAINST 
       values of Pseudonitzschia .A.SP",
       subtitle = "where the species is for common mussels") +
  geom_smooth(formula = y ~ x,method = "lm", se = T)

###comment:

#### Qustion 8
####Use a plot to show Alexandrium.Sp in different region by farming species

ggplot(planktondata, aes(Region,   Alexandrium.Sp)) + 
  geom_jitter(alpha = 0.2,color='red')+
  labs(title ="A plot to show Alexandrium.Sp in different region by farming species",
       subtitle = "Use jitter on the X- Axis")

###comment:

#qustion 9
# to determine the pair that our Correlated and non Correlated we first
#Testing the assumptions (Linearity and Normalcy)
#  Correlation Coefficient
#Checking for the significance
shapiro.test(planktondata$Pseudonitzschia.A.Sp)
shapiro.test(planktondata$Alexandrium.Sp)
shapiro.test(planktondata$Robgordia.Sp)
shapiro.test(planktondata$Pseudonitzschia.A.Sp)
#Normality: Using Shapiro test (This is a test of normality, here we are checking whether the variables are normally distributed or not )

cor(planktondata$Pseudonitzschia.A.Sp,planktondata$Alexandrium.Sp)  
cor(planktondata$Pseudonitzschia.A.Sp,planktondata$Robgordia.Sp)   
cor(planktondata$Alexandrium.Sp,planktondata$Pseudonitzschia.A.Sp)  
cor(planktondata$Alexandrium.Sp,planktondata$Robgordia.Sp)   
cor(planktondata$Robgordia.Sp,planktondata$Pseudonitzschia.A.Sp)  
cor(planktondata$Robgordia.Sp,planktondata$Alexandrium.Sp)  

#Correlation Coefficient : the above WE ARE TESTING FOR THE EFFICENCY

Tes<- cor.test(planktondata$Pseudonitzschia.A.Sp,planktondata$Alexandrium.Sp,method = "pearson")
Tes
Tes0<- cor.test(planktondata$Pseudonitzschia.A.Sp,planktondata$Robgordia.Sp,method = "pearson")
Tes0
Tes1<- cor.test(planktondata$Alexandrium.Sp,planktondata$Pseudonitzschia.A.Sp,method = "pearson")
Tes1
Tes2<- cor.test(planktondata$Alexandrium.Sp,planktondata$Robgordia.Sp,method = "pearson")
Tes2
Tes3<- cor.test(planktondata$Robgordia.Sp,planktondata$Pseudonitzschia.A.Sp,method = "pearson")
Tes3
Tes4<- cor.test(planktondata$Robgordia.Sp,planktondata$Alexandrium.Sp,method = "pearson")
Tes4

# or do all three correlated and non correlated together
data77 <- planktondata %>% select(Pseudonitzschia.A.Sp,Robgordia.Sp,Alexandrium.Sp)
data77
data88 <- cor(data77,method = "pearson")
data88
summary(data88)

# CONCLUSION :Since the p-value is above  0.05 
#(here it is 0.09212, 2.2e-16,0.09212, and 0.07524, 
#we can conclude that Pseudonitzschia.A.Sp,
#Alexandrium.Sp and Robgordia.Sp are significantly correlated 
#with a value of 0.06482684,0.975273 and 0.06138349 and a p-value of 0.09212, 
#2.2e-16,0.09212, and 0.07524 .
#As we can see there is a positive value between Pseudonitzschia.A.Sp ,
#Alexandrium.Sp and Robgordia.Sp, the point to be noted here is correlation
#is just a measure of association. It will tell the degree of association 
#along with the direct or indirect proportionality.

# since all three pairing are above 0.05 , we thus conclude that the is 
#no pair among planktondata( Pseudonitzschia.A.Sp Alexandrium.Sp Robgordia.Sp)

# refrence:
# Correlation Analysis Using R: 
#https://www.analyticsvidhya.com/blog/2021/01/correlation-analysis-using-r/#h2_5

#Qustion 10
#10. Produce a line plot Which shows the water temperature (y axis) 
#against the sample index (x axis), for samples of common cockles and 
#pacific oyster.

planktondata %>% 
  subset(Species == "Common cockles") %>% 
  ggplot(aes(Sample, Water.Temp)) +
  geom_line() +
  geom_line(data = , aes(Sample, Water.Temp),color='red')+
  labs(title = "a line plot Which shows the water temperature (y axis) against the sample index (x axis)",
       subtitle = "samples of common
cockles and pacific oyster. ") +
  geom_smooth(formula = y ~ x,method = "lm", se = T)

#qustion 11
#produce a linear regression model of Pseudonsitzshia.A.SP for 
#a value of Robgordia.sp of 1000,2500,and 4000 cells per litre
data101 <- planktondata %>% select(Pseudonitzschia.A.Sp,Robgordia.Sp)
data101
cor(data101,method = "pearson")
linear_regression_model <- lm(Pseudonitzschia.A.Sp ~ Robgordia.Sp,
                              data=  data101)
summary(linear_regression_model)

#linear regression model of Pseudonsitzshia.A.SP for a value of Robgordia.sp of 1000 cells per litre

cells_per_litre_1000 = -275.97689 + (9.50029*1000) 
cells_per_litre_1000

#linear regression model of Pseudonsitzshia.A.SP for a value of Robgordia.sp of 2500 cells per litre
cells_per_litre_2500 = -275.97689 + (9.50029*2500)
cells_per_litre_2500 

#linear regression model of Pseudonsitzshia.A.SP for a value of Robgordia.sp of 4000 cells per litre
cells_per_litre_4000  = -275.97689 + (9.50029*4000)
cells_per_litre_4000

##justify the appropriateness of the model and comments
#We found a significant relationship between Pseudonsitzshia.A.SP and  Robgordia.sp (p < 2.2e-16, R2 = 0.9512), with a Robgordia.sp of 1000,2500,and 4000 cells per litre there is increase.

#QUSTION 12
#Create a dat frame with three columns: month,year,and the mean of the water temperatures observed in the planktondataset during that month-year period
group <- paste(month,year)
avg <- tapply(Water.Temp, group, mean)
df <- data.frame("Average temp" = avg,"Month" = as.numeric(substring(names(avg),1,2)),"Year" = as.numeric(substring(names(avg),3)),row.names = NULL)
df
### Water temperature is 12 deg
#H0 -> mu = 12 (the water temperature is 12 degree)
#H1 -> mu != 12 (the water temperature is not 12 degree)
t.test(x = Water.Temp,alternative = "two.sided",mu = 12,conf.level = .99)
#From the obtained P-value there is no enough statistical evidence to reject H0 which state that the Water temperature is 12 degrees

#Since at 99% level of confidence the estimated mean of the water 
#temperature is between 11.8 and 12.6 12.5 should be at the extreme 
#of the sampling distribution of the mean, So if 12.5 should be moved 
#to the middle of the distribution which will distort the confidence
#interval making the mean value 12.17 far off to the left of the sampling 
#distribution, Therefore when a using type I error criteria of 95% 
#we'll obtain a low p-value making it easier to reject the null hypothesis 
#which will come to a conclusion of the water temperature not being 12.5

#QUSTION 13
#You suspect that the water temperature is affected by the period of the year. use plankton dataset to produce a test to check this suspicion,clearly highlighting the Null hypothesis and the alternative hypothesis and justifying whether the suspicion is supported by the data or not

#### water temperature afected period

#H0 -> mu1 == mu2 (Water temperature of 1st half period is equal to water temperature of the 2nd half period)
#H1 -> mu1 != mu2 (Water temperature of 1st half period is not equal to water temperature of the 2nd half period)

t.test(Water.Temp ~ period,alternative = "two.sided",conf.level = .99)
#JUSTIFICATION
#From the above analysis its clear that there is no difference between the water temperature in both the 1st  and 2nd half of the year, so we conclude that the water temperature is not affected by the period

#QUSTION 14
#O CODE NEEDED FOR THIS TASK
#Our  work  is  not  done  once  we  have  found  a  statistically 
#significant  difference  between the group means. When we calculate MSb, 
#and end up with an average difference between the group means.
#Since we are comparing four group means, we might find a relatively 
#large average difference between these group means even if two of the four 
#group means are identical. Therefore,  a statistically  significant 
#F  value  tells  us  only  that  somewhere  there  is  a meaningful 
#difference between the group means. But it does not tell us which groups differ
#from each other significantly.

#QUSTION15
#effective
plotdata <-planktondata %>%
  count(Region) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(Region, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Region", 
       y = "Percent", 
       title  = "A bar plot for Region")

#Non effectives
plotdata <-planktondata %>%
  count(Alexandrium.Sp) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(Alexandrium.Sp, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Alexandrium.Sp", 
       y = "Percent", 
       title  = "A bar plot for Alexandrium.Sp")

#comment