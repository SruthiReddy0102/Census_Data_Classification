#Performing Decision-Tree and Logistic Regression Comparison Model using Census Income Data

#Importing the Required Packages
library(tidyverse)  # data manipulation
library(caret)      # rocr analysis
library(ROCR)       # rocr analysis
library(gridExtra)  # arranging ggplot in grid
library(rpart)      # decision tree
library(rpart.plot) # decision tree plotting
library(caTools)    # (rolling, running) window statistic functions
library(ggplot2)    # create complex plots from data in a data frame.
library(class)      # Print a Vector of class names
library(C50)        # Generate decision Tree
library(dplyr)      # Used for data manipulation challenges
library(plyr)       # split data apart, do stuff to it, and mash it back together. 
library(tidyr)      # Changing the shape & hierarchy of the data
library(gmodels)    # Display the frequencies and relative frequencies of observations
library(psych)      # Used to check correlation between two variables
library(Hmisc)      # Used for data analysis like imputing missing values,variable clustering etc.
library(MASS)       # Modern Applied Statistics with S
library(Amelia)     # multiple imputation of multivariate incomplete data
library(Rmisc)      # collection of functions useful for data analysis and utility operations
library(corrplot)   # a visual exploratory tool on correlation matrix

#Loading the dataset
census_data = read.csv("F:/Salford Uni/CourseWork/ASDM/Task 1/CensusData.csv")
View(census_data)
head(census_data) # Gives Top records
tail(census_data) # Gives Bottom records
str(census_data) # Structure of Data-set
summary(census_data) # Statistical Analysis of Data-set
attach(census_data)

######Exploring the Dataset#####
#Changing income to 0, 1
census_data$Income <- as.factor(census_data$Income)
count(census_data$Income)
census_data$Income <- as.numeric(census_data$Income)-1

#Correlation plot
num.var <- c(1, 3, 5, 11:13, 15) 
corrplot(cor(census_data[,num.var]))

############### Data Pre Processing ##########################

# Converting categorical variables to Factors
census_data$workclass <- as.factor(census_data$workclass)
census_data$education <- as.factor(census_data$education)
census_data$marital.status <- as.factor(census_data$marital.status)
census_data$occupation <- as.factor(census_data$occupation)
census_data$relationship <- as.factor(census_data$relationship)
census_data$race <- as.factor(census_data$race)
census_data$sex <- as.factor(census_data$sex)
census_data$native.country <- as.factor(census_data$native.country)
census_data$Income <- as.factor(census_data$Income)
str(census_data)

# Replacing ' ? ' with NA in Data-set
census_data[census_data == ' ?'] = NA 

# Checking for the percentage of missing values
print(sapply(census_data, function(census_data){ sum(is.na(census_data)==T) * 100 /length(census_data) }))

#Graphical Representation of Missingness Map for resizing
missmap(census_data,
        main = "Missingness Map of Census Income Dataset",
        y.labels = NULL,
        y.at = NULL)

# Dropping missing values of "workclass","Occupation" and native country as it consists only 1% of data
census_data <- na.omit(census_data)

# Final output of missing map
missmap(census_data,
        main = "Census Income Data Without Missing Values",
        y.labels = NULL,
        y.at = NULL)

################### Data Visualization and analysis ################
#Income 
ggplot(data = census_data, mapping = aes(x = census_data$Income, fill = census_data$Income)) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..)/sum(..count..)),y = (..count..)/sum(..count..) ), stat = "count",vjust = -.1) +
  scale_fill_manual(values=c("darkolivegreen", "darkmagenta"))+
  labs(x = "Income",  y = "",fill = "Income") +
  scale_y_continuous()

# Age
summary(census_data$age)

# Boxplot of age
ggplot(mapping = aes(x = factor(0), y = age),data = census_data) + 
  geom_boxplot(fill = "yellow") +
  stat_summary(fun = mean, geom = 'point',shape = 19,color = "red",cex = 2) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  ylab("Age") + xlab("") +  
  ggtitle("Box Plot of Age") +
  scale_x_discrete(breaks = NULL)

# Density plot of age 
ggplot(census_data, aes(x = age, fill = Income)) + 
  geom_density(alpha = 0.8)+ 
  scale_fill_manual(values=c("goldenrod2","deeppink4")) +
  ggtitle("The age distribution is right-skewed",subtitle = "The median age of high-income groups is higher") +
  labs(y = NULL)

# Workclass
table(census_data$workclass)
#Graph for % of people belongs to each category of workclass
census_data$workclass <- factor(census_data$workclass, 
                             levels = 
                               names(sort(table(census_data$workclass),decreasing = TRUE)))
# Workclass Vs Income
ggplot(census_data, aes(x = census_data$workclass, fill = census_data$Income)) + 
  geom_bar() + scale_fill_manual(values=c("forestgreen","darkred"))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Finalweight vs Income
ggplot(census_data, aes(x = fnlwgt, fill = Income)) + 
  geom_density(alpha = 0.8) + 
  scale_fill_manual(values=c("deeppink4","lightsalmon"))+
  ggtitle("No Significant Difference in fnlwgt") + 
  labs(y = NULL)

#Hours per week
summary(census_data$hours.per.week)

# Average hours per week vs Age
ggplot(mapping = aes(x = age, y = hours.per.week),data = census_data) + 
  geom_line(mapping = aes(color = Income), stat = 'summary', fun.y = mean) +
  geom_smooth(mapping = aes(color = Income)) +
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  labs(x = "Age", y = "Average Hours Per Week") +
  ggtitle("Average Hours Per Week vs. Age")

# Hours per Week Vs Income
ggplot(aes(x = Income, y = hours.per.week),data = census_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean,geom = 'point',shape = 19, color = "red", cex = 2) +
  coord_cartesian(ylim = c(30, 60))+
  scale_y_continuous(breaks = seq(30, 60, 1)) +
  ylab("Hours per Week") +
  xlab("Income") +  
  ggtitle("Box Plot of Hours per Week by Income") 

# Education
census_data$education <- factor(census_data$education,levels = names(sort(table(census_data$education),decreasing = TRUE)))
ggplot(census_data, aes(x = census_data$education, fill = census_data$education)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y = (..count..)/sum(..count..) ),stat = "count",vjust = -.1,size = 3.5) +
  labs(x = "Education", y = "",fill = "Education") +
  scale_fill_manual(values=c(rainbow(16)))+
  theme(legend.position = 'none',axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous()

ggplot(census_data, aes(x = census_data$education.num, fill = census_data$Income)) + 
  geom_density(alpha = 0.8) + 
  scale_fill_manual(values=c(rainbow(2)))+
  ggtitle("Most low-income groups have 9-10 years of education") +
  labs(x = "education/year", y = NULL)

# Marital Status Vs Income
ggplot(census_data) + 
  geom_bar(aes(marital.status, fill = Income), position = "fill") + 
  scale_fill_manual(values=c("mediumpurple3","orange")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5))

#RelationShip Vs Income
census_data %>% group_by(Income, relationship) %>% dplyr::summarise(n = n()) %>% ggplot(aes(relationship,n, fill = Income)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.ticks.length = unit(0.5, "cm"), axis.text.x = element_text(angle = 330)) + 
  guides(fill = guide_legend(title = NULL)) +
  coord_flip() + labs(x = NULL, y = NULL) + ggtitle("Families with children are overwhelmingly low-income")

#Race Vs Income
summary(census_data$race) 

lg.race <- lapply(levels(census_data$race), function(v){
  ggplot(data = subset(census_data, census_data$race == v), aes(x = subset(census_data, census_data$race == v)$Income,fill = subset(census_data, census_data$race == v)$Income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y = (..count..)/sum(..count..) ),stat = "count",vjust = c(2, -0.1)) +
    labs(x = "Income",y = "",fill = "Income") +
    scale_fill_manual(values=c("maroon2","mediumblue")) +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',plot.title = element_text(size = 11, face = "bold")) +     
    scale_y_continuous()})
grid.arrange(grobs = lg.race, ncol = 3)

#Occupation Vs Income
occ <- as.data.frame(table(census_data$occupation)) %>% arrange(desc(Freq))
census_data %>% filter(occupation %in% head(occ, 7)$Var1) %>% group_by(Income, occupation) %>% 
  dplyr::summarise(n = n()) %>% ggplot(aes(occupation, n, fill = Income)) + geom_bar(stat = "identity", position = "dodge")+
  theme(axis.ticks.length = unit(0.5, "cm"), axis.text.x = element_text(angle = 330)) + 
  guides(fill = guide_legend(title = NULL)) + coord_flip() + labs(x = NULL, y = NULL) + 
  ggtitle("Prof-specialty & Exec-managerial paid really well!")

#Sex
census_data %>% group_by(sex) %>% dplyr::summarise(n = n()) %>% ggplot(aes(x = "", y = n, fill = sex)) + 
  geom_bar(stat = "identity", width = 2) + 
  coord_polar("y") + geom_text(aes(label = n),position = position_stack(vjust = 0.5), check_overlap = T, size = 5) + 
  labs(x = NULL,y = NULL, fill = NULL, title = "Proportion of Gender") + 
  scale_fill_manual(values=c(rainbow(8))) +
  theme(axis.line = element_blank(),axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size = 14))

# Sex vs Income
gender.income <- lapply(levels(census_data$sex), function(v){
  ggplot(data = subset(census_data, census_data$sex == v), aes(x = subset(census_data, census_data$sex == v)$Income,fill = subset(census_data, census_data$sex == v)$Income))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y = (..count..)/sum(..count..) ),stat = "count",vjust = -0.1,size = 3) +
    labs(x = "Income",y = "",fill = "Income") +
    scale_fill_manual(values=c(rainbow(8))) +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',plot.title = element_text(size = 11, face = "bold"),axis.text.x = element_text(hjust = 1)) +     
    scale_y_continuous() })

grid.arrange(grobs = gender.income, ncol = 2)

#Capital Gain and Capital Loss
capital_gain <- census_data %>% filter(capital.gain < 20000) %>% ggplot(aes(x = capital.gain, fill = Income)) + 
  geom_density(alpha = 0.8) +
  scale_fill_manual(values=c("deeppink4","lightsalmon")) +
  ggtitle("The capital gain gap between the two income groups is huge") + 
  labs(y = NULL)
capital_loss <- census_data %>% ggplot(aes(x = capital.loss, fill = Income)) + 
  geom_density(alpha = 0.8) + 
  scale_fill_manual(values=c("deeppink4","lightsalmon")) +
  ggtitle("The high-income group peaked at 1,900") + 
  labs(y = NULL)
Rmisc::multiplot(capital_gain, capital_loss)
