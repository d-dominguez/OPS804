# Homework 1
library(dplyr)
library(ggplot2)
library(e1071) #add skewness and kurtosis

#Set working directory
setwd("/Users/ddomin2363/Documents/GitHub/OPS804/dd18/class 1 HW/")

## Assign your student ID as text to the following variable
student.ID <- "0940473"


## Calculate a full descriptive summary for the variable "age" including and as:
## count, sum, min, max, mean, median, range, q1, q3, iqr, sd, var, kurtosis, skewness
## into the data frame "age.desc"
survey.df <- read.csv(file = "data/surveyData.csv")

#Get first rows of data
head(survey.df)

#get col names only
colnames(survey.df)

#change col names
colnames(survey.df) <- c("timestamp","age","gender","pet","fammem","Comp",
                        "height","color","major","season","gpa","homestate",
                        "countryvisits","carcolor","cartype","ussport",
                        "mornornight","study","workout", "residence","wintersports",
                        "nopets","exerciseweek","wedclass","eatperday","naps","haircolor","smcscore")
colnames(survey.df)

#calculate descriptive summary
age.desc <- survey.df %>% 
  filter(!is.na(survey.df$age) == TRUE) %>%
  summarise(
    count = n(),
    sum = sum(age, na.rm=T),
    min = min(age, na.rm=T), 
    max = max(age, na.rm=T), 
    mean = mean(age, na.rm=T), 
    median = median(age, na.rm=T), 
    range = max - min, 
    q1 = quantile(survey.df$age, na.rm=T)[2], 
    q3 = quantile(survey.df$age, na.rm=T)[4], 
    iqr = q3 - q1, 
    sd = sd(age, na.rm=T), 
    var = var(age, na.rm=T), 
    kurtosis = kurtosis(age, na.rm=T), 
    skewness = skewness(age, na.rm=T)
  )




## Plot a boxplot including error bars using the ggplot2 package
## and save the result into the variable "age.bp" and print the result

age.bp <- ggplot(survey.df, aes("1", age)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot()

age.bp
