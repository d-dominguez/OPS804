library(dplyr)
library(ggplot2)
library(moments)
library(plotly)

# Homework

## Assign your student ID as text to the following variable

student.ID <- 0940473



## Calculate a full descriptive summary for the variable "age" including and as:
## count, sum, min, max, mean, median, range, q1, q3, iqr, sd, var, kurtosis, skewness
## into the data frame "age.desc"

#Reads data into variable "data"
data <- read.csv("homework/data/surveyData.csv")
age_data = subset(data, select = c(2))
colnames(age_data) <- "age"
transform(age_data, age = as.numeric(age))

count <- count(age_data$age)
min <- min(age_data$age)
max <- max(age_data$age)
mean <- mean(age_data$age, na.rm = FALSE)
range <- range(age_data$age, na.rm = FALSE)
q1 <- quantile(age_data$age, 0.25)
q3 <- quantile(age_data$age, 0.75)
iqr <- IQR(age_data$age, na.rm = FALSE)
sd <- sd(age_data$age, na.rm = FALSE)
var <- var(age_data$age, na.rm = FALSE)
kurtosis <- kurtosis(age_data$age, na.rm = FALSE)
skewness <- skewness(age_data$age, na.rm = FALSE)

age.desc <- data.frame(min, max, mean, q1, q3, iqr, sd, var, kurtosis, skewness)
transform(age.desc, max = as.numeric(max), min = as.numeric(min), q1 = as.numeric(q1), q3 = as.numeric(q3))



## Plot a boxplot including error bars using the ggplot2 package
## and save the result into the variable "age.bp" and print the result
age.bp <- ggplot(data = age.desc) + geom_boxplot(ymin=min, ymax=max, x=range)
age.bp
