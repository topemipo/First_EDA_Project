library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

dataset <- read_csv("School Documents/Statistics for Data Science/melanoma.csv", show_col_types = FALSE)
melanoma_dataset = select(dataset, -1)

#head(melanoma_dataset, 10)

selected_columns <- c("time", "age", "thickness")
summary(melanoma_dataset[, selected_columns])

# Count of Each Status Category
status_counts <- table(melanoma_dataset$status)
print(status_counts)

# Count of Each Status Category
sex_counts <- table(melanoma_dataset$sex)
print(sex_counts)

# Count of Each Status Category
year_counts <- table(melanoma_dataset$year)
print(year_counts)

# Count of Each Status Category
ulcer_counts <- table(melanoma_dataset$ulcer)
print(ulcer_counts)


boxplot(melanoma_dataset$time, main = "Survival Time", ylab = "Values")
boxplot(melanoma_dataset$age, main = "Patient Age", ylab = "Values")
boxplot(melanoma_dataset$thickness, main = "Tumor Thickness", ylab = "Values")

par(mfrow = c(1, 1))

# identify time outliers
Q3 <- quantile(melanoma_dataset$time, 0.75)  
IQR_value <- IQR(melanoma_dataset$time)      

upper_limit <- Q3 + 1.5 * IQR_value

outliers_time <- melanoma_dataset$time[which(melanoma_dataset$time > upper_limit)]

rows_with_outliers_time <- melanoma_dataset[melanoma_dataset$time == outliers_time, ]
print(rows_with_outliers_time)

# identify age outliers
Q1_a <- quantile(melanoma_dataset$age, 0.25)  
IQR_value_a <- IQR(melanoma_dataset$age)      

lower_limit_a <- Q1_a - 1.5 * IQR_value_a

outliers_age <- melanoma_dataset$age[which(melanoma_dataset$age < lower_limit_a)]

rows_with_outliers_age <- melanoma_dataset[melanoma_dataset$age == outliers_age, ]
print(rows_with_outliers_age)

# identify thickness outliers
Q3_th <- quantile(melanoma_dataset$thickness, 0.75)  
IQR_value_th <- IQR(melanoma_dataset$thickness)
upper_limit_th <- Q3_th + 1.5 * IQR_value_th

outliers_thickness <- melanoma_dataset$thickness[which(melanoma_dataset$thickness > upper_limit_th)]

rows_with_outliers_thickness <- melanoma_dataset[melanoma_dataset$thickness %in% outliers_thickness, ]
print(rows_with_outliers_thickness)


attach(melanoma_dataset)
cor(thickness, time, method = "pearson")
cor(age, time, method = "pearson")
cor(age, thickness, method = "pearson")

par(mfrow = c(3, 1))
#par(mfrow = c(1, 1), mar = par('mar'))
plot(thickness, time, main= 'time ~ thickness')
plot(age, time, main = 'time ~ age')
plot(age, thickness, main = 'thickness ~ age')


library(ggplot2)

# Create a scatterplot with a regression line
ggplot(melanoma_dataset, aes(x = thickness, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "time ~ thickness", x = "thickness", y = "time")



model_time_thickness = lm(formula = melanoma_dataset$time ~ melanoma_dataset$thickness)
print(model_time_thickness)
print(summary(model_time_thickness))

model_thickness_age = lm(formula = melanoma_dataset$thickness ~ melanoma_dataset$age)
print(model_thickness_age)
print(summary(model_thickness_age))

model_time_age = lm(formula = melanoma_dataset$time ~ melanoma_dataset$age)
print(model_time_age)
print(summary(model_time_age))


male_melanoma_dataset <- melanoma_dataset[melanoma_dataset$sex == 1,]
print(male_melanoma_dataset)
print(nrow(male_melanoma_dataset))

female_melanoma_dataset <- melanoma_dataset[melanoma_dataset$sex == 0,]
print(female_melanoma_dataset)
print(nrow(female_melanoma_dataset))

#time ~ thickness - test
#male
t_test_time_gender <- t.test(time ~ sex, data = melanoma_dataset)
print(t_test_time_gender)

t_test_thickness_gender <- t.test(thickness ~ sex, data = melanoma_dataset)
print(t_test_thickness_gender)

t_test_age_gender <- t.test(age ~ sex, data = melanoma_dataset)
print(t_test_age_gender)

#qqplots
# Set up the plotting area
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# QQ plot for time ~ thickness (Females)
xlabel <- "thickness"
ylabel <- "time"
female_resid <- residuals(lm(time ~ thickness, data = melanoma_dataset, subset = sex == 0))
qqnorm(female_resid, col = 'blue', main = "QQ Plot for time ~ thickness (Females)", xlab = xlabel, ylab = ylabel)
qqline(female_resid, col = 'red')

# QQ plot for time ~ thickness (Males)
male_resid <- residuals(lm(time ~ thickness, data = melanoma_dataset, subset = sex == 1))
qqnorm(male_resid, col = 'blue', main = "QQ Plot for time ~ thickness (Males)", xlab = xlabel, ylab = ylabel)
qqline(male_resid, col = 'red')

# QQ plot for time ~ age (Females)
xlabel <- "age"
ylabel <- "time"
female_resid_age <- residuals(lm(time ~ age, data = melanoma_dataset, subset = sex == 0))
qqnorm(female_resid_age, col = 'blue', main = "QQ Plot for time ~ age (Females)", xlab = xlabel, ylab = ylabel)
qqline(female_resid_age, col = 'red')

# QQ plot for time ~ age (Males)
male_resid_age <- residuals(lm(time ~ age, data = melanoma_dataset, subset = sex == 1))
qqnorm(male_resid_age, col = 'blue', main = "QQ Plot for time ~ age (Males)", xlab = xlabel, ylab = ylabel)
qqline(male_resid_age, col = 'red')

# QQ plot for thickness ~ age (Females)
xlabel <- "age"
ylabel <- "thickness"
female_resid_thickness_age <- residuals(lm(thickness ~ age, data = melanoma_dataset, subset = sex == 0))
qqnorm(female_resid_thickness_age, col = 'blue', main = "QQ Plot for thickness ~ age (Females)", xlab = xlabel, ylab = ylabel)
qqline(female_resid_thickness_age, col = 'red')

# QQ plot for thickness ~ age (Males)
male_resid_thickness_age <- residuals(lm(thickness ~ age, data = melanoma_dataset, subset = sex == 1))
qqnorm(male_resid_thickness_age, col = 'blue', main = "QQ Plot for thickness ~ age (Males)", xlab = xlabel, ylab = ylabel)
qqline(male_resid_thickness_age, col = 'red')

# Reset to default plotting parameters
par(mfrow = c(1, 1), mar = par('mar'))


# Assuming 'status' is the variable representing the patient status
# Calculate the mean age
mean_age <- mean(melanoma_dataset$age)

# Filter the dataset for patients above the mean age
above_mean_age <- subset(melanoma_dataset, age > mean_age)

# Count the breakdown of status for patients above the mean age
status_breakdown <- table(above_mean_age$status)

# Create a pie chart
pie(status_breakdown, 
    main = "Status Breakdown for Patients Above Mean Age",
    labels = c("Died from Melanoma", "Still Alive", "Died from Unrelated Causes"),
    col = c("lightcoral", "lightgreen", "lightblue"),
    border = "white"
)


