#Import libraries
library(tidyverse)
library(corrplot)
library(lubridate)
library(stats)
library(xts)
library(caTools)
library(e1071)

#Import dataset
weather_df <- read.csv("weatherHistory.csv")
head(weather_df)

#Summary stats and structure of columns in dataset
str(weather_df)
summary(weather_df)

#----------------------Pre-processing---------------------#
#Checking for missing data in the entire df
colSums(is.na(weather_df))

#Check column names
colnames(weather_df)
#Rename columns
colnames(weather_df) <- tolower(colnames(weather_df))
colnames(weather_df) <- gsub("\\.", "_", colnames(weather_df))
colnames(weather_df) <- gsub("_+", "_", colnames(weather_df))
colnames(weather_df) <- gsub("_$", "", colnames(weather_df))
#View changes
colnames(weather_df)

#Drop 'loud_cover' as the values are the same across all rows of data
weather_df <- subset(weather_df, select = -c(loud_cover))

#Drop 'daily_summary' as it provides the same info as the 'summary' column
weather_df <- subset(weather_df, select = -c(daily_summary))

#Replacing 'null' with 'other'
weather_df$precip_type <- ifelse(weather_df$precip_type == "null", "other", 
                                 weather_df$precip_type)

#Convert datetime column from character to POSIXct
weather_df$formatted_date <- ymd_hms(weather_df$formatted_date)
str(weather_df)

#Adjust time zone based on the offset
weather_df$formatted_date <- with_tz(weather_df$formatted_date, 
                                     ifelse(attr(weather_df$formatted_date, 
                                                 "tzone") == "CEST", "Europe/Paris", "CET"))
str(weather_df)

#Create new columns for year and month from datetime column
weather_df$year <- year(weather_df$formatted_date)
weather_df$month <- month(weather_df$formatted_date)


year_counts <- table(weather_df$year)
print(year_counts)

#----------------------EDA---------------------#
#Temperature and precipitation type
#Summary statistics for temperature
summary(weather_df$temperature_c)
str(weather_df$temperature_c)

#Skewness for temperature_c column
hist(weather_df$temperature_c)
skewness(weather_df$temperature_c)

#Count for each observation type for precipitation
table(weather_df$precip_type)

#Barplot for each precipitation type - with counts on the bars
ggplot(weather_df, aes(x = precip_type)) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Count of Precipitation Types",
       x = "Precipitation Type",
       y = "Count") +
  theme_minimal()

#Box plots showing the distribution of temperature for each precipitation type
ggplot(weather_df, aes(x = precip_type, y = temperature_c, fill = precip_type)) +
  geom_boxplot() +
  labs(title = "Precipitation types against Temperature",
       x = "Precipitation Type",
       y = "Temperature in Celsius (C)") +
  theme_minimal() +
  guides(fill = FALSE)

#Statistical test to determine if the relationship is significant between temp and precio type
#ANOVA test
anova_result <- aov(temperature_c ~ precip_type, data = weather_df)

#ANOVA summary
summary(anova_result)

#Tukey's post-hoc test - significance across every precipitation type
posthoc_result <- TukeyHSD(anova_result)

#Results
print(posthoc_result)

#----------------------Correlation Analysis---------------------#
#Selecting the numeric columns for correlation analysis - Pearson method
numeric_columns <- weather_df %>%
  select(temperature_c, apparent_temperature_c,	humidity, wind_speed_km_h, 
         wind_bearing_degrees, visibility_km, pressure_millibars)

#Calculate correlation matrix with the Pearson method
correlation_matrix <- cor(numeric_columns)

#Print correlation matrix
print(correlation_matrix)

#Visualise correlation matrix using heatmap
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.col = "black")

#Title
mtext("Heatmap showing Correlation matrix for the variables in the dataset", 
      side = 3, line = 2, cex = 1.5)

#----------------------Temporal Analysis---------------------#
#Avg temp per year
weather_df %>%
  group_by(year) %>%
  summarise(avg_temperature = mean(temperature_c, na.rm = TRUE))

#Bar plot for avg temp per year
weather_df %>%
  group_by(year) %>%
  summarise(avg_temperature = mean(temperature_c, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(year), y = avg_temperature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Average Temperature in degrees Celcius (C)", title = "Average Temperature by Year") +
  theme_minimal()

#Avg temp per month
weather_df %>%
  group_by(month) %>%
  summarise(avg_temperature = mean(temperature_c, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(month), y = avg_temperature)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Average Temperature in degrees Celsius (C)", title = "Average Temperature by Month") +
  theme_minimal() +
  ylim(0, 25)

#Avg visibility per year
weather_df %>%
  group_by(year) %>%
  summarise(avg_visibility = mean(visibility_km, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(year), y = avg_visibility)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Average Visibility", title = "Average Visibility by Year") +
  theme_minimal()

#Avg visibility per month
weather_df %>%
  group_by(month) %>%
  summarise(avg_visibility = mean(visibility_km, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(month), y = avg_visibility)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Average Visibility", title = "Average Visibility by Month") +
  theme_minimal()

#----------------------Linear regression---------------------#
set.seed(123) #for reproducibility

#Splitting the data into train and test set (80:20)
sample <- sample.split(weather_df$temperature_c, SplitRatio = 0.8)
train <- subset(weather_df, sample == TRUE)
test <- subset(weather_df, sample == FALSE)

#Dimensions of the subsets
dim(train)
dim(test)

#Linear regression model with 'temperature_c' as the target variable
lm_model <- lm(temperature_c ~ humidity + wind_speed_km_h + wind_bearing_degrees + 
                 visibility_km + pressure_millibars + precip_type, data = train)

#Print the summary of the regression model
summary(lm_model)

#model residuals
model_residuals = lm_model$residuals

#Plot the residuals
qqnorm(model_residuals)
#Plot the Q-Q line
qqline(model_residuals)

#Normailty test
shapiro.test(model_residuals) #sample size exceeds 5000 so test cannot be run

length(model_residuals) #residuals sample size is 77401

#Evaluate model on test set
predictions <- predict(lm_model, newdata = test)

#evaluation metrics
#mse
mse <- mean((test$temperature_c - predictions)^2)
mse

#RMSE
rmse <- sqrt(mse)
rmse

#MAE
mae <- mean(abs(test$temperature_c - predictions))
mae

#R-squared
rsquared <- 1 - sum((test$temperature_c - predictions)^2) / sum((test$temperature_c - mean(test$temperature_c))^2)
rsquared

#normality checks
residuals <- residuals(lm_model)

#Q-Q plot
qqnorm(residuals)
qqline(residuals)

#data frame with actual and predicted values
scatter_data <- data.frame(Actual = test$temperature_c, Predicted = predictions)

#scatter plot
ggplot(scatter_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  #Adds a diagonal line for reference
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Temperature (C)",
       y = "Predicted Temperature (C)")
