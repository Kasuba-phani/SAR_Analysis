library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(cluster)
library(factoextra)
library(mice)
library(VIM)
library(missForest)
library(ROCR)
library(e1071)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(adabag)
library(NeuralNetTools)
library(pROC)
library(kernlab)
library(nnet)
library(neuralnet)
library(lubridate)



SAR<-read.csv("C:/Users/kasub/Desktop/CSDA 6010/PROJECT_1/SAR Rental.csv")
SAR
##Checking the data-----------------------------------------------------------------------------

#View the structure of the dataset
str(SAR)
# Summary statistics for each column
summary(SAR)

# Check for missing values
colSums(is.na(SAR))

# Check for duplicated rows
sum(duplicated(SAR))

#unique
length(unique(SAR$user_id))

## Distance
# Function to calculate distance between two points using Haversine formula
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert decimal degrees to radians
  lat1_rad <- lat1 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lon2_rad <- lon2 * pi / 180
  
  # Radius of the Earth in kilometers
  R <- 6371
  
  # Differences in coordinates
  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad
  
  # Haversine formula
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Calculate distance
  distance <- R * c
  
  return(distance)
}

# Add distances to your SAR dataframe
SAR <- SAR %>%
  rowwise() %>%
  mutate(
    distance_miles = calculate_distance(from_lat, from_long, to_lat, to_long)* 0.621371,
  ) %>%
  ungroup()

# View the updated data frame
head(SAR)
str(SAR)
colSums(is.na(SAR))

## Handling Missing data
#Handeling Categorical data
SAR_1 <- SAR

# KNN Imputation for spatial co-oridnate attributes (latitude and longitude)
library('VIM')
location_cols <- c("from_lat", "from_long", "to_lat", "to_long")
existing_location_cols <- location_cols[location_cols %in% names(SAR_1)]
if (length(existing_location_cols) > 0) {
  SAR_1 <- kNN(SAR_1, variable = existing_location_cols, k = 5)
}
str(SAR_1)
colSums(is.na(SAR_1))

##Using MICE for distance
# Select only the relevant columns for comparison
columns_to_compare <- c("from_lat", "from_long", "to_lat", "to_long")

library(mice)

# Select relevant predictors for "distance_miles"
predictors <- c("distance_miles", "from_lat", "from_long", "to_lat", "to_long")

# Ensure the selected columns exist in SAR_1
predictors <- predictors[predictors %in% names(SAR_1)]

# Perform MICE imputation using the predictors
mice_model <- mice(SAR_1[, predictors], method = "pmm", m = 5)

# Replace missing values in "distance_miles" with the imputed ones
SAR_1$distance_miles <- complete(mice_model)$distance_miles

colSums(is.na(SAR_1))

##handling categorical data

# Convert all columns to factors, except for numerical columns
SAR_1 <- SAR_1 %>% mutate(across(-c(user_id,distance_miles,from_date,to_date,from_lat, 
                                    from_long, to_lat,to_long,booking_created),as.factor))


#check if "from_city_id" always corresponds to a unique from_lat and from_long
from_city <- SAR %>%
  group_by(from_city_id) %>%
  summarise(unique_from_lat = n_distinct(from_lat),
            unique_from_long = n_distinct(from_long))

# Check if "to_city_id" always corresponds to a unique to_lat and to_long
to_city <- SAR %>%
  group_by(to_city_id) %>%
  summarise(unique_to_lat = n_distinct(to_lat),
            unique_to_long = n_distinct(to_long))

# Print the first few rows as proof
head(from_city)
head(to_city)

#dropping "from_city_id" and "to_city_id" 
SAR_1 <- SAR_1 %>% select(-from_city_id,-to_city_id)

#Function to impute missing "area_id" using mode of existing values
impute_mode <- function(df, target_col, lat_col, long_col) {
  df <- df %>%
    group_by(.data[[lat_col]], .data[[long_col]]) %>%
    mutate(!!target_col := ifelse(is.na(.data[[target_col]]),
                                  as.numeric(names(sort(table(.data[[target_col]]), decreasing = TRUE)[1])),
                                  .data[[target_col]])) %>%
    ungroup()
  return(df)
}

# Apply mode imputation
SAR_1 <- impute_mode(SAR_1, "from_area_id", "from_lat", "from_long")
SAR_1 <- impute_mode(SAR_1, "to_area_id", "to_lat", "to_long")

SAR_1

colSums(is.na(SAR_1))

###Date and time Columns
# Convert the 'Booking Created', 'from_date', 'to_date' to date-time format
SAR_1$from_date = as.POSIXct(SAR$from_date, format = '%m/%d/%Y %H:%M' )
SAR_1$to_date = as.POSIXct(SAR$to_date, format = '%m/%d/%Y %H:%M')
SAR_1$booking_created = as.POSIXct(SAR$booking_created, format = '%m/%d/%Y %H:%M')

# Waiting period calculation
SAR_1$time_diff_hours <- as.numeric(difftime(SAR_1$from_date, SAR_1$booking_created, units = "hours"))
str(SAR_1)
colSums(is.na(SAR_1))

# Plot the distribution of distance_km when to_date is NA
distance_na_data <- SAR_1[is.na(SAR_1$to_date), ]
ggplot(distance_na_data, aes(x = distance_miles)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distance Distribution (distance_Miles) when to_date is NA",
    x = "Distance (Miles)",
    y = "Frequency"
  ) +
  theme_minimal()

SAR_1$to_date[is.na(SAR_1$to_date)] <- SAR_1$from_date[is.na(SAR_1$to_date)]

#Adding other method's column
SAR_1 <- SAR_1 %>%
  mutate(other_methods = ifelse(online_booking == 0 & mobile_site_booking == 0, 1, 0))


# Separate the from_date column into date and time
SAR_1 <- SAR_1 %>%
  mutate(
    # Extract date and time parts for from_date
    from_date_date = as.Date(from_date),  
    from_date_time = format(from_date, "%H:%M:%S"),  
    
    # Extract date and time parts for to_date
    to_date_date = as.Date(to_date),
    to_date_time = format(to_date, "%H:%M:%S"),
    
    # Extract date and time parts for Booking_created
    Booking_date = as.Date(booking_created),
    Booking_time = format(booking_created, "%H:%M:%S")
  )

str(SAR_1)
colSums(is.na(SAR_1))

# Extract hour and day of the week
library(lubridate)
SAR_1$hour <- hour(SAR_1$from_date)
SAR_1$day <- weekdays(SAR_1$from_date, abbreviate = TRUE)
# Load necessary libraries
library(tidyverse)


library(lubridate)
library(hms)


#Changing dtype of from_date_time, To_date_time and Booking_time.

SAR_1$from_date_time <- as_hms(SAR_1$from_date_time)
SAR_1$to_date_time <- as_hms(SAR_1$to_date_time)
SAR_1$Booking_time <- as_hms(SAR_1$Booking_time)
str(SAR_1)
colSums(is.na(SAR_1))
# dropping Package_id
SAR_1 <- SAR_1 %>% select(c(-package_id, -row.))

SAR_1 <- SAR_1 %>%
  mutate(across(c(day,other_methods,from_area_id,to_area_id), as.factor))

str(SAR_1)
colSums(is.na(SAR_1))


## Handeling outliers
#for Numerical data
# Function to count outliers using IQR
count_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Count outliers
  outlier_count <- sum(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, na.rm = TRUE)
  return(outlier_count)
}

# Select numerical columns excluding 'user_id'
numeric_cols <- setdiff(names(SAR_1)[sapply(SAR_1, is.numeric)], "user_id")

# Count outliers for each numerical column
outlier_counts <- sapply(numeric_cols, function(col) count_outliers(SAR_1, col))
outlier_counts
# Find columns that contain outliers
outlier_cols <- names(outlier_counts[outlier_counts > 0])

#Plot boxplots for outliers BEFORE treatment
par(mfrow = c(length(outlier_cols), 1))  # Adjust layout for multiple plots

for (col in outlier_cols) {
  boxplot(SAR_1[[col]], 
          main = paste("Boxplot for", col, "(Before Winsorization)"),
          col = "lightblue", horizontal = TRUE, xlab = col)
}

# Function to apply Winsorization with IQR-based capping
winsorize_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Print summary before Winsorization
  cat("\nBefore Winsorization -", column, "\n")
  print(summary(data[[column]]))
  
  # Winsorization: Replace extreme values with threshold values
  data[[column]] <- pmin(pmax(data[[column]], lower_bound), upper_bound)
  
  # Print summary after Winsorization
  cat("\nAfter Winsorization -", column, "\n")
  print(summary(data[[column]]))
  
  return(data)
}

# Apply Winsorization to all outlier columns
for (col in outlier_cols) {
  SAR_1 <- winsorize_outliers(SAR_1, col)
}

# Set up plotting to visualize Winsorization effect
par(mfrow = c(length(outlier_cols), 1))

# Plot boxplots AFTER Winsorization for all outlier columns
for (col in outlier_cols) {
  boxplot(SAR_1[[col]], 
          main = paste("Boxplot for", col, "(After Winsorization)"), 
          col = "orange", horizontal = TRUE, xlab = col)
  
  abline(v = median(SAR_1[[col]], na.rm = TRUE), col = "red", lty = 2) # Median line for reference
}

#For categorical columns performing Chi Squared test
# Function to perform chi-squared test
perform_chi_squared_tests <- function(data, target_variable) {
  # Ensure the target variable is a factor
  data[[target_variable]] <- as.factor(data[[target_variable]])
  
  # Initialize a results list
  results <- list()
  
  # Loop through all columns
  for (col in names(data)) {
    # Skip the target variable
    if (col != target_variable && is.factor(data[[col]])) {
      # Perform chi-squared test
      chi_test <- chisq.test(table(data[[target_variable]], data[[col]]))
      
      # Store results
      results[[col]] <- list(
        variable = col,
        p_value = chi_test$p.value,
        significant = ifelse(chi_test$p.value < 0.05, TRUE, FALSE)
      )
    }
  }
  
  # Convert results to a data frame
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  return(results_df)
}

# Perform chi-squared tests
chi_squared_results <- perform_chi_squared_tests(SAR_1, "Car_Cancellation")

# View the results
print(chi_squared_results)

SAR_3 <- SAR_1 %>% select(c(-from_lat_imp,-from_long_imp))  # since chi square

### Building models
# Load necessary libraries
library(caret)
library(e1071)  # For Naive Bayes
library(glmnet) # For Logistic Regression with Lasso
library(rpart)  # For Decision Tree
library(randomForest) # For Random Forest
library(class)  # For KNN
library(cluster) # For K-means
library(dplyr)

# Load your dataset (assuming SAR_3 is already loaded)
SAR_3 <- SAR_3 %>%
  mutate_if(is.character, as.factor)

# Convert target variable to numeric if necessary
SAR_3$Car_Cancellation <- as.numeric(as.character(SAR_3$Car_Cancellation))

# Split Data: 80% Train, 20% Test
set.seed(123)
trainIndex <- createDataPartition(SAR_3$Car_Cancellation, p = 0.8, list = FALSE)
train <- SAR_3[trainIndex, ]
sum(is.na(train))
test <- SAR_3[-trainIndex, ]
sum(is.na(test))
# -------------------------------------------------
# 1. NAIVE BAYES
# -------------------------------------------------
nb_model <- naiveBayes(Car_Cancellation ~ ., data = train)
nb_pred <- predict(nb_model, test)
nb_conf_matrix <- confusionMatrix(as.factor(nb_pred), as.factor(test$Car_Cancellation))
print(nb_conf_matrix)

# -------------------------------------------------
# 2. LOGISTIC REGRESSION with LASSO (L1 Regularization)
# -------------------------------------------------
x_train <- model.matrix(Car_Cancellation ~ ., data = train)[, -1]  # Convert factors to dummy vars
y_train <- train$Car_Cancellation
x_test <- model.matrix(Car_Cancellation ~ ., data = test)[, -1]
y_test <- test$Car_Cancellation

lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")  # Lasso Regression
lasso_pred <- predict(lasso_model, s = "lambda.min", newx = x_test, type = "class")
lasso_conf_matrix <- confusionMatrix(as.factor(lasso_pred), as.factor(y_test))
print(lasso_conf_matrix)
summary(lasso_pred)
# -------------------------------------------------
# 3. DECISION TREE
# -------------------------------------------------
dt_model <- rpart(Car_Cancellation ~ ., data = train, method = "class")
dt_pred <- predict(dt_model, test, type = "class")
dt_conf_matrix <- confusionMatrix(as.factor(dt_pred), as.factor(test$Car_Cancellation))
print(dt_conf_matrix)

# -------------------------------------------------
# 4. K-NEAREST NEIGHBORS (KNN)
# -------------------------------------------------
# Load required library
library(caret)
library(class)

# Ensure target variable is a factor
SAR_kn <- SAR_3
SAR_kn$Car_Cancellation <- as.factor(SAR_kn$Car_Cancellation)

# Check for missing values
if (any(is.na(SAR_kn))) {
  SAR_kn <- na.omit(SAR_kn)  # Remove rows with NA values
}

# Convert categorical variables to numeric (excluding the target variable)
num_features <- SAR_kn %>% select_if(is.numeric) %>% colnames()
cat_features <- SAR_kn %>% select_if(is.factor) %>% colnames()
cat_features <- setdiff(cat_features, "Car_Cancellation")  # Exclude target variable

# One-hot encoding for categorical variables
dummies <- dummyVars(" ~ .", data = SAR_kn[, cat_features], fullRank = TRUE)
cat_encoded <- predict(dummies, newdata = SAR_kn[, cat_features])
SAR_kn_numeric <- cbind(SAR_kn[, num_features], cat_encoded, Car_Cancellation = SAR_kn$Car_Cancellation)

# Split data
set.seed(123)
trainIndex <- createDataPartition(SAR_kn_numeric$Car_Cancellation, p = 0.8, list = FALSE)
train <- SAR_kn_numeric[trainIndex, ]
test <- SAR_kn_numeric[-trainIndex, ]

# KNN requires numeric input
train_x <- train[, -which(names(train) == "Car_Cancellation")]
test_x <- test[, -which(names(test) == "Car_Cancellation")]
train_y <- train$Car_Cancellation

# Ensure no missing or infinite values
train_x[is.na(train_x)] <- 0
test_x[is.na(test_x)] <- 0

# Normalize features (KNN is distance-based)
preProcessRange <- preProcess(train_x, method = c("center", "scale"))
train_x <- predict(preProcessRange, train_x)
test_x <- predict(preProcessRange, test_x)

# Apply KNN
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)

# Evaluate model performance
knn_conf_matrix <- confusionMatrix(knn_pred, test$Car_Cancellation)
print(knn_conf_matrix)

# -------------------------------------------------
# 5. RANDOM FOREST
# -------------------------------------------------
# Create a copy of the dataset for Random Forest processing
SAR_rf <- SAR_3

# Convert target variable to factor (for classification)
SAR_rf$Car_Cancellation <- as.factor(SAR_rf$Car_Cancellation)

# Identify high-cardinality categorical columns correctly
high_cardinality_cols <- names(SAR_rf)[sapply(SAR_rf, function(col) is.factor(col) && nlevels(col) > 53)]

# Print identified high-cardinality columns
print("High-cardinality categorical columns:")
print(high_cardinality_cols)

# Reduce levels for high-cardinality columns using `forcats::fct_lump()` (group rare categories)
library(forcats)
for (col in high_cardinality_cols) {
  SAR_rf[[col]] <- fct_lump(SAR_rf[[col]], prop = 0.05)  # Group categories <5% frequency
}

# Confirm that categorical columns are now reduced
sapply(SAR_rf[high_cardinality_cols], nlevels)



# Load necessary libraries
library(caret)
library(randomForest)
library(forcats)

# Create a copy of the dataset for Random Forest processing
SAR_rf <- SAR_3

# Convert target variable to factor (for classification)
SAR_rf$Car_Cancellation <- as.factor(SAR_rf$Car_Cancellation)

# Identify high-cardinality categorical columns correctly
high_cardinality_cols <- names(SAR_rf)[sapply(SAR_rf, function(col) is.factor(col) && nlevels(col) > 53)]

# Reduce levels for high-cardinality columns using `forcats::fct_lump()` (group rare categories)
for (col in high_cardinality_cols) {
  SAR_rf[[col]] <- fct_lump(SAR_rf[[col]], prop = 0.05)  # Group categories <5% frequency
}

# Split data: 80% Train, 20% Test
set.seed(123)
trainIndex <- createDataPartition(SAR_rf$Car_Cancellation, p = 0.8, list = FALSE)
train <- SAR_rf[trainIndex, ]
test <- SAR_rf[-trainIndex, ]

# Train the Random Forest Model
rf_model <- randomForest(Car_Cancellation ~ ., data = train, ntree = 100, importance = TRUE)

# Make Predictions
rf_pred <- predict(rf_model, test)

# Evaluate Model Performance
rf_conf_matrix <- confusionMatrix(rf_pred, test$Car_Cancellation)
print(rf_conf_matrix)

# Feature Importance
importance_values <- importance(rf_model)
print(importance_values)

# Plot Feature Importance
varImpPlot(rf_model)
