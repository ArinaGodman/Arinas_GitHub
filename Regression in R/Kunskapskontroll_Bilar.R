
# Importing libraries -----------------------------------------------------
library(readr)
library(dplyr)
library(glmnet)
library(Matrix)
library(leaps)
library(fastDummies)
library(car)


# Data preprocessing and variable selection -------------------------------

excel_file <- "C:/your_path/Blocket_Data_Group1.csv"

blocket_cars <- read_delim(excel_file, delim = ";", col_types = cols(), locale = locale(encoding = "UTF-8"), na = "NULL")

blocket_cars

names(blocket_cars)

dim(blocket_cars)

na_count_per_column <- colSums(is.na(blocket_cars))

print(na_count_per_column)

blocket_cars_clean <- na.omit(blocket_cars)

na_count_per_column <- colSums(is.na(blocket_cars_clean))

print(na_count_per_column)

dim(blocket_cars_clean)

summary(blocket_cars)

# Categorical data --------------------------------------------------------

categorical <- c('Stad', 'Lan', 'Bransle', 'Vaxellada', 'Marke', 'Biltyp', 'Drivning', 'Farg', 'Modell')

result <- lapply(categorical, function(cat) {
  cat_table <- table(blocket_cars[[cat]])
  cat_table <- sort(cat_table, decreasing = TRUE)  
  return(cat_table)
})

names(result) <- categorical
result

blocket_cars_clean <- select(blocket_cars_clean, -c(Stad, Motorstorlek, Datum_i_trafik, Lan, Modell))


# Numerical data ----------------------------------------------------------

numerical_cols <- sapply(blocket_cars_clean, is.numeric)

numerical_data <- blocket_cars_clean[, numerical_cols]

pairs(numerical_data)


# Outliers ----------------------------------------------------------------

z_scores <- scale(numerical_data)

threshold <- 3

outlier_rows <- apply(abs(z_scores) > threshold, 1, any)

cleaned_data <- blocket_cars_clean[!outlier_rows, ]

numerical_data <- cleaned_data[, numerical_cols]

pairs(numerical_data)


# Dummies  and correlation-----------------------------------------------------------------

data_for_reg <- fastDummies::dummy_cols(cleaned_data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

correlation_matrix <- cor(data_for_reg)

high_correlation_threshold <- 0.7
high_correlation_pairs <- which(abs(correlation_matrix) > high_correlation_threshold & correlation_matrix != 1, arr.ind = TRUE)

if (length(high_correlation_pairs) > 0) {
  print("Highly correlated variables:")
  print(high_correlation_pairs)
} else {
  print("No highly correlated variables found.")
}

data_for_reg <- data_for_reg[, !colnames(data_for_reg) %in% "Miltal"]


# Modeling ---------------------------------------------------------------

# Train-test spit ---------------------------------------------------------

set.seed(123)

shuffled_indices <- sample(nrow(data_for_reg))

train_proportion <- 0.7

train_size <- floor(train_proportion * nrow(data_for_reg))
test_size <- nrow(data_for_reg) - train_size

train_data <- data_for_reg[shuffled_indices[1:train_size], ]
test_data <- data_for_reg[shuffled_indices[(train_size + 1):nrow(data_for_reg)], ]

dim(train_data)
dim(test_data)


# Model #1 ----------------------------------------------------------------

lm_1 <- lm(Pris ~ ., data = train_data)
summary(lm_1)

par(mfrow = c(2, 2))
plot(lm_1)


# Model #2 ----------------------------------------------------------------

train_data <- train_data[,!colnames(train_data) %in% c("Marke_Volvo","Marke_Dacia", "Marke_Chrysler")]
lm_2 <- lm(Pris ~ ., data = train_data)
summary(lm_2)

par(mfrow = c(2, 2))
plot(lm_2)


# VIF ---------------------------------------------------------------------

vif_values <- vif(lm_2)

threshold <- 5

high_vif_variables <- names(vif_values[vif_values > threshold])

print(high_vif_variables)

correlation_matrix <- cor(train_data[, !colnames(train_data) %in% c("Pris")])

high_correlation_threshold <- 0.5

high_correlation_pairs <- which(abs(correlation_matrix) > high_correlation_threshold & correlation_matrix != 1, arr.ind = TRUE)

if (length(high_correlation_pairs) > 0) {
  print("Highly correlated variables:")
  print(high_correlation_pairs)
} else {
  print("No highly correlated variables found.")
}


# Getting rid of VIF ------------------------------------------------------

train_data_no_VIF <- train_data[, !colnames(train_data) %in% c("Biltyp_Kombi")]


# Model #3 ----------------------------------------------------------------

lm_3 <- lm(Pris ~ ., data = train_data_no_VIF)
summary(lm_3)

vif_values <- vif(lm_3)

high_vif_variables <- names(vif_values[vif_values > threshold])

print(high_vif_variables)

par(mfrow = c(2, 2))
plot(lm_3)


# High Leverage -----------------------------------------------------------
observations <- c(101, 261)

high_leverage_data <- blocket_cars[observations, ]

high_leverage_data

train_data_whl <- train_data_no_VIF[-observations, ]


# Model #4 ----------------------------------------------------------------

lm_4 <-lm(Pris ~ . -Farg_Orange - Marke_Opel, data = train_data_whl)
summary(lm_4)

par(mfrow = c(2, 2))
plot(lm_4)


# Model #5 Forward selection ----------------------------------------------

lm_5 <- step(lm_4, direction = "forward", trace = 0)

summary(lm_5)

par(mfrow = c(2, 2))
plot(lm_5)

# Model #6 Backward Elimination -------------------------------------------

lm_6 <- step(lm_4, direction = "backward", trace = 0)

summary(lm_6)

par(mfrow = c(2, 2))
plot(lm_6)


# RMSE for chosen model ---------------------------------------------------

predictions_test <- predict(lm_6, test_data)

residuals_test <- test_data$Pris - predictions_test

squared_residuals_test <- residuals_test^2

mean_squared_residuals_test <- mean(squared_residuals_test)

rmse_test <- sqrt(mean_squared_residuals_test)

print(paste("RMSE on test data:", rmse_test))

rmse_test/mean(test_data$Pris)

# Prediction my own car's price -------------------------------------------

my_car <- data.frame(
  Hastkrafter = 90,
  Bransle_Diesel = 0,
  Bransle_El = 0,
  Bransle_Miljobransle_Hyb = 0,
  Vaxellada_Manuell = 1,
  Biltyp_Coupe = 0,
  Biltyp_Halvkombi = 0,
  Biltyp_Sedan = 0,
  Biltyp_SUV = 1,
  Modellar = 2017,
  Drivning_Tvahjulsdriven = 1,
  Farg_Brun = 1,
  Farg_Gra = 0,
  Farg_Gron = 0,
  Farg_Rod = 0,
  Farg_Silver = 0,
  Farg_Svart = 0,
  Farg_Orange = 0,
  Farg_Vit = 0,
  Marke_Audi = 0,
  Marke_BMW = 0,
  Marke_Citroen = 0,
  Marke_Cupra = 0,
  Marke_Fiat = 0,
  Marke_Ford = 0,
  Marke_Honda = 0,
  Marke_Opel = 0,
  Marke_Hyundai = 0,
  Marke_Jaguar = 0,
  Marke_Jeep = 0,
  Marke_Kia = 0,
  Marke_Land_Rover = 0,
  Marke_Lexus = 0,
  Marke_Mazda = 0,
  Marke_Mercedes_Benz = 0,
  Marke_MINI = 0,
  Marke_Mitsubishi = 0,
  Marke_Nissan = 0,
  Marke_Peugeot = 0,
  Marke_Porsche = 0,
  Marke_Renault = 1,
  Marke_Seat = 0,
  Marke_Skoda = 0,
  Marke_Subaru = 0,
  Marke_Suzuki = 0,
  Marke_Tesla = 0,
  Marke_Toyota = 0,
  Marke_Volkswagen = 0
)

prediction <- predict(lm_6, newdata = my_car)

cat("Predicted price:", prediction)


confidence_intervals <- predict(lm_5, newdata = my_car, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_5, newdata = my_car, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals
