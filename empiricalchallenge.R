# Empirical Challenge Topic 4

# ------ Install Packages ------- #
library(boot)
library(dplyr)
library(tidyverse)
library(readr)
library(corrplot)
library(caret)
library(leaps)

# ------ Import Data -------- #
mat <- read_csv("studentMat.csv")
por <- read_csv("studentPor.csv")

# ------- Initial EDA ------- # 
sum(is.na(mat))
sum(is.na(por))
str(mat)
str(por)
names(mat)
names(por)


# ----------------------- Transformations --------------------------#

create_binary_columns <- function(df) {
  df <- df %>%
    mutate(
      Dalc_low = case_when(Dalc <= 2 ~ 1, TRUE ~ 0),
      Dalc_medium = case_when(Dalc > 2 & Dalc <= 4 ~ 1, TRUE ~ 0),
      Dalc_high = case_when(Dalc > 4 ~ 1, TRUE ~ 0),
      Walc_low = case_when(Walc <= 2 ~ 1, TRUE ~ 0),
      Walc_medium = case_when(Walc > 2 & Walc <= 4 ~ 1, TRUE ~ 0),
      Walc_high = case_when(Walc > 4 ~ 1, TRUE ~ 0)
    )
  return(df)
}

mat <- create_binary_columns(mat)
por <- create_binary_columns(por)


head(select(
  mat, Dalc, Dalc_low, Dalc_medium, Dalc_high,
  Walc, Walc_low, Walc_medium, Walc_high))
head(select(
  por, Dalc, Dalc_low, Dalc_medium, Dalc_high,
  Walc, Walc_low, Walc_medium, Walc_high))

# Change sex col to binary
mat <- mat %>% mutate(sex = ifelse(sex == "F", 1, 0))
por <- por %>% mutate(sex = ifelse(sex == "F", 1, 0))
# female is 1

# Change yes/no columns to binary
mat <- mat %>%
  mutate(
    across(c(
      "schoolsup", "famsup", "paid", "activities", "nursery", "higher", 
      "internet", "romantic"),
      ~ifelse(. == "yes", 1, 0)
      )
    )

por <- por %>%
  mutate(
    across(c(
      "schoolsup", "famsup", "paid", "activities", "nursery", "higher", 
      "internet", "romantic"),
      ~ifelse(. == "yes", 1, 0)
    )
  )

# Remove features that will be too difficult to onehot encode'
mat <- mat %>%
  select(
    -c("famsize", "Medu", "Fedu", "Mjob", "Fjob", "reason")
    )
por <- por %>%
  select(
    -c("famsize", "Medu", "Fedu", "Mjob", "Fjob", "reason")
  )

# Remove G1 & G2 - multicollinearity
mat <- mat %>% select(-c(G1, G2))
por <- por %>% select(-c(G1, G2))

# Onehot encoding
dummies <- dummyVars(" ~ .", data = mat, fullRank = TRUE)
mat <- predict(dummies, newdata = mat)
mat <- as.data.frame(mat)

dummies <- dummyVars(" ~ .", data = por, fullRank = TRUE)
por <- predict(dummies, newdata = por)
por <- as.data.frame(por)


# Create a combined dataset
mat$subject <- 0
por$subject <- 1
comb.data <- rbind(mat, por)
#drop subjects from individual sets(redundant column)
mat <- mat %>% select(-subject)
por <- por %>% select(-subject)


# ---------- Plots ----------- #
ggplot(mat, aes(x=age)) +
  geom_histogram(binwidth=1) +
  ggtitle("Age Distribution in Math Class")

ggplot(por, aes(x=age)) +
  geom_histogram(binwidth=1) +
  ggtitle("Age Distribution in Portuguese Class")

# Correlation Map
mat_cor <- cor(select_if(mat, is.numeric))
corrplot(mat_cor, method = "circle")
por_cor <- cor(select_if(por, is.numeric))
corrplot(por_cor, method = "circle")

# Study Time vs. Final Grade
ggplot(mat, aes(x=studytime, y=G3)) +
  geom_count(aes(size=..n..)) +
  scale_size_continuous(range = c(3, 30)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Study Time vs Final Grade in Math")
ggplot(por, aes(x=studytime, y=G3)) +
  geom_count(aes(size=..n..)) +
  scale_size_continuous(range = c(3, 30)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Study Time vs Final Grade in Portugese")


# --- Models for Individual Sets --- #
dim(mat)
dim(por)

regfit.mat <- regsubsets(G3~., data=mat, method="forward", nvmax=15)
regfit.por <- regsubsets(G3~., data=por, method="forward", nvmax=15)

plot(regfit.mat,scale="Cp")
summary(regfit.mat)$cp

plot(regfit.por,scale="Cp")
summary(regfit.por)$cp

summary_mat <- summary(regfit.mat)
which_mat <- summary_mat$which
best_num_features_mat <- which.min(summary(regfit.mat)$cp)
print(paste("The best number of features for the 'mat' dataset is: ",
            best_num_features_mat))

summary_por <- summary(regfit.por)
which_por <- summary_por$which
best_num_features_por <- which.min(summary(regfit.por)$cp)
print(paste("The best number of features for the 'por' dataset is: ",
            best_num_features_por))

# To see the predictors for the best model for mat and por
best_model_mat <- which_mat[best_num_features_mat,]
best_model_por <- which_por[best_num_features_por,]

best_predictors_mat <- names(best_model_mat)[best_model_mat]
best_predictors_por <- names(best_model_por)[best_model_por]
best_predictors_mat
best_predictors_por

# --- Cross-Validation for Mat Data --- #
# Fit the model once outside the loop
glm_fit_mat <- glm(G3 ~ sex + age + addressU + studytime + failures + schoolsup
                   + higher + internet + romantic + goout + absences, 
                   data = mat)
cv_glm_mat <- cv.glm(mat, glm_fit_mat, K = 5)
mean_cv_error_mat <- cv_glm_mat$delta[1]
print(paste("Mean CV Error for mat: ", mean_cv_error_mat))

# --- Cross-Validation for Por Data --- #
glm_fit_por <- glm(G3 ~ schoolMS + sex + age + guardianmother + studytime
                   + failures + schoolsup + higher + internet + romantic
                   + Dalc + health + absences + Dalc_high, 
                   data = por)
cv_glm_por <- cv.glm(por, glm_fit_por, K = 10)
mean_cv_error_por <- cv_glm_por$delta[1]
print(paste("Mean CV Error for por: ", mean_cv_error_por))


# --- Model for Combined Dataset --- #
regfit.comb <- regsubsets(G3~., data=comb.data, method="forward", nvmax=15)
plot(regfit.comb, scale="Cp")
summary_comb <- summary(regfit.comb)
which_comb <- summary_comb$which
best_num_features_comb <- which.min(summary(regfit.comb)$cp)
print(paste("The best number of features for the 'comb' dataset is: ",
            best_num_features_comb))

# To see the predictors for the best model for the combined dataset
best_model_comb <- which_comb[best_num_features_comb,]
best_predictors_comb <- names(best_model_comb)[best_model_comb]
best_predictors_comb

# --- Cross-Validation for Combined Data --- #
glm_fit_comb <- glm(G3 ~ schoolMS + addressU + studytime + failures + schoolsup
                    + higher + internet + romantic + goout + Dalc + health
                    + Dalc_low + Dalc_medium + subject, 
                    data = comb.data)
cv_glm_comb <- cv.glm(comb.data, glm_fit_comb, K = 5)
mean_cv_error_comb <- cv_glm_comb$delta[1]
print(paste("Mean CV Error for combined data: ", mean_cv_error_comb))



# --- Final Model Training --- #

set.seed(123)

sample_index_mat <- sample(seq_len(nrow(mat)), size = 0.8 * nrow(mat))
train_mat <- mat[sample_index_mat, ]
test_mat <- mat[-sample_index_mat, ]
glm_fit_mat <- glm(
  G3 ~ sex + age + addressU + studytime + failures + schoolsup +
    higher + internet + romantic + goout + absences,
  data = train_mat
)
summary(glm_fit_mat)
pred_mat <- predict(glm_fit_mat, newdata = test_mat)
mse_mat <- mean((test_mat$G3 - pred_mat)^2)
residuals_mat_test <- test_mat$G3 - pred_mat
plot(
  pred_mat, residuals_mat_test,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residual Plot for mat data"
)



sample_index_por <- sample(seq_len(nrow(por)), size = 0.8 * nrow(por))
train_por <- por[sample_index_por, ]
test_por <- por[-sample_index_por, ]
glm_fit_por <- glm(
  G3 ~ schoolMS + sex + age + guardianmother + studytime + failures +
    schoolsup + higher + internet + romantic + Dalc + health + absences + Dalc_high,
  data = train_por
)
summary(glm_fit_por)
pred_por <- predict(glm_fit_por, newdata = test_por)
mse_por <- mean((test_por$G3 - pred_por)^2)
residuals_por_test <- test_por$G3 - pred_por
plot(
  pred_por, residuals_por_test,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residual Plot for por data"
)



sample_index_comb <- sample(seq_len(nrow(comb.data)), size = 0.8 * nrow(comb.data))
train_comb <- comb.data[sample_index_comb, ]
test_comb <- comb.data[-sample_index_comb, ]
glm_fit_comb <- glm(
  G3 ~ schoolMS + addressU + studytime + failures + schoolsup +
    higher + internet + romantic + goout + Dalc + health +
    Dalc_low + Dalc_medium + subject,
  data = train_comb
)
summary(glm_fit_comb)
pred_comb <- predict(glm_fit_comb, newdata = test_comb)
mse_comb <- mean((test_comb$G3 - pred_comb)^2)
residuals_comb_test <- test_comb$G3 - pred_comb
plot(
  pred_comb, residuals_comb_test,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residual Plot for Combined Data"
)



# --- Comparison of Models --- # 

# Print Mean CV Errors
print(paste("Mean CV Error for mat: ", mean_cv_error_mat))
print(paste("Mean CV Error for por: ", mean_cv_error_por))
print(paste("Mean CV Error for combined data: ", mean_cv_error_comb))

# Print Mean Squared Errors
print(paste("Mean Squared Error for mat: ", mse_mat))
print(paste("Mean Squared Error for por: ", mse_por))
print(paste("Mean Squared Error for combined data: ", mse_comb))

summary(glm_fit_mat)
summary(glm_fit_por)
summary(glm_fit_comb)



