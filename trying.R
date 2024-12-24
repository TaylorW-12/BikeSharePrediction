# load libraries
library(tidyverse) # tidyverse

library(GGally) # for pairs plots with ggplot

library(caret) # our primary modeling package

library(broom)

library(mvrsquared)

library(ggridges)

library(dplyr)

library(ggplot2)

library(car)
install.packages("lmtest") 
install.packages("sandwich") 
library(lmtest) 
library(sandwich)

#### Load Data
bike_data=read.csv("C:/Users/LokiA/OneDrive/Desktop/Machine Learning I/OPAN-6602/data-raw/Bike_data.csv")
View(bike_data)

### Set seed for reproducibility
set.seed(410)

### Create a train/test split
bike_data_partition=
  createDataPartition(
    1:nrow(bike_data),
    times=1,
    p=.3
  )

bike_data_training=bike_data[-bike_data_partition[[1]], ] #creates training set
bike_data_test=bike_data[bike_data_partition[[1]], ] #creates test set

validation_bike_data=
  createDataPartition(
    1:nrow(bike_data_training),
    times=1,
    p=0.3
  )
validation_set=bike_data_training[validation_bike_data[[1]],] #creates validation set
bike_train=bike_data_training[-validation_bike_data[[1]],] #creates training set

### Data Exploration 
numeric_summaries_all=   #summary statistics for all data
  bike_data |>
  select(
    where(is.numeric),
    -yr
    
  )|>
  pivot_longer(everything()) |> 
  summarize(
    mean=mean(value,na.rm=TRUE),
    median=median(value,na.rm=TRUE),
    sd=sd(value,na.rm=TRUE),
    num_missing=sum(is.na(value)),
    var=var(value,na.rm=TRUE),
    .by=name
  )
numeric_summaries_all

numeric_summaries=   #summary statistics for training data
  bike_train |>
  select(
    where(is.numeric),
    -yr
    
  )|>
  pivot_longer(everything()) |> 
  summarize(
    mean=mean(value,na.rm=TRUE),
    median=median(value,na.rm=TRUE),
    sd=sd(value,na.rm=TRUE),
    num_missing=sum(is.na(value)),
    var=var(value,na.rm=TRUE),
    .by=name
  )
numeric_summaries


str(bike_train) #list dataframe

##Transformations----

bike_train$season<- as.factor(bike_train$season)
bike_train$weathersit<- as.factor(bike_train$weathersit)
bike_train$hr_c<- as.factor(bike_train$hr)
# Add centered variables
bike_train <- bike_train %>%
  mutate(
    temp_centered = temp - mean(temp, na.rm = TRUE),
    hum_centered = hum - mean(hum, na.rm = TRUE),
    windspeed_centered = windspeed - mean(windspeed, na.rm = TRUE),
    hr_centered = hr - mean(hr, na.rm = TRUE)
    
  )
##Creating Regression Model----

f12 <- lm(I(log(cnt))~ . +  
            I(workingday*hr) +
            I(temp_centered*hum_centered)+
            I(temp_centered^2)+
            I(hum_centered^2),
          data = bike_train %>% 
            select(
              -atemp,
              -casual,
              -temp,
              -mnth,
              -yr,
              -dteday,
              -windspeed,
              -hum,
              -holiday,
              -hr_centered,
              -weekday,
              -windspeed_centered,
              -registered,
              -instant
            )
)
summary(f12)

f_stepwise <- step( # start with a full model
  object = f12,
  direction = "both"
)
summary(f_stepwise)

f12 <- lm(cnt~ . +  
            I(workingday*hr) +
            I(temp_centered*hum_centered)+
            I(temp_centered^2)+
            I(hum_centered^2),
          data = bike_train %>% 
            select(
              -atemp,
              -casual,
              -temp,
              -mnth,
              -yr,
              -dteday,
              -windspeed,
              -hum,
              -holiday,
              -hr_centered,
              -weekday,
              -windspeed_centered,
              -registered,
              -instant
            )
)
summary(f12)


# Prepare the data


library(car)
vif(f12, type="predictor")

plot(f12)

f_stepwise <- step( # start with a full model
  object = f12,
  direction = "both"
)
summary(f_stepwise)

vif(f12, type="predictor")

plot(f12)












### Extracting Coefficient Model 1
f12_coef <- tidy(f12)

f12_coef %>%
  ggplot() +
  geom_bar(
    aes(x = term, y = estimate),
    stat = "identity",
    fill = "skyblue"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_errorbar(
    aes(x = term, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    width = 0.4,
    color = "orange",
    alpha = 0.9
  )


### Step 6: Model Evaluation for Model 12 
# get fold indices
# Create k-fold indices
# Create k-fold indices
# Create k-fold indices
k_fold_indices <- createFolds(
  1:nrow(bike_train),
  k = 11
)

# Declare a function to fit the model
my_model_function <- function(dat) {
  
  bike_data_test <- dat # Make sure bike_data_test is the same as the input data
  
  # Ensure test data uses the same factor levels
  dat$season<- as.factor(dat$season)
  dat$weathersit<- as.factor(dat$weathersit)
  dat$hr_c<- as.factor(dat$hr)
  
  
  # Add centered variables
 dat <- dat %>% 
   mutate( 
     temp_centered = temp - mean(temp, na.rm = TRUE), 
     hum_centered = hum - mean(hum, na.rm = TRUE), 
     windspeed_centered = windspeed - mean(windspeed, na.rm = TRUE), 
     hr_centered = hr - mean(hr, na.rm = TRUE), 
     #cnt = ifelse(cnt == 0, 1, cnt),
     # Replace 0 with 1 to avoid log(0) 
     #log_cnt = log(cnt) 
     )

  # Fit the model
  f12 <- 
    f12 <- lm(cnt~ . +  
                I(workingday*hr) +
                I(temp_centered*hum_centered)+
                I(temp_centered^2)+
                I(hum_centered^2),
              data = dat %>% 
                select(
                  -atemp,
                  -casual,
                  -temp,
                  -mnth,
                  -yr,
                  -dteday,
                  -windspeed,
                  -hum,
                  -holiday,
                  -hr_centered,
                  -weekday,
                  -windspeed_centered,
                  -registered,
                  -instant
                )
    )
  
  # Return the model and updated test data
  list(f12 = f12, bike_data_test = bike_data_test)
}

# Declare a function to get out-of-sample evaluations
my_evaluation_function <- function(model, new_data) {
  
  # Remove any NA values for simplicity
  new_data <- new_data %>%
    na.omit()
  
  preds <- predict(model, new_data)
  
  # Ensure y is a numeric vector
  y <- as.numeric(new_data$cnt)
  
  oos_r2 <- calc_rsquared(
    y = y,
    yhat = preds
  )
  
  oos_rmse <- ((new_data$cnt - preds) ^ 2) %>%
    mean() %>%
    sqrt()
  
  tibble(   # Creating a dataframe
    r2 = oos_r2,
    rmse = oos_rmse
  )
}

# Loop over folds and apply
k_fold_results <- k_fold_indices %>%
  map(function(fold) {
    train <- bike_train[-fold, ]
    test <- bike_train[fold, ]
    model_result <- my_model_function(train)
    f12 <- model_result$f12
    bike_data_test <- model_result$bike_data_test
    
    coefs <- tidy(f12)
    
    evals <- my_evaluation_function(model = f12, new_data = test)
    
    list(
      evals = evals,
      coefs = coefs
    )
  }) 

# Summarize R^2 and RMSE
cv_evals <- k_fold_results %>%
  map(function(x) x$evals) %>%
  bind_rows()

cv_coefs <- k_fold_results %>%
  map(function(x) x$coefs)

for (j in seq_along(cv_coefs)) {
  cv_coefs[[j]]$fold = j
}

cv_coefs <- bind_rows(cv_coefs)

# Plot our results
cv_coefs %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term, fill = term)) +
  geom_density_ridges() + 
  theme_ridges() + 
  theme(legend.position = "none")

cv_coefs %>%
  filter(term %>% str_detect("^I")) %>%
  ggplot(aes(x = estimate, y = term, fill = term)) +
  geom_density_ridges() + 
  theme_ridges() + 
  theme(legend.position = "none")

### Step 7: Predictions and Conclusions ----
model_result <- my_model_function(bike_train)
f12 <- model_result$f12
bike_data_test <- model_result$bike_data_test

test_predictions <- predict(f12, newdata = bike_data_test)

# Ensure y is a numeric vector
y <- as.numeric(bike_data_test$log_cnt)

# We have na values so need to address before calc_rsquared()
test_results <- tibble(
  r2 = calc_rsquared(
    y = y[! is.na(test_predictions)],
    yhat = test_predictions[! is.na(test_predictions)]
  ),
  rmse = ((bike_data_test$log_cnt - test_predictions) ^ 2) %>%
    mean(na.rm = TRUE) %>%
    sqrt()
)

test_results

tibble(
  test_predictions,
  actual = bike_data_test$log_cnt
) %>%
  ggplot(aes(x = actual, y = test_predictions)) + 
  geom_point()



