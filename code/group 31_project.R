### Title         : Research Skills: Programming with R
### Group Number  : 31
### Group Member  : 
###                 1. Adrian Konja             (SNR: 2037123 | ANR: u242159)
###                 2. Fernando Catalan Resquin (SNR: 2048042 | ANR: u270800)
###                 3. George Satyridis         (SNR: 2046944 | ANR: u836891)
###                 4. Konstantinos Soiledis    (SNR: 2037001 | ANR: u226393)
###                 5. Miftahul Ridwan          (SNR: 2040778 | ANR: u989303)
### Created       : 14 - Dec - 2019
### Last Modified : 09 - Jan - 2020

## Loading the Necessary Library -----------------------------------------------
require(dplyr) # Used primarily in data cleaning
require(tidyr) # Used primarily in data cleaning
require(ggplot2) # Used to create graphs, primarily in EDA
require(caret) # Used to implement Machine learning algorithms for prediction
require(mice) # Used for imputation
require(ranger) # Used for feature selection with Random forests

## Loading the Dataset ---------------------------------------------------------
wd <- getwd()
setwd(wd)

hawai <- read.csv("./listings.csv", na.strings = c("", "NA"),
                  stringsAsFactors = FALSE)

## Data Cleaning ---------------------------------------------------------------

### Define Necessary Function ---------------------------------------------- ###
reformat <- function(strings){
  strings <- gsub(strings, pattern = "_", replacement = ".") 
  strings <- gsub(strings, pattern = "[$]|[%]|NA|[*]", replacement = "")
  strings <- toupper(strings) 
}

### Tidying the Data ------------------------------------------------------- ###
hawai_col <- hawai %>%
  # Selec for specific column
  select(c(23, 26, 27, 29, 37, 45, 52, 53:58, 61, 66:68, 85, 87:93, 99, 102,
           106)) %>%
  
  # Applying predefined function
  select_all(funs(reformat)) %>%
  mutate_all(funs(reformat)) %>%
  
  # Change column type to integer
  mutate_at(c('REVIEW.SCORES.RATING','BATHROOMS', 'EXTRA.PEOPLE', 
              'HOST.RESPONSE.RATE', 'PRICE', 'REVIEWS.PER.MONTH', 
              'CALCULATED.HOST.LISTINGS.COUNT'), 
            as.integer) %>%
  
  # Change column type to numeric
  mutate_at(c('BEDS', 'BEDROOMS','GUESTS.INCLUDED', 'MINIMUM.NIGHTS',
              'REVIEW.SCORES.RATING', 'REVIEW.SCORES.ACCURACY',
              'ACCOMMODATES', 'REVIEW.SCORES.CLEANLINESS', 
              'REVIEW.SCORES.CHECKIN', 'REVIEW.SCORES.LOCATION', 
              'REVIEW.SCORES.COMMUNICATION', 'REVIEW.SCORES.VALUE'), 
            as.numeric) %>%
  
  # Factorize column
  mutate_at(c('FIRST.REVIEW', 'MARKET', 'PROPERTY.TYPE', 
              'HOST.RESPONSE.TIME', 'CANCELLATION.POLICY'), 
            as.factor) %>%
  
  # Filter for row which is not NA
  filter(!is.na(BATHROOMS), # 11
         !is.na(MARKET), #14
         !is.na(BEDS), # 44
         !is.na(PRICE), # 1029
         !is.na(HOST.SINCE), #276 NA 's !
         !is.na(HOST.RESPONSE.TIME), # 276 !
         !is.na(HOST.RESPONSE.RATE), # 1476 ! 
         !is.na(BEDROOMS), # 19
         !is.na(HOST.IS.SUPERHOST), # 276!
         !is.na(HOST.IDENTITY.VERIFIED),  #276 NA as well
         !is.na(HOST.IDENTITY.VERIFIED)) #276
  
### Missing Data Imputation ------------------------------------------------ ###
miss_method <- rep("", ncol(hawai_col))
names(miss_method) <- names(hawai_col)

miss_method["FIRST.REVIEW"] <- "pmm"
miss_method[c("REVIEW.SCORES.RATING", "REVIEW.SCORES.ACCURACY", 
              "REVIEW.SCORES.CLEANLINESS", "REVIEW.SCORES.CHECKIN", 
              "REVIEW.SCORES.COMMUNICATION", "REVIEW.SCORES.LOCATION", 
              "REVIEW.SCORES.VALUE", "REVIEWS.PER.MONTH")] <- "norm"

miss_pred <- quickpred(hawai_col, mincor = 0.25)

miceOut <- mice(data            = hawai_col,
                m               = 5,
                maxit           = 10,
                method          = miss_method,
                predictorMatrix = miss_pred,
                seed            = 42)

clean_hawai <- complete(miceOut)

clean_hawai <- clean_hawai %>%
  mutate(REVIEW.SCORES.RATING = ifelse(REVIEW.SCORES.RATING >= 100, 10,
                                       REVIEW.SCORES.RATING/10)) %>% 
  mutate_at(c('HOST.SINCE', 'FIRST.REVIEW'), as.Date, format = '%Y-%m-%d') %>%
  mutate(HOST.SINCE = as.numeric(format(HOST.SINCE,'%Y'))) %>%
  mutate(FIRST.REVIEW = as.numeric(format(FIRST.REVIEW, '%Y')),
         FIRST.REVIEW = 2019 - FIRST.REVIEW) %>% 
  mutate(YEARS.HOST = as.integer(2019 - HOST.SINCE)) %>%
  mutate_if(is.numeric, as.integer) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-HOST.SINCE) %>%
  droplevels() #Dropping unused factor levels

str(clean_hawai)

## Exploratory Data Analysis ---------------------------------------------------
'We are using market as our main geographical variable, and we are filtering 
some observations that contain sparse locations, and are not in Hawai'

clean_hawai_EDA <- clean_hawai %>% 
  filter(MARKET == "OAHU" | MARKET == "MAUI" | MARKET == "KAUAI" | 
           MARKET == "THE BIG ISLAND")

### --------------------   Section Property Features   --------------------- ###
  
### Fig 01: Nr. of Listings per Market ------------------------------------- ###

# A bar chart is appropriate for counting values
fig01 <- ggplot(data = clean_hawai_EDA, aes(x = MARKET)) +
  geom_bar() +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Nr. of Listings", 
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  labs (title = "Fig 01: Nr. of Listings per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 02: Nr. of Listings per Property Type ------------------------------ ###

fig02 <- ggplot(data = clean_hawai_EDA, aes(x = PROPERTY.TYPE)) +
  geom_bar() +
  scale_x_discrete(name = "Property Type") +
  scale_y_continuous(name = "Nr. of Listings")  +
  labs (title = "Fig 02: Nr. of Listings per Property Type") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

### Fig 03: Nr. of Listings per Main Property Type ------------------------- ###

# Aggregate the many less frequent property types into "Other"
hawai_property_grouped <- clean_hawai_EDA %>%
  within(PROPERTY.TYPE[(PROPERTY.TYPE != 'APARTMENT') & 
                         (PROPERTY.TYPE != 'CONDOMINIUM') &
                         (PROPERTY.TYPE != 'HOUSE')] <- 'OTHER')

fig03 <- ggplot(data = hawai_property_grouped, aes(x = PROPERTY.TYPE)) +
  geom_bar() +
  scale_x_discrete(name = "Property Type") +
  scale_y_continuous(name = "Nr. of Listings")  +
  labs (title = "Fig 03: Nr. of Listings per Main Property Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 04: Mix of Main Property Type per Market --------------------------- ###

# A stacked 100% bar chart is appropriate for showing the percentage split of a 
#variable within another

fig04 <- ggplot(data = hawai_property_grouped, aes(x = MARKET)) +
  geom_bar(aes(fill = PROPERTY.TYPE), position = "fill") +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "% of Listings", labels = scales::percent) +
  scale_fill_discrete(name = "Property Type")  +
  labs (title = "Fig 04: Mix of Main Property Types per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 05: Nr. of Listings per Room Type ---------------------------------- ###

fig05 <- ggplot(data = clean_hawai_EDA, aes(x = ROOM.TYPE)) +
  geom_bar() +
  scale_x_discrete(name = "Room Type") +
  scale_y_continuous(name = "Nr. of Listings", 
                     breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000, 
                                17500, 20000)) +
  labs (title = "Fig 05: Nr. of Listings per Room Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 06: Median Minimum Nights per Market ------------------------------- ###

# To decrease the influence of outliers in an unlimited scale, use median 
#instead of mean. A point chart is appropriate for showing specific calculated 
#measures

market_nights_medians <- clean_hawai_EDA %>% 
  group_by(MARKET) %>% 
  dplyr::summarise(median.nights = median(MINIMUM.NIGHTS))

fig06 <- ggplot(data = market_nights_medians, 
                aes(x = reorder(MARKET, -median.nights), y = median.nights)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Median Minimum Nights", limits = c(0, NA), 
                     breaks = c(0, 1, 2, 3))   +
  labs (title = "Fig 06: Median Minimum Nights per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 07: Median Minimum Nights per Main Property Type ------------------- ###

property_nights_medians <- hawai_property_grouped %>% 
  group_by(PROPERTY.TYPE) %>% 
  dplyr::summarise(median.nights = median(MINIMUM.NIGHTS))

fig07 <- ggplot(data = property_nights_medians, aes(x = reorder(PROPERTY.TYPE,
                                                       -median.nights), 
                                           y = median.nights)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Property Type") +
  scale_y_continuous(name = "Median Minimum Nights", 
                     limits = c(0, NA), breaks = c(0, 1, 2, 3))  +
  labs (title = "Fig 07: Median Minimum Nights per Main Property Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 08: Median Minimum Nights per Room Type ---------------------------- ###

room_nights_medians <- clean_hawai_EDA %>% 
  group_by(ROOM.TYPE) %>% 
  dplyr::summarise(median.nights = median(MINIMUM.NIGHTS))

fig08 <- ggplot(data = room_nights_medians, 
                aes(x = reorder(ROOM.TYPE, -median.nights), 
                    y = median.nights)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Room Type") +
  scale_y_continuous(name = "Median Minimum Nights", limits = c(0, NA), 
                     breaks = c(0, 1, 2, 3))   +
  labs (title = "Fig 08: Median Minimum Nights per Room Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 09: Median Nr. of Guests Accommodated per Market ------------------- ###

market_guests_medians <- hawai_property_grouped %>% 
  group_by(MARKET) %>% 
  dplyr::summarise(median.guests = median(ACCOMMODATES))

fig09 <- ggplot(data = market_guests_medians, 
                aes(x = reorder(MARKET, -median.guests),
                    y = median.guests)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Median Nr. of Guests Accommodated", 
                     limits = c(0, NA)) +
  labs (title = "Fig 09: Median Nr. of Guests Accommodated per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 10: Median Nr. of Guests Accommodated per Main Property Type ------- ###

property_guests_medians <- hawai_property_grouped %>% 
  group_by(PROPERTY.TYPE) %>% 
  dplyr::summarise(median.guests = median(ACCOMMODATES))

fig10 <- ggplot(data = property_guests_medians, 
                aes(x = reorder(PROPERTY.TYPE, -median.guests), 
                                           y = median.guests)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Property Type") +
  scale_y_continuous(name = "Median Nr. of Guests Accommodated", 
                     limits = c(0, NA)) +
  labs (title = "Fig 10: Median Nr. of Guests Accommodated per 
        Main Property Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### ----------------------   Section Host Features   ----------------------- ###

### Fig 11: Mean Response Rate per Market ---------------------------------- ###

RR_market_means <- clean_hawai_EDA %>% 
  group_by(MARKET) %>% 
  dplyr::summarise(mean.RR = mean(HOST.RESPONSE.RATE))

fig11 <- ggplot(data = RR_market_means, aes(x = MARKET, y = mean.RR)) +
  geom_bar(stat ="identity") +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Mean Response Rate (%)", 
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  labs (title = "Fig 11: Mean Response Rate per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 12: Mean Review Scores Rating per Market --------------------------- ###

review_market_means <- clean_hawai_EDA %>% 
  group_by(MARKET) %>% 
  dplyr::summarise(mean.review = mean(REVIEW.SCORES.RATING))

fig12 <- ggplot(data = review_market_means, aes(x = MARKET, y = mean.review)) +
  geom_point(stat ="identity", size = 5) +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Mean Review Scores Rating", 
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                     limits = c(0, 10))  +
  labs (title = "Fig 12: Mean Review Scores Rating per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 13: Mix of Cancellation Policy Types per Market -------------------- ###

fig13 <- ggplot(data = clean_hawai_EDA, aes(x = MARKET)) +
  geom_bar(aes(fill = CANCELLATION.POLICY), position = "fill") +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "% of Listings", labels = scales::percent) +
  scale_fill_discrete(name = "Cancellation Policy Type")  +
  labs (title = "Fig 13: Mix of Cancellation Policy Types per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 14: Mix of Response Times per Superhost Status --------------------- ###

fig14 <- ggplot(data = clean_hawai_EDA, aes(x = HOST.IS.SUPERHOST)) +
  geom_bar(aes(fill = HOST.RESPONSE.TIME), position = "fill") +
  scale_x_discrete(name = "Superhost Status", limits = c("T", "F"),
                   labels = c("Yes", "No")) +
  scale_y_continuous(name = "% of Listings", labels = scales::percent) +
  scale_fill_discrete(name = "Response Time")  +
  labs (title = "Fig 14: Mix of Response Times per Superthost Status") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 15: Mean Response Rate per Superhost Status ------------------------ ###

RR_superhost_means <- clean_hawai_EDA %>% 
  group_by(HOST.IS.SUPERHOST) %>% 
  dplyr::summarise(mean.RR = mean(HOST.RESPONSE.RATE))

fig15 <- ggplot(data = RR_superhost_means, 
                aes(x = HOST.IS.SUPERHOST, y = mean.RR)) +
  geom_bar(stat ="identity") +
  scale_x_discrete(name = "Superhost Status", limits = c("T", "F"),
                   labels = c("Yes", "No")) +
  scale_y_continuous(name = "Mean Response Rate (%)", 
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  labs (title = "Fig 15: Mean Response Rate per Superhost Status") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 16: Mean Review Scores Rating per Superhost Status ----------------- ###

review_superhost_means <- clean_hawai_EDA %>% 
  group_by(HOST.IS.SUPERHOST) %>% 
  dplyr::summarise(mean.review = mean(REVIEW.SCORES.RATING))

fig16 <- ggplot(data = review_superhost_means, aes(x = HOST.IS.SUPERHOST, 
                                          y = mean.review)) +
  geom_point(stat ="identity", size = 5) +
  scale_x_discrete(name = "Superhost Status", limits = c("T", "F"),
                   labels = c("Yes", "No")) +
  scale_y_continuous(name = "Mean Review Scores Rating",
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                     limits = c(0, 10))  +
  labs (title = "Fig 16: Mean Review Scores Rating per Superhost Status") + 
  theme(plot.title = element_text(hjust = 0.5))

### ---------------------   Section Price Analysis   ----------------------- ###

### Fig 17: Median Price per Market----------------------------------------- ###

market_price_medians <- hawai_property_grouped %>% 
  group_by(MARKET) %>% 
  dplyr::summarise(median.price = median(PRICE))

fig17 <- ggplot(data = market_price_medians, 
                aes(x = reorder(MARKET, -median.price), y = median.price)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Market") +
  scale_y_continuous(name = "Median Price", limits = c(0, NA),
                     breaks = c(0, 50, 100, 150, 200, 250, 300))  +
  labs (title = "Fig 17: Median Price per Market") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 18: Median Price per Property Type---------------------------------- ###

property_price_medians <- clean_hawai_EDA %>% 
  group_by(PROPERTY.TYPE) %>% 
  dplyr::summarise(median.price = median(PRICE))

fig18 <- ggplot(data = property_price_medians, 
                aes(x = reorder(PROPERTY.TYPE, -median.price), 
                    y = median.price)) +
  geom_point(stat="identity", size = 3) +
  scale_x_discrete(name = "Property Type") +
  scale_y_continuous(name = "Median Price", limits = c(0, NA))   +
  labs (title = "Fig 18: Median Price per Property Type") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

### Fig 19: Median Price per Main Property Type----------------------------- ###

main_property_price_medians <- hawai_property_grouped %>% 
  group_by(PROPERTY.TYPE) %>% 
  dplyr::summarise(median.price = median(PRICE))

fig19 <- ggplot(data = main_property_price_medians, 
       aes(x = reorder(PROPERTY.TYPE, -median.price), y = median.price)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Main Property Type") +
  scale_y_continuous(name = "Median Price", limits = c(0, NA),
                     breaks = c(0, 50, 100, 150, 200, 250))   +
  labs (title = "Fig 19: Median Price per Main Property Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 20: Median Price per Room Type-------------------------------------- ###

room_price_medians <- clean_hawai_EDA %>% 
  group_by(ROOM.TYPE) %>% 
  dplyr::summarise(median.price = median(PRICE))

fig20 <- ggplot(data = room_price_medians, 
                aes(x = reorder(ROOM.TYPE, -median.price), y = median.price)) +
  geom_point(stat="identity", size = 5) +
  scale_x_discrete(name = "Room Type") +
  scale_y_continuous(name = "Median Price", limits = c(0, NA), 
                     breaks = c(0, 50, 100, 150, 200, 250))   +
  labs (title = "Fig 20: Median Price per Room Type") + 
  theme(plot.title = element_text(hjust = 0.5))

### Fig 21: Median Price per Nr. of Guest Accommodated --------------------- ###

guest_price_medians <- clean_hawai_EDA %>% 
  group_by(ACCOMMODATES) %>% 
  dplyr::summarise(median.price = median(PRICE))

# Since both axis carry continuous variables, 
#we can fit a locally fitted regression line
fig21 <- ggplot(data = guest_price_medians, 
                aes(x = ACCOMMODATES, y = median.price)) +
  geom_point(stat="identity", size = 4) +
  geom_smooth() +
  scale_x_continuous(name = "Nr. of Guests Accommodated", 
                     breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                10, 11, 12, 13, 14, 15, 16)) +
  scale_y_continuous(name = "Median Price", limits = c(0, NA), 
                     breaks = c(0, 100, 200, 300, 400, 500, 600, 700))  +
  labs (title = "Fig 21: Median Price per Nr. of Guests Accommodated") + 
  theme(plot.title = element_text(hjust = 0.5))

### ----------------------------   End of EDA   ---------------------------- ###

## Feature selection------------------------------------------------------------

'The function below performs fearute selection using Random forests algorithm
It returns a matrix with accuracy scores and corresponding number of variables
It also returns an object to be used in later classification algorithms called
final_vars'

feature_selection <- function(dataset, rf_class, eval_type){
  
  set.seed(123)
  
  hawai_final_rf <- dataset
  
  #Splitting data
  ind =  createDataPartition(get(rf_class, dataset), p = 0.8, list = FALSE)
  train_X = hawai_final_rf[ind,]
  train_Y = get(rf_class, hawai_final_rf)[ind]
  test_X = hawai_final_rf[-ind,]
  test_Y = get(rf_class, hawai_final_rf)[-ind]
  
  #Delete class column from X
  train_X <- select(train_X, -rf_class)
  test_X <- select(test_X, -rf_class)
  
  #Preparing matrix for model evaluation
  variable_names <- colnames(hawai_final_rf)
  results_rf <- matrix(nrow = length(train_X) - 1 , ncol = 3)
  
  #Conditional statement for eval_type.
  if (eval_type == 1){
    colnames(results_rf) <- c("Accuracy", "No Variables", "Variables")
  } else if (eval_type == 2){
    colnames(results_rf) <- c("Kappa", "No Variables", "Variables")
  } else {
    return(print("Wrong eval_type"))
  }
  
  #Creating a copy for the while loop
  train_X2 <- train_X
  i <- 1
  
  #Train, Predict, Evaluating model
  while (i < length(variable_names) - 1) {#Excluding models with only 1 variable
    
    rf <- ranger(train_Y ~ ., data = train_X2, 
                 importance = "impurity", mtry = 1)
    
    predicted_Y <- predict(object = rf, data = test_X)
    evaluate <- confusionMatrix(test_Y, predicted_Y$predictions)
    
    results_rf[i, 1] = as.numeric(round(evaluate$overall[eval_type], 4))
    results_rf[i, 2] = as.numeric(rf$num.independent.variables)
    results_rf[i, 3] = paste(names(sort(rf$variable.importance, 
                                        decreasing = T)), collapse = " , ")
    
    togo <- which.min(rf$variable.importance)
    train_X2 <- select(train_X2,-togo)
    
    i = i + 1
  }
  
  #Getting the most important variables according to evaluation metric
  place <- which.max(results_rf[, 1])
  cols <- strsplit(results_rf[place, 3], split = " , ")
  final_vars <- as.formula(paste(paste0(rf_class, " ~ "), 
                                 paste0(cols$Variables, collapse = " + ")))
  
  print(results_rf[, 1:2])
  return(final_vars)
}

## Logistic regression----------------------------------------------------------

#Creating Test and train set
set.seed(23)
trn_index = createDataPartition(y = clean_hawai$HOST.IS.SUPERHOST, p = 0.80, 
                                list = FALSE)
trn_data = clean_hawai[trn_index, ]
tst_data = clean_hawai[-trn_index, ]

### Logistic Regression Baseline Model ------------------------------------- ###

# Train baseline model
set.seed(42)

'We set warnings off for the moment, since warnings are produced from very few
observations in specific levels of some factors. That, combined with CV, forces
LogReg to produce NA coefficients. These coefficients however are of low impor-
tance and rarely contribute, so instead of discarding the whole factor, we dec-
ided to keep it as it is. After all, multicollinearity does not affect predict-
ion resutls.'

options(warn = -1) 

host_lgr_baseline <-  train(HOST.IS.SUPERHOST ~ .,
                            method = "glm",
                            family = binomial(link = "logit" ),
                            data = trn_data,
                            trControl = trainControl(method = 'cv', 
                                                     number = 10))

#Checking coefficients
summary(host_lgr_baseline)

# Testing the performance on the test set
predicted_outcome_bl <- predict(host_lgr_baseline, tst_data)

# Confusion Matrix 
host_confM_bl <- confusionMatrix(table(predicted_outcome_bl, 
                                       tst_data$HOST.IS.SUPERHOST))
options(warn = 1)

# Recall: 0.8159393 
host_confM_bl$byClass["Recall"]

# Precision: 0.7750541 
host_confM_bl$byClass["Precision"]

# Accuracy: 0.7393655 
host_confM_bl$overall["Accuracy"]

####------------------Plotting variable importance---------------------------###

#Calculating the importance of the variables 
var_importance_b <- varImp(host_lgr_baseline)

#Plot of the importance of each variable sorted by importance
fig22 <- plot(var_importance_b, ylab = 'Variables', asp = 2, type = 's', 
              main = "Fig 22. Variable importance - Baseline model")

### Logistic Regression Embedded Model ------------------------------------- ###
log_reg_vars <- feature_selection(clean_hawai, "HOST.IS.SUPERHOST", 2)

# Train the Embedded Model
set.seed(42)

options(warn = -1) #Same as above

host_lgr_embedded <- train(log_reg_vars,
                           method = "glm",
                           family = binomial(link = "logit" ),
                           data = trn_data, 
                           trControl = trainControl(method = 'cv', number = 10))

#Checking coefficients
summary(host_lgr_embedded)

# Testing the performance on the test set
predicted_outcome_emb <- predict(host_lgr_embedded, tst_data)

# Confusion Matrix 
host_confM_emb <- confusionMatrix(table(predicted_outcome_emb, 
                                        tst_data$HOST.IS.SUPERHOST))
options(warn = 1)

# Recall: 0.8136622
host_confM_emb$byClass["Recall"]

# Precision: 0.7715005
host_confM_emb$byClass["Precision"]

# Accuracy: 0.7353702 
host_confM_emb$overall["Accuracy"]

####------------------Plotting variable importance---------------------------###

#Calculating the importance of the variables 
var_importance_e <- varImp(host_lgr_embedded)

#Plot of the importance of each variable sorted by importance
fig23 <- plot(var_importance_e, ylab = 'Variables', asp = 2, type = 's',
              main = "Fig 23. Variable importance - Embedded method model")

## k - Nearest Neighbours-------------------------------------------------------

'Since we are using classification algorithm, we discretize price variable into
4 clusters, namely LOW, MEDIUM.LOW, MEDIUM.HIGH, HIGH'

fig_24 <- hist(clean_hawai$PRICE,
               main ="Fig 24. Histogram of Price",
               xlab = "Hawaiian Airbnb Prices")

# Creating dataset for k-NN
clean_hawai_knn <- clean_hawai %>%
  mutate(PRICE = cut_number(PRICE, n = 4),
         PRICE = ifelse(PRICE == "[0,125]", "LOW",
                        ifelse(PRICE == "(125,195]", "MEDIUM.LOW",
                               ifelse(PRICE == "(195,305]", "MEDIUM.HIGH",
                                      ifelse(PRICE == "(305,999]", "HIGH", "")
                               )))) %>%
  mutate(PRICE = factor(PRICE, levels = c("LOW", "MEDIUM.LOW", "MEDIUM.HIGH", 
                                          "HIGH")))

# Price Category after discretization
fig_25 <- clean_hawai_knn %>% 
  group_by(PRICE) %>% 
  summarise(NUMBER = n()) %>% 
  ggplot(aes(x = PRICE, y = NUMBER)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Price Category",
                   limits = c("LOW", "MEDIUM.LOW", "MEDIUM.HIGH", "HIGH"),
                   labels = c("LOW", "MEDIUM - LOW", "MEDIUM - HIGH", "HIGH")) +
  scale_y_continuous(name = "Number of Listing") +
  ggtitle("Fig 25. Number of Listing per Price Category") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Split train-test data -------------------------------------------------- ###
set.seed(42)
clean_hawai_frac = sample_frac(clean_hawai_knn, size = 0.8, replace = FALSE)
knn_idx <- createDataPartition(clean_hawai_frac$PRICE, p = 0.8, list = FALSE)
trn_knn <- clean_hawai_frac[knn_idx, ]
tst_knn <- clean_hawai_frac[-knn_idx, ]


### k-NN for Baseline Model ------------------------------------------------ ###
set.seed(42)

# Train Baseline Model
baseline_knn <- train(PRICE ~ .,
                      method = "knn", 
                      data = trn_knn,
                      trControl = trainControl(method = "cv", number = 5),
                      metric = "Accuracy",
                      tuneGrid = expand.grid(k = 4:8))

# Plotting Variable Importance
fig_26 <- ggplot(varImp(baseline_knn)) +
  geom_histogram(stat = "identity") +
  ggtitle(
    "Fig 26. Variable Importance in Predicting Price using baseline k-NN model"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Testing the performance on the test set
baseline_knn_pred <- predict(baseline_knn, tst_knn)
baseline_knn_cm <- confusionMatrix(baseline_knn_pred, tst_knn$PRICE)

# Accuracy = 0.5994
baseline_knn_cm$overall["Accuracy"]

### k-NN Embedded Model ---------------------------------------------------- ###
knn_vars <- feature_selection(clean_hawai_frac, "PRICE", 1)

# Train Embedded Model
set.seed(42)
embedded_knn <- train(knn_vars,
                      method = "knn", 
                      data = trn_knn,
                      trControl = trainControl(method = "cv", number = 5),
                      metric = "Accuracy",
                      tuneGrid = expand.grid(k = 4:8))

# Plotting Variable Importance
fig_27 <- ggplot(varImp(embedded_knn)) +
  geom_histogram(stat = "identity") +
  ggtitle(
    "Fig 27. Variable Importance in Predicting Price using embedded k-NN model"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Testing the performance on the test set
embedded_knn_pred <- predict(embedded_knn, tst_knn)
embedded_knn_cm <- confusionMatrix(embedded_knn_pred, tst_knn$PRICE)

# Accuracy = 0.613
embedded_knn_cm$overall["Accuracy"]
