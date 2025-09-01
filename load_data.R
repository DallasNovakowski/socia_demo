# load data


set.seed(123)

# Define date range
start_date <- Sys.Date() - 100
end_date <- Sys.Date()
dates <- seq.Date(from = start_date, to = end_date, by = "day")

### 1. Transactions Data with Systematic Differences ###
transactions_revised <- data.frame(
  created_at = sample(dates, 300, replace = TRUE)
)

# Add store, day-of-week
transactions_revised$store <- as.factor(sample(paste("Store", 1:3), 300, replace = TRUE))
transactions_revised$weekday <- weekdays(transactions_revised$created_at)

# Add transaction_value with store-based and day-of-week effects
transactions_revised$transaction_value <- with(transactions_revised, 
                                               round(runif(300, 50, 100) + 
                                                       ifelse(store == "Store 3", 100, 0) + 
                                                       ifelse(weekday %in% c("Saturday", "Sunday"), 50, 0), 2)
)

# Add num_items_sold with weekend boost
transactions_revised$num_items_sold <- with(transactions_revised,
                                            sample(1:10, 300, replace = TRUE) + 
                                              ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0)
)

# Average sale value
transactions_revised <- transactions_revised %>%
  mutate(av_sale_value = transaction_value / num_items_sold)





### 2. Survey Data with Systematic Differences ###
survey_data <- data.frame(
  created_at = sample(dates, 600, replace = TRUE)
)

# Add other variables
survey_data$store <- sample(paste("Store", 1:3), 600, replace = TRUE)
survey_data$online_vs_instore <- sample(c("Online", "In-Store"), 600, replace = TRUE)

# Satisfaction: higher in Store 1 and in-store
survey_data$satisfaction_score <- with(survey_data,
                                       pmin(7, pmax(1,
                                                    round(rnorm(600, 
                                                                mean = ifelse(store == "Store 1", 6, ifelse(store == "Store 2", 5, 4.5)) +
                                                                  ifelse(online_vs_instore == "In-Store", 0.5, -0.5),
                                                                sd = 1
                                                    ))
                                       ))
)

# Would Recommend: More likely if satisfaction is high
survey_data$would_recommend <- ifelse(survey_data$satisfaction_score >= 5, "Yes", "No")

# Other scores: increase with satisfaction
survey_data$ease_of_purchase <- pmin(5, pmax(1, round(rnorm(600, mean = survey_data$satisfaction_score / 2, sd = 1))))
survey_data$staff_friendliness <- pmin(5, pmax(1, round(rnorm(600, mean = survey_data$satisfaction_score / 2, sd = 1))))
survey_data$product_quality <- pmin(5, pmax(1, round(rnorm(600, mean = survey_data$satisfaction_score / 2, sd = 1))))
survey_data$value_for_money <- pmin(5, pmax(1, round(rnorm(600, mean = survey_data$satisfaction_score / 2, sd = 1))))

# Remaining variables
survey_data$visit_frequency <- sample(c("First time", "Occasionally", "Regularly"), 600, replace = TRUE)
survey_data$purchase_amount <- round(runif(600, min = 5, max = 200) * ifelse(survey_data$store == "Store 3", 1.2, 1), 2)
survey_data$customer_age <- sample(18:75, 600, replace = TRUE)
survey_data$customer_segment <- sample(c("Budget Shopper", "Mid-range", "Premium Shopper"), 600, replace = TRUE)
survey_data$feedback_comments <- sample(
  c("Great service!", "Could be better", "Too expensive", "Loved the experience!", "Won’t return"),
  600, replace = TRUE
)

survey_data <- survey_data %>%
  mutate(
    store = as.factor(store),
    would_recommend = as.factor(would_recommend),
    visit_frequency = as.factor(visit_frequency),
    customer_segment = as.factor(customer_segment),
    online_vs_instore = as.factor(online_vs_instore)
  )






merged <- merge(x = transactions_revised, 
                y = survey_data, by = c("store", "created_at"), all = TRUE)

merged <- merged %>% filter(
  !is.na(ease_of_purchase) & !is.na(customer_segment) & !is.na(transaction_value) 
)