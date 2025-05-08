########################################################################
### Conjoint Analysis Exercise
### Using GLM
### to model possible multiple selection of product config(s)
########################################################################

# load required packages
# install.packages("mlogit")
library(mlogit) # for multinomial logit models & heating data set
# install.packages("dfidx")
library(dfidx) # to index a data set
# install.packages("lmtest")
library(lmtest) # for likelihood ratio test
# install.packages("expm")
library(expm)  # for matrix exponentiation
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

# load data
nf <- read.csv("netflix_customer_survey.csv")

summary(nf)
str(nf)

############################
### Data Preprocessing
############################

# notice that ExtraContent, ads, NumberAccounts needs to be factors 
# as it represents different possible levels

nf$ExtraContent<- as.factor(nf$ExtraContent)
nf$ads<- as.factor(nf$ads)
nf$NumberAccounts<- as.factor(nf$NumberAccounts)

# check again the stats
summary(nf)

# Use xtabs() to determine the number of times different levels in some features was selected.
# This could indicate which levels are more popularly selected on a Netflix subscription 
xtabs(selected ~ ExtraContent, data = nf)
xtabs(selected ~ ads, data = nf)
xtabs(selected ~ NumberAccounts, data = nf)

# Set default/baseline level per attribute
nf$ExtraContent <- relevel(factor(nf$ExtraContent), ref = "less content") #assume that Netflix today has barely any extra content 
nf$ads <- relevel(factor(nf$ads), ref = "none") #assume that Netflix today has no ads 
nf$NumberAccounts <- relevel(factor(nf$NumberAccounts), ref = "1") #the most basic Netflix allow only for 1 account/device per stream 

############################
### Conjoint Modelling
############################

# Fit logistic regression model
model <- glm(selected ~ NumberAccounts + ExtraContent + ads + price, 
             data = nf, 
             family = binomial())

# View results
summary(model)

# Save model + training data for Shiny
saveRDS(model, "conjoint_model.rds")
saveRDS(nf, "training_data.rds")

# Levels that may make up the most optimal package are NumberAccount6, 
# extra HBO content, and 1 ad per show, with the lower the price, the better (which makes sense)

# Predict selections
nf$pred_prob <- predict(model, type = "response")
nf$predicted_class <- ifelse(nf$pred_prob > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(nf$predicted_class == nf$selected)
accuracy

# Confusion matrix
table(Actual = nf$selected, Predicted = nf$predicted_class)

# Check utilities (part-worths) per level
coefs <- coef(model)
coefs

# Visualize the utilities
visualize_utilities <- function(model) {
  coefs <- coef(model)
  coefs <- coefs[!names(coefs) %in% c("(Intercept)", "price")]  # remove intercept and price
  df <- data.frame(Level = names(coefs), Utility = coefs)
  
  library(ggplot2)
  ggplot(df, aes(x = reorder(Level, Utility), y = Utility)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Utility Rank per Level", x = "Level", y = "Utility") +
    theme_minimal()
}
visualize_utilities(model)

# Compute the WTP for different levels (assuming all else equal)
wtp_all <- coefs/(-coef(model)["price"])
wtp_all

############################
### Conjoint Simulation
############################

# Function to simulate and produce SoP, sum utility, recommended WTP 
predict.glm.sop <- function(model, newdata, price_var = "price", scale_price = 1) {
  # Build model matrix consistent with training
  X <- model.matrix(delete.response(terms(model)), newdata)
  
  # Compute total utility (linear predictor)
  utility <- X %*% coef(model)
  
  # Softmax to get share of preference
  sop <- as.vector(exp(utility) / sum(exp(utility)))
  
  # Get price coefficient name
  price_coef <- coef(model)[price_var]
  if (is.na(price_coef)) stop("Price variable not found in model coefficients.")
  
  # WTP = utility / (-price_coef / scale)
  wtp <- as.vector(utility) / (-price_coef / scale_price)
  
  # Combine into result
  result <- cbind(
    data.frame(share = sop, utility = as.vector(utility), WTP = round(wtp, 2)),
    newdata
  )
  
  return(result)
}

# Example product configs to simulate
scenarios <- data.frame(
  NumberAccounts = factor(c("1", "6", "2"), levels = levels(nf$NumberAccounts)),
  ExtraContent = factor(c("Disney", "HBO", "Soccer"), levels = levels(nf$ExtraContent)),
  ads = factor(c("none", "one_per_show", "one_per_day"), levels = levels(nf$ads)),
  price = c(8, 12, 10)
)

# Run the simulation
predict.glm.sop(model, scenarios)

# Simulate how SoP of each Profile change given different price points 
simulate_price_sensitivity <- function(model, scenarios,
                                       interval = 0.2,
                                       n_points = 5,
                                       price_var = "price") {
  
  steps <- seq(-floor(n_points / 2), floor(n_points / 2))
  all_results <- list()
  
  for (i in 1:nrow(scenarios)) {
    base_profiles <- scenarios
    price_base <- scenarios[i, price_var, drop = TRUE]
    price_range <- round(price_base * (1 + steps * interval), 2)
    
    for (p in price_range) {
      temp_profiles <- base_profiles
      temp_profiles[i, price_var] <- p
      
      temp_profiles$utility <- predict(model, newdata = temp_profiles, type = "link")
      temp_profiles$SoP <- exp(temp_profiles$utility) / sum(exp(temp_profiles$utility))
      temp_profiles$wtp <- temp_profiles$utility / (-coef(model)[[price_var]])
      
      temp_profiles$target_profile <- paste0("Profile_", i)
      temp_profiles$simulated_price <- temp_profiles[[price_var]]
      temp_profiles$varied <- seq_len(nrow(temp_profiles)) == i
      
      all_results[[length(all_results) + 1]] <- temp_profiles
    }
  }
  
  final_df <- do.call(rbind, all_results)
  final_df <- final_df %>% filter(varied)  # keep only the varied profile’s SoP over price
  return(final_df)
}

# Function to plot price sensitivity
plot_price_sensitivity <- function(results_df,
                                   label_var = "SoP",
                                   x_breaks = NULL,
                                   y_limits = NULL,
                                   decimal_digits = 2) {
  
  # Round labels to desired precision
  results_df <- results_df %>%
    mutate(label_val = round(.data[[label_var]], decimal_digits))
  
  # Identify base price rows (middle price for each profile)
  base_points <- results_df %>%
    group_by(target_profile) %>%
    arrange(simulated_price) %>%
    slice(ceiling(n() / 2)) %>%
    ungroup()
  
  # Build plot
  p <- ggplot(results_df, aes(x = simulated_price, y = !!sym(label_var), color = target_profile)) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5) +
    geom_text(aes(label = label_val), vjust = -0.7, size = 3) +
    geom_point(data = base_points, aes(x = simulated_price, y = !!sym(label_var)),
               color = "black", size = 2.5, shape = 21, stroke = 1.2, fill = "lightgrey") +
    labs(
      title = paste("Price Sensitivity Curve (", label_var, ")", sep = ""),
      x = "Price ($)",
      y = label_var,
      color = "Profile"
    ) +
    theme_minimal()
  
  if (!is.null(x_breaks)) {
    p <- p + scale_x_continuous(breaks = x_breaks)
  }
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# Your scenarios data: 1 row = 1 profile
results <- simulate_price_sensitivity(model, scenarios, interval = 0.1, n_points = 11)
plot_price_sensitivity(
  results_df = results,
  label_var = "SoP",
  x_breaks = seq(2, 20, 1),
  y_limits = c(0, 0.6),
  decimal_digits = 2
)

### Possible improvement:
# add modeling of most optimal config mix
# had there been data about customer segmentation (e.g. region where they come from), 
# segment the simulation results and see possible differences in Netflix preference

find_optimal_tested_profile <- function(model, training_data, 
                                        price_var = "price", 
                                        scale_price = 1,
                                        optimize_for = c("utility", "SoP", "WTP")) {
  optimize_for <- match.arg(optimize_for)
  
  # Extract all unique tested profiles
  profiles <- training_data %>%
    distinct(NumberAccounts, ExtraContent, ads, !!sym(price_var)) %>%
    mutate(across(everything(), ~ if (is.character(.)) factor(.) else .))  # coerce factors
  
  # Predict utility
  profiles$utility <- predict(model, newdata = profiles, type = "link")
  
  # Share of preference across all tested profiles
  profiles$SoP <- exp(profiles$utility) / sum(exp(profiles$utility))
  
  # Calculate WTP
  price_coef <- coef(model)[[price_var]]
  profiles$WTP <- profiles$utility / (-price_coef / scale_price)
  
  # Sort by the chosen optimization target
  profiles_sorted <- profiles %>%
    arrange(desc(.data[[optimize_for]]))
  
  # Extract best profile
  best <- profiles_sorted %>% slice(1)
  
  return(list(best_profile = best, all_profiles = profiles_sorted))
}

result <- find_optimal_tested_profile(
  model = model,
  training_data = nf,
  optimize_for = "SoP"  # or "SoP" or "WTP"
)

result$best_profile    # <-- best configuration
result$all_profiles    # <-- full table to inspect or plot
