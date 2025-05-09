########################################################################
### Conjoint Analysis Exercise
### Using GLM
### to model possible multiple selection of product config(s)
########################################################################

# load required packages
library(ggplot2)
library(dplyr)

# load data
d <- read.csv("netflix_customer_survey.csv")

summary(d)
str(d)

############################
### Data Preprocessing & Exploration
############################

# notice that ExtraContent, ads, NumberAccounts needs to be factors 
# as it represents different possible levels
d$ExtraContent<- as.factor(d$ExtraContent)
d$ads<- as.factor(d$ads)
d$NumberAccounts<- as.factor(d$NumberAccounts)

# check again the stats
summary(d)

# Use xtabs() to determine the number of times different levels in some features was selected.
# This could indicate which levels are more popularly selected on a Netflix subscription 
xtabs(selected ~ ExtraContent, data = d)
xtabs(selected ~ ads, data = d)
xtabs(selected ~ NumberAccounts, data = d)
# HBO is the most popularly chosen 'ExtraContent'
# No Ads is the most popularly chosen 'ads'
# 6 accounts is the most popularly chosen 'NumberAccounts'

# Experimental design checking - to validate conjoint design (# options respondents exposed to, # selection allowed)
respondent_summary <- d %>%
  group_by(customerid) %>%
  summarise(
    options_shown = n(),
    options_selected = sum(selected == 1),
    prop_selected = round(options_selected / options_shown, 2)
  )

summary(respondent_summary)
# Observation:
# The number of alternatives shown varied: from 5 to 12 options per respondent
# The number of selections also varied: some respondents selected only 1 option, others up to 6
# We need to model each alternative independently; using glm() to predict probability of a given product config being selected

############################
### Conjoint Modelling
############################

# Set default/baseline level per attribute
d$ExtraContent <- relevel(factor(d$ExtraContent), ref = "less content") #assume that Netflix today has barely any "extra content" 
d$ads <- relevel(factor(d$ads), ref = "none") #assume that Netflix today has no ads 
d$NumberAccounts <- relevel(factor(d$NumberAccounts), ref = "1") #the most basic Netflix allow only for 1 account/device per stream 

# Fit logistic regression model
model <- glm(selected ~ NumberAccounts + ExtraContent + ads + price, 
             data = d, 
             family = binomial())

# View results
summary(model)
# Levels that may make up the most optimal package are NumberAccount6, 
# extra HBO content, and 1 ad per show, with the lower the price, the better (which makes sense)

# Save model + training data for Shiny
saveRDS(model, "conjoint_model.rds")
saveRDS(d, "training_data.rds")

# Predict selections
d$pred_prob <- predict(model, type = "response")
d$predicted_class <- ifelse(d$pred_prob > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(d$predicted_class == d$selected)
accuracy

# Confusion matrix
table(Actual = d$selected, Predicted = d$predicted_class)

# Check utilities (part-worths) per level
coefs <- coef(model)
coefs

# Visualize the utilities
visualize_utilities <- function(model, alpha = 0.05, include_price = TRUE) {
  # Extract coefficient table
  coef_table <- summary(model)$coefficients
  coef_df <- as.data.frame(coef_table)
  coef_df$Level <- rownames(coef_df)
  
  # Remove intercept
  coef_df <- coef_df[coef_df$Level != "(Intercept)", ]
  
  # Optionally exclude price
  if (!include_price) {
    coef_df <- coef_df[!grepl("price", coef_df$Level, ignore.case = TRUE), ]
  }
  
  # Add significance flag
  coef_df$Significance <- ifelse(coef_df$`Pr(>|z|)` < alpha, "Significant", "Not Significant")
  
  # Plot
  ggplot(coef_df, aes(x = reorder(Level, Estimate), y = Estimate, fill = Significance)) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_fill_manual(values = c("Significant" = "#1b9e77", "Not Significant" = "gray80")) +
    labs(
      title = "Utility per Level (with Significance)",
      x = "Feature Level",
      y = "Coefficient (Utility)",
      fill = "Statistical Significance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
visualize_utilities(model)

# Compute the WTP for each different level, had the level been added to the package (assuming everything else stays equal)
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
  NumberAccounts = factor(c("1", "6", "2"), levels = levels(d$NumberAccounts)),
  ExtraContent = factor(c("Disney", "HBO", "Soccer"), levels = levels(d$ExtraContent)),
  ads = factor(c("none", "one_per_show", "one_per_day"), levels = levels(d$ads)),
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
    geom_line(linewidth = 1.2) +
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

# Your scenarios data: 1 row = 1 profile, 7 simulated price points, +-10% interval each step
results <- simulate_price_sensitivity(model, scenarios, interval = 0.1, n_points = 7)
plot_price_sensitivity(
  results_df = results,
  label_var = "SoP",
  x_breaks = seq(2, 20, 1),
  y_limits = c(0, 0.6),
  decimal_digits = 2
)
