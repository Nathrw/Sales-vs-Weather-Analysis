library(DBI)
library(RSQLite)
library(tidyverse)
library(tidytext)
library(lubridate)

# ---------------------------------------- Loading SQL Database ----------------------------------------

con <- dbConnect(SQLite(), "project1_raw_data.db")

# ---------------------------------------- SQL Queries -------------------------------------------------

# Finding top 3 products with the highest total sales (based on units sold) from train table
top_3_summary <- dbGetQuery(con, "
    SELECT item_nbr, SUM(units) AS total_units_sold
    FROM train
    GROUP BY item_nbr
    ORDER BY total_units_sold DESC
    LIMIT 3;")

print("Top 3 Products Identified")
print(top_3_summary) # Top 3 - 45, 5, 9

top3_ids <- paste(top_3_summary$item_nbr, collapse = ", ")

# Using Key table to join sales data (train) with the correct weather station
q_joined <- paste0("
    SELECT *
    FROM train AS t
    JOIN key AS k USING (store_nbr)
    JOIN weather AS w USING (date, station_nbr)
    WHERE t.item_nbr IN (", top3_ids, ");")

sw <- dbGetQuery(con, q_joined)

print(head(sw[, c("date", "item_nbr", "units", "tavg")]))

nbr_9sw <- top_3_summary$item_nbr[top_3_summary$item_nbr == 9] # Viewing one item

# Using paste0 to build the query to return daily sales and average temperature for Item 9
item_9_query <- paste0("
    SELECT t.date, t.units, w.tavg
    FROM train AS t
    JOIN key AS k ON t.store_nbr = k.store_nbr
    JOIN weather AS w ON t.date = w.date AND k.station_nbr = w.station_nbr
    WHERE t.item_nbr = ", nbr_9sw, ";")

item_9_view <- dbGetQuery(con, item_9_query)

print(paste("Daily Sales and Tavg for Item", nbr_9sw))
print(head(item_9_view))

# ---------------------------------------- R Programming ----------------------------------------

# Loading Joined Dataset 
sw_df <- sw

# Previewing Dataset
colSums(is.na(sw_df)) 
colnames(sw_df) 
str(sw_df)
names(sw_df)[duplicated(names(sw_df))]
view(sw_df)

# ---------------------------------------- Data Cleaning ----------------------------------------

# Dropping null values
sw_df <- sw_df %>% mutate(date = as.Date(date),
                          snowfall = if_else(is.na(snowfall), 0, snowfall))

# Due to tavg being a derived variable from the tmax and tmin column, imputing missing tavg values.
# As the missing tavg values are being imputed by using data from the same row, it's safe to do before splitting
sw_df <- sw_df %>%
  mutate(tavg = if_else(is.na(tavg), (tmax + tmin)/2, tavg))

# Correlation matrix for all temperature columns, checking for data redundance
temp_cols <- sw_df %>%
  select(tmax, tmin, tavg, heat, cool, depart)

# Compute correlation matrix
temp_corr <- cor(temp_cols, use = "complete.obs")
print(round(temp_corr, 2))

# Tavg calculates the day's temperature in one number, it's the primary feature
# Dropping all other temperature columns due to high correlation and subsequent redundancy
sw_df <- sw_df %>%
  select(-tmax, -tmin, -depart, -heat, -cool)

# Converting sunrise/sunset into numeric 'Minutes from Midnight'
sw_df <- sw_df %>% mutate(item_nbr = factor(item_nbr), 
                          year = year(date), 
                          month = month(date), 
                          sunrise_mfm = as.numeric(hms(sunrise)) / 60, 
                          sunset_mfm = as.numeric(hms(sunset)) / 60)

# Checking for and handling special characters 
sp_char <- sw_df %>%
  select(-codesum) %>%
  mutate(across(everything(), as.character)) %>%
  summarise(across(everything(), ~sum(. %in% c("T", "M", " T"), na.rm = TRUE))) %>%
  # Reshaping to view easier
  pivot_longer(everything(), names_to = "Col name", values_to = "Count") %>%
  filter(Count > 0)  %>%
  arrange(desc(Count))

print(sp_char) # None Present, no need for imputing

# Checking for correlation between wind columns 
wind_cols <- cor(sw_df[, c("avgspeed", "resultspeed", "resultdir")],
                 use = "complete.obs")
print(round(wind_cols, 2)) # very correlated, dropping redundant columns
sw_df <- sw_df %>% select(-resultspeed, -resultdir)

# Calculating fundamental statistics of the units column
unit_stat <- sw_df %>%
  summarise(
    mean = mean(units, na.rm = TRUE),
    sdev = sd(units, na.rm = TRUE),
    iqr = IQR(units, na.rm = TRUE),
    max = max(units, na.rm = TRUE), 
    sd_distance = (max(units, na.rm = TRUE) - mean(units, na.rm = TRUE)) / sd(units, na.rm = TRUE))
print(unit_stat)

# Viewing top percentiles - to see where outliers are
quantile(sw_df$units, probs = c(0.5, 0.9, 0.95, 0.99), na.rm = TRUE)

# Viewing Top units
top_outliers <- sw_df %>%
  mutate(z_score = (units - mean(units, na.rm = TRUE)) /
           sd(units, na.rm = TRUE)) %>%
  filter(z_score > 3) %>%
  arrange(desc(z_score))

print(top_outliers) # This shows two unit stats 5568 and 3369
# Next highest units is 503 - Data needs filtering
sw_df <- sw_df %>%
  filter(units < 600)

# Data is heavily zero inflated - Counting Zeros
zero_freq <- mean(sw_df$units == 0)
print(zero_freq)

# ---------------------------------------- Feature Engineering ----------------------------------------

# Separating and counting weather events by code
weather_codes <- c("FC", "TS", "GR", "RA", "DZ", "SN", "SG", "GS", "PL", "IC", 
                   "FG\\+", "FG", "BR", "UP", "HZ", "FU", "VA", "DU", "DS", "PO", 
                   "SA", "SS", "PY", "SQ", "DR", "SH", "FZ", "MI", "PR", "BC", 
                   "BL", "VC", "\\+", "-")

code_srch <- paste(weather_codes, collapse = "|")
wcode_extract <- str_extract_all(sw_df$codesum, regex(code_srch, ignore_case = TRUE))

codes_table <- unlist(wcode_extract) %>% table() %>% sort(decreasing = TRUE)
head(codes_table, 3)

# Calculate % of days each code appears
code_counts <- unlist(wcode_extract) %>% table()
code_pct <- round(prop.table(code_counts) * 100, 2)
print(sort(code_pct, decreasing = TRUE))

# Frequency of weather codes (codesum) and found that "BR" (Mist), "RA" (Rain), and "TS" (Thunderstorm) were the top three 
# Creating binary flags
# One-Hot-Encoding common weather events, limiting overcrowding
sw_df <- sw_df %>% 
  mutate(
    is_misty = if_else(str_detect(codesum, "BR"), 1, 0),
    is_raining = if_else(str_detect(codesum, "RA"), 1, 0),
    thunderstorm = if_else(str_detect(codesum, "TS"), 1, 0)
  ) %>% 
  select(-codesum) 

# Checking to see correlation with preciptotal (all are wet weather events)
wet_check <- sw_df %>%
  select(preciptotal, is_raining, is_misty, thunderstorm) %>%
  cor(use = "complete.obs")

print(round(wet_check, 2)) 

library(corrplot)
# Creating correlation plot of variables with similar descriptors
# Chance of multicollinearity
corr_vars <- sw_df %>% 
  select(stnpressure, sealevel, tavg, dewpoint, wetbulb)

# Calculate correlation
M <- cor(corr_vars, use = "complete.obs")

# Plot it
corrplot(M, method = "number", type = "upper", 
         title = "Multicollinearity Check",
         mar=c(0,0,1,0))


# To combat zero inflation, I'm aggregating the sales by sales per week
weather_var <- c( "tavg", "snowfall", "preciptotal",
                  "stnpressure", "avgspeed", "sunrise_mfm", 
                  "sunset_mfm", "thunderstorm")

sw_clean <- sw_df %>%
  mutate(date = as.Date(date)) %>%
  select(-sealevel, -dewpoint, -wetbulb, -is_misty, -is_raining) %>%
  mutate(
    week = week(date),
    month = month(date),
    year = year(date)
  ) %>%
  group_by(year, month, week, item_nbr) %>%
  summarise(
    weekly_units = sum(units, na.rm = TRUE),
    # Average weather conditions for that week
    across(all_of(weather_var), mean, na.rm = TRUE),
    .groups = "drop"
  )

view(sw_clean)
# Verify the transformation
str(sw_clean)
summary(sw_clean$weekly_units)
colSums(is.na(sw_clean))

# ---------------------------------------- Data Modelling ----------------------------------------

# Train, Test and Validation Splits - Loading Split library
library(splitTools)

set.seed(123)

split <- partition(
  seq_len(nrow(sw_clean)),
  p = c(train = 0.6, val = 0.2, test = 0.2))

str(split)

train_set <- sw_clean[split$train, ]
val_set <- sw_clean[split$val, ]
test_set <- sw_clean[split$test, ]

# Imputing missing data after split to avoid data leakage
medians <- train_set %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE)))

imp_df <- function(df, medians) {
  df %>%
    mutate(across(names(medians), ~ replace_na(., medians[[cur_column()]])))
}

train_set <- imp_df(train_set, medians)
val_set <- imp_df(val_set, medians)
test_set <- imp_df(test_set, medians)

rise_mean <- mean(train_set$sunrise_mfm, na.rm = TRUE)
set_mean  <- mean(train_set$sunset_mfm, na.rm = TRUE)

sun_imp <- function(df, sun_r, sun_s) {
  df %>%
    mutate(
      sunrise_mfm = replace_na(sunrise_mfm, sun_r),
      sunset_mfm  = replace_na(sunset_mfm, sun_s)
    )
}

train_set <- sun_imp(train_set, rise_mean, set_mean)
val_set   <- sun_imp(val_set, rise_mean, set_mean)
test_set  <- sun_imp(test_set, rise_mean, set_mean)

weather_pred <- as.formula(
  paste("weekly_units ~", paste(weather_var, collapse = " + "))
)

class(weather_pred) # Verifying 'weather_pred' is a formula

# Seperating each model by Item.

train_45 <- train_set %>% filter(item_nbr == 45)
val_45   <- val_set %>% filter(item_nbr == 45)

train_5  <- train_set %>% filter(item_nbr == 5)
val_5    <- val_set %>% filter(item_nbr == 5)

train_9  <- train_set %>% filter(item_nbr == 9)
val_9    <- val_set %>% filter(item_nbr == 9)

# Installing Decision Tree Libraries
library(rpart)
library(rpart.plot)

# Item 45
lm_45 <- lm(weather_pred, data = train_45)
pred_lm_45 <- predict(lm_45, val_45)
tree_uf_45 <- rpart(weather_pred, data = train_45,
                    method = "anova",
                    control = rpart.control(cp = 0.01, minsplit = 50, minbucket = 20))
# Min Split 50 as Item 45 is sensitive to weather, captures more detailed patterns

# Viewing CP of Item 45 Decision Tree. Applying Pruning to avoid overfitting and improve interpretability
printcp(tree_uf_45)
tree_45 <- prune(tree_uf_45, cp = 0.0034)
plotcp(tree_45)

tree_pred_45 <- predict(tree_45, val_45)

# Item 5
lm_5 <- lm(weather_pred, data = train_5)
pred_lm_5 <- predict(lm_5, val_5)
tree_uf_5 <- rpart(weather_pred, data = train_5,
                   method = "anova",
                   control = rpart.control(cp = 0.01, minsplit = 100))

# Viewing CP of Item 5 Decision Tree 
printcp(tree_uf_5)

# Constraints lowered to 20 because the previous 100 exceeded the total n=90, preventing any tree construction
tree_5 <- rpart(weather_pred, data = train_5,
                method = "anova",
                control = rpart.control(cp = 0.005, minsplit = 40, minbucket = 20))
tree_pred_5 <- predict(tree_5, val_5)

# Item 9
lm_9 <- lm(weather_pred, data = train_9)
pred_lm_9 <- predict(lm_9, val_9)
tree_uf_9 <- rpart(weather_pred, data = train_9,
                   method = "anova",
                   control = rpart.control(cp = 0.01, minsplit = 200))
printcp(tree_uf_9)

# Lowered minsplit to 20 to allow the model to actually partition the n=93 observations into meaningful nodes
tree_9 <- rpart(weather_pred, data = train_9,
                method = "anova",
                control = rpart.control(cp = 0.005, minsplit = 20))

tree_pred_9 <- predict(tree_9, val_9)

# ---------------------------------------- Model Visualisation ----------------------------------------

# Visualising OLS Coefficients 
library(broom)
library(jtools)

ols_results <- bind_rows(
  tidy(lm_45, conf.int = TRUE) %>% mutate(Item = 45),
  tidy(lm_5,  conf.int = TRUE) %>% mutate(Item = 5),
  tidy(lm_9,  conf.int = TRUE) %>% mutate(Item = 9)
) %>%
  filter(term != "(Intercept)")  # Drop the intercept

# Checking
head(ols_results)

# Plotting OLS 
ggplot(ols_results, aes(x = estimate, y = term, color = factor(Item))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Linear Model Coefficients for Weather Variables",
    subtitle = "Effect of each weather variable on units sold by item",
    x = "Coefficient Estimate",
    y = "Weather Variable",
    color = "Item Number"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "top"
  )

ols_grouped <- ols_results %>%
  arrange(term, Item) %>%
  select(term, Item, estimate, std.error, conf.low, conf.high, p.value)

print(ols_grouped) # Viewing Coefficients table

# Decision Tree importance visualisation
library(rpart.plot)

# Item 45 Tree Diagram
rpart.plot(tree_45, main = "Item 45", 
           type = 4, extra = 101, fallen.leaves = TRUE, cex = 0.8)

# Item 5 Tree Diagram
rpart.plot(tree_5, 
           main = "Item 5",
           type = 4, extra = 101, fallen.leaves = TRUE, cex = 0.7, box.palette = "BlGn")

# Item 9 Tree Diagram
rpart.plot(tree_9, 
           main = "Item 9 Sales", 
           type = 4, extra = 101, fallen.leaves = TRUE, cex = 0.8, box.palette = "OrRd")

# Extract importance for all items
imp_45 <- as.data.frame(tree_45$variable.importance) %>% 
  rename(Score = 1) %>% mutate(Item = 45, Feature = rownames(.))
imp_5  <- as.data.frame(tree_5$variable.importance) %>% 
  rename(Score = 1) %>% mutate(Item = 5, Feature = rownames(.))
imp_9  <- as.data.frame(tree_9$variable.importance) %>% 
  rename(Score = 1) %>% mutate(Item = 9, Feature = rownames(.))

# Combine and calculate percentages for interpretability
importance_comparison <- bind_rows(imp_45, imp_5, imp_9) %>%
  group_by(Item) %>%
  mutate(Importance_Pct = round(100 * (Score / sum(Score)), 2)) %>%
  select(Item, Feature, Importance_Pct) %>%
  arrange(Item, desc(Importance_Pct))

# Printing Table
print(as.data.frame(importance_comparison))

# Visualization of Decision Tree Importance
ggplot(importance_comparison, aes(x = reorder_within(Feature, Importance_Pct, Item), 
                                  y = Importance_Pct, 
                                  fill = Importance_Pct)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  scale_fill_gradient(low = "#D1E5F0", high = "#084594") + 
  coord_flip() +
  facet_wrap(~Item, scales = "free_y", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Decision Tree: Feature Importance by Item",
    x = "Weather Features",
    y = "Importance Percentage (%)"
  ) +
  # Add percentage labels to the ends of the bars
  geom_text(aes(label = paste0(Importance_Pct, "%")), 
            hjust = -0.1, 
            size = 3.2, 
            fontface = "bold") +
  theme(
    panel.spacing = unit(2.5, "lines"), 
    plot.margin = margin(20, 20, 20, 20),
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Performance metrics
mod_metrics <- function(pred, actual) {
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  c(RMSE = rmse, MAE = mae, R2 = r2)}

# Validation set model peformance
# Item 45 Performance
perf_lm_45    <- data.frame(Item = 45, Model = "Linear Regression", t(as.data.frame(mod_metrics(pred_lm_45, val_45$weekly_units))))
perf_tree_45  <- data.frame(Item = 45, Model = "Decision Tree", t(as.data.frame(mod_metrics(tree_pred_45, val_45$weekly_units))))

#Item 5 Performance 
perf_lm_5    <- data.frame(Item = 5, Model = "Linear Regression", t(as.data.frame(mod_metrics(pred_lm_5, val_5$weekly_units))))
perf_tree_5  <- data.frame(Item = 5, Model = "Decision Tree", t(as.data.frame(mod_metrics(tree_pred_5, val_5$weekly_units))))

# Item 9 Performance
perf_lm_9    <- data.frame(Item = 9, Model = "Linear Regression", t(as.data.frame(mod_metrics(pred_lm_9, val_9$weekly_units))))
perf_tree_9  <- data.frame(Item = 9, Model = "Decision Tree", t(as.data.frame(mod_metrics(tree_pred_9, val_9$weekly_units))))

perf_vis <- rbind(
  perf_lm_45, perf_tree_45,
  perf_lm_5,  perf_tree_5,
  perf_lm_9,  perf_tree_9
)

rownames(perf_vis) <- NULL

# Rounding the metrics to make them more readable
perf_vis <- perf_vis %>%
  mutate(
    RMSE = round(RMSE, 2),
    MAE  = round(MAE, 2),
    R2   = round(R2, 4)
  ) %>%
  # Sorting by item
  arrange(Item)

# Viewing the table
print(perf_vis)

# ---------------------------------------- Data Visualisation (Using 'winning' models for each item) ----------------------------------------

# Actual vs Predicted
test_45 <- test_set %>% filter(item_nbr == 45)
test_5  <- test_set %>% filter(item_nbr == 5)
test_9  <- test_set %>% filter(item_nbr == 9)

test_45 <- test_45 %>% mutate(pred = predict(tree_45, newdata = test_45))
test_5  <- test_5  %>% mutate(pred = predict(tree_5, newdata = test_5))
test_9  <- test_9  %>% mutate(pred = predict(lm_9, newdata = test_9))

final_plot <- bind_rows(test_45, test_5, test_9) %>%
  mutate(item = paste("Item", item_nbr)) %>%
  select(item, item_nbr, year, month, week, weekly_units, pred)



# Correlation Heatmap, Top 10 weather variables against each item

sw_wide <- train_set %>%
  select(week, month, year, item_nbr, weekly_units, all_of(weather_var)) %>%
  mutate(item_nbr = as.character(item_nbr)) %>% 
  pivot_wider(
    names_from = item_nbr, 
    values_from = weekly_units, 
    names_prefix = "item_",
    values_fn = sum) 

heat_cols <- c("item_45", "item_5", "item_9", weather_var)
corr_matrix <- cor(sw_wide[, heat_cols], use = "pairwise.complete.obs")

item_vs_weather <- corr_matrix[c("item_45", "item_5", "item_9"), weather_var]

rownames(item_vs_weather) <- c("Item 45", "Item 5", "Item 9")
colnames(item_vs_weather) <- c("Avg Temp", "Snowfall",      
                               "Precip Total", "Pressure",      
                               "Wind Speed", "Sunrise",     
                               "Sunset", "Thunderstorm")
print(item_vs_weather)

# Creating an actual vs predicted variable with changes to increase interpretability
weth_vs_units <- final_plot %>%
  left_join(
    # Select from sw_clean using its actual column name: item_nbr
    sw_clean %>% select(year, month, week, item_nbr, all_of(weather_var)), 
    by = c("year", "month", "week", "item_nbr")
  ) %>%
  # Imputation was done after split to reduce leakage, this uses unimputed data
  filter(!is.na(sunrise_mfm), !is.na(sunset_mfm)) %>% 
  pivot_longer(
    cols = c(stnpressure, sunrise_mfm, sunset_mfm, tavg, avgspeed),
    names_to = "weather_pred",
    values_to = "weather_value"
  ) %>%
  mutate(
    weather_bin = case_when(
      weather_pred == "tavg" ~ floor(weather_value / 2) * 2, # Every 5 degrees
      weather_pred %in% c("sunrise_mfm", "sunset_mfm") ~ floor(weather_value / 10) * 10, # 10-min blocks
      weather_pred == "avgspeed" ~ floor(weather_value / 0.5) * 0.5, # Every 0.5 mph
      weather_pred == "stnpressure" ~ floor(weather_value * 10) / 10, # 0.1 increments
      TRUE ~ floor(weather_value)
    ),
    top5_weather = case_match(weather_pred,
                              "stnpressure" ~ "Station Pressure",
                              "sunrise_mfm" ~ "Sunrise (mins from midnight)",
                              "sunset_mfm" ~ "Sunset (mins from midnight)",
                              "tavg" ~ "Average Temp",
                              "avgspeed" ~ "Avg Wind Speed")) %>%
  group_by(item, top5_weather, weather_bin) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(weekly_units, pred),
    names_to = "type",
    values_to = "unit_value"
  )

# Creating Multiple Bar charts for each item to reduce overlap

library(patchwork) # Importing library to 'patch' bar charts together

item_plot <- function(data, item_id) {
  data_subset <- data %>% filter(item == item_id)
  
  ggplot(data_subset, aes(x = factor(weather_bin), y = unit_value, fill = type)) +
    stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.8) +
    facet_wrap(~ top5_weather, scales = "free_x", nrow = 1) + 
    scale_fill_manual(values = c("weekly_units" = "grey", "pred" = "blue")) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) +
    theme_minimal(base_size = 11) +
    labs(subtitle = paste("Item:", item_id), y = "Units", x = NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "none", 
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold")
    )
}

# Generating three Item Plots
p1 <- item_plot(weth_vs_units, "Item 45")
p2 <- item_plot(weth_vs_units, "Item 5")
p3 <- item_plot(weth_vs_units, "Item 9")

# Viewing all plots simultaneously
bar_view <- (p1 / p2 / p3) + 
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "Actual vs. Predicted Units: Performance by Item",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  legend.position = "bottom")
  )

# Displaying final bar chart
bar_view

# Correlation Heatmap - Weather variables against units
cor_data <- sw_clean %>%
  group_by(item_nbr) %>%
  summarise(across(all_of(weather_var), 
                   ~cor(.x, weekly_units, use = "complete.obs"))) %>%
  pivot_longer(cols = -item_nbr, 
               names_to = "Weather_Variable", 
               values_to = "Correlation")

# Correlation Heatmap
ggplot(cor_data, aes(x = factor(item_nbr), y = Weather_Variable, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.8) + 
  geom_text(aes(label = round(Correlation, 2)), 
            color = ifelse(abs(cor_data$Correlation) > 0.3, "white", "black"), 
            size = 5) + 
  # Using a blue gradient
  scale_fill_gradient(low = "white", high = "blue", 
                      name = "Correlation") +
  theme_minimal() +
  labs(title = "Weather vs. Sales Correlation",
       subtitle = "Darker blue indicates a stronger relationship",
       x = "Item Number",
       y = "Weather Variable") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 11, face = "bold", color = "#01579B"),
    title = element_text(color = "black", face = "bold")
  )

# Station Pressure Visualisation
final_blue_data <- sw_clean %>%
  filter(item_nbr %in% c(5, 9, 45)) %>%
  mutate(Weather_Zone = case_when(
    stnpressure < 28.5 ~ "Low Pressure",
    stnpressure >= 28.5 & stnpressure <= 28.9 ~ "Stable Weather",
    stnpressure > 28.9 ~ "Optimal Conditions"
  )) %>%
  # Ensure the order is intuitive (Low to Optimal)
  mutate(Weather_Zone = factor(Weather_Zone, 
                               levels = c("Low Pressure", "Stable Weather", "Optimal Conditions")),
         item_label = paste("Item", item_nbr)) %>% 
  group_by(item_label, Weather_Zone) %>%
  summarise(Avg_Weekly_Sales = mean(weekly_units, na.rm = TRUE), .groups = "drop")

# Plotting the Faceted Bar Chart
ggplot(final_blue_data, aes(x = Weather_Zone, y = Avg_Weekly_Sales, fill = Weather_Zone)) +
  geom_col(width = 0.7, color = "white") +
  facet_wrap(~item_label) + 
  # Using the corporate blue palette
  scale_fill_manual(values = c("Low Pressure" = "#BDD7EE",    
                               "Stable Weather" = "#5B9BD5",  
                               "Optimal Conditions" = "#1F4E78")) + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#1F4E78"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
    plot.title = element_text(face = "bold", color = "#1F4E78", size = 18),
    plot.subtitle = element_text(color = "#5B9BD5", size = 12)
  ) +
  labs(
    title = "Inventory Strategy: Performance by Station Pressure",
    subtitle = "Darker blue indicates peak barometric conditions and highest sales volume",
    x = NULL, 
    y = "Average Units Sold (Week)"
  )

# Final Visualisation - Winning Variable x Winning Model

final_test_metrics <- rbind(
  data.frame(Item = "Item 45", as.list(mod_metrics(test_45$pred, test_45$weekly_units))),
  data.frame(Item = "Item 5",  as.list(mod_metrics(test_5$pred,  test_5$weekly_units))),
  data.frame(Item = "Item 9",  as.list(mod_metrics(test_9$pred,  test_9$weekly_units)))
)

print("--- Final Model Performance (Test Set) ---")
print(final_test_metrics)

dbDisconnect(con)



