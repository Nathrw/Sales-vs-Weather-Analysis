library(DBI)
library(RSQLite)
library(tidyverse)
library(lubridate)

con <- dbConnect(SQLite(), "project1_raw_data.db")

top_3_units <- dbGetQuery(con, "
                        SELECT item_nbr, SUM(units) AS total_units_sold
                        FROM train
                        GROUP BY item_nbr
                        ORDER BY total_units_sold DESC
                        LIMIT 3;")

sw <- dbGetQuery(con, "
                            SELECT *
                            FROM train AS t
                            JOIN key AS k USING (store_nbr)
                            JOIN weather AS w USING (date, station_nbr)
                            WHERE item_nbr IN (5, 9, 45);")

nbr9_sw <- dbGetQuery(con, "
                            SELECT t.date, t.units, w.tavg
                            FROM train AS t
                            JOIN key AS k ON t.store_nbr = k.store_nbr
                            JOIN weather AS w ON t.date = w.date AND k.station_nbr= w.station_nbr
                            WHERE t.item_nbr = 9;")

colSums(is.na(sw))
colnames(sw)
str(sw)

# Dropping null values

sw_df <- sw %>%
  mutate(date = as.Date(date),
         snowfall = if_else(is.na(snowfall), 0, snowfall)) %>%
  drop_na(tavg)%>%
  select(-depart)

# Checking for and handling special characters 

sp_char <- sw_df %>%
  select(-codesum) %>%
  mutate(across(everything(), as.character)) %>%
  summarise(across(everything(), ~sum(. %in% c("T", "M", " T"), na.rm = TRUE))) %>%
  # Reshaping to view easier
  pivot_longer(everything(), names_to = "Col name", values_to = "Count") %>%
  filter(Count > 0)  %>%
  arrange(desc(Count))

#print(sp_char) - None Present

# Checking for correlation between columns 

wind_cols <- cor(sw_df[, c("avgspeed", "resultspeed", "resultdir")],
                 use = "complete.obs")
#glimpse(wind_cols) - very correlated, dropping redundant columns

sw_df <- sw_df %>% select(-resultspeed, -resultdir)

# Separating and counting weather events by code

weather_codes <- c("FC", "TS", "GR", "RA", "DZ", "SN", "SG", "GS", "PL", "IC", 
                   "FG\\+", "FG", "BR", "UP", "HZ", "FU", "VA", "DU", "DS", "PO", 
                   "SA", "SS", "PY", "SQ", "DR", "SH", "FZ", "MI", "PR", "BC", 
                   "BL", "VC", "\\+", "-")

code_srch <- paste(weather_codes, collapse = "|")
wcode_extract <- str_extract_all(sw_df$codesum, regex(code_srch, ignore_case = TRUE))

codes_table <- unlist(wcode_extract) %>% table() %>% sort(decreasing = TRUE)
head(codes_table, 3)

sw_df <- sw_df %>% 
  mutate(
    is_misty = if_else(str_detect(codesum, "BR"), 1, 0),
    is_raining = if_else(str_detect(codesum, "RA"), 1, 0),
    thunderstorm = if_else(str_detect(codesum, "TS"), 1, 0)
  ) %>% 
  select(-codesum) 

#summary(sw_df$units)

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

# Filtering data to remove extreme outliers
sw_final <- sw_df %>% filter(units < 1000)%>% 
  mutate(item_nbr = factor(item_nbr), 
         year = year(date), 
         month = month(date), 
         sunrise_mfm = as.numeric(hms(sunrise)) / 60, 
         sunset_mfm = as.numeric(hms(sunset)) / 60) %>% 
  select(-store_nbr, -station_nbr)

# Splitting train and test data

library(randomForest)
library(splitTools)

split <- partition(
  seq_len(nrow(sw_final)),
  p = c(train = 0.6, val = 0.2, test = 0.2))

str(split)

train_set <- sw_final[split$train, ]
val_set <- sw_final[split$val, ]
test_set <- sw_final[split$test, ]

# Imputing missing data

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

# Discovering how many Random Forest trees to use in the modl

library(optRF)

rf_opt <- opt_prediction(
  y = train_set$units,
  X = train_set %>% select(-units, -date, -item_nbr),
  X_Test = test_set %>% select(-units, -date, -item_nbr)
)

weather_var <- units ~ tavg + dewpoint + wetbulb + snowfall + 
  preciptotal + stnpressure + sealevel + avgspeed + sunrise_mfm + sunset_mfm + 
  is_misty + is_raining + thunderstorm

class(weather_var)

lm_item_models <- list()
rf_item_models <- list()

for(i in c(45, 5, 9)) {
  itemnbr_models <- filter(train_set, item_nbr == i)
  lm_item_models[[as.character(i)]] <- lm(weather_var, itemnbr_models)
  rf_item_models[[as.character(i)]] <- randomForest(weather_var, itemnbr_models, ntree = 1000, 
                                                    importance = TRUE)
}

saveRDS(rf_item_models, "rf_item_models.rds")
saveRDS(lm_item_models, "lm_item_models.rds")

# Performance metrics

mod_metrics <- function(pred, actual) {
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  r2 <- cor(pred, actual)^2
  c(RMSE = rmse, MAE = mae, R2 = r2)}

# Validation set model peformance

perform_vis <- list()
for (i in c(45, 5, 9)) {
  valid <- filter(val_set, item_nbr == i)
  perform_vis[[paste0("Item", i, "LM")]] <- mod_metrics(
    predict(lm_item_models[[as.character(i)]], valid),
    valid$units)
  perform_vis[[paste0("Item", i, "RF")]] <- mod_metrics(
    predict(rf_item_models[[as.character(i)]], valid),
    valid$units)}

perf_vis <- do.call(rbind, perform_vis)
print(perf_vis)

# Random Forest Visualisation

rf_imp <- bind_rows(
  lapply(names(rf_item_models), function(i) {
    as.data.frame(importance(rf_item_models[[i]])) %>%
      tibble::rownames_to_column("weather") %>%
      mutate(item = paste("Item", i))
  })
) %>%
  rename(effect = IncNodePurity)

ggplot(rf_imp,
       aes(x = reorder(weather, effect), y = effect, fill = item)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ item, scales = "free_y") +
  labs(
    title = "Weather Variable Effects on Sales (Random Forest)",
    x = "Weather Variable",
    y = "Variable Importance"
  ) +
  theme_minimal()

# Visualising OLS Coefficients 

library(broom)
library(jtools)

ols_fit <- list()
for(i in c(45, 5, 9)) {
  s <- summ(lm_item_models[[as.character(i)]], scale = TRUE, n.sd = 2)
  ols_fit[[as.character(i)]] <- tidy(s, conf.int = TRUE)%>%
    mutate(model = paste("Item", i))}

ols_results <- bind_rows(ols_fit)%>%
  filter(term != "(Intercept)")
ggplot(data = ols_results,
       aes(x = estimate, y = term, xmin = conf.low,
           xmax = conf.high, color = model)) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "LM Model Estimates of Weather effect on Units",
       x = "Coefficient Estimate",
       y = "Weather Variables",
       caption = "Models fit with OLS") +
  theme_minimal()
ols_grouped <- ols_results %>%
  arrange(term, model) %>%
  select(term, model, estimate, std.error, conf.low, conf.high, p.value)

print(ols_grouped)

# Actual vs Predicted
test_45 <- test_set %>% filter(item_nbr == 45)
test_5  <- test_set %>% filter(item_nbr == 5)
test_9  <- test_set %>% filter(item_nbr == 9)

test_45 <- test_45 %>% mutate(pred = predict(rf_item_models[["45"]], .))
test_5  <- test_5  %>% mutate(pred = predict(rf_item_models[["5"]], .))
test_9  <- test_9  %>% mutate(pred = predict(rf_item_models[["9"]], .))

final_plot <- bind_rows(test_45, test_5, test_9) %>%
  mutate(item = paste("Item", item_nbr)) %>%
  select(
    item,
    date,
    units,
    pred,
    stnpressure,
    sunrise_mfm,
    sunset_mfm,
    tavg,
    avgspeed)

# Scatter plot
ggplot(final_plot, aes(x = units, y = pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, size=1, linetype = "solid", color = "blue") +
  facet_wrap(~ item, scales = "free") +
  labs(
    title = "Actual vs Predicted units",
    x = "Actual Units",
    y = "Model Prediction"
  ) 

weth_vs_units <- final_plot %>%
  pivot_longer(
    cols = c(stnpressure, sunrise_mfm, sunset_mfm, tavg, avgspeed),
    names_to = "weather_var",
    values_to = "weather_value"
  ) %>%
  mutate(
    weather_bin = case_when(
      weather_var == "tavg" ~ floor(weather_value / 10) * 10,
      weather_var %in% c("sunrise_mfm", "sunset_mfm") ~ floor(weather_value / 30) * 30, # 30-min blocks
      weather_var == "avgspeed" ~ floor(weather_value / 5) * 5,
      TRUE ~ floor(weather_value)
    ),
    top5_weather = case_match(weather_var,
                              "stnpressure" ~ "Station Pressure",
                              "sunrise_mfm" ~ "Sunrise (mins from midnight)",
                              "sunset_mfm" ~ "Sunset (mins from midnight)",
                              "tavg" ~ "Average Temp",
                              "avgspeed" ~ "Avg Wind Speed")) %>%
  group_by(item, top5_weather, weather_bin) %>%
  filter(n() >= 3) %>% 
  ungroup() %>%
  pivot_longer(
    cols = c(units, pred),
    names_to = "type",
    values_to = "unit_value"
  )

ggplot(weth_vs_units, aes(x = factor(weather_bin), y = unit_value, fill = type)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  facet_grid(item ~ top5_weather, scales = "free_x") + 
  scale_fill_manual(values = c("units" = "grey", "pred" = "blue")) +
  theme_bw() +
  labs(
    title = "Actual vs. Predicted Units: Weather Impact Analysis",
    subtitle = "Filtered to show weather conditions with at least 3 days of historical data",
    x = "Weather Value",
    y = "Avg Units Sold"
  ) +
  theme(
    axis.text.x = element_text(vjust = 0.5, size = 7), # Vertical labels save space
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

szn_plot <- final_plot %>%
  mutate(month_split = month(date, label = TRUE)) %>%
  group_by(item, month_split) %>%
  summarise(av_day = mean(pred, na.rm = TRUE))

ggplot(szn_plot, aes(x = month_split, y = av_day, group = item))+
  geom_line(colour = "blue") +
  facet_wrap(~item, ncol = 1, scales = "free_y") +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    title = "Average Predicted Unit Sales Per Day",
    subtitle = "Across all years (2012 - 2014)",
    x = "Month",
    y = "Average Predicted Units"
  )

library(ggcorrplot)

# Heat map of correlation, top 5 weather variables against each item
sw_wide <- train_set %>%
  select(date, item_nbr, units, stnpressure, sunrise_mfm, sunset_mfm, tavg, avgspeed) %>%
  mutate(item_nbr = as.character(item_nbr)) %>% 
  pivot_wider(
    names_from = item_nbr, 
    values_from = units, 
    names_prefix = "item_",
    values_fn = sum) 

heat_cols <- c("item_45", "item_5", "item_9", 
               "stnpressure", "sunrise_mfm", "sunset_mfm", "tavg", "avgspeed")

corr_matrix <- cor(sw_wide[, heat_cols], use = "pairwise.complete.obs")

item_vs_weather <- corr_matrix[c("item_45", "item_5", "item_9"), 
                               c("stnpressure", "sunrise_mfm", "sunset_mfm", "tavg", "avgspeed")]

rownames(item_vs_weather) <- c("Item 45", "Item 5", "Item 9")
colnames(item_vs_weather) <- c("Station Pressure", "Sunrise", "Sunset", "Avg Temp", "Wind Speed")

ggcorrplot(item_vs_weather, 
           lab = TRUE,              
           colors = c("blue", "white"), 
           title = "Correlation Heatmap: Item Sales vs. Top 5 Weather variables",
           legend.title = "Pearson Corr") +
  labs(x = "Item Number", y = "Weather Variable") +
  theme_minimal()
print(item_vs_weather)

dbDisconnect(con)

