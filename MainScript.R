install.packages(c("randomForest", "xgboost", "Metrics", "tidyverse"))
library(randomForest)
library(xgboost)
library(Metrics)      
library(tidyverse)

# 2. Import data (adjust path as needed)
outages <- read_csv("Data/processed_df_final.csv",
                    col_types = cols(
                      OUTAGEID                 = col_double(),
                      SUBSTATIONID             = col_character(),
                      EQUIPMENTTYPE            = col_character(),
                      EQUIPMENTID              = col_character(),
                      RECALLTIMEDAY            = col_double(),
                      RECALLTIMENIGHT          = col_double(),
                      REASON                   = col_character(),
                      ISSECONDARY              = col_double(),
                      ACTUAL_STARTTIME         = col_character(),
                      ACTUAL_ENDTIME           = col_character(),
                      COMPANYREFCODE           = col_character(),
                      DURATION_TIMEDELTA       = col_character(),  # will parse manually
                      OUTAGE_DURATION_MINUTES  = col_double(),
                      SUBSTATION_SUBURB        = col_character(),
                      VOLTAGE                  = col_character(),
                      EQUIPMENTTYPE_y          = col_character(),
                      EQ_VALIDFROM_YEAR        = col_character(),
                      EQ_VALIDTO_YEAR          = col_character(),
                      EQUIPMENT_VERSION_ID     = col_character(),
                      EQ_AGE_AT_OUTAGE         = col_character(),
                      START_MONTH              = col_integer(),
                      START_YEAR               = col_integer(),
                      END_MONTH                = col_integer(),
                      END_YEAR                 = col_integer()
                    )
)


glimpse(outages)

# 3. Data cleaning & feature engineering
library(tidyverse)
library(lubridate)

outages_clean <- outages %>%
  # 1. Parse start/end timestamps
  mutate(
    ACTUAL_STARTTIME = ymd_hms(ACTUAL_STARTTIME),
    ACTUAL_ENDTIME   = ymd_hms(ACTUAL_ENDTIME)
  ) %>%
  
  # 2. Recompute duration
  mutate(
    OUTAGE_DURATION_MINUTES = as.numeric(difftime(ACTUAL_ENDTIME,
                                                  ACTUAL_STARTTIME,
                                                  units = "mins"))
  ) %>%
  
  # 3. Clean REASON
  mutate(
    REASON = str_to_lower(REASON) %>%
      str_replace_all("equipt", "equipment") %>%
      str_to_title() %>%
      factor()
  ) %>%
  
  # 4. Convert your voltage column to numeric (kV)
  mutate(
    VOLTAGE_kV = parse_number(VOLTAGE)
  ) %>%
  
  # 5. Compute equipment age from your valid-from year
  mutate(
    EQ_VALIDFROM = ymd(EQ_VALIDFROM_YEAR),
    AGE_FROM_INSTALL = interval(EQ_VALIDFROM, ACTUAL_STARTTIME) / years(1)
  ) %>%
  
  # 6. Factor categorical predictors
  mutate(
    EQUIPMENTTYPE   = factor(EQUIPMENTTYPE),
    SUBSTATIONID    = factor(SUBSTATIONID),
    COMPANYREFCODE  = factor(COMPANYREFCODE)
  ) %>%
  

  
#  mutate(
#    ISSECONDARY = factor(ISSECONDARY,
#                         levels = c(0, 1),
#                         labels = c("Primary", "Secondary"))
#  ) %>%
  
  # 7. Select only modeling columns
  select(
    OUTAGE_DURATION_MINUTES,
    REASON,
    EQUIPMENTTYPE,
    ISSECONDARY,
    VOLTAGE_kV,
    EQ_AGE_AT_OUTAGE
  )

# Check result
glimpse(outages_clean)

# Load it to df
df <- outages_clean %>%
  select(REASON, EQUIPMENTTYPE, ISSECONDARY, VOLTAGE_kV, EQ_AGE_AT_OUTAGE, OUTAGE_DURATION_MINUTES)

glimpse(df)

# 1. Quick summaries
df %>% summarise(
  n = n(),
  mean_dur = mean(OUTAGE_DURATION_MINUTES),
  med_dur  = median(OUTAGE_DURATION_MINUTES),
  sd_dur   = sd(OUTAGE_DURATION_MINUTES),
  mean_volt = mean(VOLTAGE_kV, na.rm = TRUE),
  mean_age  = mean(EQ_AGE_AT_OUTAGE, na.rm = TRUE)
)



# ====================== EXPLORING DATA DISTRIBUTION ============================

# 2. Distribution of outage duration
ggplot(df, aes(x = OUTAGE_DURATION_MINUTES)) +
  geom_histogram(bins = 50) +
  labs(title = "Histogram of Outage Duration (minutes)",
       x = "Duration (mins)", y = "Count")


# 2. Distribution of outage duration (log scaled)
ggplot(df, aes(x = OUTAGE_DURATION_MINUTES)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Histogram of Outage Duration (minutes, log scale)",
       x = "Duration (mins, log10)", y = "Count")

# 3. Boxplot of duration by REASON
ggplot(df, aes(x = REASON, y = OUTAGE_DURATION_MINUTES)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  labs(title = "Outage Duration by Reason",
       y = "Duration (mins, log10)", x = "Reason")

# 3. Boxplot of duration by REASON (log scaled)
ggplot(df, aes(x = REASON, y = OUTAGE_DURATION_MINUTES)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Outage Duration by Reason",
       y = "Duration (mins, log10)", x = "Reason")

outages_clean %>%
  count(REASON) %>%
  ggplot(aes(x = fct_reorder(REASON, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Outages by Reason",
    x = "Reason",
    y = "Count"
  ) +
  theme_minimal()

# 4. Boxplot of duration by EQUIPMENTTYPE (limited to top 10 types)
top_types <- df %>%
  count(EQUIPMENTTYPE, sort = TRUE) %>%
  top_n(10) %>%
  pull(EQUIPMENTTYPE)

ggplot(filter(df, EQUIPMENTTYPE %in% top_types),
       aes(x = fct_reorder(EQUIPMENTTYPE, OUTAGE_DURATION_MINUTES, .fun = median),
           y = OUTAGE_DURATION_MINUTES)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Outage Duration by Top 10 Equipment Types",
       x = "Equipment Type", y = "Duration (mins, log10)")

# 5. Scatter: Voltage vs Duration
ggplot(df, aes(x = VOLTAGE_kV, y = OUTAGE_DURATION_MINUTES)) +
  geom_point(alpha = 0.3) +
  scale_y_log10() +
  labs(title = "Voltage (kV) vs Outage Duration",
       x = "Voltage (kV)", y = "Duration (mins, log10)")

# 6. Scatter: Equipment Age vs Duration (colored by Reason)
ggplot(df, aes(x = EQ_AGE_AT_OUTAGE, y = OUTAGE_DURATION_MINUTES, color = REASON)) +
  geom_point(alpha = 0.4) +
  scale_y_log10() +
  labs(title = "Equipment Age vs Outage Duration by Reason",
       x = "Equipment Age at Outage (years)", y = "Duration (mins, log10)")

# 7. Boxplot of outage duration by voltage class
ggplot(outages_clean, aes(x = factor(VOLTAGE_kV), y = OUTAGE_DURATION_MINUTES)) +
  geom_boxplot(outlier.size = 0.5) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Outage Duration by Voltage (kV)",
    x = "Voltage Class (kV)", 
    y = "Duration (mins, log scale)"
  )

# 8.1 Boxplot of outage duration by primary vs secondary equipment
ggplot(
  df %>% filter(!is.na(ISSECONDARY)), 
  aes(
    x = factor(ISSECONDARY, levels = c(0,1), labels = c("Primary","Secondary")),
    y = OUTAGE_DURATION_MINUTES
  )
) +
  geom_boxplot(outlier.size = 0.5) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Outage Duration: Primary vs Secondary Equipment",
    x = "Equipment Class",
    y = "Duration (mins, log scale)"
  )

# 8.2 Histogram of ISSECONDARY
outages_clean %>%
  # ensure it’s a factor with clear labels
  mutate(ISSECONDARY = factor(ISSECONDARY, levels = c(0, 1),
                              labels = c("Primary", "Secondary"))) %>%
  count(ISSECONDARY) %>%
  ggplot(aes(x = ISSECONDARY, y = n, fill = ISSECONDARY)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Outages: Primary vs Secondary Equipment",
    x = "Equipment Class",
    y = "Count"
  ) +
  theme_minimal()



# 1. Histogram of outage counts by voltage class
ggplot(outages_clean, aes(x = VOLTAGE_kV)) +
  geom_histogram(binwidth = 10, fill = "darkorange", color = "white") +
  labs(
    title = "Outage Counts by Voltage (kV)",
    x = "Voltage (kV)",
    y = "Count"
  ) +
  theme_minimal()

# 2. Histogram of outage counts by equipment age
ggplot(outages_clean, aes(x = EQ_AGE_AT_OUTAGE)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "white") +
  labs(
    title = "Outage Counts by Equipment Age (years)",
    x = "Equipment Age at Outage",
    y = "Count"
  ) +
  theme_classic()




ggplot(outages_clean, aes(x = EQ_AGE_AT_OUTAGE, y = OUTAGE_DURATION_MINUTES)) +
  geom_point(alpha = 0.3, size = 1) +
  scale_y_log10() +
  facet_wrap(~ REASON, scales = "free_x", ncol = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", size = 0.7) +
  labs(
    title = "Outage Duration vs. Equipment Age, Faceted by Reason",
    x = "Equipment Age at Outage (years)",
    y = "Duration (mins, log scale)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.5, "lines")
  )




library(tidyr)

heat_data <- df %>%
  group_by(REASON, EQUIPMENTTYPE) %>%
  summarise(mean_dur = mean(OUTAGE_DURATION_MINUTES), .groups = "drop") %>%
  # keep only cells with enough data
  left_join(
    outages_clean %>% count(REASON, EQUIPMENTTYPE),
    by = c("REASON","EQUIPMENTTYPE")
  ) %>%
  filter(n >= 50)

ggplot(heat_data, aes(x = EQUIPMENTTYPE, y = REASON, fill = mean_dur)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(trans = "log10", name = "Mean Duration") +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Mean Outage Duration by Reason & Equipment Type",
    x = "Equipment Type",
    y = "Reason"
  )



library(tidyverse)
library(viridis)

# 1. Compute mean duration by Reason × Secondary flag
heat_rs <- df %>%
  mutate(
    ISSECONDARY = factor(ISSECONDARY, levels = c(0,1),
                         labels = c("Primary","Secondary"))
  ) %>%
  group_by(REASON, ISSECONDARY) %>%
  summarise(
    mean_dur = mean(OUTAGE_DURATION_MINUTES),
    count    = n(),
    .groups  = "drop"
  ) %>%
  filter(count >= 30)  # only keep cells with at least 30 observations

# 2. Plot heatmap
ggplot(heat_rs, aes(x = ISSECONDARY, y = REASON, fill = mean_dur)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(trans = "log10", name = "Mean Duration\n(mins)") +
  labs(
    title = "Mean Outage Duration by Reason & Equipment Class",
    x = "Equipment Class",
    y = "Reason"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank()
  )





# ================ CORRELATION MATRIX ========================

# 1. Compute correlation matrix
cor_mat <- df %>%
  select(OUTAGE_DURATION_MINUTES, VOLTAGE_kV, EQ_AGE_AT_OUTAGE) %>%
  cor(use = "pairwise.complete.obs")

# 2. Convert to long format via tidyr
cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

# 3. Plot heatmap
ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Correlation Heatmap",
    x = NULL, y = NULL,
    fill = "Pearson r"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggplot(outages_clean, aes(x = EQ_AGE_AT_OUTAGE, y = OUTAGE_DURATION_MINUTES)) +
  geom_point(alpha = 0.3) +
  scale_y_log10() +
  facet_wrap(~ REASON, scales = "free_x") +
  labs(
    title = "Equipment Age vs Outage Duration, by Reason",
    x = "Equipment Age at Outage (years)",
    y = "Duration (mins, log10)"
  )




#======================= Indentifying important interactions =====================

#–– 1. Install & load packages
install.packages(c("xgboost", "shapviz", "dplyr", "Metrics"))
library(xgboost)
library(shapviz)
library(dplyr)
library(Metrics)
library(tidyr)

#–– 2. One‐hot encode your predictors
X <- model.matrix(
  ~ REASON + EQUIPMENTTYPE + ISSECONDARY + VOLTAGE_kV + EQ_AGE_AT_OUTAGE - 1,
  data = df
)
y <- df$OUTAGE_DURATION_MINUTES

#–– 3. Create XGBoost DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

#–– 4. Train a modest XGB model
params <- list(
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  eta         = 0.1,
  max_depth   = 6,
  subsample   = 0.8
)
set.seed(42)
xgb_model <- xgb.train(
  params   = params,
  data     = dtrain,
  nrounds  = 150,
  verbose  = 0
)


# 1. Get SHAP values (incl. bias term in last column)
shap_contrib <- predict(xgb_model, dtrain, predcontrib = TRUE)

# 2. Drop the bias column (last one) and name columns after your predictors
shap_mat <- shap_contrib[, -ncol(shap_contrib)]
colnames(shap_mat) <- colnames(X)

# 3. Convert to data.frame for dplyr
shap_df <- as.data.frame(shap_mat)

# 4. Compute all pairwise interaction scores = mean|phi_i * phi_j|
feature_names <- colnames(shap_df)
pairs <- combn(feature_names, 2, simplify = FALSE)

int_scores <- map_df(pairs, function(p) {
  prod_vals <- shap_df[[p[1]]] * shap_df[[p[2]]]
  tibble(
    interaction = paste(p, collapse = ":"),
    score       = mean(abs(prod_vals), na.rm = TRUE)
  )
})

# 5. Top 10 interactions
top_interactions <- int_scores %>%
  arrange(desc(score)) %>%
  slice_head(n = 5)

print(top_interactions)



# 6. Plot top 5 interactions
library(ggplot2)
library(forcats)

top_interactions %>%
  mutate(interaction = fct_reorder(interaction, score)) %>%
  ggplot(aes(x = interaction, y = score, fill = interaction)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set3", guide = FALSE) +
  labs(
    title   = "Top SHAP Interaction Scores",
    x       = "Feature Interaction",
    y       = "Score",
    caption = "Interaction strength measured as mean absolute product of SHAP values"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank()
  )











# ======================== fitting the model ================================

# 1. Prepare modeling frame
mod_df <- df %>%
  mutate(
    log_dur = log1p(OUTAGE_DURATION_MINUTES),   # log( duration + 1 )
    REASON = factor(REASON),
    EQUIPMENTTYPE = factor(EQUIPMENTTYPE)
  )

# 1. Start from mod_df (which has log_dur)
#    and compute inverse‐frequency weights on REASON, EQUIPMENTTYPE, ISSECONDARY
wts <- mod_df %>%
  count(REASON, EQUIPMENTTYPE, ISSECONDARY, name = "freq") %>%
  mutate(inv_freq = 1 / freq) %>%
  select(REASON, EQUIPMENTTYPE, ISSECONDARY, inv_freq)

# 2. Join weights into mod_df and rescale so mean = 1
mod_w <- mod_df %>%
  left_join(wts, by = c("REASON","EQUIPMENTTYPE","ISSECONDARY")) %>%
  mutate(inv_freq = inv_freq / mean(inv_freq, na.rm = TRUE))

# 3. Fit the weighted linear model
lm_final_w <- lm(
  log_dur ~ 
    REASON * EQ_AGE_AT_OUTAGE +
    VOLTAGE_kV * EQ_AGE_AT_OUTAGE +
    REASON * VOLTAGE_kV +
    REASON * ISSECONDARY,
  data    = mod_w,
  weights = inv_freq
)

# 4. Inspect
summary(lm_final_w)



# ================= model evaluation =========================

# 1. Residuals vs Fitted
# Base‐R
plot(lm_final_w, which = 1)


resid_df <- tibble(
  fitted = fitted(lm_final_w),
  resid  = residuals(lm_final_w)
)

ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values (log duration)",
    y = "Residuals"
  ) +
  theme_minimal()


# 2. QQ Plot
# Base‐R
plot(lm_final_w, which = 2)

# ggplot2 version
ggplot(resid_df, aes(sample = resid)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(color = "red") +
  labs(
    title = "Normal Q–Q Plot of Residuals",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal()


# 3. Predicted vs Actual
pred_df <- tibble(
  actual   = log1p(mod_w$OUTAGE_DURATION_MINUTES),
  predicted= fitted(lm_final_w)
)

ggplot(pred_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Predicted vs Actual (log scale)",
    x = "Actual log(duration+1)",
    y = "Predicted log(duration+1)"
  ) +
  theme_minimal()



# Adjusted R²
adj_r2 <- summary(lm_final_w)$adj.r.squared

# RMSE on the log scale
rmse_log <- sqrt(mean(residuals(lm_final_w)^2))

cat("Adjusted R²:", round(adj_r2, 4), "\n")
cat("RMSE (log(duration+1)):", round(rmse_log, 4), "\n")


# 1. Load required packages
install.packages(c("broom", "dplyr", "stringr", "ggplot2"))
library(broom)
library(dplyr)
library(stringr)
library(ggplot2)

# 2. Tidy up the model coefficients
coef_df <- tidy(lm_final_w) %>%
  filter(str_detect(term, ":")) %>%            # only interactions
  arrange(desc(abs(estimate))) %>%             # rank by magnitude
  slice_head(n = 5) %>%                        # top 5
  mutate(
    pct_change = (exp(estimate) - 1) * 100,    # translate log‐scale β to % change
    term = factor(term, levels = term)         # preserve order for plotting
  )

# 3. View the numbers
print(coef_df %>% select(term, estimate, pct_change))

# 4. Plot
ggplot(coef_df, aes(x = term, y = pct_change, fill = term)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", guide = FALSE) +
  labs(
    title = "Top 3 Interaction Effects on Outage Duration",
    x = "Interaction term",
    y = "Percent change in expected duration\nper one‐unit increase"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank()
  )


# 1. See all coefficient names
coef_names <- names(coef(lm_final_w))
print(coef_names)

# 2. Identify the ISSECONDARY term
sec_term <- coef_names[grep("ISSECONDARY", coef_names)]
print(sec_term)

# 3. Extract its value
beta_sec <- coef(lm_final_w)[sec_term]

# 4. Compute the multiplier and percent
multiplier <- exp(beta_sec)
pct_of_primary <- multiplier * 100

cat("Term:", sec_term, "\n")
cat("β =", round(beta_sec, 3), "\n")
cat("Multiplier (exp(β)) =", round(multiplier, 3), "\n")
cat("Secondary outages are", round(pct_of_primary, 1), "% of primary outages.\n")

# 5. (Optional) 95% CI
ci_log <- confint(lm_final_w, sec_term)
ci_mult <- exp(ci_log)
cat("95% CI on multiplier: [", round(ci_mult[1], 3), ",", round(ci_mult[2], 3), "]\n")


# =========================================================================


# 1. Split mod_w into train and test (80/20)
set.seed(42)
n      <- nrow(mod_w)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))
train_df  <- mod_w[train_idx, ]
test_df   <- mod_w[-train_idx, ]



# Install/load required packages
#install.packages(c("Metrics", "ggplot2", "dplyr"))
library(Metrics)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)

# 1. Predict on train and test
train_df <- train_df %>%
  mutate(pred_log_dur = predict(lm_final_w, newdata = train_df))

test_df <- test_df %>%
  mutate(pred_log_dur = predict(lm_final_w, newdata = test_df))

# 2. Compute RMSE by hand
train_rmse <- sqrt(mean((train_df$log_dur - train_df$pred_log_dur)^2, na.rm = TRUE))
test_rmse  <- sqrt(mean((test_df$log_dur  - test_df$pred_log_dur)^2, na.rm = TRUE))

cat("Train RMSE (log scale):", round(train_rmse, 3), "\n")
cat("Test  RMSE (log scale):", round(test_rmse, 3), "\n")

# 3. Combine for plotting
pred_df <- bind_rows(
  train_df %>% select(log_dur, pred_log_dur) %>% mutate(dataset = "Train"),
  test_df  %>% select(log_dur, pred_log_dur) %>% mutate(dataset = "Test")
)

# 4. Plot Predicted vs Actual (log scale), faceted by dataset
ggplot(pred_df, aes(x = log_dur, y = pred_log_dur)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~ dataset, ncol = 2) +
  labs(
    title = "LinearModel: Predicted vs Actual Log(Duration + 1)",
    x     = "Actual log(Duration+1)",
    y     = "Predicted log(Duration+1)"
  ) +
  theme_minimal()


# 2. Compute R² on the training set
ss_res_train <- sum((train_df$log_dur - train_df$pred_log_dur)^2, na.rm = TRUE)
ss_tot_train <- sum((train_df$log_dur - mean(train_df$log_dur, na.rm = TRUE))^2, na.rm = TRUE)
r2_train <- 1 - (ss_res_train / ss_tot_train)

# 3. Compute R² on the test set
ss_res_test <- sum((test_df$log_dur - test_df$pred_log_dur)^2, na.rm = TRUE)
ss_tot_test <- sum((test_df$log_dur - mean(test_df$log_dur, na.rm = TRUE))^2, na.rm = TRUE)
r2_test <- 1 - (ss_res_test / ss_tot_test)

# 4. Print results
cat("Train R²:", round(r2_train, 4), "\n")
cat("Test  R²:", round(r2_test, 4), "\n")







# ================= XGBOOST =========================

# Install/load packages
install.packages(c("xgboost", "ggplot2", "dplyr", "Metrics"))
library(xgboost)
library(ggplot2)
library(dplyr)
library(Metrics)

# 1. Prepare matrices and weights
# One‐hot encode predictors
X_full <- model.matrix(
  ~ REASON + EQUIPMENTTYPE + ISSECONDARY + VOLTAGE_kV + EQ_AGE_AT_OUTAGE -1,
  data = mod_w
)

y_full <- mod_w$log_dur
w_full <- mod_w$inv_freq

# Split into train/test using your existing indices
dtrain <- xgb.DMatrix(
  data   = X_full[train_idx, ],
  label  = y_full[train_idx],
  weight = w_full[train_idx]
)
dtest  <- xgb.DMatrix(
  data   = X_full[-train_idx, ],
  label  = y_full[-train_idx],
  weight = w_full[-train_idx]
)

# 2. Train an XGBoost model
params <- list(
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  eta         = 0.1,
  max_depth   = 6,
  subsample   = 0.8
)

set.seed(42)
xgb_mod <- xgb.train(
  params   = params,
  data     = dtrain,
  nrounds  = 150,
  watchlist= list(train = dtrain, test = dtest),
  verbose  = 1
)

# 3. Predict on train and test
pred_train <- predict(xgb_mod, dtrain)
pred_test  <- predict(xgb_mod, dtest)

# 4. Compute RMSE
train_errs <- y_full[train_idx] - pred_train
test_errs  <- y_full[-train_idx] - pred_test

train_rmse_xgb <- sqrt(mean(train_errs^2, na.rm = TRUE))
test_rmse_xgb  <- sqrt(mean(test_errs^2,  na.rm = TRUE))

cat("XGB Train RMSE (log scale):", round(train_rmse_xgb, 3), "\n")
cat("XGB Test  RMSE (log scale):", round(test_rmse_xgb, 3), "\n")

# 5. Plot Predicted vs Actual, faceted
results_df <- bind_rows(
  tibble(actual = y_full[train_idx], predicted = pred_train, dataset = "Train"),
  tibble(actual = y_full[-train_idx], predicted = pred_test, dataset = "Test")
)

ggplot(results_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~ dataset, ncol = 2) +
  labs(
    title = "XGBoost: Predicted vs Actual Log(Duration+1)",
    x     = "Actual log(Duration+1)",
    y     = "Predicted log(Duration+1)"
  ) +
  theme_minimal()



# ============== fine tuning XGBoost =========================

library(xgboost)
library(dplyr)
library(purrr)

# 1. Define parameter grid
param_grid <- expand.grid(
  max_depth        = c(4, 6, 8),
  eta              = c(0.01, 0.05, 0.1),
  subsample        = c(0.6, 0.8, 1.0),
  colsample_bytree = c(0.6, 0.8, 1.0)
)

# 2. Prepare DMatrix (train only)
dtrain <- xgb.DMatrix(
  data   = X_full[train_idx, ],
  label  = y_full[train_idx],
  weight = w_full[train_idx]
)

# 3. Cross-validation function
cv_results <- param_grid %>%
  mutate(
    # run CV for each row of param_grid
    cv = pmap(
      list(max_depth, eta, subsample, colsample_bytree),
      function(max_depth, eta, subsample, colsample_bytree) {
        xgb.cv(
          params = list(
            objective        = "reg:squarederror",
            eval_metric      = "rmse",
            max_depth        = max_depth,
            eta              = eta,
            subsample        = subsample,
            colsample_bytree = colsample_bytree
          ),
          data       = dtrain,
          nrounds    = 200,
          nfold      = 5,
          early_stopping_rounds = 10,
          verbose    = FALSE,
          showsd     = TRUE
        ) %>%
          # return the best score and best nrounds
          { list(
            best_rmse = min(.$evaluation_log$test_rmse_mean),
            best_n    = which.min(.$evaluation_log$test_rmse_mean)
          ) }
      }
    )
  ) %>%
  unnest_wider(cv)

# 4. Pick the best parameter set
best <- cv_results %>% arrange(best_rmse) %>% slice(1)
best

# 5. Refit final model on full training set
final_params <- list(
  objective        = "reg:squarederror",
  eval_metric      = "rmse",
  max_depth        = best$max_depth,
  eta              = best$eta,
  subsample        = best$subsample,
  colsample_bytree = best$colsample_bytree
)

set.seed(42)
xgb_tuned <- xgb.train(
  params   = final_params,
  data     = dtrain,
  nrounds  = best$best_n,
  verbose  = 1
)

# 6. Evaluate on test set
dtest <- xgb.DMatrix(
  data   = X_full[-train_idx, ],
  label  = y_full[-train_idx],
  weight = w_full[-train_idx]
)

pred_test_tuned <- predict(xgb_tuned, dtest)
test_rmse_tuned <- sqrt(mean((y_full[-train_idx] - pred_test_tuned)^2))
cat("Tuned XGB Test RMSE (log scale):", round(test_rmse_tuned, 3), "\n")

























# Install/load required packages
install.packages(c("Metrics", "dplyr", "xgboost"))
library(Metrics)
library(dplyr)
library(xgboost)

# Assume mod_w, train_idx, lm_final_w, xgb_tuned, X_full, y_full, w_full already exist

# 1. Define test indices
test_idx <- setdiff(seq_len(nrow(mod_w)), train_idx)

# 2. Prepare train/test data for the linear model
train_df <- mod_w[train_idx, ]
test_df  <- mod_w[test_idx, ]

y_train <- train_df$log_dur
y_test  <- test_df$log_dur

# 3. Predict with the linear model
pred_train_lm <- predict(lm_final_w, newdata = train_df)
pred_test_lm  <- predict(lm_final_w, newdata = test_df)

# 4. Compute LM metrics
adj_r2_train_lm <- summary(lm_final_w)$adj.r.squared
rmse_train_lm   <- sqrt(mean((y_train - pred_train_lm)^2, na.rm = TRUE))
rmse_test_lm    <- sqrt(mean((y_test  - pred_test_lm )^2, na.rm = TRUE))
r2_test_lm      <- 1 - sum((y_test - pred_test_lm)^2) / sum((y_test - mean(y_test))^2)

# 5. Prepare XGBoost DMatrix for train/test
dtrain <- xgb.DMatrix(
  data   = X_full[train_idx, ], 
  label  = y_full[train_idx], 
  weight = w_full[train_idx]
)
dtest <- xgb.DMatrix(
  data   = X_full[test_idx, ],  
  label  = y_full[test_idx],  
  weight = w_full[test_idx]
)

# 6. Predict with the tuned XGBoost
pred_train_xgb <- predict(xgb_tuned, dtrain)
pred_test_xgb  <- predict(xgb_tuned, dtest)

# 7. Compute XGB metrics
rmse_train_xgb <- sqrt(mean((y_full[train_idx] - pred_train_xgb)^2, na.rm = TRUE))
rmse_test_xgb  <- sqrt(mean((y_full[test_idx]  - pred_test_xgb )^2, na.rm = TRUE))
r2_train_xgb   <- 1 - sum((y_full[train_idx] - pred_train_xgb)^2) / sum((y_full[train_idx] - mean(y_full[train_idx]))^2)
r2_test_xgb    <- 1 - sum((y_full[test_idx]  - pred_test_xgb )^2) / sum((y_full[test_idx]  - mean(y_full[test_idx]))^2)

# 8. Print all results
cat("Linear Model (weighted):\n")
cat("  Train Adj. R²:", round(adj_r2_train_lm, 4), "\n")
cat("  Train RMSE    :", round(rmse_train_lm,   4), "\n")
cat("  Test  R²     :", round(r2_test_lm,       4), "\n")
cat("  Test  RMSE    :", round(rmse_test_lm,    4), "\n\n")

cat("XGBoost (tuned):\n")
cat("  Train R²     :", round(r2_train_xgb,   4), "\n")
cat("  Train RMSE    :", round(rmse_train_xgb, 4), "\n")
cat("  Test  R²     :", round(r2_test_xgb,    4), "\n")
cat("  Test  RMSE    :", round(rmse_test_xgb,  4), "\n")


