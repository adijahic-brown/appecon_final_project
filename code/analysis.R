###################
### REGRESSIONS ###
###################

# Check if the analysis is for Hyderabad and Karnataka regions
if (hyderabad_karnataka == 1) {

  # Define a list of control variables for the regression models
  h_control <- c("any_old_biz", "area_debt_total_base", "area_business_total_base", "area_exp_pc_mean_base",
                 "area_literate_head_base", "area_literate_base",  "hhsize_adj_1", "adults_1",
                 "children_1", "male_head_1", "head_age_1", "head_noeduc_1", "women1845_1", 
                 "anychild1318_1", "spouse_literate_1", "spouse_works_wage_1", "ownland_hyderabad_1", "ownland_village_1")

  # Create a formula snippet by concatenating control variables with "+"
  h_control_vars_formula_snippet <- paste0(h_control, collapse = "+")

  # Define the formula for the LASSO regression to select controls for treatment
  lasso_formula_treatment <- as.formula(paste0("treatment~", h_control_vars_formula_snippet, ""))
  
  # Run the LASSO regression to select relevant controls
  model_lasso_treatment <- rlasso(formula = lasso_formula_treatment,
                                  penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                 lambda.start = NULL, c = 1.1, gamma = 0.05),
                                  data = hyderabad_df) # Adjust penalty arguments as needed
  
  # Extract the names of selected variables (non-zero coefficients)
  support_treatment <- names(model_lasso_treatment$coefficients[model_lasso_treatment$coefficients != 0])
  
  # Remove the intercept term from the selected variables
  toRemove <- grep(pattern = "ntercept", x = support_treatment) # Match "Intercept" or similar terms
  support_treatment <- support_treatment[-toRemove]
  
  # Create a formula string for the selected controls
  controls_formula <- paste(support_treatment, collapse = " + ")

  # Build the regression formula for income analysis
  formula <- as.formula(paste("income_bizwage ~ treatment * anyinformal_1", "+", controls_formula))

  # Run the linear regression model for income
  h_income_transfers <- lm(formula, data = hyderabad_df)
  
  # Display the summary of the income regression model
  summary(h_income_transfers)

  # Add "any_old_biz" to the selected controls for the new business model
  support_treatment_biz <- c(support_treatment, "any_old_biz")
  controls_formula <- paste(support_treatment_biz, collapse = " + ")
  
  # Define the base formula for the new business model
  base_formula <- "any_newbiz_1 ~ treatment * anyinformal_1"
  
  # Combine the base formula with the selected controls
  full_formula <- as.formula(paste(base_formula, "+", controls_formula))

  # Run the probit regression model for new business analysis
  h_newbiz_transfers <- glm(full_formula, data = hyderabad_df, family = binomial(link = "probit"))
  
  # Display the summary of the new business probit model
  summary(h_newbiz_transfers)

  # Repeat the process for Karnataka region with a simplified control set
  support_treatment_biz <- c("any_old_biz")
  controls_formula <- paste(support_treatment_biz, collapse = " + ")
  base_formula <- "any_newbiz_1 ~ treatment * anyinformal_1"
  full_formula <- as.formula(paste(base_formula, "+", controls_formula))
  
  # Run the probit regression model for Karnataka
  k_newbiz_transfers <- glm(full_formula, data = karnataka_df, family = binomial(link = "probit"))
  
  # Display the summary of the Karnataka new business probit model
  summary(k_newbiz_transfers)

  ### COEF PLOTS FOR REGRESSIONS ###

  # Tidy the regression results for plotting
  h_income_tidy <- tidy(h_income_transfers, conf.int = TRUE) %>%
    mutate(model = "Hyderabad: Income")

  h_newbiz_tidy <- tidy(h_newbiz_transfers, conf.int = TRUE) %>%
    mutate(model = "Hyderabad: New business")

  k_newbiz_tidy <- tidy(k_newbiz_transfers, conf.int = TRUE) %>%
    mutate(model = "Karnataka: New business")

  # -------- Plot 1: Hyderabad income model alone --------

  # Filter relevant terms for plotting
  h_income_plot <- h_income_tidy %>%
    filter(grepl("treatment", term) | grepl("anyinformal_1", term))

  # Create a coefficient plot for the income model
  income_coef <- ggplot(h_income_plot, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(color = "steelblue", size = 1, position = position_dodge(width = 0.5)) +
    scale_x_discrete(labels = c(
      "treatment" = "Treatment",
      "anyinformal_1Yes" = "Any Informal Loan",
      "treatment:anyinformal_1Yes" = "Treatment × Informal"
    )) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Hyderabad: Effect on Income",
      x = "Variable",
      y = "Estimate"
    ) +
    theme_minimal(base_size = 14)
  
  # Save the income coefficient plot
  ggsave("output/coef_plot_income.png", plot = income_coef)

  # -------- Plot 2: Combined new business models (Hyderabad & Karnataka) --------

  # Combine the new business models for Hyderabad and Karnataka
  newbiz_combined <- bind_rows(h_newbiz_tidy, k_newbiz_tidy) %>%
    filter(grepl("treatment", term) | grepl("anyinformal_1", term))

  # Create a coefficient plot for the combined new business models
  newbiz_coef <- ggplot(newbiz_combined, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = model)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    scale_x_discrete(labels = c(
      "treatment" = "Treatment",
      "anyinformal_1Yes" = "Any Informal Loan",
      "treatment:anyinformal_1" = "Treatment × Informal",
      "treatment:anyinformal_1Yes" = "Treatment × Informal",  # In case this shows up
      "treatment:anyinformal_1TRUE" = "Treatment × Informal (TRUE)"
    )) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Effect on New Business Startups (Probit)",
      x = "Variable",
      y = "Estimate",
      color = "Model"
    ) +
    theme_minimal(base_size = 14)
  
  # Save the new business coefficient plot
  ggsave("output/coef_plot_newbiz.png", plot = newbiz_coef)
}

##############################
### FIGURES/VISUALIZATIONS ###
##############################

if (gem == 1) {
# Summarize the data to calculate the share of observations for each su_isic4_1d and busangyy
su_isic4_1d_shares <- gem_2020_df |>
  filter(!is.na(su_isic4_1d), !is.na(busangyy)) |> # Remove rows where su_isic4_1d or busangyy is NA
  mutate(su_isic4_1d_first_word = str_extract(su_isic4_1d, "^[^\\s]+")) |> # Extract the first word of su_isic4_1d
  group_by(su_isic4_1d_first_word, busangyy) |> # Group by the first word of su_isic4_1d and busangyy
  summarise(count = n(), .groups = "drop") |> # Count the number of observations
  group_by(busangyy) |> # Group by busangyy to calculate total counts within each group
  mutate(total_count = sum(count), # Calculate total count for each busangyy group
         share = count / total_count) |> # Calculate the share for each su_isic4_1d_first_word
  ungroup() |> # Ungroup the data
  arrange(desc(share)) # Arrange by share for better visualization

# Create the horizontal bar chart with side-by-side bars for busangyy
hbar_ind_gem <- ggplot(su_isic4_1d_shares, aes(x = reorder(su_isic4_1d_first_word, -share), y = share, fill = busangyy)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + # Side-by-side bars with thinner width
  coord_flip() + # Flip the coordinates for a horizontal bar chart
  labs(
    title = "Share of Firms' Industry by Investment Sources",
    x = "Industry",
    y = "Share",
    fill = "Source", # Legend title
    caption = "Data Source: Global Entrepreneurship Monitor (2022)" # Add data source at the bottom
  ) +
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentages
  theme_minimal()

# Save the updated plot
ggsave("output/hbar_industry_informal_side_by_side.png", plot = hbar_ind_gem)
}

if (enterprise == 1) {
# Summarize the data to calculate the share of observations for each a4a and k9
a4a_k9_shares <- enterprise_df |>
  filter(!is.na(a4a), !is.na(k9), k9 != "Other", k9 != "Don't know (spontaneous)") |> # Remove rows where a4a or k9 is NA or k9 is "Other" or "Don't know (spontaneous)"
  mutate(a4a_first_word = str_extract(a4a, "^[^\\s]+")) |> # Extract the first word of a4a
  group_by(a4a_first_word, k9) |> # Group by the first word of a4a and k9
  summarise(count = n(), .groups = "drop") |> # Count the number of observations
  group_by(k9) |> # Group by k9 to calculate total counts within each group
  mutate(total_count = sum(count), # Calculate total count for each k9 group
         share = count / total_count) |> # Calculate the share for each a4a_first_word
  ungroup() |> # Ungroup the data
  arrange(desc(share)) # Arrange by share for better visualization

# Create the horizontal bar chart with side-by-side bars for k9
hbar_ind_enterp <- ggplot(a4a_k9_shares, aes(x = reorder(a4a_first_word, -share), y = share, fill = k9)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + # Side-by-side bars with thinner width
  coord_flip() + # Flip the coordinates for a horizontal bar chart
  labs(
    title = "Share of Firms' Industry by Investment Sources",
    x = "Industry",
    y = "Share",
    fill = "Source", # Legend title
    caption = "Data Source: World Bank Enterprise Survey (2007-2018)" # Add data source at the bottom
  ) +
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentages
  theme_minimal()

# Save the updated plot
ggsave("output/hbar_a4a_k9_side_by_side.png", plot = hbar_ind_enterp)

# Define a mapping for string replacements
# Replace the placeholder mappings with your actual mappings
string_mapping <- c(
  "Shareholding company with non-traded shares or shares traded privately" = "Shareholding Co: Private",
  "Limited partnership" = "LLP",
  "Shareholding company with shares trade in the stock market" = "Shareholding Co: Public"
  # Add more mappings as needed
)

# Apply the mapping to the relevant column(s)
enterprise_df <- enterprise_df |>
  mutate(
    b1 = recode(b1, !!!string_mapping), # Replace values in the 'b1' column
  )

# Summarize the data to calculate the share of observations for each b1 and k9
b1_k9_shares <- enterprise_df |>
  filter(!is.na(b1), !is.na(k9), k9 != "Other", k9 != "Don't know (spontaneous)") |> # Remove rows where b1 or k9 is NA or k9 is "Other" or "Don't know (spontaneous)"
  group_by(b1, k9) |> # Group by b1 and k9
  summarise(count = n(), .groups = "drop") |> # Count the number of observations
  group_by(k9) |> # Group by k9 to calculate total counts within each group
  mutate(total_count = sum(count), # Calculate total count for each k9 group
         share = count / total_count) |> # Calculate the share for each b1
  ungroup() |> # Ungroup the data
  arrange(desc(share)) # Arrange by share for better visualization

# Create the horizontal bar chart with side-by-side bars for k9
hbar_b1_k9 <- ggplot(b1_k9_shares, aes(x = reorder(b1, -share), y = share, fill = k9)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + # Side-by-side bars with thinner width
  coord_flip() + # Flip the coordinates for a horizontal bar chart
  labs(
    title = "Share of Firms' Firm Type by Investment Sources",
    x = "Firm Type",
    y = "Share",
    fill = "Source", # Legend title
    caption = "Data Source: World Bank Enterprise Survey (2007-2018)" # Add data source at the bottom
  ) +
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentages
  theme_minimal()

# Save the updated plot
ggsave("output/hbar_b1_k9_side_by_side.png", plot = hbar_b1_k9)
}
