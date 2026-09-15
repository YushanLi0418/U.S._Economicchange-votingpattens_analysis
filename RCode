# ============================================================
# 1. Load packages
# ============================================================

library(tidycensus)
library(dplyr)
library(stringr)


# ============================================================
# 2. API key
# ============================================================


census_api_key(
  "0ded78586ee61cb3a5ea1c9b51c599f7ee94d200",
  install = TRUE,
  overwrite = TRUE
)
#
# 安装完成以后，以后不需要再把 API key 写进代码。


# ============================================================
# 3. Define ACS variables
# ============================================================

acs_variables <- c(
  
  # Population
  population = "B01003_001",
  
  # Median household income
  median_income = "B19013_001",
  
  # Education: population age 25+
  education_25plus = "B15003_001",
  
  bachelors = "B15003_022",
  masters = "B15003_023",
  professional = "B15003_024",
  doctorate = "B15003_025",
  
  # Employment
  civilian_labor_force = "B23025_003",
  unemployed = "B23025_005"
)


# ============================================================
# 4. Download 2016-2020 ACS 5-year county data
# ============================================================

county_raw <- get_acs(
  geography = "county",
  variables = acs_variables,
  year = 2020,
  survey = "acs5",
  output = "wide"
)


# ============================================================
# 5. Clean and create analytical variables
# ============================================================

county_data <- county_raw %>%
  
  mutate(
    
    # County FIPS
    FIPS = GEOID,
    
    # State FIPS = first 2 digits
    state_fips = str_sub(GEOID, 1, 2),
    
    # County FIPS within state = last 3 digits
    county_fips = str_sub(GEOID, 3, 5),
    
    # Bachelor's degree OR higher
    bachelor_plus =
      bachelorsE +
      mastersE +
      professionalE +
      doctorateE,
    
    # Percentage of age 25+ population with bachelor's or higher
    bachelor_plus_pct =
      100 * bachelor_plus / education_25plusE,
    
    # Unemployment rate
    unemployment_rate =
      100 * unemployedE / civilian_labor_forceE
    
  ) %>%
  
  transmute(
    
    FIPS,
    state_fips,
    county_fips,
    
    county_name = NAME,
    
    population = populationE,
    
    median_income = median_incomeE,
    
    bachelor_plus,
    bachelor_plus_pct,
    
    civilian_labor_force = civilian_labor_forceE,
    
    unemployed = unemployedE,
    
    unemployment_rate
  )


# ============================================================
# 6. Inspect data
# ============================================================

head(county_data)

dim(county_data)

summary(county_data)


# ============================================================
# 7. Check number of counties
# ============================================================

n_distinct(county_data$FIPS)


# ============================================================
# 8. Check missing values
# ============================================================

colSums(is.na(county_data))


# ============================================================
# 9. Optional: save data
# ============================================================

write.csv(
  county_data,
  "county_acs_2020.csv",
  row.names = FALSE
)

# ============================================================
# 10. Load packages for visualization and regression
# ============================================================

library(ggplot2)
library(scales)
library(fixest)
library(broom)


# ============================================================
# 11. Prepare analysis dataset
# ============================================================

analysis_data <- county_data %>%
  
  filter(
    !is.na(population),
    !is.na(median_income),
    !is.na(bachelor_plus_pct),
    !is.na(unemployment_rate),
    
    # Remove invalid / zero values
    population > 0,
    median_income > 0,
    civilian_labor_force > 0
  ) %>%
  
  mutate(
    
    # Median household income measured in $10,000
    # Makes regression coefficients easier to interpret
    income_10k = median_income / 10000,
    
    # County population is highly skewed,
    # so use log population in regression
    log_population = log(population)
    
  )


# Check cleaned dataset
dim(analysis_data)

summary(analysis_data)

n_distinct(analysis_data$FIPS)



# ============================================================
# 12. Descriptive statistics
# ============================================================

analysis_data %>%
  summarise(
    
    number_of_counties = n(),
    
    mean_income =
      mean(median_income, na.rm = TRUE),
    
    median_income_US =
      median(median_income, na.rm = TRUE),
    
    mean_unemployment =
      mean(unemployment_rate, na.rm = TRUE),
    
    mean_bachelor_plus =
      mean(bachelor_plus_pct, na.rm = TRUE),
    
    mean_population =
      mean(population, na.rm = TRUE)
    
  )



# ============================================================
# Visualization theme
# ============================================================

policy_theme <- theme_minimal(base_size = 13) +
  
  theme(
    
    plot.title = element_text(
      size = 17,
      face = "bold",
      color = "#1F2937",
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#6B7280",
      margin = margin(b = 14)
    ),
    
    axis.title = element_text(
      size = 11.5,
      face = "bold",
      color = "#374151"
    ),
    
    axis.text = element_text(
      size = 10,
      color = "#4B5563"
    ),
    
    panel.grid.major = element_line(
      color = "#E5E7EB",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#9CA3AF",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(
      15, 20, 15, 15
    ),
    
    legend.position = "bottom"
  )



# ============================================================
# 13. Visualization 1:
# Distribution of county unemployment rates
# ============================================================

mean_unemployment <- mean(
  analysis_data$unemployment_rate,
  na.rm = TRUE
)

median_unemployment <- median(
  analysis_data$unemployment_rate,
  na.rm = TRUE
)


plot_unemployment <- ggplot(
  analysis_data,
  aes(x = unemployment_rate)
) +
  
  geom_histogram(
    bins = 40,
    fill = "#2F5D8C",
    color = "white",
    linewidth = 0.35,
    alpha = 0.90
  ) +
  
  geom_vline(
    xintercept = mean_unemployment,
    color = "#D97706",
    linewidth = 1.1
  ) +
  
  geom_vline(
    xintercept = median_unemployment,
    color = "#374151",
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  
  annotate(
    "text",
    x = mean_unemployment,
    y = Inf,
    label = paste0(
      "Mean: ",
      round(mean_unemployment, 1),
      "%"
    ),
    vjust = 2,
    hjust = -0.1,
    size = 3.6,
    fontface = "bold",
    color = "#D97706"
  ) +
  
  labs(
    title = "Distribution of Unemployment Rates Across U.S. Counties",
    subtitle = "2016–2020 American Community Survey 5-Year Estimates",
    x = "Unemployment rate (%)",
    y = "Number of counties",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020\nOrange line = mean; dashed line = median"
  ) +
  
  policy_theme


plot_unemployment



# ============================================================
# 14. Visualization 2:
# Median income vs unemployment rate
# ============================================================

plot_income_unemployment <- ggplot(
  analysis_data,
  aes(
    x = median_income,
    y = unemployment_rate
  )
) +
  
  geom_point(
    shape = 21,
    fill = "#3B6D9A",
    color = "white",
    stroke = 0.2,
    alpha = 0.45,
    size = 2.1
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D97706",
    fill = "#F6C177",
    linewidth = 1.2,
    alpha = 0.20
  ) +
  
  scale_x_continuous(
    labels = dollar_format(
      scale = 0.001,
      suffix = "K"
    )
  ) +
  
  labs(
    title = "Household Income and Unemployment Across U.S. Counties",
    subtitle = "Each point represents one county; orange line shows the fitted OLS relationship",
    x = "Median household income",
    y = "Unemployment rate (%)",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020\nShaded area represents the 95% confidence interval"
  ) +
  
  policy_theme


plot_income_unemployment



# ============================================================
# 15. Visualization 3:
# Education vs median household income
# ============================================================

plot_education_income <- ggplot(
  analysis_data,
  aes(
    x = bachelor_plus_pct,
    y = median_income
  )
) +
  
  geom_point(
    shape = 21,
    fill = "#457B9D",
    color = "white",
    stroke = 0.2,
    alpha = 0.45,
    size = 2.1
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D97706",
    fill = "#F6C177",
    linewidth = 1.2,
    alpha = 0.20
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_y_continuous(
    labels = dollar_format(
      scale = 0.001,
      suffix = "K"
    )
  ) +
  
  labs(
    title = "Educational Attainment and Household Income",
    subtitle = "Higher educational attainment is associated with higher county household income",
    x = "Population age 25+ with bachelor's degree or higher (%)",
    y = "Median household income",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020\nOrange line shows the fitted OLS relationship"
  ) +
  
  policy_theme


plot_education_income



# ============================================================
# 16. Visualization 4:
# Education vs unemployment
# ============================================================

plot_education_unemployment <- ggplot(
  analysis_data,
  aes(
    x = bachelor_plus_pct,
    y = unemployment_rate
  )
) +
  
  geom_point(
    shape = 21,
    fill = "#4C78A8",
    color = "white",
    stroke = 0.2,
    alpha = 0.45,
    size = 2.1
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D97706",
    fill = "#F6C177",
    linewidth = 1.2,
    alpha = 0.20
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Educational Attainment and Unemployment",
    subtitle = "County-level relationship between education and unemployment",
    x = "Bachelor's degree or higher (%)",
    y = "Unemployment rate (%)",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020\nShaded area represents the 95% confidence interval"
  ) +
  
  policy_theme


plot_education_unemployment



# ============================================================
# 17. Visualization 5:
# Population vs household income
# ============================================================

plot_population_income <- ggplot(
  analysis_data,
  aes(
    x = population,
    y = median_income
  )
) +
  
  geom_point(
    shape = 21,
    fill = "#52796F",
    color = "white",
    stroke = 0.2,
    alpha = 0.45,
    size = 2.1
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D97706",
    fill = "#F6C177",
    linewidth = 1.2,
    alpha = 0.20
  ) +
  
  scale_x_log10(
    labels = label_number(
      scale_cut = cut_short_scale()
    )
  ) +
  
  scale_y_continuous(
    labels = dollar_format(
      scale = 0.001,
      suffix = "K"
    )
  ) +
  
  labs(
    title = "County Population and Household Income",
    subtitle = "Population displayed on a logarithmic scale",
    x = "County population (log scale)",
    y = "Median household income",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020\nOrange line shows the fitted OLS relationship"
  ) +
  
  policy_theme


plot_population_income



# ============================================================
# 18. Correlation analysis
# ============================================================

correlation_data <- analysis_data %>%
  
  select(
    median_income,
    unemployment_rate,
    bachelor_plus_pct,
    log_population
  )


cor(
  correlation_data,
  use = "complete.obs"
)



# ============================================================
# 19. Regression Model 1:
# Simple bivariate regression
#
# Question:
# Is county income associated with unemployment?
# ============================================================

model1 <- lm(
  
  unemployment_rate ~ income_10k,
  
  data = analysis_data
  
)


summary(model1)



# ============================================================
# 20. Regression Model 2:
# Multiple regression
#
# Add education and county population
# ============================================================

model2 <- lm(
  
  unemployment_rate ~
    income_10k +
    bachelor_plus_pct +
    log_population,
  
  data = analysis_data
  
)


summary(model2)



# ============================================================
# 21. Regression Model 3:
# Robust standard errors
#
# fixest is commonly used in applied economics /
# policy analysis because it handles robust SE
# and fixed effects very easily.
# ============================================================

model3 <- feols(
  
  unemployment_rate ~
    income_10k +
    bachelor_plus_pct +
    log_population,
  
  data = analysis_data,
  
  vcov = "hetero"
  
)


summary(model3)



# ============================================================
# 22. Regression Model 4:
# State Fixed Effects
#
# Compare counties within the same state
# ============================================================

model4 <- feols(
  
  unemployment_rate ~
    income_10k +
    bachelor_plus_pct +
    log_population
  |
    state_fips,
  
  data = analysis_data,
  
  cluster = ~state_fips
  
)


summary(model4)



# ============================================================
# 23. Compare regression models
# ============================================================

etable(
  model1,
  model2,
  model3,
  model4,
  
  headers = c(
    "Simple OLS",
    "Multiple OLS",
    "Robust SE",
    "State FE"
  )
)



# ============================================================
# 24. Extract regression coefficients
# ============================================================

model_results <- tidy(
  model3,
  conf.int = TRUE
)


model_results



# ============================================================
# 25. Visualization 6:
# Regression coefficient plot
# ============================================================

coefficient_plot_data <- model_results %>%
  
  filter(
    term != "(Intercept)"
  )


plot_coefficients <- ggplot(
  coefficient_plot_data,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.2
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Estimated Relationships with County Unemployment",
    subtitle = "OLS estimates with heteroskedasticity-robust standard errors",
    x = "Regression coefficient",
    y = NULL,
    caption = "Source: U.S. Census Bureau, ACS 2016–2020"
  ) +
  
  theme_minimal()


plot_coefficients



# ============================================================
# 26. Create income quartiles
# ============================================================

analysis_data <- analysis_data %>%
  
  mutate(
    
    income_quartile = ntile(
      median_income,
      4
    ),
    
    income_group = case_when(
      
      income_quartile == 1 ~ "Lowest income quartile",
      
      income_quartile == 2 ~ "Second quartile",
      
      income_quartile == 3 ~ "Third quartile",
      
      income_quartile == 4 ~ "Highest income quartile"
      
    )
    
  )



# ============================================================
# 27. Visualization 7:
# Compare unemployment across income quartiles
# ============================================================

plot_income_groups <- ggplot(
  analysis_data,
  aes(
    x = income_group,
    y = unemployment_rate
  )
) +
  
  geom_boxplot(
    outlier.alpha = 0.15
  ) +
  
  labs(
    title = "County Unemployment Across Income Groups",
    subtitle = "Counties grouped by median household income quartile",
    x = NULL,
    y = "Unemployment rate (%)",
    caption = "Source: U.S. Census Bureau, ACS 2016–2020"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x =
      element_text(
        angle = 20,
        hjust = 1
      )
  )


plot_income_groups



# ============================================================
# 28. Summary statistics by income quartile
# ============================================================

income_group_summary <- analysis_data %>%
  
  group_by(income_group) %>%
  
  summarise(
    
    counties = n(),
    
    average_income =
      mean(
        median_income,
        na.rm = TRUE
      ),
    
    average_unemployment =
      mean(
        unemployment_rate,
        na.rm = TRUE
      ),
    
    average_bachelor_pct =
      mean(
        bachelor_plus_pct,
        na.rm = TRUE
      )
    
  )


income_group_summary



# ============================================================
# 29. Optional: Save plots
# ============================================================

ggsave(
  "unemployment_distribution.png",
  plot_unemployment,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  "income_unemployment.png",
  plot_income_unemployment,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  "education_income.png",
  plot_education_income,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  "regression_coefficients.png",
  plot_coefficients,
  width = 8,
  height = 5,
  dpi = 300
)



# ============================================================
# 30. Save cleaned analysis dataset
# ============================================================

write.csv(
  analysis_data,
  "county_acs_2020_analysis.csv",
  row.names = FALSE
)

