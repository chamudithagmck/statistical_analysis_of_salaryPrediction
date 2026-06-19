Dataset Overview - Job Salary Prediction Dataset
# 1. SETUP AND LIBRARIES
# ---------------------------------------------------------
# Install these if you haven't: 
# install.packages(c("ggplot2", "dplyr", "scales", "gridExtra", "qcc", "SixSigma"))

library(ggplot2)
## Warning: package 'ggplot2' was built under R version 4.5.3
library(dplyr)
## Warning: package 'dplyr' was built under R version 4.5.3
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(scales)
## Warning: package 'scales' was built under R version 4.5.3
library(gridExtra)
## Warning: package 'gridExtra' was built under R version 4.5.3
## 
## Attaching package: 'gridExtra'
## The following object is masked from 'package:dplyr':
## 
##     combine
library(qcc)
## Warning: package 'qcc' was built under R version 4.5.3
## Package 'qcc' version 2.7
## Type 'citation("qcc")' for citing this R package in publications.
library(SixSigma)
## Warning: package 'SixSigma' was built under R version 4.5.3
# ---------------------------------------------------------
# 2. DATA LOADING & SUMMARY
# ---------------------------------------------------------
df <- read.csv("C:/Users/GMCha/Downloads/job_salary_prediction_dataset.csv")

# Quick look at the structure and summary
str(df)
## 'data.frame':    250000 obs. of  10 variables:
##  $ job_title       : chr  "AI Engineer" "Data Analyst" "Frontend Developer" "Business Analyst" ...
##  $ experience_years: int  10 5 18 19 15 0 6 4 5 18 ...
##  $ education_level : chr  "Bachelor" "Bachelor" "PhD" "PhD" ...
##  $ skills_count    : int  2 17 4 13 7 4 16 18 14 2 ...
##  $ industry        : chr  "Healthcare" "Telecom" "Media" "Retail" ...
##  $ company_size    : chr  "Medium" "Small" "Medium" "Medium" ...
##  $ location        : chr  "India" "Australia" "Singapore" "Canada" ...
##  $ remote_work     : chr  "Hybrid" "No" "No" "Yes" ...
##  $ certifications  : int  2 0 1 0 0 2 3 5 0 5 ...
##  $ salary          : int  109413 93764 148123 189123 165069 180351 165375 202463 171834 128377 ...
summary(df)
##   job_title         experience_years education_level     skills_count   
##  Length:250000      Min.   : 0.00    Length:250000      Min.   : 1.000  
##  Class :character   1st Qu.: 5.00    Class :character   1st Qu.: 5.000  
##  Mode  :character   Median :10.00    Mode  :character   Median :10.000  
##                     Mean   :10.01                       Mean   : 9.998  
##                     3rd Qu.:15.00                       3rd Qu.:15.000  
##                     Max.   :20.00                       Max.   :19.000  
##    industry         company_size         location         remote_work       
##  Length:250000      Length:250000      Length:250000      Length:250000     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##  certifications      salary      
##  Min.   :0.000   Min.   : 31867  
##  1st Qu.:1.000   1st Qu.:119358  
##  Median :2.000   Median :143453  
##  Mean   :2.492   Mean   :145718  
##  3rd Qu.:4.000   3rd Qu.:169492  
##  Max.   :5.000   Max.   :333046
# ---------------------------------------------------------
# THE 7 BASIC QUALITY TOOLS
# ---------------------------------------------------------

# --- Tool 1: Histogram (Salary Distribution) ---
# Load the library
library(ggplot2)

# Load your full dataset
df <- read.csv("C:/Users/GMCha/Downloads/job_salary_prediction_dataset.csv")

# Calculate the Average Salary (Measure of Central Tendency)
average_salary <- mean(df$salary)

# Create the Histogram
p1 <- ggplot(df, aes(x = salary)) +
  geom_histogram(bins = 50, fill = "#4db6ac", color = "white") +
  geom_vline(aes(xintercept = average_salary), color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Salary Distribution (All 250,000 Records)",
       subtitle = "Red dashed line indicates the Average Salary",
       x = "Annual Salary ($)",
       y = "Frequency (Count of Employees)") +
  scale_x_continuous(labels = scales::comma)
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once per session.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
# Display the plot
print(p1)
 
# Print Analysis Summary
cat("\n--- Histogram Analysis Summary ---\n")
## 
## --- Histogram Analysis Summary ---
cat("1. Purpose: This frequency distribution shows how often each different salary value occurs in the dataset[cite: 1012].\n")
## 1. Purpose: This frequency distribution shows how often each different salary value occurs in the dataset[cite: 1012].
cat("2. Central Tendency: The red dashed line represents the Mean (Average Salary), which is a key measure of central tendency[cite: 114].\n")
## 2. Central Tendency: The red dashed line represents the Mean (Average Salary), which is a key measure of central tendency[cite: 114].
cat("   Current Value:", round(average_salary, 2), "\n")
##    Current Value: 145718.1
cat("3. Data Insights: This histogram helps understand the location, spread, and shape of the salary data[cite: 1015].\n")
## 3. Data Insights: This histogram helps understand the location, spread, and shape of the salary data[cite: 1015].
cat("4. Role in Quality: As one of the 'Magnificent Seven' tools, it is an essential first step in Preliminary Data Analysis (EDA) to spot anomalies or patterns[cite: 306, 98, 99].\n")
## 4. Role in Quality: As one of the 'Magnificent Seven' tools, it is an essential first step in Preliminary Data Analysis (EDA) to spot anomalies or patterns[cite: 306, 98, 99].
cat("----------------------------------\n")
--- Histogram Analysis Summary ---
1. Purpose: This frequency distribution shows how often each different salary value occurs in the dataset[cite: 1012].
2. Central Tendency: The red dashed line represents the Mean (Average Salary), which is a key measure of central tendency[cite: 114].
   Current Value: 145718.1 
3. Data Insights: This histogram helps understand the location, spread, and shape of the salary data[cite: 1015].
4. Role in Quality: As one of the 'Magnificent Seven' tools, it is an essential first step in Preliminary Data Analysis (EDA) to spot anomalies or patterns[cite: 306, 98, 99].

## -----------------------------------------------------------------------------------
# ---------------------------------------------------------
# Pareto Chart
# ---------------------------------------------------------

library(ggplot2)
library(dplyr)

# 1. Identify the Top 10% Salary threshold
threshold <- quantile(df$salary, 0.90)

# 2. Filter for only High Earners
high_earners <- df %>% filter(salary >= threshold)

# 3. Pareto Data: High Earners by Location
loc_pareto <- high_earners %>%
  count(location) %>%
  arrange(desc(n)) %>%
  mutate(
    location = factor(location, levels = location),
    cumulative = cumsum(n) / sum(n) * 100
  )

# 4. Plot Chart (Using the smooth white theme)
max_n <- max(loc_pareto$n)

ggplot(loc_pareto, aes(x = location)) +
  geom_bar(aes(y = n), stat = "identity", fill = "darkblue") +
  geom_line(aes(y = cumulative * max_n / 100, group = 1), color = "#e74c3c", size = 1.2) +
  geom_point(aes(y = cumulative * max_n / 100), color = "#e74c3c", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max_n, name = "Cumulative %")) +
  theme_minimal() +
  labs(title = "Pareto: Locations of Top 10% Earners", x = "Location", y = "Count of High Earners")
 
# 5. Print Analysis Summary
cat("\n--- Pareto Chart Analysis Summary ---\n")
## 
## --- Pareto Chart Analysis Summary ---
cat("1. Purpose: This Pareto chart visually identifies which locations are most significant in terms of 'High Earners' (Top 10% of the dataset)[cite: 1078, 1082].\n")
## 1. Purpose: This Pareto chart visually identifies which locations are most significant in terms of 'High Earners' (Top 10% of the dataset)[cite: 1078, 1082].
cat("2. Order of Significance: The bars are arranged from longest on the left to shortest on the right, representing the frequency of high earners per location[cite: 1077].\n")
## 2. Order of Significance: The bars are arranged from longest on the left to shortest on the right, representing the frequency of high earners per location[cite: 1077].
cat("3. Cumulative Impact: The red line represents the cumulative percentage of top earners across locations.\n")
## 3. Cumulative Impact: The red line represents the cumulative percentage of top earners across locations.
cat("   - Analysis Tip: Identify which locations account for the first 80% of high earners to find the 'vital few'[cite: 1092, 1101].\n")
##    - Analysis Tip: Identify which locations account for the first 80% of high earners to find the 'vital few'[cite: 1092, 1101].
cat("4. Role in Quality Control: As one of the 'Magnificent Seven,' this tool helps prioritize efforts by showing which categories have the greatest impact on the target objective[cite: 302, 307].\n")
## 4. Role in Quality Control: As one of the 'Magnificent Seven,' this tool helps prioritize efforts by showing which categories have the greatest impact on the target objective[cite: 302, 307].
cat("-------------------------------------\n")
--- Pareto Chart Analysis Summary ---
1. Purpose: This Pareto chart visually identifies which locations are most significant in terms of 'High Earners' (Top 10% of the dataset)[cite: 1078, 1082].
2. Order of Significance: The bars are arranged from longest on the left to shortest on the right, representing the frequency of high earners per location[cite: 1077].
3. Cumulative Impact: The red line represents the cumulative percentage of top earners across locations.
   - Analysis Tip: Identify which locations account for the first 80% of high earners to find the 'vital few'[cite: 1092, 1101].
4. Role in Quality Control: As one of the 'Magnificent Seven,' this tool helps prioritize efforts by showing which categories have the greatest impact on the target objective[cite: 302, 307].
--------------------------------------------------------------------------------------
# ---------------------------------------------------------
# Scatter Plot
# ---------------------------------------------------------
# 1. Load libraries
library(ggplot2)

# 2. Sample the data for visual clarity
set.seed(42) # Keeps the sample consistent
df_sample <- df[sample(nrow(df), 2000), ]

# 3. Create the Scatter Plot
ggplot(df_sample, aes(x = experience_years, y = salary, color = education_level)) +
  # Add the points with some transparency (alpha)
  geom_point(alpha = 0.6, size = 2) +
  
  # Add a smooth regression line (Trend line)
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  
  # Styling
  theme_minimal() +
  scale_color_viridis_d() + # Professional color blind friendly palette
  labs(title = "Relationship: Experience vs. Salary",
       subtitle = "Sample of 2,000 records with Linear Trend Line",
       x = "Years of Experience",
       y = "Annual Salary ($)",
       color = "Education Level") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  )
## `geom_smooth()` using formula = 'y ~ x'
 
cat("\n--- Scatter Plot Analysis Summary ---\n")
## 
## --- Scatter Plot Analysis Summary ---
cat("1. Purpose: This scatterplot graphs pairs of numerical data (Experience vs. Salary) to look for a relationship between them[cite: 308, 1128, 1136].\n")
## 1. Purpose: This scatterplot graphs pairs of numerical data (Experience vs. Salary) to look for a relationship between them[cite: 308, 1128, 1136].
cat("2. Correlation: The red linear trend line indicates how the response variable (Salary) is associated with the input variable (Experience)[cite: 20, 1142].\n")
## 2. Correlation: The red linear trend line indicates how the response variable (Salary) is associated with the input variable (Experience)[cite: 20, 1142].
cat("3. Data Discovery: As part of Preliminary Data Analysis (EDA), this plot helps discover patterns, spot anomalies, and check assumptions[cite: 98, 99].\n")
## 3. Data Discovery: As part of Preliminary Data Analysis (EDA), this plot helps discover patterns, spot anomalies, and check assumptions[cite: 98, 99].
cat("4. Role in Quality Control: This is one of the 'Magnificent Seven' tools used to identify if variables are correlated and if they fall along a line or curve[cite: 302, 308, 1142].\n")
## 4. Role in Quality Control: This is one of the 'Magnificent Seven' tools used to identify if variables are correlated and if they fall along a line or curve[cite: 302, 308, 1142].
cat("------------------------------------\n")





--- Scatter Plot Analysis Summary ---
1. Purpose: This scatterplot graphs pairs of numerical data (Experience vs. Salary) to look for a relationship between them[cite: 308, 1128, 1136].
2. Correlation: The red linear trend line indicates how the response variable (Salary) is associated with the input variable (Experience)[cite: 20, 1142].
3. Data Discovery: As part of Preliminary Data Analysis (EDA), this plot helps discover patterns, spot anomalies, and check assumptions[cite: 98, 99].
4. Role in Quality Control: This is one of the 'Magnificent Seven' tools used to identify if variables are correlated and if they fall along a line or curve[cite: 302, 308, 1142].
--------------------------------------------------------------------------------------# ---------------------------------------------------------
# X bar chart
# ---------------------------------------------------------
# 1. Load libraries
library(qcc)
library(ggplot2)
library(dplyr)

# 2. Prepare Subgroups
# We take 1000 records to create 200 subgroups of size 5
subgroup_size <- 5
num_samples <- 200 
salary_data <- df$salary[1:(subgroup_size * num_samples)]
salary_matrix <- matrix(salary_data, ncol = subgroup_size, byrow = TRUE)

# 3. Calculate X-bar stats using qcc
q <- qcc(salary_matrix, type = "xbar", plot = FALSE)

# 4. Convert to a data frame for ggplot styling
df_qcc <- data.frame(
  Subgroup = 1:num_samples,
  Mean = q$statistics,
  UCL = q$limits[1, "UCL"],
  LCL = q$limits[1, "LCL"],
  Center = q$center
)

# 5. Create the "Smooth White" Plot
ggplot(df_qcc, aes(x = Subgroup, y = Mean)) +
  # Draw Control Limit Areas (Shaded region for stability)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = LCL, ymax = UCL), 
            fill = "#f8f9fa", alpha = 0.5) +
  
  # Draw the Control Lines
  geom_hline(yintercept = df_qcc$UCL[1], color = "#e74c3c", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = df_qcc$LCL[1], color = "#e74c3c", linetype = "dashed", size = 0.8) +
  geom_hline(yintercept = df_qcc$Center[1], color = "#2c3e50", size = 1) +
  
  # Draw the Data Points and Path
  geom_line(color = "#3498db", size = 0.7) +
  geom_point(color = "#3498db", size = 2) +
  
  # Labels and Theme
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "X-bar Control Chart: Salary Process Stability",
       subtitle = "Subgroup Size = 5 | UCL/LCL set at 3 Sigma",
       x = "Subgroups (Groups of 5)",
       y = "Average Salary of Subgroup ($)") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    panel.grid.minor = element_blank()
  ) +
  # Highlight Outliers in Red
  geom_point(data = filter(df_qcc, Mean > UCL | Mean < LCL), 
             color = "#e74c3c", size = 3)
 
grand_mean <- mean(df$salary)

# 2. Calculate Standard Error
# We group by skills_count and find the pooled standard deviation
stats <- aggregate(salary ~ skills_count, data = df, function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
stats <- do.call(data.frame, stats)

pooled_sd <- sqrt(sum((stats$salary.n - 1) * stats$salary.sd^2) / sum(stats$salary.n - 1))
avg_n <- mean(stats$salary.n)
se <- pooled_sd / sqrt(avg_n)


# --- Extracting values from the qcc object to match the plot ---
chart_cl  <- q$center
chart_ucl <- q$limits[1, "UCL"]
chart_lcl <- q$limits[1, "LCL"]

# 5. Print Analysis Summary
cat("\n--- X-bar Chart Analysis Summary ---\n")
## 
## --- X-bar Chart Analysis Summary ---
cat("1. Purpose: This chart is used to study variable data (Salary) to determine if the process average is stable over time[cite: 473].\n")
## 1. Purpose: This chart is used to study variable data (Salary) to determine if the process average is stable over time[cite: 473].
cat("2. Subgrouping: The dataset was divided into", num_samples, "subgroups of size", subgroup_size, "to smooth individual noise and focus on process shifts[cite: 476].\n")
## 2. Subgrouping: The dataset was divided into 200 subgroups of size 5 to smooth individual noise and focus on process shifts[cite: 476].
cat("3. The Statistical Lines (Matching the Plot):\n")
## 3. The Statistical Lines (Matching the Plot):
cat("   - Center Line (Grand Mean):", round(chart_cl, 2), "(Represents the historical average of the process)[cite: 371].\n")
##    - Center Line (Grand Mean): 144594.6 (Represents the historical average of the process)[cite: 371].
cat("   - Upper Control Limit (UCL):", round(chart_ucl, 2), "(Three standard errors above the mean)[cite: 371, 484].\n")
##    - Upper Control Limit (UCL): 192941.6 (Three standard errors above the mean)[cite: 371, 484].
cat("   - Lower Control Limit (LCL):", round(chart_lcl, 2), "(Three standard errors below the mean)[cite: 371, 486].\n")
##    - Lower Control Limit (LCL): 96247.64 (Three standard errors below the mean)[cite: 371, 486].
cat("4. Interpretation Guide:\n")
## 4. Interpretation Guide:
cat("   - In Control: If points stay within the limits, the variation is consistent[cite: 389].\n")
##    - In Control: If points stay within the limits, the variation is consistent[cite: 389].
cat("   - Out of Control: Any point outside the red dashed lines suggests a 'special cause' that requires investigation[cite: 435, 451].\n")
##    - Out of Control: Any point outside the red dashed lines suggests a 'special cause' that requires investigation[cite: 435, 451].
cat("----------------------------------------------------\n")





--- X-bar Chart Analysis Summary ---
1. Purpose: This chart is used to study variable data (Salary) to determine if the process average is stable over time[cite: 473].
2. Subgrouping: The dataset was divided into 200 subgroups of size 5 to smooth individual noise and focus on process shifts[cite: 476].
3. The Statistical Lines (Matching the Plot):
   - Center Line (Grand Mean): 144594.6 (Represents the historical average of the process)[cite: 371].
   - Upper Control Limit (UCL): 192941.6 (Three standard errors above the mean)[cite: 371, 484].
   - Lower Control Limit (LCL): 96247.64 (Three standard errors below the mean)[cite: 371, 486].
4. Interpretation Guide:
   - In Control: If points stay within the limits, the variation is consistent[cite: 389].
   - Out of Control: Any point outside the red dashed lines suggests a 'special cause' that requires investigation[cite: 435, 451].
--------------------------------------------------------------------------------------
# ---------------------------------------------------------
# Range Chat
# ---------------------------------------------------------
# --- Range Chart (R-Chart) ---

# 1. Load libraries
library(ggplot2)
library(dplyr)

# 2. Prepare Range Data for Technology Field
tech_range_data <- df %>%
  filter(industry == "Technology") %>%
  group_by(skills_count) %>%
  summarise(
    Salary_Range = max(salary) - min(salary)
  )

# 3. Calculate Average Range and Control Limits
mean_R <- mean(tech_range_data$Salary_Range)
sd_R <- sd(tech_range_data$Salary_Range)

ucl_r <- mean_R + (3 * sd_R)
lcl_r <- max(0, mean_R - (3 * sd_R))

# 4. Create the R-Chart
p <- ggplot(tech_range_data, aes(x = skills_count, y = Salary_Range)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = lcl_r, ymax = ucl_r), 
            fill = "#f8f9fa", alpha = 0.5) +
  geom_hline(yintercept = ucl_r, color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = lcl_r, color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = mean_R, color = "#2c3e50", size = 1) +
  geom_line(color = "#8e44ad", size = 1) +
  geom_point(color = "#8e44ad", size = 3) +
  scale_x_continuous(breaks = seq(1, 20, by = 1), limits = c(1, 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Range Chart (R-Chart): Salary Dispersion",
       subtitle = "Technology Industry | Range = Max Salary - Min Salary",
       x = "Skill Count",
       y = "Salary Range ($)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(p)
 
# 5. Print Analysis Summary
cat("\n--- Range (R) Chart Analysis Summary ---\n")
## 
## --- Range (R) Chart Analysis Summary ---
cat("1. The Data Points: Calculated using (Max - Min) for each subgroup.\n")
## 1. The Data Points: Calculated using (Max - Min) for each subgroup.
cat("2. The Center Line (R-bar): The average of those individual ranges.\n")
## 2. The Center Line (R-bar): The average of those individual ranges.
cat("   Current Value:", round(mean_R, 2), "\n")
##    Current Value: 231001.7
cat("3. The Upper Limit (UCL_R): Derived using the D4 constant (2.114 for n=5) multiplied by R-bar.\n")
## 3. The Upper Limit (UCL_R): Derived using the D4 constant (2.114 for n=5) multiplied by R-bar.
cat("   Current Value:", round(ucl_r, 2), "\n")
##    Current Value: 275006.4
cat("4. The Lower Limit (LCL_R): Calculated as D3 * R-bar (which is 0 for subgroups smaller than 7).\n")
## 4. The Lower Limit (LCL_R): Calculated as D3 * R-bar (which is 0 for subgroups smaller than 7).
cat("   Current Value:", round(lcl_r, 2), "\n")
##    Current Value: 186997
cat("----------------------------------------\n")
--- Range (R) Chart Analysis Summary ---
1. The Data Points: Calculated using (Max - Min) for each subgroup.
2. The Center Line (R-bar): The average of those individual ranges.
   Current Value: 231001.7 
3. The Upper Limit (UCL_R): Derived using the D4 constant (2.114 for n=5) multiplied by R-bar.
   Current Value: 275006.4 
4. The Lower Limit (LCL_R): Calculated as D3 * R-bar (which is 0 for subgroups smaller than 7).
   Current Value: 186997 
--------------------------------------------------------------------------------------
# --- S Chart Analysis ---

# 1. Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# 2. Prepare Data for Technology Field
tech_stats <- df %>%
  filter(industry == "Technology") %>%
  group_by(skills_count) %>%
  summarise(
    Avg_Salary = mean(salary),
    SD_Salary = sd(salary),
    Count = n()
  )

# 3. Calculate Control Limits for S
mean_S <- mean(tech_stats$SD_Salary)
# Using Normal Approximation for massive subgroup size (n)
avg_n_tech <- mean(tech_stats$Count)
sigma_s_tech <- mean_S / sqrt(2 * avg_n_tech)

ucl_s <- mean_S + 3 * sigma_s_tech
lcl_s <- max(0, mean_S - 3 * sigma_s_tech)

# 4. Create S Chart
p2 <- ggplot(tech_stats, aes(x = skills_count, y = SD_Salary)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = lcl_s, ymax = ucl_s), fill = "#f8f9fa", alpha = 0.5) +
  geom_hline(yintercept = c(ucl_s, lcl_s), color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = mean_S, color = "#2c3e50") +
  geom_line(color = "#e67e22", size = 1) + geom_point(color = "#e67e22", size = 2.5) +
  scale_x_continuous(breaks = seq(1, 20, by = 1), limits = c(1, 20)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + 
  labs(title = "S Chart: Salary Standard Deviation (Tech)", 
       subtitle = "Subgrouping by Skill Count",
       x = "Number of Skills", 
       y = "Standard Deviation ($)")

# Display the Chart
print(p2)
 
# 5. Print Analysis Summary
cat("\n--- Standard Deviation (S) Chart Analysis Summary ---\n")
## 
## --- Standard Deviation (S) Chart Analysis Summary ---
cat("1. Plotted Data Points (s): Subgroup standard deviations calculated using the formula:\n")
## 1. Plotted Data Points (s): Subgroup standard deviations calculated using the formula:
cat("   s = sqrt(sum(Xi - X_bar)^2 / (n - 1))\n")
##    s = sqrt(sum(Xi - X_bar)^2 / (n - 1))
cat("2. Center Line (S-bar): The average of all subgroup standard deviations.\n")
## 2. Center Line (S-bar): The average of all subgroup standard deviations.
cat("   Current Value:", round(mean_S, 2), "\n")
##    Current Value: 37012.97
cat("3. Control Limits: Derived using the subgroup size and process variability.\n")
## 3. Control Limits: Derived using the subgroup size and process variability.
cat("   Formula: UCL = B4 * S-bar | LCL = B3 * S-bar\n")
##    Formula: UCL = B4 * S-bar | LCL = B3 * S-bar
cat("   Note: For large n, normal approximation is used (3-sigma limits).\n")
##    Note: For large n, normal approximation is used (3-sigma limits).
cat("4. Current UCL (Upper Limit):", round(ucl_s, 2), "\n")
## 4. Current UCL (Upper Limit): 39181.72
cat("5. Current LCL (Lower Limit):", round(lcl_s, 2), "\n")
## 5. Current LCL (Lower Limit): 34844.21
cat("----------------------------------------------------\n")
--- Standard Deviation (S) Chart Analysis Summary ---
1. Plotted Data Points (s): Subgroup standard deviations calculated using the formula:
   s = sqrt(sum(Xi - X_bar)^2 / (n - 1))
2. Center Line (S-bar): The average of all subgroup standard deviations.
   Current Value: 37012.97 
3. Control Limits: Derived using the subgroup size and process variability.
   Formula: UCL = B4 * S-bar | LCL = B3 * S-bar
   Note: For large n, normal approximation is used (3-sigma limits).
4. Current UCL (Upper Limit): 39181.72 
5. Current LCL (Lower Limit): 34844.21 
--------------------------------------------------------------------------------------#------------------------------------------
#Individual chart
#------------------------------------------
# 1. Load library
library(qcc)

# 2. Prepare Data
# Plotting the first 100 observations for clarity
individual_data <- df$salary[1:100]

# 3. Create the Individual Chart (I-Chart)
# type = "xbar.one" is the standard for Individual Charts
i_chart <- qcc(individual_data, 
               type = "xbar.one", 
               title = "Individual Chart (I-Chart) for Salary",
               xlab = "Observation Number (Index)",
               ylab = "Salary ($)")
 	
# 4. View statistics
summary(i_chart)
## 
## Call:
## qcc(data = individual_data, type = "xbar.one", title = "Individual Chart (I-Chart) for Salary",     xlab = "Observation Number (Index)", ylab = "Salary ($)")
## 
## xbar.one chart for individual_data 
## 
## Summary of group statistics:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##  77939.0 117741.0 146059.0 144322.9 166064.8 213920.0 
## 
## Group sample size:  1
## Number of groups:  100
## Center of group statistics:  144322.9
## Standard deviation:  32108.61 
## 
## Control limits:
##       LCL      UCL
##  47997.09 240648.8
# 5. Print Analysis Summary
cat("\n--- Individual Chart (I-Chart) Analysis Summary ---\n")
## 
## --- Individual Chart (I-Chart) Analysis Summary ---
cat("1. Purpose: This chart is used to study variable data (Salary) when data are not sub-grouped or are generated infrequently[cite: 573, 574].\n")
## 1. Purpose: This chart is used to study variable data (Salary) when data are not sub-grouped or are generated infrequently[cite: 573, 574].
cat("2. Plotted Points: Each point represents a single individual salary observation rather than a group average.\n")
## 2. Plotted Points: Each point represents a single individual salary observation rather than a group average.
cat("3. Statistical Lines:\n")
## 3. Statistical Lines:
cat("   - Center Line (Average): Calculated as the mean of individual values (X-bar)[cite: 586].\n")
##    - Center Line (Average): Calculated as the mean of individual values (X-bar)[cite: 586].
cat("   - Control Limits: Set at 3-sigma, typically estimated using 2.66 times the Average Moving Range (MR)[cite: 610, 611].\n")
##    - Control Limits: Set at 3-sigma, typically estimated using 2.66 times the Average Moving Range (MR)[cite: 610, 611].
cat("4. Usage Context: It is particularly useful when frequent data are costly or the process changes slowly, though it is less sensitive than X-bar charts[cite: 578, 629].\n")
## 4. Usage Context: It is particularly useful when frequent data are costly or the process changes slowly, though it is less sensitive than X-bar charts[cite: 578, 629].
cat("5. Interpretation: This tool identifies specific 'out-of-control' signals where a single point falls outside the historical process performance limits[cite: 389, 451].\n")
## 5. Interpretation: This tool identifies specific 'out-of-control' signals where a single point falls outside the historical process performance limits[cite: 389, 451].
cat("--------------------------------------------------\n")
Summary of group statistics:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
 77939.0 117741.0 146059.0 144322.9 166064.8 213920.0 

Group sample size:  1
Number of groups:  100
Center of group statistics:  144322.9
Standard deviation:  32108.61 

Control limits:
      LCL      UCL
 47997.09 240648.8

--- Individual Chart (I-Chart) Analysis Summary ---
1. Purpose: This chart is used to study variable data (Salary) when data are not sub-grouped or are generated infrequently[cite: 573, 574].
2. Plotted Points: Each point represents a single individual salary observation rather than a group average.
3. Statistical Lines:
   - Center Line (Average): Calculated as the mean of individual values (X-bar)[cite: 586].
   - Control Limits: Set at 3-sigma, typically estimated using 2.66 times the Average Moving Range (MR)[cite: 610, 611].
4. Usage Context: It is particularly useful when frequent data are costly or the process changes slowly, though it is less sensitive than X-bar charts[cite: 578, 629].
5. Interpretation: This tool identifies specific 'out-of-control' signals where a single point falls outside the historical process performance limits[cite: 389, 451].
--------------------------------------------------------------------------------------

