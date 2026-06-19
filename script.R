# 1. Install and load the qcc library
# install.packages("qcc")
library(qcc)

# 2. Load your data
df <- read.csv("C:/Users/GMCha/Downloads/job_salary_prediction_dataset.csv")

# 3. Prepare data for X-bar Chart
# We take a portion of the salary data and group it into samples of 5
# To use qcc for X-bar, the data should be in a matrix where each row is a subgroup
subgroup_size <- 25
num_samples <- 1000  # Taking the first 1000 records to make 200 subgroups
salary_vector <- df$salary[1:(subgroup_size * num_samples)]
salary_matrix <- matrix(salary_vector, ncol = subgroup_size, byrow = TRUE)

# 4. Generate the X-bar Chart
x_bar_chart <- qcc(salary_matrix, 
                   type = "xbar", 
                   title = "X-bar Chart for Job Salaries",
                   xlab = "Subgroup (Group of 5)",
                   ylab = "Average Salary")

# Optional: Display a summary of the control limits and violations
summary(x_bar_chart)
#----------------------------------------------------------------------------------------------------------
#histogram
# Load the library
library(ggplot2)

# Load your full dataset
df <- read.csv("C:/Users/GMCha/Downloads/job_salary_prediction_dataset.csv")

# Create the Histogram
ggplot(df, aes(x = salary)) +
  geom_histogram(bins = 50, fill = "#4db6ac", color = "white") +
  geom_vline(aes(xintercept = mean(salary)), color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Salary Distribution (All 250,000 Records)",
       subtitle = "Red dashed line indicates the Average Salary",
       x = "Annual Salary ($)",
       y = "Frequency (Count of Employees)") +
  scale_x_continuous(labels = scales::comma)
average_salary <- mean(df$salary)
print(average_salary)

#--------------------------------------------------------------------------------------------------------------------
#Pareto char

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
  geom_bar(aes(y = n), stat = "identity", fill = "#2c3e50") +
  geom_line(aes(y = cumulative * max_n / 100, group = 1), color = "#e74c3c", size = 1.2) +
  geom_point(aes(y = cumulative * max_n / 100), color = "#e74c3c", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max_n, name = "Cumulative %")) +
  theme_minimal() +
  labs(title = "Pareto: Locations of Top 10% Earners", x = "Location", y = "Count of High Earners")

#-----------------------------------------------------------------------------------------------------------------------
#Scatter Plot


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

#----------------------------------------------------------------------------------------------------------------------------
#X bar chart

# 1. Load libraries
library(qcc)
library(ggplot2)
library(dplyr)

# 2. Prepare Subgroups
# We take 500 records to create 200 subgroups of size 5
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

# 3. Calculate Limits
ucl <- grand_mean + (3 * se)
lcl <- grand_mean - (3 * se)

# Print Results
cat("Grand Mean (CL):", grand_mean, "\n")
cat("UCL:", ucl, "\n")
cat("LCL:", lcl, "\n")

#--------------------------------------------------------------------------------------------------------------------
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
cat("1. The Data Points: Calculated using (Max - Min) for each subgroup.\n")
cat("2. The Center Line (R-bar): The average of those individual ranges.\n")
cat("   Current Value:", round(mean_R, 2), "\n")
cat("3. The Upper Limit (UCL_R): Derived using the D4 constant (2.114 for n=5) multiplied by R-bar.\n")
cat("   Current Value:", round(ucl_r, 2), "\n")
cat("4. The Lower Limit (LCL_R): Calculated as D3 * R-bar (which is 0 for subgroups smaller than 7).\n")
cat("   Current Value:", round(lcl_r, 2), "\n")
cat("----------------------------------------\n")

#-------------------------------------------------------------------------------------------------------------------------------
# S chart
# 1. Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra) # To display both charts together

# 2. Prepare Data for Technology Field
tech_stats <- df %>%
  filter(industry == "Technology") %>%
  group_by(skills_count) %>%
  summarise(
    Avg_Salary = mean(salary),
    SD_Salary = sd(salary),
    Count = n()
  )

# 3. Calculate Control Limits for X-bar and S
grand_mean_X <- mean(tech_stats$Avg_Salary)
mean_S <- mean(tech_stats$SD_Salary)
# Standard Error for X-bar
se_X <- mean_S / sqrt(mean(tech_stats$Count))
ucl_x <- grand_mean_X + 3 * se_X
lcl_x <- grand_mean_X - 3 * se_X

# Approx Standard Error for S (assuming large n)
se_S <- mean_S / sqrt(2 * mean(tech_stats$Count))
ucl_s <- mean_S + 3 * se_S
lcl_s <- max(0, mean_S - 3 * se_S)

# 5. Create S Chart
p2 <- ggplot(tech_stats, aes(x = skills_count, y = SD_Salary)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = lcl_s, ymax = ucl_s), fill = "#f8f9fa", alpha = 0.5) +
  geom_hline(yintercept = c(ucl_s, lcl_s), color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = mean_S, color = "#2c3e50") +
  geom_line(color = "#e67e22", size = 1) + geom_point(color = "#e67e22", size = 2.5) +
  scale_x_continuous(breaks = seq(1, 20, by = 1), limits = c(1, 20)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + labs(title = "S Chart: Salary Standard Deviation (Tech)", x = "Number of Skills", y = "Standard Deviation ($)")

# Arrange both charts vertically
grid.arrange(p1, p2, ncol = 1)

# 1. Calculate Standard Deviation for each group
stats_s <- aggregate(salary ~ skills_count, data = df, sd)
colnames(stats_s) <- c("skills_count", "S")

# 2. Calculate Center Line (Average S)
CL_S <- mean(stats_s$S)

# 3. Calculate n (Subgroup size)
avg_n <- nrow(df) / length(unique(df$skills_count))

# 4. Calculate Limits (Using Normal Approximation for large n)
sigma_s <- CL_S / sqrt(2 * avg_n)
UCL_S <- CL_S + (3 * sigma_s)
LCL_S <- CL_S - (3 * sigma_s)

# Print Results
cat("Center Line (CL):", CL_S, "\n")
cat("UCL:", UCL_S, "\n")
cat("LCL:", LCL_S, "\n")

#--------------------------------------------------------------------------------------------------------------------------------
#R Chart
# 1. Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# 2. Prepare Data (Group by Skill Count)
stats <- df %>%
  group_by(skills_count) %>%
  summarise(
    Avg_Salary = mean(salary),
    Sal_Range = max(salary) - min(salary),
    SD_Salary = sd(salary),
    Count = n()
  )

# 3. Calculate Limits for X-bar (X-double-bar)
grand_mean_X <- mean(stats$Avg_Salary)
se_X <- mean(stats$SD_Salary) / sqrt(mean(stats$Count))
ucl_x <- grand_mean_X + 3 * se_X
lcl_x <- grand_mean_X - 3 * se_X

# 4. Calculate Limits for Range (R-bar)
mean_R <- mean(stats$Sal_Range)
sd_R <- sd(stats$Sal_Range)
ucl_r <- mean_R + 3 * sd_R
lcl_r <- max(0, mean_R - 3 * sd_R)

# 5. Create X-bar Plot
p_xbar <- ggplot(stats, aes(x = skills_count, y = Avg_Salary)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = lcl_x, ymax = ucl_x), fill = "#f8f9fa", alpha = 0.5) +
  geom_hline(yintercept = c(ucl_x, lcl_x), color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = grand_mean_X, color = "#2c3e50") +
  geom_line(color = "#3498db", size = 1) + geom_point(color = "#3498db", size = 2) +
  scale_x_continuous(breaks = seq(1, 20, by = 1), limits = c(1, 20)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + labs(title = "X-bar Chart: Avg Salary vs Skill Count", x = "", y = "Mean Salary ($)")

# 6. Create Range Plot
p_range <- ggplot(stats, aes(x = skills_count, y = Sal_Range)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = lcl_r, ymax = ucl_r), fill = "#f8f9fa", alpha = 0.5) +
  geom_hline(yintercept = c(ucl_r, lcl_r), color = "#e74c3c", linetype = "dashed") +
  geom_hline(yintercept = mean_R, color = "#2c3e50") +
  geom_line(color = "#e74c3c", size = 1) + geom_point(color = "#e74c3c", size = 2) +
  scale_x_continuous(breaks = seq(1, 20, by = 1), limits = c(1, 20)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + labs(title = "R-Chart: Salary Range vs Skill Count", x = "Skill count", y = "Range ($)")

# Combine plots
grid.arrange(p_xbar, p_range, ncol = 1)

#------------------------------------------------------------------------------------------------------------------------------------
#Individual chart
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

# 5. Print Analysis Summary
cat("\n--- Individual Chart (I-Chart) Analysis Summary ---\n")
cat("1. Purpose: This chart is used to study variable data (Salary) when data are not sub-grouped or are generated infrequently[cite: 573, 574].\n")
cat("2. Plotted Points: Each point represents a single individual salary observation rather than a group average.\n")
cat("3. Statistical Lines:\n")
cat("   - Center Line (Average): Calculated as the mean of individual values (X-bar)[cite: 586].\n")
cat("   - Control Limits: Set at 3-sigma, typically estimated using 2.66 times the Average Moving Range (MR)[cite: 610, 611].\n")
cat("4. Usage Context: It is particularly useful when frequent data are costly or the process changes slowly, though it is less sensitive than X-bar charts[cite: 578, 629].\n")
cat("5. Interpretation: This tool identifies specific 'out-of-control' signals where a single point falls outside the historical process performance limits[cite: 389, 451].\n")
cat("--------------------------------------------------\n")

#-------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------
# 1. SETUP AND LIBRARIES
# ---------------------------------------------------------
# Install these if you haven't: 
# install.packages(c("ggplot2", "dplyr", "scales", "gridExtra", "qcc", "SixSigma"))

library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(qcc)
library(SixSigma)

# ---------------------------------------------------------
# 2. DATA LOADING & SUMMARY
# ---------------------------------------------------------
df <- read.csv("C:/Users/GMCha/Downloads/job_salary_prediction_dataset.csv")

# Quick look at the structure and summary
str(df)
summary(df)

# ---------------------------------------------------------
# 3. THE 7 BASIC QUALITY TOOLS
# ---------------------------------------------------------

# --- Tool 1: Histogram (Salary Distribution) ---
p_hist <- ggplot(df, aes(x = salary)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498db", color = "white") +
  geom_density(color = "#e74c3c", size = 1) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  labs(title = "Tool 1: Salary Distribution", x = "Annual Salary ($)", y = "Density")

# --- Tool 2: Check Sheet (Job Title Frequency) ---
p_check <- df %>%
  count(job_title) %>%
  ggplot(aes(x = reorder(job_title, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2c3e50") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tool 2: Job Title Frequencies", x = "Job Title", y = "Count")

# --- Tool 3: Pareto (High Earners by Location) ---
threshold <- quantile(df$salary, 0.90)
loc_pareto <- df %>%
  filter(salary >= threshold) %>%
  count(location) %>%
  arrange(desc(n)) %>%
  mutate(location = factor(location, levels = location),
         cum = cumsum(n) / sum(n) * 100)

p_pareto <- ggplot(loc_pareto, aes(x = location)) +
  geom_bar(aes(y = n), stat = "identity", fill = "#2c3e50") +
  geom_line(aes(y = cum * max(n) / 100, group = 1), color = "#e74c3c", size = 1) +
  geom_point(aes(y = cum * max(n) / 100), color = "#e74c3c") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max(loc_pareto$n), name = "Cumulative %")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Tool 3: Pareto - Top Earner Locations")

# --- Tool 4: Scatter Plot (Experience vs. Salary) ---
# Using a sample of 2000 for visual clarity
p_scatter <- ggplot(sample_n(df, 2000), aes(x = experience_years, y = salary)) +
  geom_point(alpha = 0.4, color = "#3498db") +
  geom_smooth(method = "lm", color = "#e74c3c") +
  theme_minimal() +
  labs(title = "Tool 4: Experience vs. Salary Correlation")

# --- Tool 5: Boxplot (Salary by Education) ---
p_box <- ggplot(df, aes(x = education_level, y = salary, fill = education_level)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Tool 5: Salary by Education Level")

# --- Tool 6: X-bar Chart (Stability of Skills/Salary) ---
# Grouped by skill count as a proxy for 'process subgroups'
stats_xbar <- df %>%
  group_by(skills_count) %>%
  summarise(mean_sal = mean(salary))

p_xbar <- ggplot(stats_xbar, aes(x = skills_count, y = mean_sal)) +
  geom_line(color = "#2ecc71", size = 1) +
  geom_point(color = "#2ecc71", size = 2) +
  scale_x_continuous(breaks = seq(1, 20, 5)) +
  theme_minimal() +
  labs(title = "Tool 6: X-bar Chart (Process Mean by Skills)")

# --- Tool 7: Cause-and-Effect (Ishikawa) ---
# Note: This tool is qualitative; we define the structure manually
ss.ceDiag(effect = "Annual Salary", 
          causes.gr = c("Individual", "Experience", "Environment", "Company"),
          causes = list(c("Education", "Certs"), c("Years Exp", "Skills"), 
                        c("Industry", "Location"), c("Job Title")),
          main = "Tool 7: Cause & Effect Diagram")

# ---------------------------------------------------------
# 4. VIEW CONSOLIDATED RESULTS
# ---------------------------------------------------------
grid.arrange(p_hist, p_check, p_pareto, p_scatter, p_box, p_xbar, ncol = 2)


#--------------------------------------------------------------------------------------------------------
