# ============================================================
# R PROGRAMMING PRACTICAL NOTEBOOK
# Organized from Module 1 foundations through Module 4 topics
# Includes: Data Wrangling, EDA, Preprocessing, Regression,
# Classification, Clustering, Association Rules, Time Series
# ============================================================
#
# HOW TO USE THIS FILE
# 1. Install the required packages listed in SECTION 0 once.
# 2. Run sections individually using Ctrl+Enter in RStudio.
# 3. Employee-data sections ask you to choose your Excel file.
# 4. Other sections use built-in R datasets.
# 5. Some sections create plots and output files in getwd().
#
# IMPORTANT:
# - Change assumptions and column names if your dataset differs.
# - Do not automatically delete/cap outliers without checking whether
#   they are data errors or genuine observations.
# - This file contains the topics covered so far in our conversation.
#   Add any extra topics specifically required by your official syllabus.
# ============================================================


# ============================================================
# SECTION 0: PACKAGE INSTALLATION (RUN ONCE, IF NEEDED)
# ============================================================
# Uncomment and run this block once to install the packages.
# Do not reinstall packages every time you open this file.

# install.packages(c(
#   "readxl", "writexl", "tidyverse", "skimr", "naniar",
#   "janitor", "DataExplorer", "GGally", "lubridate",
#   "corrplot", "class", "rpart", "e1071", "randomForest",
#   "gbm", "cluster", "factoextra", "arules", "arulesViz",
#   "forecast", "tseries"
# ))


# Install all required packages
install.packages(c(
  "readxl", "writexl", "tidyverse", "janitor", "lubridate",
  "skimr", "naniar", "DataExplorer", "GGally", "corrplot",
  "class", "rpart", "e1071", "randomForest", "gbm",
  "cluster", "factoextra", "arules", "arulesViz",
  "forecast", "tseries"
))

# Load all required libraries
library(readxl)
library(writexl)
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(naniar)
library(DataExplorer)
library(GGally)
library(corrplot)
library(class)
library(rpart)
library(e1071)
library(randomForest)
library(MASS)
library(gbm)
library(cluster)
library(factoextra)
library(arules)
library(arulesViz)
library(forecast)
library(tseries)

# ============================================================
# MODULE 1: R PROGRAMMING FOUNDATIONS
# ============================================================

# ------------------------------------------------------------
# 1.1 Basic calculations and variables
# ------------------------------------------------------------
a <- 10
b <- 5

a + b
a - b
a * b
a / b
a^2

# ------------------------------------------------------------
# 1.2 Vectors
# ------------------------------------------------------------
marks <- c(75, 82, 91, 68, 88)

marks
length(marks)
sum(marks)
mean(marks)
median(marks)
min(marks)
max(marks)
sort(marks)
marks[1]
marks[1:3]

# ------------------------------------------------------------
# 1.3 Common data types
# ------------------------------------------------------------
student_name <- "Amina"     # character
student_age <- 22           # numeric
is_passed <- TRUE           # logical
student_grade <- factor("A") # factor

class(student_name)
class(student_age)
class(is_passed)
class(student_grade)

# ------------------------------------------------------------
# 1.4 Matrices
# ------------------------------------------------------------
matrix_data <- matrix(1:12, nrow = 3, ncol = 4)
matrix_data
dim(matrix_data)
matrix_data[1, 2]

# ------------------------------------------------------------
# 1.5 Lists
# ------------------------------------------------------------
student <- list(
  Name = "Amina",
  Age = 22,
  Marks = c(75, 82, 91)
)

student
student$Name
student$Marks

# ------------------------------------------------------------
# 1.6 Data frames
# ------------------------------------------------------------
students <- data.frame(
  Name = c("Amina", "Ravi", "Sara", "John"),
  Age = c(22, 21, 23, 20),
  Marks = c(85, 72, 91, 65)
)

students
head(students)
str(students)
summary(students)
View(students)

# ------------------------------------------------------------
# 1.7 Conditions and loops
# ------------------------------------------------------------
score <- 76

if (score >= 50) {
  print("Pass")
} else {
  print("Fail")
}

for (i in 1:5) {
  print(i)
}

# ------------------------------------------------------------
# 1.8 Functions
# ------------------------------------------------------------
calculate_average <- function(x) {
  mean(x, na.rm = TRUE)
}

calculate_average(c(10, 20, 30, NA))

# ------------------------------------------------------------
# 1.9 Basic plots using built-in data
# ------------------------------------------------------------
data(iris)

plot(
  iris$Sepal.Length, iris$Petal.Length,
  main = "Sepal Length vs Petal Length",
  xlab = "Sepal Length",
  ylab = "Petal Length",
  pch = 19,
  col = iris$Species
)

hist(
  iris$Sepal.Length,
  main = "Distribution of Sepal Length",
  xlab = "Sepal Length"
)

boxplot(
  Sepal.Length ~ Species,
  data = iris,
  main = "Sepal Length by Species",
  xlab = "Species",
  ylab = "Sepal Length"
)


# ============================================================
# MODULE 1 / DATA EXPLORATION: BASIC EDA
# ============================================================

# ------------------------------------------------------------
# 1.10 Load and inspect a built-in dataset
# ------------------------------------------------------------
data(iris)

head(iris)       # first six rows
tail(iris)       # last six rows
dim(iris)        # number of rows and columns
names(iris)      # column names
str(iris)        # column types
summary(iris)    # summary statistics
View(iris)

# ------------------------------------------------------------
# 1.11 Missing values and duplicates
# ------------------------------------------------------------
colSums(is.na(iris))
sum(duplicated(iris))

# ------------------------------------------------------------
# 1.12 Frequency tables
# ------------------------------------------------------------
table(iris$Species)
prop.table(table(iris$Species)) * 100


# ============================================================
# DATA WRANGLING WITH DPLYR AND TIDYR
# ============================================================

library(tidyverse)

# The examples in this section use the built-in iris dataset.
data(iris)
iris_df <- as_tibble(iris)

# ------------------------------------------------------------
# DW 1: SELECT COLUMNS
# ------------------------------------------------------------
select(iris_df, Sepal.Length, Sepal.Width, Species)

# Select a range of columns
select(iris_df, Sepal.Length:Petal.Width)

# Exclude a column
select(iris_df, -Species)

# ------------------------------------------------------------
# DW 2: FILTER ROWS
# ------------------------------------------------------------
filter(iris_df, Species == "setosa")
filter(iris_df, Sepal.Length > 5)
filter(iris_df, Species == "virginica", Sepal.Length > 6)

# Use OR condition
filter(iris_df, Species == "setosa" | Species == "versicolor")

# ------------------------------------------------------------
# DW 3: ARRANGE / SORT ROWS
# ------------------------------------------------------------
arrange(iris_df, Sepal.Length)
arrange(iris_df, desc(Sepal.Length))

# ------------------------------------------------------------
# DW 4: MUTATE / CREATE NEW COLUMNS
# ------------------------------------------------------------
iris_new <- iris_df %>%
  mutate(
    Sepal_Ratio = Sepal.Length / Sepal.Width,
    Petal_Total = Petal.Length + Petal.Width
  )

head(iris_new)

# ------------------------------------------------------------
# DW 5: RENAME COLUMNS
# ------------------------------------------------------------
iris_renamed <- iris_df %>%
  rename(
    SepalLength = Sepal.Length,
    FlowerSpecies = Species
  )

head(iris_renamed)

# ------------------------------------------------------------
# DW 6: GROUP BY AND SUMMARISE
# ------------------------------------------------------------
iris_summary <- iris_df %>%
  group_by(Species) %>%
  summarise(
    Mean_Sepal_Length = mean(Sepal.Length, na.rm = TRUE),
    Mean_Petal_Length = mean(Petal.Length, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

print(iris_summary)

# Count rows by group
iris_df %>%
  count(Species)

# ------------------------------------------------------------
# DW 7: DISTINCT / REMOVE DUPLICATE ROWS
# ------------------------------------------------------------
distinct(iris_df)
distinct(iris_df, Species, .keep_all = TRUE)

# ------------------------------------------------------------
# DW 8: HANDLE MISSING VALUES
# ------------------------------------------------------------
wrangle_missing <- tibble(
  Name = c("A", "B", "C", "D"),
  Score = c(80, NA, 90, NA)
)

# Keep only complete rows
drop_na(wrangle_missing)

# Replace missing scores with the mean
wrangle_missing %>%
  mutate(Score = replace_na(Score, mean(Score, na.rm = TRUE)))

# ------------------------------------------------------------
# DW 9: PIVOT LONGER AND PIVOT WIDER
# ------------------------------------------------------------
wide_data <- tibble(
  Student = c("A", "B"),
  Math = c(80, 90),
  Science = c(85, 88)
)

long_data <- wide_data %>%
  pivot_longer(
    cols = c(Math, Science),
    names_to = "Subject",
    values_to = "Marks"
  )

print(long_data)

back_to_wide <- long_data %>%
  pivot_wider(
    names_from = Subject,
    values_from = Marks
  )

print(back_to_wide)

# ------------------------------------------------------------
# DW 10: JOINS
# ------------------------------------------------------------
student_info <- tibble(
  StudentID = c(1, 2, 3),
  Name = c("Amina", "Ravi", "Sara")
)

student_marks <- tibble(
  StudentID = c(1, 2, 4),
  Marks = c(85, 72, 90)
)

left_join(student_info, student_marks, by = "StudentID")
inner_join(student_info, student_marks, by = "StudentID")
right_join(student_info, student_marks, by = "StudentID")
full_join(student_info, student_marks, by = "StudentID")


# ============================================================
# MODULE 2: DATA PREPROCESSING USING EMPLOYEE EXCEL DATA
# ============================================================

# ------------------------------------------------------------
# 2.1 Load packages and import the Excel file
# ------------------------------------------------------------
library(readxl)
library(tidyverse)
library(skimr)
library(naniar)
library(DataExplorer)
library(GGally)
library(lubridate)
library(writexl)

# Choose Employee_Preprocessing_Dataset.xlsx when prompted.
# The workbook should contain a sheet named "EmployeeData".
emp <- read_excel(
  path = file.choose(),
  sheet = "EmployeeData"
)

# Keep a working copy
emp1 <- emp

# ------------------------------------------------------------
# 2.2 Inspect the raw dataset
# ------------------------------------------------------------
head(emp)
tail(emp)
View(emp)
dim(emp)
names(emp)
str(emp)
summary(emp)
skim(emp)

colSums(is.na(emp))
round(colMeans(is.na(emp)) * 100, 2)

# Check category values and frequencies
unique(emp$Gender)
unique(emp$Department)
unique(emp$City)
unique(emp$Performance)

table(emp$Gender, useNA = "ifany")
table(emp$Department, useNA = "ifany")
table(emp$City, useNA = "ifany")
table(emp$Performance, useNA = "ifany")

# Check duplicate rows and IDs
sum(duplicated(emp))
if ("EmployeeID" %in% names(emp)) {
  sum(duplicated(emp$EmployeeID))
}

# ------------------------------------------------------------
# 2.3 Clean Gender
# ------------------------------------------------------------
if ("Gender" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Gender = str_to_title(trimws(as.character(Gender))),
      Gender = case_when(
        Gender %in% c("M", "Male") ~ "Male",
        Gender %in% c("F", "Female") ~ "Female",
        TRUE ~ Gender
      ),
      Gender = as.factor(Gender)
    )

  print(unique(emp1$Gender))
  print(table(emp1$Gender, useNA = "ifany"))
}

# ------------------------------------------------------------
# 2.4 Clean Department
# ------------------------------------------------------------
if ("Department" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Department = str_to_title(trimws(as.character(Department))),
      Department = str_replace_all(Department, "\\.", ""),
      Department = case_when(
        Department == "It" ~ "IT",
        TRUE ~ Department
      ),
      Department = as.factor(Department)
    )

  print(unique(emp1$Department))
  print(table(emp1$Department, useNA = "ifany"))
}

# ------------------------------------------------------------
# 2.5 Clean City
# ------------------------------------------------------------
if ("City" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      City = str_to_title(trimws(as.character(City))),
      City = as.factor(City)
    )

  print(unique(emp1$City))
  print(table(emp1$City, useNA = "ifany"))
}

# ------------------------------------------------------------
# 2.6 Clean Performance as an ordered factor
# ------------------------------------------------------------
if ("Performance" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Performance = str_to_title(trimws(as.character(Performance))),
      Performance = factor(
        Performance,
        levels = c("Poor", "Average", "Good", "Very Good", "Excellent"),
        ordered = TRUE
      )
    )

  print(levels(emp1$Performance))
  print(table(emp1$Performance, useNA = "ifany"))
}

# ------------------------------------------------------------
# 2.7 Clean Age and impute missing values
# ------------------------------------------------------------
if ("Age" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Age = str_to_lower(trimws(as.character(Age))),
      Age = str_replace(Age, "^twenty six$", "26"),
      Age = suppressWarnings(as.numeric(Age)),
      Age = if_else(Age < 18 | Age > 100, NA_real_, Age)
    )

  # Median imputation; only runs if at least one valid age exists
  if (any(!is.na(emp1$Age))) {
    emp1$Age[is.na(emp1$Age)] <- median(emp1$Age, na.rm = TRUE)
  }

  summary(emp1$Age)
  sum(is.na(emp1$Age))
}

# ------------------------------------------------------------
# 2.8 Clean Salary
# ------------------------------------------------------------
if ("Salary" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Salary = str_to_upper(trimws(as.character(Salary))),
      Salary = str_replace(Salary, "K$", "000"),
      Salary = readr::parse_number(Salary),
      Salary = if_else(Salary < 0, NA_real_, Salary)
    )

  # Inspect before deciding whether 999999 is an invalid value.
  print(summary(emp1$Salary))

  # This replacement reflects the assumption in the supplied code.
  # Remove or edit this line if 999999 can be a genuine salary.
  emp1$Salary[emp1$Salary == 999999] <- NA_real_

  if (any(!is.na(emp1$Salary))) {
    emp1$Salary[is.na(emp1$Salary)] <- median(emp1$Salary, na.rm = TRUE)
  }

  summary(emp1$Salary)
  sum(is.na(emp1$Salary))
}

# ------------------------------------------------------------
# 2.9 Convert JoinDate to Date
# ------------------------------------------------------------
if ("JoinDate" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      JoinDate = parse_date_time(
        as.character(JoinDate),
        orders = c("ymd", "dmy", "mdy", "bdy"),
        quiet = TRUE
      ),
      JoinDate = as.Date(JoinDate)
    )

  summary(emp1$JoinDate)
  sum(is.na(emp1$JoinDate))
}

# ------------------------------------------------------------
# 2.10 Clean Name
# ------------------------------------------------------------
if ("Name" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(Name = str_to_title(trimws(as.character(Name))))

  unique(emp1$Name)
}

# ------------------------------------------------------------
# 2.11 Validate Email
# ------------------------------------------------------------
if ("Email" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Email = str_to_lower(trimws(as.character(Email))),
      Email = if_else(
        str_detect(
          Email,
          "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
        ),
        Email,
        NA_character_
      )
    )

  sum(is.na(emp1$Email))
  filter(emp1, is.na(Email))
}

# ------------------------------------------------------------
# 2.12 Clean Phone
# ------------------------------------------------------------
if ("Phone" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Phone = as.character(Phone),
      Phone = str_replace_all(Phone, "[^0-9]", ""),
      Phone = if_else(
        str_detect(Phone, "^[0-9]{10}$"),
        Phone,
        NA_character_
      )
    )

  sum(is.na(emp1$Phone))
  filter(emp1, is.na(Phone))
}

# ------------------------------------------------------------
# 2.13 Validate Experience
# ------------------------------------------------------------
if ("Experience" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Experience = suppressWarnings(as.numeric(Experience)),
      Experience = if_else(
        Experience < 0 | Experience > 50,
        NA_real_,
        Experience
      )
    )

  summary(emp1$Experience)
  sum(is.na(emp1$Experience))
}

# ------------------------------------------------------------
# 2.14 Remove exact duplicate rows and inspect duplicate IDs
# ------------------------------------------------------------
sum(duplicated(emp1))
emp1 <- distinct(emp1)

if ("EmployeeID" %in% names(emp1)) {
  emp1 %>%
    count(EmployeeID) %>%
    filter(n > 1)
}

# ------------------------------------------------------------
# 2.15 Missing-value analysis and visualizations
# ------------------------------------------------------------
colSums(is.na(emp1))
round(colMeans(is.na(emp1)) * 100, 2)

vis_miss(emp1)
skim(emp1)
plot_intro(emp1)
plot_missing(emp1)

# Pair plot only when the expected numeric columns exist
numeric_needed <- c("Age", "Salary", "Experience")
if (all(numeric_needed %in% names(emp1))) {
  GGally::ggpairs(
    emp1 %>% select(all_of(numeric_needed))
  )
}

# ------------------------------------------------------------
# 2.16 Outlier detection using the IQR method
# ------------------------------------------------------------
if ("Salary" %in% names(emp1)) {
  Q1 <- quantile(emp1$Salary, 0.25, na.rm = TRUE)
  Q3 <- quantile(emp1$Salary, 0.75, na.rm = TRUE)
  IQR_value <- IQR(emp1$Salary, na.rm = TRUE)

  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value

  outliers <- emp1 %>%
    filter(!is.na(Salary), Salary < lower | Salary > upper)

  print(outliers)

  boxplot(
    emp1$Salary,
    main = "Employee Salary Outliers",
    ylab = "Salary"
  )

  # OPTIONAL: cap outliers at IQR limits.
  # Use only if capping is appropriate for your analysis.
  # emp1 <- emp1 %>%
  #   mutate(Salary = pmin(pmax(Salary, lower), upper))
}

# ------------------------------------------------------------
# 2.17 Validation checks
# ------------------------------------------------------------
if ("Age" %in% names(emp1)) {
  print(filter(emp1, !is.na(Age), Age < 18 | Age > 100))
}
if ("Salary" %in% names(emp1)) {
  print(filter(emp1, !is.na(Salary), Salary < 0))
}
if ("Experience" %in% names(emp1)) {
  print(filter(emp1, !is.na(Experience), Experience < 0 | Experience > 50))
}
if ("EmployeeID" %in% names(emp1)) {
  print(emp1 %>% count(EmployeeID) %>% filter(n > 1))
}

# Validation status, if Age and Salary are available
if (all(c("Age", "Salary") %in% names(emp1))) {
  emp1 <- emp1 %>%
    mutate(
      Validation_Status = case_when(
        is.na(Age) | is.na(Salary) ~ "Missing Data",
        Age < 18 | Age > 100 ~ "Invalid Age",
        Salary < 0 ~ "Invalid Salary",
        TRUE ~ "Valid"
      )
    )

  print(table(emp1$Validation_Status))
}

# ------------------------------------------------------------
# 2.18 Data transformations
# ------------------------------------------------------------

# Standardization (z-score)
if ("Salary" %in% names(emp1)) {
  emp1$Salary_Standard <- as.numeric(scale(emp1$Salary))
}
if ("Age" %in% names(emp1)) {
  emp1$Age_Standard <- as.numeric(scale(emp1$Age))
}

# Min-max normalization
if ("Salary" %in% names(emp1) &&
    diff(range(emp1$Salary, na.rm = TRUE)) > 0) {
  emp1$Salary_Normalized <-
    (emp1$Salary - min(emp1$Salary, na.rm = TRUE)) /
    (max(emp1$Salary, na.rm = TRUE) - min(emp1$Salary, na.rm = TRUE))
}

# Log transformation
if ("Salary" %in% names(emp1)) {
  emp1$Salary_Log <- ifelse(
    !is.na(emp1$Salary) & emp1$Salary > 0,
    log(emp1$Salary),
    NA_real_
  )

  hist(
    emp1$Salary_Log,
    main = "Log-Transformed Salary",
    xlab = "Log Salary"
  )
}

# ------------------------------------------------------------
# 2.19 Feature engineering
# ------------------------------------------------------------
if ("Salary" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Salary_Category = case_when(
        is.na(Salary) ~ NA_character_,
        Salary < 30000 ~ "Low",
        Salary < 60000 ~ "Medium",
        TRUE ~ "High"
      )
    )
}

if ("Age" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Age_Group = case_when(
        is.na(Age) ~ NA_character_,
        Age < 30 ~ "Young",
        Age < 45 ~ "Middle-Aged",
        TRUE ~ "Senior"
      )
    )
}

if ("Experience" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Experience_Group = case_when(
        is.na(Experience) ~ NA_character_,
        Experience < 3 ~ "Fresher",
        Experience < 10 ~ "Mid-Level",
        TRUE ~ "Experienced"
      )
    )
}

# Tenure in years, when JoinDate is available
if ("JoinDate" %in% names(emp1)) {
  emp1 <- emp1 %>%
    mutate(
      Tenure_Years = as.numeric(difftime(Sys.Date(), JoinDate, units = "days")) / 365.25
    )
}

# Salary per year of experience; avoid division by zero
if (all(c("Salary", "Experience") %in% names(emp1))) {
  emp1 <- emp1 %>%
    mutate(
      Salary_Per_Experience = if_else(
        !is.na(Experience) & Experience > 0 & !is.na(Salary),
        Salary / Experience,
        NA_real_
      )
    )
}

# ------------------------------------------------------------
# 2.20 Correlation analysis
# ------------------------------------------------------------
if (all(c("Age", "Salary") %in% names(emp1))) {
  print(cor(
    emp1$Age, emp1$Salary,
    use = "complete.obs",
    method = "pearson"
  ))
}

if (all(c("Experience", "Salary") %in% names(emp1))) {
  print(cor(
    emp1$Experience, emp1$Salary,
    use = "complete.obs",
    method = "pearson"
  ))
}

if (all(c("Age", "Salary", "Experience") %in% names(emp1))) {
  num_data <- emp1 %>% select(Age, Salary, Experience)
  cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
  print(cor_matrix)

  # Install corrplot if needed: install.packages("corrplot")
  if (requireNamespace("corrplot", quietly = TRUE)) {
    corrplot::corrplot(
      cor_matrix,
      method = "color",
      addCoef.col = "black",
      type = "upper"
    )
  }

  plot(
    emp1$Age, emp1$Salary,
    main = "Age vs Salary",
    xlab = "Age",
    ylab = "Salary",
    pch = 19
  )

  plot(
    emp1$Experience, emp1$Salary,
    main = "Experience vs Salary",
    xlab = "Experience",
    ylab = "Salary",
    pch = 19
  )

  plot(
    emp1$Age, emp1$Experience,
    main = "Age vs Experience",
    xlab = "Age",
    ylab = "Experience",
    pch = 19
  )
}

# ------------------------------------------------------------
# 2.21 Hypothesis testing
# ------------------------------------------------------------

# Independent t-test: compare salary by gender
if (all(c("Salary", "Gender") %in% names(emp1))) {
  test_data <- emp1 %>%
    filter(!is.na(Salary), !is.na(Gender), Gender %in% c("Male", "Female"))

  if (n_distinct(test_data$Gender) == 2) {
    print(t.test(Salary ~ Gender, data = test_data))
  }
}

# Chi-square test: association between Gender and Performance
if (all(c("Gender", "Performance") %in% names(emp1))) {
  tab <- table(emp1$Gender, emp1$Performance)
  print(tab)

  if (nrow(tab) > 1 && ncol(tab) > 1) {
    print(chisq.test(tab))
  }
}

# Correlation hypothesis test: Age and Salary
if (all(c("Age", "Salary") %in% names(emp1))) {
  cor_data <- emp1 %>%
    select(Age, Salary) %>%
    drop_na()

  if (nrow(cor_data) >= 3 &&
      sd(cor_data$Age) > 0 &&
      sd(cor_data$Salary) > 0) {
    print(cor.test(cor_data$Age, cor_data$Salary, method = "pearson"))
  }
}

# ------------------------------------------------------------
# 2.22 Final checks and export cleaned data
# ------------------------------------------------------------
head(emp1)
View(emp1)
str(emp1)
summary(emp1)
skim(emp1)
dim(emp1)
names(emp1)
colSums(is.na(emp1))
sum(duplicated(emp1))

if ("EmployeeID" %in% names(emp1)) {
  sum(duplicated(emp1$EmployeeID))
}

# Export cleaned files to the current working directory.
# Check the location with getwd().
write_xlsx(emp1, "Employee_Preprocessing_Dataset_Cleaned.xlsx")
write.csv(emp1, "Employee_Preprocessing_Dataset_Cleaned.csv", row.names = FALSE)


# ============================================================
# MODULE 3: REGRESSION
# ============================================================

# ------------------------------------------------------------
# 3.1 Simple Linear Regression
# Dataset: cars
# ------------------------------------------------------------
data(cars)

head(cars)
summary(cars)

slr_model <- lm(dist ~ speed, data = cars)
summary(slr_model)
coef(slr_model)

new_data <- data.frame(speed = c(10, 15, 20))
predict(slr_model, newdata = new_data)

plot(
  cars$speed, cars$dist,
  main = "Speed vs Stopping Distance",
  xlab = "Speed",
  ylab = "Stopping Distance",
  pch = 19
)
abline(slr_model, col = "red", lwd = 2)

# In-sample regression metrics demonstration
slr_pred <- predict(slr_model, newdata = cars)
slr_actual <- cars$dist
slr_mae <- mean(abs(slr_actual - slr_pred))
slr_mse <- mean((slr_actual - slr_pred)^2)
slr_rmse <- sqrt(slr_mse)
slr_r2 <- 1 - sum((slr_actual - slr_pred)^2) /
  sum((slr_actual - mean(slr_actual))^2)

cat("SLR MAE:", slr_mae, "\n")
cat("SLR MSE:", slr_mse, "\n")
cat("SLR RMSE:", slr_rmse, "\n")
cat("SLR R-squared:", slr_r2, "\n")

# ------------------------------------------------------------
# 3.2 Multiple Linear Regression
# Dataset: mtcars
# ------------------------------------------------------------
data(mtcars)

mlr_model <- lm(mpg ~ wt + hp, data = mtcars)
summary(mlr_model)

mlr_new <- data.frame(wt = c(2.5, 3.0), hp = c(100, 150))
predict(mlr_model, newdata = mlr_new)

mlr_pred <- predict(mlr_model, newdata = mtcars)
mlr_actual <- mtcars$mpg

cat("MLR MAE:", mean(abs(mlr_actual - mlr_pred)), "\n")
cat("MLR RMSE:", sqrt(mean((mlr_actual - mlr_pred)^2)), "\n")
cat(
  "MLR R-squared:",
  1 - sum((mlr_actual - mlr_pred)^2) /
    sum((mlr_actual - mean(mlr_actual))^2),
  "\n"
)


# ============================================================
# MODULE 3: CLASSIFICATION - ALL MAIN ALGORITHMS
# Dataset: iris
# ============================================================

# Required packages: class, rpart, e1071, randomForest, gbm
# MASS is commonly installed with R.

library(class)
library(rpart)
library(e1071)
library(randomForest)
library(MASS)
library(gbm)

data(iris)
df <- na.omit(iris)

set.seed(123)
index <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))

train <- df[index, ]
test <- df[-index, ]

x_train <- train[, 1:4]
x_test <- test[, 1:4]
y_train <- train$Species
y_test <- test$Species

classification_results <- data.frame(
  Model = character(),
  Accuracy = numeric()
)

evaluate_classification <- function(actual, predicted, model_name) {
  cat("\n------------------------------\n")
  cat("Model:", model_name, "\n")
  print(table(Actual = actual, Predicted = predicted))

  acc <- mean(as.character(actual) == as.character(predicted))
  cat("Accuracy:", round(acc * 100, 2), "%\n")

  classification_results <<- rbind(
    classification_results,
    data.frame(Model = model_name, Accuracy = acc)
  )
}

# ------------------------------------------------------------
# 3.3 K-Nearest Neighbours (KNN)
# ------------------------------------------------------------
train_scaled <- scale(x_train)
test_scaled <- scale(
  x_test,
  center = attr(train_scaled, "scaled:center"),
  scale = attr(train_scaled, "scaled:scale")
)

pred_knn <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = y_train,
  k = 5
)

evaluate_classification(y_test, pred_knn, "KNN")

# ------------------------------------------------------------
# 3.4 Decision Tree
# ------------------------------------------------------------
model_tree <- rpart(Species ~ ., data = train, method = "class")
pred_tree <- predict(model_tree, newdata = test, type = "class")
evaluate_classification(y_test, pred_tree, "Decision Tree")

plot(model_tree, uniform = TRUE, margin = 0.1)
text(model_tree, use.n = TRUE, cex = 0.7)

# ------------------------------------------------------------
# 3.5 Naive Bayes
# ------------------------------------------------------------
model_nb <- naiveBayes(Species ~ ., data = train)
pred_nb <- predict(model_nb, newdata = test)
evaluate_classification(y_test, pred_nb, "Naive Bayes")

# ------------------------------------------------------------
# 3.6 Support Vector Machine (SVM)
# ------------------------------------------------------------
model_svm <- svm(Species ~ ., data = train, kernel = "radial", scale = TRUE)
pred_svm <- predict(model_svm, newdata = test)
evaluate_classification(y_test, pred_svm, "SVM")

# ------------------------------------------------------------
# 3.7 Random Forest
# ------------------------------------------------------------
set.seed(123)
model_rf <- randomForest(Species ~ ., data = train, ntree = 100)
pred_rf <- predict(model_rf, newdata = test)
evaluate_classification(y_test, pred_rf, "Random Forest")

importance(model_rf)
varImpPlot(model_rf)

# ------------------------------------------------------------
# 3.8 Linear Discriminant Analysis (LDA)
# ------------------------------------------------------------
model_lda <- MASS::lda(Species ~ ., data = train)
pred_lda <- predict(model_lda, newdata = test)$class
evaluate_classification(y_test, pred_lda, "LDA")

# ------------------------------------------------------------
# 3.9 Quadratic Discriminant Analysis (QDA)
# ------------------------------------------------------------
model_qda <- MASS::qda(Species ~ ., data = train)
pred_qda <- predict(model_qda, newdata = test)$class
evaluate_classification(y_test, pred_qda, "QDA")

# ------------------------------------------------------------
# 3.10 Logistic Regression (binary: Setosa vs not Setosa)
# ------------------------------------------------------------
train_log <- train %>%
  mutate(Is_Setosa = as.integer(Species == "setosa"))

test_log <- test %>%
  mutate(Is_Setosa = as.integer(Species == "setosa"))

model_log <- glm(
  Is_Setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = train_log,
  family = binomial
)

prob_log <- predict(model_log, newdata = test_log, type = "response")
pred_log <- ifelse(prob_log >= 0.5, 1L, 0L)
actual_log <- test_log$Is_Setosa

cat("\nLogistic Regression (binary) confusion matrix:\n")
print(table(Actual = actual_log, Predicted = pred_log))
acc_log <- mean(actual_log == pred_log)
cat("Accuracy:", round(acc_log * 100, 2), "%\n")

classification_results <- rbind(
  classification_results,
  data.frame(Model = "Logistic Regression (Binary)", Accuracy = acc_log)
)

# ------------------------------------------------------------
# 3.11 Gradient Boosting (binary: Setosa vs not Setosa)
# ------------------------------------------------------------
set.seed(123)
model_gbm <- gbm(
  Is_Setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = train_log,
  distribution = "bernoulli",
  n.trees = 100,
  interaction.depth = 2,
  shrinkage = 0.05,
  n.minobsinnode = 5,
  verbose = FALSE
)

prob_gbm <- predict(
  model_gbm,
  newdata = test_log,
  n.trees = 100,
  type = "response"
)

pred_gbm <- ifelse(prob_gbm >= 0.5, 1L, 0L)

cat("\nGradient Boosting (binary) confusion matrix:\n")
print(table(Actual = actual_log, Predicted = pred_gbm))
acc_gbm <- mean(actual_log == pred_gbm)
cat("Accuracy:", round(acc_gbm * 100, 2), "%\n")

classification_results <- rbind(
  classification_results,
  data.frame(Model = "Gradient Boosting (Binary)", Accuracy = acc_gbm)
)

# ------------------------------------------------------------
# 3.12 Compare classification model accuracy
# ------------------------------------------------------------
classification_results <- classification_results %>%
  mutate(Accuracy_Percentage = round(Accuracy * 100, 2)) %>%
  arrange(desc(Accuracy))

print(classification_results)

barplot(
  height = classification_results$Accuracy_Percentage,
  names.arg = classification_results$Model,
  las = 2,
  main = "Classification Model Accuracy",
  ylab = "Accuracy (%)",
  ylim = c(0, 100),
  cex.names = 0.7
)


# ============================================================
# MODULE 3: CLUSTERING
# ============================================================

library(cluster)
library(factoextra)

data(iris)
cluster_data <- iris[, 1:4]
cluster_scaled <- scale(cluster_data)

# ------------------------------------------------------------
# 3.13 K-means clustering
# ------------------------------------------------------------
set.seed(123)
kmeans_model <- kmeans(cluster_scaled, centers = 3, nstart = 25)

print(kmeans_model)
iris$KMeans_Cluster <- as.factor(kmeans_model$cluster)
head(iris)
table(iris$KMeans_Cluster)

fviz_cluster(kmeans_model, data = cluster_scaled, main = "K-means Clustering")

# ------------------------------------------------------------
# 3.14 Elbow method
# ------------------------------------------------------------
set.seed(123)
wss <- numeric(10)

for (k in 1:10) {
  temp_model <- kmeans(cluster_scaled, centers = k, nstart = 25)
  wss[k] <- temp_model$tot.withinss
}

plot(
  1:10, wss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method"
)

# ------------------------------------------------------------
# 3.15 Hierarchical clustering and dendrogram
# ------------------------------------------------------------
distance <- dist(cluster_scaled, method = "euclidean")
hc_model <- hclust(distance, method = "complete")

plot(
  hc_model,
  labels = FALSE,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Observations",
  ylab = "Distance"
)

hc_clusters <- cutree(hc_model, k = 3)
iris$Hierarchical_Cluster <- as.factor(hc_clusters)
table(iris$Hierarchical_Cluster)

# ------------------------------------------------------------
# 3.16 PAM clustering
# ------------------------------------------------------------
pam_model <- pam(cluster_scaled, k = 3)
print(pam_model)

iris$PAM_Cluster <- as.factor(pam_model$clustering)
table(iris$PAM_Cluster)

fviz_cluster(pam_model, data = cluster_scaled, main = "PAM Clustering")

# ------------------------------------------------------------
# 3.17 Gower distance + PAM for mixed data
# ------------------------------------------------------------
mixed_data <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
mixed_data$Size_Group <- factor(
  ifelse(
    mixed_data$Petal.Length > median(mixed_data$Petal.Length),
    "Long",
    "Short"
  )
)

gower_distance <- daisy(mixed_data, metric = "gower")
gower_pam_model <- pam(gower_distance, k = 3, diss = TRUE)

iris$Gower_PAM_Cluster <- as.factor(gower_pam_model$clustering)
table(iris$Gower_PAM_Cluster)

# Compare clusters with known species (species is not used to fit clusters)
table(Actual_Species = iris$Species, KMeans_Cluster = iris$KMeans_Cluster)
table(Actual_Species = iris$Species, Hierarchical_Cluster = iris$Hierarchical_Cluster)
table(Actual_Species = iris$Species, PAM_Cluster = iris$PAM_Cluster)


# ============================================================
# MODULE 3: ASSOCIATION RULE MINING
# ============================================================

library(arules)
library(arulesViz)

# ------------------------------------------------------------
# 3.18 Load transactions and inspect
# ------------------------------------------------------------
data("Groceries")

summary(Groceries)
inspect(Groceries[1:5])

# ------------------------------------------------------------
# 3.19 Apriori rule generation
# ------------------------------------------------------------
rules <- apriori(
  Groceries,
  parameter = list(
    support = 0.01,
    confidence = 0.30,
    minlen = 2
  )
)

cat("Total rules generated:", length(rules), "\n")
inspect(head(rules, 10))

# ------------------------------------------------------------
# 3.20 Sort rules by lift
# ------------------------------------------------------------
rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_lift, 10))
quality(head(rules_lift, 10))

# ------------------------------------------------------------
# 3.21 Visualize and export rules
# ------------------------------------------------------------
if (length(rules_lift) > 0) {
  plot(
    rules_lift[1:min(20, length(rules_lift))],
    method = "scatterplot",
    measure = c("support", "confidence"),
    shading = "lift"
  )

  write(
    rules_lift,
    file = "association_rules.csv",
    sep = ",",
    quote = TRUE,
    row.names = FALSE
  )
}


# ============================================================
# MODULE 4: TIME SERIES ANALYSIS AND FORECASTING
# ============================================================

library(forecast)

# adf.test() is from package tseries.
# Install if needed: install.packages("tseries")
if (requireNamespace("tseries", quietly = TRUE)) {
  library(tseries)
}

# ------------------------------------------------------------
# 4.1 Load and plot AirPassengers time series
# ------------------------------------------------------------
data("AirPassengers")

ts_data <- ts(
  AirPassengers,
  start = c(1949, 1),
  frequency = 12
)

print(ts_data)
summary(ts_data)

plot(
  ts_data,
  main = "Monthly Airline Passengers",
  xlab = "Year",
  ylab = "Number of Passengers",
  lwd = 2
)

# ------------------------------------------------------------
# 4.2 Moving average
# ------------------------------------------------------------
moving_average <- forecast::ma(ts_data, order = 12)

plot(
  ts_data,
  main = "Moving Average Analysis",
  xlab = "Year",
  ylab = "Passengers"
)
lines(moving_average, lwd = 2)

# ------------------------------------------------------------
# 4.3 Time-series decomposition
# ------------------------------------------------------------
decomp <- decompose(ts_data, type = "multiplicative")
plot(decomp)

# ------------------------------------------------------------
# 4.4 Stationarity test
# ------------------------------------------------------------
if (requireNamespace("tseries", quietly = TRUE)) {
  print(tseries::adf.test(ts_data))
} else {
  message("Install package 'tseries' to run the ADF test.")
}

# ------------------------------------------------------------
# 4.5 Differencing, ACF and PACF
# ------------------------------------------------------------
diff_data <- diff(ts_data)

plot(
  diff_data,
  main = "Differenced Time Series",
  xlab = "Year",
  ylab = "Change in Passengers"
)

acf(diff_data, main = "Autocorrelation Function (ACF)")
pacf(diff_data, main = "Partial Autocorrelation Function (PACF)")

# ------------------------------------------------------------
# 4.6 Train/test split and ARIMA forecast
# ------------------------------------------------------------
train_ts <- window(ts_data, end = c(1958, 12))
test_ts <- window(ts_data, start = c(1959, 1))

arima_model <- auto.arima(train_ts)
print(arima_model)
summary(arima_model)

forecast_result <- forecast(arima_model, h = length(test_ts))
plot(forecast_result, main = "Airline Passenger Forecast")

comparison <- data.frame(
  Actual = as.numeric(test_ts),
  Predicted = as.numeric(forecast_result$mean)
)
print(comparison)

actual <- as.numeric(test_ts)
predicted <- as.numeric(forecast_result$mean)

forecast_mae <- mean(abs(actual - predicted))
forecast_rmse <- sqrt(mean((actual - predicted)^2))

cat("Forecast MAE:", round(forecast_mae, 2), "\n")
cat("Forecast RMSE:", round(forecast_rmse, 2), "\n")

# ------------------------------------------------------------
# 4.7 Forecast the next 12 months using the full series
# ------------------------------------------------------------
final_arima_model <- auto.arima(ts_data)
future_forecast <- forecast(final_arima_model, h = 12)

plot(future_forecast, main = "Next 12 Months Passenger Forecast")
print(future_forecast)


# ============================================================
# FINAL SECTION: SAVE OUTPUTS AND CHECK WORKING DIRECTORY
# ============================================================

cat("\nCurrent working directory:\n")
print(getwd())

cat("\nCompleted sections in this file:\n")
cat("Module 1: R foundations and basic EDA\n")
cat("Data Wrangling: dplyr, tidyr, joins and missing values\n")
cat("Module 2: Employee preprocessing, outliers, transformations,\n")
cat("          feature engineering, correlation and hypothesis tests\n")
cat("Module 3: Regression, classification, clustering and association rules\n")
cat("Module 4: Time series analysis and forecasting\n")

# Optional: export clustering results
write.csv(iris, "iris_clustering_results.csv", row.names = FALSE)

# END OF FILE
