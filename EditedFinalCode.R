data = read.csv("C:/Users/rebei/Downloads/SHARIS(in).csv")
#dimension
dim(data)
#checking for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Identify duplicate rows
duplicates <- data[duplicated(data), ]
print(duplicates)
# Step 1: Make column names unique
colnames(data) <- make.unique(colnames(data))

# Step 2: Load required libraries
library(tidyr)
library(dplyr)

# Step 3: Split 'admittime' into 'admit_date' and 'admit_time'
data <- data %>%
  separate(admittime, into = c("admit_date", "admit_time"), sep = " ")

# Step 4: Split 'dischtime' into 'discharge_date' and 'discharge_time'
data <- data %>%
  separate(dischtime, into = c("discharge_date", "discharge_time"), sep = " ")

# Step 5: View the updated dataset
head(data)
library(lubridate)
# Convert 'admit_date' to date-time format (if not already)
data$admit_date <- mdy(data$admit_date)
data$discharge_date <- mdy(data$discharge_date)
# Extract the month and create a new column 'admit_month'
data <- data %>%
  mutate(admit_month = month(admit_date))
data <- data%>%
  mutate(discharge_month= month(discharge_date))
# View the updated data
head(data)

# Add a column 'age' with random values between 20 and 85
set.seed(123)  # Set seed for reproducibility
data$age <- sample(20:85, size = nrow(data), replace = TRUE)



# Rename columns
data <- data %>%
  rename(
    intervention = sectionheader,
    care = subsectionheader,
    wards = costcenter,
    discharge_year = discharge_month,
    admit_year= admit_month
  )

# Drop the specified columns
data <- data[, !names(data) %in% c("icustay_id", "itemid", "value", "drg_code","admit_date","discharge_date")]

# View updated column names
print(colnames(data))
-----------------------------------


# Grouped bar plot for admission type and discharge location
ggplot(data, aes(x = admission_type, fill = discharge_location)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Grouped Bar Plot of Admission Type vs. Discharge Location",
       x = "Admission Type", 
       y = "Count of Patients", 
       fill = "Discharge Location") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Bar plot for admission type vs. insurance
ggplot(data, aes(x = insurance, fill = admission_type)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Bar Plot of Insurance Type vs. Admission Type",
       x = "Insurance Type", 
       y = "Count of Patients",
       fill = "Admission Type") +
  theme_minimal()

# Create the grouped bar plot
ggplot(data, aes(x = insurance, fill = admission_type)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Grouped Bar Plot of Admission Type vs. Insurance Type",
       x = "Admission Type", 
       y = "Count of Patients", 
       fill = "Insurance Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Density plot for age
ggplot(data, aes(x = age)) + 
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Density Plot of Age", x = "Age", y = "Density") +
  theme_minimal()
-------
  
library(lubridate)


# Create the correlation matrix
cor_matrix <- cor(data[, c("age", "admit_year", "discharge_year")])

# Display the correlation matrix
print(cor_matrix)

# Linear regression
model2 <- lm(admit_year ~ age, data = data)
summary(model2)
print(model2)
# Scatter plot with regression line
ggplot(data, aes(x = age, y = admit_year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Age vs. Admission Year with Linear Regression",
       x = "Age", y = "Admission Year") +
  theme_minimal()

------------------------
# Install and load necessary libraries
# install.packages("wordcloud")
# install.packages("tm")
library(wordcloud)
library(tm)

# Example: Create a word cloud from the 'description of diagnosis' column
text_data <- tolower(data$description.y)  # Convert text to lowercase
text_data <- Corpus(VectorSource(text_data))  # Create a text corpus
text_data <- tm_map(text_data, removePunctuation)  # Remove punctuation
text_data <- tm_map(text_data, removeNumbers)  # Remove numbers
text_data <- tm_map(text_data, removeWords, stopwords("en"))  # Remove common stopwords

# Create the word cloud
wordcloud(words = unlist(strsplit(as.character(text_data), " ")), 
          min.freq = 10, 
          scale = c(3, 0.2), 
          colors = brewer.pal(8, "Dark2"))

-----------------------------------

# Linear regression
model2 <- lm(discharge_year ~ admit_year, data = data)
summary(model2)

# Scatter plot with regression line
ggplot(data, aes(x = admit_year, y = discharge_year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Admission vs. Discharge Year with Linear Regression",
       x = "Admission year", y = "Discharge Year") +
  theme_minimal()
--------------------------------------
#boxplot

ggplot(data, aes(x = intervention, y = age, fill = intervention)) +
  geom_boxplot() +
  labs(title = "Age Distribution Across Intervention Types",
       x = "Intervention",
       y = "Age") +
  theme_minimal()


# Boxplot: Age vs Insurance
ggplot(data, aes(x = insurance, y = age, fill = insurance)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Insurance Type",
       x = "Insurance Type",
       y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
----------------------------
# Scatter plot: Admission Year vs. Age
ggplot(data, aes(x = as.factor(admit_year), y = age)) +
  geom_point(aes(color = admit_year), alpha = 0.5) +
  labs(title = "Scatter Plot: Admission Year vs. Age",
       x = "Admission Year",
       y = "Age") +
  theme_minimal()
library(ggplot2)

ggplot(data, aes(x = age, fill = intervention)) +
  geom_bar(position = "stack") +
  labs(title = "Interventions by Age Group", x = "Age", y = "Count") +
  theme_minimal()
--------------------------------

# Step 1: Create a Binary Target Variable
data$admission_flag <- ifelse(data$admit_year > median(data$admit_year, na.rm = TRUE), 1, 0)

# Step 2: Build the Logistic Regression Model
logistic_model <- glm(admission_flag ~ age, data = data, family = binomial)

# Step 3: Print Summary of the Model
summary(logistic_model)

# Step 4: Predict Probabilities
data$predicted_prob <- predict(logistic_model, type = "response")

# Step 5: Model Evaluation
# Threshold of 0.5 for binary classification
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Confusion Matrix
confusion_matrix <- table(Predicted = data$predicted_class, Actual = data$admission_flag)
print("Confusion Matrix:")
print(confusion_matrix)

# Step 6: Visualization of Logistic Regression
ggplot(data, aes(x = age, y = admission_flag)) +
  geom_jitter(height = 0.05, width = 0, color = "blue", alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red", se = FALSE) +
  labs(title = "Logistic Regression: Admission Year Flag vs Age",
       x = "Age", y = "Admission Flag (1 = High Year, 0 = Low Year)") +
  theme_minimal()


