title: "How Do the Levels of Physical Activity, Smoking Status, and Alcohol Consumption Relate to Different Levels of Obesity?"
output:
  html_document:
  toc: true
toc_float: true
theme: cerulean
highlight: kate

# loading libaries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(gt)
library(gtsummary)
library(kableExtra)

# reading the dataset
obesity_data <- read_excel("C:\\Users\\preci\\Downloads\\Obesity_Data.xlsx")

# to get a comprehensive overview of the data
# to inspect the data structure and identify the data types
glimpse(obesity_data)
str(obesity_data)
summary(obesity_data)

# data types of categorical variables like gender and daily meals are mismatched (in character format)

# to get a better view of the data across different segments
head(obesity_data)
tail(obesity_data)
sample_n(obesity_data, 10)

# data inconsistencies were observed in variables- Age, Height, Weight, and categorical variables- FCVC, NCP, CH2O, FAF, and TUE

# assessing data completeness

colSums(is.na(obesity_data))
sum(is.na(obesity_data))

# there were no missing data

# checking for duplicate entries 
sum(duplicated(obesity_data))

# also no duplicates

# initial data cleaning

# copying the original dataframe to retain the original data
data<- obesity_data


# correcting data type mismatch by converting categorical variables from character to factors

data$Gender <- as.factor(data$Gender)
data$family_history_with_overweight <- as.factor(data$family_history_with_overweight)
data$FAVC <- as.factor(data$FAVC)
data$CAEC <- as.factor(data$CAEC)
data$SMOKE <- as.factor(data$SMOKE)
data$SCC <- as.factor(data$SCC)
data$CALC <- as.factor(data$CALC)
data$MTRANS <- as.factor(data$MTRANS)
data$NObeyesdad <- factor(data$NObeyesdad,
                          levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

# to correct the data type for the variables FCVC, NCP, CH2O, FAF, and TUE, the decimal factors within these variables will first be mapped to the nearest category to ensure they are not treated as distinct levels of the factor


# Creating a function to map to nearest category
map_to_nearest_category <- function(x, categories) {
  sapply(x, function(value) {
    # Find which category is closest to the current value
    closest_category <- categories[which.min(abs(categories - value))]
    return(closest_category)
  })
}

# defining the intended categories for FCVC
intended_categories_FCVC <- c(1, 2, 3)

# applying the mapping
data$FCVC_mapped <- map_to_nearest_category(data$FCVC, intended_categories_FCVC)

# converting the mapped variable to a factor
data$FCVC_mapped <- as.factor(data$FCVC_mapped)

print(data$FCVC_mapped)
print(unique(data$FCVC_mapped))


# converting the others
# correcting NCP data type
intended_categories_NCP <- c(1, 2, 3)

data$NCP_mapped <- map_to_nearest_category(data$NCP, intended_categories_NCP)

data$NCP_mapped <- as.factor(data$NCP_mapped)

print(unique(data$NCP_mapped))

# correcting CH2O data type
intended_categories_CH2O <- c(1, 2, 3)

data$CH2O_mapped <- map_to_nearest_category(data$CH2O, intended_categories_CH2O)

data$CH2O_mapped <- as.factor(data$CH2O_mapped)

print(unique(data$CH2O_mapped))

# correcting FAF data type
intended_categories_FAF <- c(0, 1, 2, 3)

data$FAF_mapped <- map_to_nearest_category(data$FAF, intended_categories_FAF)

data$FAF_mapped <- as.factor(data$FAF_mapped)

print(unique(data$FAF_mapped))

# correcting TUE data type
intended_categories_TUE <- c(0, 1, 2)

data$TUE_mapped <- map_to_nearest_category(data$TUE, intended_categories_TUE)

data$TUE_mapped <- as.factor(data$TUE_mapped)

print(unique(data$NCP_mapped))

# cleaning weight and height by rounding up to two decimal values
data$New_Weight <- round(data$Weight, digits = 2)
print(data$New_Weight)

data$New_Height <- round(data$Height, digits = 2)
print(data$New_Height)


# creating a neww dataframe with cleaned variables for easy viaualisation

new_data<- data %>%
  select(Gender, Age, New_Height, New_Weight, family_history_with_overweight, FAVC, FCVC_mapped, NCP_mapped, CAEC, SMOKE, CH2O_mapped, SCC, FAF_mapped, TUE_mapped, CALC, MTRANS, NObeyesdad)

str(new_data)


# cleaning weight and height by rounding up to two decimal values
data$New_Weight <- round(data$Weight, digits = 2)
print(data$New_Weight)

data$New_Height <- round(data$Height, digits = 2)
print(data$New_Height)


# creating a neww dataframe with cleaned variables for easy viaualisation

new_data<- data %>%
  select(Gender, Age, New_Height, New_Weight, family_history_with_overweight, FAVC, FCVC_mapped, NCP_mapped, CAEC, SMOKE, CH2O_mapped, SCC, FAF_mapped, TUE_mapped, CALC, MTRANS, NObeyesdad)

str(new_data)


# FURTHER EXPLORATION

# evaluating for outliers
# creating a function to spot outliers
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x < lower | x > upper
}

# applying the function to the numerical columns in the dataset
outliers <- new_data %>%
  select_if(is.numeric) %>%
  sapply(detect_outliers)


sum_outliers <- colSums(outliers)
print(sum_outliers)

# outliers are present in age (168), New_Weight (1)

# visualising the outliers in age
ggplot(new_data, aes(y = Age)) +
  geom_boxplot(fill = "grey", color = "black") +
  theme_minimal() +
  ggtitle("Box Plot of Age")


# visualisng age distribution to better understand the outliers

# categorising age into age_bins to improve consistency and reduce the complexity of the data for visualisation

new_data$age_group <- cut(new_data$Age,
                          breaks = c(-Inf, 18, 25, 35, 45, 55, 65, Inf),
                          labels = c("Under 18", "18-25", "26-35", "36-45", "46-55", "56-65", "Over 65"),
                          right = FALSE)

# plotting age groups to visualise the distribution

age_group_counts <- new_data %>%
  group_by(age_group) %>%
  summarise(count = n())

ggplot(age_group_counts, aes(x = age_group, y = count)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Number of Individuals by Age Group",
       x = "Age Group",
       y = "Number of Individuals") + 
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# the dataset is skwed by age, 1827 of the total 2111 participants are within the ages of 18-35 years.


# visualising the outlier in weight
# boxplot of weight by gender with outliers highlighted in blue

ggplot(new_data, aes(x = Gender, y = New_Weight)) + 
  geom_boxplot(fill= "grey", outlier.color = "blue", outlier.shape = 8) +
  theme_minimal() +
  ggtitle("Box Plot of Weight")

max(new_data$New_Weight)

# this outlier will be ignored considering it is a single outlier and unlikely to have a significant effect on the dataset




***
  
  ## Data Visualisations
  
  ***
  
  **Table 1. Demographic Summary Table**
  
  
  # creating a demographic summary table
  summary_table <- new_data %>%
  select(-age_group) %>%
  rename(
    Height = New_Height, 
    Weight = New_Weight,
    'Family History of Overweight' = family_history_with_overweight, 
    FAVC= FAVC,
    FCVC= FCVC_mapped, 
    NCP= NCP_mapped, 
    CH2O = CH2O_mapped, 
    FAF= FAF_mapped, 
    TUE= TUE_mapped, 
    'Obesity Types'= NObeyesdad
  ) %>%
  tbl_summary(
    by = 'Gender', # stratified by gender for easy comparison and to provide more detailed understanding of the dataset  
    type = list(
      all_continuous() ~ "continuous2",
      all_categorical() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median} [{p25}, {p75}]"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany"
  ) %>%
  add_n() %>% 
  add_p() %>%
  bold_labels() %>% 
  modify_header(label = "**Characteristics**")

# Converting and styling with kableExtra to improve accessibility
kable_table <- summary_table %>%
  as_kable() %>% 
  kable_styling(
    bootstrap_options = c("striped", "condensed", "hover"),
    font_size = 16,
    full_width = F
  ) %>%
  kableExtra::scroll_box(width = "100%", height = "500px")

kable_table

```
FAVC (High Caloric Food Consumption, no/yes), FCVC (Vegetable Intake, scale: Never (1), Sometimes (2), Always(3)), NCP (Main Meals Daily, scale: between 1 and 2(1), three(2), more than three(3)), CAEC (Snacking between meals), CH2O (Water Consumption Daily, scale: <1L(1), 1-2L(2), >2L(3)), SCC (Calorie Monitoring), FAF (Physical Activity Frequency, scale: none (0) ,1 or 2 days (1), 2 or 3 days (2), 4 or 5 days (3)), TUE (Tech Use Duration, 0-2 hours (0), 3-5 hours (1), more than 5 hours (2)), CALC (Alcohol Consumption), MTRANS (Usual Transport Method), NObeyesdad (Obesity Categories).
```



# for a more detailed summary
# creating a demographic summary table by level of obesity
table_demographic <- new_data %>%
  select(Age, New_Height, New_Weight, Gender, NObeyesdad) %>%
  group_by(NObeyesdad) %>%  # stratified by obesity level for easy comparison and to provide more detailed understanding of the dataset  
  tbl_summary(
    by = NObeyesdad, 
    type = all_continuous() ~ "continuous2",
    statistic =
      all_continuous() ~ c("{mean} ({sd})", "{median}, ({p25}, {p75})"),
    digits = all_continuous() ~ 2
  ) %>%
  add_p() %>%
  add_n() %>%
  modify_header(label = "**Demographic**") %>%
  bold_labels()

# Converting and styling with kableExtra to improve accessibility
kable_table <- table_demographic %>%
  as_kable() %>% 
  kable_styling(
    bootstrap_options = c("striped", "condensed", "hover"),
    font_size = 16,
    full_width = F
  ) %>%
  kableExtra::scroll_box(width = "100%", height = "500px")

kable_table



***
  #### Visualisation 2:Physical Activity Across Obesity Types
  
  ***
  
  # filtering the dataset by obesity levels I, II, and III
  data_filtered <- subset(new_data, NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

# applying labels to the physical activity levels for potting
data_filtered$FAF_mapped <- factor(data_filtered$FAF_mapped, 
                                   levels = c(0, 1, 2, 3),
                                   labels = c("None", "1-2 days", "2-3 days", "4-5 days"))

# improving readability of obesity levels
data_filtered$NObeyesdad <- factor(data_filtered$NObeyesdad,
                                   levels = c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"),
                                   labels = c("Obesity Level I", "Obesity Level II", "Obesity Level III"))


# plotting a bar chart to show the distribution of physical activities across obesity levels 
ggplot(data_filtered, aes(x = FAF_mapped)) +
  geom_bar(fill = "#1F4E79") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -1, color = "black") +
  facet_wrap(~ NObeyesdad) +
  labs(title = "Comparison of Physical Activity Across Obesity Levels",
       x = "Frequency of Physical Activity",
       y = "Total Individuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none")


***
  
  #### Visualisation 3: Smoking Status Across Obesity Types
  
  ***
  
  
  # plotting smoking status by obesity levels
  data_filtered$SMOKE <- factor(data_filtered$SMOKE, labels = c("No", "Yes"))

ggplot(data_filtered, aes(x = NObeyesdad, fill = SMOKE)) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = after_stat(count)),  position = position_dodge(width = 0.9), vjust = -0.25, size = 3.5, color = "black") +
  labs(title = "Comparison of Smoking Status Across Obesity Categories",
       subtitle = "Visualising the distribution of smokers and non-smokers within each obesity category",
       x = "Obesity Levels",
       y = "Total Individuals") +
  scale_fill_manual(values = c("No" = "darkgrey", "Yes" = "#1F4E79"), name = "Smoking Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12))



***
  
  #### Visualisation 4: Alcohol Consumpution Across Obesity Types
  
  ***
  
  # creating an interactive plot with ggplot and plotly
  
  data_filtered$CALC <- factor(data_filtered$CALC, 
                               levels = c("no", "Sometimes", "Frequently", "Always"),
                               labels = c("no", "Sometimes", "Frequently", "Always"))

p <- ggplot(data_filtered, aes(x = NObeyesdad, fill = CALC)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("no"= "darkgrey", "Sometimes"= "#1F4E79", "Frequently" = "brown", "Always"= "lightblue"),name = "Alcohol Consumption") + 
  labs(title = "Alcohol Consumption Across Obesity Categories",
       x = "Obesity Levels",
       y = "Total Individuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))

ggplotly(p)

