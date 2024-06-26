# Obesity Health Data Visualisaton

## Introduction
The purpose of this project is to investigate the relationships between obesity levels and specific lifestyle factors including physical activity, smoking status, and alcohol consumption. By understanding these relationships, we can gain insights into potential risk factors and develop targeted interventions for obesity prevention and management.

## Data Cleaning and Preparation
The dataset was cleaned to ensure data consistency and quality. This included:

- Correcting data type mismatches for categorical variables
- Mapping decimal factors to the nearest category for variables like FCVC, NCP, CH2O, FAF, and TUE
- Rounding weight and height values to two decimal places
- Creating a new dataframe with cleaned variables for visualisation

## Data Exploration
The dataset was explored to identify outliers and understand the distribution of variables. Key findings include:

- Outliers were detected in the age variable (168 instances) and weight variable (1 instance)
- The dataset is skewed by age, with 1,827 of the total 2,111 participants being within the ages of 18-35 years

## Data Visualisation

**Demographic Summary Table**
A demographic summary table was generated to provide an overview of the dataset, stratified by gender. The table includes mean, standard deviation, median, and interquartile range for continuous variables, and counts and percentages for categorical variables.

**Physical Activity Across Obesity Types**
A bar chart was created to compare the frequency of physical activity across different obesity levels (I, II, and III). The chart shows the distribution of individuals engaging in physical activity for various durations (none, 1-2 days, 2-3 days, and 4-5 days) within each obesity level.

![image](https://github.com/PliciousG/obesity_health_data_visualisaton/blob/main/Images/vis_1.png)

**Smoking Status Across Obesity Types**
A bar chart was generated to visualise the distribution of smokers and non-smokers within each obesity category.

![image](https://github.com/PliciousG/obesity_health_data_visualisaton/blob/main/Images/vis_2.png)


**Alcohol Consumption Across Obesity Types**
An interactive plot was created using ggplot and plotly to display the distribution of alcohol consumption (no, sometimes, frequently, and always) across different obesity levels.

![image](https://github.com/PliciousG/obesity_health_data_visualisaton/blob/main/Images/vis_3.png)

## Summary of Findings

The charts reveals a trend where higher levels of obesity are associated with lower levels of physical activity and higher alcohol consumption. The chart on smoking status revealed a significant disproportion in data which would require further clarification.

## Implications for Policy and Public Health

The observed trends in physical activity and alcohol consumption reveals potential areas for public health intervention. The inverse relationship between physical activity and obesity types suggests a need for policies that promote physical activity, especially for individuals at higher obesity levels, and as a preventive measure for individuals in overweight categories. Likewise, alcohol reduction interventions could be beneficial considering the positive association between alcohol consumption and obesity levels.

## Conclusion
This project provides valuable insights into the relationships between obesity levels and lifestyle factors such as physical activity, smoking status, and alcohol consumption. The visualizations can help inform targeted interventions and public health strategies to address obesity and its associated risk factors.
