#  load package 

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# load the data 
Data_5Q <- read_excel("//tsclient/code files/Data_5Q.xlsx")

# Filter data for chronic ischaemic heart disease

chronic_ihd_data <- Data_5Q %>% 
  filter(Condition == "Chronic Ischaemic Heart Disease")
ls(chronic_ihd_data)
# Check the structure of the data
str(chronic_ihd_data$`Age-Standardised Rate Per 100,000 Person Years`)

# Remove commas or other non-numeric characters, if present
chronic_ihd_data$`Age-Standardised Rate Per 100,000 Person Years` <- 
  gsub(",", "", chronic_ihd_data$`Age-Standardised Rate Per 100,000 Person Years`)
# Convert to numeric
chronic_ihd_data$`Age-Standardised Rate Per 100,000 Person Years` <- 
  as.numeric(chronic_ihd_data$`Age-Standardised Rate Per 100,000 Person Years`)

# Group by Index of Multiple Deprivation Quintile and summarize the Age-Standardised Rate
deprivation_analysis <- chronic_ihd_data %>%
  group_by(`Index Of Multiple Deprivation Quintile`) %>%
  summarize(avg_age_std_rate = mean(`Age-Standardised Rate Per 100,000 Person Years`, na.rm = TRUE))

# View the result
print(deprivation_analysis)

# graph visualazation 

ggplot(deprivation_analysis, aes(x = `Index Of Multiple Deprivation Quintile`, y = avg_age_std_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Average Age-Standardised Mortality Rate ofChronic Ischaemic Heart Disease by Deprivation Quintile",
       x = "Index of Multiple Deprivation Quintile",
       y = "Average Age-Standardised Rate (per 100,000 Person Years)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by Ethnic Group and summarize the Age-Standardised Rate

ethnicity_analysis <- chronic_ihd_data %>%
  group_by(`Ethnic Group`) %>%
  summarize(avg_age_std_rate = mean(`Age-Standardised Rate Per 100,000 Person Years`, na.rm = TRUE))

# Check the results
print(ethnicity_analysis)

# Plot the results ethinicity analysis

ggplot(ethnicity_analysis, aes(x = `Ethnic Group`, y = avg_age_std_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Age-Standardised Mortality Rate of Chronic Ischaemic Heart Disease by Ethnic Group",
       x = "Ethnic Group",
       y = "Average Age-Standardised Rate per 100,000 Person Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
