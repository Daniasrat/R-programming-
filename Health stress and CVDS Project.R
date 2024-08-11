
# Loading the packages 

packages <- c("dplyr", "janitor", "ggplot2")
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

install.packages("data.table")
library(data.table)
       
install.packages("dplyr")
library(dplyr)

# Load final merged data files

load("C:/Users/DanielAsrat/Desktop/data project/code files/cvd_admissions_final1 .RData")

# Viualzation data 
View(cvd_admissions_final1)

#  verify the gender data
summary(cvd_admissions_final1$Gender)

# Filter the data to remove unwanted gender values

cvd_admissions_final1_filtered <- cvd_admissions_final1 %>%
  filter(Gender %in% c(1, 3))

# Update the Gender column labels

cvd_admissions_final1_filtered$Gender <- recode(cvd_admissions_final1_filtered$Gender,
                                                `1` = "Male",
                                                `3` = "Female")
# Verify the changes
summary(cvd_admissions_final1_filtered$Gender)

save(cvd_admissions_final1_filtered , file = "cvd_admissions_final1_filtered .RData")










