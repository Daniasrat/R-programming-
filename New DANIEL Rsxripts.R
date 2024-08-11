packages <- c("dplyr", "janitor", "ggplot2")
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

install.packages("data.table")
library(data.table)

# Load temperature data and hospital admission record data files

load("//tsclient/code files/merged_hospital_recored_Data .RData")
load("//tsclient/code files/filtered_hospital_dmission_data .RData")
load("//tsclient/code files/temperature_Data .RData")

# Viualzation data 

View(filtered_hospital_admission_data)
View(merged_hospital_recored_Data)
View(temperature_Data)

# summary of filtered_hospital_admission data and temperature_ data based admission_Date column

summary(filtered_hospital_admission_data$Admission_Date)
summary(merged_hospital_recored_Data$Admission_Date)
summary(temperature_Data$Admission_Date)

# verify the type of data in the admission date columns
str(filtered_hospital_admission_data$Admission_Date)
str(merged_hospital_recored_Data$Admission_Date)
str(temperature_Data$Admission_Date)

# verify the data columns of  munipality residence 
str(filtered_hospital_admission_data$Municipality_Residence_4)
str(merged_hospital_recored_Data$Municipality_Residence_4)
str(temperature_Data$Municipality_Residence_4)

# list of the two data files 
ls(merged_hospital_recored_Data)
ls(filtered_hospital_admission_data)
ls(temperature_Data)

# Merge the filtered_mergedd hospital data and temperature data files using left joint 
filtered_merged_final_data <- left_join(filtered_hospital_admission_data, temperature_Data, 
                               by = c("Municipality_Residence_4", "Admission_Date"))

unfiltered_meregd_final_data <- left_join(merged_hospital_recored_Data, temperature_Data,
                                by =c("Municipality_Residence_4","Admission_Date"))
ls(unfiltered_meregd_final_data)

# Remove the specified unwanted columns

unfiltered_final_data <- unfiltered_meregd_final_data %>%
  select(-Health_Establishment_Code, -Hospital_cost, -ICU_cost, -ICU_Months, 
         -Professional_cost, -province, -State, -Municipality_Movement, 
         -Municipality_Name, -Municipality_Residence)

# View the unfiltered data
str(unfiltered_final_data)
ls(unfiltered_final_data)

unfiltered_final_data1 <- unfiltered_final_data %>%
  filter(Admission_Date >= as.IDate("2008-01-01") & Admission_Date <= as.IDate("2019-12-31"))


# Calculate the percentage of missing values in each column
missing_data_percentage <- colMeans(is.na(unfiltered_final_data1)) * 100

# Display the percentage of missing values
missing_data_percentage

save(unfiltered_final_data1 , file = "unfiltered_final_data1 .RData")

summary(unfiltered_final_data1$Gender)
# 1=male and 3= Female


#  addressing the missing value using multiple imputation

# Install the mice package if you haven't already
install.packages("mice")

library(mice)
imputed_data <- mice(unfiltered_final_data[, c("Max_Temperature", "Year", "Month")], 
                     m = 5, method = 'pmm', seed = 500)
# Choose one imputed dataset for analysis, or combine results from all imputations
unfiltered_final_data <- complete(imputed_data, 1)

# Mode imputation for Year and Month
mode_imputation <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

unfiltered_final_data$Year[is.na(unfiltered_final_data$Year)] <- 
  mode_imputation(unfiltered_final_data$Year)

unfiltered_final_data$Month[is.na(unfiltered_final_data$Month)] <- 
  mode_imputation(unfiltered_final_data$Month)

unfiltered_final_data$Max_Temperature[is.na(unfiltered_final_data$Max_Temperature)] <- 
  mode_imputation(unfiltered_final_data$Max_Temperature)

save(unfiltered_final_data , file = "unfiltered_final_data .RData")


# Define the ICD-10 codes for CVD
cvd_icd_codes <- c(
  "I00", "I01", "I02", "I05", "I06", "I07", "I08", "I09",
  "I10", "I11", "I12", "I13", "I14", "I15",
  "I20", "I21", "I22", "I23", "I24", "I25",
  "I26", "I27", "I28",
  "I30", "I31", "I32", "I33", "I34", "I35", "I36", "I37", "I38", "I39",
  "I40", "I41", "I42", "I43", "I44", "I45", "I46", "I47", "I48", "I49", "I50", "I51", "I52",
  "I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69",
  "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79",
  "I80", "I81", "I82", "I83", "I84", "I85", "I86", "I87", "I88", "I89",
  "I95", "I96", "I97", "I98", "I99"
)

# Filter the data for CVD Disease hospitalizations

cvd_admissions <- unfiltered_final_data1 %>%
  filter(substr(Primary_Diagnosis, 1, 3) %in% cvd_icd_codes)

# Verify the filtered data

summary(cvd_admissions)
str(cvd_admissions)

# Function to categorize CVDS based on the Disease
cvd_admissions_final <- cvd_admissions %>%
  mutate(CVDs_Category = case_when(
    Primary_Diagnosis %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Ischaemic Heart Diseases",
    Primary_Diagnosis %in% c("I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69") ~ "Cerebrovascular Diseases",
    Primary_Diagnosis %in% c("I50", "I42", "I43", "I44", "I45", "I46", "I47", "I48", "I49") ~ "Heart Failure and Cardiomyopathies",
    Primary_Diagnosis %in% c("I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79",
                                        "I80", "I81", "I82", "I83", "I84", "I85", "I86", "I87", "I88", "I89") ~ "Peripheral and Other Vascular Diseases",
    TRUE ~ "Other CVDs"
  ))

# Create a new categorical column from the continuous Age column
cvd_admissions_final1 <- cvd_admissions_final %>%
  mutate(Age_Category = cut(Age, 
                            breaks = c(-Inf, 4, 14, 24, 44, 64, 74, Inf), 
                            labels = c("0-4", "5-14", "15-24", 
                                       "25-44", "45-64", "65-74s", "75+"),
                            right = TRUE))  # right=TRUE means intervals are closed on the right and open on the left

#  save the data set

save(cvd_admissions_final1 , file = "cvd_admissions_final1 .RData")


# Calculate the percentage of missing values in each column
missing_data_percentage <- colMeans(is.na(cvd_admissions_final1)) * 100

# Display the percentage of missing values
missing_data_percentage





