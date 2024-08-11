
packages <- c("dplyr", "janitor", "ggplot2")
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})


# Load temperature data and hospital admission record data files

load("C:/Users/DanielAsrat/Desktop/data project/code files/Admission_PB.RData")
load("C:/Users/DanielAsrat/Desktop/data project/code files/Admission_SP.RData")
load("C:/Users/DanielAsrat/Desktop/data project/code files/tmax_city.RData")

# Summary and structure of temperature data
head(tmax_obs_city)
summery(tmax_obs_city)s

# Assuming 'df_SP' and 'df_PB' are the names of the variables in the .RData files
# Summarize and view hospital admission data
head(df_PB)
head(df_SP)
str(df_PB)
str(df_SP)

# Merge the two hospital admission data

merged_df<- rbind(df_SP, df_PB)

# Rename columns in the hospital admission merged data

merged_hospital_recored_Data <- merged_df %>%
  rename(
    State = UF_ZI,
    Postal_Code = CEP,
    Municipality_Residence_4 = MUNIC_RES,
    Birth_Date = NASC,
    Gender = SEXO,
    ICU_Months = UTI_MES_TO,
    Hospital_cost = VAL_SH,
    Professional_cost = VAL_SP,
    Total_cost = VAL_TOT,
    ICU_cost = VAL_UTI,
    Admission_Date = DT_INTER,
    Discharge_Date = DT_SAIDA,
    Primary_Diagnosis = DIAG_PRINC,
    Municipality_Movement = MUNIC_MOV,
    Age = IDADE,
    Days_Stay = DIAS_PERM,
    Death = MORTE,
    Health_Establishment_Code = CNES
  )


# Rename columns in the temperature data

temperature_Data <- tmax_obs_city %>%
  rename(
    province = SIGLA_UF,
    Municipality_Residence = CD_MUN,
    Municipality_Name = NM_MUN,
    Max_Temperature = MAX_TEMP,
    Admission_Date = DATE,
    Year = YEAR,
    Month = MONTH
  )

# save the merged hospital record data and temperature data
save(merged_hospital_recored_Data , file = "merged_hospital_recored_Data .RData")
save(temperature_Data , file = "temperature_Data .RData")

# list of the rename data files 

ls(merged_hospital_recored_Data)
ls(temperature_Data)

# Install and load the data.table package
install.packages("data.table")
library(data.table)

# Convert factor to character merged_hospital_recored_Data the column of admission date

merged_hospital_recored_Data$Admission_Date <- as.character(merged_hospital_recored_Data$Admission_Date)

# Display the first few rows to inspect the date format
head(merged_hospital_recored_Data$Admission_Date)

# Convert Admission_Date to Date object with appropriate format string

merged_hospital_recored_Data$Admission_Date <- as.Date(merged_hospital_recored_Data$Admission_Date, format = "%Y%m%d")

# Convert Admission_Date to IDate
merged_hospital_recored_Data$Admission_Date <- as.IDate(merged_hospital_recored_Data$Admission_Date)

# Display the result
head(merged_hospital_recored_Data)

 # Checking the matching of two data set admission date

str(merged_hospital_recored_Data$Admission_Date)
str(temperature_Data$Admission_Date)

# summary of admission data of both data

summary(merged_hospital_recored_Data$Admission_Date)
summary(temperature_Data$Admission_Date)

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Filter the Hospital merged data based on the date range "2008-01-01" and ("2019-12-31)

filtered_hospital_admission_data <- merged_hospital_recored_Data %>%
  filter(Admission_Date >= as.IDate("2008-01-01") & Admission_Date <= as.IDate("2019-12-31"))


# summary of filtered_hospital_admission data and temperature_ data based admission_Date column

summary(filtered_hospital_admission_data$Admission_Date)
summary(merged_hospital_recored_Data$Admission_Date)
summary(temperature_Data$Admission_Date)

# list of the two data files 
lst(merged_hospital_recored_Data)
ls(filtered_hospital_admission_data)
ls(temperature_Data)

# Municipality_Residence checking the matching for merged_df2 and max_temp data files 

str(filtered_hospital_admission_data$Municipality_Residence_4)
str(temperature_Data$Municipality_Residence) 

# Ensure Municipality_Residence in temp data is a character string
temperature_Data <- temperature_Data %>%
  mutate(Municipality_Residence = as.character(Municipality_Residence))

# Convert seven-digit to six-digit by removing the last character
temperature_Data <- temperature_Data %>%
  mutate(Municipality_Residence_4 = substr(Municipality_Residence, 1, 6))

# Convert filtered hospital admission data Municipality_Residence_4 from factor to character

filtered_hospital_admission_data <- filtered_hospital_admission_data %>%
  mutate(Municipality_Residence_4 = as.character(Municipality_Residence_4))

#  /////////verify the the type of data before merging 

str(filtered_hospital_admission_data$Municipality_Residence_4)
str(temperature_Data$Municipality_Residence_4)

str(filtered_hospital_admission_data$Admission_Date)
str(temperature_Data$Admission_Date)

# save the filtered hospital record data and temperature data
save(filtered_hospital_admission_data , file = "filtered_hospital_dmission_data .RData")
save(temperature_Data , file = "temperature_Data .RData")


# Perform the left join for filtered  Hospital admision filtered data with temperature data

merged_final_data_left <- left_join(filtered_hospital_admission_data, temperature_Data, 
                                    by = c("Municipality_Residence_4", "Admission_Date"))


# perform the left join before filtered hospital data by the admission_date with temperature data///

merged_final_data_left_before_filtered <- left_join(merged_hospital_recored_Data, temperature_Data, 
  
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                                                                      by = c("Municipality_Residence_4", "Admission_Date"))

# \\\\\\\\\\\\'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Calculate the number of missing values in each column

missing_data_Merged <- colSums(is.na(merged_final_data_left_before_filtered))

# Display the number of missing values
print(missing_data_Merged)


original_row_count <- nrow(merged_Admission_Data)
print(paste("Number of rows in the original data:", original_row_count))

filtered_row_count <- nrow(filtered_hospital_admission_data)
print(paste("Number of rows in the original data:", filtered_row_count))

temprature_row_count <- nrow(temperature_Data)

print(paste("Number of rows in the original data:", temprature_row_count))

# Calculate the percentage of missing values in each column
missing_data_percentage <- colMeans(is.na(Cvd_Admissions_Data_Final)) * 100

# Display the percentage of missing values
missing_data_percentage




str(filtered_hospital_admission_data$Municipality_Residence_4)
str(temperature_Data$Municipality_Residence_4)
str(filtered_hospital_admission_data$Admission_Date)
str(temperature_Data$Admission_Date)




# Verify the data types and checking matching 


summary(temperature_Data$Municipality_Residence_4)
summary(filtered_hospital_admission_data$Municipality_Residence_4)
summary(temperature_Data$Admission_Date)
summary(filtered_hospital_admission_data$Admission_Date)


# Merge the filtered_mergedd hospital data and temperature data files using left joint 
merged_final_data <- left_join(filtered_hospital_admission_data, temperature_Data, 
                               by = c("Municipality_Residence_4", "Admission_Date"))

# summery the data of the merged dataset  
 
head(merged_final_data)
tail(merged_final_data)
str(merged_final_data)

#  remove the unwanted data files  
rm(tmax_obs_city, df_SP,df_PB,merged_df1,merged_df2, unmatched_data,cvd_admissions_final1)

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

cvd_admissions <- merged_final_data %>%
  filter(substr(Primary_Diagnosis, 1, 3) %in% cvd_icd_codes)

# Verify the filtered data
summary(cvd_admissions)
str(cvd_admissions)

# Remove unwanted columns

cvd_admissions1 <- cvd_admissions %>%
  select(-c(Hospital_cost, ICU_cost, Professional_cost, Total_cost, 
            State, province, Health_Establishment_Code, Postal_Code, 
            ICU_Months,))

# Function to categorize CVDS based on the Disease
cvd_admissions_final1 <- cvd_admissions_final1 %>%
  mutate(Major_CVD_Category = case_when(
    CVDs_Diagnosis_hospilazation %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Ischaemic Heart Diseases",
    CVDs_Diagnosis_hospilazation %in% c("I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69") ~ "Cerebrovascular Diseases",
    CVDs_Diagnosis_hospilazation %in% c("I50", "I42", "I43", "I44", "I45", "I46", "I47", "I48", "I49") ~ "Heart Failure and Cardiomyopathies",
    CVDs_Diagnosis_hospilazation %in% c("I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79",
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


#  remove the unwanted data files  
rm(max_temp, cvd_admissions_final, cvd_admissions,merged_final_data, cvd_admissions1, cvd_admissions_final)

# verify the type of final data

summary(cvd_admissions_final1)
str(cvd_admissions_final1)
ls(cvd_admissions_final1)

# Calculate count and percentage for the Gender column
gender_summary <- cvd_admissions_final1 %>% group_by(Gender) %>%
  summarise( count = n(),
    percentage = n() / nrow(cvd_admissions1) * 100
    
Age_summary <- cvd_admissions_final1 %>%
      group_by(Age_Category) %>%
      summarise(
        count = n(),
        percentage = n() / nrow(cvd_admissions_final1) * 100
      )

print(Age_summary)
        

# Visualization and further analysis (example)

# Plot the distribution of maximum temperatures
ggplot(max_temp, aes(x = Max_Temperature)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Maximum Temperatures", x = "Max Temperature (°C)", y = "Frequency")

# Plot the number of admissions by gender
ggplot(merged_final_data, aes(x = Gender)) +
  geom_bar(fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Number of Admissions by Gender", x = "Gender", y = "Number of Admissions")

  )

print(gender_summary)

# Load necessary package for regression analysis
if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
library(mgcv)

# Create a time series data frame for the analysis
cvd_time_series <- cvd_admissions %>%
  group_by(Admission_Date) %>%
  summarise(CVD_Hospitalizations = n())

# Merge with temperature data to include Max_Temperature
time_series_data <- cvd_time_series %>%
  left_join(tmax_obs_city, by = "Admission_Date")

# Verify the merged data
summary(time_series_data)
str(time_series_data)

# Poisson regression model to quantify the risk
poisson_model <- glm(CVD_Hospitalizations ~ Max_Temperature, family = poisson, data = time_series_data)
summary(poisson_model)

# Alternatively, use a Generalized Additive Model (GAM)
gam_model <- gam(CVD_Hospitalizations ~ s(Max_Temperature), family = poisson, data = time_series_data)
summary(gam_model)
plot(gam_model)







