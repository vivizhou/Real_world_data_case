---
title: "Radboudumc_case"
author: "Vivi Zhou"
date: "2023-12-06"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/viviz/Radboudumc_case/")
library(writexl)
library(tidyverse)
library(lme4)
library(data.table)
```

# Problem 1

The program to transform xls files to xlsx files in one batch
```{r}
convert_data <- function(main_directory, xlsx_directory){
#' Convert and Combine Data from Multiple XLS Files
#'
#' This function reads data from multiple XLS files, organizes the data into different groups based on keywords in the filenames, processes the data, and combines it into separate XLSX files for each data group.
#'
#' @param main_directory The main directory containing patient subdirectories with XLS files.
#' @param xlsx_directory The directory where the combined data for each group will be saved as separate XLSX files.
#'
#' @return None (Output is saved as separate XLSX files for each data group in the specified directory).
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' convert_data(main_directory = "path/to/main_directory", xlsx_directory = "path/to/xlsx_directory")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split
#' @importFrom tools file_path_sans_ext
#' 
#' @export

  # List all patient directories
  patient_directories <- list.dirs(main_directory, recursive = FALSE, full.names = TRUE)
  
  # Define data groups
  data_groups <- c("AdmitMedsTac", "AdmitDate", "Admissions", "BasicPatient", "BasicTransplant", "CIMSmisc", "CIMSother",
                   "Dialysis", "Glucose", "Labsavg", "Labsraw", "Meds", "mGFRMRI", "MMF", "serology", "Tac", "vitals")
  
  # Create an empty list to store data frames for each data group
  data_list <- lapply(1:length(data_groups), function(i) {
    data.frame()
  })
  names(data_list) <- data_groups
  
  # Define keywords for each data group
  list_keywords <- list(c("admi.*", ".*med.*", ".*tac.*"),
                        c("admit", "date"),
                        c("admission.*"),
                        c("basic.*patient"),
                        c("basic.*transplant"),
                        c("cims.*misc"),
                        c("cims.*other"),
                        c("dialysis"),
                        c("glucose"),
                        c("labs.*avg"),
                        c("labs.*raw"),
                        c("meds"),
                        c(".*mgfr.*", ".*mri.*"),
                        c(".*mmf.*"),
                        c(".*serology.*"),
                        c(".*tac.*"),
                        c(".*vital.*")
  )
  
  # Function to check if the filename contains all keywords
  contains_all_keywords <- function(file_name, keywords) {
    file_name_lower <- tolower(file_name)
    return(all(sapply(keywords, function(keyword) length(grep(keyword, perl = TRUE, file_name_lower)) > 0)))
  }
  
  # Process each patient directory
  for (patient_directory in patient_directories) {
    
    # Exclude malfunctioned files in the "Data/11-20" directory
    xls_files <- if (patient_directory == "Data/11-20") {
      list.files(patient_directory, pattern = "[^1]\\.xls$", full.names = TRUE)
    } else {
      list.files(patient_directory, pattern = "\\.xls$", full.names = TRUE)
    }
    
    # Process each xls file in the directory
    for (xls_file in xls_files) {
      file_name <- tools::file_path_sans_ext(basename(xls_file))
      index <- 0
      
      # Check which data group the file belongs to based on keywords
      for (i in 1:length(list_keywords)){
        
        keywords <-list_keywords[[i]]
        if (contains_all_keywords(file_name, keywords)) {
          index <- i
          cat(paste("Processing",file_name, "match to index ", index), "\n")
          break
        }
      }
      
      # Read and process the data based on the determined data group
    
      if (index > 0) {
        if (index == 1) {
          # for now, for this data type, sheet 2 instead of sheet 1 will be taken.
          # for the data type with MED and TAC, some data have 10 columns, while some have 9,
          # I harmonize them by adding an extra column to the ones with 9.
          data <- readxl::read_excel(xls_file, sheet = 2)
          if (ncol(data) < 10) {
            data <- readxl::read_excel(xls_file, sheet = 2, col_types = c('numeric', 'date', 'text', 'guess', 'text', 
                                                                          "text", "numeric", "text", "date"))
            names(data[10]) <- "remark"
          } else if (ncol(data) == 10) {
            data <- readxl::read_excel(xls_file, sheet = 2, col_types = c('numeric', 'date', 'text', 'guess', 'text', 
                                                                          "text", "numeric", "text", "date", "text"))
            names(data)[10] <- "remark"
          }
          
        } else if (index == 13) { 
          # For mGFR data, some are not well separated into columns.
          data_raw <- readxl::read_excel(xls_file, col_types = "text")
          
          if (ncol(data_raw)< 2){
            data <- data.frame()
            for (j in 1:nrow(data_raw)){
              delim = "\\s{2,}"
              split_columns <- strsplit(as.character(data_raw[j,1]), delim, perl = TRUE)[[1]]
              data <- rbind(data,split_columns )
            }
            colnames(data) <-  strsplit(as.character(colnames(data_raw)), delim, perl = TRUE)[[1]]
            
          }
        } else{
          
          # For other data groups, read data directly, all columns considered text type for convenience.
          data <- readxl::read_excel(xls_file, col_types = "text")
          
        }
        names(data) <- tolower(names(data))
        
        if (nrow(data_list[[index]])>0) {
          data_list[[index]] <- merge(data_list[[index]],data, by = intersect(names(data_list[[index]]),names(data)), all = TRUE)
          
        }else{
          data_list[[index]] <- data
        }
        cat(paste("\n", "number of rows", nrow(data_list[[index]]), "\n" ))
      }else{
        # Internal check to find inconsistent data
        cat(paste("Error with file", file_name))
      }
      
    }
  }
  
  # Save each group of data into one xlsx file
  for (key in names(data_list)) {
    # Combine data frames along the rows
    combined_data <- data_list[[key]]
    xlsx_file <- paste0(key, ".xlsx")
    xlsx_path <- file.path(xlsx_directory, xlsx_file)
    # Write data to xlsx file
    writexl::write_xlsx(combined_data, xlsx_path)
  }
  
}

```

Execute the program.

```{r, eval=FALSE}
# Directory containing patient data files
main_directory <- "Data"

# Directory to store the output xlsx files
output_directory <- "Output2/"

# Convert patient xls files into xlsx and combine patients in separate folders in one batch
convert_data(main_directory, output_directory)
```


# Problem 2

## Problem 2.1

There are three approaches to assessing kidney function: utilizing data from the mGFR file, calculating based on laboratory results, or defining progressive CKD through dialysis events. In this discussion, our focus will be solely on the mGFR data, specifically G3 of CKD.
In the mGFR file, while opinion from radiologists are valuable, GFR values provide a quantitative foundation for thorough analysis. 

Ideally I would consult different people to make sure this is done correctly, but here I source information from [website](https://ukkidney.org/health-professionals/information-resources/uk-eckd-guide/ckd-stages#:~:text=CKD%20Definition&text=The%20definition%20of%20CKD%20includes,without%20markers%20of%20kidney%20damage).

CKD patients are defined as having GFR < 59 for at least two time points. Patients who already had CKD before transplant are excluded.


1. Read mGFRMRI.xlsx File and Process GFR Data:

```{r}
gfr <- readxl::read_excel('Output2/mGFRMRI.xlsx')
head(gfr)
gfr <- gfr[gfr$bexam_type == 'gfr',]
```

Define a function to extract gfr from text result

```{r}
extract_gfr_from_text <- function(str) {
  ####Description:
  ####This function extracts glomerular filtration rate (GFR) information from a text string by searching for predefined keywords related to GFR values.
  ####Parameters:
  ####  str (character): The input text string from which to extract GFR information.
  ####Output:
  ####  Numeric vector: The extracted GFR value(s) as a numeric vector.
  ####Details:
  ####  The function searches for specific keywords related to GFR in the input text.
  ####  If any of the primary keywords are found, the function extracts the relevant information.
  ####  If the primary keywords do not yield a result, the function searches for secondary keywords to extract GFR information.
  ####  The extracted GFR value is cleaned to retain only numeric characters.
  
  keyword <- c("GFR/1.73 sq.m.:", "GFR/1.73 sq.m:", "GFR/1.73 sqm.", "GFR/1.73sqm =", "per 1.73 m2 is", 
               "GFR/1.73sqm:", "GFR is calculated at at", "is calculated at")
  gfr1.73 <- NA
  for (i in (1:length(keyword))){
    if (grepl(keyword[i], str)) {
      sub_text <- str_split(str, keyword[i])[[1]][2]
      gfr_text <- substr(sub_text, 1, 4)
      gfr1.73 <- gsub("[^0-9]", "", gfr_text)
      break
    }
  }
  
  if (is.na(gfr1.73)){
    keyword2 <- c('ml minute per 1.73', "ml per minute per 1.73", "ml minute per 1.73", "ml minute per 1.70", 
                  "ml and minute per square meter", "ml /minute per 1.73 m2", "ml per 1.73 square meter")
    for (j in (1:length(keyword2))){
      if (grepl(keyword2[j], str)) {
        sub_text <- str_split(str, keyword2[j])[[1]][1]
        gfr_text <- substr(sub_text, str_count(sub_text) - 3, str_count(sub_text))
        gfr1.73 <- gsub("[^0-9]", "", gfr_text)
      }
    }
  }
  return(as.numeric(gfr1.73))
}
```


Extract GFR values using the function.

```{r}
gfr <- gfr %>% mutate(gfr1.73 = sapply(result, FUN = function(str) {
 extract_gfr_from_text(str)
  }))

```

Sanity check:

```{r}
## check missing values
cat(nrow(gfr[is.na(gfr$gfr1.73),]), "missing from this extraction, all checked.")

## Check the distribution of GFR (sanity check)
hist(gfr$gfr1.73)
```

To assess who developed CKD, it is relevant to know if measurements were before or after transplant.

2. Load and process transplant data

```{r}
## Read BasicTransplant.xlsx File:
TxDate <- readxl::read_excel('Output2/BasicTransplant.xlsx')

## Extract Relevant Columns from BasicTransplant Data:
TxDate_1 <- TxDate[,c(1,2,3,6,7,9)]
colnames(TxDate_1) <- c('id', 'tx_date', 'tx_organ', 'tx_donor_age', 'tx_donor_sex', 'tx_donor_race')
TxDate_1$tx_date <- as.Date(TxDate_1$tx_date, "%Y-%m-%d")

## Create TxDate_only with ID and Transplant Date:
TxDate_only <- TxDate_1[,1:3]
head(TxDate_only)
```


3, Clean&filter the GFR data combined with transplant data:

```{r}
## Renames a column in the GFR data, 
## selects specific columns, and merges the GFR data with 'TxDate_only' based on the 'id' column.
colnames(gfr)[1] <- "id"
gfr1.73 <- gfr[,c('id', 'dstartdate', 'gfr1.73')]
# Remove duplicated values
gfr1.73 <- unique(gfr1.73)
## Remove missing values
gfr1.73 <- gfr1.73[!is.na(gfr1.73$gfr1.73),]
gfr_all_id <- unique(gfr1.73$id)
length(gfr_all_id) # 346 patients have gfr measures

head(gfr1.73)
gfr1.73 <- merge(gfr1.73, TxDate_only)
gfr1.73$dstartdate <- as.Date(gfr1.73$dstartdate,  "%Y-%m-%d")
gfr1.73 <- gfr1.73 %>% mutate(days_to_tx = as.numeric(difftime(dstartdate, tx_date, units = 'days')))

hist(gfr1.73$days_to_tx)


```

4. Exclude those that already had GFR <=59 before transplant.

```{r}
## Check these records
CKD_bf <- gfr1.73[gfr1.73$gfr1.73<=59 & gfr1.73$days_to_tx < 0,]
table(CKD_bf$tx_organ) 
```

Most received organ 2 which is kidney transplant. For now we will remove these people, but potentially they can also be analysed.

```{r}
id_ckd_beforeTx <- unique(CKD_bf$id)

gfr_1.73_filtered <- gfr1.73[!gfr1.73$id %in% id_ckd_beforeTx,]
cat("Number of records left after filtering:", nrow(gfr_1.73_filtered), "\n") # 303 patients remained

## Remove duplicated records
gfr_1.73_filtered <- unique(gfr_1.73_filtered)
gfr_filter_id <- gfr_1.73_filtered$id
cat("Number of patients left after filtering:", length(unique(gfr_filter_id))) # 303 patients remained

```

5. Identify patients with CKD.

```{r}

ckd_patients <- gfr_1.73_filtered %>%
  group_by(id) %>%
  filter(sum(gfr1.73 < 60) >= 2)
gfr_ckd_id <- unique(ckd_patients$id)

cat("The number of patients developed CKD defined by GFR <60 for at least two time points is:", length(gfr_ckd_id), "out of ", length(unique(gfr_filter_id)), "patients with GFR levels available.\n") # 33 out of 303 patients with gfr measurements developed CKD.
summary(ckd_patients[ckd_patients$gfr1.73 < 60, "days_to_tx"]) 

```
Nearly 75% develop CKD after 1 year.

### Other definitions of CKD

Dialysis (Dialysis/Dialysis dates) data and Lab data (Labsavg) can also be used to define CKD or progressive CKD.

In Lab data CKD is defined using albumin/creatinine. However, I am not sure about the unit of the measurements so will skip this.

### Conclusion:

Using definition of GFR <60 for at least two time points,
The number of patients developed CKD is: 33 (out of 303 patients with GFR levels available).

## Problem 2.2

Factors influencing kidney function will be assessed by considering various elements such as basic indicators, blood pressure (BP), medications, glucose levels, mycophenolate mofetil (MMF), and Tacrolimus (Tac). Additionally, other factors within medication categories and miscellaneous/other (CIMSmiscl/other) can be evaluated to encompass a comprehensive analysis. However, due to time constraints, a thorough resolution of the problem, involving extensive data cleaning, data mining, and literature review, is not feasible. Here I select a few files for analysis.

Two potential outcomes will be examined: CKD as a binary outcome, analyzed using logistic regression models, and GFR as a continuous outcome, involving repeated measurements over time. 

Univariate analysis will be conducted using a linear mixed model for repeated outcomes and a generalized linear model (glm) for binary outcomes, exploring potential factors influencing both outcomes.

Note: the impact of missing values on the results was not evaluated during the initial analysis, but this consideration will be addressed if time permits. 

Note: for efficiency, the analysis will focus on average values over time rather than assessing factors at the baseline (time at transplant).

Note: for the time constraints, a quick and preliminary analysis has been performed; however, a more meticulous step-by-step modeling process would be ideal for identifying the best-fit model.

### Prepare GFR outcome data

To analyze GFR values over time, I will recode the time data by year.

```{r}

gfr_1.73_filtered$year_to_tx <- round(gfr_1.73_filtered$days_to_tx / 365)
head(gfr_1.73_filtered)

```
```{r}
gfr_avg_year <- gfr_1.73_filtered %>% group_by(id, year_to_tx) %>% 
  summarize(gfr_1yearavg = mean(gfr1.73), .groups = "drop")
nrow(gfr_avg_year)
```
Merge data.

```{r}
gfr_1.73_filtered <- left_join(gfr_avg_year, gfr_1.73_filtered[,-3])
head(gfr_1.73_filtered)
```
 
Plot the change of gfr overtime for a random set of 20 patients.

```{r}
set.seed(123)

gfr_subset <- gfr_1.73_filtered[gfr_1.73_filtered$id %in% sample(gfr_1.73_filtered$id, 20, replace = FALSE),]
head(gfr_subset)
ggplot(gfr_subset, aes(x= year_to_tx, y = gfr_1yearavg)) +
  geom_line(aes(group = id))+
  geom_point(aes(fill = as.factor(id)), pch = 21, size = 1, stroke = 1) +
  theme_bw()

```

Only include data after transplant.

```{r}
gfr_1.73_filtered <- gfr_1.73_filtered %>% arrange(id, year_to_tx)
gfr_1.73_analysis <- gfr_1.73_filtered[gfr_1.73_filtered$year_to_tx >=0,c('id', "gfr_1yearavg", "year_to_tx")]
head(gfr_1.73_analysis)

```

### GFR before/around transplant as baseline

I will use the average levels measured before/within a month of transplant as baseline levels.

```{r}

gfr_1.73_preTx <- gfr_1.73_filtered[gfr_1.73_filtered$days_to_tx <= 30,]
gfr_preTx <- gfr_1.73_preTx %>% group_by(id) %>% summarize(baseline_gfr = mean(gfr_1yearavg))
nrow(gfr_preTx)
```
The logistic model for association between baseline GFR and CKD.

```{r}
gfr_preTx <- gfr_preTx %>% mutate(ckd = ifelse(id %in% gfr_ckd_id, 1, 0))
model <- glm(ckd ~ baseline_gfr, data = gfr_preTx)
print(summary(model))
```
There seems to be no significant association. 

### Function for univariate linear mixed model.

A program to run univariate linear mixed model for gfr over time

```{r}
lmer_univariate <- function(data_to_test, variable_name, outcome, outcome_type = "cont"){
  ml_formula <- paste(outcome," ~ (1 + year_to_tx| id) + ", variable_name)

  if (outcome_type == "cont") {
    model <- lmer(ml_formula, data = data_to_test)
  } else if (outcome_type == "bin") {
    model <- glmer(ml_formula, data = data_to_test, family = binomial)
  }
  print(summary(model))
  print(anova(model))
}

```

```{r}
lmer_univariate_2 <- function(data_to_test, variable_name, outcome, outcome_type = "cont"){
  ml_formula <- paste(outcome," ~ (1 + year_to_tx| id) + ",variable_name,"*year_to_tx + year_to_tx +", variable_name)

  if (outcome_type == "cont") {
    model <- lmer(ml_formula, data = data_to_test)
  } else if (outcome_type == "bin") {
    model <- glmer(ml_formula, data = data_to_test, family = binomial)
  }
  print(summary(model))
  print(anova(model))
}

```



### Basic: patient sex (age not available), donor sex and age

Load and process data.

```{r}
basic <- readxl::read_excel('Output2/BasicPatient.xlsx')
basic <- unique(basic)
# Here we focus on patients with GFR available
basic_filter <- basic[basic$id %in% gfr_filter_id,]

data <- basic_filter %>% mutate(ckd = ifelse(id %in% gfr_ckd_id, 1, 0))
data <- merge(data, gfr_preTx, all = TRUE)
head(data)
# 2x2 table for sex and ckd
table(data$ckd, data$sex)
print(chisq.test(data$ckd, data$sex))
```

```{r}
# include data of donor age & sex
data <- left_join(data, TxDate_1, by = 'id')
head(data)
```

Check if donor age and sex are possible factors for CKD development

```{r}

uni_model <- glm(ckd ~ as.numeric(tx_donor_age), data = data)
print(summary(uni_model))

uni_model <- glm(ckd ~ tx_donor_sex, data = data)
print(summary(uni_model))
```
Both donor sex and donor age are not related to CKD. Note as there are half of missing values, there may not be enough power to detect the association.

Next, combine data with gfr values.

```{r}
data_cont <- full_join(gfr_1.73_analysis, data, by = 'id')
head(data_cont)
```

### Average blood pressure and other signs overtime in vitals

```{r}
vitals <- readxl::read_excel('Output2/vitals.xlsx')
vitals <- merge(vitals, TxDate_only)
vitals$`date taken` <- as.Date(vitals$`date taken`, "%Y-%m-%d")
vitals <- vitals %>% mutate(days_to_tx = as.numeric(difftime(`date taken`, tx_date, units = 'days')))
vitals$`the value` <- as.numeric(vitals$`the value`)
summary(vitals$days_to_tx) # all values were after transplant
head(vitals)

```

Transform the data into wide format

```{r}

vitals <- vitals %>% pivot_wider(
  id_cols = c(id,`date taken`, days_to_tx),
  names_from = `vital sign`,
  values_from = `the value`,
  values_fn =  ~ mean(.x, na.rm = TRUE),
  values_fill = NA)
head(vitals)

```



```{r}
vitals <- vitals %>% mutate(year_to_tx = round(days_to_tx / 365))
head(vitals)
```



Extract relevant data to be combined with GFR longitudinal data.

```{r}
vitals_select <- vitals[,c(1,4:10)]

# This next step average values measure in one year.
vitals_select_gp <- vitals_select %>% group_by(id, year_to_tx) %>% 
  summarize(systolic_1 = mean(systolic, na.rm = TRUE),
            diastolic_1 = mean(diastolic, na.rm = TRUE),
            HR_1 = mean(HR, na.rm = TRUE),
            WT_1 = mean(WT, na.rm = TRUE),
            UO_1 = mean(UO, na.rm = TRUE),
            RR_1 = mean(RR, na.rm = TRUE),
            )
head(vitals_select_gp)


```


```{r}
data_cont_1 <- left_join(data_cont, vitals_select_gp, by = c("id", "year_to_tx"))
head(data_cont_1)
ckd_data <- data_cont_1 %>% group_by(id) %>% summarize(ckd_m = max(ckd))

# Sanity check
print(sum(ckd_data$ckd_m))
```

Linear mixed model for one variable.

```{r}
model <- lmer(gfr_1yearavg ~ sex + systolic_1 + (1 +year_to_tx| id), data = data_cont_1)
print(summary(model))
print(anova(model))
```
This model suggests that there is a significant variability between individuals in their baseline gfr1.73 levels and their rate of change over time. The fixed effect of systolic_1 is not statistically significant, meaning that there is no strong evidence of a linear relationship between systolic_1 and gfr1.73 after accounting for individual variability.

Linear mixed model for variables.

```{r}
model <- lmer(gfr_1yearavg ~ sex + diastolic_1 + (1 +year_to_tx| id), data = data_cont_1)
print(summary(model))
print(anova(model))
```
```{r}
model <- lmer(gfr_1yearavg ~ sex + HR_1 + (1 +year_to_tx| id), data = data_cont_1)
print(summary(model))
print(anova(model))
```
It shows no significant associaiton with the variable.

We can also analyse it as average levels overtime for each patient.
```{r}
vitals_avg <- vitals %>% group_by(id) %>%
  summarize(across(c(systolic, diastolic, HR, WT, UO, RR), mean, na.rm = TRUE, 
                   .names ="mean_{.col}"))

head(vitals_avg)


```

Merge vitals with other data.

```{r}
vitals_avg_gfr <- left_join(data_cont_1, vitals_avg, by = 'id')
head(vitals_avg_gfr)


```


```{r}
variables <- colnames(vitals_avg_gfr)[20:25]
variables
```
Check their associations.

```{r}

for (variable in variables) {
  formula_str <- paste("ckd ~", variable)
  uni_model = glm(data = vitals_avg_gfr, formula_str, family = binomial)
  # Print summary
  print(summary(uni_model))
  cat("\n")
}
```

The results show that only average systolic BP shows border line significance. And it seems higher systolic BP is associated with higher eGFR, which is counter-intuitive.

Also model vitals vs change in eGFR

```{r}
for (variable in variables){
    lmer_univariate(data_to_test = vitals_avg_gfr, variable_name = variable,  outcome = "gfr_1yearavg")
}

```

The results show that none of the variables tested were significantly associated with change in mGFR.

### Functions used to process data and analyze repeated measurement data.

For convenience, I build a few functions to process and analyse different data files of repeated measurements.

A function to process measurement data files.

```{r}
process_data <- function(the_data, date_var, name_var, value_var, data_merge = data_cont){
  the_data$new_date <- as.Date(unlist(the_data[,date_var]), "%Y-%m-%d")
  the_data <- merge(the_data, TxDate_only, by = 'id')

  the_data <- the_data %>% mutate(days_to_tx = as.numeric(difftime(new_date, tx_date, 
                                                                   units = 'days')))

  the_data <- the_data %>% mutate(across(value_var, as.numeric))

  variables <- unique(the_data[, name_var])

  the_data <- the_data %>% pivot_wider(
  id_cols = c(id, date_var, days_to_tx),
  names_from = name_var,
  values_from = value_var,
  values_fn =  ~ mean(.x, na.rm = TRUE),
  values_fill = NA)
  
  the_data <- the_data %>% mutate(year_to_tx = round(days_to_tx / 365))
  
  the_data_select <- the_data[the_data$id %in% unique(data_merge$id),]
  
  setDT(the_data_select)
  summarized_data <- the_data_select[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                            by = .(id, year_to_tx), 
                            .SDcols = variables]

  data_cont_1 <- left_join(summarized_data, data_cont, by = c("id", "year_to_tx"))

  return(data_cont_1)
}

```

A program to get average level per patient and merge to main data
```{r}
get_average <- function(variable, data_to_summarize, data_to_merge){
  setDT(data_to_summarize)
  mean_variable <- data_to_summarize[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                            by = .(id), 
                            .SDcols = variable]
  new_column_names <- paste0("mean_", names(mean_variable)[-1])  # Exclude the 'id' column
  setnames(mean_variable, old = names(mean_variable)[-1], new = new_column_names)
  data_1 <- merge(data_to_merge, mean_variable)
  return(data_1)
}

```


#### Glucose levels

Load glucose data

```{r}
data_file = 'Output2/Glucose.xlsx'
glucose = readxl::read_excel(data_file)
colnames(glucose)[2] <-  c("collected_date")
head(glucose)

```
```{r}
hist(as.numeric(glucose$result))

```
Glucose data is skewed, tranform with log10.

```{r}
glucose_1 <- glucose
glucose_1$result <- log10(as.numeric(glucose_1$result))
hist(glucose_1$result)

```
Remove outliers,
```{r}
mean(glucose_1$result, na.rm = TRUE) - 4* sd(glucose_1$result, na.rm = TRUE)

```

```{r}


glucose_1<- glucose_1[glucose_1$result > mean(glucose_1$result, na.rm = TRUE) - 4* sd(glucose_1$result, na.rm = TRUE),]
```

```{r}
hist(glucose_1$result)

```


```{r}
glucose <- glucose_1
glucose_data_cont <- process_data(the_data = glucose, date_var = "collected_date", name_var = "test", value_var = "result")
head(glucose_data_cont)
```



```{r}

print(summary(glucose_data_cont$year_to_tx))
```
All the glucose levels were collected after transplant.


Model to assess association between glucose levels and gfr levels over time.

```{r}
lmer_univariate(glucose_data_cont, "glucose", outcome = "gfr_1yearavg")

```
No association was observed between glucose levels and GFR levels over time.

In this next one I also evaluate interaction between time and glucose as fixed-effect.

```{r}
lmer_univariate_2(glucose_data_cont, "glucose", outcome = "gfr_1yearavg")

```

Also get the average levels of glucose over time.

```{r}
glucose_avg_gfr <- get_average('glucose', data_to_summarize = glucose_data_cont, data_to_merge = glucose_data_cont)
head(glucose_avg_gfr)
```

```{r}
model = glm(ckd ~ mean_glucose, data = glucose_avg_gfr)
summary(model)

```
I also did not find an association between average glucose levels and CKD.

Also analyse the assciation between mean glucose value and GFR change

```{r}
lmer_univariate(data_to_test = glucose_avg_gfr, variable_name = "mean_glucose", outcome = "gfr_1yearavg")

```
The association bewteen them is not significant.

### MMF levels

```{r}
mmf_data_file = 'Output2/MMF.xlsx'
mmf = readxl::read_excel(mmf_data_file)
colnames(mmf)[2] <-  c("collected_date")
head(mmf)

```


Process data and merge with gfr longitudinal data.

```{r}
mmf$measure <- "mmf"
mmf_data_gfr <- process_data(mmf, date_var = "collected_date", value_var = "result", name_var = "measure", data_merge = data_cont)
head(mmf_data_gfr)
```

```{r}
nrow(mmf_data_gfr)
sum(is.na(mmf_data_gfr$mmf))

```

```{r}
cat("The number of patients with MMF levels:", length(unique(mmf_data_gfr[!is.na(mmf_data_gfr$mmf),]$id)))

```

Most of data are missing because they have not used mmf

```{r}
hist(mmf_data_gfr$mmf)

```

Get a list of ids who have used mmf.

```{r}
suppressWarnings(Meds <- readxl::read_excel('Output2/Meds.xlsx'))
head(Meds)

```


```{r}
mmf_id <- unique(Meds[grep("mycophenolate", Meds$`drug name`, ignore.case = TRUE),]$id)
tac_id <- unique(Meds[grep("tacrolimus", Meds$`drug name`, ignore.case = TRUE),]$id)
head(mmf_id)
cat("The number of patients treated with mmf:", length(mmf_id),"\n")
head(tac_id)
cat("The number of patients treated with Tac:", length(tac_id))

```
It seems many have used MMF but their levels were not monitered.

Add this data to compare with CKD.

```{r}
data$ever_mmf <- ifelse(data$id %in% mmf_id, 1, 0)
data$ever_tac <- ifelse(data$id %in% tac_id, 1, 0)

```

```{r}
table(data$ckd, data$ever_mmf)
table(data$ckd, data$ever_tac)
```

The distribution seems similar between group. Almost everyone has used Tac.

```{r}
head(mmf_data_gfr)

```

Also get average values for mmf.

```{r}
mmf_avg <- get_average(variable = "mmf", data_to_summarize = mmf_data_gfr, data_to_merge = mmf_data_gfr)
head(mmf_avg)
```

Test if the average level of MMF is associated with GFR values over time.

```{r}
lmer_univariate(data_to_test = mmf_avg, variable_name = "mean_mmf", outcome = "gfr_1yearavg")
```

Test if the average level of MMF is associated with CKD outcome.
```{r}
mmf_avg_1 <- unique(mmf_avg[,c("id","ckd","mean_mmf")])
model = glm(ckd ~ mean_mmf, data = mmf_avg_1)
print(summary(model))
```
### Tac blood levels

Load Tac blood level data.

```{r}
tac_data_file = 'Output2/Tac.xlsx'
tac = readxl::read_excel(tac_data_file)
colnames(tac)[2] <-  c("collected_date")
head(tac)
hist(as.numeric(tac$result))
```


Here I can see the data is skewed, so I will transform the data.

```{r}
tac_1 <- tac
tac_1$result <- log10(as.numeric(tac_1$result))
hist(tac_1$result)
```

Process data for analysis. Note the Tac levels have been transformed.

```{r}
tac <- tac_1
tac$measure <- "tac"
tac_data_gfr <- process_data(tac, date_var = "collected_date", value_var = "result", name_var = "measure", data_merge = data_cont)
head(tac_data_gfr)
nrow(tac_data_gfr)
sum(is.na(tac_data_gfr$tac))
```


Test the association using univariate model.

```{r}
lmer_univariate(data_to_test = tac_data_gfr, variable_name = 'tac', outcome = "gfr_1yearavg")

```


```{r}
lmer_univariate(data_to_test = tac_data_gfr, variable_name = 'tac', outcome = "ckd", outcome_type = "bin")

```

Also get the average levels of Tac.
```{r}
tac_avg <- get_average(variable = 'tac', data_to_summarize = tac_data_gfr, data_to_merge = tac_data_gfr)

```

Also get the average levels of Tac.
```{r}
tac_avg <- get_average(variable = 'tac', data_to_summarize = tac_data_gfr, data_to_merge = tac_data_gfr)
head(tac_avg)
```
Also test if average tac level is associated with GFR over time.

```{r}

lmer_univariate(data_to_test = tac_avg, variable_name = 'mean_tac', outcome = "gfr_1yearavg")

```
```{r}
tac_avg_glm <- unique(tac_avg[, c("id", "ckd", "mean_tac")])
```

```{r}

model = glm(ckd ~ mean_tac, data = tac_avg_glm)
print(summary(model))

```

### Conclusion for problem 2.2:
I only found average systolic BP after transplant as possible factor contributing to change of renel function. However, it was a positive association, i.e. higher systolic BP associated with higher GFR, which is likely to be confounded. 

Since the association between Tac blood level and change in GFR was not significant, I do not expect an association between them after correcting for other covariates. However, in case of effect maskers I will continue the analysis.

## Problem 2.3

The previous analysis was only based on complete data, records with missing values were dropped from the analysis. In this following task, I will combine all the data to assess whether there is  dose-response relationship between tacrolimus and renal function.

### Statistical analysis

In this design, I employ longitudinal data analysis with linear mixed models to investigate the potential dose-response relationship between tacrolimus blood levels and renal function, as represented by GFR time series data. This methodology allows for the consideration of repeated measurements within each participant and the exploration of individual variability in the relationship over time.

I will use linear mixed model to model the longitudinal data.

**Model specification**

\[ GFR_{ij} = \beta_0 + \beta_1 \times \text{TacBloodLevel}_{ij} + \beta_2 \times X_{ij} + \gamma_{i0} + \gamma_{i1} \times \text{Time}_{ij} + \epsilon_{ij} \]

Where:

- \( GFR_{ij} \): GFR for the \(i\)-th subject at the \(j\)-th time point.

- \( \text{TacBloodLevel}_{ij} \): Tacrolimus blood level for the \(i\)-th subject at the \(j\)-th time point.

- \( X_{ij} \): Additional covariate for the \(i\)-th subject at the \(j\)-th time point.

- \( \beta_0 \): Overall intercept.

- \( \beta_1 \): Fixed effect coefficient for the dose-response relationship.

- \( \beta_2 \): Fixed effect coefficient for the additional covariate.

- \( \gamma_{i0} \): Random intercept for the \(i\)-th subject.

- \( \gamma_{i1} \): Random slope for Time for the \(i\)-th subject.

- \( \text{Time}_{ij} \): Time variable.

- \( \epsilon_{ij} \): Residual error.


Note: Potentially we can fill in missing data assuming all the missing data were missed at random (MAR), using R package imputeTS.

**Inclusion of covariates**
Additional covariates were considered in the model to account for potential confounding effects or variations, such as glucose (known risk factor), systolic (shown to be possible factor), mmf ever use (as it is another immunosuppresive drug).


### Combine data

```{r}
vitals_extr <- vitals_avg_gfr[,-c(9:13)]
glucose_extr <- glucose_avg_gfr[,c("id", "year_to_tx","glucose", "mean_glucose")]
mmf_extr <- mmf_avg[,c("id", "year_to_tx","mmf", "mean_mmf" )]
tac_extr <- tac_avg[,c("id", "year_to_tx","tac", "mean_tac" )]

```

```{r}
# List of data frames
list_of_dfs <- list(vitals_extr, glucose_extr, mmf_extr, tac_extr)

# Merge data frames using Reduce and merge
merged_df <- Reduce(function(x, y) merge(x, y, by = c("id", "year_to_tx"), all = TRUE), list_of_dfs)
merged_df <- unique(merged_df)
head(merged_df)
```

Sanity check.

```{r}
count_records <- merged_df %>% group_by(id, year_to_tx) %>%count()
count_records[which(count_records$n >1),]
```

An impression of the missingness.

```{r}
cat("Expected records if all data are not missing:", length(gfr_filter_id) * length(unique(merged_df$year_to_tx)), "\n")
cat("Current dataset has: ", nrow(merged_df))

```

Distribution of time points.

```{r}
hist(merged_df$year_to_tx, breaks = 15)

```

Here we see that there are much fewer data points after 1 year. (It is also likely that patients with lower renal functions are monitered more often.)

An impression of change of GFR over time for each individual.

```{r}
set.seed(123)

merged_df_subset <- merged_df[merged_df$id %in% sample(merged_df$id, 20, replace = FALSE),]


ggplot(merged_df_subset, aes(x= year_to_tx, y = gfr_1yearavg)) +
  geom_line(aes(group = id))+
  geom_point(aes(fill = as.factor(id)), pch = 21, size = 1, stroke = 1) +
  theme_bw()

```

### Modeling

#### model Tac level as one average value.

Run linear mixed model between mean of tac and change of gfr with average values including covariates.

```{r}
colnames(merged_df)
```
```{r}
merged_df$ever_mmf <- ifelse(merged_df$id %in% mmf_id, 1, 0)

```

```{r}
model <- lmer(gfr_1yearavg ~ (1+ year_to_tx | id) + sex + mean_tac + mean_systolic + mean_glucose + ever_mmf, 
     data = merged_df)

print(summary(model))
print(anova(model))
```

The result shows that there is no association between mean Tac levels in the blood and change of GFR.

#### Model Tac as time series data.

The formula below assess whether there is dose-response relationship between Tac and GFR after correcting for othre covariates.

```{r}
model <- lmer(gfr_1yearavg ~ (1+ year_to_tx | id) + sex + year_to_tx + year_to_tx*tac + tac + mean_systolic + mean_glucose + ever_mmf, 
     data = merged_df)

print(summary(model))
print(anova(model))
```

Below I also add year_to_tx and its interaction with tac as fixed effect.

```{r}
model <- lmer(gfr_1yearavg ~ (1+ year_to_tx | id) + sex + year_to_tx + year_to_tx*tac + tac + mean_systolic + mean_glucose + ever_mmf, 
     data = merged_df)

print(summary(model))
print(anova(model))
```
Tac change over time is also not associated with change in GFR.

### Further analyses

- Check model assumptions: normality, linearity

- Check model fit

- Machine learning methods: ML methods such as Random forest could analyse all the variables at the same time without over-fitting, and provide an impression of which variables have the most impact on renal decline.


### Conclusion:

I did not observe a dose-dependent relationship bewteen  tacrolimus and renal function.

Note: 
I did not assess dose in labels, instead I used blood Tac levels. 
Due to time constraint, bias was not carefully evaluated. Potential bias could be survival bias, etc.
