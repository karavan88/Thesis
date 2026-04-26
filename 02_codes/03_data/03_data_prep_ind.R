#-------------------------------------------------------------------
# Project: PhD Thesis
# Script:  RLMS 2007-2023 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    1 December 2024
#-------------------------------------------------------------------

# Read the file with the packages needed for the analysis
# source(file.path(rCodes, "00_libraries.R"))

# Read the metadata xlsx file
codebook_ind <- 
  readxl::read_excel("00_metadata/ind_codebook.xlsx") %>%
  filter(Keep == "T") 

ind_vars_sel <- codebook_ind %>% pull(var)
  

# View(codebook_ind)

missing <- 99999996
vars_to_clean <- c("marst", "occup08", "educ", "diplom", "age", 
                   "j319", # type of school completed
                   # studies now in...
                   "j70_2", "j72_1a", "j72_2a", "j72_3a", "j72_4a", "j72_5a", "j72_6a",
                   # have completed edu level 
                   "j72_1", "j72_2", "j72_3", "j72_4", "j72_5", "j72_6",
                   # year of completion of each level of education
                   "j72_1e", "j72_2e", "j72_3e", "j72_4e", "j72_5e", "j72_6e",
                   # employment status and industry
                   "j1", "j11_1", "j4_1", 
                   # job search and its duration in weeks
                   "j81", "j81_1", 
                   # horizontal job/skill/edu mismatch
                   "j72_26",
                   # overall work experience
                   "j161_3y", "j161_3m", 
                   # hiring channel
                   "j5_2",
                   # working hours
                   "j6_1", "j6_1a", "j6_1b", "j6_2", "j10", "j13_2", 
                   # working overtime and payment delays
                   "j482", "j484", "j14",
                   # tenure
                   "j5a", "j5b",
                   # self-employed
                   "j26", "j29",
                   # job satisfaction measures
                   "j1_1_1", "j1_1_2", "j1_1_3", "j1_1_4", "j1_1_5", 
                   "j1_1_6", "j1_1_7", "j1_1_8", "j1_1_9", "j1_1_10",
                   # disability and self-perceived health
                   "m3", "m20_7")

# Read the data
rlms_ind_2001_2023 <- 
  readRDS(file.path(processedData, "rlms_ind_2001_2023.rds")) %>%
  select(all_of(ind_vars_sel)) %>%
  # replace with NAs obs in exp variable
  mutate(across(all_of(vars_to_clean), ~ ifelse(. >= missing, NA, .))) %>%
  # we need to carry some data imputation for experience
  mutate(idind = as_factor(idind),
         year_int = as.numeric(int_y), 
         # this converts labels into values - a year (numeric) and character string next to it
         wave = as_factor(id_w),
         # this keeps the original value but converts it into factor
         id_w = as.factor(id_w),
         area = as_factor(status),
         region = as_factor(region),
         sex = ifelse(h5 == 1, "Male", "Female"),
         edu_lvl = case_when(diplom %in% c(1,2,3) ~ "1. No school",
                             diplom == 4          ~ "2. Secondary School",
                             diplom == 5          ~ "3. Secondary Vocational",
                             diplom == 6          ~ "4. Tertiary"),
         employed  = ifelse(j1 %in% c(1:4), 1, 0), 
         employed_officially = ifelse(j11_1 == 1, 1, 0),
         self_employed = ifelse(j26 == 1, 1, 0),
         # we need to use case when to set all NAs to 0s
         seeking_employment = case_when(j81 == 1 ~ 1, TRUE ~ 0),
         exp_conv = j161_3m/12, 
         experience = j161_3y + exp_conv,
         wages = j10,
         industry = as_factor(j4_1),
         occupation = as_factor(occup08),
         in_school = ifelse(j70_2 == 1, 1, 0),
         in_vocat_courses = ifelse(j72_1a == 2, 1, 0),
         in_ptu_non_sec = ifelse(j72_2a == 2, 1, 0),
         in_ptu_sec = ifelse(j72_3a == 2, 1, 0),
         in_tech = ifelse(j72_4a == 2, 1, 0),
         in_uni = ifelse(j72_5a == 2, 1, 0),
         in_postgrad = ifelse(j72_6a == 2, 1, 0),
         in_education = case_when(in_school == 1 | in_vocat_courses == 1 | in_ptu_non_sec == 1  |
                                 in_ptu_sec == 1  | in_tech == 1  | 
                                 in_uni == 1  | in_postgrad == 1 ~ 1, 
                                 TRUE ~ 0)) %>%
  mutate(satisf_job         = ifelse(j1_1_1  %in% c(1,2), 1, 0),
         satisf_labor_cond  = ifelse(j1_1_2  %in% c(1,2), 1, 0),
         satisf_wage        = ifelse(j1_1_3  %in% c(1,2), 1, 0),
         satisf_career      = ifelse(j1_1_4  %in% c(1,2), 1, 0),
         satisf_wbl         = ifelse(j1_1_10 %in% c(1,2), 1, 0)) %>%
  # create variables on working over time
  mutate(working_overtime = case_when(j482 %in% c(3,4,5) ~ 1,
                                      TRUE           ~ 0),
         working_weekends = case_when(j484 %in% c(3,4,5) ~ 1,
                                      TRUE           ~ 0)) %>%
  mutate(marital_status = case_when(marst %in% c(2, 3)    ~ "2. Married/Civil partnership",
                                    marst %in% c(4, 5, 6) ~ "3. Divorced/Separated/Widowed",
                                    # let's treat some refusals etc as not married/base
                                    TRUE ~ "1. Single")) %>%
  mutate(school_completed = case_when(j319 %in% c(1,2,3) ~ "2. Advanced School",
                                      TRUE               ~ "1. Other")) %>%
  mutate(having_kids = case_when(j72_171 == 1 ~ "2. Has kids",
                                 TRUE         ~ "1. No kids")) %>%
  mutate(disability = case_when(m20_7 %in% c(1,5) ~ "2. Disabled",
                                TRUE              ~ "1. Not Disabled")) %>%
  mutate(poor_health = case_when(m3 %in% c(4,5) ~ "2. Poor or Very Poor Health",
                                 TRUE           ~ "1. Normal or Good Health")) %>%
  mutate(neet = case_when(in_education == 0 & employed == 0 ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(neet_unempl = case_when(in_education == 0 & 
                                   employed == 0 & 
                                   seeking_employment == 1 ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(neet_inactive = case_when(in_education == 0 &
                                     employed == 0 & 
                                     seeking_employment == 0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(neet_status = case_when(neet_unempl   == 1  ~ "NEET: Unemployed",
                                 neet_inactive == 1  ~ "NEET: Inactive",
                                 in_education == 1   ~ "In Education",
                                 employed == 1       ~ "Employed",
                                 TRUE ~ NA_character_)) 

# # a quick check on the neet rates
# neet_check = 
#   rlms_ind_2001_2023 %>%
#   filter(age >= 15 & age < 25) %>%
#   filter(id_w %in% c("25", "28")) %>%
#   group_by(id_w) %>%
#   summarise(neet = mean(neet, na.rm = TRUE)) 
# 
# # a quick check on the neet rates
# neet_status_check = 
#   rlms_ind_2001_2023 %>%
#   select(id_w, year, age, employed, in_education, seeking_employment, neet_status, neet) %>%
#   filter(age >= 15 & age < 25) %>%
#   filter(id_w %in% c("25", "28")) 
# 
# View(neet_status_check)
# 
# table(neet_status_check$neet_status, neet_status_check$year)
# 
# table(neet_status_check$neet_status[neet_status_check$year == 2016], 
#       neet_status_check$neet[neet_status_check$year == 2016])
  
  

summary(rlms_ind_2001_2023$j6_1b)
rlms_ind_2001_2023$area <- 
  factor(rlms_ind_2001_2023$area, 
         levels = c("Село", "ПГТ", "Город", "Областной центр"))

rlms_ind_2001_2023$marital_status <- 
  factor(rlms_ind_2001_2023$marital_status, levels = c("1. Single",
                                                       "2. Married/Civil partnership",
                                                       "3. Divorced/Separated/Widowed"))
# recode area variable
rlms_ind_2001_2023$area <- recode(rlms_ind_2001_2023$area,
                                  "Село" = "Rural", 
                                  "ПГТ" = "Urban-Type Settlement", 
                                  "Город" = "City", 
                                  "Областной центр" = "Regional Center")

# summary(rlms_ind_2007_2023$experience)
summary(rlms_ind_2001_2023$industry)

saveRDS(rlms_ind_2001_2023, "01_input_data/processed/rlms_ind_sel_2001_2023.rds")
