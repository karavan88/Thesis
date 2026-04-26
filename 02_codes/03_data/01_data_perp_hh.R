#-------------------------------------------------------------------
# Project: PhD Thesis
# Script:  RLMS Household Data Preparation
# Author:  Garen Avanesian
# Date:    1 December 2024
#-------------------------------------------------------------------


# Read the file with the packages needed for the analysis
source(file.path(rCodes, "00_libraries.R"))

# list of variables to select from the constructed data
hh_constr_vars <- c(
  "id_w", "redid_h", "id_h", "psu",
  "mh", "fh", "rmh", "rfh", "yh", "hhtype",
  "ncat1", "ncat2", "ncat3", "ncat4", "ncat5", "ncat6", # number of persons in a certain cat per hh
  "totexpr", "totexpn", "tincm_n", "tincm_r", # income and consumption
  "a_pind", "a_pgrp", "a_pthrn", "r_pind", "r_pgrp", "r_pthrn" # poverty tresholds
)

hh_vars <- c(
  "id_w", "redid_h", "id_h", "psu", "origsam",
  "status", "popul", "site", "region", "a3", "nfm1_n", "nfm"
  
)

# Load the officially constructed variables based on the RLMS household data
hh_1994_2023_constr <- 
  read_dta("01_input_data/HH_1994_2023_constr_rus.dta") %>%
  select(all_of(hh_constr_vars)) %>%
  # generate a variable with psu labels
  mutate(psu_label1 = as_factor(psu)) %>%
  # calculate total number of family members
  mutate(nfam_constr = ncat1 + ncat2 + ncat3 + ncat4 + ncat5 + ncat6) %>%
  select(-starts_with("ncat"))

# View(hh_1994_2023_constr)
# view_df(hh_1994_2023_constr)

# Load the full HH dataset 
hh_1994_2023 <- 
  readRDS("01_input_data/RLMS_HH_1994_2023_rus.rds") %>%
  select(all_of(hh_vars)) %>%
  # generate a variable with psu labels
  mutate(psu_label2 = as_factor(psu)) 

hh_1994_2023_selected <-
  hh_1994_2023 %>%
  full_join(hh_1994_2023_constr) %>%
  # seems like the issue with psu labels is not critical cause its just labeling
  select(-psu_label1, -psu_label2)

# View(hh_1994_2023_selected)

rm(hh_1994_2023_constr, hh_1994_2023)

#plot nfm and nfam_constr
ggplot(hh_1994_2023_selected, aes(x = nfam_constr, y = nfm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Number of Family Members in HH",
       x = "Constructed Number of Family Members",
       y = "Official Number of Family Members") +
  theme_minimal()

summary(hh_1994_2023_selected$nfam_constr)
summary(hh_1994_2023_selected$nfm)

# create a varable nfam which takes nfm if nfm is not NA, otherwise takes nfam_constr
hh_1994_2023_selected <- 
  hh_1994_2023_selected %>%
  mutate(nfam = ifelse(is.na(nfm), nfam_constr, nfm)) %>%
  # where nfam is NA (still around 40 obs), replace with rounded mean value
  mutate(nfam = ifelse(is.na(nfam), round(mean(nfam, na.rm = T)), nfam)) %>%
  mutate(hh_income = case_when(tincm_r <= 0 | is.na(tincm_r) ~ totexpr,
                               TRUE                          ~ tincm_r)) %>%
  # impute missing values in hh_income
  group_by(redid_h) %>%
  fill(hh_income, .direction = "downup") %>%
  mutate(hh_income_per_cap = hh_income/nfam) %>%
  ungroup() %>%
  group_by(id_w) %>%
  mutate(hh_per_cap_quantile = percent_rank(hh_income_per_cap)) %>%
  ungroup() 
  

# summary(hh_1994_2023_selected$nfam)

summary(hh_1994_2023_selected$hh_income)

View(hh_1994_2023_selected)

# save the dataset into the processed folder
saveRDS(hh_1994_2023_selected, "01_input_data/processed/hh_1994_2023_selected.rds")
