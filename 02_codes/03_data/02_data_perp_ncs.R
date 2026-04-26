#-------------------------------------------------------------------
# Project: PhD Thesis
# Script:  RLMS 2016 and 2019 Non-Cognitive Skills Data Preparation
# Author:  Garen Avanesian
# Date:    1 December 2024
#-------------------------------------------------------------------

# Read the file with the packages needed for the analysis
source(file.path(rCodes, "00_libraries.R"))

# Read the data
rlms_ind_2016_2023 <- readRDS("01_input_data/rlms_ind_2016_2023.rds")

# Variables for non-cognitive skills
ncs_vars <- 
  c("j445_3", "j445_11", "j445_14",
    "j445_2", "j445_12", "j445_17",
    "j445_1", "j445_4",  "j445_20",
    "j445_9", "j445_16", "j445_19",
    "j445_5", "j445_10", "j445_18")

openness <- c("o1", "o2", "o3") 
con <- c("c1", "c2", "c3")
ex <- c("e1", "e2", "e3")
ag <- c("a1", "a2", "a3")
em_st <- c("es1", "es2", "es3") 
grit <- c("g1", "g2", "g3")

#### produce non-cognitive skill measures
rlms_ncs <-
  rlms_ind_2016_2023 %>%
  filter(id_w %in% c(25, 28)) %>%
  select(id_w, idind, 
         all_of(ncs_vars)) %>%
  mutate(idind = as.factor(idind)) %>%
  mutate(across(all_of(ncs_vars), ~ ifelse(. >= 88888888, NA, .))) %>%
  mutate(o1 = 5 - j445_3,
         o2 = 5 - j445_11,
         o3 = 5 - j445_14,
         c1 = 5 - j445_2,
         c2 = j445_12,
         c3 = 5 - j445_17,
         e1 = 5 - j445_1,
         e2 = j445_4, 
         e3 = 5 - j445_20,
         a1 = 5 - j445_9,
         a2 = 5 - j445_16,
         a3 = 5 - j445_19,
         es1 = 5 - j445_5,
         es2 = j445_10,
         es3 = j445_18) %>%
  mutate(O = scale(rowMeans(select(., all_of(openness)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         C = scale(rowMeans(select(., all_of(con)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         E = scale(rowMeans(select(., all_of(ex)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         A = scale(rowMeans(select(., all_of(ag)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         ES = scale(rowMeans(select(., all_of(em_st)), na.rm = T),center = TRUE, scale = TRUE)[,1]) %>%
  select(-all_of(ncs_vars), -all_of(openness), -all_of(con), -all_of(ex), -all_of(ag), -all_of(em_st)) %>%
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(O, C, E, A, ES, .direction = "downup") %>%
  ungroup() %>%
  drop_na() 

# View(rlms_ncs)

#summary(rlms_ncs)

# Save the dataset in the processed 
saveRDS(rlms_ncs, "01_input_data/processed/rlms_ncs.rds")
