##############################################
## Setup and Library Loading
##############################################
library(ggplot2)
library(dplyr)
library(nortest)
library(ggpubr)
library(dunn.test)
library(stringr)
library(rlang)
library(ggpattern)
library(gridExtra)
library(tidyr)

# Set working directory
setwd("C:/Users/user5012100/Desktop/Data/Models_Outputs/")

# Source custom functions
source("C:/Users/user5012100/Desktop/R/Functions_20240507.R")

##############################################
## Data Loading
##############################################
FN_FlatRF <- read.csv("Output_template_20240325_FN.csv")
FN_hML_RF <- read.csv("Output_template_hML_20240325.csv")
EB_FlatRF <- read.csv("Output_template_20240325.csv")
EB_hML_RF <- read.csv("Output_template_hML_p2_20240327.csv")
fn_missing <- read.csv("../FoodNet/FNC_Missing_samples_20240308.csv")

# Metadata
enterobase_df <- read.csv("enterobase_sistr_combined_20240201.csv")
foodnet_df <- read.csv("FN_allele_clean_v2_20240319.csv") 
enterobase_df_source <- read.csv("../../../Downloads/Ebase_source_final_20240702.csv")

# New outputs - FoodNet
hml_20240501_FN <- read.csv("new_out/Output_template_hml_20240501_FN.csv")
hml_20240530_FN <- read.csv("new_out2/Output_FN_hml_20240530.csv")

gbc_20240423_FN <- read.csv("new_out/Output_template_gbc_20240423_FN.csv")
svm_20240423_FN <- read.csv("new_out/Output_template_svm_20240423_FN.csv")
lb_20240423_FN <- read.csv("new_out/Output_template_lb_20240423_FN.csv")
rf_777_20240423_FN <- read.csv("new_out/Output_template_rf_20240423_FN.csv") 
rerun_fn_20240329 <- read.csv("Output_template_rerun_20240329.csv")
rf_20240426_fn <- read.csv("new_out/Output_rf_20240426_fn.csv") 
svm_20240426_fn <- read.csv("new_out/Output_svm_20240426_fn.csv") 
gbc_20240426_fn <- read.csv("new_out/Output_gbc_20240426_fn.csv") 
svm_20240430_fn <- read.csv("new_out/Output_template_svm_20240430_FN.csv")
gbc_20240430_fn <- read.csv("new_out/Output_template_gbc_20240430_FN.csv")
rf_20240430_fn <- read.csv("new_out/Output_template_rf_20240430_FN.csv")
flat_20240502_fn <- read.csv("new_out/Output_template_flat_20240502_FN.csv")
flat_20240510_fn <- read.csv("new_out/Output_fn_05102024.csv")
flat_20240530_fn1 <- read.csv("new_out2/Output_FN_20240530.csv")
flat_20240530_fn2 <- read.csv("new_out2/Output_fn2_20240530_v2.csv")
flat_20240610_fn <- read.csv("new_out2/Output_FN_20240610.csv")

# Public
hml_20240501_ebase <- read.csv("new_out/Output_template_hml_20240501_ebase.csv")
hml_20240502_ebase <- read.csv("new_out/Output_template_hML_20240502_ebase.csv")
hml_20240530_ebase <- read.csv("new_out2/Output_sistr_hml_20240530.csv")

rerun_ebase_20240329 <- read.csv("Output_template_rerun_ebase_20240329.csv")
flat_20240423_ebase <- read.csv("new_out/Output_template_flat_20240424_ebase.csv")
flat_20240426_ebase <- read.csv("new_out/Output_template_flat_20240426_ebase.csv") 
flat_20240430_ebase <- read.csv("new_out/Output_template_flat_20240430_ebase.csv") 
heid_20240502_ebase <- read.csv("new_out/Output_template_flat_heid_20240502_ebase.csv")
heid_unitig_ebase_20240510 <- read.csv("new_out/Output_ebase_05102024.csv")
flat_20240530_ebase1 <- read.csv("new_out2/Output_sistr_20240530.csv")
flat_20240530_ebase2 <- read.csv("new_out2/Output_sistr2_20240530.csv")
flat_20240610_ebase <- read.csv("new_out2/Output_sistr_20240610.csv")

##############################################
## Dictionary for Province Rename
##############################################
rename_dict <- c(
  "British Columbia" = "BC",
  "Alberta" = "AB",
  "Manitoba" = "MB",
  "Quebec" = "QC",
  "Ontario" = "ON",
  "Saskatchewan" = "SK",
  "Prince Edward Island" = "PE",
  "Nova Scotia" = "NS",
  "New Brunswick" = "NB",
  "Newfoundland and Labrador" = "NL"
)

##############################################
## Filtering Metadata
##############################################
foodnet <- foodnet_df %>%
  filter(!Key %in% fn_missing$Key) %>%
  select(1:6) %>%
  filter(SourceState != "Not Available")

enterobase_df <- enterobase_df %>%
  filter(qc_status != "FAIL", Accession2 != "SRR1183798")

enterobase <- enterobase_df %>%
  select(Accession2, Region, serovar_cgmlst) %>%
  rename(Key = Accession2,
         Province = Region,
         Sistr_Serovar = serovar_cgmlst) %>%
  mutate(Province = str_replace_all(Province, rename_dict)) %>%
  mutate(Province = ifelse(Province %in% c("MB", "SK"), "MB / SK", Province))

enterobase_df_source <- enterobase_df_source %>% 
  rename(Key = Accession,
         Sistr_Serovar = serovar_cgmlst)

##############################################
## Combining Data Frames from Reruns
##############################################
hML_df <- rbind(hml_20240501_FN, hml_20240501_ebase, hml_20240502_ebase, hml_20240530_FN,
                hml_20240530_ebase)

Ebase_df <- rbind(EB_FlatRF, rerun_ebase_20240329, flat_20240423_ebase, flat_20240426_ebase,
                  flat_20240430_ebase, heid_20240502_ebase, heid_unitig_ebase_20240510,
                  flat_20240530_ebase1, flat_20240530_ebase2, flat_20240610_ebase)

FN_df <- rbind(FN_FlatRF, rerun_fn_20240329, gbc_20240423_FN, svm_20240423_FN, lb_20240423_FN,
               rf_777_20240423_FN, rf_20240426_fn, svm_20240426_fn, gbc_20240426_fn,
               svm_20240430_fn, gbc_20240430_fn, rf_20240430_fn, flat_20240502_fn, 
               flat_20240510_fn, flat_20240530_fn1, flat_20240530_fn2, flat_20240610_fn)

##############################################
## Adjusting Column Names and Harmonizing
##############################################
# Add Pred_label to Ebase_df and harmonize serovar naming
Ebase_df <- Ebase_df %>%
  mutate(Pred_label = "Province") %>%
  relocate(Pred_label, .after = dataType) %>%
  mutate(Serovar = case_when(
    Serovar == "4512" ~ "I:4,[5],12:i:-",
    Serovar == "4,5,12:i:−" ~ "I:4,[5],12:i:-",
    Serovar == "kent" ~ "Kentucky",
    Serovar == "unclass" ~ "Unclassified",
    Serovar == "ty" ~ "Typhimurium",
    Serovar == "infantis" ~ "Infantis",
    Serovar == "heid" ~ "Heidelberg",
    Serovar == "entr" ~ "Enteritidis",
    TRUE ~ Serovar
  )) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, Bal_acc, Kappa, runTime_mins), ~round(.x, 2))) %>%
  mutate(Sero_calling = case_when(
    Sero_calling == "ebase" ~ "enterobase",
    Sero_calling == "None" ~ "enterobase",
    TRUE ~ Sero_calling
  ))

FN_df <- FN_df %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, Bal_acc, Kappa, runTime_mins), ~round(.x, 2)))

hML_df <- hML_df %>%
  mutate(Feat_elim_method = NA,
         sampling_strat = "RandomBalancingSampler",
         Bal_acc = NA,
         Kappa = NA,
         cv_split = NA) %>%
  relocate(Feat_elim_method, .after = n_features_used) %>%
  relocate(sampling_strat, .after = n) %>%
  relocate(Bal_acc, .after = Accuracy) %>%
  relocate(Kappa, .after = Bal_acc) %>%
  relocate(cv_split, .after = runTime_mins) %>% 
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), ~round(.x, 2))) %>%
  mutate(model_type = case_when(
    model_type == "Random Forest" ~ "hrf",
    model_type == "GradientBoostingClassifier" ~ "hgbc",
    model_type == "Logit Boost" ~ "hlb",
    model_type == "SVM" ~ "hsvm",
    TRUE ~ model_type
  )) %>%
  mutate(Serovar = case_when(
    Serovar == "4512" ~ "I:4,[5],12:i:-",
    Serovar == "4,5,12:i:−" ~ "I:4,[5],12:i:-",
    Serovar == "kent" ~ "Kentucky",
    Serovar == "unclass" ~ "Unclassified",
    Serovar == "ty" ~ "Typhimurium",
    Serovar == "infantis" ~ "Infantis",
    Serovar == "heid" ~ "Heidelberg",
    Serovar == "entr" ~ "Enteritidis",
    TRUE ~ Serovar
  )) %>%
  mutate(Sero_calling = case_when(
    Sero_calling == "ebase" ~ "enterobase",
    Sero_calling == "None" ~ "enterobase",
    TRUE ~ Sero_calling
  ))

##############################################
## Combine All ML Outputs
##############################################
all_ML_output <- rbind(FN_df, hML_df, Ebase_df) %>%
  mutate(sampling_strat = ifelse(sampling_strat == "No_strat", "None", sampling_strat),
         model_type = case_when(
           model_type == "hrf" ~ "Hierarchical Random Forest",
           model_type == "hgbc" ~ "Hierarchical GBC",
           model_type == "hlb" ~ "Hierarchical Logit Boost",
           model_type == "hsvm" ~ "Hierarchical SVM",
           model_type == "Random Forest" ~ "Flat Random Forest",
           model_type == "GradientBoostingClassifier" ~ "Flat GBC",
           model_type == "SVM" ~ "Flat SVM",
           model_type == "Logit Boost" ~ "Flat Logit Boost",
           TRUE ~ model_type
         ),
         Sero_calling = case_when(
           Sero_calling == "enterobase" ~ "Enterobase",
           Sero_calling == "sistr" ~ "SISTR_Label",
           Sero_calling == "FoodNet" ~ "FoodNet_Custom_Label",
           TRUE ~ Sero_calling
         ),
         Pred_label = ifelse(Pred_label == "Location or Food", 
                             "Farm-to-fork / Animal_Type+", Pred_label)) %>%
  select(-filter_low_provFreq, -stratified, -Optimization) %>%
  filter(Sero_calling != "Enterobase")

##############################################
## Further Cleaning
##############################################
all_output_clean2 <- all_ML_output %>% 
  filter(Serovar != "All") %>%
  distinct(across(c(1:15, 22)), .keep_all = TRUE) %>%
  mutate(n_proportion = n_features_used / n_original_features) %>%
  mutate(across(c(n_proportion), ~round(.x, 2))) 

##############################################
## Split Data Based on Sample Size
##############################################
ml_filter <- all_output_clean2 %>%
  filter(n <= 100)

ml_notFilter <- all_output_clean2 %>%
  filter(n >= 100) 

##############################################
## Creating Full Combinations for Missing Runs
##############################################
dataTypes <- c("allele", "unitig")
serovars <- c("Heidelberg", "Enteritidis", "Infantis", "I:4,[5],12:i:-", "Typhimurium", "Kentucky")
featElimMethods <- c("RFE", "RFECV", "None", NA)
seeds <- c(34, 42, 777)
samplingStrats <- c("SMOTE", "random_upsampling", "SMOTEENN", "None", "RandomBalancingSampler")
modelTypes <- c("Flat Random Forest", "Flat GBC", "Flat SVM", "Flat Logit Boost",
                "Hierarchical GBC", "Hierarchical SVM", "Hierarchical Logit Boost",
                "Hierarchical Random Forest")
testSize <- c(0.2, 0.25, 0.3)
filter_thresholds <- c(2, 3, 5, 10)
sero_callers <- c("FoodNet_Custom_Label", "SISTR_Label")

combinations <- expand.grid(
  dataType = dataTypes,
  Serovar = serovars,
  Sero_calling = sero_callers,
  Feat_elim_method = featElimMethods,
  seed = seeds,
  sampling_strat = samplingStrats,
  model_type = modelTypes,
  train_test_split = testSize,
  filter_threshold = filter_thresholds,
  stringsAsFactors = FALSE
)

filtered_combinations <- combinations %>%
  filter(!(grepl("Hierarchical", model_type) & !(sampling_strat == "RandomBalancingSampler"))) %>%
  filter(!(dataType == "unitig" & Sero_calling == "FoodNet_Custom_Label")) %>%
  filter(!(model_type == "Flat Logit Boost" & Feat_elim_method != "None")) %>%
  filter(!(grepl("Flat", model_type) & sampling_strat == "RandomBalancingSampler")) %>%
  filter(!(Sero_calling == "SISTR_Label" & !Serovar %in% c("Enteritidis", "Heidelberg", "Typhimurium"))) %>%
  filter((grepl("Hierarchical", model_type) & is.na(Feat_elim_method)) | 
           (grepl("Flat", model_type) & Feat_elim_method == "None"))

distinct_configs <- ml_notFilter %>%
  select(dataType, Serovar, Sero_calling, Feat_elim_method, train_test_split, 
         filter_threshold, sampling_strat, model_type, seed) %>%
  distinct()

missing_combinations <- anti_join(filtered_combinations, 
                                  distinct_configs, 
                                  by = c("dataType", "Serovar", "Sero_calling", 
                                         "Feat_elim_method", "train_test_split", 
                                         "filter_threshold", "sampling_strat", 
                                         "model_type", "seed")) %>%
  filter(dataType %in% c("allele", "unitig")) %>%
  filter(!sampling_strat %in% c("SMOTE", "SMOTEENN"))

missing_combinations2 <- anti_join(distinct_configs, 
                                   filtered_combinations, 
                                   by = c("dataType", "Serovar", "Sero_calling", 
                                          "Feat_elim_method", "train_test_split", 
                                          "filter_threshold", "sampling_strat", 
                                          "model_type", "seed")) %>%
  filter(dataType %in% c("allele", "unitig")) %>%
  filter(!sampling_strat %in% c("SMOTE", "SMOTEENN"))

num_cv_splits <- ml_notFilter %>% 
  group_by(dataType, Pred_label, Serovar, Sero_calling, seed, Feat_elim_method, 
           train_test_split, filter_threshold) %>%
  summarise(num_cv_splits = n_distinct(cv_split), .groups = "drop") %>%
  filter(num_cv_splits > 1)

##############################################
## Adding Sample Size Info
##############################################
fn_counts_sero <- foodnet %>%
  group_by(Serovar) %>%
  summarise(sero_counts = n(), .groups = 'drop') %>%
  filter(Serovar %in% unique(all_output_clean2$Serovar)) %>% 
  mutate(Sero_calling = "FoodNet_Custom_Label") %>%
  relocate(Sero_calling, .after = "Serovar")

eb_sero <- ml_notFilter %>% 
  filter(Sero_calling == "SISTR_Label") %>%
  pull(Serovar) %>%
  unique()

eb_counts_sero <- enterobase %>%
  tidyr::pivot_longer(cols = Sistr_Serovar, names_to = "source", values_to = "Serovar") %>%
  filter(Serovar %in% eb_sero) %>%
  mutate(Sero_calling = "SISTR_Label") %>%
  group_by(Serovar, Sero_calling) %>%
  summarise(sero_counts = n(), .groups = 'drop') %>% 
  filter(sero_counts > 100)

sero_counts_all <- rbind(fn_counts_sero, eb_counts_sero)
sero_counts_sum <- sero_counts_all %>%
  group_by(Serovar) %>%
  summarise(sero_counts = sum(sero_counts), .groups = "drop") %>%
  mutate(Sero_calling = "all") %>%
  relocate(Sero_calling, .after = "Serovar")
sero_counts_all <- rbind(sero_counts_all, sero_counts_sum)

sero_all_filtered <- sero_counts_all %>%
  filter(Sero_calling == "all") %>%
  select(-Sero_calling)

ml_notFilter <- ml_notFilter %>%
  left_join(sero_all_filtered, by = "Serovar") %>%
  mutate(Sero_counts = paste0(Serovar, "\n(n = ", sero_counts, ")")) %>%
  select(-sero_counts)

##############################################
## Label Counts and Data Type Counts
##############################################
sero_list_fn <- all_output_clean2 %>%
  filter(Sero_calling == "FoodNet_Custom_Label") %>%
  pull(Serovar) %>%
  unique()

foodnet_run <- foodnet %>%
  filter(Serovar %in% sero_list_fn)

eb_sistr <- filter(eb_counts_sero, Sero_calling == "SISTR_Label")

df_sistr <- semi_join(enterobase, eb_sistr, by = c("Sistr_Serovar" = "Serovar"))
enterobase_run <- df_sistr

ml_notFilter <- ml_notFilter %>%
  mutate(dataType_counts = case_when(
    str_detect(dataType, "allele") ~ paste0(dataType, "\n(n = ", nrow(foodnet_run) + nrow(enterobase_run), ")"),
    dataType == "unitig" ~ paste0("unitig \n(n = ", nrow(enterobase_run), ")"),
    TRUE ~ as.character(dataType)
  ),
  Label_counts = case_when(
    Pred_label == "Province" ~ paste0(Pred_label, "\n(n = ", nrow(foodnet_run) + nrow(enterobase_run), ")"),
    Pred_label == "Animal type" ~ paste0(Pred_label, "\n(n = ", nrow(foodnet_run), ")"),
    Pred_label == "Farm-to-fork / Animal_Type+" ~ paste0(Pred_label, "\n(n = ", nrow(foodnet_run), ")"),
    TRUE ~ as.character(Pred_label)
  ))

fn_label_sero_counts <- foodnet_run %>%
  filter(Serovar %in% fn_counts_sero$Serovar) %>%
  group_by(Serovar) %>%
  summarise(sero_counts = n(), .groups = "drop") %>% 
  mutate(Sero_calling = "FoodNet_Custom_Label") %>%
  relocate(Sero_calling, .after = "Serovar")

prov_label_sero_counts <- rbind(fn_label_sero_counts, eb_counts_sero) %>%
  group_by(Serovar) %>%
  summarise(label_sero_counts = sum(sero_counts), .groups = "drop")

ml_notFilter <- ml_notFilter %>%
  left_join(fn_label_sero_counts %>% select(Serovar, fn_counts = sero_counts), 
            by = "Serovar") %>%
  left_join(prov_label_sero_counts %>% select(Serovar, prov_counts = label_sero_counts),
            by = "Serovar") %>%
  mutate(cat1_counts = case_when(
    Pred_label %in% c("Animal type", "Farm-to-fork / Animal_Type+") ~ paste0(Serovar, "\n(n = ", fn_counts, ")"),
    TRUE ~ NA_character_
  ),
  cat2_counts = case_when(
    Pred_label == "Province" ~ paste0(Serovar, "\n(n = ", prov_counts, ")"),
    TRUE ~ NA_character_
  )) %>%
  mutate(Serovar_label = case_when(
    Pred_label %in% c("Animal type", "Farm-to-fork / Animal_Type+") ~ cat1_counts,
    Pred_label == "Province" ~ cat2_counts,
    TRUE ~ as.character(Serovar)
  )) %>%
  select(-fn_counts, -prov_counts)

ml_notFilter <- ml_notFilter %>%
  mutate(Sero_call_n = case_when(
    Sero_calling == "FoodNet_Custom_Label" ~ paste0(Sero_calling, " (n = ", nrow(foodnet_run), ")"),
    Sero_calling == "SISTR_Label" ~ paste0(Sero_calling, " (n = ", nrow(df_sistr), ")"),
    TRUE ~ as.character(Sero_calling)
  ))

sample_strat <- ml_notFilter %>%
  group_by(sampling_strat) %>%
  summarise(n_permute = n(), .groups = "drop")

ml_notFilter <- ml_notFilter %>%
  left_join(sample_strat, by = "sampling_strat") %>%
  mutate(sampling_strat_n = paste0(sampling_strat, "\n(n_permute = ", n_permute, ")"))

##############################################
## Normality Tests (for reference)
##############################################
# Example: 
# ad.test(ml_notFilter$F1)
# qqnorm(ml_notFilter$F1); qqline(ml_notFilter$F1, col = "blue")

##############################################
## Statistical Tests and Plotting (Examples)
##############################################
# Example kruskal test and dunn test
kruskal.test(F1 ~ seed, data = ml_notFilter)
dunn_seed <- dunn.test(ml_notFilter$F1, ml_notFilter$seed, method = "bonferroni")

# Create a few example plots (You can organize these better or put in another script)
seed_plot <- ggplot(ml_notFilter, aes(x = factor(seed), y = F1)) + 
  geom_violin(trim = TRUE, fill = "pink", scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw() +
  coord_flip() + 
  labs(x = "Seed")
kruskal.test(F1 ~ seed, data = ml_notFilter)
dunn_seed <- dunn.test(ml_notFilter$F1, ml_notFilter$seed, method = "bonferroni")

dataType_plot <- ggplot(ml_notFilter, aes(x = dataType_counts, y = F1)) + 
  geom_violin(trim = T, fill = "orange", scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw() +
  coord_flip() + 
  labs(x = "Data Type")
kruskal.test(F1 ~ dataType, data = ml_notFilter)
dunn_dataType <- dunn.test(ml_notFilter$F1, ml_notFilter$dataType, method = "bonferroni")

SeroCount_plot <- ggplot(ml_notFilter, aes(x = Sero_counts, y = F1)) + 
  geom_boxplot(fill = "green") +
  # geom_violin(trim = T, fill = "green", scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) + 
  coord_flip() +
  theme_bw() + 
  labs(x = "Serovar")
kruskal.test(F1 ~ Serovar, data = ml_notFilter)
dunn_SeroCount <- dunn.test(ml_notFilter$F1, ml_notFilter$Serovar, method = "bonferroni")

label_plot <- ggplot(ml_notFilter, aes(x = Label_counts, y = F1)) + 
  geom_violin(trim = T, fill = "blue", scale = "count") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Predicted label") 
kruskal.test(F1 ~ Pred_label, data = ml_notFilter)
dunn_label <- dunn.test(ml_notFilter$F1, ml_notFilter$Pred_label, method = "bonferroni")

sero_call_plot <- ggplot(ml_notFilter, aes(x = Sero_call_n, y = F1)) + 
  geom_violin(trim = T, fill = "grey", scale = "count") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Dataset Label")
kruskal.test(F1 ~ Sero_calling, data = ml_notFilter)
dunn_sero_call <- dunn.test(ml_notFilter$F1, ml_notFilter$Sero_calling, method = "bonferroni")

model_plot <- ggplot(ml_notFilter, aes(x = model_type, y = F1)) + 
  geom_boxplot(fill = "red") + 
  # geom_violin(trim = T, fill = "red") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Model")
kruskal.test(F1 ~ model_type, data = ml_notFilter)
dunn_model <- dunn.test(ml_notFilter$F1, ml_notFilter$model_type)

# new df for Felim to prevent plotting NAs
felim_df <- ml_notFilter %>%
  filter(!is.na(Feat_elim_method))
Felim_plot <- ggplot(felim_df, aes(x = Feat_elim_method, y = F1)) + 
  geom_violin(trim = T, fill = "maroon") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Feat Elim Method")
kruskal.test(F1 ~ Feat_elim_method, data = ml_notFilter) # No sig difs
dunn_Felim <- dunn.test(ml_notFilter$F1, ml_notFilter$Feat_elim_method)

sample_strat_plot <- ggplot(ml_notFilter, aes(x = sampling_strat_n, y = F1)) + 
  geom_boxplot(fill = "skyblue") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Sampling Strategy")
kruskal.test(F1 ~ sampling_strat, data = ml_notFilter) 
dunn_sample_strat <- dunn.test(ml_notFilter$F1, ml_notFilter$sampling_strat)

filter_threshold_plot <- ggplot(ml_notFilter, aes(x = factor(filter_threshold), y = F1)) + 
  geom_violin(trim = T, fill = "purple", scale = "count") + 
  theme_bw() +
  coord_flip() + 
  labs(x = "Filter Threshold")
kruskal.test(F1 ~ filter_threshold, data = ml_notFilter) 
dunn_filter_threshold <- dunn.test(ml_notFilter$F1, ml_notFilter$filter_threshold)

ggarrange(seed_plot, sero_call_plot, label_plot, dataType_plot, 
          SeroCount_plot, model_plot, Felim_plot, 
          sample_strat_plot, filter_threshold_plot, 
          align = "hv", labels="AUTO", ncol = 3, nrow = 3)

######## Function for facet_grid plots ############
fn_keep_serov <- ml_notFilter %>% 
  filter(Sero_calling == "FoodNet_Custom_Label") %>%
  distinct(Serovar) %>%
  pull(Serovar)

sistr_keep_serov <- ml_notFilter %>% 
  filter(Sero_calling == "SISTR_Label") %>%
  distinct(Serovar) %>%
  pull(Serovar)

fn_class_count <- foodnet %>%
  filter(Serovar %in% fn_keep_serov) %>%
  group_by(Serovar) %>%
  summarise(fn_prov_class = n_distinct(SourceState),
            Animal_type = n_distinct(CombinedSites),
            Animal_typePlus = n_distinct(CombinedSite2)) %>%
  mutate(Sero_calling = "FoodNet_Custom_Label")

sistr_class_count <- enterobase %>%
  filter(Sistr_Serovar %in% sistr_keep_serov) %>%
  group_by(Sistr_Serovar) %>%
  summarise(sistr_prov_class = n_distinct(Province)) %>%
  mutate(Sero_calling = "SISTR_Label") %>%
  rename(Serovar = Sistr_Serovar)

ml_notFilter_v2 <- left_join(ml_notFilter, fn_class_count, by = c("Serovar", "Sero_calling")) 
ml_notFilter_v2 <- left_join(ml_notFilter_v2, sistr_class_count, by = c("Serovar", "Sero_calling"))
ml_notFilter_v2 <- ml_notFilter_v2 %>%
  mutate(label_count = if_else(label_filtered == "[]", 
                               0, 
                               str_count(label_filtered, "',") + 1)) %>%
  relocate(label_count, .after = label_filtered)

ml_notFilter_v2 <- ml_notFilter_v2 %>% 
  mutate(num_pred = case_when(
    Pred_label == "Province" & Sero_calling == "FoodNet_Custom_Label" ~ fn_prov_class - label_count,
    Pred_label == "Animal type" & Sero_calling == "FoodNet_Custom_Label" ~ Animal_type - label_count,
    Pred_label == "Farm-to-fork / Animal_Type+" & Sero_calling == "FoodNet_Custom_Label" ~ Animal_typePlus - label_count,
    Pred_label == "Province" & Sero_calling == "SISTR_Label" ~ sistr_prov_class - label_count,
    TRUE ~ NA_real_  
  ))

# List of columns to iterate for plots 
x_col_list <- c("dataType", "Pred_label", "Sero_calling", "Feat_elim_method",
                "train_test_split", "model_type", "sampling_strat", "Serovar", 
                "filter_threshold")
y_col_list <- c("F1", "Kappa", "runTime_mins")

# List of serovars
serovar_list <- c("Heidelberg", "Enteritidis", "Infantis", "I:4,[5],12:i:-", "Typhimurium", "Kentucky")
# List of predicted labels

pred_label_list <- c("Province", "Animal type", "Farm-to-fork / Animal_Type+") 
# List of variables to compare against F1

variables_list <- c("dataType", "Pred_label", "Feat_elim_method", "train_test_split",
                    "sampling_strat", "filter_threshold", "model_type") 

## Plotting for specific serovars within each dataset ##
## SISTR with different Serovars ## 
ml_notFilter_v2 %>%
  filter(Sero_calling == "SISTR_Label") %>%
  create_custom_plot(
    x_col = x_col_list[1],
    y_col = y_col_list[1],
    fill_col = x_col_list[9],
    facet_cols = c(x_col_list[9], x_col_list[7])
  )

## SISTR with different filter_thresholds ##
plot2 <- ml_notFilter_v2 %>%
  filter(Sero_calling == "SISTR_Label") %>%
  create_custom_plot(
    x_col = x_col_list[1],
    y_col = y_col_list[1],
    fill_col = x_col_list[8],
    facet_cols = c(x_col_list[9], x_col_list[6])
  )

ml_notFilter_v2 %>%
  filter(dataType != "unitig") %>%
  create_custom_plot(
    x_col = x_col_list[1],
    y_col = y_col_list[1],
    fill_col = x_col_list[8],
    facet_cols = c(x_col_list[3], x_col_list[2])
  )


## FN data per Serovar ##
# Foodnet and Heidelberg 
fn_heid_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[1], 
                              y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_heid_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                               serovar_filter = serovar_list[1], 
                               pred_label = pred_label_list[1],
                               y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                               nrow = 2, ncol = 4,
                               variables_list = variables_list)
fn_heid_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                 serovar_filter = serovar_list[1], 
                                 pred_label = pred_label_list[2],
                                 y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
fn_heid_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                     serovar_filter = serovar_list[1], 
                                     pred_label = pred_label_list[3],
                                     y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                     nrow = 2, ncol = 4,
                                     variables_list = variables_list)
# Foodnet and Enteritidis
fn_entr_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[2], 
                              y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_entr_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                               serovar_filter = serovar_list[2], 
                               pred_label = pred_label_list[1],
                               y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                               nrow = 2, ncol = 4,
                               variables_list = variables_list)
fn_entr_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                 serovar_filter = serovar_list[2], 
                                 pred_label = pred_label_list[2],
                                 y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
fn_entr_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                     serovar_filter = serovar_list[2], 
                                     pred_label = pred_label_list[3],
                                     y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                     nrow = 2, ncol = 4,
                                     variables_list = variables_list)
# Foodnet and Infantis
fn_inf_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                             serovar_filter = serovar_list[3], 
                             y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                             nrow = 2, ncol = 4,
                             variables_list = variables_list)
fn_inf_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[3], 
                              pred_label = pred_label_list[1],
                              y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_inf_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                serovar_filter = serovar_list[3], 
                                pred_label = pred_label_list[2],
                                y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                nrow = 2, ncol = 4,
                                variables_list = variables_list)
fn_inf_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                    serovar_filter = serovar_list[3], 
                                    pred_label = pred_label_list[3],
                                    y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                    nrow = 2, ncol = 4,
                                    variables_list = variables_list)
# Foodnet and 4512
fn_4512_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[4], 
                              y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_4512_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                               serovar_filter = serovar_list[4], 
                               pred_label = pred_label_list[1],
                               y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                               nrow = 2, ncol = 4,
                               variables_list = variables_list)
fn_4512_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                 serovar_filter = serovar_list[4], 
                                 pred_label = pred_label_list[2],
                                 y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
fn_4512_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                     serovar_filter = serovar_list[4], 
                                     pred_label = pred_label_list[3],
                                     y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                     nrow = 2, ncol = 4,
                                     variables_list = variables_list)
# Foodnet and Typh
fn_typh_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[5], 
                              y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_typh_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                               serovar_filter = serovar_list[5], 
                               pred_label = pred_label_list[1],
                               y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                               nrow = 2, ncol = 4,
                               variables_list = variables_list)
fn_typh_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                 serovar_filter = serovar_list[5], 
                                 pred_label = pred_label_list[2],
                                 y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
fn_typh_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                     serovar_filter = serovar_list[5], 
                                     pred_label = pred_label_list[3],
                                     y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                     nrow = 2, ncol = 4,
                                     variables_list = variables_list)
# Foodnet and Kent
fn_kent_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                              serovar_filter = serovar_list[6], 
                              y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                              nrow = 2, ncol = 4,
                              variables_list = variables_list)
fn_kent_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                               serovar_filter = serovar_list[6], 
                               pred_label = pred_label_list[1],
                               y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                               nrow = 2, ncol = 4,
                               variables_list = variables_list)
fn_kent_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                 serovar_filter = serovar_list[6], 
                                 pred_label = pred_label_list[2],
                                 y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
fn_kent_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "FoodNet_Custom_Label", 
                                     serovar_filter = serovar_list[6], 
                                     pred_label = pred_label_list[3],
                                     y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                     nrow = 2, ncol = 4,
                                     variables_list = variables_list)
# SISTR and Heidelberg
sistr_heid_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                 serovar_filter = serovar_list[1], 
                                 y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
sistr_heid_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                  serovar_filter = serovar_list[1], 
                                  pred_label = pred_label_list[1],
                                  y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                  nrow = 2, ncol = 4,
                                  variables_list = variables_list)
# sistr_heid_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
#                                       serovar_filter = serovar_list[1],
#                                       pred_label = pred_label_list[2],
#                                       y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
#                                       nrow = 2, ncol = 4,
#                                       variables_list = variables_list)
# sistr_heid_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
#                                           serovar_filter = serovar_list[1],
#                                           pred_label = pred_label_list[3],
#                                           y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
#                                           nrow = 2, ncol = 4,
#                                           variables_list = variables_list)
# SISTR and Enteritidis
sistr_entr_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                 serovar_filter = serovar_list[2], 
                                 y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
sistr_entr_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                  serovar_filter = serovar_list[2], 
                                  pred_label = pred_label_list[1],
                                  y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                  nrow = 2, ncol = 4,
                                  variables_list = variables_list)
sistr_entr_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
                                  serovar_filter = serovar_list[2],
                                  pred_label = pred_label_list[2],
                                  y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
                                  nrow = 2, ncol = 4,
                                  variables_list = variables_list)
sistr_entr_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
                                      serovar_filter = serovar_list[2],
                                      pred_label = pred_label_list[3],
                                      y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
                                      nrow = 2, ncol = 4,
                                      variables_list = variables_list)
# SISTR and Typhimurium
sistr_typh_all <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                 serovar_filter = serovar_list[5], 
                                 y_col = y_col_list[1], x_col_indices = c(1:2, 4:7, 9), 
                                 nrow = 2, ncol = 4,
                                 variables_list = variables_list)
sistr_typh_prov <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label", 
                                  serovar_filter = serovar_list[5], 
                                  pred_label = pred_label_list[1],
                                  y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9), 
                                  nrow = 2, ncol = 4,
                                  variables_list = variables_list)
sistr_typh_animal <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
                                      serovar_filter = serovar_list[5],
                                      pred_label = pred_label_list[2],
                                      y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
                                      nrow = 2, ncol = 4,
                                      variables_list = variables_list)
sistr_typh_animalPlus <- param_analysis(data = ml_notFilter_v2, sero_calling_filter = "SISTR_Label",
                                          serovar_filter = serovar_list[5],
                                          pred_label = pred_label_list[3],
                                          y_col = y_col_list[1], x_col_indices = c(1, 4:7, 9),
                                          nrow = 2, ncol = 4,
                                          variables_list = variables_list)

### Checking RFE / RunTime plots ###
create_boxplot_and_dunn_test <- function(data, pred_label, serovar, sero_calling, fe_method, y_var, x_var) {
  # Filter the data
  filtered_data <- data %>% 
    filter(Pred_label == pred_label, 
           Serovar == serovar, 
           Sero_calling == sero_calling, 
           FE_method == fe_method)
  
  # Create the boxplot
  boxplot <- filtered_data %>% 
    ggplot(aes_string(x = x_var, y = y_var)) +
    geom_boxplot() +
    xlab(x_var) +
    ylab(y_var) +
    ggtitle(paste("Boxplot of", y_var, "by", x_var))
  
  # Display the boxplot
  print(boxplot)
  
  # Perform Dunn's test
  dunn_test_result <- dunn.test(filtered_data[[y_var]], as.factor(filtered_data[[x_var]]), method = "bonferroni")
  
  # Print the Dunn's test result
  print(dunn_test_result)
  
  # Return the results
  return(list(Boxplot = boxplot, DunnTestResult = dunn_test_result))
}

# Example usage
rfe_heid_animal <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                           data_type = c("allele", "OHE_allele"),
                                           pred_label = "Animal type", 
                                           filter_threshold = c(2, 3, 5),
                                           all_model = "Flat SVM",
                                           serovar = "Heidelberg", 
                                           sero_calling = "FoodNet_Custom_Label", 
                                           sampling_strat = "SMOTE", 
                                           fe_method = c("RFE", "RFECV"), 
                                           y_var = "F1", 
                                           x_var = "n_proportion")
rfe_entr_prov <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                         data_type = c("allele", "OHE_allele"),
                                         pred_label = "Province", 
                                         filter_threshold = 10,
                                         all_model = c("Flat SVM", "Hierarchical Random Forest"), 
                                         serovar = "Enteritidis", 
                                         sero_calling = "FoodNet_Custom_Label", 
                                         sampling_strat = "SMOTE", 
                                         fe_method = c("RFE", "RFECV"), 
                                         y_var = "F1", 
                                         x_var = "n_proportion")
rfe_infantis_all <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                            data_type = c("nzv_OHE_allele", "nzv_allele"),
                                            #pred_label = "Province", 
                                            filter_threshold = 10,
                                            all_model = c("Flat GBC", "Flat Random Forest"), 
                                            serovar = "Infantis", 
                                            sero_calling = "FoodNet_Custom_Label",
                                            sampling_strat = "SMOTE", 
                                            fe_method = c("RFE", "RFECV"), 
                                            y_var = "F1", 
                                            x_var = "n_proportion")
rfe_infantis_prov <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                             data_type = c("nzv_OHE_allele", "nzv_allele"),
                                             pred_label = "Province", 
                                             filter_threshold = 10,
                                             all_model = c("Flat SVM", "Flat Random Forest"), 
                                             serovar = "Infantis", 
                                             sero_calling = "FoodNet_Custom_Label", 
                                             sampling_strat = "SMOTE",
                                             fe_method = c("RFE", "RFECV"), 
                                             y_var = "F1", 
                                             x_var = "n_proportion")
rfe_typh_all <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                        data_type = c("OHE_allele"),
                                        filter_threshold = 10,
                                        all_model = c("Flat Random Forest"), 
                                        serovar = "Typhimurium", 
                                        sero_calling = "FoodNet_Custom_Label", 
                                        sampling_strat = "SMOTE",
                                        fe_method = c("RFE", "RFECV"), 
                                        y_var = "F1", 
                                        x_var = "n_proportion")
rfe_typh_prov <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                         data_type = c("allele", "OHE_allele"),
                                         pred_label = "Province", 
                                         filter_threshold = 10,
                                         all_model = c("Flat Random Forest"), 
                                         serovar = "Typhimurium", 
                                         sero_calling = "FoodNet_Custom_Label", 
                                         sampling_strat = "SMOTE",
                                         test_split = c(0.2, 0.3), 
                                         fe_method = c("RFE", "RFECV"), 
                                         y_var = "F1", 
                                         x_var = "n_proportion")
rfe_typh_animal <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                           data_type = c("OHE_allele"),
                                           pred_label = "Animal type", 
                                           filter_threshold = 10,
                                           all_model = c("Flat SVM"), 
                                           serovar = "Typhimurium", 
                                           sero_calling = "FoodNet_Custom_Label", 
                                           sampling_strat = "SMOTE",
                                           test_split = c(0.2), 
                                           fe_method = c("RFECV"), 
                                           y_var = "F1", 
                                           x_var = "n_proportion")
rfe_typh_animal <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                           data_type = c("OHE_allele"),
                                           pred_label = "Animal type", 
                                           filter_threshold = 10,
                                           all_model = c("Flat SVM"), 
                                           serovar = "Typhimurium", 
                                           sero_calling = "FoodNet_Custom_Label", 
                                           sampling_strat = "SMOTE",
                                           test_split = c(0.2), 
                                           fe_method = c("RFECV"), 
                                           y_var = "F1", 
                                           x_var = "n_proportion")
rfe_kent_all <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                        data_type = c("allele", "OHE_allele"),
                                        #pred_label = "Animal type", 
                                        filter_threshold = 5,
                                        all_model = c("Flat SVM", "Flat GBC"), 
                                        serovar = "Kentucky", 
                                        sero_calling = "FoodNet_Custom_Label", 
                                        sampling_strat = c("SMOTE", "random_upsampling"),
                                        fe_method = c("RFE", "RFECV"), 
                                        y_var = "F1", 
                                        x_var = "n_proportion")
rfe_kent_prov <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                         data_type = c("allele", "OHE_allele"),
                                         pred_label = "Province", 
                                         filter_threshold = c(5, 10),
                                         all_model = c("Flat SVM", "Flat GBC"), 
                                         serovar = "Kentucky", 
                                         sero_calling = "FoodNet_Custom_Label", 
                                         sampling_strat = c("SMOTE"),
                                         fe_method = c("RFE", "RFECV"), 
                                         test_split = c(0.2, 0.25), 
                                         y_var = "F1", 
                                         x_var = "n_proportion")
rfe_kent_animalPlus <- RFE_proportion_analysis(data = ml_notFilter_v2, 
                                               data_type = c("allele"),
                                               pred_label = "Farm-to-fork / Animal_Type+", 
                                               filter_threshold = c(10),
                                               all_model = c("Flat GBC"), 
                                               serovar = "Kentucky", 
                                               sero_calling = "FoodNet_Custom_Label", 
                                               sampling_strat = c("SMOTE"),
                                               fe_method = c("RFE", "RFECV"), 
                                               y_var = "F1", 
                                               x_var = "n_proportion")

plot1 <- ggplot(ml_notFilter, aes(x = Serovar, y = F1, fill = Serovar)) +
  geom_boxplot() +
  facet_grid(Sero_calling ~ Label_counts, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = c(0, 1.0))

plot2 <- ggplot(ml_notFilter, aes(x = Serovar, y = F1, fill = Serovar)) +
  geom_boxplot() +
  facet_grid(Sero_calling ~ model_type, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = c(0, 1.0))

plot3 <- ggplot(ml_notFilter, aes(x = Serovar, y = Accuracy, fill = Serovar)) +
  geom_boxplot() +
  facet_grid(Sero_calling ~ Label_counts, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = c(0, 1.0))

plot4 <- ggplot(ml_notFilter, aes(x = Serovar, y = Accuracy, fill = Serovar)) +
  geom_boxplot() +
  facet_grid(Sero_calling ~ model_type, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = c(0, 1.0))

ggarrange(plot1, plot2, align = "hv", labels = "AUTO", common.legend = T, legend = "bottom")
ggarrange(plot3, plot4, align = "hv", labels = "AUTO", common.legend = T, legend = "bottom")
ggarrange(plot1, plot3, align = "hv", labels = "AUTO", common.legend = T, legend = "bottom")
ggarrange(plot2, plot4, align = "hv", labels = "AUTO", common.legend = T, legend = "bottom")

### Finding labels that are filtered out ####
# Usage with SourceState
filter_fn_prov <- group_and_filter(foodnet, "foodnet", "Serovar", fn_keep_serov, 
                                   c("Serovar", "SourceState"), "count", c(2, 3, 5, 10))
filter_pub_prov <- group_and_filter(enterobase, "public", "Sistr_Serovar", sistr_keep_serov, 
                                    c("Sistr_Serovar", "Province"), "count", c(2, 3, 5, 10))

# Combine the results into a single data frame
comb_filter_fn_prov <- combine_results(filter_fn_prov, "Serovar", "SourceState")
comb_filter_ebase_prov <- combine_results(filter_pub_prov, "Sistr_Serovar", "Province") %>%
  rename(Serovar = Sistr_Serovar)
filter_all_prov <- rbind(comb_filter_fn_prov, comb_filter_ebase_prov) %>% 
  relocate(dataset, .before = "Serovar") %>%
  mutate(Label = "Province") %>%
  relocate(Label, .after = "dataset")

# Usage with host
filter_fn_animal <- group_and_filter(foodnet, "foodnet", "Serovar", fn_keep_serov, 
                                     c("Serovar", "CombinedSites"), "count", c(2, 3, 5, 10))
comb_filter_fn_animal <- combine_results(filter_fn_animal, "Serovar", "CombinedSites") %>%
  mutate(Label = "Animal") %>%
  relocate(Label, .after = "dataset")
filter_fn_animalPlus <- group_and_filter(foodnet, "foodnet", "Serovar", fn_keep_serov, 
                                         c("Serovar", "CombinedSite2"), "count", c(2, 3, 5, 10))
comb_filter_fn_animalPlus <- combine_results(filter_fn_animalPlus, "Serovar", "CombinedSite2") %>%
  mutate(Label = "AnimalPlus") %>%
  relocate(Label, .after = "dataset")
filter_fn_animals <- rbind(comb_filter_fn_animal, comb_filter_fn_animalPlus) %>%
  relocate(dataset, .before = "Serovar")
filter_all_labels <- rbind(filter_all_prov, filter_fn_animals)

####### EDA Scatter plot ######
scatter_labels <- ggplot(ml_notFilter_v2, aes(x = num_pred, y = F1)) +
  geom_rect(data = ml_notFilter_v2, aes(xmin = case_when(
    Pred_label == "Animal type" ~ max(Animal_type, na.rm = TRUE),
    Pred_label == "Farm-to-fork / Animal_Type+" ~ max(Animal_typePlus, na.rm = TRUE),
    Pred_label == "Province" ~ max(c(fn_prov_class, sistr_prov_class), na.rm = TRUE),
    TRUE ~ as.numeric(NA)  # To ensure it does not affect other panels
  ) - 0.5, xmax = case_when(
    Pred_label == "Animal type" ~ max(Animal_type, na.rm = TRUE),
    Pred_label == "Farm-to-fork / Animal_Type+" ~ max(Animal_typePlus, na.rm = TRUE),
    Pred_label == "Province" ~ max(c(fn_prov_class, sistr_prov_class), na.rm = TRUE),
    TRUE ~ as.numeric(NA)
  ) + 0.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
  geom_point(aes(color = as.factor(filter_threshold), size = n), alpha = 0.7) +
  geom_smooth(method = "loess", span = 0.7, se = FALSE, color = "blue") +
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 4.5, label.y = 1.0) +  
  scale_x_continuous(breaks = 3:10) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  scale_color_manual(values = c("#CC79A7", "#22A884FF", "#FDE725FF")) + 
  theme_bw() +  
  facet_wrap(~Pred_label, scales = "free_y") + 
  labs(
    x = "Predicted labels (n)", 
    y = "F1 score", 
    color = "Filter threshold", 
    size = "Sample size"
  ) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 12))

scatter_all <- ggplot(ml_notFilter_v2, aes(x = num_pred, y = F1)) +
  geom_point(aes(color = as.factor(filter_threshold), size = n), alpha = 0.7) +
  # geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  # geom_smooth(aes(color = as.factor(filter_threshold)), method = "loess", se = FALSE) +
  geom_smooth(method = "loess", span = 0.7, se = FALSE) + 
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 6.5, label.y = 1.0) +  
  scale_x_continuous(limits = c(3, 10), breaks = 3:10) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  scale_color_manual(values = c("#CC79A7", "#22A884FF", "#FDE725FF")) + 
  theme_bw() +  
  labs(
    x = "Predicted labels (n)", 
    y = "F1 score", 
    color = "Filter threshold", 
    size = "Sample size"
  ) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 12))

scatter_filter <- ggplot(ml_notFilter_v2, aes(x = num_pred, y = F1)) +
  geom_point(aes(color = as.factor(filter_threshold), size = n), alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, span = 0.8, color = "blue") +
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 4.5, label.y = 1.0) +  
  scale_x_continuous(limits = c(3, 10), breaks = 3:10) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  scale_color_manual(values = c("#CC79A7", "#22A884FF", "#FDE725FF")) + 
  theme_bw() +  
  facet_wrap(~filter_threshold, scales = "free_y") + 
  labs(
    x = "Predicted labels (n)", 
    y = "F1 score", 
    color = "Filter threshold", 
    size = "Sample size", 
  ) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 12))

scatter_sero <- ggplot(ml_notFilter_v2, aes(x = num_pred, y = F1)) +
  geom_point(aes(color = as.factor(filter_threshold), size = n), alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, span = 0.8, color = "blue") +
  stat_cor(method = "spearman", cor.coef.name = "rho", label.x = 4.5, label.y = 1.0) +  
  scale_x_continuous(limits = c(3, 10), breaks = 3:10) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  scale_color_manual(values = c("#CC79A7", "#22A884FF", "#FDE725FF")) + 
  theme_bw() +  
  facet_grid(Pred_label ~ Serovar, scales = "free_y") + 
  labs(
    x = "Predicted labels (n)", 
    y = "F1 score", 
    color = "Filter threshold", 
    size = "Sample size", 
  ) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 12))

count_data <- ml_notFilter_v2 %>%
  group_by(num_pred) %>%
  summarise(count = n())

histogram_plot <- ggplot(count_data, aes(x = num_pred, y = count)) +
  geom_col() +  
  scale_x_continuous(limits = c(2.5, 10), breaks = 3:10) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 5000)) +
  labs(x = "Predicted labels (n)", y = "Count") +
  theme_bw()
# 
# ggarrange(histogram_plot, scatter_all, scatter_labels, scatter_filter,
#           ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom", align = "hv", labels = "AUTO")

num_pred_bar <- ml_notFilter_v2 %>%
  filter(Sero_calling == "SISTR_Label") %>%
  ggplot(aes(x = dataType, y = num_pred, fill = Serovar)) + 
  geom_col(position = "dodge") + 
  facet_grid(x_col_list[6], scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) 
ggarrange(plot2, plot, align = "hv", labels = "AUTO", common.legend = T,
          legend = "bottom", ncol = 1, nrow = 2)

############################
##Looping Dunn's test #####
############################

# List of variables to compare against F1
variables_list <- c("seed", "dataType", "Serovar", "Pred_label", "Sero_calling", "Feat_elim_method",
                    "sampling_strat", "filter_threshold", "model_type") # Add more as needed

# Initialize an empty dataframe to store results
combined_df <- data.frame(Variable = character(), Pairs = character(), Bonferroni_adj_P_value = numeric(), stringsAsFactors = FALSE)

# Loop through each variable and perform dunn.test
for(variable in variables_list) {
  # Perform dunn.test comparing F1 against the current variable
  test <- dunn.test(ml_notFilter$F1, ml_notFilter[[variable]])
  
  # Create a dataframe for this test's results, rounding P-values to 3 decimal places
  temp_df <- data.frame(
    Variable = rep(variable, length(test$comparisons)), # Repeat the variable name
    Pairs = test$comparisons,
    Bonferroni_adj_P_value = round(test$P.adjusted, digits = 3), # Round P-values here
    stringsAsFactors = FALSE
  )
  
  # Combine with the main dataframe
  combined_df <- rbind(combined_df, temp_df)
}

## Showing the breakdown of counts for each Serovar based on the overall typing scheme (FN/Ebase) and
## the different dataTypes that were done for each

ggplot(ml_notFilter, aes(x = Serovar, y = n)) +
  geom_bar(stat = "identity", position = "dodge", fill = "navy") +  
  facet_grid(dataType ~ Sero_calling, scales = "free_y") +  
  theme_bw() +
  labs(y = "Count", x = "Combined Sites") +
  coord_flip() +  
  theme(legend.position = "none") 

# Create a scatter plot with a 95% CI for the regression line
ggscatter(ml_notFilter, x = "n_proportion", y = "F1", 
          add = "reg.line",  
          conf.int = TRUE,   
          cor.coef = TRUE,   
          cor.method = "spearman", 
          xlab = "Column 1", 
          ylab = "Column 2", 
          title = "Scatter Plot of Column 1 vs Column 2 with 95% CI"
)

ggplot(ml_notFilter, aes(x = log10(n_proportion), y = F1)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  # Add regression line with 95% CI
  theme_bw() +  # Use the theme_bw theme
  labs(
    title = "Scatter Plot of log10(Column 1) vs Column 2 with 95% CI",
    x = "log10(Column 1)",
    y = "Column 2"
  ) +
  stat_cor(method = "spearman")  # Add Spearman correlation coefficient

######### Class imbalance figs - FN/Enterobase #########
site_colors <- c("Chicken" = "#1f77b4", "Beef" = "#ff7f0e", "Water" = "#2ca02c", "Pig" = "#d62728",
                 "Turkey" = "#9467bd", "Dairy" = "#7f7f7f")
site2_colors <- c("Chicken_animal" = "#1f77b4", "Chicken_food" = "#8c564b", "Beef_animal" = "#ff7f0e", 
                  "Water" = "#2ca02c", "Pig_animal" = "#d62728", "Pig_food" = "maroon", 
                  "Turkey_animal" = "#9467bd", "Turkey_food" = "darkblue",  
                  "Dairy_animal" = "#7f7f7f", "Beef_food" = "pink")
fn_prov_colors <- c("BC" = "#1f77b4", "AB" = "#ff7f0e", "ON" = "#2ca02c", "QC" = "#d62728",
                    "NS" = "#9467bd", "MB / SK" = "#8c564b", "PE" = "#e377c2", "NB" = "#7f7f7f")
ebase_prov_colors <- c("BC" = "#1f77b4", "ON" = "#2ca02c", "QC" = "#d62728",
                       "AB" = "#ff7f0e", "NS" = "#9467bd", "MB / SK" = "#8c564b",
                       "NL" = "maroon", "NB" = "#7f7f7f", "PE" = "#e377c2")
enterobase <- enterobase %>%
  mutate(Sistr_Serovar = ifelse(Sistr_Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Sistr_Serovar))

enterobase_clean <- enterobase_df %>% 
  select(Accession2, Region, 54:3072) 
enterobase_clean <- enterobase_clean %>%
  select(-(3005:3020)) %>%
  relocate(serovar_cgmlst, .after = Region)
enterobase_clean <- enterobase_clean %>%
  rename(Key = Accession2,
         Province = Region,
         Sistr_Serovar = serovar_cgmlst) %>%
  mutate(Province = str_replace_all(Province, rename_dict)) 
foodnet_df2 <- foodnet_df %>%
  filter(SourceState != "Not Available")

# Clean and filtered dfs for plotting
enterobase_clean2 <- enterobase_clean %>%
  filter(Sistr_Serovar %in% sistr_keep_serov) 
foodnet_clean <- foodnet_df %>%
  filter(Serovar %in% fn_keep_serov) %>%
  filter(SourceState != "Not Available") 

enterobase_df_source <- enterobase_df_source %>%
  mutate(CombinedSites = ifelse(CombinedSites == "Pork", "Pig", CombinedSites),
         Sistr_Serovar = ifelse(Sistr_Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Sistr_Serovar))

df <- foodnet_df2 %>%
  filter(Serovar %in% c("Enteritidis", "I:4,[5],12:i:-")) %>%
  group_by(CombinedSite2) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  filter(count >= 10) %>%
  mutate(sero_counts = sum(count)) %>%
  ungroup() 

df_top6_combined <- foodnet_clean %>%
  mutate(Serovar = ifelse(Serovar %in% fn_keep_serov, Serovar, "top6")) %>%
  group_by(Serovar, CombinedSites) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup() %>% 
  group_by(CombinedSites) %>%
  mutate(sero_counts = sum(count)) %>%
  ungroup() %>%
  mutate(Sero_counts = ifelse(Serovar == "top6", paste0("top6\n(n = ", sero_counts, ")"), 
                              paste0(Serovar, "\n(n = ", sero_counts, ")")))

# Plotting
fn_prov_f5 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "SourceState", "Province", 
                                    5, fn_prov_colors, fn_keep_serov, "Top_6")
fn_prov_f10 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "SourceState", "Province", 
                                     10, fn_prov_colors, fn_keep_serov, "Top_6") 
fn_animal_f5 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSites", "Animal", 
                                      5, site_colors, fn_keep_serov, "Top_6")
fn_animal_f10 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSites", "Animal", 
                                       10, site_colors, fn_keep_serov, "Top_6")
fn_animalPlus_f5 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSite2", 
                                          "AnimalPlus", 5, site2_colors, fn_keep_serov, "Top_6")
fn_animalPlus_f10 <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSite2", 
                                           "AnimalPlus", 10, site2_colors, fn_keep_serov, "Top_6")

ebase_prov_f5 <- serovar_class_summary(enterobase_clean2, enterobase_clean,"Sistr_Serovar", "Province", 
                                       "Province", 5, ebase_prov_colors, sistr_keep_serov, "Top_3")
ebase_prov_f10 <- serovar_class_summary(enterobase_clean2, enterobase_clean,"Sistr_Serovar", "Province", 
                                        "Province", 10, ebase_prov_colors, sistr_keep_serov, "Top_3")
ebase_animal_f10 <- serovar_class_summary(enterobase_df_source, enterobase_df_source, "Sistr_Serovar", "CombinedSites", 
                                          "Animal", 10, site_colors, sistr_keep_serov, "Top_3")
ebase_animalPlus_f10 <- serovar_class_summary(enterobase_df_source, enterobase_df_source, "Sistr_Serovar", "CombinedSite2", 
                                              "Animal", 10, site2_colors, sistr_keep_serov, "Top_3")

fn_prov_all <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "SourceState", "Province", 
                                     0, fn_prov_colors, fn_keep_serov, "Top_6")
fn_animal_all <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSites", "Animal", 
                                       0, site_colors, fn_keep_serov, "Top_6")
fn_animalPlus_all <- serovar_class_summary(foodnet_clean, foodnet_df2, "Serovar", "CombinedSite2", 
                                           "AnimalPlus", 0, site2_colors, fn_keep_serov, "Top_6")

ebase_prov_all <- serovar_class_summary(enterobase, enterobase_clean, "Sistr_Serovar", "Province", "Province", 
                                        0, ebase_prov_colors, fn_keep_serov, "Top_6")
ebase_animal_all <- serovar_class_summary(enterobase_df_source, enterobase_df_source, "Sistr_Serovar", "CombinedSites", "Animal", 
                                          0, site_colors, fn_keep_serov, "Top_6")
ebase_animalPlus_all <- serovar_class_summary(enterobase_df_source, enterobase_df_source, "Sistr_Serovar", "CombinedSite2", 
                                              "AnimalPlus", 0, site2_colors, fn_keep_serov, "Top_6")

ggarrange(fn_prov_f5, ebase_prov_f5, fn_animal_f5, fn_animalPlus_f5, align = "hv", labels = "AUTO")
ggarrange(fn_prov_f10, ebase_prov_f10, fn_animal_f10, fn_animalPlus_f10, align = "hv", labels = "AUTO")

fn_plot_all <- ggarrange(fn_prov_all, fn_animal_all, fn_animalPlus_all, align = "hv", labels = "AUTO")
ebase_plot_all <- ggarrange(ebase_prov_all, ebase_animal_all, ebase_animalPlus_all, align = "hv", labels = "AUTO")

ggarrange(fn_prov_all, ebase_prov_all, align = "hv", labels = "AUTO")

### Generate wgMLST ###
ebase_wgmlst_profile <- read.csv("../../../Downloads/ebase_prov_wgmlst_20240705.csv")
ebase_wgmlst_metadata <- read.csv("../../../Downloads/ebase_wgmlst_meta_20240705.csv")
ebase_wgmlst_metadata <- ebase_wgmlst_metadata %>%
  separate(col = Data.Source.Accession.No..Sequencing.Platform.Sequencing.Library.Insert.Size.Experiment.Bases.Average.Length.Status., 
           into = c("Accession", "Rest"), sep = ";", extra = "merge") 
ebase_wgmlst_metadata <- ebase_wgmlst_metadata %>%
  select(Uberstrain, Accession, Name, Source.Niche, Source.Type, Source.Details, Region, ST)

ebase_wgmlst_full <- left_join(ebase_wgmlst_metadata, ebase_wgmlst_profile, by = c("Name", "ST"))

prov_accession_wgmlst <- enterobase_df %>%
  filter(qc_status != "FAIL", Accession2 != "SRR1183798") %>%
  select(Uberstrain, Name, Accession2, Region, serovar_cgmlst) %>%
  rename(Accession = Accession2,
         Province = Region,
         Sistr_Serovar = serovar_cgmlst) %>%
  mutate(Province = str_replace_all(Province, rename_dict))

ebase_prov_wgmlst <- prov_accession_wgmlst %>%
  left_join(ebase_wgmlst_full, by = c("Uberstrain", "Accession", "Name")) %>%
  rows_patch(
    prov_accession_wgmlst %>%
      left_join(ebase_wgmlst_full, by = c("Uberstrain" = "Uberstrain", "Accession" = "Accession")) %>%
      select(-Name.y, -Name.x),
    by = c("Uberstrain", "Accession")
  ) %>%
  rows_patch(
    prov_accession_wgmlst %>%
      left_join(ebase_wgmlst_full, by = c("Uberstrain" = "Uberstrain", "Name" = "Name")) %>%
      select(-Accession.y, -Accession.x),
    by = c("Uberstrain", "Name")
  )

ebase_prov_wgmlst <- ebase_prov_wgmlst %>%
  select(-Name, -Region, -ST, -Source.Niche, -Source.Type, -Source.Details) %>%
  rename(serovar_cgmlst = Sistr_Serovar) 

# write.csv(ebase_prov_wgmlst, "ebase_prov_full_wgmlst_20240708.csv", row.names = F)
