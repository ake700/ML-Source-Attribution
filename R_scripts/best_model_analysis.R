##############################################
## Setup and Libraries
##############################################
library(ggplot2)
library(dplyr)
library(nortest)
library(ggpubr)
library(stringr)
library(rlang)
library(ggpattern)
library(gridExtra)

setwd("C:/Users/user5012100/Desktop/Data/Models_Outputs/")
source("C:/Users/user5012100/Desktop/R/Functions_20240507.R")

##############################################
## Global Constants and Theme Settings
##############################################
# Colors and shapes for plotting
model_type_colors <- c(
  "Random Forest" = "lightblue", 
  "GradientBoostingClassifier" = "pink", 
  "SVM" = "lightgreen"
)

shape_values <- c(
  'GradientBoostingClassifier' = 22, 
  'Random Forest' = 21, 
  'SVM' = 4
)

# These numbers represent how many serovars are included for certain groups
enterobase_prov_n_sero <- 83
enterobase_host_n_sero <- 45
fn_n_sero <- 75 

##############################################
## Data Import
## Loading best parameter datasets and wgMLST data
##############################################
best_flat <- read.csv("best_flat_all_20240613.csv")
best_hml <- read.csv("best_hml_all_20240613.csv")
best_ebase_source <- read.csv("output_ebase_source_20240702.csv")

foodnet_df <- read.csv("FN_allele_clean_v2_20240319.csv") 
enterobase_df <- read.csv("enterobase_sistr_combined_20240201.csv")
enterobase_df_source <- read.csv("../../../Downloads/Ebase_source_final_20240702.csv")

wgmlst_independent_flat <- read.csv("new_out_20240710/Output_template_wgmlst_independent_20240709.csv") 
wgmlst_independent_hml <- read.csv("new_out_20240710/Output_template_hML_wgmlst_independent_20240709.csv")

## Data Cleaning ##
## Dictionary for Province Rename
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
## Filtering and Adjusting wgMLST Independent Flat Data
## Remove certain combinations and recode values for consistency.
##############################################
wgmlst_independent_flat <- wgmlst_independent_flat %>% 
  filter(!(Serovar == "All" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "All" & dataType == "allele" & Pred_label == "Location or Food")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Location or Food")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Location or Food")) %>%
  filter(n_original_features != 3002)

wgmlst_independent_flat <- wgmlst_independent_flat %>%
  mutate(
    Sero_calling = ifelse(Sero_calling %in% c("sistr", "enterobase"), "Public", Sero_calling),
    Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), round, 2)) %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" & Pred_label == "Province" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
      Serovar == "All" & Pred_label != "Province" ~ paste0("All\n(n_serovar = ", enterobase_host_n_sero, ")"),
      TRUE ~ Serovar
    ),
    genome = "wgMLST"
  )

# Compute mean F1 per group for wgMLST independent flat
mean_F1_ebase_wgmlst_flat <- wgmlst_independent_flat %>%
  group_by(Serovar, Sero_calling, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

wgmlst_independent_flat <- wgmlst_independent_flat %>%
  left_join(mean_F1_ebase_wgmlst_flat, by = c("Serovar", "Sero_calling", "Pred_label"))

##############################################
## Filtering and Adjusting wgMLST Independent HML Data
##############################################
wgmlst_independent_hml <- wgmlst_independent_hml %>%
  mutate(
    Sero_calling = ifelse(Sero_calling %in% c("sistr", "enterobase"), "Public", Sero_calling),
    Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), round, 2)) %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
      TRUE ~ Serovar
    ),
    genome = "wgMLST",
    model_type = ifelse(model_type == "RandomForestClassifier", "Random Forest", model_type)
  )

# Compute mean F1 per group for wgMLST independent HML
mean_F1_ebase_wgmlst_hml <- wgmlst_independent_hml %>%
  group_by(Serovar, Sero_calling, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

wgmlst_independent_hml <- wgmlst_independent_hml %>%
  left_join(mean_F1_ebase_wgmlst_hml, by = c("Serovar", "Sero_calling", "Pred_label"))

##############################################
## Filtering and Adjusting best_ebase_source Data
##############################################
best_ebase_source <- best_ebase_source %>%
  mutate(
    Sero_calling = ifelse(Sero_calling == "sistr", "Public", Sero_calling),
    Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), round, 2)) %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_host_n_sero, ")"),
      TRUE ~ Serovar
    )
  ) %>%
  # Filter out certain OHE_allele conditions and model types
  filter(!(Serovar == "All" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "All" & dataType == "allele" & Pred_label == "Location or food")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Location or food")) %>%
  filter(!(Serovar == "Heidelberg" & model_type == "SVM")) %>%
  filter(!(Serovar == "Typhimurium" & model_type == "SVM")) %>% 
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Location or food"))

mean_F1_ebase_source <- best_ebase_source %>%
  group_by(Serovar, Sero_calling, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

best_ebase_source <- best_ebase_source %>%
  left_join(mean_F1_ebase_source, by = c("Serovar", "Sero_calling", "Pred_label")) %>%
  mutate(genome = "cgMLST")

##############################################
## Filtering and Adjusting best_hml Data
##############################################
best_hml <- best_hml %>%
  mutate(Sero_calling = ifelse(Sero_calling == "enterobase", "Public", Sero_calling)) %>%
  mutate(
    Serovar = case_when(
      Sero_calling == "Public" & Serovar == "Enteritidis,Typhimurium,Heidelberg" ~ "Top_3",
      Sero_calling == "FoodNet" & Serovar == "Heidelberg,Kentucky,Enteritidis" ~ "Heid_Kent_Entr",
      Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Heidelberg,Kentucky,Enteritidis,Infantis,Typhimurium" ~ "Top_6",
      TRUE ~ Serovar
    ),
    model_type = ifelse(model_type == "RandomForestClassifier", "Random Forest", model_type)
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), round, 2))

mean_F1_hml <- best_hml %>%
  group_by(Serovar, Sero_calling, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

best_hml <- best_hml %>%
  left_join(mean_F1_hml, by = c("Serovar", "Sero_calling", "Pred_label"))

##############################################
## Filtering and Adjusting best_flat Data
##############################################
best_flat <- best_flat %>%
  mutate(Sero_calling = ifelse(Sero_calling == "enterobase", "Public", Sero_calling)) %>%
  mutate(
    Serovar = case_when(
      Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Enteritidis" ~ "Entr_4512",
      Sero_calling == "FoodNet" & Serovar == "Heidelberg,Kentucky,Enteritidis" ~ "Heid_Kent_Entr",
      Sero_calling == "Public" & Serovar == "Enteritidis,Typhimurium,Heidelberg" ~ "Top_3",
      Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Heidelberg,Kentucky,Enteritidis,Infantis,Typhimurium" ~ "Top_6",
      TRUE ~ Serovar
    )
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), round, 2))

mean_F1_flat <- best_flat %>%
  group_by(Serovar, Sero_calling, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

best_flat <- best_flat %>%
  left_join(mean_F1_flat, by = c("Serovar", "Sero_calling", "Pred_label"))

# Remove top-level combined serovars that are not needed
best_flat <- best_flat %>% 
  filter(!Serovar %in% c("Top_6", "Top_3", "Heid_Kent_Entr", "Entr_4512"))

best_hml <- best_hml %>% 
  filter(!Serovar %in% c("Top_3", "Top_6", "Heid_Kent_Entr"))

##############################################
## Separating Datasets by Sero_calling (Public/FoodNet)
## and adding n_sero and genome columns
##############################################
best_hml_public <- best_hml %>%
  filter(Sero_calling == "Public") %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
      TRUE ~ Serovar
    ),
    genome = "cgMLST"
  )

best_hml_fn <- best_hml %>%
  filter(Sero_calling == "FoodNet") %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", fn_n_sero, ")"),
      TRUE ~ Serovar
    ),
    genome = "wgMLST"
  )

best_flat_public <- best_flat %>%
  filter(Sero_calling == "Public") %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
      TRUE ~ Serovar
    ),
    genome = "cgMLST"
  )

best_flat_fn <- best_flat %>%
  filter(Sero_calling == "FoodNet") %>%
  mutate(
    n_sero = case_when(
      Serovar == "All" ~ paste0("All\n(n_serovar = ", fn_n_sero, ")"),
      TRUE ~ Serovar
    )
  )

##############################################
## Further Filtering by Pred_label
##############################################
best_flat_fn_prov <- best_flat_fn %>% filter(Pred_label == "Province")
best_flat_fn_animal <- best_flat_fn %>% filter(Pred_label == "Animal type")
best_flat_fn_animalPlus <- best_flat_fn %>% filter(Pred_label == "Location or food")

best_flat_pub_animal <- best_ebase_source %>% filter(Pred_label == "Animal type")
best_flat_pub_animalPlus <- best_ebase_source %>% filter(Pred_label == "Location or food")

wgmlst_pub_prov <- wgmlst_independent_flat %>% filter(Pred_label == "Province")
wgmlst_pub_animal <- wgmlst_independent_flat %>% filter(Pred_label == "Animal type")
wgmlst_pub_animalPlus <- wgmlst_independent_flat %>% filter(Pred_label == "Location or food")

##############################################
## Factor Level Adjustments for Serovar
## Ensuring consistent ordering of levels in factors.
##############################################
best_hml_public$Serovar <- factor(best_hml_public$Serovar, 
                                  levels = c("Enteritidis", "Heidelberg", "Typhimurium", "All"))
wgmlst_independent_hml$Serovar <- factor(wgmlst_independent_hml$Serovar,
                                         levels = c("Enteritidis", "Heidelberg", "Typhimurium", "All"))
best_flat_public$Serovar <- factor(best_flat_public$Serovar, 
                                   levels = c("Enteritidis", "Heidelberg", "Typhimurium", "All"))
wgmlst_independent_flat$Serovar <- factor(wgmlst_independent_flat$Serovar, 
                                          levels = c("Enteritidis", "Heidelberg", "Typhimurium", "All"))
best_ebase_source$Serovar <- factor(best_ebase_source$Serovar, 
                                    levels = c("Heidelberg", "Typhimurium", "All"))

best_hml_fn$Serovar <- factor(best_hml_fn$Serovar, 
                              levels = c("Enteritidis", "Heidelberg", "I:4,[5],12:i:-", "Infantis",
                                         "Kentucky", "Typhimurium", "All"))
# Note: best_flat_fn$Serovar is factorized but top_6 etc. were removed earlier.
best_flat_fn$Serovar <- factor(best_flat_fn$Serovar, 
                               levels = c("Enteritidis", "Heidelberg", "I:4,[5],12:i:-", 
                                          "Infantis","Kentucky", "Typhimurium", "All"))

# Repeat for subsets if needed:
best_flat_fn_prov$Serovar <- factor(best_flat_fn_prov$Serovar, 
                                    levels = c("Enteritidis", "Heidelberg", 
                                               "I:4,[5],12:i:-", "Infantis","Kentucky", "Typhimurium", "All"))
best_flat_fn_animal$Serovar <- factor(best_flat_fn_animal$Serovar, 
                                      levels = c("Enteritidis", "Heidelberg", 
                                                 "I:4,[5],12:i:-", "Infantis","Kentucky", "Typhimurium", "All"))
best_flat_fn_animalPlus$Serovar <- factor(best_flat_fn_animalPlus$Serovar, 
                                          levels = c("Enteritidis", "Heidelberg", 
                                                     "I:4,[5],12:i:-", "Infantis","Kentucky", "Typhimurium", "All"))
wgmlst_pub_prov$Serovar <- factor(wgmlst_pub_prov$Serovar, 
                                  levels = c("Enteritidis", "Heidelberg", "Typhimurium", "All"))
wgmlst_pub_animal$Serovar <- factor(wgmlst_pub_animal$Serovar, 
                                    levels = c("Heidelberg", "Typhimurium", "All"))
wgmlst_pub_animalPlus$Serovar <- factor(wgmlst_pub_animalPlus$Serovar, 
                                        levels = c("Heidelberg", "Typhimurium", "All"))
best_flat_pub_animal$Serovar <- factor(best_flat_pub_animal$Serovar, 
                                       levels = c("Heidelberg", "Typhimurium", "All"))
best_flat_pub_animalPlus$Serovar <- factor(best_flat_pub_animalPlus$Serovar, 
                                           levels = c("Heidelberg", "Typhimurium", "All"))

##############################################
## Example Plots
## Here we create various ggplots comparing F1 by serovar, model type, etc.
##############################################
plot_hml_public <- ggplot(best_hml_public, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Hierarchical Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = 'darkgreen'))

plot_hml_fn <- ggplot() +
  geom_point(data = best_hml_fn , aes(x = n_sero, y = F1, shape = model_type, 
                                      fill = as.factor(genome)), size = 5, shape = 21, show.legend = TRUE) +
  geom_point(data = subset(best_hml_fn , genome == "cgMLST"), 
             aes(x = n_sero, y = mean_F1), size = 7, shape = 21, color = 'cyan', fill = NA, 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_point(data = subset(best_hml_fn , genome == "wgMLST"), 
             aes(x = n_sero, y = mean_F1), size = 7, shape = 21, color = 'green', fill = NA, 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_point(data = best_hml_fn , aes(x = n_sero, y = F1, shape = model_type, 
                                      color = as.factor(filter_threshold)), size = 5, stroke = 1, show.legend = TRUE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Hierarchical Model type", fill = "MLST type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan', 'wgMLST' = 'green')) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(
    shape = guide_legend(order = 1, override.aes = list(fill = NA)),  # Remove fill from shape legend
    fill = guide_legend(order = 2, override.aes = list(shape = 21, color = NA)),  # Ensure correct shape in fill legend
    color = guide_legend(order = 3, override.aes = list(shape = 21, fill = NA, stroke = 1.5)))

plot_flat_public <- ggplot(best_flat_public, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_public, aes(y = mean_F1), size = 5, shape = 1, color = 'cyan', 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", fill = "MLST type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan')) +
  guides(
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

plot_flat_fn_prov <- ggplot(best_flat_fn_prov, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_fn_prov, aes(y = mean_F1), size = 5, shape = 21, color = 'blue', position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5))
  )

plot_flat_fn_animal <- ggplot(best_flat_fn_animal, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_fn_animal, aes(y = mean_F1), size = 5, shape = 21, color = 'blue', position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5))
  )

plot_flat_fn_animalPlus <- ggplot(best_flat_fn_animalPlus, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_fn_animalPlus, aes(y = mean_F1), size = 5, shape = 21, color = 'blue', position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5))
  )

plot_flat_public_animal <- ggplot(best_flat_pub_animal, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_pub_animal, aes(y = mean_F1), size = 5, shape = 1, color = 'cyan', 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan')) +
  guides(
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

plot_flat_public_animalPlus <- ggplot(best_flat_pub_animalPlus, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(size = 3) +
  geom_point(data = best_flat_pub_animalPlus, aes(y = mean_F1), size = 5, shape = 1, color = 'cyan', 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan')) +
  guides(
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

plot_wgmlst_prov <- ggplot(wgmlst_pub_prov, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(size = 3) +
  geom_point(data = wgmlst_pub_prov, aes(y = mean_F1), size = 5, shape = 1, color = 'green', 
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('wgMLST' = 'green')) +
  guides(
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

##############################################
## Combining Public DataFrames
## For Province, Animal, and AnimalPlus categories
##############################################

# Combine public-province data from flat and wgMLST
bind_pub_prov1 <- best_flat_public %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)
bind_pub_prov2 <- wgmlst_pub_prov %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)

pub_prov_combined <- rbind(bind_pub_prov1, bind_pub_prov2)

# Combine public-animal data from flat and wgMLST
bind_pub_animal1 <- best_flat_pub_animal %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)
bind_pub_animal2 <- wgmlst_pub_animal %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)

pub_animal_combined <- rbind(bind_pub_animal1, bind_pub_animal2)

# Combine public-animalPlus data from flat and wgMLST
bind_pub_animalPlus1 <- best_flat_pub_animalPlus %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)
bind_pub_animalPlus2 <- wgmlst_pub_animalPlus %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1)

pub_animalPlus_combined <- rbind(bind_pub_animalPlus1, bind_pub_animalPlus2)

##############################################
## Filtering wgMLST-only Rows and Adding Filter Threshold
##############################################
# For consistency, assign filter_threshold = 10 to wgMLST subsets
pub_prov_wgmlst <- pub_prov_combined %>%
  filter(genome == "wgMLST") %>%
  mutate(filter_threshold = 10)

pub_animal_wgmlst <- pub_animal_combined %>%
  filter(genome == "wgMLST") %>%
  mutate(filter_threshold = 10)

pub_animalPlus_wgmlst <- pub_animalPlus_combined %>%
  filter(genome == "wgMLST") %>%
  mutate(filter_threshold = 10)

##############################################
## Plotting wgMLST Public Data for Province, Animal, AnimalPlus
##############################################
plot_flat_pub_prov <- ggplot(pub_prov_wgmlst, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', 
             position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(shape = guide_legend(order = 1),
         color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5)))

plot_flat_pub_animal <- ggplot(pub_animal_wgmlst, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', 
             position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(shape = guide_legend(order = 1),
         color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5)))

plot_flat_pub_animalPlus <- ggplot(pub_animalPlus_wgmlst, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', 
             position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5' = 'maroon', '10' = '#008080')) +
  guides(shape = guide_legend(order = 1),
         color = guide_legend(order = 2, override.aes = list(shape = 21, fill = NA, stroke = 1.5)))

# Arrange the three public wgMLST plots side-by-side
ggarrange(plot_flat_pub_prov, plot_flat_pub_animal, plot_flat_pub_animalPlus,
          align = "hv", labels = "AUTO", common.legend = TRUE, legend = "bottom")

##############################################
## Additional Combined Plots for Public Data
##############################################
# The code below combines cgMLST and wgMLST data for province, animal, and animalPlus

plot_bind_pub_prov <- ggplot(pub_prov_combined, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  # Cyan border for cgMLST mean F1
  geom_point(data = subset(pub_prov_combined, genome == "cgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'cyan', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  # Green border for wgMLST mean F1
  geom_point(data = subset(pub_prov_combined, genome == "wgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'green', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", fill = "MLST type") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan', 'wgMLST' = 'green')) +
  guides(shape = guide_legend(order = 1, override.aes = list(fill = NA)),
         fill = guide_legend(order = 2, override.aes = list(shape = 21)))

# Repeat similar approach for animal and animalPlus
plot_bind_pub_animal <- ggplot(pub_animal_combined, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(data = subset(pub_animal_combined, genome == "cgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'cyan', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_point(data = subset(pub_animal_combined, genome == "wgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'green', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", fill = "MLST type") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan', 'wgMLST' = 'green')) +
  guides(shape = guide_legend(order = 1, override.aes = list(fill = NA)),
         fill = guide_legend(order = 2, override.aes = list(shape = 21)))

plot_bind_pub_animalPlus <- ggplot(pub_animalPlus_combined, aes(x = n_sero, y = F1, shape = model_type, fill = as.factor(genome))) +
  geom_point(data = subset(pub_animalPlus_combined, genome == "cgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'cyan', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = TRUE) +
  geom_point(data = subset(pub_animalPlus_combined, genome == "wgMLST"),
             aes(y = mean_F1), size = 5, shape = 21, color = 'green', fill = NA,
             stroke = 1.5, position = position_dodge(width = 0.75), show.legend = TRUE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", fill = "MLST type") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = c('cgMLST' = 'cyan', 'wgMLST' = 'green')) +
  guides(shape = guide_legend(order = 1, override.aes = list(fill = NA)),
         fill = guide_legend(order = 2, override.aes = list(shape = 21)))

ggarrange(plot_bind_pub_prov, plot_bind_pub_animal, plot_bind_pub_animalPlus,
          align = "hv", labels = "AUTO", common.legend = TRUE, legend = "bottom")

##############################################
## Combining HML Public and wgMLST Data
##############################################
bind_pub_hml_prov1 <- best_hml_public %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1, filter_threshold)
bind_pub_hml_prov2 <- wgmlst_independent_hml %>%
  select(n_sero, Serovar, F1, model_type, genome, mean_F1, filter_threshold)

pub_prov_combined_hml <- rbind(bind_pub_hml_prov1, bind_pub_hml_prov2) 

pub_prov_wgmlst_hml <- pub_prov_combined_hml %>%
  filter(genome == "wgMLST")

##############################################
## Plotting HML Public with wgMLST Data
##############################################
plot_hml_pub <- ggplot(pub_prov_wgmlst_hml, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', 
             position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,by=0.25)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5'='maroon','10'='#008080')) +
  guides(shape = guide_legend(order=1),
         color = guide_legend(order=2, override.aes = list(shape=21, fill=NA, stroke=1.5)))

plot_hml_fn <- ggplot(best_hml_fn, aes(x = n_sero, y = F1, shape = model_type, color = as.factor(filter_threshold))) +
  geom_point(size = 3) +
  geom_point(aes(y = mean_F1), size = 5, shape = 21, color = 'blue', 
             position = position_dodge(width = 0.75), show.legend = FALSE) +
  coord_flip() +
  labs(x = "Serovar", y = "F1", shape = "Model type", color = "Filter threshold") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,by=0.25)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = c('5'='maroon','10'='#008080')) +
  guides(shape = guide_legend(order=1),
         color = guide_legend(order=2, override.aes = list(shape=21, fill=NA, stroke=1.5)))

ggarrange(plot_hml_pub, plot_hml_fn, align="hv", labels="AUTO",
          common.legend=TRUE, legend="bottom")
# ggarrange(left_public, right_public, ncol = 1)
# ggarrange(left_fn_prov, right_fn_prov, ncol = 1)
# ggarrange(left_fn_animal, right_fn_animal, ncol = 1)
# ggarrange(left_fn_animalPlus, right_fn_animalPlus, ncol = 1)

##### Final dot plots ########
plot1 <- ggarrange(plot_hml_public, plot_hml_fn, align="hv", labels="AUTO", common.legend=TRUE, legend="bottom")
plot2 <- ggarrange(plot_flat_public, plot_flat_public_animal, plot_flat_public_animalPlus, align="hv", labels="AUTO",
                   common.legend=TRUE, legend="bottom")
plot3 <- ggarrange(plot_flat_fn_prov, plot_flat_fn_animal, plot_flat_fn_animalPlus, align="hv", labels="AUTO")
plot4 <- ggarrange(plot_bind_pub_prov, plot_bind_pub_animal, plot_bind_pub_animalPlus, align="hv", labels="AUTO",
                   common.legend=TRUE, legend="bottom")
plot5 <- ggarrange(plot_bind_pub_prov, plot_hml_fn, align="hv", labels="AUTO", common.legend=TRUE, legend="bottom")

##############################################
## Further Data Merging for Correlation Analysis
## Adding total_labels from count data and merging with corr_combined_df
##############################################
# These merges combine label counts with the main dataframe for correlation analysis

# After merges, final_corr_df will have new columns for total_labels and predicted_label counts
# The code block merges multiple counts (prov, animal, etc.) with corr_combined_df

#### Num_Pred with F1 ########
# Values for the Predicted_label and the avg_F1 below are from the ML_OptimalParams spreadsheet
flat1 <- read.csv("best_flat_all_20240613.csv") %>%
  mutate(Sero_calling = ifelse(Sero_calling == "enterobase", "Public", Sero_calling)) %>%
  mutate(Serovar = case_when(
    Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Enteritidis" ~ "Entr_4512",
    Sero_calling == "FoodNet" & Serovar == "Heidelberg,Kentucky,Enteritidis" ~ "Heid_Kent_Entr",
    Sero_calling == "Public" & Serovar == "Enteritidis,Typhimurium,Heidelberg" ~ "Top_3",
    Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Heidelberg,Kentucky,Enteritidis,Infantis,Typhimurium" ~ "Top_6",
    TRUE ~ Serovar
  ))

flat2 <- read.csv("output_ebase_source_20240702.csv")

flat2 <- flat2 %>%
  mutate(
    Sero_calling = ifelse(Sero_calling == "sistr", "Public", Sero_calling),
    Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)
  ) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), ~ round(., 2))) %>%
  mutate(n_sero = case_when(
    Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_host_n_sero, ")"),
    TRUE ~ Serovar
  )) %>%
  filter(!(Serovar == "All" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "All" & dataType == "allele" & Pred_label == "Location or food")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Location or food")) %>%
  filter(!(Serovar == "Heidelberg" & model_type == "SVM")) %>%
  filter(!(Serovar == "Typhimurium" & model_type == "SVM")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Location or food")) %>%
  select(-n_sero)

flat3 <- read.csv("new_out_20240710/Output_template_wgmlst_independent_20240709.csv")
flat3 <- flat3 %>% 
  filter(!(Serovar == "All" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "All" & dataType == "allele" & Pred_label == "Location or Food")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Heidelberg" & dataType == "OHE_allele" & Pred_label == "Location or Food")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Animal type")) %>%
  filter(!(Serovar == "Typhimurium" & dataType == "OHE_allele" & Pred_label == "Location or Food")) %>%
  filter(n_original_features != 3002) %>%
  mutate(Sero_calling = ifelse(Sero_calling %in% c("sistr", "enterobase"), "Public", Sero_calling),
         Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), ~ round(., 2))) %>%
  mutate(n_sero = case_when(
    Serovar == "All" & Pred_label == "Province" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
    Serovar == "All" & Pred_label != "Province" ~ paste0("All\n(n_serovar = ", enterobase_host_n_sero, ")"),
    TRUE ~ Serovar
  )) %>% select(-wg_type) %>%
  mutate(Sero_calling = ifelse(Sero_calling %in% c("sistr", "enterobase"), "Public", Sero_calling),
         Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), ~ round(., 2))) %>%
  mutate(n_sero = case_when(
    Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
    TRUE ~ Serovar
  )) %>%
  select(-n_sero)

hml1 <- read.csv("best_hml_all_20240613.csv") %>%
  mutate(Sero_calling = ifelse(Sero_calling == "enterobase", "Public", Sero_calling)) %>%
  mutate(Serovar = case_when(
    Sero_calling == "Public" & Serovar == "Enteritidis,Typhimurium,Heidelberg" ~ "Top_3",
    Sero_calling == "FoodNet" & Serovar == "Heidelberg,Kentucky,Enteritidis" ~ "Heid_Kent_Entr",
    Sero_calling == "FoodNet" & Serovar == "I:4,[5],12:i:-,Heidelberg,Kentucky,Enteritidis,Infantis,Typhimurium" ~ "Top_6",
    TRUE ~ Serovar
  )) 

hml2 <- read.csv("new_out_20240710/Output_template_hML_wgmlst_independent_20240709.csv")
hml2 <- hml2 %>%
  mutate(Sero_calling = ifelse(Sero_calling %in% c("sistr", "enterobase"), "Public", Sero_calling),
         Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label)) %>%
  mutate(across(c(Recall, Precision, F1, Accuracy, runTime_mins), ~ round(., 2))) %>%
  mutate(n_sero = case_when(
    Serovar == "All" ~ paste0("All\n(n_serovar = ", enterobase_prov_n_sero, ")"),
    TRUE ~ Serovar
  )) %>% select(-wg_type, -n_sero)

flat_combined <- rbind(flat1, flat2, flat3)
hml_combined <- rbind(hml1, hml2) %>%
  mutate(model_type = ifelse(model_type == "RandomForestClassifier", "Random Forest", model_type))

# Identify columns in flat_combined that are not in hml_combined
missing_in_hml <- setdiff(colnames(flat_combined), colnames(hml_combined))

# Identify columns in hml_combined that are not in flat_combined
missing_in_flat <- setdiff(colnames(hml_combined), colnames(flat_combined))

# Add missing columns to hml_combined with default value "None"
for (col in missing_in_hml) {
  hml_combined[[col]] <- "None"
}

# Add missing columns to flat_combined with default value "None"
for (col in missing_in_flat) {
  flat_combined[[col]] <- "None"
}

# Reorder columns in hml_combined to match flat_combined
hml_combined <- hml_combined[, colnames(flat_combined)]

# Combine data frames using rbind
corr_combined_df <- rbind(flat_combined, hml_combined)
corr_combined_df <- corr_combined_df %>% filter(!Serovar %in% c("Top_3", "Top_6", "Entr_4512", "Heid_Kent_Entr"))
corr_combined_df <- distinct(corr_combined_df) %>%
  mutate(Pred_label = ifelse(Pred_label == "Location or Food", "Location or food", Pred_label))
combined_labels <- corr_combined_df %>% distinct(Pred_label, Serovar, Sero_calling)

fn_unique_serovar <- corr_combined_df %>% filter(Sero_calling == "FoodNet" & Serovar != "All") %>% distinct(Serovar)
ebase_unique_serovar_prov <- corr_combined_df %>% 
  filter(Sero_calling == "Public" & Serovar != "All" & Pred_label == "Province") %>% distinct(Serovar)
ebase_unique_serovar_host <- corr_combined_df %>% 
  filter(Sero_calling == "Public" & Serovar != "All" & Pred_label != "Province") %>% distinct(Serovar)

fn_count_labels_prov <- foodnet_df2 %>%
  filter(Serovar %in% fn_unique_serovar$Serovar) %>%
  group_by(Serovar) %>% 
  summarise(total_labels = n_distinct(SourceState)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Province") 

fn_count_labels_animal <- foodnet_df2 %>%
  filter(Serovar %in% fn_unique_serovar$Serovar) %>%
  group_by(Serovar) %>% 
  summarise(total_labels = n_distinct(CombinedSites)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Animal type") 

fn_count_labels_animalPlus <- foodnet_df2 %>%
  filter(Serovar %in% fn_unique_serovar$Serovar) %>%
  group_by(Serovar) %>% 
  summarise(total_labels = n_distinct(CombinedSite2)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Location or food") 

ebase_count_labels_prov <- enterobase %>% 
  filter(Sistr_Serovar %in% ebase_unique_serovar_prov$Serovar) %>%
  group_by(Sistr_Serovar) %>%
  summarise(total_labels = n_distinct(Province)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Province") %>%
  rename(Serovar = Sistr_Serovar)

ebase_count_labels_animal <- enterobase_df_source %>% 
  filter(Sistr_Serovar %in% ebase_unique_serovar_host$Serovar) %>%
  group_by(Sistr_Serovar) %>%
  summarise(total_labels = n_distinct(CombinedSites)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Animal type") %>%
  rename(Serovar = Sistr_Serovar)

ebase_count_labels_animalPlus <- enterobase_df_source %>% 
  filter(Sistr_Serovar %in% ebase_unique_serovar_host$Serovar) %>%
  group_by(Sistr_Serovar) %>%
  summarise(total_labels = n_distinct(CombinedSite2)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Location or food") %>%
  rename(Serovar = Sistr_Serovar)

fn_count_label_all_prov <- foodnet_df2 %>%
  summarise(total_labels = n_distinct(SourceState)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Province",
         Serovar = "All") 
fn_count_label_all_host1 <- foodnet_df2 %>%
  summarise(total_labels = n_distinct(CombinedSites)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Animal type",
         Serovar = "All") 
fn_count_label_all_host2 <- foodnet_df2 %>%
  summarise(total_labels = n_distinct(CombinedSite2)) %>%
  mutate(Sero_calling = "FoodNet",
         Pred_label = "Location or food",
         Serovar = "All") 

ebase_count_label_all_prov <- enterobase %>%
  summarise(total_labels = n_distinct(Province)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Province",
         Serovar = "All") 
ebase_count_label_all_host1 <- enterobase_df_source %>%
  summarise(total_labels = n_distinct(CombinedSites)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Animal type",
         Serovar = "All") 
ebase_count_label_all_host2 <- enterobase_df_source %>%
  summarise(total_labels = n_distinct(CombinedSite2)) %>%
  mutate(Sero_calling = "Public",
         Pred_label = "Location or food",
         Serovar = "All") 

merge1 <- left_join(fn_count_labels_prov, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge2 <- left_join(fn_count_labels_animal, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge3 <- left_join(fn_count_labels_animalPlus, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge4 <- left_join(ebase_count_labels_prov, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge5 <- left_join(ebase_count_labels_animal, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge6 <- left_join(ebase_count_labels_animalPlus, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge7 <- left_join(fn_count_label_all_prov, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge8 <- left_join(fn_count_label_all_host1, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge9 <- left_join(fn_count_label_all_host2, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge10 <- left_join(ebase_count_label_all_prov, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge11 <- left_join(ebase_count_label_all_host1, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))
merge12 <- left_join(ebase_count_label_all_host2, corr_combined_df, by = c("Serovar", "Sero_calling", "Pred_label"))

final_corr_df <- rbind(
  merge1, merge2, merge3, merge4, merge5, merge6,
  merge7, merge8, merge9, merge10, merge11, merge12
)

# Join back with the main df
join_columns <- setdiff(names(final_corr_df), "total_labels")
final_corr_df <- left_join(corr_combined_df, final_corr_df, by = join_columns)

# Calculate predicted_label = total_labels - num_label
final_corr_df <- final_corr_df %>%
  mutate(num_label = sapply(strsplit(label_filtered, ",\\s*"), function(x) sum(x != ""))) %>%
  relocate(label_filtered, .before = total_labels) %>%
  mutate(Predicted_label = total_labels - num_label)

##############################################
## Correlation Analysis
## Final correlation test between Predicted_label and F1
##############################################

spearman_correlation <- cor.test(final_corr_df$Predicted_label, final_corr_df$F1, method = "spearman")
scorr_flat <- spearman_correlation$estimate
scorr_pval_flat <- spearman_correlation$p.value

print(paste("Spearman correlation coefficient:", spearman_correlation))