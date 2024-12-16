library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(cowplot)
library(caret)
library(ggrepel)
library(ggbreak)

setwd("C:/Users/user5012100/Desktop/Data/global_enterobase/")
source("C:/Users/user5012100/Desktop/R/Functions_20240507.R")

###################################################
############ results for full dataset ###############
global_enterobase_results <- read.csv("output/global_Output_template_ebase.csv")
fn_results <- read.csv("output/global_Output_template_fn_skesa.csv")

canada_only_results <- read.csv("output/canada_Output_template_ebase.csv")
canada_combined_results <- read.csv("output/canada_Output_template_combined.csv")
############ confusion matrices ##############
cm_fn_animal1 <- read.csv("cm/cm_All_Random Forest_CombinedSites_FoodNet_34.csv")
cm_fn_animal2 <- read.csv("cm/cm_All_Random Forest_CombinedSites_FoodNet_42.csv")
cm_fn_animal3 <- read.csv("cm/cm_All_Random Forest_CombinedSites_FoodNet_777.csv")
cm_fn_animalPlus1 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_FoodNet_34.csv")
cm_fn_animalPlus2 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_FoodNet_42.csv")
cm_fn_animalPlus3 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_FoodNet_777.csv")

cm_global_ebase_animal1 <- read.csv("cm/cm_All_Random Forest_CombinedSites_Enterobase_34.csv")
cm_global_ebase_animal2 <- read.csv("cm/cm_All_Random Forest_CombinedSites_Enterobase_42.csv")
cm_global_ebase_animal3 <- read.csv("cm/cm_All_Random Forest_CombinedSites_Enterobase_777.csv")
cm_global_ebase_animalPlus1 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_Enterobase_34.csv")
cm_global_ebase_animalPlus2 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_Enterobase_42.csv")
cm_global_ebase_animalPlus3 <- read.csv("cm/cm_All_Random Forest_CombinedSite2_Enterobase_777.csv")

cm_cad_ebase_animal1 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Enterobase_34.csv")
cm_cad_ebase_animal2 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Enterobase_42.csv")
cm_cad_ebase_animal3 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Enterobase_777.csv")
cm_cad_ebase_animalPlus1 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Enterobase_34.csv")
cm_cad_ebase_animalPlus2 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Enterobase_42.csv")
cm_cad_ebase_animalPlus3 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Enterobase_777.csv")

cm_cad_combined_animal1 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Combined_34.csv")
cm_cad_combined_animal2 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Combined_42.csv")
cm_cad_combined_animal3 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSites_Combined_777.csv")
cm_cad_combined_animalPlus1 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Combined_34.csv")
cm_cad_combined_animalPlus2 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Combined_42.csv")
cm_cad_combined_animalPlus3 <- read.csv("cm/cm_cad_All_Random Forest_CombinedSite2_Combined_777.csv")

########## top features ##############
feat_fn_animal1 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSites_34.csv")
feat_fn_animal2 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSites_42.csv")
feat_fn_animal3 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSites_777.csv")
feat_fn_animalPlus1 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSite2_34.csv")
feat_fn_animalPlus2 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSite2_42.csv")
feat_fn_animalPlus3 <- read.csv("features/features_FoodNet_All_Random Forest_CombinedSite2_777.csv")

feat_global_ebase_animal1 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSites_34.csv")
feat_global_ebase_animal2 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSites_42.csv")
feat_global_ebase_animal3 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSites_777.csv")
feat_global_ebase_animalPlus1 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSite2_34.csv")
feat_global_ebase_animalPlus2 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSite2_42.csv")
feat_global_ebase_animalPlus3 <- read.csv("features/features_Enterobase_All_Random Forest_CombinedSite2_777.csv")

feat_cad_ebase_animal1 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSites_34.csv")
feat_cad_ebase_animal2 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSites_42.csv")
feat_cad_ebase_animal3 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSites_777.csv")
feat_cad_ebase_animalPlus1 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSite2_34.csv")
feat_cad_ebase_animalPlus2 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSite2_42.csv")
feat_cad_ebase_animalPlus3 <- read.csv("features/features_cad_Enterobase_All_Random Forest_CombinedSite2_777.csv")

feat_cad_combined_animal1 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSites_34.csv")
feat_cad_combined_animal2 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSites_42.csv")
feat_cad_combined_animal3 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSites_777.csv")
feat_cad_combined_animalPlus1 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSite2_34.csv")
feat_cad_combined_animalPlus2 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSite2_42.csv")
feat_cad_combined_animalPlus3 <- read.csv("features/features_cad_Combined_All_Random Forest_CombinedSite2_777.csv")

########### test and pred labels ###############
pred_fn_animal1 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_FoodNet_34.csv")
pred_fn_animal2 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_FoodNet_42.csv")
pred_fn_animal3 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_FoodNet_777.csv")
pred_fn_animalPlus1 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_FoodNet_34.csv")
pred_fn_animalPlus2 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_FoodNet_42.csv")
pred_fn_animalPlus3 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_FoodNet_777.csv")

pred_global_ebase_animal1 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_Enterobase_34.csv")
pred_global_ebase_animal2 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_Enterobase_42.csv")
pred_global_ebase_animal3 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSites_Enterobase_777.csv")
pred_global_ebase_animalPlus1 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_Enterobase_34.csv")
pred_global_ebase_animalPlus2 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_Enterobase_42.csv")
pred_global_ebase_animalPlus3 <- read.csv("test_pred_labels/pred_All_Random Forest_CombinedSite2_Enterobase_777.csv")

pred_cad_ebase_animal1 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Enterobase_34.csv")
pred_cad_ebase_animal2 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Enterobase_42.csv")
pred_cad_ebase_animal3 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Enterobase_777.csv")
pred_cad_ebase_animalPlus1 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Enterobase_34.csv")
pred_cad_ebase_animalPlus2 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Enterobase_42.csv")
pred_cad_ebase_animalPlus3 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Enterobase_777.csv")

pred_cad_combined_animal1 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Combined_34.csv")
pred_cad_combined_animal2 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Combined_42.csv")
pred_cad_combined_animal3 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSites_Combined_777.csv")
pred_cad_combined_animalPlus1 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Combined_34.csv")
pred_cad_combined_animalPlus2 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Combined_42.csv")
pred_cad_combined_animalPlus3 <- read.csv("test_pred_labels/pred_cad_All_Random Forest_CombinedSite2_Combined_777.csv")
###################################################
canada_grouped_results <- rbind(canada_only_results, canada_combined_results) %>%
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "cad_Enterobase",
                               "Combined" = "cad_Combined")) %>%
  group_by(Pred_label, Sero_calling) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

## Calculating the confidence intervals ##
grouped_results <- rbind(fn_results, global_enterobase_results) %>%
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "global_Enterobase",
                               "Combined" = "global_Combined")) %>%
  group_by(Pred_label, Sero_calling) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

## Creating the dumbbell plots ##
nudge_value <- 0.02  # Adjust nudge value for text

all_results <- rbind(canada_grouped_results, grouped_results)

all_results_filter <- all_results %>%
  select(Pred_label, Sero_calling, mean_F1, mean_f1_ci) %>%
  distinct(Pred_label, Sero_calling, .keep_all = T) 

all_results_filter <- all_results_filter %>%
  mutate(Sero_calling = recode(Sero_calling, 
                             "cad_Enterobase" = "Canadian Enterobase",
                             "cad_Combined" = "Canadian and FoodNet",
                             "global_Enterobase" = "Global Enterobase")) %>%
  mutate(Pred_label = recode(Pred_label,
                             "Animal type" = "Animal",
                             "Location or Food" = "AnimalPlus \n(Location or Food)"))
  
# Create the dumbbell plot
p_all_results <- all_results_filter %>%
  ggplot(aes(x = mean_F1, y = Pred_label)) +
  
  # Add a line connecting the two Sero_calling methods for each Pred_label
  geom_line(aes(group = Pred_label), color = "#E7E7E7", linewidth = 3.5) +
  
  # Add points for the Sero_calling methods with different colors
  geom_point(aes(color = Sero_calling), size = 3) +
  
  # Add text labels for the confidence intervals (mean_f1_ci)
  geom_text_repel(aes(label = mean_f1_ci, color = Sero_calling),
                  size = 3.5,
                  nudge_x = 0.05,  # Adjust label placement for better spacing
                  direction = "y",  # Repel vertically to avoid overlap
                  hjust = 0.5,      # Center text labels
                  segment.size = 0.2,  # Line connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Customize the theme
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Set axis titles
  labs(x = "Mean F1 Score", y = "Predicted Category") +
  
  # Set custom x-axis scale (adjust based on range of F1 scores)
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  
  # Set custom colors for the Sero_calling methods
  scale_color_manual(values = c("FoodNet" = "#436685", "Global Enterobase" = "#BF2F24",
                                "Canadian Enterobase" = "darkgreen", "Canadian and FoodNet" = "lightgreen",
                                "global_Combined" = "pink"))  # Adjust colors as needed

######## Confusion matrices ###########
rm(full_cm_list) # To start feat_list as a blank slate
full_cm_list <- mget(ls(pattern = "^cm_"))
full_cm_list <- full_cm_list[!grepl("(_ci|_prop|_mean)$", names(full_cm_list))]

# Loop over each data frame
for (df_name in names(full_cm_list)) {
  # Get the data frame from the string name
  df <- full_cm_list[[df_name]]
  
  # Convert the first column to rownames and remove the first column
  rownames(df) <- df[, 1]
  df <- df[, -1]
  
  # Convert the data frame to a matrix
  df <- as.matrix(df)
  
  # Assign the modified matrix back to the original variable name
  assign(df_name, df)
}

calculate_mean_ci_matrix(cm_fn_animal1, cm_fn_animal2, cm_fn_animal3, 
                         output_mean = "cm_all_animal_fn_mean", output_ci = "cm_all_animal_fn_ci",
                         output_prop = "cm_all_animal_fn_prop")
calculate_mean_ci_matrix(cm_fn_animalPlus1, cm_fn_animalPlus2, cm_fn_animalPlus3, 
                         output_mean = "cm_all_animalPlus_fn_mean", output_ci = "cm_all_animalPlus_fn_ci",
                         output_prop = "cm_all_animalPlus_fn_prop")

calculate_mean_ci_matrix(cm_global_ebase_animal1, cm_global_ebase_animal2, cm_global_ebase_animal3, 
                         output_mean = "cm_all_animal_global_ebase_mean", 
                         output_ci = "cm_all_animal_global_ebase_ci",
                         output_prop = "cm_all_animal_global_ebase_prop")
calculate_mean_ci_matrix(cm_global_ebase_animalPlus1, cm_global_ebase_animalPlus2, cm_global_ebase_animalPlus3, 
                         output_mean = "cm_all_animalPlus_global_ebase_mean", 
                         output_ci = "cm_all_animalPlus_global_ebase_ci",
                         output_prop = "cm_all_animalPlus_global_ebase_prop")

calculate_mean_ci_matrix(cm_cad_ebase_animal1, cm_cad_ebase_animal2, cm_cad_ebase_animal3, 
                         output_mean = "cm_all_animal_cad_ebase_mean", 
                         output_ci = "cm_all_animal_cad_ebase_ci",
                         output_prop = "cm_all_animal_cad_ebase_prop")
calculate_mean_ci_matrix(cm_cad_ebase_animalPlus1, cm_cad_ebase_animalPlus2, cm_cad_ebase_animalPlus3, 
                         output_mean = "cm_all_animalPlus_cad_ebase_mean", 
                         output_ci = "cm_all_animalPlus_cad_ebase_ci",
                         output_prop = "cm_all_animalPlus_cad_ebase_prop")

calculate_mean_ci_matrix(cm_cad_combined_animal1, cm_cad_combined_animal2, cm_cad_combined_animal3, 
                         output_mean = "cm_all_animal_cad_combined_mean", 
                         output_ci = "cm_all_animal_cad_combined_ci",
                         output_prop = "cm_all_animal_cad_combined_prop")
calculate_mean_ci_matrix(cm_cad_combined_animalPlus1, cm_cad_combined_animalPlus2, cm_cad_combined_animalPlus3, 
                         output_mean = "cm_all_animalPlus_cad_combined_mean", 
                         output_ci = "cm_all_animalPlus_cad_combined_ci",
                         output_prop = "cm_all_animalPlus_cad_combined_prop")

## Confusion matrix plots ##
set_breaks <- seq(0, 100, by = 10)
set_colors <- colorRamp2(c(0, 59.9, 60, 100), c("pink", "pink", "#ccffcc", "#006600"))
gen_legend(set_breaks, set_colors, title = "Proportion (%)", direction = "vertical")

heat_fn_animal <- gen_heatmap(cm_all_animal_fn_prop, cm_all_animal_fn_ci, 
                              set_breaks, set_colors, fontsize = 25)

heat_fn_animalPlus <- gen_heatmap(cm_all_animalPlus_fn_prop, cm_all_animalPlus_fn_ci, 
                                  set_breaks, set_colors, fontsize = 20)
  
heat_global_ebase_animal <- gen_heatmap(cm_all_animal_global_ebase_prop, cm_all_animal_global_ebase_ci, 
                                       set_breaks, set_colors, fontsize = 25)
  
heat_global_ebase_animalPlus <- gen_heatmap(cm_all_animalPlus_global_ebase_prop, cm_all_animalPlus_global_ebase_ci, 
                                            set_breaks, set_colors, fontsize = 20)

heat_cad_ebase_animal <- gen_heatmap(cm_all_animal_cad_ebase_prop, cm_all_animal_cad_ebase_ci, 
                                     set_breaks, set_colors, fontsize = 25)

heat_cad_ebase_animalPlus <- gen_heatmap(cm_all_animalPlus_cad_ebase_prop, cm_all_animalPlus_cad_ebase_ci, 
                                         set_breaks, set_colors, fontsize = 20)

heat_cad_combined_animal <- gen_heatmap(cm_all_animal_cad_combined_prop, cm_all_animal_cad_combined_ci, 
                                     set_breaks, set_colors, fontsize = 25)

heat_cad_combined_animalPlus <- gen_heatmap(cm_all_animalPlus_cad_combined_prop, cm_all_animalPlus_cad_combined_ci, 
                                         set_breaks, set_colors, fontsize = 20)

##### Feature importances ######
if (exists("all_feat_list")) {
  rm(all_feat_list)
}

all_feat_list <- mget((ls(pattern = "^feat_")))

# Get all data frames with "animal" in their name but not "animalPlus"
animal_feat_list <- all_feat_list[grepl("animal", names(all_feat_list)) & !grepl("animalPlus", names(all_feat_list))]
# Get all data frames with "animalPlus" in their name
animalPlus_feat_list <- all_feat_list[grepl("animalPlus", names(all_feat_list))]

######### TESTING #########
calculate_feature_counts <- function(feature_list) {
  features_unique <- unique(unlist(lapply(feature_list, function(df) unique(df$Feature))))
  
  feature_counts <- sapply(features_unique, function(feature) {
    sum(sapply(feature_list, function(df) feature %in% df$Feature))
  })
  
  counts_table <- table(feature_counts)
  counts_df <- as.data.frame(counts_table)
  colnames(counts_df) <- c('n_df', 'count')
  counts_df$n_df <- as.numeric(as.character(counts_df$n_df))
  counts_df <- counts_df[order(counts_df$n_df), ]
  
  # Return both counts_df and feature_counts
  return(list(counts_df = counts_df, feature_counts = feature_counts))
}

# For feat_list
result_all <- calculate_feature_counts(all_feat_list)
counts_df <- result_all$counts_df
feature_counts <- result_all$feature_counts

# For feat_list_animal
result_animal <- calculate_feature_counts(animal_feat_list)
counts_df_animal <- result_animal$counts_df
feature_counts_animal <- result_animal$feature_counts

# For feat_list_animalPlus
result_animalPlus <- calculate_feature_counts(animalPlus_feat_list)
counts_df_animalPlus <- result_animalPlus$counts_df
feature_counts_animalPlus <- result_animalPlus$feature_counts

n_df_value <- 2

# For feat_list
features_in_2_dfs <- names(feature_counts[feature_counts == n_df_value])

# For feat_list_animal
features_in_2_dfs_animal <- names(feature_counts_animal[feature_counts_animal == n_df_value])

# For feat_list_animalPlus
features_in_2_dfs_animalPlus <- names(feature_counts_animalPlus[feature_counts_animalPlus == n_df_value])

# writeLines(features_in_2_dfs, "all_data_feat_2df.txt")
# writeLines(features_in_2_dfs_animal, "animal_data_feat_2df.txt")
# writeLines(features_in_2_dfs_animalPlus, "animalPlus_data_feat_2df.txt")

ggplot(counts_df, aes(x = as.numeric(n_df), y = count)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'black') +
  geom_line(aes(y = count), color = 'red', size = 1) +
  geom_point(aes(y = count), color = 'red', size = 2) +
  scale_x_continuous(breaks = seq(1, 13, by = 1)) + 
  scale_y_continuous(limits = c(0, 1400), breaks = seq(0, 1400, by = 200)) +
  labs(
    x = 'Number of Data Frames (n_df)',
    y = 'Number of Features (count)'
  ) +
  theme_bw()

ggplot(counts_df_animal, aes(x = as.numeric(n_df), y = count)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'black') +
  geom_line(aes(y = count), color = 'red', size = 1) +
  geom_point(aes(y = count), color = 'red', size = 2) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) + 
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) +
  labs(
    x = 'Number of Data Frames (n_df)',
    y = 'Number of Features (count)'
  ) +
  theme_bw()

ggplot(counts_df_animalPlus, aes(x = as.numeric(n_df), y = count)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'black') +
  geom_line(aes(y = count), color = 'red', size = 1) +
  geom_point(aes(y = count), color = 'red', size = 2) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + 
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) +
  labs(
    x = 'Number of Data Frames (n_df)',
    y = 'Number of Features (count)'
  ) +
  theme_bw()

## Checking if the test_pred labels actually match the confusion matrix -- YES 
# cmat_test <- confusionMatrix(as.factor(pred_fn_animal1$pred_label), as.factor(pred_fn_animal1$true_label))
# cmat_test2 <- confusionMatrix(as.factor(pred_global_ebase_animalPlus2$pred_label), as.factor(pred_global_ebase_animalPlus2$true_label))                     

# Function to collate and count features for a model
collate_and_count_features <- function(replicates_list) {
  # Combine replicates into one data frame
  combined_df <- do.call(rbind, replicates_list)
  
  # Count occurrences of each feature across replicates
  feature_counts <- table(combined_df$Feature)
  
  # Return the count table as a named vector
  return(feature_counts)
}

# Function to get features present in at least 2 out of 3 replicates
get_features_in_2_reps <- function(feature_counts) {
  return(names(feature_counts[feature_counts >= 2]))
}

# Step 1 & 2: Collate features and get counts for each model
models <- list(
  feat_cad_combined = collate_and_count_features(all_feat_list[grepl("^feat_cad_combined_animal", names(all_feat_list))]),
  feat_cad_ebase = collate_and_count_features(all_feat_list[grepl("^feat_cad_ebase_animal", names(all_feat_list))]),
  feat_fn = collate_and_count_features(all_feat_list[grepl("^feat_fn_animal", names(all_feat_list))]),
  feat_global_ebase = collate_and_count_features(all_feat_list[grepl("^feat_global_ebase_animal", names(all_feat_list))])
)

# Step 3: Get features in at least 2 replicates for each model
features_in_2_reps <- lapply(models, get_features_in_2_reps)

# Step 4: Compare features between models
# Get features present in at least 2 out of 4 models
compare_models <- function(features_list) {
  # Flatten the list to get all features across models
  all_features <- unique(unlist(features_list))
  
  # Count how many models each feature appears in
  model_feature_counts <- sapply(all_features, function(feature) {
    sum(sapply(features_list, function(features) feature %in% features))
  })
  
  # Return features present in at least 2 models
  return(names(model_feature_counts[model_feature_counts >= 2]))
}

# Step 5: Get and write the final list of features
features_in_2_models <- compare_models(features_in_2_reps)

# writeLines(features_in_2_models, "all_data_feat_2models.txt")
###################################################
############ Results for mini models ############
mini_global_enterobase_results <- read.csv("output/mini_global_Output_template_ebase.csv")
mini_global_enterobase_results_set2 <- read.csv("output/mini_global_Output_template_ebase_set2.csv")
mini_fn_results <- read.csv("output/mini_global_Output_template_fn_skesa.csv")
mini_global_combined_results <- read.csv("output/mini_global_Output_template_combined.csv")
mini_global_combined_results_set2 <- read.csv("output/mini_global_Output_template_combined_set2.csv")

mini_canada_only_results <- read.csv("output/canada_miniFeat_Output_template_ebase.csv")
mini_canada_combined_results <- read.csv("output/canada_miniFeat_Output_template_combined.csv")

######### Confusion matrices ##########
mini_cm_fn_animal1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_FoodNet_34.csv")
mini_cm_fn_animal2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_FoodNet_42.csv")
mini_cm_fn_animal3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_FoodNet_777.csv")
mini_cm_fn_animalPlus1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_FoodNet_34.csv")
mini_cm_fn_animalPlus2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_FoodNet_42.csv")
mini_cm_fn_animalPlus3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_FoodNet_777.csv")

mini_cm_global_ebase_animal1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_cm_global_ebase_animal2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_cm_global_ebase_animal3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_cm_global_ebase_animalPlus1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_cm_global_ebase_animalPlus2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_cm_global_ebase_animalPlus3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_cm_global_combined_animal1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Combined_34.csv")
mini_cm_global_combined_animal2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Combined_42.csv")
mini_cm_global_combined_animal3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSites_Combined_777.csv")
mini_cm_global_combined_animalPlus1 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_cm_global_combined_animalPlus2 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_cm_global_combined_animalPlus3 <- read.csv("cm/miniFeat_cm_All_Random Forest_CombinedSite2_Combined_777.csv")

mini_cm_global_ebase_animal1_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_cm_global_ebase_animal2_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_cm_global_ebase_animal3_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_cm_global_ebase_animalPlus1_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_cm_global_ebase_animalPlus2_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_cm_global_ebase_animalPlus3_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_cm_global_combined_animal1_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Combined_34.csv")
mini_cm_global_combined_animal2_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Combined_42.csv")
mini_cm_global_combined_animal3_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSites_Combined_777.csv")
mini_cm_global_combined_animalPlus1_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_cm_global_combined_animalPlus2_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_cm_global_combined_animalPlus3_set2 <- read.csv("cm/miniFeat_set2_cm_All_Random Forest_CombinedSite2_Combined_777.csv")

mini_cm_cad_ebase_animal1 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_cm_cad_ebase_animal2 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_cm_cad_ebase_animal3 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_cm_cad_ebase_animalPlus1 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_cm_cad_ebase_animalPlus2 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_cm_cad_ebase_animalPlus3 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_cm_cad_combined_animal1 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Combined_34.csv")
mini_cm_cad_combined_animal2 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Combined_42.csv")
mini_cm_cad_combined_animal3 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSites_Combined_777.csv")
mini_cm_cad_combined_animalPlus1 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_cm_cad_combined_animalPlus2 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_cm_cad_combined_animalPlus3 <- read.csv("cm/miniFeat_cad_cm_All_Random Forest_CombinedSite2_Combined_777.csv")

####### Pred labels #########
mini_pred_fn_animal1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_FoodNet_34.csv")
mini_pred_fn_animal2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_FoodNet_42.csv")
mini_pred_fn_animal3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_FoodNet_777.csv")
mini_pred_fn_animalPlus1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_FoodNet_34.csv")
mini_pred_fn_animalPlus2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_FoodNet_42.csv")
mini_pred_fn_animalPlus3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_FoodNet_777.csv")

mini_pred_global_ebase_animal1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_pred_global_ebase_animal2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_pred_global_ebase_animal3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_pred_global_ebase_animalPlus1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_pred_global_ebase_animalPlus2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_pred_global_ebase_animalPlus3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_pred_global_combined_animal1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Combined_34.csv")
mini_pred_global_combined_animal2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Combined_42.csv")
mini_pred_global_combined_animal3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSites_Combined_777.csv")
mini_pred_global_combined_animalPlus1 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_pred_global_combined_animalPlus2 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_pred_global_combined_animalPlus3 <- read.csv("test_pred_labels/miniFeat_pred_All_Random Forest_CombinedSite2_Combined_777.csv")

mini_pred_global_ebase_animal1_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_pred_global_ebase_animal2_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_pred_global_ebase_animal3_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_pred_global_ebase_animalPlus1_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_pred_global_ebase_animalPlus2_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_pred_global_ebase_animalPlus3_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_pred_global_combined_animal1_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Combined_34.csv")
mini_pred_global_combined_animal2_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Combined_42.csv")
mini_pred_global_combined_animal3_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSites_Combined_777.csv")
mini_pred_global_combined_animalPlus1_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_pred_global_combined_animalPlus2_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_pred_global_combined_animalPlus3_set2 <- read.csv("test_pred_labels/miniFeat_set2_pred_All_Random Forest_CombinedSite2_Combined_777.csv")

mini_pred_cad_ebase_animal1 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Enterobase_34.csv")
mini_pred_cad_ebase_animal2 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Enterobase_42.csv")
mini_pred_cad_ebase_animal3 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Enterobase_777.csv")
mini_pred_cad_ebase_animalPlus1 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Enterobase_34.csv")
mini_pred_cad_ebase_animalPlus2 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Enterobase_42.csv")
mini_pred_cad_ebase_animalPlus3 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Enterobase_777.csv")

mini_pred_cad_combined_animal1 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Combined_34.csv")
mini_pred_cad_combined_animal2 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Combined_42.csv")
mini_pred_cad_combined_animal3 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSites_Combined_777.csv")
mini_pred_cad_combined_animalPlus1 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Combined_34.csv")
mini_pred_cad_combined_animalPlus2 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Combined_42.csv")
mini_pred_cad_combined_animalPlus3 <- read.csv("test_pred_labels/miniFeat_cad_pred_All_Random Forest_CombinedSite2_Combined_777.csv")
##################################
mini_canada_grouped_results <- rbind(mini_canada_only_results, mini_canada_combined_results) %>%
  filter(Feat_elim_method == "None") %>% ## RFECV Does not improve scores
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "mini_cad_Enterobase",
                               "Combined" = "mini_cad_Combined")) %>%
  group_by(Pred_label, Sero_calling, Feat_elim_method) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))


## Calculating the confidence intervals ##
mini_global_enterobase_results <- mini_global_enterobase_results %>%
  mutate(set = "set1")
mini_global_enterobase_results_set2 <- mini_global_enterobase_results_set2 %>%
  mutate(set = "set2")
mini_global_combined_results <- mini_global_combined_results %>%
  mutate(set = "set1")
mini_global_combined_results_set2 <- mini_global_combined_results_set2 %>%
  mutate(set = "set2")
mini_fn_results <- mini_fn_results %>%
  mutate(set = "set1")

mini_global_enterobase_results_grouped <- rbind(mini_global_enterobase_results, 
                                                mini_global_enterobase_results_set2, 
                                                mini_global_combined_results,
                                                mini_global_combined_results_set2) %>%
  group_by(Pred_label, Sero_calling, Feat_elim_method, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

mini_fn_results_grouped <- mini_fn_results %>%
  group_by(Pred_label, Feat_elim_method) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

mini_grouped_results <- rbind(mini_global_enterobase_results, mini_global_combined_results, mini_fn_results) %>%
  filter(Feat_elim_method == "None") %>%
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "mini_global_Enterobase",
                               "Combined" = "mini_global_Combined",
                               "FoodNet" = "mini_FoodNet")) %>%
  group_by(Pred_label, Sero_calling) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

mini_all_results <- rbind(mini_canada_grouped_results, mini_grouped_results)

# Create the dumbbell plot
p_mini_all_results <- mini_all_results %>%
  ggplot(aes(x = mean_F1, y = Pred_label)) +
  
  # Add a line connecting the two Sero_calling methods for each Pred_label
  geom_line(aes(group = Pred_label), color = "#E7E7E7", linewidth = 3.5) +
  
  # Add points for the Sero_calling methods with different colors
  geom_point(aes(color = Sero_calling), size = 3) +
  
  # Add text labels for the confidence intervals (mean_f1_ci)
  geom_text_repel(aes(label = mean_f1_ci, color = Sero_calling),
                  size = 5,
                  nudge_x = 0.05,  # Adjust label placement for better spacing
                  direction = "y",  # Repel vertically to avoid overlap
                  hjust = 0.5,      # Center text labels
                  segment.size = 0.2,  # Line connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Customize the theme
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Set axis titles
  labs(x = "Mean F1 Score", y = "Predicted Category") +
  
  # Set custom x-axis scale (adjust based on range of F1 scores)
  scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.1), limits = c(0.2, 0.8)) +
  
  # Set custom colors for the Sero_calling methods
  scale_color_manual(values = c("mini_FoodNet" = "#436685", "mini_global_Enterobase" = "#BF2F24",
                                "mini_cad_Enterobase" = "darkgreen", "mini_cad_Combined" = "lightgreen",
                                "mini_global_Combined" = "pink"))  # Adjust colors as needed

mini_grouped_results_set <- rbind(mini_global_enterobase_results, mini_global_enterobase_results_set2, 
                                   mini_global_combined_results, mini_global_combined_results_set2) %>%
  filter(Feat_elim_method == "None") %>%
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "mini_global_Enterobase",
                               "Combined" = "mini_global_Combined",
                               "FoodNet" = "mini_FoodNet")) %>%
  group_by(Pred_label, Sero_calling, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

p_mini_all_results_set2 <- mini_grouped_results_set %>%
  ggplot(aes(x = mean_F1, y = Pred_label)) +
  
  # Add a line connecting the two Sero_calling methods for each Pred_label
  geom_line(aes(group = Pred_label), color = "#E7E7E7", linewidth = 3.5) +
  
  # Add points for the Sero_calling methods with different colors
  geom_point(aes(color = set), size = 3) +
  
  # Add text labels for the confidence intervals (mean_f1_ci)
  geom_text_repel(aes(label = mean_f1_ci, color = set),
                  size = 5,
                  nudge_x = 0.05,  # Adjust label placement for better spacing
                  direction = "y",  # Repel vertically to avoid overlap
                  hjust = 0.5,      # Center text labels
                  segment.size = 0.2,  # Line connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Customize the theme
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Set axis titles
  labs(x = "Mean F1 Score", y = "Predicted Category") +
  
  facet_wrap(~Sero_calling, scales="free_y") + 
  
  # Set custom x-axis scale (adjust based on range of F1 scores)
  scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.1), limits = c(0.2, 0.8)) +
  
  # Set custom colors for the Sero_calling methods
  scale_color_manual(values = c("set1" = "#436685", "set2" = "#BF2F24"))


if (exists("mini_full_cm_list")) {
  rm(mini_full_cm_list)
}
mini_full_cm_list <- mget(ls(pattern = "^mini_cm_"))
mini_full_cm_list <- mini_full_cm_list[!grepl("(_ci|_prop|_mean)$", names(mini_full_cm_list))]

# Loop over each data frame
for (df_name in names(mini_full_cm_list)) {
  # Get the data frame from the string name
  df <- mini_full_cm_list[[df_name]]
  
  # Convert the first column to rownames and remove the first column
  rownames(df) <- df[, 1]
  df <- df[, -1]
  
  # Convert the data frame to a matrix
  df[] <- lapply(df, function(col) as.numeric(as.character(col)))
  df <- as.matrix(df)
  
  # Assign the modified matrix back to the original variable name
  assign(df_name, df)
}

calculate_mean_ci_matrix(mini_cm_cad_combined_animal1, mini_cm_cad_combined_animal2, mini_cm_cad_combined_animal3, 
                         output_mean = "mini_cm_cad_combined_animal_mean", output_ci = "mini_cm_cad_combined_animal_ci",
                         output_prop = "mini_cm_cad_combined_animal_prop")

calculate_mean_ci_matrix(mini_cm_cad_combined_animalPlus1, mini_cm_cad_combined_animalPlus2, mini_cm_cad_combined_animalPlus3, 
                         output_mean = "mini_cm_cad_combined_animalPlus_mean", output_ci = "mini_cm_cad_combined_animalPlus_ci",
                         output_prop = "mini_cm_cad_combined_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_cad_ebase_animal1, mini_cm_cad_ebase_animal2, mini_cm_cad_ebase_animal3, 
                         output_mean = "mini_cm_cad_ebase_animal_mean", output_ci = "mini_cm_cad_ebase_animal_ci",
                         output_prop = "mini_cm_cad_ebase_animal_prop")

calculate_mean_ci_matrix(mini_cm_cad_ebase_animalPlus1, mini_cm_cad_ebase_animalPlus2, mini_cm_cad_ebase_animalPlus3, 
                         output_mean = "mini_cm_cad_ebase_animalPlus_mean", output_ci = "mini_cm_cad_ebase_animalPlus_ci",
                         output_prop = "mini_cm_cad_ebase_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_fn_animal1, mini_cm_fn_animal2, mini_cm_fn_animal3, 
                         output_mean = "mini_cm_fn_animal_mean", output_ci = "mini_cm_fn_animal_ci",
                         output_prop = "mini_cm_fn_animal_prop")

calculate_mean_ci_matrix(mini_cm_fn_animalPlus1, mini_cm_fn_animalPlus2, mini_cm_fn_animalPlus3, 
                         output_mean = "mini_cm_fn_animalPlus_mean", output_ci = "mini_cm_fn_animalPlus_ci",
                         output_prop = "mini_cm_fn_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_global_combined_animal1, mini_cm_global_combined_animal2, mini_cm_global_combined_animal3, 
                         output_mean = "mini_cm_global_combined_animal_mean", output_ci = "mini_cm_global_combined_animal_ci",
                         output_prop = "mini_cm_global_combined_animal_prop")

calculate_mean_ci_matrix(mini_cm_global_combined_animalPlus1, mini_cm_global_combined_animalPlus2, mini_cm_global_combined_animalPlus3, 
                         output_mean = "mini_cm_global_combined_animalPlus_mean", output_ci = "mini_cm_global_combined_animalPlus_ci",
                         output_prop = "mini_cm_global_combined_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_global_ebase_animal1, mini_cm_global_ebase_animal2, mini_cm_global_ebase_animal3, 
                         output_mean = "mini_cm_global_ebase_animal_mean", output_ci = "mini_cm_global_ebase_animal_ci",
                         output_prop = "mini_cm_global_ebase_animal_prop")

calculate_mean_ci_matrix(mini_cm_global_ebase_animalPlus1, mini_cm_global_ebase_animalPlus2, mini_cm_global_ebase_animalPlus3, 
                         output_mean = "mini_cm_global_ebase_animalPlus_mean", output_ci = "mini_cm_global_ebase_animalPlus_ci",
                         output_prop = "mini_cm_global_ebase_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_global_combined_animal1_set2, mini_cm_global_combined_animal2_set2, 
                         mini_cm_global_combined_animal3_set2, 
                         output_mean = "mini_cm_set2_global_combined_animal_mean", output_ci = "mini_cm_set2_global_combined_animal_ci",
                         output_prop = "mini_cm_set2_global_combined_animal_prop")

calculate_mean_ci_matrix(mini_cm_global_combined_animalPlus1_set2, mini_cm_global_combined_animalPlus2_set2, 
                         mini_cm_global_combined_animalPlus3_set2, 
                         output_mean = "mini_cm_set2_global_combined_animalPlus_mean", output_ci = "mini_cm_set2_global_combined_animalPlus_ci",
                         output_prop = "mini_cm_set2_global_combined_animalPlus_prop")

calculate_mean_ci_matrix(mini_cm_global_ebase_animal1_set2, mini_cm_global_ebase_animal2_set2, 
                         mini_cm_global_ebase_animal3_set2, 
                         output_mean = "mini_cm_set2_global_ebase_animal_mean", output_ci = "mini_cm_set2_global_ebase_animal_ci",
                         output_prop = "mini_cm_set2_global_ebase_animal_prop")

calculate_mean_ci_matrix(mini_cm_global_ebase_animalPlus1_set2, mini_cm_global_ebase_animalPlus2_set2, 
                         mini_cm_global_ebase_animalPlus3_set2, 
                         output_mean = "mini_cm_set2_global_ebase_animalPlus_mean", output_ci = "mini_cm_set2_global_ebase_animalPlus_ci",
                         output_prop = "mini_cm_set2_global_ebase_animalPlus_prop")

mini_heat_cad_combined_animal <- gen_heatmap(mini_cm_cad_combined_animal_prop, mini_cm_cad_combined_animal_ci, 
                              set_breaks, set_colors, fontsize = 25)
mini_heat_cad_combined_animalPlus <- gen_heatmap(mini_cm_cad_combined_animalPlus_prop, 
                                                      mini_cm_cad_combined_animalPlus_ci, 
                                                      set_breaks, set_colors, fontsize = 20)
mini_heat_cad_ebase_animal <- gen_heatmap(mini_cm_cad_ebase_animal_prop, mini_cm_cad_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
mini_heat_cad_ebase_animalPlus <- gen_heatmap(mini_cm_cad_ebase_animalPlus_prop, 
                                                 mini_cm_cad_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)

mini_heat_global_combined_animal <- gen_heatmap(mini_cm_global_combined_animal_prop, mini_cm_global_combined_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
mini_heat_global_combined_animalPlus <- gen_heatmap(mini_cm_global_combined_animalPlus_prop, 
                                                 mini_cm_global_combined_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)
mini_heat_global_ebase_animal <- gen_heatmap(mini_cm_global_ebase_animal_prop, mini_cm_global_ebase_animal_ci, 
                                          set_breaks, set_colors, fontsize = 25)
mini_heat_global_ebase_animalPlus <- gen_heatmap(mini_cm_global_ebase_animalPlus_prop, 
                                              mini_cm_global_ebase_animalPlus_ci, 
                                              set_breaks, set_colors, fontsize = 20)

mini_heat_fn_animal <- gen_heatmap(mini_cm_fn_animal_prop, mini_cm_fn_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
mini_heat_fn_animalPlus <- gen_heatmap(mini_cm_fn_animalPlus_prop, 
                                                 mini_cm_fn_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)

mini_heat_set2_global_combined_animal <- gen_heatmap(mini_cm_set2_global_combined_animal_prop, mini_cm_set2_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)
mini_heat_set2_global_combined_animalPlus <- gen_heatmap(mini_cm_set2_global_combined_animalPlus_prop, 
                                                    mini_cm_set2_global_combined_animalPlus_ci, 
                                                    set_breaks, set_colors, fontsize = 20)
mini_heat_set2_global_ebase_animal <- gen_heatmap(mini_cm_set2_global_ebase_animal_prop, mini_cm_set2_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
mini_heat_set2_global_ebase_animalPlus <- gen_heatmap(mini_cm_set2_global_ebase_animalPlus_prop, 
                                                 mini_cm_set2_global_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)

#### Pred Label Analysis #########
if (exists("mini_full_pred_list")) {
  rm(mini_full_pred_list)
}
mini_full_pred_list <- mget(ls(pattern = "^mini_pred_"))

# Initialize an empty list to store modified dataframes
modified_pred_dfs <- list()

# Loop over each data frame in mini_full_pred_list
for (df_name in names(mini_full_pred_list)) {
  # Get the data frame by its name
  df <- mini_full_pred_list[[df_name]]
  
  # 1) Create the "Sero_calling" column based on df_name patterns
  if (grepl("cad_combined", df_name)) {
    df$Sero_calling <- "cad_Combined"
  } else if (grepl("cad_ebase", df_name)) {
    df$Sero_calling <- "cad_Enterobase"
  } else if (grepl("fn", df_name)) {
    df$Sero_calling <- "FoodNet"
  } else if (grepl("global_ebase", df_name)) {
    df$Sero_calling <- "global_Enterobase"
  } else if (grepl("global_combined", df_name)) {
    df$Sero_calling <- "global_Combined"
  } else {
    df$Sero_calling <- NA  # In case the df_name does not match any known pattern
  }
  
  # 2) Create the "set" column based on whether "set2" is in df_name
  if (grepl("set2", df_name)) {
    df$set <- "set2"
  } else {
    df$set <- "set1"
  }
  
  # Store the modified dataframe in the list
  modified_pred_dfs[[df_name]] <- df
}

final_pred_df <- do.call(rbind, modified_pred_dfs)
pred_df_animal <- final_pred_df %>% filter(Pred_label == "CombinedSites")
pred_df_animalPlus <- final_pred_df %>% filter(Pred_label == "CombinedSite2")

pred_df_animal <- pred_df_animal %>%
  filter(Sero_calling %in% c("FoodNet", "global_Combined", "global_Enterobase"))
pred_df_animalPlus <- pred_df_animalPlus %>%
  filter(Sero_calling %in% c("FoodNet", "global_Combined", "global_Enterobase"))

wrong_label_animal <- pred_df_animal %>%
  filter(true_label == pred_label) %>%
  count(Serovar, Sero_calling, set, sort = T)

top_serovars_animal <- wrong_label_animal %>%
  group_by(Serovar) %>%
  summarise(total = sum(n)) %>%
  top_n(10, total) %>%
  pull(Serovar)

wrong_labels_top_animal <- wrong_label_animal %>%
  filter(Serovar %in% top_serovars_animal)

p_serovar_true_label_animal <- ggplot(wrong_labels_top_animal, aes(x = Serovar, y = n, fill = Serovar)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars
  facet_grid(set ~ Sero_calling) +  # Facet by Sero_calling (x) and set (y)
  labs(x = "Serovar",  # Remove x-axis label
       y = "Count of Wrong Labels") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Optionally remove vertical grid lines
  scale_fill_manual(values = RColorBrewer::brewer.pal(10, "Paired"))  # Use a palette with 10 colors

wrong_label_animal <- pred_df_animal %>%
  filter(true_label != pred_label) %>%
  count(Serovar, Sero_calling, set, true_label, sort = TRUE)

# 2) Identify the top 10 Serovars based on total wrong labels
top_serovars_animal <- wrong_label_animal %>%
  group_by(Serovar) %>%
  summarise(total = sum(n)) %>%
  top_n(10, total) %>%
  pull(Serovar)

# 3) Filter the dataset to include only top Serovars
wrong_labels_top_animal <- wrong_label_animal %>%
  filter(Serovar %in% top_serovars_animal)

# 4) Calculate proportions for each combination of Serovar, Sero_calling, and set
wrong_labels_proportions_animal <- wrong_labels_top_animal %>%
  group_by(Serovar, Sero_calling, set) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

p_true_label_animal <- ggplot(wrong_labels_proportions_animal, aes(x = Serovar, y = proportion, fill = true_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  facet_grid(set ~ Sero_calling) +  # Facet by Sero_calling and set
  labs(x = "Serovar", y = "Proportion (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

ggarrange(p_serovar_true_label_animal, p_true_label_animal, align = "hv", labels = "AUTO", nrow = 2)

wrong_label_animalPlus <- pred_df_animalPlus %>%
  filter(true_label == pred_label) %>%
  count(Serovar, Sero_calling, set, sort = T)

top_serovars_animalPlus <- wrong_label_animalPlus %>%
  group_by(Serovar) %>%
  summarise(total = sum(n)) %>%
  top_n(10, total) %>%
  pull(Serovar)

wrong_labels_top_animalPlus <- wrong_label_animalPlus %>%
  filter(Serovar %in% top_serovars_animalPlus)

p_serovar_true_label_animalPlus <- ggplot(wrong_labels_top_animalPlus, aes(x = Serovar, y = n, fill = Serovar)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' for side-by-side bars
  facet_grid(set ~ Sero_calling) +  # Facet by Sero_calling (x) and set (y)
  labs(x = "Serovar",  # Remove x-axis label
       y = "Count of Wrong Labels") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Optionally remove vertical grid lines
  scale_fill_manual(values = RColorBrewer::brewer.pal(10, "Paired"))  # Use a palette with 10 colors

wrong_label_animalPlus <- pred_df_animalPlus %>%
  filter(true_label != pred_label) %>%
  count(Serovar, Sero_calling, set, true_label, sort = TRUE)

# 2) Identify the top 10 Serovars based on total wrong labels
top_serovars_animalPlus <- wrong_label_animalPlus %>%
  group_by(Serovar) %>%
  summarise(total = sum(n)) %>%
  top_n(10, total) %>%
  pull(Serovar)

# 3) Filter the dataset to include only top Serovars
wrong_labels_top_animalPlus <- wrong_label_animalPlus %>%
  filter(Serovar %in% top_serovars_animalPlus)

# 4) Calculate proportions for each combination of Serovar, Sero_calling, and set
wrong_labels_proportions_animalPlus <- wrong_labels_top_animalPlus %>%
  group_by(Serovar, Sero_calling, set) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

p_true_label_animalPlus <- ggplot(wrong_labels_proportions_animalPlus, aes(x = Serovar, y = proportion, fill = true_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  facet_grid(set ~ Sero_calling) +  # Facet by Sero_calling and set
  labs(x = "Serovar", y = "Proportion (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

ggarrange(p_serovar_true_label_animalPlus, p_true_label_animalPlus, align = "hv", labels = "AUTO", nrow = 2)

############ results for serovar datasets ###############
serovar_global_enterobase_results <- read.csv("output/serovar_global_Output_template_ebase.csv")
serovar_fn_results <- read.csv("output/serovar_global_Output_template_fn_skesa.csv")
serovar_global_combined_results <- read.csv("output/serovar_global_Output_template_combined.csv")

serovar_results_global <- rbind(serovar_global_enterobase_results, serovar_fn_results, serovar_global_combined_results) %>%
  group_by(Pred_label, Sero_calling, Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2))) %>%
  mutate(Sero_calling = recode(Sero_calling, 
                               "Enterobase" = "Global Enterobase",
                               "Combined" = "Global Combined")) %>%
  mutate(Pred_label = recode(Pred_label, 
                               "Animal type" = "Animal",
                               "Location or Food" = "AnimalPlus (Location or Food)")) 

# Create the dumbbell plot
p_serovar_results <- serovar_results_global %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Add a line connecting the two Sero_calling methods for each Pred_label
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Add points for the Sero_calling methods with different colors
  geom_point(aes(color = Sero_calling), size = 3) +
  
  # Add text labels for the confidence intervals (mean_f1_ci)
  geom_text_repel(aes(label = mean_f1_ci, color = Sero_calling),
                  size = 3.5,
                  nudge_x = 0.05,  # Adjust label placement for better spacing
                  direction = "y",  # Repel vertically to avoid overlap
                  hjust = 0.5,      # Center text labels
                  segment.size = 0.2,  # Line connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Customize the theme
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Set axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  facet_wrap(~Pred_label, scales = "free_y") +
  
  # Set custom x-axis scale (adjust based on range of F1 scores)
  scale_x_continuous(breaks = seq(0, 1.2, by = 0.1), limits = c(0, 1.2)) +
  
  # Set custom colors for the Sero_calling methods
  scale_color_manual(values = c("FoodNet" = "#436685", "Global Enterobase" = "#BF2F24", 
                                "Global Combined" = "lightgreen")) 

############ Serovar Analysis ##############
# Define the serovar name mappings
serovar_mapping <- list(
  Heidelberg = "heid",
  Enteritidis = "entr",
  Typhimurium = "typh",
  Kentucky = "kent",
  Infantis = "infantis",
  i4512 = "i4512"
)

folder_to_run <- "cm/"
folder_to_run2 <- "features/"
folder_to_run3 <- "test_pred_labels/"

read_and_assign(folder_to_run)
read_and_assign(folder_to_run2)
read_and_assign(folder_to_run3)

# Get all variable names in the environment
all_vars <- ls()

cm_serovar <- list()
feat_serovar <- list()
pred_serovar <- list()

# Loop through each entry in the serovar mapping
for (name in names(serovar_mapping)) {
  abbrev <- serovar_mapping[[name]]
  
  # Filter variables for each category using grepl
  cm_serovar[[name]] <- all_vars[grepl(paste0("^cm_", abbrev), all_vars)]
  feat_serovar[[name]] <- all_vars[grepl(paste0("^feat_", abbrev), all_vars)]
  pred_serovar[[name]] <- all_vars[grepl(paste0("^pred_", abbrev), all_vars)]
}

print(cm_serovar)
print(feat_serovar)
print(pred_serovar)

# Function to filter variables that do not end with _ci, _prop, or _mean
filter_variables <- function(var_list) {
  lapply(var_list, function(vars) {
    vars[!grepl("(_ci|_prop|_mean)$", vars)]
  })
}

# Apply the filtering function to each serovar list
cm_serovar <- filter_variables(cm_serovar)
feat_serovar <- filter_variables(feat_serovar)
pred_serovar <- filter_variables(pred_serovar)

# Print the filtered lists for verification
print(cm_serovar)
print(feat_serovar)
print(pred_serovar)

############ Serovar CM ##############
# Flatten the character vectors in cm_serovar into a single vector of data frame names
serovar_cm_dfs <- unlist(cm_serovar, use.names = FALSE)

# Extract the data frames into a flat list using mget
serovar_cm_list <- mget(serovar_cm_dfs, envir = .GlobalEnv)

# Loop over each data frame
for (df_name in names(serovar_cm_list)) {
  # Get the data frame from the string name
  df <- serovar_cm_list[[df_name]]
  
  # Convert the first column to rownames and remove the first column
  rownames(df) <- df[, 1]
  df <- df[, -1]
  
  # Convert the data frame to a matrix
  df[] <- lapply(df, function(col) as.numeric(as.character(col)))
  df <- as.matrix(df)
  
  # Assign the modified matrix back to the original variable name
  assign(df_name, df)
}

# Define a function to group data frames by serovar, dataset, and label type
serovar_cm_group <- function(df_list) {
  # Initialize a nested list to hold serovar, dataset, and label-type groups
  grouped_dfs <- list()
  
  # Loop through each data frame name in the list
  for (df_name in names(df_list)) {
    # Adjusted regex to capture dataset with underscores and replicate number
    matches <- regmatches(df_name, regexec("^cm_([a-z0-9]+)_([a-z0-9_]+)_(animal|animalPlus)_(\\d+)$", df_name))
    if (length(matches[[1]]) == 5) {
      serovar <- matches[[1]][2]    # Serovar (e.g., "heid", "entr", etc.)
      dataset <- matches[[1]][3]    # Dataset (e.g., "fn", "global_combined", etc.)
      label_type <- matches[[1]][4] # Label type ("animal" or "animalPlus")
      replicate <- matches[[1]][5]  # Replicate number (e.g., "1", "2", etc.)
      
      # Create nested lists for serovar, dataset, and label type
      if (!is.list(grouped_dfs[[serovar]])) grouped_dfs[[serovar]] <- list()
      if (!is.list(grouped_dfs[[serovar]][[dataset]])) grouped_dfs[[serovar]][[dataset]] <- list()
      if (!is.list(grouped_dfs[[serovar]][[dataset]][[label_type]])) grouped_dfs[[serovar]][[dataset]][[label_type]] <- list()
      
      # Add the data frame name to the appropriate group
      grouped_dfs[[serovar]][[dataset]][[label_type]] <- c(grouped_dfs[[serovar]][[dataset]][[label_type]], df_name)
    }
  }
  
  return(grouped_dfs)
}

serovar_grouped_cm <- serovar_cm_group(serovar_cm_list)

# Loop through each serovar in the grouped list
for (serovar in names(serovar_grouped_cm)) {
  # Loop through each dataset within the serovar
  for (dataset in names(serovar_grouped_cm[[serovar]])) {
    # Loop through each label type within the dataset
    for (label_type in names(serovar_grouped_cm[[serovar]][[dataset]])) {
      # Get the list of replicates for the current combination
      replicates <- serovar_grouped_cm[[serovar]][[dataset]][[label_type]]
      
      # Ensure there are exactly 3 replicates
      if (length(replicates) == 3) {
        # Extract the replicate data frames
        df1 <- get(replicates[[1]])
        df2 <- get(replicates[[2]])
        df3 <- get(replicates[[3]])
        
        # Define output names for the mean, CI, and prop matrices
        output_mean <- paste0("cm_", serovar, "_", dataset, "_", label_type, "_mean")
        output_ci <- paste0("cm_", serovar, "_", dataset, "_", label_type, "_ci")
        output_prop <- paste0("cm_", serovar, "_", dataset, "_", label_type, "_prop")
        
        # Call the function on the replicates
        calculate_mean_ci_matrix(df1, df2, df3, 
                                 output_mean = output_mean, 
                                 output_ci = output_ci, 
                                 output_prop = output_prop)
      }
    }
  }
}

## Heidelberg cm heat map ##
heid_fn_heat_animal <- gen_heatmap(cm_heid_fn_animal_prop, cm_heid_fn_animal_ci, 
                            set_breaks, set_colors, fontsize = 25)
heid_global_ebase_heat_animal <- gen_heatmap(cm_heid_global_ebase_animal_prop, cm_heid_global_ebase_animal_ci, 
                            set_breaks, set_colors, fontsize = 25)
heid_global_combined_heat_animal <- gen_heatmap(cm_heid_global_combined_animal_prop, cm_heid_global_combined_animal_ci, 
                            set_breaks, set_colors, fontsize = 25)

heid_fn_heat_animalPlus <- gen_heatmap(cm_heid_fn_animalPlus_prop, cm_heid_fn_animalPlus_ci, 
                                   set_breaks, set_colors, fontsize = 20)
heid_global_ebase_heat_animalPlus <- gen_heatmap(cm_heid_global_ebase_animalPlus_prop, cm_heid_global_ebase_animalPlus_ci, 
                                             set_breaks, set_colors, fontsize = 20)
heid_global_combined_heat_animalPlus <- gen_heatmap(cm_heid_global_combined_animalPlus_prop, cm_heid_global_combined_animalPlus_ci, 
                                                set_breaks, set_colors, fontsize = 20)

## Enteritidis cm heat map ##
entr_fn_heat_animal <- gen_heatmap(cm_entr_fn_animal_prop, cm_entr_fn_animal_ci, 
                                   set_breaks, set_colors, fontsize = 25)
entr_global_ebase_heat_animal <- gen_heatmap(cm_entr_global_ebase_animal_prop, cm_entr_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
entr_global_combined_heat_animal <- gen_heatmap(cm_entr_global_combined_animal_prop, cm_entr_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)

entr_fn_heat_animalPlus <- gen_heatmap(cm_entr_fn_animalPlus_prop, cm_entr_fn_animalPlus_ci, 
                                       set_breaks, set_colors, fontsize = 20)
entr_global_ebase_heat_animalPlus <- gen_heatmap(cm_entr_global_ebase_animalPlus_prop, cm_entr_global_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)
entr_global_combined_heat_animalPlus <- gen_heatmap(cm_entr_global_combined_animalPlus_prop, cm_entr_global_combined_animalPlus_ci, 
                                                    set_breaks, set_colors, fontsize = 20)

## Typhimurium cm heat map ##
typh_fn_heat_animal <- gen_heatmap(cm_typh_fn_animal_prop, cm_typh_fn_animal_ci, 
                                   set_breaks, set_colors, fontsize = 25)
typh_global_ebase_heat_animal <- gen_heatmap(cm_typh_global_ebase_animal_prop, cm_typh_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
typh_global_combined_heat_animal <- gen_heatmap(cm_typh_global_combined_animal_prop, cm_typh_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)

typh_fn_heat_animalPlus <- gen_heatmap(cm_typh_fn_animalPlus_prop, cm_typh_fn_animalPlus_ci, 
                                       set_breaks, set_colors, fontsize = 20)
typh_global_ebase_heat_animalPlus <- gen_heatmap(cm_typh_global_ebase_animalPlus_prop, cm_typh_global_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)
typh_global_combined_heat_animalPlus <- gen_heatmap(cm_typh_global_combined_animalPlus_prop, cm_typh_global_combined_animalPlus_ci, 
                                                    set_breaks, set_colors, fontsize = 20)
## i4512 cm heat map ##
i4512_fn_heat_animal <- gen_heatmap(cm_i4512_fn_animal_prop, cm_i4512_fn_animal_ci, 
                                   set_breaks, set_colors, fontsize = 25)
i4512_global_ebase_heat_animal <- gen_heatmap(cm_i4512_global_ebase_animal_prop, cm_i4512_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
i4512_global_combined_heat_animal <- gen_heatmap(cm_i4512_global_combined_animal_prop, cm_i4512_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)

i4512_fn_heat_animalPlus <- gen_heatmap(cm_i4512_fn_animalPlus_prop, cm_i4512_fn_animalPlus_ci, 
                                       set_breaks, set_colors, fontsize = 20)
i4512_global_ebase_heat_animalPlus <- gen_heatmap(cm_i4512_global_ebase_animalPlus_prop, cm_i4512_global_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)
i4512_global_combined_heat_animalPlus <- gen_heatmap(cm_i4512_global_combined_animalPlus_prop, cm_i4512_global_combined_animalPlus_ci, 
                                                    set_breaks, set_colors, fontsize = 20)
## Kentucky cm heat map ##
kent_fn_heat_animal <- gen_heatmap(cm_kent_fn_animal_prop, cm_kent_fn_animal_ci, 
                                    set_breaks, set_colors, fontsize = 25)
kent_global_ebase_heat_animal <- gen_heatmap(cm_kent_global_ebase_animal_prop, cm_kent_global_ebase_animal_ci, 
                                              set_breaks, set_colors, fontsize = 25)
kent_global_combined_heat_animal <- gen_heatmap(cm_kent_global_combined_animal_prop, cm_kent_global_combined_animal_ci, 
                                                 set_breaks, set_colors, fontsize = 25)

kent_fn_heat_animalPlus <- gen_heatmap(cm_kent_fn_animalPlus_prop, cm_kent_fn_animalPlus_ci, 
                                        set_breaks, set_colors, fontsize = 20)
kent_global_ebase_heat_animalPlus <- gen_heatmap(cm_kent_global_ebase_animalPlus_prop, cm_kent_global_ebase_animalPlus_ci, 
                                                  set_breaks, set_colors, fontsize = 20)
kent_global_combined_heat_animalPlus <- gen_heatmap(cm_kent_global_combined_animalPlus_prop, cm_kent_global_combined_animalPlus_ci, 
                                                     set_breaks, set_colors, fontsize = 20)
## Infantis cm heat map ##
infantis_fn_heat_animal <- gen_heatmap(cm_infantis_fn_animal_prop, cm_infantis_fn_animal_ci, 
                                   set_breaks, set_colors, fontsize = 25)
infantis_global_ebase_heat_animal <- gen_heatmap(cm_infantis_global_ebase_animal_prop, cm_infantis_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
infantis_global_combined_heat_animal <- gen_heatmap(cm_infantis_global_combined_animal_prop, cm_infantis_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)

infantis_fn_heat_animalPlus <- gen_heatmap(cm_infantis_fn_animalPlus_prop, cm_infantis_fn_animalPlus_ci, 
                                       set_breaks, set_colors, fontsize = 20)
infantis_global_ebase_heat_animalPlus <- gen_heatmap(cm_infantis_global_ebase_animalPlus_prop, cm_infantis_global_ebase_animalPlus_ci, 
                                                 set_breaks, set_colors, fontsize = 20)
infantis_global_combined_heat_animalPlus <- gen_heatmap(cm_infantis_global_combined_animalPlus_prop, cm_infantis_global_combined_animalPlus_ci, 
                                                    set_breaks, set_colors, fontsize = 20)
############ Serovar Features ##########
serovar_feat_df <- unlist(feat_serovar, use.names = F)
serovar_feat_list <- mget(serovar_feat_df, envir = .GlobalEnv)

# For feat_list
serovar_feat_all <- calculate_feature_counts(serovar_feat_list)
serovar_counts_df <- serovar_feat_all$counts_df
serovar_feature_counts <- serovar_feat_all$feature_counts

# For feat_list
serovar_shared_feat_2 <- names(serovar_feature_counts[serovar_feature_counts == n_df_value])
# writeLines(serovar_shared_feat_2, "serovar_feat_2df.txt")

# Generate shared features from "All" And "Serovar" features
all_and_sero_feats <- c(serovar_feat_list, all_feat_list)
all_and_serovar_feat_all <- calculate_feature_counts(all_and_sero_feats)
all_and_serovar_counts_df <- all_and_serovar_feat_all$counts_df
all_and_serovar_feature_counts <- all_and_serovar_feat_all$feature_counts
all_and_serovar_shared_feat_2 <- names(all_and_serovar_feature_counts[all_and_serovar_feature_counts == n_df_value])
# writeLines(all_and_serovar_shared_feat_2, "../global_enterobase/all_and_serovar_feat_2df.txt")

############ Serovar pred_label ############
serovar_pred_list <- unlist(pred_serovar, use.names = F)

############## Mini Feat with Serovar Data + Other feats ############
mini_seed_results <- read.csv("output/mini_modelFeat.csv")
mini_serovar_results <- read.csv("output/mini_model_serovarFeat.csv")
mini_all_and_sero_results <- read.csv("output/mini_model_all_and_seroFeat.csv")

miniSero_results <- read.csv("output/mini_serovar_all_clean.csv")
  
mini_seed_results_group <- mini_seed_results %>%
  group_by(Pred_label, Sero_calling, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

mini_serovar_results_group <- mini_serovar_results %>%
  group_by(Pred_label, Sero_calling, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

mini_all_and_sero_results_group <- mini_all_and_sero_results %>%
  group_by(Pred_label, Sero_calling, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

miniSero_results_group <- miniSero_results %>%
  group_by(Pred_label, Serovar, Sero_calling, feat_set, set) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_F1 = mean(F1),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

# miniSero_results_group_model <- miniSero_results %>%
#   group_by(Pred_label, Serovar, Sero_calling, feat_set, set, model_type) %>%
#   summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
#             mean_F1 = mean(F1),
#             mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))

full_results1 <- grouped_results %>%
  mutate(feat_set = "All features",
         Serovar = "All", 
         set = "Set1") %>% ungroup()
full_results2 <- serovar_results_global %>%
  mutate(feat_set = "All features", 
         set = "Set1") %>% ungroup()
mini_results1 <- mini_all_and_sero_results_group %>%
  mutate(feat_set = "all_and_serovar",
         Serovar = "All") %>%
  ungroup()
mini_results2 <- mini_serovar_results_group %>%
  mutate(feat_set = "serovar",
         Serovar = "All") %>%
  ungroup()
mini_results3 <- mini_global_enterobase_results_grouped %>%
  filter(Feat_elim_method == "None") %>%
  ungroup() %>%
  select(-Feat_elim_method) %>%
  mutate(Serovar = "All", 
         feat_set = "all_shared") 
mini_results4 <- mini_fn_results_grouped %>%
  filter(Feat_elim_method == "None") %>%
  ungroup() %>% 
  select(-Feat_elim_method) %>%
  mutate(Serovar = "All",
         feat_set = "all_shared", 
         set = "Set1", 
         Sero_calling = "FoodNet")

full_all_results <- bind_rows(full_results1, full_results2)
mini_all_results <- bind_rows(miniSero_results_group, mini_results1, mini_results2, mini_results3,
                              mini_results4) 
  
complete_results <- bind_rows(full_all_results, mini_all_results) %>%
  mutate(Sero_calling = recode(Sero_calling, "Enterobase" = "Global Enterobase",
                               "global_Enterobase" = "Global Enterobase",
                               "Combined" = "Global Combined"), 
         feat_set = recode(feat_set, "all_and_serovar" = "All and Serovar shared",
                           "all_shared" = "All shared",
                           "serovar" = "Serovar shared"),
         Pred_label = recode(Pred_label, "Animal" = "Animal type", 
                             "Location or Food" = "AnimalPlus (Location or Food)"),
         set = recode(set, "set1" = "Set1", "set2" = "Set2"),
         feat_set = recode(feat_set, "All features" = "Full model"))

complete_results %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Add a line connecting the two Sero_calling methods
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Add points for the Sero_calling methods with different colors
  geom_point(aes(color = Sero_calling, shape = feat_set), size = 3) +
  
  facet_grid(set ~ Pred_label, scales = "free_y") + 
  
  # Customize the theme
  theme_bw() +
  theme(
    legend.position = "bottom",  # Move legends to the bottom
    legend.box = "horizontal",  # Arrange legends horizontally
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),  # Adjust legend text size
    legend.spacing = unit(0.5, "cm")        # Add spacing between legend items
  ) +

  # Set axis titles
  labs(x = "Mean F1-Score", y = "Serovar") +
  
  # Set custom x-axis scale (adjust based on range of F1 scores)
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  
  # Set custom colors for the Sero_calling methods
  scale_color_manual(name = "Dataset",
                     values = c("FoodNet" = "#1b9e77", "Global Enterobase" = "#d95f02",
                                "Global Combined" = "#7570b3")) +  # Adjust colors as needed
  # Set custom shapes for the feat_set column
  scale_shape_manual(name = "Feature set",
                     values = c("Full model" = 8, "All and Serovar shared" = 17, 
                                "All shared" = 15, "Serovar shared" = 18))

## Table format instead of plot above ##
complete_results_wide <- complete_results %>%
  pivot_wider(id_cols = c(Pred_label, Sero_calling, Serovar, set),
              names_from = feat_set, 
              values_from = mean_f1_ci)
####### Aggregated heatmap ###########
folder_to_run4 <- "cm/all_and_serovar_miniFeat/"
folder_to_run4b <- "cm/serovar_miniFeat/"
read_and_assign2(folder_to_run4b, prefix_name = "mini_seroFeat_all")
read_and_assign2(folder_to_run4, prefix_name = "mini_all_seroFeat_all")

folder_to_run5 <- "cm/miniSero_all_and_serovarFeat/"
folder_to_run6 <- "cm/miniSero_allFeat/"
folder_to_run7 <- "cm/miniSero_serovarFeat/"
read_and_assign2(folder_to_run5, prefix_name = "miniSero_all_and_serovarFeat")
read_and_assign2(folder_to_run6, prefix_name = "miniSero_allFeat")
read_and_assign2(folder_to_run7, prefix_name = "miniSero_serovarFeat")

if (exists("mini_seroFeat_cm_list")) {
  rm(mini_seroFeat_cm_list)
}
mini_seroFeat_cm_list <- mget(ls(pattern = "^mini_seroFeat_"))
mini_seroFeat_cm_list <- mini_seroFeat_cm_list[!grepl("(_ci|_prop|_mean)$", names(mini_seroFeat_cm_list))]

if (exists("mini_all_and_seroFeat_cm_list")) {
  rm(mini_all_and_seroFeat_cm_list)
}
mini_all_and_seroFeat_cm_list <- mget(ls(pattern = "^mini_all_seroFeat_all"))
mini_all_and_seroFeat_cm_list <- mini_all_and_seroFeat_cm_list[!grepl("(_ci|_prop|_mean)$", 
                                                                      names(mini_all_and_seroFeat_cm_list))]

if (exists("miniSero_all_and_serovarFeat")) {
  rm(miniSero_all_and_serovarFeat)
}
miniSero_all_and_serovarFeat <- mget(ls(pattern = "^miniSero_all_and_serovarFeat"))
miniSero_all_and_serovarFeat <- miniSero_all_and_serovarFeat[!grepl("(_ci|_prop|_mean)$", 
                                                                      names(miniSero_all_and_serovarFeat))]

if (exists("miniSero_allFeat")) {
  rm(miniSero_allFeat)
}
miniSero_allFeat <- mget(ls(pattern = "^miniSero_allFeat"))
miniSero_allFeat <- miniSero_allFeat[!grepl("(_ci|_prop|_mean)$", names(miniSero_allFeat))]

if (exists("miniSero_serovarFeat")) {
  rm(miniSero_serovarFeat)
}
miniSero_serovarFeat <- mget(ls(pattern = "^miniSero_serovarFeat"))
miniSero_serovarFeat <- miniSero_serovarFeat[!grepl("(_ci|_prop|_mean)$", 
                                                                      names(miniSero_serovarFeat))]

convert_to_matrices <- function(df_list) {
  # Loop over each data frame in the list
  for (df_name in names(df_list)) {
    # Get the data frame by name
    df <- df_list[[df_name]]
    
    # Convert the first column to row names and remove the first column
    rownames(df) <- df[, 1]
    df <- df[, -1]
    
    # Convert the data frame to a matrix
    df_matrix <- as.matrix(df)
    
    # Assign the modified matrix back to the global environment
    assign(df_name, df_matrix, envir = .GlobalEnv)
  }
}

convert_to_matrices(mini_seroFeat_cm_list)
convert_to_matrices(mini_all_and_seroFeat_cm_list)
convert_to_matrices(miniSero_all_and_serovarFeat)
convert_to_matrices(miniSero_allFeat)
convert_to_matrices(miniSero_serovarFeat)

full_cm_list2 <- full_cm_list[!grepl("(cm_cad_combined_|cm_cad_ebase)", names(full_cm_list))]

all_cm_list <- c(mini_seroFeat_cm_list, full_cm_list2, serovar_cm_list, mini_all_and_seroFeat_cm_list)
names(all_cm_list)

calculate_mean_ci_matrix(mini_seroFeat_all_fn_animal_NA_set1_1, mini_seroFeat_all_fn_animal_NA_set1_2, 
                         mini_seroFeat_all_fn_animal_NA_set1_3, 
                         output_mean = "mini_seroFeat_cm_fn_animal_mean", 
                         output_ci = "mini_seroFeat_cm_fn_animal_ci",
                         output_prop = "mini_seroFeat_cm_fn_animal_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_fn_animalPlus_NA_set1_1, mini_seroFeat_all_fn_animalPlus_NA_set1_2, 
                         mini_seroFeat_all_fn_animalPlus_NA_set1_3, 
                         output_mean = "mini_seroFeat_cm_fn_animalPlus_mean", 
                         output_ci = "mini_seroFeat_cm_fn_animalPlus_ci",
                         output_prop = "mini_seroFeat_cm_fn_animalPlus_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_ebase_animal_set1_1, 
                         mini_seroFeat_all_global_ebase_animal_set1_2, 
                         mini_seroFeat_all_global_ebase_animal_set1_3, 
                         output_mean = "mini_seroFeat_cm_global_ebase_animal_mean", 
                         output_ci = "mini_seroFeat_cm_global_ebase_animal_ci",
                         output_prop = "mini_seroFeat_cm_global_ebase_animal_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_ebase_animalPlus_set1_1, 
                         mini_seroFeat_all_global_ebase_animalPlus_set1_2, 
                         mini_seroFeat_all_global_ebase_animalPlus_set1_3, 
                         output_mean = "mini_seroFeat_cm_global_ebase_animalPlus_mean", 
                         output_ci = "mini_seroFeat_cm_global_ebase_animalPlus_ci",
                         output_prop = "mini_seroFeat_cm_global_ebase_animalPlus_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_combined_animal_set1_1, 
                         mini_seroFeat_all_global_combined_animal_set1_2, 
                         mini_seroFeat_all_global_combined_animal_set1_3, 
                         output_mean = "mini_seroFeat_cm_global_combined_animal_mean", 
                         output_ci = "mini_seroFeat_cm_global_combined_animal_ci",
                         output_prop = "mini_seroFeat_cm_global_combined_animal_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_combined_animalPlus_set1_1, 
                         mini_seroFeat_all_global_combined_animalPlus_set1_2, 
                         mini_seroFeat_all_global_combined_animalPlus_set1_3, 
                         output_mean = "mini_seroFeat_cm_global_combined_animalPlus_mean", 
                         output_ci = "mini_seroFeat_cm_global_combined_animalPlus_ci",
                         output_prop = "mini_seroFeat_cm_global_combined_animalPlus_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_ebase_animal_set2_1, 
                         mini_seroFeat_all_global_ebase_animal_set2_2, 
                         mini_seroFeat_all_global_ebase_animal_set2_3, 
                         output_mean = "mini_seroFeat_cm_global_ebase_set2_animal_mean", 
                         output_ci = "mini_seroFeat_cm_global_ebase_set2_animal_ci",
                         output_prop = "mini_seroFeat_cm_global_ebase_set2_animal_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_ebase_animalPlus_set2_1, 
                         mini_seroFeat_all_global_ebase_animalPlus_set2_2, 
                         mini_seroFeat_all_global_ebase_animalPlus_set2_3, 
                         output_mean = "mini_seroFeat_cm_global_ebase_set2_animalPlus_mean", 
                         output_ci = "mini_seroFeat_cm_global_ebase_set2_animalPlus_ci",
                         output_prop = "mini_seroFeat_cm_global_ebase_set2_animalPlus_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_combined_animal_set2_1, 
                         mini_seroFeat_all_global_combined_animal_set2_2, 
                         mini_seroFeat_all_global_combined_animal_set2_3, 
                         output_mean = "mini_seroFeat_cm_global_combined_set2_animal_mean", 
                         output_ci = "mini_seroFeat_cm_global_combined_set2_animal_ci",
                         output_prop = "mini_seroFeat_cm_global_combined_set2_animal_prop")

calculate_mean_ci_matrix(mini_seroFeat_all_global_combined_animalPlus_set2_1, 
                         mini_seroFeat_all_global_combined_animalPlus_set2_2, 
                         mini_seroFeat_all_global_combined_animalPlus_set2_3, 
                         output_mean = "mini_seroFeat_cm_global_combined_set2_animalPlus_mean", 
                         output_ci = "mini_seroFeat_cm_global_combined_set2_animalPlus_ci",
                         output_prop = "mini_seroFeat_cm_global_combined_set2_animalPlus_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_fn_animal_NA_set1_1, mini_all_seroFeat_all_fn_animal_NA_set1_2, 
                         mini_all_seroFeat_all_fn_animal_NA_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_fn_animal_mean", 
                         output_ci = "mini_all_seroFeat_cm_fn_animal_ci",
                         output_prop = "mini_all_seroFeat_cm_fn_animal_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_fn_animalPlus_NA_set1_1, mini_all_seroFeat_all_fn_animalPlus_NA_set1_2, 
                         mini_all_seroFeat_all_fn_animalPlus_NA_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_fn_animalPlus_mean", 
                         output_ci = "mini_all_seroFeat_cm_fn_animalPlus_ci",
                         output_prop = "mini_all_seroFeat_cm_fn_animalPlus_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_ebase_animal_set1_1, 
                         mini_all_seroFeat_all_global_ebase_animal_set1_2, 
                         mini_all_seroFeat_all_global_ebase_animal_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_global_ebase_animal_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_ebase_animal_ci",
                         output_prop = "mini_all_seroFeat_cm_global_ebase_animal_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_ebase_animalPlus_set1_1, 
                         mini_all_seroFeat_all_global_ebase_animalPlus_set1_2, 
                         mini_all_seroFeat_all_global_ebase_animalPlus_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_global_ebase_animalPlus_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_ebase_animalPlus_ci",
                         output_prop = "mini_all_seroFeat_cm_global_ebase_animalPlus_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_combined_animal_set1_1, 
                         mini_all_seroFeat_all_global_combined_animal_set1_2, 
                         mini_all_seroFeat_all_global_combined_animal_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_global_combined_animal_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_combined_animal_ci",
                         output_prop = "mini_all_seroFeat_cm_global_combined_animal_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_combined_animalPlus_set1_1, 
                         mini_all_seroFeat_all_global_combined_animalPlus_set1_2, 
                         mini_all_seroFeat_all_global_combined_animalPlus_set1_3, 
                         output_mean = "mini_all_seroFeat_cm_global_combined_animalPlus_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_combined_animalPlus_ci",
                         output_prop = "mini_all_seroFeat_cm_global_combined_animalPlus_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_ebase_animal_set2_1, 
                         mini_all_seroFeat_all_global_ebase_animal_set2_2, 
                         mini_all_seroFeat_all_global_ebase_animal_set2_3, 
                         output_mean = "mini_all_seroFeat_cm_global_ebase_set2_animal_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_ebase_set2_animal_ci",
                         output_prop = "mini_all_seroFeat_cm_global_ebase_set2_animal_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_ebase_animalPlus_set2_1, 
                         mini_all_seroFeat_all_global_ebase_animalPlus_set2_2, 
                         mini_all_seroFeat_all_global_ebase_animalPlus_set2_3, 
                         output_mean = "mini_all_seroFeat_cm_global_ebase_set2_animalPlus_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_ebase_set2_animalPlus_ci",
                         output_prop = "mini_all_seroFeat_cm_global_ebase_set2_animalPlus_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_combined_animal_set2_1, 
                         mini_all_seroFeat_all_global_combined_animal_set2_2, 
                         mini_all_seroFeat_all_global_combined_animal_set2_3, 
                         output_mean = "mini_all_seroFeat_cm_global_combined_set2_animal_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_combined_set2_animal_ci",
                         output_prop = "mini_all_seroFeat_cm_global_combined_set2_animal_prop")

calculate_mean_ci_matrix(mini_all_seroFeat_all_global_combined_animalPlus_set2_1, 
                         mini_all_seroFeat_all_global_combined_animalPlus_set2_2, 
                         mini_all_seroFeat_all_global_combined_animalPlus_set2_3, 
                         output_mean = "mini_all_seroFeat_cm_global_combined_set2_animalPlus_mean", 
                         output_ci = "mini_all_seroFeat_cm_global_combined_set2_animalPlus_ci",
                         output_prop = "mini_all_seroFeat_cm_global_combined_set2_animalPlus_prop")

base_names1 <- unique(gsub("_[1-3]$", "", names(miniSero_all_and_serovarFeat)))
base_names2 <- unique(gsub("_[1-3]$", "", names(miniSero_allFeat)))
base_names3 <- unique(gsub("_[1-3]$", "", names(miniSero_serovarFeat)))

# Create a list to store the results for debugging
results1 <- list()
results2 <- list()
results3 <- list()

# Loop through each base name and run the calculations
for (base_name in base_names1) {
  # Construct the command
  cmd <- sprintf(
    "calculate_mean_ci_matrix(%s_1, %s_2, %s_3, output_mean = '%s_mean', output_ci = '%s_ci', output_prop = '%s_prop')",
    base_name, base_name, base_name, base_name, base_name, base_name
  )
  
  # Print the command for debugging (optional)
  print(cmd)
  
  # Try to execute the command and capture any errors
  tryCatch({
    eval(parse(text = cmd))
    
    # Add output names to the results list
    results1[[base_name]] <- list(
      output_mean = paste0(base_name, "_mean"),
      output_ci = paste0(base_name, "_ci"),
      output_prop = paste0(base_name, "_prop")
    )
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", base_name, e$message))
  })
}

# Print all the outputs generated
for (base_name in names(results1)) {
  cat(sprintf(
    "Base Name: %s\n  Mean: %s\n  CI: %s\n  Prop: %s\n\n",
    base_name, results1[[base_name]]$output_mean, results1[[base_name]]$output_ci, results1[[base_name]]$output_prop
  ))
}

# Loop through each base name and run the calculations
for (base_name in base_names2) {
  # Construct the command
  cmd <- sprintf(
    "calculate_mean_ci_matrix(%s_1, %s_2, %s_3, output_mean = '%s_mean', output_ci = '%s_ci', output_prop = '%s_prop')",
    base_name, base_name, base_name, base_name, base_name, base_name
  )
  
  # Print the command for debugging (optional)
  print(cmd)
  
  # Try to execute the command and capture any errors
  tryCatch({
    eval(parse(text = cmd))
    
    # Add output names to the results list
    results2[[base_name]] <- list(
      output_mean = paste0(base_name, "_mean"),
      output_ci = paste0(base_name, "_ci"),
      output_prop = paste0(base_name, "_prop")
    )
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", base_name, e$message))
  })
}

# Print all the outputs generated
for (base_name in names(results2)) {
  cat(sprintf(
    "Base Name: %s\n  Mean: %s\n  CI: %s\n  Prop: %s\n\n",
    base_name, results2[[base_name]]$output_mean, results2[[base_name]]$output_ci, results2[[base_name]]$output_prop
  ))
}

# Loop through each base name and run the calculations
for (base_name in base_names3) {
  # Construct the command
  cmd <- sprintf(
    "calculate_mean_ci_matrix(%s_1, %s_2, %s_3, output_mean = '%s_mean', output_ci = '%s_ci', output_prop = '%s_prop')",
    base_name, base_name, base_name, base_name, base_name, base_name
  )
  
  # Print the command for debugging (optional)
  print(cmd)
  
  # Try to execute the command and capture any errors
  tryCatch({
    eval(parse(text = cmd))
    
    # Add output names to the results list
    results3[[base_name]] <- list(
      output_mean = paste0(base_name, "_mean"),
      output_ci = paste0(base_name, "_ci"),
      output_prop = paste0(base_name, "_prop")
    )
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", base_name, e$message))
  })
}

# Print all the outputs generated
for (base_name in names(results3)) {
  if (is.null(results3[[base_name]]$output_mean) ||
      is.null(results3[[base_name]]$output_ci) ||
      is.null(results3[[base_name]]$output_prop)) {
    # Handle cases where outputs were not generated
    cat(sprintf("Base Name: %s\n  Error: Outputs not generated due to an error.\n\n", base_name))
  } else {
    # Print generated outputs
    cat(sprintf(
      "Base Name: %s\n  Mean: %s\n  CI: %s\n  Prop: %s\n\n",
      base_name, results3[[base_name]]$output_mean, results3[[base_name]]$output_ci, results3[[base_name]]$output_prop
    ))
  }
}

gen_legend(set_breaks, set_colors, title = "Proportion (%)", direction = "horizontal")

heid_fn_heat_animal <- gen_heatmap(cm_heid_fn_animal_prop, cm_heid_fn_animal_ci, 
                                   set_breaks, set_colors, fontsize = 25)
heid_global_ebase_heat_animal <- gen_heatmap(cm_heid_global_ebase_animal_prop, cm_heid_global_ebase_animal_ci, 
                                             set_breaks, set_colors, fontsize = 25)
heid_global_combined_heat_animal <- gen_heatmap(cm_heid_global_combined_animal_prop, cm_heid_global_combined_animal_ci, 
                                                set_breaks, set_colors, fontsize = 25)
miniHeid_fn_heat_animal <- gen_heatmap(miniSero_all_and_serovarFeat_fn_animal_heid_NA_set1_prop, 
                                       miniSero_all_and_serovarFeat_fn_animal_heid_NA_set1_ci, 
                                   set_breaks, set_colors, fontsize = 25)
miniHeid_global_ebase_heat_animal <- gen_heatmap(miniSero_all_and_serovarFeat_global_ebase_heid_animal_set1_prop, 
                                                 miniSero_all_and_serovarFeat_global_ebase_heid_animal_set1_ci, 
                                             set_breaks, set_colors, fontsize = 25)
miniHeid_global_combined_heat_animal <- gen_heatmap(miniSero_all_and_serovarFeat_global_combined_heid_animal_set1_prop, 
                                                    miniSero_all_and_serovarFeat_global_combined_heid_animal_set1_ci, 
                                                    set_breaks, set_colors, fontsize = 25)
miniHeid_global_ebase_heat_animal_set2 <- gen_heatmap(miniSero_all_and_serovarFeat_global_ebase_heid_animal_set2_prop, 
                                                 miniSero_all_and_serovarFeat_global_ebase_heid_animal_set2_ci, 
                                                 set_breaks, set_colors, fontsize = 25)
miniHeid_global_combined_heat_animal_set2 <- gen_heatmap(miniSero_all_and_serovarFeat_global_combined_heid_animal_set2_prop, 
                                                    miniSero_all_and_serovarFeat_global_combined_heid_animal_set2_ci, 
                                                    set_breaks, set_colors, fontsize = 25)

heid_fn_heat_animalPlus <- gen_heatmap(cm_heid_fn_animalPlus_prop, cm_heid_fn_animalPlus_ci, 
                                   set_breaks, set_colors, fontsize = 25)
heid_global_ebase_heat_animalPlus <- gen_heatmap(cm_heid_global_ebase_animalPlus_prop, cm_heid_global_ebase_animalPlus_ci, 
                                             set_breaks, set_colors, fontsize = 25)
heid_global_combined_heat_animalPlus <- gen_heatmap(cm_heid_global_combined_animalPlus_prop, cm_heid_global_combined_animalPlus_ci, 
                                                set_breaks, set_colors, fontsize = 25)
miniHeid_fn_heat_animalPlus <- gen_heatmap(miniSero_all_and_serovarFeat_fn_animalPlus_heid_NA_set1_prop, 
                                       miniSero_all_and_serovarFeat_fn_animalPlus_heid_NA_set1_ci, 
                                       set_breaks, set_colors, fontsize = 25)
miniHeid_global_ebase_heat_animalPlus_set2 <- gen_heatmap(miniSero_all_and_serovarFeat_global_ebase_heid_animalPlus_set2_prop, 
                                                      miniSero_all_and_serovarFeat_global_ebase_heid_animalPlus_set2_ci, 
                                                      set_breaks, set_colors, fontsize = 25)
miniHeid_global_combined_heat_animalPlus_set2 <- gen_heatmap(miniSero_all_and_serovarFeat_global_combined_heid_animalPlus_set2_prop, 
                                                         miniSero_all_and_serovarFeat_global_combined_heid_animalPlus_set2_ci, 
                                                         set_breaks, set_colors, fontsize = 25)

all_fn_heat_animal <- gen_heatmap(cm_all_animal_fn_prop, cm_all_animal_fn_ci, 
                                       set_breaks, set_colors, fontsize = 25)
all_global_ebase_heat_animal <- gen_heatmap(cm_all_animal_global_ebase_prop, cm_all_animal_global_ebase_ci, 
                                                 set_breaks, set_colors, fontsize = 25)
miniall_fn_heat_animal <- gen_heatmap(mini_all_seroFeat_cm_fn_animal_prop, mini_all_seroFeat_cm_fn_animal_ci,
                                           set_breaks, set_colors, fontsize = 25)
miniall_global_ebase_heat_animal_set2 <- gen_heatmap(mini_all_seroFeat_cm_global_ebase_set2_animal_prop, 
                                                     mini_all_seroFeat_cm_global_ebase_set2_animal_ci,
                                                          set_breaks, set_colors, fontsize = 25)
miniall_global_combined_heat_animal_set2 <- gen_heatmap(mini_all_seroFeat_cm_global_combined_set2_animal_prop, 
                                                        mini_all_seroFeat_cm_global_combined_set2_animal_ci, 
                                                             set_breaks, set_colors, fontsize = 25)

all_fn_heat_animalPlus <- gen_heatmap(cm_all_animalPlus_fn_prop, cm_all_animalPlus_fn_ci, 
                                  set_breaks, set_colors, fontsize = 22)
all_global_ebase_heat_animalPlus <- gen_heatmap(cm_all_animalPlus_global_ebase_prop, cm_all_animalPlus_global_ebase_ci, 
                                            set_breaks, set_colors, fontsize = 22)
miniall_fn_heat_animalPlus <- gen_heatmap(mini_all_seroFeat_cm_fn_animalPlus_prop, mini_all_seroFeat_cm_fn_animalPlus_ci,
                                      set_breaks, set_colors, fontsize = 22)
miniall_global_ebase_heat_animalPlus_set2 <- gen_heatmap(mini_all_seroFeat_cm_global_ebase_set2_animalPlus_prop, 
                                                     mini_all_seroFeat_cm_global_ebase_set2_animalPlus_ci,
                                                     set_breaks, set_colors, fontsize = 20)
miniall_global_combined_heat_animalPlus_set2 <- gen_heatmap(mini_all_seroFeat_cm_global_combined_set2_animalPlus_prop, 
                                                        mini_all_seroFeat_cm_global_combined_set2_animalPlus_ci, 
                                                        set_breaks, set_colors, fontsize = 18)

##################################
# Heatmap
##################################
## Cleaning up the environment a bit ##
rm(list = ls(pattern = "_[1-3]$"), envir = .GlobalEnv)
rm(list = ls(pattern = "_mean$"), envir = .GlobalEnv)
summary_global <- read.csv("Summary_global2.csv")

# Get all variable names in the environment
all_vars_prop <- ls(pattern = "_prop$", envir = .GlobalEnv)
all_vars_prop_clean <- grep("fn|global_ebase|global_combined", all_vars_prop, value = T)
all_vars_prop_animal <- grep("animal_", all_vars_prop_clean, value = T)

for (matrix_name in all_vars_prop_animal) {
  # Retrieve the matrix by name
  cm_prop <- get(matrix_name, envir = .GlobalEnv)
  
  # Replace "Pig" with "Pork" in row and column names
  rownames(cm_prop) <- gsub("Pig", "Pork", rownames(cm_prop))
  colnames(cm_prop) <- gsub("Pig", "Pork", colnames(cm_prop))
  
  # Assign the modified matrix back to its original name
  assign(matrix_name, cm_prop, envir = .GlobalEnv)
}

all_vars_prop_animalPlus <- grep("animalPlus", all_vars_prop_clean, value = T)

# Get all unique host labels from "mini_seroFeat_cm_global_combined_set2_animalPlus_prop"
all_labels_animalPlus <- unique(c(rownames(mini_seroFeat_cm_global_combined_set2_animalPlus_prop),
                       colnames(mini_seroFeat_cm_global_combined_set2_animalPlus_prop)))

if (exists("annotations")) {
  rm(annotations)
}
if (exists("annotations_df")) {
  rm(annotations_df)
}

# Invert the mapping to map abbreviated names to full names
serovar_mapping_inv <- setNames(names(serovar_mapping), unlist(serovar_mapping))
# Initialize an empty data frame to store annotations
annotations_df <- data.frame(matrix_name = character(),
                             serovar = character(),
                             datatype = character(),
                             label = character(),
                             set = character(),
                             Model_size = character(),
                             stringsAsFactors = FALSE)

# Function to extract annotations from matrix names
extract_annotations <- function(matrix_name) {
  # Initialize variables
  serovar <- NA
  datatype <- NA
  label <- NA
  set <- "set1"  # Default to set1 if not found
  Model_size <- NA
  feat_type <- NA
  
  # Extract serovar
  # Search for serovar abbreviations in the name
  serovar_abbr <- names(serovar_mapping_inv)
  serovar_pattern <- paste(serovar_abbr, collapse = "|")
  serovar_match <- regexpr(serovar_pattern, matrix_name)
  if (serovar_match != -1) {
    serovar_abbr_found <- regmatches(matrix_name, serovar_match)
    serovar <- serovar_mapping_inv[[serovar_abbr_found]]
  } else {
    serovar <- "All"
  }
  
  # Extract datatype
  if (grepl("fn", matrix_name)) {
    datatype <- "FoodNet"
  } else if (grepl("global_ebase", matrix_name)) {
    datatype <- "global_Enterobase"
  } else if (grepl("global_combined", matrix_name)) {
    datatype <- "global_Combined"
  }
  
  # Extract label
  if (grepl("animalPlusPlus", matrix_name)) {
    label <- "animalPlusPlus"
  } else if (grepl("animalPlus", matrix_name)) {
    label <- "animalPlus"
  }
  
  # Extract set
  set_match <- regexpr("set[12]", matrix_name)
  if (set_match != -1) {
    set <- regmatches(matrix_name, set_match)
  } else {
    # Default to set1
    set <- "Set1"
  }
  
  # Determine mini or full
  if (grepl("mini", matrix_name)) {
    Model_size <- "Mini"
  } else {
    Model_size <- "Full"
  }
  
  # Determine feat_type
  if (grepl("miniSero_all_and_serovarFeat", matrix_name)) {
    feat_type <- "All and Serovar Shared"
  } else if (grepl("miniSero_serovarFeat", matrix_name)) {
    feat_type <- "Serovar Shared"
  } else if (grepl("miniSero_allFeat", matrix_name)) {
    feat_type <- "Shared"
  } else if (grepl("mini_seroFeat", matrix_name)) {
    feat_type <- "Serovar Shared"
  } else if (grepl("mini_all_seroFeat", matrix_name)) {
    feat_type <- "All and Serovar Shared"
  } else if (grepl("mini", matrix_name) && !grepl("seroFeat", matrix_name)) {
    feat_type <- "Shared"
  } else {
    feat_type <- "All"
  }
  
  return(data.frame(matrix_name = matrix_name,
                    serovar = serovar,
                    datatype = datatype,
                    label = label,
                    set = set,
                    Model_size = Model_size,
                    feat_type = feat_type,
                    stringsAsFactors = FALSE))
}

# Process each matrix name in all_vars_prop_animalPlus and extract annotations
for (matrix_name in all_vars_prop_animalPlus) {
  annotation <- extract_annotations(matrix_name)
  annotations_df <- rbind(annotations_df, annotation)
}

# Initialize a list to store data frames of proportions
proportion_list <- list()

for (i in 1:nrow(annotations_df)) {
  matrix_name <- annotations_df$matrix_name[i]
  cm_prop <- get(matrix_name)  # Retrieve the proportion matrix
  
  # Initialize proportions vector with zeros
  proportions <- setNames(rep(-1, length(all_labels_animalPlus)), all_labels_animalPlus)
  
  # Loop through each label in all_labels
  for (label in all_labels_animalPlus) {
    if (label %in% rownames(cm_prop) && label %in% colnames(cm_prop)) {
      # Extract the proportion from the diagonal
      proportions[label] <- cm_prop[label, label]
    }
    # Else, proportion remains 0
  }
  
  # Create a data frame for the current matrix
  proportion_df <- data.frame(matrix_name = matrix_name,
                              animalPlus_label = all_labels_animalPlus,
                              proportion = proportions,
                              stringsAsFactors = FALSE)
  proportion_list[[i]] <- proportion_df
}

# Combine all proportion data frames into one
proportions_df <- do.call(rbind, proportion_list)

# Merge the proportions_df with annotations_df on matrix_name
final_df <- merge(proportions_df, annotations_df, by = "matrix_name")

heatmap_data <- final_df %>%
  pivot_wider(id_cols = c(matrix_name, serovar, datatype, label, set, Model_size, feat_type), 
              names_from = animalPlus_label, values_from = proportion)

# Set row names to matrix_name
rownames(heatmap_data) <- heatmap_data$matrix_name

# Separate heatmap data and annotations
annotation_cols <- c("matrix_name", "serovar", "datatype", "label", "set", "Model_size", "feat_type")
heatmap_cols <- setdiff(colnames(heatmap_data), annotation_cols)

heatmap_matrix <- as.matrix(heatmap_data[, heatmap_cols])

# Create a data frame for row annotations
row_annotations_df <- heatmap_data[, annotation_cols]
row_annotations_df$label <- NULL
rownames(row_annotations_df) <- row_annotations_df$matrix_name

row_annotations_df <- row_annotations_df %>%
  mutate(set = recode(set, "set1" = "Set1"),
         set = recode(set, "set2" = "Set2"))

# Now, ensure the row names of heatmap_matrix match those of row_annotations_df
rownames(heatmap_matrix) <- rownames(row_annotations_df)

summary_global_animalPlus <- summary_global %>% 
  filter(label == "animalPlus") %>%
  rename(datatype = category) %>%
  mutate(datatype = recode(datatype, 
                           "global_Ebase" = "global_Enterobase")) %>%
  mutate(serovar = recode(serovar, 
                          "I:4,[5],12:i:-" = "i4512")) %>%
  mutate(set = recode(set, "set1" = "Set1"),
         set = recode(set, "set2" = "Set2")) %>%
  select(-label)

merged_annotations_df <- left_join(row_annotations_df, summary_global_animalPlus, 
                                   by = c("datatype", "serovar", "set"))

# Set row names of merged_annotations_df to match row_annotations_df
rownames(merged_annotations_df) <- merged_annotations_df$matrix_name

merged_annotations_df2 <- merged_annotations_df %>%
  rename(Serovar = serovar,
         Data = datatype,
         Dataset = set,
         "Model size" = Model_size,
         Feature = feat_type,
         "Sample size" = sample_size,
         "Prediction labels" = num_pred) %>%
  mutate(Data = recode(Data,
                       "global_Enterobase" = "Global Enterobase",
                       "global_Combined" = "Global Combined")) %>%
  mutate(Serovar = recode(Serovar,
                       "i4512" = "I:4,[5],12:i:-")) %>%
  mutate(Dataset = recode(Dataset,
                          "set2" = "Set2"))

# Create color mappings for annotations
# Ensure that the number of colors matches the number of categories
serovar_levels <- unique(merged_annotations_df2$Serovar)
print(serovar_levels)
serovar_colors <- setNames(colorRampPalette(brewer.pal(9, "Set1"))(length(serovar_levels)), serovar_levels)
print(serovar_colors)

datatype_levels <- unique(merged_annotations_df2$Data)
print(datatype_levels)
datatype_colors <- setNames(colorRampPalette(brewer.pal(8, "Set2"))(length(datatype_levels)), datatype_levels)
print(datatype_colors)

set_levels <- unique(merged_annotations_df2$Dataset)
print(set_levels)
dataset_colors <- c("Set1" = "#1B9E77", "Set2" = "#D95F02")
print(set_colors)

mod_size_colors <- c("Mini" = "lightblue", "Full" = "pink")
feat_type_colors <- c("Serovar Shared" = "cyan", "Shared" = "purple", "All" = "yellow", "All and Serovar Shared" = "#FF4500")

# Create the bar plots as right annotations
ha_barplot <- rowAnnotation(
  `Sample size` = anno_barplot(merged_annotations_df2$`Sample size`, 
                             border = T, 
                             gp = gpar(fill = "grey")),
  `Number of \npredicted \nlabels` = anno_barplot(merged_annotations_df2$`Prediction labels`, 
                          border = T, 
                          gp = gpar(fill = "blue")),
  annotation_name_side = "top",
  width = unit(3, "cm")  # Adjust width as needed
)

row_annotations_df <- merged_annotations_df %>%
  select(-sample_size, -num_pred, -matrix_name) %>%
  rename(Serovar = serovar,
         Data = datatype,
         Dataset = set,
         "Model size" = Model_size,
         Feature = feat_type) %>%
  mutate(Data = recode(Data,
                       "global_Enterobase" = "Global Enterobase",
                       "global_Combined" = "Global Combined")) %>%
  mutate(Serovar = recode(Serovar,
                          "i4512" = "I:4,[5],12:i:-")) %>%
  mutate(Dataset = recode(Dataset,
                          "set2" = "Set2"))

# Create a HeatmapAnnotation object for the rows
ha_row <- rowAnnotation(df = row_annotations_df,
                        col = list(
                          Serovar = serovar_colors,
                          Data = datatype_colors,
                          Dataset = set_colors,
                          `Model size` = mod_size_colors,
                          Feature = feat_type_colors
                        ),
                        show_annotation_name = TRUE,
                        show_legend = FALSE,  # Suppress legends in annotations
                        annotation_width = unit(4, "cm"))

set_colors2 <- colorRamp2(
  c(-1, 0, 59.9, 60, 100),
  c("black", "pink", "pink", "#ccffcc", "#006600")
)

# Define the breaks for the legend
legend_breaks <- c(0, 60, 80, 100)

# Define labels for the legend
legend_labels <- c("0.0", "60", "80", "100")

# Create the heatmap with right annotations
ht <- Heatmap(heatmap_matrix,
              name = "Proportion",
              left_annotation = ha_row,
              right_annotation = ha_barplot,
              col = set_colors2,
              cluster_rows = T,
              cluster_columns = T,
              show_row_names = FALSE,
              show_column_names = TRUE,
              show_heatmap_legend = FALSE,  # Suppress the heatmap legend
              column_names_side = "bottom")

# Draw the heatmap, suppressing annotation legends as well
draw(ht, show_annotation_legend = FALSE)

set_colors_legend <- colorRamp2(
  c(0, 59.9, 60, 100),
  c("pink", "pink", "#ccffcc", "#006600")
)

# Heatmap legend
heatmap_legend <- Legend(
  col_fun = set_colors_legend,
  title = "Proportion (%)",
  at = legend_breaks,
  labels = legend_labels,
  direction = "horizontal"
)

draw(heatmap_legend)
# Legends for annotations
serovar_legend <- Legend(
  labels = names(serovar_colors),
  legend_gp = gpar(fill = serovar_colors),
  title = "Serovar",
  ncol = 2,
  direction = "horizontal"
)

draw(serovar_legend)

datatype_legend <- Legend(
  labels = names(datatype_colors),
  legend_gp = gpar(fill = datatype_colors),
  title = "Data",
  ncol = 1
)

draw(datatype_legend)

set_legend <- Legend(
  labels = names(dataset_colors),
  legend_gp = gpar(fill = dataset_colors),
  title = "Dataset",
  ncol = 1
)

draw(set_legend)

mini_full_legend <- Legend(
  labels = names(mod_size_colors),
  legend_gp = gpar(fill = mod_size_colors),
  title = "Model size",
  ncol = 1
)

draw(mini_full_legend)

feat_type_legend <- Legend(
  labels = names(feat_type_colors),
  legend_gp = gpar(fill = feat_type_colors),
  title = "Feature Set",
  ncol = 1
)

draw(feat_type_legend)

########### Heatmap with false positive/negatives ############
