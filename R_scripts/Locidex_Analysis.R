library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(dunn.test)
library(stringr)
library(rlang)
library(ggpattern)
library(readr)
library(scales)
library(forcats)
library(ggalt)
library(ggrepel)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(cowplot)

setwd("C:/Users/user5012100/Desktop/Data/locidex_out/")
source("C:/Users/user5012100/Desktop/R/Functions_20240507.R")

############ LOCIDEX ANALYSIS ###################
## Profiles ##
fn_profile <- read_tsv("fn_locidex_skesa.tsv")
ebase_host_profile <- read_tsv("ebase_host_locidex_LE_20240820.tsv")
ebase_prov_profile <- read_tsv("ebase_prov_locidex_LE_20240820.tsv")
combined_host_profile <- read_tsv("combined_host_LE_20240820.tsv")
combined_prov_profile <- read_tsv("combined_prov_LE_20240820.tsv")

combined_prov_profile <- combined_prov_profile %>%
  mutate(Province = case_when(
    Province %in% c("MB", "SK") ~ "MB / SK",
    TRUE ~ Province
  ))
combined_host_profile <- combined_host_profile %>%
  mutate(CombinedSites = case_when(
  CombinedSites == "Pork" ~ "Pig",
  TRUE ~ CombinedSites
)) 

ebase_prov_profile <- ebase_prov_profile %>%
  mutate(Province = case_when(
    Province %in% c("MB", "SK") ~ "MB / SK",
    TRUE ~ Province
  ))

## ML Results##
# Results using the base allelic profiles -- cleaned/modified using Best_Models_EDA_20240614.R
fn_flat_results <- read.csv("best_flat_fn_clean.csv")
fn_hml_results <- read.csv("best_hml_fn_clean.csv")
ebase_flat_results <- read.csv("best_flat_pub_clean.csv")
ebase_hml_results <- read.csv("best_hml_pub_clean.csv")

fn_flat_results <- fn_flat_results %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal_type" ~ "Animal type",
    Pred_label == "Location_or_Food" ~ "Location or Food",
    TRUE ~ Pred_label
  )) 

ebase_flat_results <- ebase_flat_results %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal_type" ~ "Animal type",
    Pred_label == "Location_or_Food" ~ "Location or Food",
    TRUE ~ Pred_label
  )) 

# Results using locidex
fn_flat_ldx_results <- read.csv("foodnet_locidex.csv")
fn_hml_ldx_results <- read.csv("foodnet_locidex_hml.csv")
ebase_flat_ldx_results <- read.csv("enterobase_locidex.csv")
ebase_hml_ldx_results <- read.csv("enterobase_locidex_hml.csv")
combined_flat_ldx_results <- read.csv("combined_locidex.csv")
combined_hml_ldx_results <- read.csv("combined_locidex_hml.csv")
  
fn_flat_ldx_results <- fn_flat_ldx_results %>%
  mutate(dataset = "FoodNet")
ebase_flat_ldx_results <- ebase_flat_ldx_results %>%
  mutate(dataset = "Enterobase")
combined_flat_ldx_results <- combined_flat_ldx_results %>%
  mutate(dataset = "Combined")
flat_ldx_results <- rbind(fn_flat_ldx_results, ebase_flat_ldx_results, combined_flat_ldx_results)

fn_hml_ldx_results <- fn_hml_ldx_results %>%
  mutate(dataset = "FoodNet")
ebase_hml_ldx_results <- ebase_hml_ldx_results %>%
  mutate(dataset = "Enterobase")
combined_hml_ldx_results <- combined_hml_ldx_results %>%
  mutate(dataset = "Combined")
hml_ldx_results <- rbind(fn_hml_ldx_results, ebase_hml_ldx_results, combined_hml_ldx_results)

mean_flat_ldx_results <- flat_ldx_results %>%
  group_by(dataset, Serovar, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

model_ldx_results_flat <- flat_ldx_results %>%
  left_join(mean_flat_ldx_results, by = c("dataset", "Serovar", "Pred_label"))

mean_hml_ldx_results <- hml_ldx_results %>%
  group_by(dataset, Serovar, Pred_label) %>%
  summarise(mean_F1 = mean(F1), .groups = 'drop')

model_ldx_results_hml <- hml_ldx_results %>%
  left_join(mean_hml_ldx_results, by = c("dataset", "Serovar", "Pred_label"))

combined_host_n_sero <- n_distinct(combined_host_profile$Serovar) # Total of 83 unique serovars
combined_prov_n_sero <- n_distinct(combined_prov_profile$Serovar) # Total of 111 unique serovars
enterobase_prov_n_sero <- 83
enterobase_host_n_sero <- 45
fn_n_sero <- 75 

##### Calculating confidence intervals for plotting ###########
fn_flat_ci <- fn_flat_results %>%
  group_by(Pred_label, Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
fn_flat_results_final <- fn_flat_results %>%
  left_join(fn_flat_ci, by = c("Pred_label", "Serovar"))

ebase_flat_ci <- ebase_flat_results %>%
  filter(genome == "wgMLST") %>%
  group_by(Pred_label, Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
ebase_flat_results_final <- ebase_flat_results %>%
  left_join(ebase_flat_ci, by = c("Pred_label", "Serovar"))

fn_hml_ci <- fn_hml_results %>%
  group_by(Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
fn_hml_results_final <- fn_hml_results %>%
  left_join(fn_hml_ci, by = c("Serovar"))

ebase_hml_ci <- ebase_hml_results %>%
  filter(genome == "wgMLST") %>%
  group_by(Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
ebase_hml_results_final <- ebase_hml_results %>%
  left_join(ebase_hml_ci, by = c("Serovar"))

ldx_flat_ci <- model_ldx_results_flat %>%
  group_by(dataset, Pred_label, Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
ldx_flat_results_final <- model_ldx_results_flat %>%
  left_join(ldx_flat_ci, by = c("dataset", "Pred_label", "Serovar"))

ldx_hml_ci <- model_ldx_results_hml %>%
  group_by(dataset, Serovar) %>%
  summarise(ci = qt(0.975, df = n() - 1) * sd(F1) / sqrt(n()),
            mean_f1_ci = paste0(round(mean(F1), 2), " ± ", round(ci, 2)))
ldx_hml_results_final <- model_ldx_results_hml %>%
  left_join(ldx_hml_ci, by = c("dataset", "Serovar"))

#### plots to show difference in composition between combined vs fn ####
serovar_list <- flat_ldx_results %>%
  filter(Serovar != "All") %>%
  distinct(Serovar) %>%
  pull()

# Example for df1 with 'Province' column
fn_prov_proportions <- get_proportions(fn_profile, "SourceState", serovar_list, "Serovar") %>%
  mutate(dataset = "FoodNet") %>%
  rename(Province = SourceState)
fn_animal_proportions <- get_proportions(fn_profile, "CombinedSites", serovar_list, "Serovar") %>%
  mutate(dataset = "FoodNet")
fn_animalPlus_proportions <- get_proportions(fn_profile, "CombinedSite2", serovar_list, "Serovar") %>%
  mutate(dataset = "FoodNet")
comb_prov_proportions <- get_proportions(combined_prov_profile, "Province", serovar_list, "Serovar") %>%
  mutate(dataset = "Combined")
comb_animal_proportions <- get_proportions(combined_host_profile, "CombinedSites", serovar_list, "Serovar") %>%
  mutate(dataset = "Combined")
comb_animalPlus_proportions <- get_proportions(combined_host_profile, "CombinedSite2", serovar_list, "Serovar") %>%
  mutate(dataset = "Combined")

prov_proportions_seroFiltered <- rbind(fn_prov_proportions, comb_prov_proportions)
animal_proportions_seroFiltered <- rbind(fn_animal_proportions, comb_animal_proportions)
animalPlus_proportions_seroFiltered <- rbind(fn_animalPlus_proportions, comb_animalPlus_proportions)

fn_proportions_all <- fn_profile %>%
  group_by(SourceState) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  rename(Province = SourceState) %>%
  mutate(dataset = "FoodNet")

combined_proportions_all <- combined_prov_profile %>%
  group_by(Province) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(dataset = "Combined")

prov_all_proportions <- bind_rows(fn_proportions_all, combined_proportions_all)

combined_diff <- prov_all_proportions %>%
  # Step 1: Separate FoodNet and Combined data
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to have proportions from both datasets side by side
  pivot_wider(names_from = dataset, values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both 'count' and 'proportion'
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select the Province and the calculated differences, and create a new row for 'Difference'
  select(Province, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(Province, count, proportion, dataset)  # Clean up columns

# Step 5: Bind the result back with the original dataframe
prov_all_proportions <- bind_rows(prov_all_proportions, combined_diff)

prov_all_proportions$dataset <- factor(prov_all_proportions$dataset,
                                  levels = c("Difference", "Combined", "FoodNet"))

dataset_palette <- c("FoodNet" = "#0073C2FF", "Combined" = "#EFC000FF", "Difference" = "darkgreen")

ordered_provinces <- prov_all_proportions %>%
  filter(dataset == "Combined") %>%
  arrange(desc(proportion)) %>%
  pull(Province)  # Extracts ordered province list

# Step 2: Set the 'Province' factor levels based on this order
prov_all_proportions$Province <- factor(prov_all_proportions$Province, levels = rev(ordered_provinces))

plot_all_prov_proportion <- ggplot(prov_all_proportions, aes(x = Province, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = Province, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "Province") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.3),
                     breaks = seq(-0.1, 0.3, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_all_prov_count <- ggplot(prov_all_proportions, aes(x = Province, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = Province, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "Province") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1700),
                     breaks = seq(0, 1700, by = 100)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

fn_proportions_all_animal <- fn_profile %>%
  group_by(CombinedSites) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(dataset = "FoodNet")

combined_proportions_all_animal <- combined_host_profile %>%
  group_by(CombinedSites) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(dataset = "Combined") 

animal_all_proportions <- bind_rows(fn_proportions_all_animal, combined_proportions_all_animal) 

combined_diff_animal <- animal_all_proportions %>%
  # Step 1: Separate FoodNet and Combined data
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to have proportions from both datasets side by side
  pivot_wider(names_from = dataset, values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both 'count' and 'proportion'
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select the Province and the calculated differences, and create a new row for 'Difference'
  select(CombinedSites, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(CombinedSites, count, proportion, dataset)  # Clean up columns

# Step 5: Bind the result back with the original dataframe
animal_all_proportions <- bind_rows(animal_all_proportions, combined_diff_animal)

animal_all_proportions$dataset <- factor(animal_all_proportions$dataset,
                                    levels = c("Difference", "Combined", "FoodNet"))

ordered_animal <- animal_all_proportions %>%
  filter(dataset == "Combined") %>%
  arrange(desc(proportion)) %>%
  pull(CombinedSites)  # Extracts ordered province list

animal_all_proportions$CombinedSites <- factor(animal_all_proportions$CombinedSites, levels = rev(ordered_animal))

plot_all_animal_proportion <- ggplot(animal_all_proportions, aes(x = CombinedSites, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "Animal") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.6),
                     breaks = seq(-0.1, 0.6, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_all_animal_count <- ggplot(animal_all_proportions, aes(x = CombinedSites, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "Animal") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 3000),
                     breaks = seq(0, 3000, by = 500)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

fn_proportions_all_animalPlus <- fn_profile %>%
  group_by(CombinedSite2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(dataset = "FoodNet")

combined_proportions_all_animalPlus <- combined_host_profile %>%
  group_by(CombinedSite2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(dataset = "Combined") 

animalPlus_all_proportions <- bind_rows(fn_proportions_all_animalPlus, combined_proportions_all_animalPlus) 

combined_diff_animalPlus <- animalPlus_all_proportions %>%
  # Step 1: Separate FoodNet and Combined data
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to have proportions from both datasets side by side
  pivot_wider(names_from = dataset, values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both 'count' and 'proportion'
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select the Province and the calculated differences, and create a new row for 'Difference'
  select(CombinedSite2, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(CombinedSite2, count, proportion, dataset)  # Clean up columns

# Step 5: Bind the result back with the original dataframe
animalPlus_all_proportions <- bind_rows(animalPlus_all_proportions, combined_diff_animalPlus)

animalPlus_all_proportions$dataset <- factor(animalPlus_all_proportions$dataset,
                                         levels = c("Difference", "Combined", "FoodNet"))

ordered_animalPlus <- animalPlus_all_proportions %>%
  filter(dataset == "Combined") %>%
  arrange(desc(proportion)) %>%
  pull(CombinedSite2)  # Extracts ordered province list

animalPlus_all_proportions$CombinedSite2 <- factor(animalPlus_all_proportions$CombinedSite2, levels = rev(ordered_animalPlus))

plot_all_animalPlus_proportions <- ggplot(animalPlus_all_proportions, aes(x = CombinedSite2, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "AnimalPlus") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.4),
                     breaks = seq(-0.1, 0.4, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_all_animalPlus_count <- ggplot(animalPlus_all_proportions, aes(x = CombinedSite2, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "AnimalPlus") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1800),
                     breaks = seq(0, 1800, by = 200)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

ggarrange(plot_all_prov_proportion, plot_all_prov_count,
          plot_all_animal_proportion, plot_all_animal_count,
          plot_all_animalPlus_proportions, plot_all_animalPlus_count,
          ncol = 2, nrow = 3,
          align = "hv", common.legend = T, legend = "bottom")

diff_prov_seroFiltered <- prov_proportions_seroFiltered %>%
  # Step 1: Filter data for 'Combined' and 'FoodNet'
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to wider format for both count and proportion, by Province and Serovar
  pivot_wider(names_from = dataset, 
              values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both count and proportion
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select Province, Serovar, and the calculated differences
  select(Province, Serovar, count_Difference, proportion_Difference) %>%
  
  # Step 5: Create a new row for 'Difference' dataset and clean up the columns
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(Province, Serovar, count, proportion, dataset)  # Clean up columns

prov_proportions_seroFiltered <- bind_rows(prov_proportions_seroFiltered, diff_prov_seroFiltered)

prov_proportions_seroFiltered$dataset <- factor(prov_proportions_seroFiltered$dataset,
                                         levels = c("Difference", "Combined", "FoodNet"))

prov_proportions_seroFiltered$Province <- factor(prov_proportions_seroFiltered$Province, levels = rev(ordered_provinces))

diff_animal_seroFiltered <- animal_proportions_seroFiltered  %>%
  # Step 1: Filter data for 'Combined' and 'FoodNet'
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to wider format for both count and proportion, by Province and Serovar
  pivot_wider(names_from = dataset, 
              values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both count and proportion
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select Province, Serovar, and the calculated differences
  select(CombinedSites, Serovar, count_Difference, proportion_Difference) %>%
  
  # Step 5: Create a new row for 'Difference' dataset and clean up the columns
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(CombinedSites, Serovar, count, proportion, dataset)  # Clean up columns

animal_proportions_seroFiltered <- bind_rows(animal_proportions_seroFiltered, diff_animal_seroFiltered)

animal_proportions_seroFiltered$dataset <- factor(animal_proportions_seroFiltered$dataset,
                                                levels = c("Difference", "Combined", "FoodNet"))

animal_proportions_seroFiltered$CombinedSites <- factor(animal_proportions_seroFiltered$CombinedSites, levels = rev(ordered_animal))

diff_animalPlus_seroFiltered <- animalPlus_proportions_seroFiltered %>%
  # Step 1: Filter data for 'Combined' and 'FoodNet'
  filter(dataset %in% c("Combined", "FoodNet")) %>%
  
  # Step 2: Pivot the data to wider format for both count and proportion, by Province and Serovar
  pivot_wider(names_from = dataset, 
              values_from = c(count, proportion), 
              values_fill = list(count = 0, proportion = 0)) %>%
  
  # Step 3: Calculate the differences for both count and proportion
  mutate(
    count_Difference = count_Combined - count_FoodNet,
    proportion_Difference = proportion_Combined - proportion_FoodNet
  ) %>%
  
  # Step 4: Select Province, Serovar, and the calculated differences
  select(CombinedSite2, Serovar, count_Difference, proportion_Difference) %>%
  
  # Step 5: Create a new row for 'Difference' dataset and clean up the columns
  mutate(
    dataset = "Difference", 
    count = count_Difference, 
    proportion = proportion_Difference
  ) %>%
  select(CombinedSite2, Serovar, count, proportion, dataset)  # Clean up columns

animalPlus_proportions_seroFiltered <- bind_rows(animalPlus_proportions_seroFiltered, diff_animalPlus_seroFiltered)

animalPlus_proportions_seroFiltered$dataset <- factor(animalPlus_proportions_seroFiltered$dataset,
                                                  levels = c("Difference", "Combined", "FoodNet"))

animalPlus_proportions_seroFiltered$CombinedSite2 <- factor(animalPlus_proportions_seroFiltered$CombinedSite2, 
                                                            levels = rev(ordered_animalPlus))

plot_serovar_proportion_province <- ggplot(prov_proportions_seroFiltered, 
                                       aes(x = Province, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = Province, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "Province") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.7),
                     breaks = seq(-0.1, 0.7, by = 0.1),
                     labels = label_number(accuracy = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

plot_serovar_count_province <- ggplot(prov_proportions_seroFiltered, 
       aes(x = Province, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = Province, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "Province") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 540),
                     breaks = seq(0, 540, by = 100),
                     labels = label_number(accuracy = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

ggarrange(plot_serovar_proportion_province, plot_serovar_count_province, 
          align = "hv", nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO")

plot_serovar_proportion_animal <- ggplot(animal_proportions_seroFiltered, 
                                           aes(x = CombinedSites, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "Animal") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.2, 1),
                     breaks = seq(-0.2, 1, by = 0.1),
                     labels = label_number(accuracy = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

plot_serovar_count_animal <- ggplot(animal_proportions_seroFiltered, 
                                      aes(x = CombinedSites, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "Animal") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1100),
                     breaks = seq(0, 1100, by = 100),
                     labels = label_number(accuracy = 1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

ggarrange(plot_serovar_proportion_animal, plot_serovar_count_animal, 
          align = "hv", nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO")

plot_serovar_proportion_animalPlus <- ggplot(animalPlus_proportions_seroFiltered, 
                                           aes(x = CombinedSite2, y = proportion, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion", x = "AnimalPlus") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, 0.9),
                     breaks = seq(-0.1, 0.9, by = 0.1),
                     labels = label_number(accuracy = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

plot_serovar_count_animalPlus <- ggplot(animalPlus_proportions_seroFiltered, 
                                      aes(x = CombinedSite2, y = count, color = dataset)) +
  # Use geom_linerange instead of geom_segment to properly dodge both ends of the line
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "AnimalPlus") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 800),
                     breaks = seq(0, 800, by = 200),
                     labels = label_number(accuracy = 1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) + 
  facet_wrap(~ Serovar)  # Facet by Serovar

ggarrange(plot_serovar_proportion_animalPlus, plot_serovar_count_animalPlus, 
          align = "hv", nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO")

##################### Comparison of ML results ########################
flat_results1 <- fn_flat_results_final %>%
  select(Pred_label, Serovar, F1, mean_F1, mean_f1_ci) %>%
  mutate(profile = "Base") %>%
  mutate(dataset = "FoodNet")
flat_results2 <- ebase_flat_results_final %>%
  filter(genome == "wgMLST") %>%
  select(Pred_label, Serovar, F1, mean_F1, mean_f1_ci) %>%
  mutate(profile = "Base") %>% ## NO SEED FROM EXPORTING
  mutate(dataset = "Enterobase")
flat_results_base_profile <- rbind(flat_results1, flat_results2) %>%
  mutate(model = "Flat")

hml_results1 <- fn_hml_results_final %>%
  select(Serovar, F1, mean_F1, mean_f1_ci) %>%
  mutate(profile = "Base") %>%
  mutate(dataset = "FoodNet")
hml_results2 <- ebase_hml_results_final %>%
  filter(genome == "wgMLST") %>%
  select(Serovar, F1, mean_F1, mean_f1_ci) %>%
  mutate(profile = "Base") %>%
  mutate(dataset = "Enterobase")
hml_results_base_profile <- rbind(hml_results1, hml_results2) %>%
  mutate(model = "Hierarchical")

flat_results_locidex <- ldx_flat_results_final %>%
  select(Pred_label, Serovar, F1, mean_F1, dataset, mean_f1_ci) %>%
  mutate(profile = "Locidex") %>%
  mutate(model = "Flat")
hml_results_locidex <- ldx_hml_results_final %>%
  select(Serovar, F1, mean_F1, dataset, mean_f1_ci) %>%
  mutate(profile = "Locidex") %>%
  mutate(model = "Hierarchical")

flat_results_all <- rbind(flat_results_base_profile, flat_results_locidex)
hml_results_all <- rbind(hml_results_base_profile, hml_results_locidex)

flat_results_all <- flat_results_all %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal_type" ~ "Animal type",
    Pred_label == "Location_or_Food" ~ "Location or Food",
    TRUE ~ Pred_label
  )) 
####### Dumbbell plots #########
serovar_list_plot <- c("Enteritidis", "Heidelberg", "I:4,[5],12:i:-", "Infantis", 
                       "Kentucky", "Typhimurium", "All")
nudge_value <- 0.02  # Adjust nudge value for text

fn_compare <- flat_results_all %>%
  filter(dataset == "FoodNet" & profile %in% c("Base", "Locidex")) %>%
  distinct(Pred_label, Serovar, profile, mean_F1, mean_f1_ci)

df <- fn_compare %>%
  filter(profile %in% c("Base", "Locidex")) %>%  # Only work with Base and Locidex
  pivot_wider(names_from = profile, values_from = c(mean_F1, mean_f1_ci)) %>%  # Pivot to compute gap
  mutate(gap = mean_F1_Locidex - mean_F1_Base) %>%  # Calculate the gap
  group_by(Pred_label, Serovar) %>% 
  mutate(max = max(mean_F1_Base, mean_F1_Locidex)) %>%  # Find the maximum F1 value between Base and Locidex
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))  # Sort Serovars by absolute gap

df_long_F1 <- df %>%
  select(-mean_f1_ci_Base, -mean_f1_ci_Locidex) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "profile", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1")

df_long_ci <- df %>%
  select(-mean_F1_Base, -mean_F1_Locidex) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci")

df_long_F1$Serovar <- factor(df_long_F1$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

p_main <- df_long_F1 %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base and Locidex values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Points for Base and Locidex
  geom_point(aes(color = profile), size = 3) +
  
  # Text labels for values
  geom_text_repel(aes(label = df_long_ci$mean_f1_ci, color = profile),
                  size = 3.25,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  
  # Custom x-axis scale (from 0 to 1 with 0.2 increments)
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.2), limits = c(0.4, 1.5)) +
  
  # Custom colors for Base and Locidex
  scale_color_manual(values = c("Base" = "#436685", "Locidex" = "#BF2F24")) +
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y")

df_gap <- df %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Locidex"), paste0("+", round(abs(gap), 2), " Base"))
  )
df_gap$Serovar <- factor(df_gap$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

# Basic plot for gap text with no background, faceted by Pred_label
# Basic plot for gap text with a light background, faceted by Pred_label
p_gap_text <- ggplot(df_gap, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Locidex", "Base"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Base" = "#436685", "Locidex" = "#BF2F24")) +
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y") +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

##########################################################
serovar_list_plot_eb <- c("Enteritidis", "Heidelberg", "Typhimurium", "All")

eb_compare <- flat_results_all %>%
  filter(dataset == "Enterobase" & profile %in% c("Base", "Locidex")) %>%
  distinct(Pred_label, Serovar, profile, mean_F1, mean_f1_ci)

df_eb <- eb_compare %>%
  filter(profile %in% c("Base", "Locidex")) %>%  # Only work with Base and Locidex
  pivot_wider(names_from = profile, values_from = c(mean_F1, mean_f1_ci)) %>%  # Pivot to compute gap
  mutate(gap = mean_F1_Locidex - mean_F1_Base) %>%  # Calculate the gap
  group_by(Pred_label, Serovar) %>% 
  mutate(max = max(mean_F1_Locidex - mean_F1_Base)) %>%  # Find the maximum F1 value between Base and Locidex
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))  # Sort Serovars by absolute gap

df_long_F1_eb <- df_eb %>%
  select(-mean_f1_ci_Base, -mean_f1_ci_Locidex) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "profile", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1")

df_long_ci_eb <- df_eb %>%
  select(-mean_F1_Base, -mean_F1_Locidex) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci")

df_long_F1_eb$Serovar <- factor(df_long_F1_eb$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

p_main_eb <- df_long_F1_eb %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base and Locidex values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Points for Base and Locidex
  geom_point(aes(color = profile), size = 3) +
  
  # Text labels for values
  geom_text_repel(aes(label = df_long_ci_eb$mean_f1_ci, color = profile),
                  size = 3.25,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  
  # Custom x-axis scale (from 0 to 1 with 0.2 increments)
  scale_x_continuous(breaks = seq(0.3, 1, by = 0.1), limits = c(0.3, 1.4)) +
  
  # Custom colors for Base and Locidex
  scale_color_manual(values = c("Base" = "#436685", "Locidex" = "#BF2F24")) +
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y")

df_gap_eb <- df_eb %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Locidex"), paste0("+", round(abs(gap), 2), " Base"))
  )
df_gap_eb$Serovar <- factor(df_gap_eb$Serovar, levels = rev(serovar_list_plot_eb))  # Set Serovar order

# Basic plot for gap text with no background, faceted by Pred_label
# Basic plot for gap text with a light background, faceted by Pred_label
p_gap_text_eb <- ggplot(df_gap_eb, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Locidex", "Base"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Base" = "#436685", "Locidex" = "#BF2F24")) +
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y") +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

###############################################
flat_all_compare <- flat_results_all %>%
  filter(profile == "Locidex") %>%
  distinct(Pred_label, Serovar, profile, mean_F1, dataset, mean_f1_ci)

df_flat_compare <- flat_all_compare %>%
  pivot_wider(names_from = dataset, values_from = c(mean_F1, mean_f1_ci)) %>%
  mutate(gap = mean_F1_Combined - mean_F1_FoodNet) %>%
  group_by(Pred_label, Serovar) %>%
  mutate(max = max(mean_F1_FoodNet, mean_F1_Enterobase, mean_F1_Combined, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))

df_long_F1_flat <- df_flat_compare %>%
  select(-mean_f1_ci_FoodNet, -mean_f1_ci_Combined, -mean_f1_ci_Enterobase) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "dataset", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1") %>%
  mutate(Pred_label = recode(Pred_label, 
                             "Location or Food" = "AnimalPlus (Location or Food)"))

df_long_ci_flat <- df_flat_compare %>%
  select(-mean_F1_FoodNet, -mean_F1_Combined, -mean_F1_Enterobase) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci") %>%
  mutate(Pred_label = recode(Pred_label, 
                             "Location or Food" = "AnimalPlus (Location or Food)"))

df_long_F1_flat$Serovar <- factor(df_long_F1_flat$Serovar, levels = rev(serovar_list_plot))

p_main_flat <- df_long_F1_flat %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base, Locidex, and Other values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +  # Grey line
  
  # Points for Base, Locidex, and Other
  geom_point(aes(color = dataset), size = 3) +

  # Text labels for values
  geom_text_repel(aes(label = df_long_ci_flat$mean_f1_ci, color = dataset),
                  size = 3.25,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar", color = "Dataset") +
  
  # Custom x-axis scale (from 0.3 to 1.4 with increments)
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1), limits = c(0.3, 1.4)) +
  
  # Custom colors for Base, Locidex, and Other
  scale_color_manual(values = c("FoodNet" = "#436685", "Enterobase" = "#BF2F24", "Combined" = "#32CD32")) +  # Green for Other
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y")

df_gap_flat <- df_long_F1_flat %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Combined"), paste0("+", round(abs(gap), 2), " FoodNet"))
  )
df_gap_flat$Serovar <- factor(df_gap_flat$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

p_gap_text_flat <- ggplot(df_gap_flat, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Combined", "FoodNet"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Combined" = "#32CD32", "FoodNet" = "#436685")) +
  
  # Facet by Pred_label
  facet_wrap(~ Pred_label, scales = "free_y") +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

############# Hierarchical plots ##############
hml_compare_fn <- hml_results_all %>%
  filter(dataset == "FoodNet" & profile %in% c("Base", "Locidex")) %>%
  distinct(Serovar, profile, mean_F1, mean_f1_ci)

df_hml_fn <- hml_compare_fn %>%
  filter(profile %in% c("Base", "Locidex")) %>%  # Only work with Base and Locidex
  pivot_wider(names_from = profile, values_from = c(mean_F1, mean_f1_ci)) %>%  # Pivot to compute gap
  mutate(gap = mean_F1_Locidex - mean_F1_Base) %>%  # Calculate the gap
  group_by(Serovar) %>% 
  mutate(max = max(mean_F1_Base , mean_F1_Locidex)) %>%  # Find the maximum F1 value between Base and Locidex
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))  # Sort Serovars by absolute gap

df_long_F1_hml_fn <- df_hml_fn %>%
  select(-mean_f1_ci_Base, -mean_f1_ci_Locidex) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "profile", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1")

df_long_ci_hml_fn <- df_hml_fn %>%
  select(-mean_F1_Base, -mean_F1_Locidex) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci")

df_long_F1_hml_fn$Serovar <- factor(df_long_F1_hml_fn$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

p_main_hml_fn <- df_long_F1_hml_fn %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base and Locidex values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Points for Base and Locidex
  geom_point(aes(color = profile), size = 3) +
  
  geom_text_repel(aes(label = df_long_ci_hml_fn$mean_f1_ci, color = profile),
                  size = 3.25,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  
  # Custom x-axis scale (from 0 to 1 with 0.2 increments)
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.2), limits = c(0.4, 1.5)) +
  
  # Custom colors for Base and Locidex
  scale_color_manual(values = c("Base" = "#353aa1", "Locidex" = "#a32254")) 

df_gap_hml_fn <- df_hml_fn %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Locidex"), paste0("+", round(abs(gap), 2), " Base"))
  )
df_gap_hml_fn$Serovar <- factor(df_gap_hml_fn$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

# Basic plot for gap text with no background, faceted by Pred_label
# Basic plot for gap text with a light background, faceted by Pred_label
p_gap_text_hml_fn <- ggplot(df_gap_hml_fn, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Locidex", "Base"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Base" = "#353aa1", "Locidex" = "#a32254")) +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

###################
hml_compare_eb <- hml_results_all %>%
  filter(dataset == "Enterobase" & profile %in% c("Base", "Locidex")) %>%
  distinct(Serovar, profile, mean_F1, mean_f1_ci)

df_hml_eb <- hml_compare_eb %>%
  filter(profile %in% c("Base", "Locidex")) %>%  # Only work with Base and Locidex
  pivot_wider(names_from = profile, values_from = c(mean_F1, mean_f1_ci)) %>%  # Pivot to compute gap
  mutate(gap = mean_F1_Locidex  - mean_F1_Base) %>%  # Calculate the gap
  group_by(Serovar) %>% 
  mutate(max = max(mean_F1_Base, mean_F1_Locidex )) %>%  # Find the maximum F1 value between Base and Locidex
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))  # Sort Serovars by absolute gap

df_long_F1_hml_eb <- df_hml_eb %>%
  select(-mean_f1_ci_Base, -mean_f1_ci_Locidex) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "profile", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1")

df_long_ci_hml_eb <- df_hml_eb %>%
  select(-mean_F1_Base, -mean_F1_Locidex) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci")

df_long_F1_hml_eb$Serovar <- factor(df_long_F1_hml_eb$Serovar, levels = rev(serovar_list_plot_eb))  # Set Serovar order

p_main_hml_eb <- df_long_F1_hml_eb %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base and Locidex values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +
  
  # Points for Base and Locidex
  geom_point(aes(color = profile), size = 3) +
  
  geom_text_repel(aes(label = df_long_ci_hml_eb$mean_f1_ci, color = profile),
                  size = 3.25,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  
  # Custom x-axis scale (from 0 to 1 with 0.2 increments)
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.2), limits = c(0.4, 1.5)) +
  
  # Custom colors for Base and Locidex
  scale_color_manual(values = c("Base" = "#353aa1", "Locidex" = "#a32254")) 

df_gap_hml_eb <- df_hml_eb %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Locidex"), paste0("+", round(abs(gap), 2), " Base"))
  )
df_gap_hml_eb$Serovar <- factor(df_gap_hml_eb$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

# Basic plot for gap text with no background, faceted by Pred_label
# Basic plot for gap text with a light background, faceted by Pred_label
p_gap_text_hml_eb <- ggplot(df_gap_hml_eb, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Locidex", "Base"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Base" = "#353aa1", "Locidex" = "#a32254")) +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

# ggarrange(p_main_hml_fn, p_main_hml_eb, align = "hv", labels = "AUTO",
#           common.legend = T, legend = "bottom")
# ggarrange(p_gap_text_hml_fn, p_gap_text_hml_eb, align = "hv")

#######################################
hml_all_compare <- hml_results_all %>%
  filter(profile == "Locidex") %>%
  distinct(Serovar, profile, mean_F1, dataset, mean_f1_ci)

df_hml_compare <- hml_all_compare %>%
  pivot_wider(names_from = dataset, values_from = c(mean_F1, mean_f1_ci)) %>%
  mutate(gap = mean_F1_Combined - mean_F1_FoodNet) %>%
  group_by(Serovar) %>%
  mutate(max = max(mean_F1_FoodNet, mean_F1_Enterobase, mean_F1_Combined, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Serovar = forcats::fct_reorder(Serovar, abs(gap)))

df_long_F1_hml <- df_hml_compare %>%
  select(-mean_f1_ci_FoodNet, -mean_f1_ci_Combined, -mean_f1_ci_Enterobase) %>%
  pivot_longer(cols = starts_with("mean_F1"), 
               names_to = "dataset", 
               names_prefix = "mean_F1_", 
               values_to = "mean_F1")

df_long_ci_hml <- df_hml_compare %>%
  select(-mean_F1_FoodNet, -mean_F1_Combined, -mean_F1_Enterobase) %>%
  pivot_longer(cols = starts_with("mean_f1_ci"), 
               names_to = "ci_profile", 
               names_prefix = "mean_f1_ci_", 
               values_to = "mean_f1_ci")

df_long_F1_hml$Serovar <- factor(df_long_F1_hml$Serovar, levels = rev(serovar_list_plot))

p_main_hml <- df_long_F1_hml %>%
  ggplot(aes(x = mean_F1, y = Serovar)) +
  
  # Line connecting Base, Locidex, and Other values
  geom_line(aes(group = Serovar), color = "#E7E7E7", linewidth = 3.5) +  # Grey line
  
  # Points for Base, Locidex, and Other
  geom_point(aes(color = dataset), size = 3) +
  
  # Text labels for mean_F1 values with repelling and optional arrows
  geom_text_repel(aes(label = df_long_ci_hml$mean_f1_ci, color = dataset),
                  size = 4.5,
                  nudge_x = 0.05,  # Slight nudge to the right for better spacing
                  direction = "y",  # Repel labels vertically to prevent overlap
                  hjust = 0.5,  # Center text labels
                  segment.size = 0.2,  # Add small lines connecting text to points
                  segment.color = "gray70",
                  max.overlaps = Inf) +
  
  # Darker axis labels
  theme_bw() +
  theme(legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),  # Darker x-axis labels
        axis.title.x = element_text(size = 12),  # Customize x-axis title size
        axis.title.y = element_text(size = 12),  # Customize y-axis title size
        panel.grid = element_blank()) +
  
  # Axis titles
  labs(x = "Mean F1 Score", y = "Serovar") +
  
  # Custom x-axis scale (from 0.3 to 1.4 with increments)
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1), limits = c(0.3, 1.2)) +
  
  # Custom colors for Base, Locidex, and Other
  scale_color_manual(values = c("FoodNet" = "#224ba3", "Enterobase" = "#a32276", "Combined" = "#22a35a"))  # Green for Other
  
df_gap_hml <- df_long_F1_hml %>%
  mutate(
    gap_label = if_else(gap > 0, paste0("+", round(abs(gap), 2), " Combined"), paste0("+", round(abs(gap), 2), " FoodNet"))
  )
df_gap_hml$Serovar <- factor(df_gap_hml$Serovar, levels = rev(serovar_list_plot))  # Set Serovar order

p_gap_text_hml <- ggplot(df_gap_hml, aes(x = 0, y = Serovar, label = gap_label, color = ifelse(gap > 0, "Combined", "FoodNet"))) +
  
  # Add gap text labels
  geom_text(size = 3.25, hjust = 0, fontface = "bold") +
  
  # Use the same color scale for Base and Locidex
  scale_color_manual(values = c("Combined" = "#22a35a", "FoodNet" = "#224ba3")) +
  
  # Add a light contrasting background color
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F5F5DC", color = NA),  # Light beige background for the plot
    plot.background = element_rect(fill = "#F5F5DC", color = NA),   # Light beige background for the entire plot
    strip.background = element_blank(),                             # Remove the background for facet labels
    strip.text = element_text(face = "bold", size = 10),             # Make facet text bold
    legend.position = "none"                                         # Hide the legend
  )

########################################
### Confusion Matrix Analysis ##########
########################################
cm_entr_prov_eb1 <- read.csv("cm/cm_entr_prov_ebase_34.csv")
cm_entr_prov_eb2 <- read.csv("cm/cm_entr_prov_ebase_42.csv")
cm_entr_prov_eb3 <- read.csv("cm/cm_entr_prov_ebase_777.csv")
cm_entr_prov_fn1 <- read.csv("cm/cm_entr_prov_fn_34.csv")
cm_entr_prov_fn2 <- read.csv("cm/cm_entr_prov_fn_42.csv")
cm_entr_prov_fn3 <- read.csv("cm/cm_entr_prov_fn_777.csv")
cm_entr_prov_comb1 <- read.csv("cm/cm_entr_prov_34.csv")
cm_entr_prov_comb2 <- read.csv("cm/cm_entr_prov_42.csv")
cm_entr_prov_comb3 <- read.csv("cm/cm_entr_prov_777.csv")

cm_heid_prov_eb1 <- read.csv("cm/cm_heid_prov_ebase_34.csv")
cm_heid_prov_eb2 <- read.csv("cm/cm_heid_prov_ebase_42.csv")
cm_heid_prov_eb3 <- read.csv("cm/cm_heid_prov_ebase_777.csv")
cm_heid_prov_fn1 <- read.csv("cm/cm_heid_prov_fn_34.csv")
cm_heid_prov_fn2 <- read.csv("cm/cm_heid_prov_fn_42.csv")
cm_heid_prov_fn3 <- read.csv("cm/cm_heid_prov_fn_777.csv")
cm_heid_prov_comb1 <- read.csv("cm/cm_heid_prov_34.csv")
cm_heid_prov_comb2 <- read.csv("cm/cm_heid_prov_42.csv")
cm_heid_prov_comb3 <- read.csv("cm/cm_heid_prov_777.csv")
  
cm_heid_animal_eb1 <- read.csv("cm/cm_heid_animal_ebase_34.csv")
cm_heid_animal_eb2 <- read.csv("cm/cm_heid_animal_ebase_42.csv")
cm_heid_animal_eb3 <- read.csv("cm/cm_heid_animal_ebase_777.csv")
cm_heid_animal_fn1 <- read.csv("cm/cm_heid_animal_fn_34.csv")
cm_heid_animal_fn2 <- read.csv("cm/cm_heid_animal_fn_42.csv")
cm_heid_animal_fn3 <- read.csv("cm/cm_heid_animal_fn_777.csv")
cm_heid_animal_comb1 <- read.csv("cm/cm_heid_animal_34.csv")
cm_heid_animal_comb2 <- read.csv("cm/cm_heid_animal_42.csv")
cm_heid_animal_comb3 <- read.csv("cm/cm_heid_animal_777.csv")
  
cm_heid_animalPlus_eb1 <- read.csv("cm/cm_heid_animalPlus_ebase_34.csv")
cm_heid_animalPlus_eb2 <- read.csv("cm/cm_heid_animalPlus_ebase_42.csv")
cm_heid_animalPlus_eb3 <- read.csv("cm/cm_heid_animalPlus_ebase_777.csv")
cm_heid_animalPlus_fn1 <- read.csv("cm/cm_heid_animalPlus_fn_34.csv")
cm_heid_animalPlus_fn2 <- read.csv("cm/cm_heid_animalPlus_fn_42.csv")
cm_heid_animalPlus_fn3 <- read.csv("cm/cm_heid_animalPlus_fn_777.csv")
cm_heid_animalPlus_comb1 <- read.csv("cm/cm_heid_animalPlus_34.csv")
cm_heid_animalPlus_comb2 <- read.csv("cm/cm_heid_animalPlus_42.csv")
cm_heid_animalPlus_comb3 <- read.csv("cm/cm_heid_animalPlus_777.csv")

cm_typh_prov_eb1 <- read.csv("cm/cm_typh_prov_ebase_34.csv")
cm_typh_prov_eb2 <- read.csv("cm/cm_typh_prov_ebase_42.csv")
cm_typh_prov_eb3 <- read.csv("cm/cm_typh_prov_ebase_777.csv")
cm_typh_prov_fn1 <- read.csv("cm/cm_typh_prov_fn_34.csv")
cm_typh_prov_fn2 <- read.csv("cm/cm_typh_prov_fn_42.csv")
cm_typh_prov_fn3 <- read.csv("cm/cm_typh_prov_fn_777.csv")
cm_typh_prov_comb1 <- read.csv("cm/cm_typh_prov_34.csv")
cm_typh_prov_comb2 <- read.csv("cm/cm_typh_prov_42.csv")
cm_typh_prov_comb3 <- read.csv("cm/cm_typh_prov_777.csv")

cm_typh_animal_eb1 <- read.csv("cm/cm_typh_animal_ebase_34.csv")
cm_typh_animal_eb2 <- read.csv("cm/cm_typh_animal_ebase_42.csv")
cm_typh_animal_eb3 <- read.csv("cm/cm_typh_animal_ebase_777.csv")
cm_typh_animal_fn1 <- read.csv("cm/cm_typh_animal_fn_34.csv")
cm_typh_animal_fn2 <- read.csv("cm/cm_typh_animal_fn_42.csv")
cm_typh_animal_fn3 <- read.csv("cm/cm_typh_animal_fn_777.csv")
cm_typh_animal_comb1 <- read.csv("cm/cm_typh_animal_34.csv")
cm_typh_animal_comb2 <- read.csv("cm/cm_typh_animal_42.csv")
cm_typh_animal_comb3 <- read.csv("cm/cm_typh_animal_777.csv")

cm_typh_animalPlus_eb1 <- read.csv("cm/cm_typh_animalPlus_ebase_34.csv")
cm_typh_animalPlus_eb2 <- read.csv("cm/cm_typh_animalPlus_ebase_42.csv")
cm_typh_animalPlus_eb3 <- read.csv("cm/cm_typh_animalPlus_ebase_777.csv")
cm_typh_animalPlus_fn1 <- read.csv("cm/cm_typh_animalPlus_fn_34.csv")
cm_typh_animalPlus_fn2 <- read.csv("cm/cm_typh_animalPlus_fn_42.csv")
cm_typh_animalPlus_fn3 <- read.csv("cm/cm_typh_animalPlus_fn_777.csv")
cm_typh_animalPlus_comb1 <- read.csv("cm/cm_typh_animalPlus_34.csv")
cm_typh_animalPlus_comb2 <- read.csv("cm/cm_typh_animalPlus_42.csv")
cm_typh_animalPlus_comb3 <- read.csv("cm/cm_typh_animalPlus_777.csv")
  
cm_all_prov_eb1 <- read.csv("cm/cm_all_prov_ebase_34.csv")
cm_all_prov_eb2 <- read.csv("cm/cm_all_prov_ebase_42.csv")
cm_all_prov_eb3 <- read.csv("cm/cm_all_prov_ebase_777.csv")
cm_all_prov_fn1 <- read.csv("cm/cm_all_prov_fn_34.csv")
cm_all_prov_fn2 <- read.csv("cm/cm_all_prov_fn_42.csv")
cm_all_prov_fn3 <- read.csv("cm/cm_all_prov_fn_777.csv")
cm_all_prov_comb1 <- read.csv("cm/rf_all_prov_combined_34.csv")
cm_all_prov_comb2 <- read.csv("cm/rf_all_prov_combined_42.csv")
cm_all_prov_comb3 <- read.csv("cm/rf_all_prov_combined_777.csv")

cm_all_animal_eb1 <- read.csv("cm/cm_CombinedSites_Enterobase_34.csv")
cm_all_animal_eb2 <- read.csv("cm/cm_CombinedSites_Enterobase_42.csv")
cm_all_animal_eb3 <- read.csv("cm/cm_CombinedSites_Enterobase_777.csv")
cm_all_animal_fn1 <- read.csv("cm/cm_CombinedSites_FoodNet_34.csv")
cm_all_animal_fn2 <- read.csv("cm/cm_CombinedSites_FoodNet_42.csv")
cm_all_animal_fn3 <- read.csv("cm/cm_CombinedSites_FoodNet_777.csv")
cm_all_animal_comb1 <- read.csv("cm/cm_CombinedSites_Combined_34.csv")
cm_all_animal_comb2 <- read.csv("cm/cm_CombinedSites_Combined_42.csv")
cm_all_animal_comb3 <- read.csv("cm/cm_CombinedSites_Combined_777.csv")

cm_all_animalPlus_eb1 <- read.csv("cm/cm_CombinedSite2_Enterobase_34.csv")
cm_all_animalPlus_eb2 <- read.csv("cm/cm_CombinedSite2_Enterobase_42.csv")
cm_all_animalPlus_eb3 <- read.csv("cm/cm_CombinedSite2_Enterobase_777.csv")
cm_all_animalPlus_fn1 <- read.csv("cm/cm_CombinedSite2_FoodNet_34.csv")
cm_all_animalPlus_fn2 <- read.csv("cm/cm_CombinedSite2_FoodNet_42.csv")
cm_all_animalPlus_fn3 <- read.csv("cm/cm_CombinedSite2_FoodNet_777.csv")
cm_all_animalPlus_comb1 <- read.csv("cm/cm_CombinedSite2_Combined_34.csv")
cm_all_animalPlus_comb2 <- read.csv("cm/cm_CombinedSite2_Combined_42.csv")
cm_all_animalPlus_comb3 <- read.csv("cm/cm_CombinedSite2_Combined_777.csv")

# List of all the data frames
cm_list <- list(
  "cm_entr_prov_eb1", "cm_entr_prov_eb2", "cm_entr_prov_eb3",
  "cm_entr_prov_fn1", "cm_entr_prov_fn2", "cm_entr_prov_fn3",
  "cm_entr_prov_comb1", "cm_entr_prov_comb2", "cm_entr_prov_comb3",
  
  "cm_heid_prov_eb1", "cm_heid_prov_eb2", "cm_heid_prov_eb3",
  "cm_heid_prov_fn1", "cm_heid_prov_fn2", "cm_heid_prov_fn3",
  "cm_heid_prov_comb1", "cm_heid_prov_comb2", "cm_heid_prov_comb3",
  
  "cm_heid_animal_eb1", "cm_heid_animal_eb2", "cm_heid_animal_eb3",
  "cm_heid_animal_fn1", "cm_heid_animal_fn2", "cm_heid_animal_fn3",
  "cm_heid_animal_comb1", "cm_heid_animal_comb2", "cm_heid_animal_comb3",
  
  "cm_heid_animalPlus_eb1", "cm_heid_animalPlus_eb2", "cm_heid_animalPlus_eb3",
  "cm_heid_animalPlus_fn1", "cm_heid_animalPlus_fn2", "cm_heid_animalPlus_fn3",
  "cm_heid_animalPlus_comb1", "cm_heid_animalPlus_comb2", "cm_heid_animalPlus_comb3",
  
  "cm_typh_prov_eb1", "cm_typh_prov_eb2", "cm_typh_prov_eb3",
  "cm_typh_prov_fn1", "cm_typh_prov_fn2", "cm_typh_prov_fn3",
  "cm_typh_prov_comb1", "cm_typh_prov_comb2", "cm_typh_prov_comb3",
  
  "cm_typh_animal_eb1", "cm_typh_animal_eb2", "cm_typh_animal_eb3",
  "cm_typh_animal_fn1", "cm_typh_animal_fn2", "cm_typh_animal_fn3",
  "cm_typh_animal_comb1", "cm_typh_animal_comb2", "cm_typh_animal_comb3",
  
  "cm_typh_animalPlus_eb1", "cm_typh_animalPlus_eb2", "cm_typh_animalPlus_eb3",
  "cm_typh_animalPlus_fn1", "cm_typh_animalPlus_fn2", "cm_typh_animalPlus_fn3",
  "cm_typh_animalPlus_comb1", "cm_typh_animalPlus_comb2", "cm_typh_animalPlus_comb3",
  
  "cm_all_prov_eb1", "cm_all_prov_eb2", "cm_all_prov_eb3",
  "cm_all_prov_fn1", "cm_all_prov_fn2", "cm_all_prov_fn3",
  "cm_all_prov_comb1", "cm_all_prov_comb2", "cm_all_prov_comb3",
  
  "cm_all_animal_eb1", "cm_all_animal_eb2", "cm_all_animal_eb3",
  "cm_all_animal_fn1", "cm_all_animal_fn2", "cm_all_animal_fn3",
  "cm_all_animal_comb1", "cm_all_animal_comb2", "cm_all_animal_comb3",
  
  "cm_all_animalPlus_eb1", "cm_all_animalPlus_eb2", "cm_all_animalPlus_eb3",
  "cm_all_animalPlus_fn1", "cm_all_animalPlus_fn2", "cm_all_animalPlus_fn3",
  "cm_all_animalPlus_comb1", "cm_all_animalPlus_comb2", "cm_all_animalPlus_comb3"
)

# Loop over each data frame
for (df_name in cm_list) {
  # Get the data frame from the string name
  df <- get(df_name)
  
  # Convert the first column to rownames and remove the first column
  rownames(df) <- df[, 1]
  df <- df[, -1]
  
  # Convert the data frame to a matrix
  df <- as.matrix(df)
  
  colnames(df) <- gsub("MB...SK", "MB / SK", colnames(df))
  
  # Assign the modified matrix back to the original variable name
  assign(df_name, df)
}

# Function to calculate mean and mean +- ci matrices for each combo of dataset/label
prov_order <- c("AB", "BC", "MB / SK", "ON", "QC", "NB", "NS", "PE")

calculate_mean_ci_matrix(cm_entr_prov_eb1, cm_entr_prov_eb2, cm_entr_prov_eb3, 
                         output_mean = "cm_entr_prov_eb_mean", output_ci = "cm_entr_prov_eb_mean_ci",
                         output_prop = "cm_entr_prov_eb_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_entr_prov_fn1, cm_entr_prov_fn2, cm_entr_prov_fn3, 
                         output_mean = "cm_entr_prov_fn_mean", output_ci = "cm_entr_prov_fn_mean_ci",
                         output_prop = "cm_entr_prov_fn_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_entr_prov_comb1, cm_entr_prov_comb2, cm_entr_prov_comb3, 
                         output_mean = "cm_entr_prov_cm_mean", output_ci = "cm_entr_prov_cm_mean_ci",
                         output_prop = "cm_entr_prov_cm_prop",
                         desired_order = prov_order)

calculate_mean_ci_matrix(cm_heid_prov_eb1, cm_heid_prov_eb2, cm_heid_prov_eb3, 
                         output_mean = "cm_heid_prov_eb_mean", output_ci = "cm_heid_prov_eb_mean_ci",
                         output_prop = "cm_heid_prov_eb_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_heid_prov_fn1, cm_heid_prov_fn2, cm_heid_prov_fn3, 
                         output_mean = "cm_heid_prov_fn_mean", output_ci = "cm_heid_prov_fn_mean_ci",
                         output_prop = "cm_heid_prov_fn_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_heid_prov_comb1, cm_heid_prov_comb2, cm_heid_prov_comb3, 
                         output_mean = "cm_heid_prov_cm_mean", output_ci = "cm_heid_prov_cm_mean_ci",
                         output_prop = "cm_heid_prov_cm_prop",
                         desired_order = prov_order)

calculate_mean_ci_matrix(cm_heid_animal_eb1, cm_heid_animal_eb2, cm_heid_animal_eb3, 
                         output_mean = "cm_heid_animal_eb_mean", output_ci = "cm_heid_animal_eb_mean_ci",
                         output_prop = "cm_heid_animal_eb_prop")
calculate_mean_ci_matrix(cm_heid_animal_fn1, cm_heid_animal_fn2, cm_heid_animal_fn3, 
                         output_mean = "cm_heid_animal_fn_mean", output_ci = "cm_heid_animal_fn_mean_ci",
                         output_prop = "cm_heid_animal_fn_prop")
calculate_mean_ci_matrix(cm_heid_animal_comb1, cm_heid_animal_comb2, cm_heid_animal_comb3, 
                         output_mean = "cm_heid_animal_cm_mean", output_ci = "cm_heid_animal_cm_mean_ci",
                         output_prop = "cm_heid_animal_cm_prop")

calculate_mean_ci_matrix(cm_heid_animalPlus_eb1, cm_heid_animalPlus_eb2, cm_heid_animalPlus_eb3, 
                         output_mean = "cm_heid_animalPlus_eb_mean", output_ci = "cm_heid_animalPlus_eb_mean_ci",
                         output_prop = "cm_heid_animalPlus_eb_prop")
calculate_mean_ci_matrix(cm_heid_animalPlus_fn1, cm_heid_animalPlus_fn2, cm_heid_animalPlus_fn3, 
                         output_mean = "cm_heid_animalPlus_fn_mean", output_ci = "cm_heid_animalPlus_fn_mean_ci",
                         output_prop = "cm_heid_animalPlus_fn_prop")
calculate_mean_ci_matrix(cm_heid_animalPlus_comb1, cm_heid_animalPlus_comb2, cm_heid_animalPlus_comb3, 
                         output_mean = "cm_heid_animalPlus_cm_mean", output_ci = "cm_heid_animalPlus_cm_mean_ci",
                         output_prop = "cm_heid_animalPlus_cm_prop")

calculate_mean_ci_matrix(cm_typh_prov_eb1, cm_typh_prov_eb2, cm_typh_prov_eb3, 
                         output_mean = "cm_typh_prov_eb_mean", output_ci = "cm_typh_prov_eb_mean_ci",
                         output_prop = "cm_typh_prov_eb_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_typh_prov_fn1, cm_typh_prov_fn2, cm_typh_prov_fn3, 
                         output_mean = "cm_typh_prov_fn_mean", output_ci = "cm_typh_prov_fn_mean_ci",
                         output_prop = "cm_typh_prov_fn_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_typh_prov_comb1, cm_typh_prov_comb2, cm_typh_prov_comb3, 
                         output_mean = "cm_typh_prov_cm_mean", output_ci = "cm_typh_prov_cm_mean_ci",
                         output_prop = "cm_typh_prov_cm_prop",
                         desired_order = prov_order)

calculate_mean_ci_matrix(cm_typh_animal_eb1, cm_typh_animal_eb2, cm_typh_animal_eb3, 
                         output_mean = "cm_typh_animal_eb_mean", output_ci = "cm_typh_animal_eb_mean_ci",
                         output_prop = "cm_typh_animal_eb_prop")
calculate_mean_ci_matrix(cm_typh_animal_fn1, cm_typh_animal_fn2, cm_typh_animal_fn3, 
                         output_mean = "cm_typh_animal_fn_mean", output_ci = "cm_typh_animal_fn_mean_ci",
                         output_prop = "cm_typh_animal_fn_prop")
calculate_mean_ci_matrix(cm_typh_animal_comb1, cm_typh_animal_comb2, cm_typh_animal_comb3, 
                         output_mean = "cm_typh_animal_cm_mean", output_ci = "cm_typh_animal_cm_mean_ci",
                         output_prop = "cm_typh_animal_cm_prop")

calculate_mean_ci_matrix(cm_typh_animalPlus_eb1, cm_typh_animalPlus_eb2, cm_typh_animalPlus_eb3, 
                         output_mean = "cm_typh_animalPlus_eb_mean", output_ci = "cm_typh_animalPlus_eb_mean_ci",
                         output_prop = "cm_typh_animalPlus_eb_prop")
calculate_mean_ci_matrix(cm_typh_animalPlus_fn1, cm_typh_animalPlus_fn2, cm_typh_animalPlus_fn3, 
                         output_mean = "cm_typh_animalPlus_fn_mean", output_ci = "cm_typh_animalPlus_fn_mean_ci",
                         output_prop = "cm_typh_animalPlus_fn_prop")
calculate_mean_ci_matrix(cm_typh_animalPlus_comb1, cm_typh_animalPlus_comb2, cm_typh_animalPlus_comb3, 
                         output_mean = "cm_typh_animalPlus_cm_mean", output_ci = "cm_typh_animalPlus_cm_mean_ci",
                         output_prop = "cm_typh_animalPlus_cm_prop")

calculate_mean_ci_matrix(cm_all_prov_eb1, cm_all_prov_eb2, cm_all_prov_eb3, 
                         output_mean = "cm_all_prov_eb_mean", output_ci = "cm_all_prov_eb_mean_ci",
                         output_prop = "cm_all_prov_eb_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_all_prov_fn1, cm_all_prov_fn2, cm_all_prov_fn3, 
                         output_mean = "cm_all_prov_fn_mean", output_ci = "cm_all_prov_fn_mean_ci",
                         output_prop = "cm_all_prov_fn_prop",
                         desired_order = prov_order)
calculate_mean_ci_matrix(cm_all_prov_comb1, cm_all_prov_comb2, cm_all_prov_comb3, 
                         output_mean = "cm_all_prov_cm_mean", output_ci = "cm_all_prov_cm_mean_ci",
                         output_prop = "cm_all_prov_cm_prop",
                         desired_order = prov_order)

calculate_mean_ci_matrix(cm_all_animal_eb1, cm_all_animal_eb2, cm_all_animal_eb3, 
                         output_mean = "cm_all_animal_eb_mean", output_ci = "cm_all_animal_eb_mean_ci",
                         output_prop = "cm_all_animal_eb_prop")
calculate_mean_ci_matrix(cm_all_animal_fn1, cm_all_animal_fn2, cm_all_animal_fn3, 
                         output_mean = "cm_all_animal_fn_mean", output_ci = "cm_all_animal_fn_mean_ci",
                         output_prop = "cm_all_animal_fn_prop")
calculate_mean_ci_matrix(cm_all_animal_comb1, cm_all_animal_comb2, cm_all_animal_comb3, 
                         output_mean = "cm_all_animal_cm_mean", output_ci = "cm_all_animal_cm_mean_ci",
                         output_prop = "cm_all_animal_cm_prop")

calculate_mean_ci_matrix(cm_all_animalPlus_eb1, cm_all_animalPlus_eb2, cm_all_animalPlus_eb3, 
                         output_mean = "cm_all_animalPlus_eb_mean", output_ci = "cm_all_animalPlus_eb_mean_ci",
                         output_prop = "cm_all_animalPlus_eb_prop")
calculate_mean_ci_matrix(cm_all_animalPlus_fn1, cm_all_animalPlus_fn2, cm_all_animalPlus_fn3, 
                         output_mean = "cm_all_animalPlus_fn_mean", output_ci = "cm_all_animalPlus_fn_mean_ci",
                         output_prop = "cm_all_animalPlus_fn_prop")
calculate_mean_ci_matrix(cm_all_animalPlus_comb1, cm_all_animalPlus_comb2, cm_all_animalPlus_comb3, 
                         output_mean = "cm_all_animalPlus_cm_mean", output_ci = "cm_all_animalPlus_cm_mean_ci",
                         output_prop = "cm_all_animalPlus_cm_prop")

### Enteritidis prov heatmaps ###
set_breaks <- seq(0, 100, by = 10)
set_colors <- colorRamp2(c(0, 59.9, 60, 100), c("pink", "pink", "#ccffcc", "#006600"))

entr_prov_eb_map <- gen_heatmap(cm_entr_prov_eb_prop, cm_entr_prov_eb_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

entr_prov_fn_map <- gen_heatmap(cm_entr_prov_fn_prop, cm_entr_prov_fn_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

entr_prov_cm_map <- gen_heatmap(cm_entr_prov_cm_prop, cm_entr_prov_cm_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

# entr_prov_eb_grid <- entr_prov_eb_map %>% draw() %>% grid.grabExpr() ###DOESNT WORK FOR SOME REASON
# entr_prov_fn_grid <- entr_prov_fn_map %>% draw() %>% grid.grabExpr()
# entr_prov_cm_grid <- entr_prov_cm_map %>% draw() %>% grid.grabExpr()
# 
# plot_grid(entr_prov_eb_grid,  plot_grid(entr_prov_fn_grid, entr_prov_cm_grid))

gen_legend(set_breaks, set_colors, title = "Proportion (%)", direction = "horizontal")


### Heidelberg prov heatmaps ###
heid_prov_eb_map <- gen_heatmap(cm_heid_prov_eb_prop, cm_heid_prov_eb_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

heid_prov_fn_map <- gen_heatmap(cm_heid_prov_fn_prop, cm_heid_prov_fn_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

heid_prov_cm_map <- gen_heatmap(cm_heid_prov_cm_prop, cm_heid_prov_cm_mean_ci, set_breaks, set_colors,
                                fontsize = 25)
# 
# heid_prov_eb_grid <- grid.grabExpr(draw(heid_prov_eb_map, heatmap_legend_side = "right"))
# heid_prov_fn_grid <- grid.grabExpr(draw(heid_prov_fn_map, heatmap_legend_side = "right"))
# heid_prov_cm_grid <- grid.grabExpr(draw(heid_prov_cm_map, heatmap_legend_side = "right"))
# plot_grid(heid_prov_eb_grid, heid_prov_fn_grid, heid_prov_cm_grid, ncol = 2, nrow = 2)


### Heidelberg animal heatmaps ###
heid_animal_eb_map <- gen_heatmap(cm_heid_animal_eb_prop, cm_heid_animal_eb_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)

heid_animal_fn_map <- gen_heatmap(cm_heid_animal_fn_prop, cm_heid_animal_fn_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)

heid_animal_cm_map <- gen_heatmap(cm_heid_animal_cm_prop, cm_heid_animal_cm_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)
# 
# heid_animal_eb_grid <- grid.grabExpr(draw(heid_animal_eb_map, heatmap_legend_side = "right"))
# heid_animal_fn_grid <- grid.grabExpr(draw(heid_animal_fn_map, heatmap_legend_side = "right"))
# heid_animal_cm_grid <- grid.grabExpr(draw(heid_animal_cm_map, heatmap_legend_side = "right"))
# plot_grid(heid_animal_eb_grid, heid_animal_fn_grid, heid_animal_cm_grid, ncol = 2, nrow = 2)


### Heidelberg animalPlus heatmaps ###
heid_animalPlus_eb_map <- gen_heatmap(cm_heid_animalPlus_eb_prop, cm_heid_animalPlus_eb_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

heid_animalPlus_fn_map <- gen_heatmap(cm_heid_animalPlus_fn_prop, cm_heid_animalPlus_fn_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

heid_animalPlus_cm_map <- gen_heatmap(cm_heid_animalPlus_cm_prop, cm_heid_animalPlus_cm_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

# heid_animalPlus_eb_grid <- grid.grabExpr(draw(heid_animalPlus_eb_map, heatmap_legend_side = "right"))
# heid_animalPlus_fn_grid <- grid.grabExpr(draw(heid_animalPlus_fn_map, heatmap_legend_side = "right"))
# heid_animalPlus_cm_grid <- grid.grabExpr(draw(heid_animalPlus_cm_map, heatmap_legend_side = "right"))
# plot_grid(heid_animalPlus_eb_grid, heid_animalPlus_fn_grid, heid_animalPlus_cm_grid, ncol = 2, nrow = 2)


### Typhimurium prov heatmaps ###
typh_prov_eb_map <- gen_heatmap(cm_typh_prov_eb_prop, cm_typh_prov_eb_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

typh_prov_fn_map <- gen_heatmap(cm_typh_prov_fn_prop, cm_typh_prov_fn_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

typh_prov_cm_map <- gen_heatmap(cm_typh_prov_cm_prop, cm_typh_prov_cm_mean_ci, set_breaks, set_colors,
                                fontsize = 25)

# typh_prov_eb_grid <- grid.grabExpr(draw(typh_prov_eb_map, heatmap_legend_side = "right"))
# typh_prov_fn_grid <- grid.grabExpr(draw(typh_prov_fn_map, heatmap_legend_side = "right"))
# typh_prov_cm_grid <- grid.grabExpr(draw(typh_prov_cm_map, heatmap_legend_side = "right"))
# plot_grid(typh_prov_eb_grid, typh_prov_fn_grid, typh_prov_cm_grid, ncol = 2, nrow = 2)


### Typhimurium animal heatmaps ###
typh_animal_eb_map <- gen_heatmap(cm_typh_animal_eb_prop, cm_typh_animal_eb_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)

typh_animal_fn_map <- gen_heatmap(cm_typh_animal_fn_prop, cm_typh_animal_fn_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)

typh_animal_cm_map <- gen_heatmap(cm_typh_animal_cm_prop, cm_typh_animal_cm_mean_ci, set_breaks, set_colors,
                                  fontsize = 25)

# typh_animal_eb_grid <- grid.grabExpr(draw(typh_animal_eb_map, heatmap_legend_side = "right"))
# typh_animal_fn_grid <- grid.grabExpr(draw(typh_animal_fn_map, heatmap_legend_side = "right"))
# typh_animal_cm_grid <- grid.grabExpr(draw(typh_animal_cm_map, heatmap_legend_side = "right"))
# plot_grid(typh_animal_eb_grid, typh_animal_fn_grid, typh_animal_cm_grid, ncol = 2, nrow = 2)


### Typhimurium animalPlus heatmaps ###
typh_animalPlus_eb_map <- gen_heatmap(cm_typh_animalPlus_eb_prop, cm_typh_animalPlus_eb_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

typh_animalPlus_fn_map <- gen_heatmap(cm_typh_animalPlus_fn_prop, cm_typh_animalPlus_fn_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

typh_animalPlus_cm_map <- gen_heatmap(cm_typh_animalPlus_cm_prop, cm_typh_animalPlus_cm_mean_ci, set_breaks, set_colors,
                                      fontsize = 25)

# typh_animalPlus_eb_grid <- grid.grabExpr(draw(typh_animalPlus_eb_map, heatmap_legend_side = "right"))
# typh_animalPlus_fn_grid <- grid.grabExpr(draw(typh_animalPlus_fn_map, heatmap_legend_side = "right"))
# typh_animalPlus_cm_grid <- grid.grabExpr(draw(typh_animalPlus_cm_map, heatmap_legend_side = "right"))
# plot_grid(typh_animalPlus_eb_grid, typh_animalPlus_fn_grid, typh_animalPlus_cm_grid, ncol = 2, nrow = 2)


### All province heatmap ###
all_prov_eb_map <- gen_heatmap(cm_all_prov_eb_prop, cm_all_prov_eb_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

all_prov_fn_map <- gen_heatmap(cm_all_prov_fn_prop, cm_all_prov_fn_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

all_prov_cm_map <- gen_heatmap(cm_all_prov_cm_prop, cm_all_prov_cm_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

# all_prov_eb_grid <- grid.grabExpr(draw(all_prov_eb_map, heatmap_legend_side = "right"))
# all_prov_fn_grid <- grid.grabExpr(draw(all_prov_fn_map, heatmap_legend_side = "right"))
# all_prov_cm_grid <- grid.grabExpr(draw(all_prov_cm_map, heatmap_legend_side = "right"))
# plot_grid(all_prov_eb_grid, all_prov_fn_grid, all_prov_cm_grid, ncol = 2, nrow = 2)

### All animal heatmap ###
all_animal_eb_map <- gen_heatmap(cm_all_animal_eb_prop, cm_all_animal_eb_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

all_animal_fn_map <- gen_heatmap(cm_all_animal_fn_prop, cm_all_animal_fn_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

all_animal_cm_map <- gen_heatmap(cm_all_animal_cm_prop, cm_all_animal_cm_mean_ci, set_breaks, 
                               set_colors, fontsize = 22)

### All animalPlus heatmap ###
all_animalPlus_eb_map <- gen_heatmap(cm_all_animalPlus_eb_prop, cm_all_animalPlus_eb_mean_ci, set_breaks, 
                                 set_colors, fontsize = 22)

all_animalPlus_fn_map <- gen_heatmap(cm_all_animalPlus_fn_prop, cm_all_animalPlus_fn_mean_ci, set_breaks, 
                                 set_colors, fontsize = 22)

all_animalPlus_cm_map <- gen_heatmap(cm_all_animalPlus_cm_prop, cm_all_animalPlus_cm_mean_ci, set_breaks, 
                                 set_colors, fontsize = 22)
##### Changes in Num_Pred ########
# Data for matrix 1
eb_pred_label_mx <- matrix(c(
  4, 0, 0,   # Enteritidis
  3, 2, 3,   # Heidelberg
  4, 3, 3,   # Typhimurium
  0, 0, 0,   # Kentucky
  0, 0, 0,   # Infantis
  0, 0, 0,   # I:4,[5],12:i:-
  8, 5, 7    # All
), nrow = 7, byrow = TRUE)

rownames(eb_pred_label_mx) <- c("Enteritidis", "Heidelberg", "Typhimurium", "Kentucky", "Infantis", "I:4,[5],12:i:-", "All")
colnames(eb_pred_label_mx) <- c("Province", "Animal", "AnimalPlus")

# Data for matrix 2
fn_pred_label_mx <- matrix(c(
  4, 2, 3,   # Enteritidis
  4, 3, 4,   # Heidelberg
  4, 3, 4,   # Typhimurium
  5, 3, 2,   # Kentucky
  5, 4, 5,   # Infantis
  3, 2, 6,   # I:4,[5],12:i:-
  7, 6, 9    # All
), nrow = 7, byrow = TRUE)

rownames(fn_pred_label_mx) <- c("Enteritidis", "Heidelberg", "Typhimurium", "Kentucky", "Infantis", "I:4,[5],12:i:-", "All")
colnames(fn_pred_label_mx) <- c("Province", "Animal", "AnimalPlus")

# Data for matrix 3
comb_pred_label_mx <- matrix(c(
  6, 3, 3,   # Enteritidis
  6, 3, 5,   # Heidelberg
  5, 5, 6,   # Typhimurium
  5, 3, 2,   # Kentucky
  5, 4, 5,   # Infantis
  3, 4, 6,   # I:4,[5],12:i:-
  8, 6, 10   # All
), nrow = 7, byrow = TRUE)

rownames(comb_pred_label_mx) <- c("Enteritidis", "Heidelberg", "Typhimurium", "Kentucky", "Infantis", "I:4,[5],12:i:-", "All")
colnames(comb_pred_label_mx) <- c("Province", "Animal", "AnimalPlus")

diff_pred_label_mx <- comb_pred_label_mx - fn_pred_label_mx

label_breaks <- seq(0, 10, by = 2)
label_colors <- colorRamp2(label_breaks, colorRampPalette(brewer.pal(5, "PuOr"))(length(label_breaks)))

eb_label_heatmap <- Heatmap(eb_pred_label_mx, 
                                name = "Entropy",  
                                col = label_colors, 
                                cluster_rows = F, 
                                cluster_columns = F,
                                show_heatmap_legend = T, 
                                heatmap_legend_param = list(
                                  legend_direction = "vertical",
                                  at = label_breaks, 
                                  labels = label_breaks),
                                column_names_rot = 90, 
                                row_dend_side = "right", 
                                row_names_side = "left",  # Ensure row labels are on the left
                                column_names_side = "bottom",  # Ensure column labels are on the top
                                row_title = "Serovar",  # Add row title "True"
                                column_title = "Label",  # Add column title "Predicted"
                                column_title_side = "bottom",
                                row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                                column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                                row_names_gp = gpar(fontsize = 15),
                            
                            # Custom text inside cells based on label_df
                            cell_fun = function(j, i, x, y, width, height, fill) {
                              grid.text(as.character(eb_pred_label_mx[i, j]), x, y, 
                                        gp = gpar(col = "black", fontsize = 14))
                            })  

fn_label_heatmap <- Heatmap(fn_pred_label_mx, 
                            name = "Entropy",  
                            col = label_colors, 
                            cluster_rows = F, 
                            cluster_columns = F,
                            show_heatmap_legend = T, 
                            heatmap_legend_param = list(
                              legend_direction = "vertical",
                              at = label_breaks, 
                              labels = label_breaks),
                            column_names_rot = 90, 
                            row_dend_side = "right", 
                            row_names_side = "left",  # Ensure row labels are on the left
                            column_names_side = "bottom",  # Ensure column labels are on the top
                            row_title = "Serovar",  # Add row title "True"
                            column_title = "Label",  # Add column title "Predicted"
                            column_title_side = "bottom",
                            row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                            column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                            row_names_gp = gpar(fontsize = 15), # Row label font size)
                            # Custom text inside cells based on label_df
                            cell_fun = function(j, i, x, y, width, height, fill) {
                              grid.text(as.character(fn_pred_label_mx[i, j]), x, y, 
                                        gp = gpar(col = "black", fontsize = 14))
                            })  


comb_label_heatmap <- Heatmap(comb_pred_label_mx, 
                            name = "Predicted label (n)",  
                            col = label_colors, 
                            cluster_rows = F, 
                            cluster_columns = F,
                            show_heatmap_legend = T, 
                            heatmap_legend_param = list(
                              legend_direction = "vertical",
                              at = label_breaks, 
                              labels = label_breaks),
                            column_names_rot = 90, 
                            row_dend_side = "right", 
                            row_names_side = "left",  # Ensure row labels are on the left
                            column_names_side = "bottom",  # Ensure column labels are on the top
                            row_title = "Serovar",  # Add row title "True"
                            column_title = "Label",  # Add column title "Predicted"
                            column_title_side = "bottom",
                            row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                            column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                            row_names_gp = gpar(fontsize = 15), # Row label font size)
                            # Custom text inside cells based on label_df
                            cell_fun = function(j, i, x, y, width, height, fill) {
                              grid.text(as.character(comb_pred_label_mx[i, j]), x, y, 
                                        gp = gpar(col = "black", fontsize = 14))
                            })  

diff_label_breaks <- seq(0, 3, by = 1)
diff_label_colors <- colorRamp2(diff_label_breaks, c("#1F77B4", "#17BECF", "yellow", "#2CA02C"))

diff_label_heatmap <- Heatmap(diff_pred_label_mx, 
                              name = "Predicted label (n)",  
                              col = diff_label_colors, 
                              cluster_rows = F, 
                              cluster_columns = F,
                              show_heatmap_legend = T, 
                              heatmap_legend_param = list(
                                legend_direction = "vertical",
                                at = diff_label_breaks, 
                                labels = diff_label_breaks),
                              column_names_rot = 90, 
                              row_dend_side = "right", 
                              row_names_side = "left",  # Ensure row labels are on the left
                              column_names_side = "bottom",  # Ensure column labels are on the top
                              row_title = "Serovar",  # Add row title "True"
                              column_title = "Label",  # Add column title "Predicted"
                              column_title_side = "bottom",
                              row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                              column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                              row_names_gp = gpar(fontsize = 15), # Row label font size)
                              # Custom text inside cells based on label_df
                              cell_fun = function(j, i, x, y, width, height, fill) {
                                grid.text(as.character(diff_pred_label_mx[i, j]), x, y, 
                                          gp = gpar(col = "black", fontsize = 14))
                              })  

label_heatmaps <- eb_label_heatmap + fn_label_heatmap + comb_label_heatmap + diff_label_heatmap
draw(label_heatmaps)

gen_legend(diff_label_breaks, diff_label_colors, title = "ΔPredicted\n Label (n)")

########################################
### Entropy Analysis ###################
########################################
fn_meta <- fn_profile %>%
  select(c(1:6))
eb_meta_prov <- ebase_prov_profile %>%
  select(c(1:3))
eb_meta_host <- ebase_host_profile %>%
  select(c(1:4))
comb_meta_prov <- combined_prov_profile %>%
  select(c(1:3))
comb_meta_host <- combined_host_profile %>%
  select(c(1:4))

serovar_list_eb <- c("Enteritidis", "Heidelberg", "Typhimurium", "Infantis", "Kentucky", "I 1,4,[5],12:i:-")

eb_entropy_prov <- calculate_entropy_full(df = eb_meta_prov, serovar_col = "serovar_cgmlst", 
                                          label_col = "Province", pred_label = "Province", 
                                          dataset = "Enterobase", serovar_list = serovar_list_eb)
eb_entropy_animal <- calculate_entropy_full(df = eb_meta_host, serovar_col = "serovar_cgmlst", 
                                            label_col = "CombinedSites", pred_label = "Animal", 
                                            dataset = "Enterobase", serovar_list = serovar_list_eb)
eb_entropy_animalPlus <- calculate_entropy_full(df = eb_meta_host, serovar_col = "serovar_cgmlst", 
                                                label_col = "CombinedSite2", pred_label = "AnimalPlus", 
                                                dataset = "Enterobase", serovar_list = serovar_list_eb)
eb_entropy_prov_all <- calculate_entropy_full(df = eb_meta_prov, serovar_col = "serovar_cgmlst", 
                                              label_col = "Province", pred_label = "Province", 
                                              dataset = "Enterobase")
eb_entropy_animal_all <- calculate_entropy_full(df = eb_meta_host, serovar_col = "serovar_cgmlst", 
                                                label_col = "CombinedSites", pred_label = "Animal", 
                                                dataset = "Enterobase")
eb_entropy_animalPlus_all <- calculate_entropy_full(df = eb_meta_host, serovar_col = "serovar_cgmlst", 
                                                    label_col = "CombinedSite2", pred_label = "AnimalPlus", 
                                                    dataset = "Enterobase")

fn_entropy_prov <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                          label_col = "SourceState", pred_label = "Province",
                                          dataset = "FoodNet", serovar_list = serovar_list)
fn_entropy_animal <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                            label_col = "CombinedSites", pred_label = "Animal",
                                            dataset = "FoodNet", serovar_list = serovar_list)
fn_entropy_animalPlus <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                                label_col = "CombinedSite2", pred_label = "AnimalPlus",
                                                dataset = "FoodNet", serovar_list = serovar_list)
fn_entropy_prov_all <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                              label_col = "SourceState", pred_label = "Province",
                                              dataset = "FoodNet")
fn_entropy_animal_all <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                                label_col = "CombinedSites", pred_label = "Animal",
                                                dataset = "FoodNet")
fn_entropy_animalPlus_all <- calculate_entropy_full(df = fn_meta, serovar_col = "Serovar", 
                                                    label_col = "CombinedSite2", pred_label = "AnimalPlus",
                                                    dataset = "FoodNet")

comb_entropy_prov <- calculate_entropy_full(df = comb_meta_prov, serovar_col = "Serovar", 
                                            label_col = "Province", pred_label = "Province",
                                            dataset = "Combined", serovar_list = serovar_list)
comb_entropy_animal <- calculate_entropy_full(df = comb_meta_host, serovar_col = "Serovar", 
                                              label_col = "CombinedSites", pred_label = "Animal",
                                              dataset = "Combined", serovar_list = serovar_list)
comb_entropy_animalPlus <- calculate_entropy_full(df = comb_meta_host, serovar_col = "Serovar", 
                                                  label_col = "CombinedSite2", pred_label = "AnimalPlus",
                                                  dataset = "Combined", serovar_list = serovar_list)
comb_entropy_prov_all <- calculate_entropy_full(df = comb_meta_prov, serovar_col = "Serovar", 
                                                label_col = "Province", pred_label = "Province",
                                                dataset = "Combined")
comb_entropy_animal_all <- calculate_entropy_full(df = comb_meta_host, serovar_col = "Serovar", 
                                                  label_col = "CombinedSites", pred_label = "Animal",
                                                  dataset = "Combined")
comb_entropy_animalPlus_all <- calculate_entropy_full(df = comb_meta_host, serovar_col = "Serovar", 
                                                      label_col = "CombinedSite2", pred_label = "AnimalPlus",
                                                      dataset = "Combined")

eb_entropy_full <- rbind(eb_entropy_prov, eb_entropy_animal, eb_entropy_animalPlus,
                         eb_entropy_prov_all, eb_entropy_animal_all, eb_entropy_animalPlus_all)
fn_entropy_full <- rbind(fn_entropy_prov, fn_entropy_animal, fn_entropy_animalPlus,
                         fn_entropy_prov_all, fn_entropy_animal_all, fn_entropy_animalPlus_all)
comb_entropy_full <- rbind(comb_entropy_prov, comb_entropy_animal, comb_entropy_animalPlus, 
                           comb_entropy_prov_all, comb_entropy_animal_all, comb_entropy_animalPlus_all)

eb_entropy_mtx <- eb_entropy_full %>% 
  select(Serovar, Pred_label, Entropy) %>% 
  distinct(Serovar, Pred_label, Entropy, .keep_all = T) %>%
  pivot_wider(names_from = Pred_label, values_from = Entropy) %>%
  as.data.frame()
rownames(eb_entropy_mtx) <- eb_entropy_mtx$Serovar
eb_entropy_mtx <- as.matrix(eb_entropy_mtx[, -1])

fn_entropy_mtx <- fn_entropy_full %>% 
  select(Serovar, Pred_label, Entropy) %>% 
  distinct(Serovar, Pred_label, Entropy, .keep_all = T) %>%
  pivot_wider(names_from = Pred_label, values_from = Entropy) %>%
  as.data.frame()
rownames(fn_entropy_mtx) <- fn_entropy_mtx$Serovar
fn_entropy_mtx <- as.matrix(fn_entropy_mtx[, -1])

comb_entropy_mtx <- comb_entropy_full %>% 
  select(Serovar, Pred_label, Entropy) %>% 
  distinct(Serovar, Pred_label, Entropy, .keep_all = T) %>%
  pivot_wider(names_from = Pred_label, values_from = Entropy) %>%
  as.data.frame()
rownames(comb_entropy_mtx) <- comb_entropy_mtx$Serovar
comb_entropy_mtx <- as.matrix(comb_entropy_mtx[, -1])

diff_entropy_mtx <- comb_entropy_mtx - fn_entropy_mtx

my_breaks <- seq(0, 2.5, by = 0.5)
my_colors <- colorRamp2(my_breaks, colorRampPalette(brewer.pal(5, "RdYlBu"))(length(my_breaks)))

eb_entropy_heatmap <- Heatmap(eb_entropy_mtx, 
                              name = "Entropy",  
                              col = my_colors, 
                              cluster_rows = F, 
                              cluster_columns = FALSE,
                              show_heatmap_legend = T, 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal",
                                at = my_breaks, 
                                labels = my_breaks),
                              row_dend_side = "right", 
                              column_names_rot = 90, 
                              row_names_side = "left",  # Ensure row labels are on the left
                              column_names_side = "bottom",  # Ensure column labels are on the top
                              row_title = "Serovar",  # Add row title "True"
                              column_title = "Label",  # Add column title "Predicted"
                              column_title_side = "bottom",
                              row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                              column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                              row_names_gp = gpar(fontsize = 15))  # Row label font size)

fn_entropy_heatmap <- Heatmap(fn_entropy_mtx, 
                              name = "Entropy",  
                              col = my_colors, 
                              cluster_rows = F, 
                              cluster_columns = FALSE,
                              show_heatmap_legend = T, 
                              heatmap_legend_param = list(
                                legend_direction = "horizontal",
                                at = my_breaks, 
                                labels = my_breaks),
                              row_dend_side = "right", 
                              column_names_rot = 90, 
                              row_names_side = "left",  # Ensure row labels are on the left
                              column_names_side = "bottom",  # Ensure column labels are on the top
                              row_title = "Serovar",  # Add row title "True"
                              column_title = "Label",  # Add column title "Predicted"
                              column_title_side = "bottom",
                              row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                              column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                              row_names_gp = gpar(fontsize = 15))  # Row label font size)

comb_entropy_heatmap <- Heatmap(comb_entropy_mtx, 
                                name = "Entropy",  
                                col = my_colors, 
                                cluster_rows = TRUE, 
                                cluster_columns = FALSE,
                                show_heatmap_legend = T, 
                                heatmap_legend_param = list(
                                  legend_direction = "horizontal",
                                  at = my_breaks, 
                                  labels = my_breaks),
                                row_dend_side = "right", 
                                column_names_rot = 90, 
                                row_names_side = "left",  # Ensure row labels are on the left
                                column_names_side = "bottom",  # Ensure column labels are on the top
                                row_title = "Serovar",  # Add row title "True"
                                column_title = "Label",  # Add column title "Predicted"
                                column_title_side = "bottom",
                                row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                                column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                                row_names_gp = gpar(fontsize = 15))  # Row label font size)

draw(Legend(col_fun = my_colors, title = "Entropy", at = my_breaks,
            labels = my_breaks, direction = "horizontal"))

diff_breaks <- seq(-0.1, 0.25, by = 0.05)
diff_colors <- colorRamp2(diff_breaks, c("#00441B", "#1B7837", "#5AAE61", "#D9F0D3", "#E7D4E8", "#C2A5CF", "#9970AB", "#762A83"))

diff_entropy_heatmap <- Heatmap(diff_entropy_mtx, 
                                name = "Entropy",  
                                col = diff_colors, 
                                cluster_rows = TRUE, 
                                cluster_columns = FALSE,
                                show_heatmap_legend = T, 
                                heatmap_legend_param = list(
                                  legend_direction = "vertical",
                                  at = diff_breaks, 
                                  labels = diff_breaks),
                                column_names_rot = 90, 
                                row_dend_side = "right", 
                                row_names_side = "left",  # Ensure row labels are on the left
                                column_names_side = "bottom",  # Ensure column labels are on the top
                                row_title = "Serovar",  # Add row title "True"
                                column_title = "Label",  # Add column title "Predicted"
                                column_title_side = "bottom",
                                row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                                column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                                row_names_gp = gpar(fontsize = 15))  # Row label font size)

entropy_heatmaps <- eb_entropy_heatmap + fn_entropy_heatmap + comb_entropy_heatmap + diff_entropy_heatmap
draw(entropy_heatmaps)

gen_legend(my_breaks, my_colors, title = "Entropy")
gen_legend(diff_breaks, diff_colors, title = "ΔEntropy")

### Finding the number of distinct labels per prediction category ###
all_labels_eb <- ebase_prov_profile %>% 
  filter(serovar_cgmlst %in% serovar_list_eb) %>%
  group_by(serovar_cgmlst) %>%
  summarise(Province = n_distinct(Province), .groups = 'drop')
all_labels_eb2 <- ebase_prov_profile %>%
  summarise(Province = n_distinct(Province)) %>%
  pull(Province)

# Step 3: Convert the summarized data to a matrix
all_labels_eb_mtx <- as.matrix(all_labels_eb$Province)
rownames(all_labels_eb_mtx) <- all_labels_eb$serovar_cgmlst
colnames(all_labels_eb_mtx) <- "Province"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_eb_mtx <- rbind(all_labels_eb_mtx, All = all_labels_eb2)

all_labels_eb3 <- ebase_host_profile %>% 
  filter(serovar_cgmlst %in% serovar_list_eb) %>%
  group_by(serovar_cgmlst) %>%
  summarise(CombinedSites = n_distinct(CombinedSites), .groups = 'drop')
all_labels_eb4 <- ebase_host_profile %>%
  summarise(CombinedSites = n_distinct(CombinedSites)) %>%
  pull(CombinedSites)

# Step 3: Convert the summarized data to a matrix
all_labels_eb_mtx2 <- as.matrix(all_labels_eb3$CombinedSites)
rownames(all_labels_eb_mtx2) <- all_labels_eb3$serovar_cgmlst
colnames(all_labels_eb_mtx2) <- "CombinedSites"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_eb_mtx2 <- rbind(all_labels_eb_mtx2, All = all_labels_eb4)

all_labels_eb5 <- ebase_host_profile %>% 
  filter(serovar_cgmlst %in% serovar_list_eb) %>%
  group_by(serovar_cgmlst) %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2), .groups = 'drop')
all_labels_eb6 <- ebase_host_profile %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2)) %>%
  pull(CombinedSite2)

# Step 3: Convert the summarized data to a matrix
all_labels_eb_mtx3 <- as.matrix(all_labels_eb5$CombinedSite2)
rownames(all_labels_eb_mtx3) <- all_labels_eb5$serovar_cgmlst
colnames(all_labels_eb_mtx3) <- "CombinedSites"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_eb_mtx3 <- rbind(all_labels_eb_mtx3, All = all_labels_eb6)

## FoodNet ##
all_labels_fn <- fn_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(SourceState = n_distinct(SourceState), .groups = 'drop')
all_labels_fn2 <- fn_profile %>%
  summarise(SourceState = n_distinct(SourceState)) %>%
  pull(SourceState)

# Step 3: Convert the summarized data to a matrix
all_labels_fn_mtx <- as.matrix(all_labels_fn$SourceState)
rownames(all_labels_fn_mtx) <- all_labels_fn$Serovar
colnames(all_labels_fn_mtx) <- "SourceState"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_fn_mtx <- rbind(all_labels_fn_mtx, All = all_labels_fn2)

all_labels_fn3 <- fn_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(CombinedSites = n_distinct(CombinedSites), .groups = 'drop')
all_labels_fn4 <- fn_profile %>%
  summarise(CombinedSites = n_distinct(CombinedSites)) %>%
  pull(CombinedSites)

# Step 3: Convert the summarized data to a matrix
all_labels_fn_mtx2 <- as.matrix(all_labels_fn3$CombinedSites)
rownames(all_labels_fn_mtx2) <- all_labels_fn3$Serovar
colnames(all_labels_fn_mtx2) <- "CombinedSites"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_fn_mtx2 <- rbind(all_labels_fn_mtx2, All = all_labels_fn4)

all_labels_fn5 <- fn_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2), .groups = 'drop')
all_labels_fn6 <- fn_profile %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2)) %>%
  pull(CombinedSite2)

# Step 3: Convert the summarized data to a matrix
all_labels_fn_mtx3 <- as.matrix(all_labels_fn5$CombinedSite2)
rownames(all_labels_fn_mtx3) <- all_labels_fn5$Serovar
colnames(all_labels_fn_mtx3) <- "CombinedSites"

# Step 4: Add a row for the total distinct SourceState under the rowname "All"
all_labels_fn_mtx3 <- rbind(all_labels_fn_mtx3, All = all_labels_fn6)

## Combined ##
all_labels_comb <- combined_prov_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(Province = n_distinct(Province), .groups = 'drop')
all_labels_comb2 <- combined_prov_profile %>%
  summarise(Province = n_distinct(Province)) %>%
  pull(Province)

# Step 3: Convert the summarized data to a matrix
all_labels_comb_mtx <- as.matrix(all_labels_comb$Province)
rownames(all_labels_comb_mtx) <- all_labels_comb$Serovar
colnames(all_labels_comb_mtx) <- "Province"

# Step 4: Add a row for the total distinct Province under the rowname "All"
all_labels_comb_mtx <- rbind(all_labels_comb_mtx, All = all_labels_comb2)

all_labels_comb3 <- combined_host_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(CombinedSites = n_distinct(CombinedSites), .groups = 'drop')
all_labels_comb4 <- combined_host_profile %>%
  summarise(CombinedSites = n_distinct(CombinedSites)) %>%
  pull(CombinedSites)

# Step 3: Convert the summarized data to a matrix
all_labels_comb_mtx2 <- as.matrix(all_labels_comb3$CombinedSites)
rownames(all_labels_comb_mtx2) <- all_labels_comb3$Serovar
colnames(all_labels_comb_mtx2) <- "CombinedSites"

# Step 4: Add a row for the total distinct Province under the rowname "All"
all_labels_comb_mtx2 <- rbind(all_labels_comb_mtx2, All = all_labels_comb4)

all_labels_comb5 <- combined_host_profile %>% 
  filter(Serovar %in% serovar_list) %>%
  group_by(Serovar) %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2), .groups = 'drop')
all_labels_comb6 <- combined_host_profile %>%
  summarise(CombinedSite2 = n_distinct(CombinedSite2)) %>%
  pull(CombinedSite2)

# Step 3: Convert the summarized data to a matrix
all_labels_comb_mtx3 <- as.matrix(all_labels_comb5$CombinedSite2)
rownames(all_labels_comb_mtx3) <- all_labels_comb5$Serovar
colnames(all_labels_comb_mtx3) <- "CombinedSites"

# Step 4: Add a row for the total distinct Province under the rowname "All"
all_labels_comb_mtx3 <- rbind(all_labels_comb_mtx3, All = all_labels_comb6)


labels_eb <- cbind(all_labels_eb_mtx, all_labels_eb_mtx2, all_labels_eb_mtx3)
labels_fn <- cbind(all_labels_fn_mtx, all_labels_fn_mtx2, all_labels_fn_mtx3)
labels_cm <- cbind(all_labels_comb_mtx, all_labels_comb_mtx2, all_labels_comb_mtx3)

max_entropy_mtx_eb <- log2(labels_eb)
max_entropy_mtx_fn <- log2(labels_fn)
max_entropy_mtx_comb <- log2(labels_cm)

max_entropy_mtx_eb[max_entropy_mtx_eb == -Inf] <- 0
max_entropy_mtx_fn[max_entropy_mtx_fn == -Inf] <- 0
max_entropy_mtx_comb[max_entropy_mtx_comb == -Inf] <- 0

max_entropy_mtx_eb <- round(max_entropy_mtx_eb, 1)
max_entropy_mtx_fn <- round(max_entropy_mtx_fn, 1)
max_entropy_mtx_comb <- round(max_entropy_mtx_comb, 1)
eb_entropy_mtx <- round(eb_entropy_mtx, 1)
fn_entropy_mtx <- round(fn_entropy_mtx, 1)
comb_entropy_mtx <- round(comb_entropy_mtx, 1)

proportion_entropy_eb <- (eb_entropy_mtx / max_entropy_mtx_eb) * 100
proportion_entropy_fn <- (fn_entropy_mtx / max_entropy_mtx_fn) * 100
proportion_entropy_cm <- (comb_entropy_mtx / max_entropy_mtx_comb) * 100

my_colors2 <- colorRamp2(set_breaks, colorRampPalette(brewer.pal(5, "RdYlBu"))(length(set_breaks)))

entropy_proportion_eb <- Heatmap(proportion_entropy_eb, 
        name = "Entropy",  
        col = my_colors2, 
        cluster_rows = F, 
        cluster_columns = FALSE,
        show_heatmap_legend = T, 
        heatmap_legend_param = list(
          legend_direction = "horizontal",
          at = set_breaks, 
          labels = set_breaks),
        row_dend_side = "right", 
        column_names_rot = 90, 
        row_names_side = "left",  # Ensure row labels are on the left
        column_names_side = "bottom",  # Ensure column labels are on the top
        row_title = "Serovar",  # Add row title "True"
        column_title = "Label",  # Add column title "Predicted"
        column_title_side = "bottom",
        row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
        column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
        row_names_gp = gpar(fontsize = 15))  # Row label font size)

entropy_proportion_fn <- Heatmap(proportion_entropy_fn, 
                                 name = "Entropy",  
                                 col = my_colors2, 
                                 cluster_rows = F, 
                                 cluster_columns = FALSE,
                                 show_heatmap_legend = T, 
                                 heatmap_legend_param = list(
                                   legend_direction = "horizontal",
                                   at = set_breaks, 
                                   labels = set_breaks),
                                 row_dend_side = "right", 
                                 column_names_rot = 90, 
                                 row_names_side = "left",  # Ensure row labels are on the left
                                 column_names_side = "bottom",  # Ensure column labels are on the top
                                 row_title = "Serovar",  # Add row title "True"
                                 column_title = "Label",  # Add column title "Predicted"
                                 column_title_side = "bottom",
                                 row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                                 column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                                 row_names_gp = gpar(fontsize = 15))  # Row label font size)

entropy_proportion_cm <- Heatmap(proportion_entropy_cm, 
                                 name = "Entropy",  
                                 col = my_colors2, 
                                 cluster_rows = F, 
                                 cluster_columns = FALSE,
                                 show_heatmap_legend = T, 
                                 heatmap_legend_param = list(
                                   legend_direction = "horizontal",
                                   at = set_breaks, 
                                   labels = set_breaks),
                                 row_dend_side = "right", 
                                 column_names_rot = 90, 
                                 row_names_side = "left",  # Ensure row labels are on the left
                                 column_names_side = "bottom",  # Ensure column labels are on the top
                                 row_title = "Serovar",  # Add row title "True"
                                 column_title = "Label",  # Add column title "Predicted"
                                 column_title_side = "bottom",
                                 row_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style row title
                                 column_title_gp = gpar(fontsize = 15, fontface = "bold", col = "black"),  # Style column title
                                 row_names_gp = gpar(fontsize = 15))  # Row label font size)

entropy_heatmaps <- entropy_proportion_eb + entropy_proportion_fn + entropy_proportion_cm 
draw(entropy_heatmaps)

###### Scatter plot ###########
eb_pred_label_df <- eb_pred_label_mx %>%
  as.data.frame() 
eb_pred_label_df$Serovar <- rownames(eb_pred_label_mx) 
eb_pred_label_df <- eb_pred_label_df %>%
  pivot_longer(cols = -Serovar, names_to = "Pred_label", values_to = "num_pred") %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal" ~ "Animal type",
    Pred_label == "AnimalPlus" ~ "Location or Food",
    TRUE ~ Pred_label  # Keep other column names unchanged
  )) %>%
  mutate(dataset = "Enterobase")

fn_pred_label_df <- fn_pred_label_mx %>%
  as.data.frame() 
fn_pred_label_df$Serovar <- rownames(fn_pred_label_mx) 
fn_pred_label_df <- fn_pred_label_df %>%
  pivot_longer(cols = -Serovar, names_to = "Pred_label", values_to = "num_pred") %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal" ~ "Animal type",
    Pred_label == "AnimalPlus" ~ "Location or Food",
    TRUE ~ Pred_label  # Keep other column names unchanged
  )) %>%
  mutate(dataset = "FoodNet")

comb_pred_label_df <- comb_pred_label_mx %>%
  as.data.frame() 
comb_pred_label_df$Serovar <- rownames(comb_pred_label_mx) 
comb_pred_label_df <- comb_pred_label_df %>%
  pivot_longer(cols = -Serovar, names_to = "Pred_label", values_to = "num_pred") %>%
  mutate(Pred_label = case_when(
    Pred_label == "Animal" ~ "Animal type",
    Pred_label == "AnimalPlus" ~ "Location or Food",
    TRUE ~ Pred_label  # Keep other column names unchanged
  )) %>%
  mutate(dataset = "Combined")

pred_label_df <- rbind(eb_pred_label_df, fn_pred_label_df, comb_pred_label_df)

pred_f1_df <- flat_results_all %>%
  left_join(pred_label_df, by = c("Serovar", "Pred_label", "dataset"))

dataset_spearman <- pred_f1_df %>%
  group_by(dataset) %>%
  summarise(
    spearman_cor = cor(num_pred, F1, method = "spearman"),
    p_value = cor.test(num_pred, F1, method = "spearman")$p.value
  ) %>%
  ungroup()

full_data_spearman <- cor.test(pred_f1_df$num_pred, pred_f1_df$F1, method = "spearman")

ggplot(pred_f1_df, aes(x = num_pred, y = F1)) +
  geom_point(aes(color = dataset), alpha = 0.7) +  # Scatter plot with dataset colors
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # LOESS curve for all data
  labs(x = "Predicted label (n)", y = "F1 Score") +
  theme_bw() +
  scale_color_discrete(name = "Dataset")

########## PCA #############
heid_pca <- fn_profile %>%
  select(-Key, -SourceSite)
heid_pca <- heid_pca %>%
  filter(Serovar == "Heidelberg")

features <- heid_pca[, 6:ncol(heid_pca)]
features_clean <- features %>% select_if(~ var(.) != 0)

# Step 3: Run PCA on the features (centering and scaling the data)
pca_result <- prcomp(features_clean, center = TRUE, scale. = TRUE)

# Step 4: Create a data frame of PCA results and include the "label" column for coloring
pca_data <- as.data.frame(pca_result$x)  # Extract the principal components
pca_data$SourceState <- heid_pca$SourceState # Add the "label" column for coloring
pca_data$CombinedSites <- heid_pca$CombinedSites
pca_data$CombinedSite2 <- heid_pca$CombinedSite2
pca_data <- pca_data %>%
  filter(SourceState %in% c("AB", "BC", "ON", "QC"))

# Calculate the percentage of variance explained by each principal component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Extract the percentage for PC1 and PC2
pc1_var <- round(explained_variance[1], 2)
pc2_var <- round(explained_variance[2], 2)

# Step 5: Plot the PCA with ggplot2, coloring by the "label" column
ggplot(pca_data, aes(x = PC1, y = PC2, color = CombinedSites)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "PCA of Heidelberg Samples",
    x = paste0("Principal Component 1 (", pc1_var, "%)"),
    y = paste0("Principal Component 2 (", pc2_var, "%)")
  ) +
  theme_minimal() +
  scale_color_discrete(name = "Source State")

