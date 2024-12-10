library(tidyverse)
library(stringr)

setwd("C:/Users/user5012100/Downloads/")

source("C:/Users/user5012100/Desktop/R/Functions_20240507.R")

ebase_meta <- read.csv("Ebase_cad_all_metadata.csv")
ebase_cgmlst <- read.csv("Ebase_cad_all_allele.csv")
enterobase_df <- read.csv("../Desktop/Data/Models_Outputs/enterobase_sistr_combined_20240201.csv")

# Extracting column names for comparison
# columns_ebase_all: from ebase_cgmlst, ignoring first two columns (Name, ST presumably)
columns_ebase_all <- colnames(ebase_cgmlst)[3:ncol(ebase_cgmlst)]
# columns_ebase_prov: a large range from enterobase_df. Consider verifying these indices carefully.
columns_ebase_prov <- colnames(enterobase_df)[54:3056]

# Checking differences in column sets to align or diagnose mismatches
setdiff(columns_ebase_all, columns_ebase_prov)
setdiff(columns_ebase_prov, columns_ebase_all)

# Joining ebase_meta and ebase_cgmlst by Name and ST. Ensure these form a unique key or expect duplicates.
ebase_all <- left_join(ebase_meta, ebase_cgmlst, by = c("Name", "ST"))

# Removing duplicates based on a set of key columns
ebase_all <- ebase_all %>%
  distinct(Uberstrain, Name, Barcode, ST, .keep_all = TRUE)

# Filter rows that have non-empty Source fields.
ebase_source <- ebase_all %>%
  filter(Source.Niche != "" & Source.Type != "")

# Applying multiple filters to remove human, lab, ND and certain "food"/"wild animal" entries.
# Converting source fields to lowercase for consistency.
ebase_clean <- ebase_source %>%
  filter(!Source.Niche %in% c("Human", "Laboratory", "ND"))  %>%
  filter(Source.Niche != "food" & Source.Type != "nd/others" & Source.Details != "food") %>%
  filter(Source.Niche != "food" & Source.Type != "food" & Source.Details != "food") %>%
  filter(Source.Niche != "wild animal" & Source.Type != "nd/others" & Source.Details != "animal") %>%
  mutate(Source.Niche = tolower(Source.Niche),
         Source.Type = tolower(Source.Type),
         Source.Details = tolower(Source.Details))

# Extract distinct source combinations for exploration
unique_host <- ebase_clean %>%
  select(Source.Niche, Source.Type, Source.Details) %>%
  distinct()

# Counting occurrences of each (Niche, Type, Details) combination to identify frequent categories
unique_host_n <- ebase_clean %>%
  count(Source.Niche, Source.Type, Source.Details) %>%
  arrange(desc(n)) %>%
  mutate(percent = n / sum(n)) %>%
  mutate(cum_percent = cumsum(percent))

# Classification into CombinedSites and CombinedSite2:
# This is a critical complex mapping. The commented-out lines are attempts or alternative mappings.
# Adding comments to explain logic in case_when:
# - The logic tries to map certain patterns in Source.Niche/Source.Type/Source.Details to standardized categories like "Chicken", "Beef", "Pork", "Water", etc.
ebase_host <- ebase_clean %>%
  mutate(CombinedSites = case_when(
    # Mapping poultry sources with mention of chicken or broiler to "Chicken"
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "chicken|broiler") ~ "Chicken",
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      str_detect(Source.Details, "gallus gallus domesticus") ~ "Chicken", 
    Source.Niche == "poultry" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken",
    Source.Niche == "environment" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken",
    # Turkey detection from food/avian combos
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "turkey") ~ "Turkey",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "chicken") ~ "Turkey", # This line might cause confusion: any "chicken" detail leads to Turkey?
    # If Source.Type == "bovine", then "Beef"
    Source.Niche != "animal feed" & Source.Type == "swine" ~ "Pork",
    Source.Type == "bovine" ~ "Beef",
    Source.Niche == "environment" & Source.Type == "water/river" ~ "Water",
    TRUE ~ NA_character_
  )) %>%
  mutate(CombinedSite2 = case_when(
    # Secondary classification focusing on more specific conditions:
    # Chicken_food if mention of bread/burger/breast/thigh/nuggets
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      str_detect(Source.Details, "bread|burger|breast|thigh|nuggets") ~ "Chicken_food",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "turkey") ~ "Turkey_food",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "chicken") ~ "Chicken_food",
    # Animal vs. food differentiation for poultry/swine/beef is done by searching for "animal|cecal|feces" or known terms
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "chicken") &
      str_detect(Source.Details, "animal|cecal|feces") ~ "Chicken_animal", 
    Source.Niche == "environment" & Source.Type == "water/river" & Source.Details == "water" ~ "Water",
    # Pig_animal or Pig_food depending on Niche and Type patterns
    Source.Niche == "livestock" & Source.Type == "swine" & 
      (str_detect(Source.Details, "sus scrofa domesticus") &
         str_detect(Source.Details, "feces|intestine|rectal")) ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "swine" & str_detect(Source.Details, "cecal|ear|feces") ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "swine" ~ "Pig_animal",
    Source.Niche == "food" & Source.Type == "swine" ~ "Pig_food",
    # Similar logic for Beef_animal, Beef_food, etc.
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "beef") ~ "Beef_food",
    Source.Niche == "livestock" & Source.Type == "bovine" & 
      (str_detect(Source.Details, "feces|bovine|cattle|bos taurus|ox")) ~ "Beef_animal",
    # Additional rules...
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      (Source.Details == "chicken" | str_detect(Source.Details, "chicken|broiler")) ~ "Chicken_animal",
    Source.Niche == "environment" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken_animal",
    Source.Niche == "poultry" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken_food",
    TRUE ~ NA_character_
  )) %>% 
  relocate(CombinedSites, CombinedSite2, .after = c(Source.Details))

# Checking which rows did not get CombinedSites or CombinedSite2 assigned
unique_host_combsites1 <- ebase_host %>%
  filter(is.na(CombinedSites)) %>%
  count(Source.Niche, Source.Type, Source.Details) %>%
  arrange(desc(n))

unique_host_combsites2 <- ebase_host %>%
  filter(is.na(CombinedSite2)) %>%
  count(Source.Niche, Source.Type, Source.Details) %>%
  arrange(desc(n))

# Keep only rows that have at least one combined site classification
ebase_host_v2 <- ebase_host %>%
  filter(!is.na(CombinedSites) | !is.na(CombinedSite2))

unique_host_combsites2_NA <- ebase_host_v2 %>%
  filter(is.na(CombinedSite2)) %>%
  count(Source.Niche, Source.Type, Source.Details, CombinedSites) %>%
  arrange(desc(n))

# Summarizing counts for CombinedSites and CombinedSite2
combsite1 <- ebase_host_v2 %>%
  group_by(CombinedSites) %>%
  summarise(count_site1 = n(), .groups = "drop")  

combsite2 <- ebase_host_v2 %>%
  group_by(CombinedSites, CombinedSite2) %>%
  summarise(count_site2 = n(), .groups = "drop") %>%
  group_by(CombinedSites) %>%
  mutate(total_count = sum(count_site2)) %>%
  ungroup() %>%
  mutate(proportion = count_site2 / total_count)

ggplot(combsite1, aes(x = CombinedSites, y = count_site1, fill = CombinedSites)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "CombinedSites", y = "Count", fill = "Animal") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

ggplot(combsite2, aes(x = CombinedSites, y = count_site2, fill = CombinedSite2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "CombinedSites", y = "Count", fill = "Animal") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

ggplot(combsite2, aes(x = CombinedSites, y = proportion, fill = CombinedSite2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "CombinedSites", y = "Relative Proportion", fill = "Animal") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

ebase_host %>%
  summarise(unique_serovars = n_distinct(Serovar))

# Splitting a complex column into Accession and Rest
ebase_source_accession <- ebase_host_v2 %>%
  separate(col = Data.Source.Accession.No..Sequencing.Platform.Sequencing.Library.Insert.Size.Experiment.Bases.Average.Length.Status., 
           into = c("Accession", "Rest"), sep = ";", extra = "merge")

ebase_accession <- ebase_source_accession %>%
  select(Uberstrain, Accession)

# Reading additional metadata and SISTR outputs
df_accessions_source <- read.csv("../Desktop/R/accessions_ebase_source.csv")
sistr_source <- read.csv("../Desktop/R/combined_sistr_output.csv")

df1 <- df_accessions_source %>% select(assembly, run)
df2 <- df_accessions_source %>% select(Uberstrain, Accession)

# Merging run IDs and fixing a special case where Accession == "GCF_000623435"
accessions <- df2 %>% 
  left_join(df1, by = c("Accession" = "assembly")) %>%
  mutate(run = coalesce(run, Accession)) %>%
  # Document why this ifelse is needed:
  # Possibly a known exception from domain knowledge.
  mutate(run = ifelse(Accession == "GCF_000623435", "SRR1183795", run))

# Filtering SISTR data to PASS QC and known matches
sistr_ebase_source <- accessions %>%
  left_join(sistr_source, by = c("run" = "genome")) %>%
  filter(!is.na(cgmlst_genome_match)) %>%
  filter(qc_status == "PASS")

sistr_ebase_source <- sistr_ebase_source %>% 
  select(Uberstrain, Accession, run, serovar_cgmlst)

ebase_source_final <- sistr_ebase_source %>%
  left_join(ebase_source_accession, by =  c("Uberstrain", "Accession"))

ebase_source_final <- ebase_source_final %>%
  select(-c(3, 5:10, 13:52))  # Consider using column names to avoid brittle indexing.

ebase_source_final <- ebase_source_final %>%
  mutate(CombinedSites = case_when(
    CombinedSites == "Turkey" & CombinedSite2 == "Chicken_food" ~ "Chicken",
    TRUE ~ CombinedSites))

# write.csv(ebase_source_final, "Ebase_source_final_20240702.csv", row.names = F) # Export if needed

metadata_ebase_source <- ebase_source_final %>%
  select(1:5) # Consider using column names here too.

# Reading wgMLST data and metadata, separating complex column again
ebase_wgmlst_profile <- read.csv("ebase_all_wgmlst_20240705.csv")
ebase_wgmlst_metadata <- read.csv("ebase_wgmlst_meta_20240705.csv")
ebase_wgmlst_metadata <- ebase_wgmlst_metadata %>%
  separate(col = Data.Source.Accession.No..Sequencing.Platform.Sequencing.Library.Insert.Size.Experiment.Bases.Average.Length.Status., 
           into = c("Accession", "Rest"), sep = ";", extra = "merge") 
ebase_wgmlst_metadata <- ebase_wgmlst_metadata %>%
  select(Uberstrain, Accession, Name, Source.Niche, Source.Type, Source.Details, Region, ST)

ebase_wgmlst_full <- left_join(ebase_wgmlst_metadata, ebase_wgmlst_profile, by = c("Name", "ST"))

source_accession_wgmlst <- left_join(metadata_ebase_source, ebase_host_v2, by = c("Uberstrain", "CombinedSites", "CombinedSite2"))
# Using positions again, ensure columns haven't changed
source_accession_wgmlst <- source_accession_wgmlst %>%
  select(Uberstrain, Accession, Name, ST, everything()) %>%
  select(1:6, ST) # Keep cautious with indexing. Try to identify column to avoid confusion.

source_accession_wgmlst$ST <- as.numeric(source_accession_wgmlst$ST)

ebase_host_wgmlst <- source_accession_wgmlst %>%
  left_join(ebase_wgmlst_full, by = c("Uberstrain", "Accession", "Name")) %>%
  # rows_patch operations indicate complex matching logic.
  # Ensure that these matches are correct and documented.
  rows_patch(
    source_accession_wgmlst %>%
      left_join(ebase_wgmlst_full, by = c("Uberstrain" = "Uberstrain", "Accession" = "Accession")) %>%
      select(-Name.y, -Name.x),
    by = c("Uberstrain", "Accession")
  ) %>%
  rows_patch(
    source_accession_wgmlst %>%
      left_join(ebase_wgmlst_full, by = c("Uberstrain" = "Uberstrain", "Name" = "Name")) %>%
      select(-Accession.y, -Accession.x),
    by = c("Uberstrain", "Name")
  )

ebase_host_wgmlst <- ebase_host_wgmlst %>%
  select(-Source.Niche, -Source.Type, -Source.Details, -Name, -ST.x, -ST.y, -Region)

# write.csv(ebase_host_wgmlst, "ebase_host_full_wgmlst_20240708.csv", row.names = F)

#########################################
### Global Enterobase samples ########################
# Reading USA and non-USA enterobase profiles and combining them
usa_enterobase_profile <- read.csv("usa_ebase_20240909", sep = "\t")
nonusa_enterobase_profile <- read.csv("nonusa_ebase_20240909", sep = "\t")

enterobase_profile_all <- rbind(usa_enterobase_profile, nonusa_enterobase_profile)

enterobase_profile_all <- enterobase_profile_all %>%
  separate(col = Data.Source.Accession.No..Sequencing.Platform.Sequencing.Library.Insert.Size.Experiment.Bases.Average.Length.Status., 
           into = c("Accession", "Rest"), sep = ";", extra = "merge") 
enterobase_profile_all_filtered <- enterobase_profile_all %>%
  filter(Source.Type != "Human") %>%
  filter(Source.Type %in% c("Swine", "Bovine", "Poultry", "Avian", "Animal Feed", "Dairy", "Water/River"))

enterobase_profile_all_clean <- enterobase_profile_all_filtered %>%
  mutate(Source.Type =  str_to_lower(str_squish(Source.Type)),
         Source.Niche = str_to_lower(str_squish(Source.Niche)),
         Source.Details = str_to_lower(str_squish(Source.Details))) %>%
  mutate(CombinedSites = case_when(
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "chicken|broiler") ~ "Chicken",
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      str_detect(Source.Details, "gallus gallus domesticus") ~ "Chicken", 
    Source.Niche == "poultry" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken",
    Source.Niche == "environment" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "turkey") ~ "Turkey",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "chicken") ~ "Chicken",
    Source.Type == "dairy" ~ "Dairy",
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "chicken") &
      str_detect(Source.Details, "animal|cecal|feces|nuggets") ~ "Chicken", 
    Source.Niche != "animal feed" & Source.Type == "swine" ~ "Pork",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "turkey") ~ "Turkey",
    Source.Type == "poultry" & str_detect(Source.Details, "(ground.*turkey|turkey.*ground)") ~ "Turkey",
    str_detect(Source.Details, "(animal.*turkey|turkey.*animal)") ~ "Turkey",
    str_detect(Source.Details, "hen egg") ~ "Chicken",
    str_detect(Source.Details, "turkey fluff") ~ "Turkey",
    Source.Type == "bovine" ~ "Beef",
    Source.Niche == "environment" & Source.Type == "water/river" ~ "Water",
    TRUE ~ NA_character_
  )) %>%
  mutate(CombinedSite2 = case_when(
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      str_detect(Source.Details, "turkey") & str_detect(Source.Details, "bread|burger|breast|thigh|nuggets") ~ "Turkey_food",
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      str_detect(Source.Details, "bread|burger|breast|thigh|nuggets") ~ "Chicken_food",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "turkey") ~ "Turkey_food",
    Source.Type == "poultry" & str_detect(Source.Details, "(ground.*turkey|turkey.*ground)") ~ "Turkey_food",
    str_detect(Source.Details, "(animal.*turkey|turkey.*animal)") ~ "Turkey_animal",
    Source.Niche == "food" & Source.Type == "avian" & str_detect(Source.Details, "chicken") ~ "Chicken_food",
    str_detect(Source.Details, "hen egg") ~ "Chicken_food",
    str_detect(Source.Details, "turkey fluff") ~ "Turkey_animal",
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "chicken") &
      str_detect(Source.Details, "animal|cecal|feces") ~ "Chicken_animal", 
    Source.Niche == "environment" & Source.Type == "water/river" & Source.Details == "water" ~ "Water",
    Source.Niche == "livestock" & Source.Type == "swine" & str_detect(Source.Details, "sus scrofa domesticus") &
      str_detect(Source.Details, "feces|intestine|rectal") ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "swine" & str_detect(Source.Details, "cecal|ear") ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "swine" & str_detect(Source.Details, "feces") ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "swine" ~ "Pig_animal",
    Source.Niche == "food" & Source.Type == "swine" ~ "Pig_food",
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "beef") ~ "Beef_food",
    Source.Niche == "livestock" & Source.Type == "swine" & str_detect(Source.Details, "porcine|pig") ~ "Pig_animal",
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "feces") ~ "Beef_animal",
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "bovine|cattle") ~ "Beef_animal",
    str_detect(Source.Details, "beef food product") ~ "Beef_food",
    str_detect(Source.Details, "animal-calf-bob veal (cecal)") ~ "Beef_animal",
    Source.Niche == "food" & Source.Type == "dairy" & str_detect(Source.Details, "milk|cheese|butter|cream") ~ "Dairy_food",
    Source.Niche == "food" & Source.Type == "dairy" & !str_detect(Source.Details, "milk|cheese|butter|cream") ~ "Dairy_animal",
    Source.Niche == "environment" & Source.Type == "dairy" ~ "Dairy_animal", 
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "bos taurus") &
      str_detect(Source.Details, "feces|liver|animal|lung|stool|intestine|lymph|blood|heart|necropsy|intestine|organ|naval|placenta") ~ "Beef_animal",
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "ox") ~ "Beef_animal",
    Source.Niche == "livestock" & Source.Type == "bovine" & Source.Details == "bos taurus" ~ "Beef_animal",
    Source.Niche == "environment" & Source.Type == "water/river" ~ "Water",
    Source.Niche == "poultry" & Source.Type == "poultry" & 
      (Source.Details == "chicken" |
         str_detect(Source.Details, "chicken|broiler")) ~ "Chicken_animal",
    Source.Niche == "poultry" & Source.Type == "poultry" & str_detect(Source.Details, "gallus gallus domesticus") &
      str_detect(Source.Details, "spleen|heart|yolk|bootie|liver|lung|fluff|jejunum|cecum|ovary|environmental") ~ "Chicken_animal",
    Source.Niche == "environment" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken_animal",
    Source.Niche == "poultry" & Source.Type == "animal-related" & str_detect(Source.Details, "chicken") ~ "Chicken_food",
    Source.Niche == "livestock" & Source.Type == "bovine" & str_detect(Source.Details, "intestine") ~ "Beef_animal",
    TRUE ~ NA_character_
  )) %>%
  relocate(CombinedSites, .after = Source.Details) %>%
  relocate(CombinedSite2, .after = CombinedSites) %>%
  filter(!is.na(CombinedSites))

enterobase_profile_all_clean <- enterobase_profile_all_clean %>%
  filter(!is.na(ST)) 

# Check for missing labels if needed
missing_labels <- enterobase_profile_all_clean %>%
  filter(is.na(CombinedSites)) %>%
  select(Source.Niche, Source.Type, Source.Details, CombinedSites, CombinedSite2)

# Summarize country distribution
enterobase_profile_all_clean %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 10)

global_country <- enterobase_profile_all_clean %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  mutate(prop = (n / sum(n)) * 100) %>%
  arrange(desc(n))

top_10_countries <- global_country %>%
  top_n(10, n) %>%
  pull(Country)

global_country_collapsed <- global_country %>%
  mutate(Country = if_else(Country %in% top_10_countries, Country, "Other")) %>%
  group_by(Country) %>%
  summarise(n = sum(n), prop = sum(prop)) %>%
  arrange(desc(n))

# Matching accessions with global enterobase
enterobase_df_source <- read.csv("../Downloads/Ebase_source_final_20240702.csv")
locidex_matching_accessions <- readLines("match_global_accession.txt")
enterobase_cad_host_accession <- enterobase_df_source$Key
combined_global_accessions <- c(locidex_matching_accessions, enterobase_cad_host_accession)
unique_combined_accessions <- unique(combined_global_accessions)

matches_in_global_enterobase <- enterobase_profile_all_clean %>%
  filter(Accession %in% unique_combined_accessions)

matches_in_global_enterobase %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 10)

canada_accessions_test <- matches_in_global_enterobase %>%
  filter(Country == "Canada")

canada_enterobase_profile_hiercc <- read.csv("enterobase_canada_hiercc.csv")
canada_enterobase_profile_hiercc <- canada_enterobase_profile_hiercc %>%
  separate(col = Data.Source.Accession.No..Sequencing.Platform.Sequencing.Library.Insert.Size.Experiment.Bases.Average.Length.Status., 
           into = c("Accession", "Rest"), sep = ";", extra = "merge") 

anti_join_canada <- anti_join(enterobase_df_source, canada_accessions_test, by = "Accession")
anti_join_canada2 <- anti_join(canada_accessions_test, enterobase_df_source, by = c("Accession"))

cad_meta_add <- canada_enterobase_profile_hiercc %>%
  filter(Accession %in% anti_join_canada$Accession)

setdiff_result <- setdiff(anti_join_canada$Accession, cad_meta_add$Accession)
df1 <- enterobase_df_source %>%
  filter(Accession %in% setdiff_result)

strain_match <- df1$Uberstrain

# Adjusting Accession based on Uberstrain matches, special case handling:
canada_enterobase_profile_hiercc <- canada_enterobase_profile_hiercc %>%
  mutate(Accession = if_else(Uberstrain %in% strain_match, 
                             df1$Accession[match(Uberstrain, df1$Uberstrain)],
                             Accession))

cad_meta_add <- canada_enterobase_profile_hiercc %>%
  filter(Accession %in% anti_join_canada$Accession) %>%
  mutate(CombinedSites = NA,
         CombinedSite2 = NA)

matches_in_global_enterobase_clean <- rbind(matches_in_global_enterobase, cad_meta_add) %>%
  filter(!Accession %in% anti_join_canada2$Accession)

matches_in_global_enterobase_clean <- matches_in_global_enterobase_clean %>%
  left_join(enterobase_df_source %>% select(Accession, CombinedSites, CombinedSite2), 
            by = "Accession", suffix = c("", ".Source")) %>%
  mutate(
    CombinedSites = if_else(is.na(CombinedSites), CombinedSites.Source, CombinedSites),
    CombinedSite2 = if_else(is.na(CombinedSite2), CombinedSite2.Source, CombinedSite2)
  ) %>%
  select(-CombinedSites.Source, -CombinedSite2.Source)

check_na <- matches_in_global_enterobase_clean %>%
  filter(is.na(CombinedSites) | is.na(CombinedSite2))

matches_in_global_enterobase_clean <- matches_in_global_enterobase_clean %>% 
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites)) %>%
  filter(!is.na(CombinedSite2)) 

# Repeat top country logic for this filtered set
top_countries <- matches_in_global_enterobase_clean %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  pull(Country)

country_sample_counts <- matches_in_global_enterobase_clean %>%
  mutate(Country = if_else(Country %in% top_countries, Country, "Others")) %>%
  group_by(Country) %>%
  summarise(n = n())

global_prop_host1 <- matches_in_global_enterobase_clean %>%
  mutate(Country = if_else(Country %in% top_countries, Country, "Others")) %>%
  group_by(Country, CombinedSites) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  left_join(country_sample_counts, by = "Country") %>%
  mutate(Country_with_count = paste0(Country, "\n(n = ", n, ")"))

global_prop_host2 <- matches_in_global_enterobase_clean %>%
  mutate(Country = if_else(Country %in% top_countries, Country, "Others")) %>%
  group_by(Country, CombinedSite2) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  left_join(country_sample_counts, by = "Country") %>%
  mutate(Country_with_count = paste0(Country, "\n(n = ", n, ")"))

global_plot_animal <- ggplot(global_prop_host1, aes(x = Country_with_count, y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Proportion (%)", fill = "Animal") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  coord_flip() +
  theme_bw()

global_plot_animalPlus <- ggplot(global_prop_host2, aes(x = Country_with_count, y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Proportion (%)", fill = "AnimalPlus") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  coord_flip() +
  theme_bw()

ggarrange(global_plot_animal, global_plot_animalPlus, align = "hv", labels = "AUTO")

######### Serovar info from NCBI ##############
global_salmonella_serovar <- read.csv("PDG000000002.3214.metadata.tsv", sep = '\t')

salmonella_filtered <- global_salmonella_serovar %>%
  filter(serovar != "NULL") %>%
  select(IFSAC_category, Run, asm_acc, attribute_package, bioproject_acc, bioproject_center,
         biosample_acc, collected_by, collection_date, epi_type, geo_loc_name, isolation_source,
         scientific_name, serovar)

lookup_df <- data.frame(
  GCF_GCA = c("GCF_000625155", "GCF_000625555", "GCF_000624535", "GCF_000624335", 
              "GCA_000625755", "GCF_000623855", "GCF_000623875", "GCF_000623995", 
              "GCF_000624235", "GCF_000624035", "GCF_000624055", "GCF_000625735", 
              "GCF_001692575", "GCF_000625115", "GCF_000625395", "GCF_000625715", 
              "GCF_000624355", "GCF_000624295", "GCF_000626535", "GCF_000623555", 
              "GCF_000623475", "GCF_000623435", "GCF_000623615", "GCF_000626495", 
              "GCF_000625695", "GCF_001690115", "GCA_000625535", "GCA_000623135", 
              "GCA_000623095"),
  SRR = c("SRR1183914", "SRR1183904", "SRR1183899", "SRR1183864", "SRR1183856",
          "SRR1183811", "SRR1183809", "SRR1183807", "SRR1183804", "SRR1183801",
          "SRR1183788", "SRR1183778", "SRR3685561", "SRR1183911", "SRR1183882",
          "SRR1183881", "SRR1183869", "SRR1183836", "SRR1183802", "SRR1183797",
          "SRR1183796", "SRR1183795", "SRR1183784", "SRR1183775", "SRR1183773",
          "SRR3707447", "SRR1183831", "SRR1183894", "SRR1183892")
)

matches_in_global_enterobase_clean2 <- matches_in_global_enterobase_clean %>%
  left_join(lookup_df, by = c("Accession" = "GCF_GCA")) %>%
  mutate(Accession = if_else(!is.na(SRR), SRR, Accession)) %>%
  select(-SRR)

anti_join_ncbi <- anti_join(matches_in_global_enterobase_clean2, salmonella_filtered, by = c("Accession" = "Run"))
anti_join_ncbi2 <- anti_join(salmonella_filtered, matches_in_global_enterobase_clean2, by = c("Run" = "Accession"))

global_country <- matches_in_global_enterobase_clean2 %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  mutate(prop = (n / sum(n)) * 100) %>%
  arrange(desc(n))

top_10_countries <- global_country %>%
  top_n(10, n) %>%
  pull(Country)

global_country_collapsed <- global_country %>%
  mutate(Country = if_else(Country %in% top_10_countries, Country, "Other")) %>%
  group_by(Country) %>%
  summarise(n = sum(n), prop = sum(prop)) %>%
  arrange(desc(n))

global_animal <- matches_in_global_enterobase_clean2 %>%
  count(CombinedSites) %>% 
  mutate(prop = (n / sum(n)) * 100) 

global_animalPlus <- matches_in_global_enterobase_clean2 %>%
  count(CombinedSite2) %>% 
  mutate(prop = (n / sum(n)) * 100) 

global_country_plot <- ggplot(global_country_collapsed, aes(x = "", y = prop, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  labs(y = "Proportion", x = "Country") +
  theme_bw()

global_animal_plot <- ggplot(global_animal, aes(x = "", y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity", width = 1) +
  labs(y = "Proportion", x = "Animal", fill = "Animal") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  theme_bw()

global_animalPlus_plot <- ggplot(global_animalPlus, aes(x = "", y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity", width = 1) +
  labs(y = "Proportion", x = "AnimalPlus", fill = "AnimalPlus") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  theme_bw()

ggarrange(global_country_plot, global_animal_plot, global_animalPlus_plot, 
          align = 'hv', labels = "AUTO", ncol = 3, nrow = 1)

# writeLines(anti_join_ncbi$Accession, "accession_download.txt")

################ Hier CC exploration ###################
# The following plots explore distributions of hierarchical clustering IDs.

global_long <- matches_in_global_enterobase_clean2 %>%
  pivot_longer(cols = 38:42, names_to = "Cluster", values_to = "Value")

ggplot(global_long, aes(x = Value, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Cluster") +
  theme_bw()

hc0_plot <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC0..indistinguishable., fill = CombinedSites)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc2_plot <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC2, fill = CombinedSites)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc5_plot <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC5, fill = CombinedSites)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc10_plot <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC10, fill = CombinedSites)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc20_plot <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC20, fill = CombinedSites)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSites, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggarrange(hc0_plot, hc2_plot, hc5_plot, hc10_plot, hc20_plot, align = "hv",
          labels = c("HC0", "HC2", "HC5", "HC10", "HC20"), common.legend = T, legend = "bottom")

hc0_plot_animalPlus <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC0..indistinguishable., fill = CombinedSite2)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSite2, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc2_plot_animalPlus <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC2, fill = CombinedSite2)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSite2, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc5_plot_animalPlus <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC5, fill = CombinedSite2)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSite2, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc10_plot_animalPlus <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC10, fill = CombinedSite2)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSite2, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

hc20_plot_animalPlus <- ggplot(matches_in_global_enterobase_clean2, aes(x = HC20, fill = CombinedSite2)) +
  geom_density(alpha = 0.5) +  # Add density plot with some transparency
  facet_wrap(~CombinedSite2, scales = "free_y") + 
  labs(x = "Cluster ID", y = "Density", fill = "Animal") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggarrange(hc0_plot_animalPlus, hc2_plot_animalPlus, hc5_plot_animalPlus, hc10_plot_animalPlus, hc20_plot_animalPlus, align = "hv",
          labels = c("HC0", "HC2", "HC5", "HC10", "HC20"), common.legend = T, legend = "bottom")


ggarrange(hc0_plot_animalPlus, hc2_plot_animalPlus, hc5_plot_animalPlus, hc10_plot_animalPlus, hc20_plot_animalPlus, align = "hv",
          labels = c("HC0", "HC2", "HC5", "HC10", "HC20"), common.legend = T, legend = "bottom")

#### Generating datasets for ML models ###
global_nzv_df <- read.csv("../Downloads/global_nzv.csv")
fn_nzv_df <- read.csv("../Downloads/fn_nzv.csv")
global_fn_nzv_df <- read.csv("../Downloads/global_fn_nzv.csv")
ebase_host_profile <- read.csv("../Desktop/Data/locidex_out/ebase_host_locidex_LE_20240820.tsv", sep = "\t")
col_remove_ebase <- read.csv("../Desktop/Data/global_enterobase/all_nzv_cols.csv")

cols_to_remove <- col_remove_ebase[[2]]
ebase_host_profile <- ebase_host_profile %>%
  select(-all_of(cols_to_remove))

global_enterobase_meta <- matches_in_global_enterobase_clean2 %>%
  select(Accession, CombinedSites, CombinedSite2, Serovar)

global_enterobase <- global_nzv_df %>%
  left_join(global_enterobase_meta, by = c("sample_id" = "Accession")) %>%
  relocate(CombinedSites, .after = sample_id) %>%
  relocate(CombinedSite2, .after = CombinedSites) %>%
  relocate(Serovar, .after = CombinedSite2)

ebase_host_profile <- ebase_host_profile %>% 
  relocate(sample_id, .before = CombinedSites) %>%
  relocate(CombinedSite2, .after = CombinedSites) %>%
  relocate(serovar_cgmlst, .after = CombinedSite2) %>%
  rename(Serovar = serovar_cgmlst)

global_enterobase2 <- global_enterobase %>%
  filter(!is.na(CombinedSites))

fn_nzv_df2 <- fn_nzv_df %>%
  select(-SourceState, -SourceSite) %>%
  rename(sample_id = Key) %>%
  relocate(sample_id, .before = CombinedSites) %>%
  relocate(CombinedSite2, .after = CombinedSites) %>%
  relocate(Serovar, .after = CombinedSite2)

# global_fn_df_clean <- rbind(all_ebase, fn_nzv_df2) 

# sistr_global_accessions reading and merging again for final touches.
sistr_global_accessions <- read.csv("sistr_output_global_clean.csv")

sistr_global_serovars <- global_enterobase2 %>%
  left_join(sistr_global_accessions, by = c("sample_id" = "genome"))

sistr_global_serovars_pass <- sistr_global_serovars %>%
  filter(qc_status == "PASS") %>%
  select(-cgmlst_found_loci, -cgmlst_genome_match, -cgmlst_matching_alleles, -qc_status, -Serovar) %>%
  relocate(serovar_cgmlst, .after = "CombinedSite2") %>%
  rename(Serovar = serovar_cgmlst)

sistr_global_serovars_fail <- sistr_global_serovars %>%
  filter(qc_status != "PASS") %>%
  select(-cgmlst_found_loci, -cgmlst_genome_match, -cgmlst_matching_alleles, -qc_status, -Serovar) %>%
  relocate(serovar_cgmlst, .after = "CombinedSite2") %>%
  rename(Serovar = serovar_cgmlst)

global_enterobase_clean <- rbind(sistr_global_serovars_pass, ebase_host_profile) %>%
  mutate(sample_id = trimws(sample_id)) %>%
  distinct(sample_id, .keep_all = T)

fn_meta <- fn_nzv_df2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar)
global_meta <- global_enterobase_clean %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar)
all_meta <- bind_rows(fn_meta, global_meta)

global_fn_clean <- global_fn_nzv_df %>%
  left_join(all_meta, by = "sample_id") %>%
  mutate(sample_id = trimws(sample_id)) %>%
  distinct(sample_id, .keep_all = T) %>%
  relocate(CombinedSites, .after = sample_id) %>%
  relocate(CombinedSite2, .after = CombinedSites) %>%
  relocate(Serovar, .after = CombinedSite2) %>%
  filter(!is.na(Serovar))

# write.table(global_enterobase_clean, "global_nzv2.csv", row.names = F, sep = "\t")
# write.table(global_fn_clean, "global_fn_nzv2.csv", row.names = F, sep = "\t")
# write.table(fn_nzv_df2, "fn_nzv_clean.tsv", row.names = F, sep = "\t")

################## 
## PLOTS OF SEROVARS AND UPDATED DATASET 
####################

sistr_sample_ids <- sistr_global_serovars_pass %>%
  select(-CombinedSites, -CombinedSite2)
global_accession_postSISTR_meta <- matches_in_global_enterobase_clean2 %>%
  left_join(sistr_sample_ids, by = c("Accession" = "sample_id"))
global_accession_postSISTR_meta <- global_accession_postSISTR_meta %>%
  select(-Serovar.x) %>%
  relocate(Serovar.y, .after = "Longitude") %>%
  rename(Serovar = Serovar.y)
global_accession_postSISTR_meta_clean <- global_accession_postSISTR_meta %>%
  select(-Rest, -Barcode, -District, -City, -Post.Code, -Longitude, -Latitude, -Disease,
         -Subspecies, -Antigenic.Formulas, -Phage.Type, -Comment, -Differences) %>%
  filter(!is.na(Serovar))

ebase_source_meta <- ebase_source_accession %>%
  select(1:50) %>%
  select(-Serovar, -CombinedSites, -CombinedSite2)

ebase_source_meta2 <- ebase_source_meta %>%
  left_join(lookup_df, by = c("Accession" = "GCF_GCA")) %>%
  mutate(Accession = if_else(!is.na(SRR), SRR, Accession)) %>%
  select(-SRR)

ebase_host_meta <- ebase_host_profile %>%
  left_join(ebase_source_meta2, by = c("sample_id" = "Accession")) %>%
  filter(!is.na(Serovar))

ebase_host_meta2 <- ebase_host_meta %>%
  relocate(5808:last_col(), .before = CombinedSites) %>%
  rename(Accession = sample_id) %>%
  select(-Rest, -Barcode, -District, -City, -Post.Code, -Longitude, -Latitude, -Disease,
         -Subspecies, -Antigenic.Formulas, -Phage.Type, -Comment, -Differences)

global_enterobase_clean2 <- rbind(global_accession_postSISTR_meta_clean, ebase_host_meta2) %>%
  mutate(Accession = trimws(Accession)) %>%
  distinct(Accession, .keep_all = T)

serovar_list <- c('Typhimurium', 'Kentucky', 'Heidelberg', 'Infantis', 'Enteritidis', 'I 1,4,[5],12:i:-')

global_serovar_count <- global_enterobase_clean2 %>%
  group_by(Serovar) %>%
  summarise(n = n()) %>%
  mutate(prop = (n / sum(n)) * 100) %>%
  arrange(desc(n)) 

global_serovar_count2 <- global_serovar_count %>%
  mutate(Serovar = if_else(Serovar %in% serovar_list, Serovar, "Other")) %>%
  group_by(Serovar) %>%
  summarise(n = sum(n), prop = sum(prop)) %>%
  arrange(desc(n))

top_10_countries2 <- global_enterobase_clean2 %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%  # Select the top 10 countries
  pull(Country)    # Extract the country names

global_country_collapsed2 <- global_enterobase_clean2 %>%
  mutate(Country = if_else(Country %in% top_countries, Country, "Others")) %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  mutate(prop = (n / sum(n)) * 100) %>%
  arrange(desc(n)) 

count_country_serovar <- global_enterobase_clean2 %>%
  mutate(Country2 = ifelse(Country %in% top_10_countries2, Country, "Others"),
         Serovar2 = ifelse(Serovar %in% serovar_list, Serovar, "Others"))

df_country_serovar <- count_country_serovar %>%
  group_by(Country2, Serovar2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Country2) %>%
  mutate(proportion = n / sum(n)) %>%
  group_by(Country2) %>%
  mutate(Country_with_count = paste0(Country2, "\n(n = ", sum(n), ")")) %>%
  ungroup()

df_country_serovar_all <- count_country_serovar %>%
  group_by(Serovar2) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n)) %>%
  mutate(Country2 = "All", 
         Country_with_count = paste0("All", "\n(n = ", sum(n), ")"))

df_country_serovar_combined <- bind_rows(df_country_serovar, df_country_serovar_all)

ggplot(df_country_serovar_combined, aes(x = Country_with_count, y = proportion, fill = Serovar2)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  labs(
    x = "Country",
    y = "Proportion",
    fill = "Serovar"
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  coord_flip() + 
  theme_bw()

df_serovar_animal <- count_country_serovar %>%
  group_by(CombinedSites, Serovar2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Serovar2) %>%
  mutate(proportion = n / sum(n)) %>%
  group_by(Serovar2) %>%
  mutate(Serovar_with_count = paste0(Serovar2, "\n(n = ", sum(n), ")")) %>%
  ungroup()

df_serovar_animal_all <- count_country_serovar %>%
  group_by(CombinedSites) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n)) %>%
  mutate(Serovar2 = "All", 
         Serovar_with_count = paste0("All", "\n(n = ", sum(n), ")"))

df_serovar_animal_combined <- bind_rows(df_serovar_animal, df_serovar_animal_all)

global_animal_plot <- ggplot(df_serovar_animal_combined, aes(x = Serovar_with_count, y = proportion, fill = CombinedSites)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  labs(
    x = "Serovar",
    y = "Proportion",
    fill = "Animal"
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  coord_flip() + 
  theme_bw()

df_serovar_animalPlus <- count_country_serovar %>%
  group_by(CombinedSite2, Serovar2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Serovar2) %>%
  mutate(proportion = n / sum(n)) %>%
  group_by(Serovar2) %>%
  mutate(Serovar_with_count = paste0(Serovar2, "\n(n = ", sum(n), ")")) %>%
  ungroup()

df_serovar_animalPlus_all <- count_country_serovar %>%
  group_by(CombinedSite2) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n)) %>%
  mutate(Serovar2 = "All", 
         Serovar_with_count = paste0("All", "\n(n = ", sum(n), ")"))

df_serovar_animalPlus_combined <- bind_rows(df_serovar_animalPlus, df_serovar_animalPlus_all)

global_animalPlus_plot <- ggplot(df_serovar_animalPlus_combined, aes(x = Serovar_with_count, y = proportion, fill = CombinedSite2)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  labs(
    x = "Serovar",
    y = "Proportion",
    fill = "animalPlus"
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  coord_flip() + 
  theme_bw()

ggarrange(global_animal_plot, global_animalPlus_plot, align = "hv", labels = "AUTO")

############# HC Clustering #################
set.seed(123)

animalPlus_hc <- global_enterobase_clean2 %>%
  select(Serovar, Accession, CombinedSites, CombinedSite2, starts_with("HC")) %>%
  mutate(ID = row_number())

animalPlus_hc %>%
  group_by(CombinedSite2) %>%
  summarise(
    n = n(),  # Total count per label
    across(starts_with("HC"), ~ n_distinct(.), .names = "unique_{col}")
  ) %>%
  ungroup()

selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_animal", "Turkey_food", "Water"),
  n_selected = c(100, 200, 10, 200, 106, 170, 20, 82, 20, 200, 300),
  cluster = c("HC50", "HC50", "HC50", "HC20", "HC0", "HC0", "HC50", "HC0", "HC20", "HC20", "HC50")
)

# Select only the required columns
animalPlus_hc2 <- animalPlus_hc %>%
  left_join(selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.)

selected_samples_all <- sample_clusters(animalPlus_hc2, "CombinedSite2", selection_counts)
selected_samples_all2 <- selected_samples_all %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_filtered <- selected_samples_all2 %>%
  select(1:4)
fn_nzv_df_filtered <- fn_nzv_df2 %>%
  select(1:4)
global_fn_nzv_cols <- global_fn_clean %>%
  select(-CombinedSites, -CombinedSite2, -Serovar, -sample_id)
global_fn_join <- global_fn_clean %>%
  select(-CombinedSites, -CombinedSite2, -Serovar)
selected_samples_filtered2 <- selected_samples_filtered %>%
  left_join(global_fn_join, by = "sample_id")
fn_nzv_df_filtered2 <- fn_nzv_df_filtered %>%
  left_join(global_fn_join, by = "sample_id")

selected_samples_fn_all <- rbind(selected_samples_filtered2, fn_nzv_df_filtered2)

# write.table(selected_samples_all2, "global_nzv_clean2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_all, "global_fn_nzv_clean2.tsv", row.names = F, sep = "\t")

## Choosing a lot more samples for miniFeat ##
selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_animal", "Turkey_food", "Water"),
  n_selected = c(400, 1500, 300, 1500, 106, 170, 400, 82, 500, 1500, 1000),
  cluster = c("HC50", "HC10", "HC50", "HC5", "HC0", "HC0", "HC50", "HC0", "HC0", "HC2", "HC20")
)
animalPlus_hc2_v2 <- animalPlus_hc %>%
  left_join(selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.)
selected_samples_all_v2 <- sample_clusters(animalPlus_hc2, "CombinedSite2", selection_counts2)
selected_samples_all2_v2 <- selected_samples_all_v2 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_filtered_v2 <- selected_samples_all2_v2 %>%
  select(1:4)
selected_samples_filtered2_v2 <- selected_samples_filtered_v2 %>%
  left_join(global_fn_join, by = "sample_id")

selected_samples_fn_all2 <- rbind(selected_samples_filtered2_v2, fn_nzv_df_filtered2)

# write.table(selected_samples_all2_v2, "global_nzv_clean3.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_all2, "global_fn_nzv_clean3.tsv", row.names = F, sep = "\t")

# Printing the metadata of everything from Enterobase that's been cleaned/in Locidex
# global_fn_meta <- global_fn_join %>%
#   left_join(global_enterobase_clean2, by = c("sample_id" = "Accession")) 

### Testing by Serovar ###
# serovar_hc0_animalPlus <- animalPlus_hc %>%
#   filter(Serovar %in% serovar_list) %>%
#   group_by(Serovar, CombinedSite2) %>%
#   summarise(
#     n = n(),  # Total count per label
#     across(starts_with("HC"), ~ n_distinct(.), .names = "unique_{col}")
#   ) %>%
#   ungroup()

## Serovar counts ##
# Enteritidis selection counts
enteritidis_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_food", "Water"),
  n_selected = c(27, 49, 50, 100, 6, 10, 70, 4, 30, 120),
  cluster = c("HC0", "HC0", "HC20", "HC5", "HC0", "HC2", "HC0", "HC20", "HC0", "HC0")
)

# Heidelberg selection counts
heidelberg_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Pig_food", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(66, 27, 100, 100, 2, 100, 1, 13, 100, 16),
  cluster = c("HC0", "HC0", "HC10", "HC5", "HC50", "HC5", "HC50", "HC0", "HC2", "HC0")
)

# I 1,4,[5],12:i:- selection counts
i4512_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Turkey_animal", "Turkey_food",
                    "Water"),
  n_selected = c(57, 38, 100, 68, 1, 33, 9, 34, 59),
  cluster = c("HC0", "HC0", "HC5", "HC0", "HC50", "HC0", "HC0", "HC0", "HC0")
)

# Infantis selection counts
infantis_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Pig_food", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(82, 100, 100, 100, 3, 100, 2, 10, 84, 100),
  cluster = c("HC0", "HC2", "HC20", "HC10", "HC0", "HC10", "HC5", "HC5", "HC0", "HC2")
)

# Kentucky selection counts
kentucky_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(100, 105, 20, 50, 25, 4, 73, 10, 29, 43),
  cluster = c("HC10", "HC0", "HC50", "HC20", "HC0", "HC0", "HC0", "HC0", "HC0", "HC0")
)

# Typhimurium selection counts
typhimurium_selection_counts <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_animal", "Turkey_food", "Water"),
  n_selected = c(100, 150, 100, 100, 14, 17, 50, 31, 37, 100, 100),
  cluster = c("HC20", "HC10", "HC20", "HC10", "HC0", "HC0", "HC50", "HC0", "HC0", "HC2", "HC20")
)

typh_animalPlus_hc <- animalPlus_hc %>%
  left_join(typhimurium_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Typhimurium")
selected_samples_typh <- sample_clusters(typh_animalPlus_hc, "CombinedSite2", typhimurium_selection_counts)

i4512_animalPlus_hc <- animalPlus_hc %>%
  left_join(i4512_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "I 1,4,[5],12:i:-")
selected_samples_i4512 <- sample_clusters(i4512_animalPlus_hc, "CombinedSite2", i4512_selection_counts)

kent_animalPlus_hc <- animalPlus_hc %>%
  left_join(kentucky_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Kentucky")
selected_samples_kent <- sample_clusters(kent_animalPlus_hc, "CombinedSite2", kentucky_selection_counts)

heid_animalPlus_hc <- animalPlus_hc %>%
  left_join(heidelberg_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Heidelberg")
selected_samples_heid <- sample_clusters(heid_animalPlus_hc, "CombinedSite2", heidelberg_selection_counts)

entr_animalPlus_hc <- animalPlus_hc %>%
  left_join(enteritidis_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Enteritidis")
selected_samples_entr <- sample_clusters(entr_animalPlus_hc, "CombinedSite2", enteritidis_selection_counts)

infantis_animalPlus_hc <- animalPlus_hc %>%
  left_join(infantis_selection_counts, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Infantis")
selected_samples_infantis <- sample_clusters(infantis_animalPlus_hc, "CombinedSite2", infantis_selection_counts)

selected_samples_typh2 <- selected_samples_typh %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_i4512_2 <- selected_samples_i4512 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession) %>%
  mutate(Serovar = ifelse(Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Serovar))

selected_samples_kent2 <- selected_samples_kent %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_heid2 <- selected_samples_heid %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_entr2 <- selected_samples_entr %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_infantis2 <- selected_samples_infantis %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)


selected_samples_fn_typh <- process_selected_samples(selected_sample_df = selected_samples_typh,
                                                     fn_nzv_df = fn_nzv_df2,
                                                     global_df = global_fn_join,
                                                     serovar_name = "Typhimurium") 

selected_samples_fn_i4512 <- process_selected_samples(selected_sample_df = selected_samples_i4512,
                                                      fn_nzv_df = fn_nzv_df2,
                                                      global_df = global_fn_join,
                                                      serovar_name = "I:4,[5],12:i:-") %>%
  mutate(Serovar = ifelse(Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Serovar))

selected_samples_fn_kent <- process_selected_samples(selected_sample_df = selected_samples_kent,
                                                     fn_nzv_df = fn_nzv_df2,
                                                     global_df = global_fn_join,
                                                     serovar_name = "Kentucky") 

selected_samples_fn_heid <- process_selected_samples(selected_sample_df = selected_samples_heid,
                                                     fn_nzv_df = fn_nzv_df2,
                                                     global_df = global_fn_join,
                                                     serovar_name = "Heidelberg") 

selected_samples_fn_entr <- process_selected_samples(selected_sample_df = selected_samples_entr,
                                                     fn_nzv_df = fn_nzv_df2,
                                                     global_df = global_fn_join,
                                                     serovar_name = "Enteritidis") 

selected_samples_fn_inf <- process_selected_samples(selected_sample_df = selected_samples_infantis,
                                                    fn_nzv_df = fn_nzv_df2,
                                                    global_df = global_fn_join,
                                                    serovar_name = "Infantis") 


# write.table(selected_samples_typh2, "global_typh.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_i4512_2, "global_i4512.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_kent2, "global_kent.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_heid2, "global_heid.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_entr2, "global_entr.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_infantis2, "global_infantis.tsv", row.names = F, sep = "\t")
# 
# write.table(selected_samples_fn_typh, "global_fn_typh.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_i4512, "global_fn_i4512.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_kent, "global_fn_kent.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_heid, "global_fn_heid.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_entr, "global_fn_entr.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_inf, "global_fn_infantis.tsv", row.names = F, sep = "\t")

# Enteritidis selection counts
enteritidis_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_food", "Water"),
  n_selected = c(27, 49, 50, 300, 6, 10, 70, 4, 30, 120),
  cluster = c("HC0", "HC0", "HC20", "HC2", "HC0", "HC2", "HC0", "HC20", "HC0", "HC0")
)

# Heidelberg selection counts
heidelberg_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Pig_food", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(66, 27, 200, 300, 2, 100, 1, 13, 300, 16),
  cluster = c("HC0", "HC0", "HC5", "HC0", "HC50", "HC5", "HC50", "HC0", "HC0", "HC0")
)

# I 1,4,[5],12:i:- selection counts
i4512_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Turkey_animal", "Turkey_food",
                    "Water"),
  n_selected = c(57, 38, 150, 68, 1, 33, 9, 34, 59),
  cluster = c("HC0", "HC0", "HC2", "HC0", "HC50", "HC0", "HC0", "HC0", "HC0")
)

# Infantis selection counts
infantis_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_food", "Pig_animal", "Pig_food", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(82, 120, 100, 300, 3, 200, 2, 10, 84, 200),
  cluster = c("HC0", "HC0", "HC20", "HC2", "HC0", "HC10", "HC5", "HC5", "HC0", "HC0")
)

# Kentucky selection counts
kentucky_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Turkey_animal",
                    "Turkey_food", "Water"),
  n_selected = c(152, 110, 100, 300, 25, 4, 73, 10, 29, 43),
  cluster = c("HC0", "HC0", "HC20", "HC10", "HC0", "HC0", "HC0", "HC0", "HC0", "HC0")
)

# Typhimurium selection counts
typhimurium_selection_counts2 <- data.frame(
  CombinedSite2 = c("Beef_animal", "Beef_food", "Chicken_animal", "Chicken_food",
                    "Dairy_animal", "Dairy_food", "Pig_animal", "Pig_food",
                    "Turkey_animal", "Turkey_food", "Water"),
  n_selected = c(300, 289, 100, 300, 14, 17, 200, 31, 37, 183, 300),
  cluster = c("HC10", "HC0", "HC20", "HC10", "HC0", "HC0", "HC20", "HC0", "HC0", "HC0", "HC20")
)

typh_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(typhimurium_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Typhimurium")
selected_samples_typh3 <- sample_clusters(typh_animalPlus_hc2, "CombinedSite2", typhimurium_selection_counts2)

i4512_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(i4512_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "I 1,4,[5],12:i:-")
selected_samples_i4512_3 <- sample_clusters(i4512_animalPlus_hc2, "CombinedSite2", i4512_selection_counts2)

kent_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(kentucky_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Kentucky")
selected_samples_kent3 <- sample_clusters(kent_animalPlus_hc2, "CombinedSite2", kentucky_selection_counts2)

heid_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(heidelberg_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Heidelberg")
selected_samples_heid3 <- sample_clusters(heid_animalPlus_hc2, "CombinedSite2", heidelberg_selection_counts2)

entr_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(enteritidis_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Enteritidis")
selected_samples_entr3 <- sample_clusters(entr_animalPlus_hc2, "CombinedSite2", enteritidis_selection_counts2)

infantis_animalPlus_hc2 <- animalPlus_hc %>%
  left_join(infantis_selection_counts2, by = "CombinedSite2") %>%
  rename(HC0 = HC0..indistinguishable.) %>%
  filter(Serovar == "Infantis")
selected_samples_infantis3 <- sample_clusters(infantis_animalPlus_hc2, "CombinedSite2", infantis_selection_counts2)

selected_samples_typh4 <- selected_samples_typh3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_i4512_4 <- selected_samples_i4512_3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession) %>%
  mutate(Serovar = ifelse(Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Serovar))

selected_samples_kent4 <- selected_samples_kent3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_heid4 <- selected_samples_heid3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_entr4 <- selected_samples_entr3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)

selected_samples_infantis4 <- selected_samples_infantis3 %>%
  select(-Serovar, -CombinedSites, -CombinedSite2, -HC_used, -HC, -ID) %>%
  left_join(global_enterobase_clean, by = c("Accession" = "sample_id")) %>%
  rename(sample_id = Accession)


selected_samples_fn_typh2 <- process_selected_samples(selected_sample_df = selected_samples_typh3,
                                                      fn_nzv_df = fn_nzv_df2,
                                                      global_df = global_fn_join,
                                                      serovar_name = "Typhimurium") 

selected_samples_fn_i4512_2 <- process_selected_samples(selected_sample_df = selected_samples_i4512_3,
                                                        fn_nzv_df = fn_nzv_df2,
                                                        global_df = global_fn_join,
                                                        serovar_name = "I:4,[5],12:i:-") %>%
  mutate(Serovar = ifelse(Serovar == "I 1,4,[5],12:i:-", "I:4,[5],12:i:-", Serovar))

selected_samples_fn_kent2 <- process_selected_samples(selected_sample_df = selected_samples_kent3,
                                                      fn_nzv_df = fn_nzv_df2,
                                                      global_df = global_fn_join,
                                                      serovar_name = "Kentucky") 

selected_samples_fn_heid2 <- process_selected_samples(selected_sample_df = selected_samples_heid3,
                                                      fn_nzv_df = fn_nzv_df2,
                                                      global_df = global_fn_join,
                                                      serovar_name = "Heidelberg") 

selected_samples_fn_entr2 <- process_selected_samples(selected_sample_df = selected_samples_entr3,
                                                      fn_nzv_df = fn_nzv_df2,
                                                      global_df = global_fn_join,
                                                      serovar_name = "Enteritidis") 

selected_samples_fn_inf2 <- process_selected_samples(selected_sample_df = selected_samples_infantis3,
                                                     fn_nzv_df = fn_nzv_df2,
                                                     global_df = global_fn_join,
                                                     serovar_name = "Infantis") 
# 
# write.table(selected_samples_typh4, "global_typh2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_i4512_4, "global_i4512_2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_kent4, "global_kent2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_heid4, "global_heid2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_entr4, "global_entr2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_infantis4, "global_infantis2.tsv", row.names = F, sep = "\t")
# 
# write.table(selected_samples_fn_typh2, "global_fn_typh2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_i4512_2, "global_fn_i4512_2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_kent2, "global_fn_kent2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_heid2, "global_fn_heid2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_entr2, "global_fn_entr2.tsv", row.names = F, sep = "\t")
# write.table(selected_samples_fn_inf2, "global_fn_infantis2.tsv", row.names = F, sep = "\t")

######### PLOTS #############
# Set1
# selected_samples_filtered
# selected_samples_fn_all
# Set2
# selected_samples_filtered2_v2
# selected_samples_fn_all2

serovar_list <- c("Enteritidis", "Heidelberg", "Typhimurium", 
                  "Infantis", "Kentucky", "I:4,[5],12:i:-")

selected_samples_fn_all <- selected_samples_fn_all %>%
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites))
selected_samples_fn_all2 <- selected_samples_fn_all2 %>%
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites))

dfs <- list(
  selected_samples_filtered,
  selected_samples_fn_all,
  selected_samples_filtered2,
  selected_samples_fn_all2
)

# Apply the replacement to each data frame
dfs <- lapply(dfs, function(df) {
  df %>%
    mutate(Serovar = str_replace(Serovar, "I 1,4,\\[5\\],12:i:-", "I:4,[5],12:i:-"))
})

# Assign back to the original data frames
selected_samples_filtered <- dfs[[1]]
selected_samples_fn_all <- dfs[[2]]
selected_samples_filtered2 <- dfs[[3]]
selected_samples_fn_all2 <- dfs[[4]]

create_stacked_bar_df <- function(data, serovar_list, group_col) {
  # Step 1: Add "Other" category for serovars not in the list
  data <- data %>%
    mutate(Serovar_grouped = ifelse(Serovar %in% serovar_list, Serovar, "Other"))
  
  # Step 2: Calculate n and prop for each Serovar_grouped with the specified group_col
  df_combined <- data %>%
    group_by(Serovar_grouped, .data[[group_col]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Serovar_grouped) %>%
    mutate(
      total_n = sum(n),  # Calculate total count for each Serovar_grouped
      prop = n / sum(n)
    ) %>%
    ungroup() %>%
    mutate(label = paste(Serovar_grouped, "\n(n=", total_n, ")", sep = ""))
  
  # Step 3: Add "All" category representing all samples irrespective of serovar
  df_all <- data %>%
    group_by(.data[[group_col]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      Serovar_grouped = "All",
      total_n = sum(n),  # Total count for 'All' category
      prop = n / sum(n),
      label = paste("All", "\n(n=", total_n, ")", sep = "")
    )
  
  # Step 4: Combine the data for specified serovars, "Other", and "All"
  df_final <- bind_rows(df_combined, df_all)
  
  return(df_final)
}

ebase_set1_animal <- create_stacked_bar_df(selected_samples_filtered, serovar_list, "CombinedSites")
ebase_set1_animalPlus <- create_stacked_bar_df(selected_samples_filtered, serovar_list, "CombinedSite2")

ebase_set2_animal <- create_stacked_bar_df(selected_samples_filtered2, serovar_list, "CombinedSites")
ebase_set2_animalPlus <- create_stacked_bar_df(selected_samples_filtered2, serovar_list, "CombinedSite2")

combined_set1_animal <- create_stacked_bar_df(selected_samples_fn_all, serovar_list, "CombinedSites")
combined_set1_animalPlus <- create_stacked_bar_df(selected_samples_fn_all, serovar_list, "CombinedSite2")

combined_set2_animal <- create_stacked_bar_df(selected_samples_fn_all2, serovar_list, "CombinedSites")
combined_set2_animalPlus <- create_stacked_bar_df(selected_samples_fn_all2, serovar_list, "CombinedSite2")

p_ebase_set1_animal <- ggplot(ebase_set1_animal, aes(x = label, y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_ebase_set1_animalPlus <- ggplot(ebase_set1_animalPlus, aes(x = label, y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_ebase_set2_animal <- ggplot(ebase_set2_animal, aes(x = label, y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_ebase_set2_animalPlus <- ggplot(ebase_set2_animalPlus, aes(x = label, y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_combined_set1_animal <- ggplot(combined_set1_animal, aes(x = label, y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_combined_set1_animalPlus <- ggplot(combined_set1_animalPlus, aes(x = label, y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_combined_set2_animal <- ggplot(combined_set2_animal, aes(x = label, y = prop, fill = CombinedSites)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

p_combined_set2_animalPlus <- ggplot(combined_set2_animalPlus, aes(x = label, y = prop, fill = CombinedSite2)) +
  geom_bar(stat = "identity", position = "stack") +  # Use 'stack' for standard stacked bars
  labs(x = "Serovar", y = "Proportion (%)") +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +  # Rotate x-axis labels for readability
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(11, "Paired"))  # Use a palette with 10 colors

ggarrange(p_ebase_set1_animal, p_ebase_set2_animal, p_combined_set1_animal, p_combined_set2_animal,
          align = "hv", labels = "AUTO")
ggarrange(p_ebase_set1_animalPlus, p_ebase_set2_animalPlus, p_combined_set1_animalPlus, p_combined_set2_animalPlus,
          align = "hv", labels = "AUTO")
######################### 
# Calculate counts and proportions for df1
combined_set1_animal_count <- selected_samples_fn_all %>%
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites)) %>%
  group_by(CombinedSites) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "Combined_Set1"
  )

# Calculate counts and proportions for df2
fn_animal_count <- fn_nzv_df2 %>%
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites)) %>%
  group_by(CombinedSites) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "FoodNet"
  )

# Calculate the difference (df2 - df1) for counts and proportions
difference_combined <- fn_animal_count %>%
  full_join(combined_set1_animal_count, by = "CombinedSites", suffix = c("_df1", "_df2")) %>%
  mutate(
    count_Difference = coalesce(count_df2, 0) - coalesce(count_df1, 0),
    proportion_Difference = coalesce(proportion_df2, 0) - coalesce(proportion_df1, 0)
  ) %>%
  select(CombinedSites, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference",
    count = count_Difference,
    proportion = proportion_Difference
  )

# Combine df1, df2, and the difference into one data frame
combined_data_animal_set1 <- bind_rows(
  combined_set1_animal_count,
  fn_animal_count,
  difference_combined
)

combined_data_animal_set1 <- combined_data_animal_set1 %>%
  mutate(plot_count = ifelse(dataset == "Difference", count_Difference, count))

# Ensure CombinedSites is a factor for plotting
combined_data_animal_set1$CombinedSites <- factor(combined_data_animal_set1$CombinedSites, 
                                                  levels = unique(combined_data_animal_set1$CombinedSites))

# Set the dataset factor levels for plotting order
combined_data_animal_set1$dataset <- factor(combined_data_animal_set1$dataset, levels = c("Difference", "FoodNet", "Combined_Set1"))

dataset_palette <- c("Combined_Set1" = "#EFC000FF", "FoodNet" = "#0073C2FF", "Difference" = "darkgreen")

# Proportion plot
plot_combined_proportion_animal <- ggplot(combined_data_animal_set1, aes(x = CombinedSites, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion (%)", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, max(combined_data_animal_set1$proportion) + 0.1),
                     breaks = seq(-0.1, max(combined_data_animal_set1$proportion) + 0.1, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_combined_count_animal <- ggplot(combined_data_animal_set1, aes(x = CombinedSites, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 2800),
                     breaks = seq(0, 2800, by = 400)) +
  guides(color = guide_legend(reverse = TRUE))

# Calculate counts and proportions for df1
combined_set1_animalPlus_count <- selected_samples_fn_all %>%
  group_by(CombinedSite2) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "Combined_Set1"
  )

# Calculate counts and proportions for df2
fn_animalPlus_count <- fn_nzv_df2 %>%
  group_by(CombinedSite2) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "FoodNet"
  )

# Calculate the difference (df2 - df1) for counts and proportions
difference_combined <- fn_animalPlus_count %>%
  full_join(combined_set1_animalPlus_count, by = "CombinedSite2", suffix = c("_df1", "_df2")) %>%
  mutate(
    count_Difference = coalesce(count_df2, 0) - coalesce(count_df1, 0),
    proportion_Difference = coalesce(proportion_df2, 0) - coalesce(proportion_df1, 0)
  ) %>%
  select(CombinedSite2, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference",
    count = count_Difference,
    proportion = proportion_Difference
  )

# Combine df1, df2, and the difference into one data frame
combined_data_animal_set1 <- bind_rows(
  combined_set1_animalPlus_count,
  fn_animalPlus_count,
  difference_combined
)

combined_data_animal_set1 <- combined_data_animal_set1 %>%
  mutate(plot_count = ifelse(dataset == "Difference", count_Difference, count))

# Ensure CombinedSite2 is a factor for plotting
combined_data_animal_set1$CombinedSite2 <- factor(combined_data_animal_set1$CombinedSite2, levels = unique(combined_data_animal_set1$CombinedSite2))

# Set the dataset factor levels for plotting order
combined_data_animal_set1$dataset <- factor(combined_data_animal_set1$dataset, levels = c("Difference", "FoodNet", "Combined_Set1"))

# Proportion plot
plot_combined_proportion_animalPlus <- ggplot(combined_data_animal_set1, aes(x = CombinedSite2, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Proportion (%)", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.1, max(combined_data_animal_set1$proportion) + 0.1),
                     breaks = seq(-0.1, max(combined_data_animal_set1$proportion) + 0.1, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_combined_count_animalPlus <- ggplot(combined_data_animal_set1, aes(x = CombinedSite2, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette) +
  labs(y = "Count", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1700),
                     breaks = seq(0, 1700, by = 200)) +
  guides(color = guide_legend(reverse = TRUE))

dataset_palette2 <- c("Combined_Set2" = "#EFC000FF", "FoodNet" = "#0073C2FF", "Difference" = "darkgreen")

# Calculate counts and proportions for df1
combined_set2_animal_count <- selected_samples_fn_all2 %>%
  mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites)) %>%
  group_by(CombinedSites) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "Combined_Set2"
  )

# Calculate the difference (df2 - df1) for counts and proportions
difference_combined <- fn_animal_count %>%
  full_join(combined_set2_animal_count, by = "CombinedSites", suffix = c("_df1", "_df2")) %>%
  mutate(
    count_Difference = coalesce(count_df2, 0) - coalesce(count_df1, 0),
    proportion_Difference = coalesce(proportion_df2, 0) - coalesce(proportion_df1, 0)
  ) %>%
  select(CombinedSites, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference",
    count = count_Difference,
    proportion = proportion_Difference
  )

# Combine df1, df2, and the difference into one data frame
combined_data_animal_set2 <- bind_rows(
  combined_set2_animal_count,
  fn_animal_count,
  difference_combined
)

combined_data_animal_set2 <- combined_data_animal_set2 %>%
  mutate(plot_count = ifelse(dataset == "Difference", count_Difference, count))

# Ensure CombinedSites is a factor for plotting
combined_data_animal_set2$CombinedSites <- factor(combined_data_animal_set2$CombinedSites, levels = unique(combined_data_animal_set2$CombinedSites))

# Set the dataset factor levels for plotting order
combined_data_animal_set2$dataset <- factor(combined_data_animal_set2$dataset, levels = c("Difference", "FoodNet", "Combined_Set2"))

# Proportion plot
plot_combined_proportion_animal_set2 <- ggplot(combined_data_animal_set2, aes(x = CombinedSites, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette2) +
  labs(y = "Proportion (%)", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.2, max(combined_data_animal_set2$proportion) + 0.1),
                     breaks = seq(-0.2, max(combined_data_animal_set2$proportion) + 0.1, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_combined_count_animal_set2 <- ggplot(combined_data_animal_set2, aes(x = CombinedSites, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette2) +
  labs(y = "Count", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 4400),
                     breaks = seq(0, 4400, by = 400)) +
  guides(color = guide_legend(reverse = TRUE))

# Calculate counts and proportions for df1
combined_set2_animalPlus_count <- selected_samples_fn_all2 %>%
  group_by(CombinedSite2) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    proportion = count / sum(count),
    dataset = "Combined_Set2"
  )

# Calculate the difference (df2 - df1) for counts and proportions
difference_combined <- fn_animalPlus_count %>%
  full_join(combined_set2_animalPlus_count, by = "CombinedSite2", suffix = c("_df1", "_df2")) %>%
  mutate(
    count_Difference = coalesce(count_df2, 0) - coalesce(count_df1, 0),
    proportion_Difference = coalesce(proportion_df2, 0) - coalesce(proportion_df1, 0)
  ) %>%
  select(CombinedSite2, count_Difference, proportion_Difference) %>%
  mutate(
    dataset = "Difference",
    count = count_Difference,
    proportion = proportion_Difference
  )

# Combine df1, df2, and the difference into one data frame
combined_data_animalPlus_set2 <- bind_rows(
  combined_set2_animalPlus_count,
  fn_animalPlus_count,
  difference_combined
)

combined_data_animalPlus_set2 <- combined_data_animalPlus_set2 %>%
  mutate(plot_count = ifelse(dataset == "Difference", count_Difference, count))

# Ensure CombinedSite2 is a factor for plotting
combined_data_animalPlus_set2$CombinedSite2 <- factor(combined_data_animalPlus_set2$CombinedSite2, levels = unique(combined_data_animalPlus_set2$CombinedSite2))

# Set the dataset factor levels for plotting order
combined_data_animalPlus_set2$dataset <- factor(combined_data_animalPlus_set2$dataset, 
                                                levels = c("Difference", "FoodNet", "Combined_Set2"))

# Proportion plot
plot_combined_proportion_animalPlus_set2 <- ggplot(combined_data_animalPlus_set2, aes(x = CombinedSite2, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette2) +
  labs(y = "Proportion (%)", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.2, max(combined_data_animalPlus_set2$proportion) + 0.1),
                     breaks = seq(-0.2, max(combined_data_animalPlus_set2$proportion) + 0.1, by = 0.1)) +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8)

plot_combined_count_animalPlus_set2 <- ggplot(combined_data_animalPlus_set2, aes(x = CombinedSite2, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  scale_color_manual(values = dataset_palette2) +
  labs(y = "Count", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 2500),
                     breaks = seq(0, 2500, by = 250)) +
  guides(color = guide_legend(reverse = TRUE))

ggarrange(plot_combined_proportion_animal, plot_combined_count_animal,
          plot_combined_proportion_animal_set2, plot_combined_count_animal_set2, align = "hv",
          labels = "AUTO")

ggarrange(plot_combined_proportion_animalPlus, plot_combined_count_animalPlus,
          plot_combined_proportion_animalPlus_set2, plot_combined_count_animalPlus_set2, align = "hv",
          labels = "AUTO")
########### Serovar plots ############
# selected_samples_fn_typh
# selected_samples_fn_i4512
# selected_samples_fn_kent
# selected_samples_fn_heid
# selected_samples_fn_entr
# selected_samples_fn_inf

create_combined_df <- function(df, fn_nzv_df2, dataset_name, group_col) {
  # Step 1: Extract the Serovar from the input data frame
  serovar_name <- unique(df$Serovar)
  
  # Step 2: Filter fn_nzv_df2 for the specific Serovar
  fn_filtered <- fn_nzv_df2 %>%
    filter(Serovar == serovar_name)
  
  # Step 3: Conditional modification if group_col is "CombinedSites"
  if (group_col == "CombinedSites") {
    df <- df %>%
      mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites))
    
    fn_filtered <- fn_filtered %>%
      mutate(CombinedSites = if_else(CombinedSites == "Pig", "Pork", CombinedSites))
  }
  
  # Step 3: Calculate counts and proportions for the Combined_Set1 (input df) based on the group_col
  combined_set1_count <- df %>%
    group_by(.data[[group_col]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      proportion = count / sum(count),
      dataset = "Combined"
    )
  
  # Step 4: Calculate counts and proportions for the filtered FoodNet dataset
  fn_count <- fn_filtered %>%
    group_by(.data[[group_col]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      proportion = count / sum(count),
      dataset = "FoodNet"
    )
  
  # Step 5: Calculate the difference (df2 - df1) for counts and proportions
  difference_combined <- fn_count %>%
    full_join(combined_set1_count, by = group_col, suffix = c("_df1", "_df2")) %>%
    mutate(
      count_Difference = coalesce(count_df2, 0) - coalesce(count_df1, 0),
      proportion_Difference = coalesce(proportion_df2, 0) - coalesce(proportion_df1, 0)
    ) %>%
    select(!!sym(group_col), count_Difference, proportion_Difference) %>%
    mutate(
      dataset = "Difference",
      count = count_Difference,
      proportion = proportion_Difference
    )
  
  # Step 6: Combine df1, df2, and the difference into one data frame
  combined_data <- bind_rows(
    combined_set1_count,
    fn_count,
    difference_combined
  )
  
  # Adjust 'plot_count' for plotting based on the dataset
  combined_data <- combined_data %>%
    mutate(plot_count = ifelse(dataset == "Difference", count_Difference, count)) %>%
    mutate(Serovar = serovar_name)
  
  # Ensure the grouping column is a factor for consistency
  combined_data[[group_col]] <- factor(combined_data[[group_col]], levels = unique(combined_data[[group_col]]))
  
  # Set the dataset factor levels for ordering
  combined_data$dataset <- factor(combined_data$dataset, levels = c("Difference", "FoodNet", "Combined"))
  
  # Return the final data frame
  return(combined_data)
}

typh_difference_animal <- create_combined_df(selected_samples_fn_typh, fn_nzv_df2, 
                                             "Typhimurium", "CombinedSites")
i4512_difference_animal <- create_combined_df(selected_samples_fn_i4512, fn_nzv_df2, 
                                              "I:4,[5],12:i:-", "CombinedSites")
heid_difference_animal <- create_combined_df(selected_samples_fn_heid, fn_nzv_df2, 
                                             "Heidelberg", "CombinedSites")
kent_difference_animal <- create_combined_df(selected_samples_fn_kent, fn_nzv_df2, 
                                             "Kentucky", "CombinedSites")
entr_difference_animal <- create_combined_df(selected_samples_fn_entr, fn_nzv_df2, 
                                             "Enteritidis", "CombinedSites")
infantis_difference_animal <- create_combined_df(selected_samples_fn_inf, fn_nzv_df2, 
                                                 "Infantis", "CombinedSites")

typh_difference_animalPlus <- create_combined_df(selected_samples_fn_typh, fn_nzv_df2, 
                                                 "Typhimurium", "CombinedSite2")
i4512_difference_animalPlus <- create_combined_df(selected_samples_fn_i4512, fn_nzv_df2, 
                                                  "I:4,[5],12:i:-", "CombinedSite2")
heid_difference_animalPlus <- create_combined_df(selected_samples_fn_heid, fn_nzv_df2, 
                                                 "Heidelberg", "CombinedSite2")
kent_difference_animalPlus <- create_combined_df(selected_samples_fn_kent, fn_nzv_df2, 
                                                 "Kentucky", "CombinedSite2")
entr_difference_animalPlus <- create_combined_df(selected_samples_fn_entr, fn_nzv_df2, 
                                                 "Enteritidis", "CombinedSite2")
infantis_difference_animalPlus <- create_combined_df(selected_samples_fn_inf, fn_nzv_df2, 
                                                     "Infantis", "CombinedSite2")

typh_difference_animal2 <- create_combined_df(selected_samples_fn_typh2, fn_nzv_df2, 
                                              "Typhimurium", "CombinedSites")
i4512_difference_animal2 <- create_combined_df(selected_samples_fn_i4512_2, fn_nzv_df2, 
                                               "I:4,[5],12:i:-", "CombinedSites")
heid_difference_animal2 <- create_combined_df(selected_samples_fn_heid2, fn_nzv_df2, 
                                              "Heidelberg", "CombinedSites")
kent_difference_animal2 <- create_combined_df(selected_samples_fn_kent2, fn_nzv_df2, 
                                              "Kentucky", "CombinedSites")
entr_difference_animal2 <- create_combined_df(selected_samples_fn_entr2, fn_nzv_df2, 
                                              "Enteritidis", "CombinedSites")
infantis_difference_animal2 <- create_combined_df(selected_samples_fn_inf2, fn_nzv_df2, 
                                                  "Infantis", "CombinedSites")

typh_difference_animalPlus2 <- create_combined_df(selected_samples_fn_typh2, fn_nzv_df2, 
                                                  "Typhimurium", "CombinedSite2")
i4512_difference_animalPlus2 <- create_combined_df(selected_samples_fn_i4512_2, fn_nzv_df2, 
                                                   "I:4,[5],12:i:-", "CombinedSite2")
heid_difference_animalPlus2 <- create_combined_df(selected_samples_fn_heid2, fn_nzv_df2, 
                                                  "Heidelberg", "CombinedSite2")
kent_difference_animalPlus2 <- create_combined_df(selected_samples_fn_kent2, fn_nzv_df2, 
                                                  "Kentucky", "CombinedSite2")
entr_difference_animalPlus2 <- create_combined_df(selected_samples_fn_entr2, fn_nzv_df2, 
                                                  "Enteritidis", "CombinedSite2")
infantis_difference_animalPlus2 <- create_combined_df(selected_samples_fn_inf2, fn_nzv_df2, 
                                                      "Infantis", "CombinedSite2")

serovar_dif_animal <- rbind(typh_difference_animal, i4512_difference_animal, heid_difference_animal,
                            kent_difference_animal, entr_difference_animal, infantis_difference_animal)
serovar_dif_animalPlus <- rbind(typh_difference_animalPlus, i4512_difference_animalPlus, heid_difference_animalPlus,
                                kent_difference_animalPlus, entr_difference_animalPlus, infantis_difference_animalPlus)

serovar_dif_animal2 <- rbind(typh_difference_animal2, i4512_difference_animal2, heid_difference_animal2,
                             kent_difference_animal2, entr_difference_animal2, infantis_difference_animal2)
serovar_dif_animalPlus2 <- rbind(typh_difference_animalPlus2, i4512_difference_animalPlus2, 
                                 heid_difference_animalPlus2, kent_difference_animalPlus2, 
                                 entr_difference_animalPlus2, infantis_difference_animalPlus2)

serovar_dataset_palette <- c("Combined" = "#EFC000FF", "FoodNet" = "#0073C2FF", "Difference" = "darkgreen")

plot_serovar_dif_proportion_animal <- ggplot(serovar_dif_animal, aes(x = CombinedSites, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Proportion (%)", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.6, max(serovar_dif_animal$proportion) + 0.1),
                     breaks = seq(-0.6, max(serovar_dif_animal$proportion) + 0.1, by = 0.2),
                     labels = scales::label_number(accuracy = 0.1))  + # Format y-axis labels
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0.0, linetype = "solid", color = "black", linewidth = 0.8)

plot_serovar_dif_count_animal <- ggplot(serovar_dif_animal, aes(x = CombinedSites, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Count", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1200),
                     breaks = seq(0, 1200, by = 100)) +
  guides(color = guide_legend(reverse = TRUE))

plot_serovar_dif_proportion_animalPlus <- ggplot(serovar_dif_animalPlus, aes(x = CombinedSite2, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Proportion (%)", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.6, max(serovar_dif_animalPlus$proportion) + 0.1),
                     breaks = seq(-0.6, max(serovar_dif_animalPlus$proportion) + 0.1, by = 0.2),
                     labels = scales::label_number(accuracy = 0.1))  + # Format y-axis labels
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0.0, linetype = "solid", color = "black", linewidth = 0.8)

plot_serovar_dif_count_animalPlus <- ggplot(serovar_dif_animalPlus, aes(x = CombinedSite2, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Count", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 800),
                     breaks = seq(0, 800, by = 100)) +
  guides(color = guide_legend(reverse = TRUE))

ggarrange(plot_serovar_dif_proportion_animal, plot_serovar_dif_count_animal, align = "hv", labels = "AUTO",
          common.legend = T, legend = "right", nrow = 2)
ggarrange(plot_serovar_dif_proportion_animalPlus, plot_serovar_dif_count_animalPlus, align = "hv", labels = "AUTO",
          common.legend = T, legend = "right", nrow = 2)

plot_serovar_dif_proportion_animal2 <- ggplot(serovar_dif_animal2, aes(x = CombinedSites, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Proportion (%)", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.6, max(serovar_dif_animal2$proportion) + 0.1),
                     breaks = seq(-0.6, max(serovar_dif_animal2$proportion) + 0.1, by = 0.2),
                     labels = scales::label_number(accuracy = 0.1))  + # Format y-axis labels
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0.0, linetype = "solid", color = "black", linewidth = 0.8)

plot_serovar_dif_count_animal2 <- ggplot(serovar_dif_animal2, aes(x = CombinedSites, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSites, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Count", x = "Animal", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1500),
                     breaks = seq(0, 1500, by = 300)) +
  guides(color = guide_legend(reverse = TRUE))

plot_serovar_dif_proportion_animalPlus2 <- ggplot(serovar_dif_animalPlus2, aes(x = CombinedSite2, y = proportion, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = proportion), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Proportion (%)", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.6, max(serovar_dif_animalPlus2$proportion) + 0.1),
                     breaks = seq(-0.6, max(serovar_dif_animalPlus2$proportion) + 0.1, by = 0.2),
                     labels = scales::label_number(accuracy = 0.1))  + # Format y-axis labels
  guides(color = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0.0, linetype = "solid", color = "black", linewidth = 0.8)

plot_serovar_dif_count_animalPlus2 <- ggplot(serovar_dif_animalPlus2, aes(x = CombinedSite2, y = plot_count, color = dataset)) +
  geom_linerange(aes(x = CombinedSite2, ymin = 0, ymax = plot_count), 
                 position = position_dodge(width = 0.5), linetype = "solid") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  coord_flip() +
  facet_grid(~Serovar, scales = "free_y") +
  scale_color_manual(values = serovar_dataset_palette) +
  labs(y = "Count", x = "AnimalPlus", color = "Dataset") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 900),
                     breaks = seq(0, 900, by = 100)) +
  guides(color = guide_legend(reverse = TRUE))

ggarrange(plot_serovar_dif_proportion_animal2, plot_serovar_dif_count_animal2, align = "hv", labels = "AUTO",
          common.legend = T, legend = "right", nrow = 2)
ggarrange(plot_serovar_dif_proportion_animalPlus2, plot_serovar_dif_count_animalPlus2, align = "hv", labels = "AUTO",
          common.legend = T, legend = "right", nrow = 2)

### Table for num_pred and sample_sizes ### 
# Set1
# selected_samples_filtered
# selected_samples_fn_all
# Set2
# selected_samples_filtered2_v2
# selected_samples_fn_all2
# selected_samples_fn_typh
# selected_samples_fn_i4512
# selected_samples_fn_kent
# selected_samples_fn_heid
# selected_samples_fn_entr
# selected_samples_fn_inf

# selected_samples_typh2
# selected_samples_i4512_2
# selected_samples_kent2
# selected_samples_heid2
# selected_samples_entr2
# selected_samples_infantis2

table_fn_set1 <- fn_nzv_df2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_ebase_set1 <- selected_samples_filtered %>% 
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_set1 <- selected_samples_fn_all %>% 
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_ebase_set2 <- selected_samples_filtered2_v2 %>% 
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_set2 <- selected_samples_fn_all2 %>% 
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")

table_combined_serovar1 <- selected_samples_fn_typh %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_serovar2 <- selected_samples_fn_i4512 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_serovar3 <- selected_samples_fn_kent %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_serovar4 <- selected_samples_fn_heid %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_serovar5 <- selected_samples_fn_entr %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_combined_serovar6 <- selected_samples_fn_inf %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")

table_combined_serovar1_set2 <- selected_samples_fn_typh2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_serovar2_set2 <- selected_samples_fn_i4512_2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_serovar3_set2 <- selected_samples_fn_kent2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_serovar4_set2 <- selected_samples_fn_heid2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_serovar5_set2 <- selected_samples_fn_entr2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_combined_serovar6_set2 <- selected_samples_fn_inf2 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")

table_ebase_serovar1_set2 <- selected_samples_typh4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_ebase_serovar2_set2 <- selected_samples_i4512_4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_ebase_serovar3_set2 <- selected_samples_kent4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_ebase_serovar4_set2 <- selected_samples_heid4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_ebase_serovar5_set2 <- selected_samples_entr4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")
table_ebase_serovar6_set2 <- selected_samples_infantis4 %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set2")

table_fn_serovar1 <- fn_nzv_df2 %>%
  filter(Serovar == "Typhimurium") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_fn_serovar2 <- fn_nzv_df2 %>%
  filter(Serovar == "Heidelberg") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_fn_serovar3 <- fn_nzv_df2 %>%
  filter(Serovar == "Kentucky") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_fn_serovar4 <- fn_nzv_df2 %>%
  filter(Serovar == "Enteritidis") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_fn_serovar5 <- fn_nzv_df2 %>%
  filter(Serovar == "Infantis") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")
table_fn_serovar6 <- fn_nzv_df2 %>%
  filter(Serovar == "I:4,[5],12:i:-") %>%
  select(sample_id, CombinedSites, CombinedSite2, Serovar) %>% 
  mutate(set = "set1")

# Define a function to process a single data frame
process_df <- function(df, df_name) {
  # Determine category and serovar
  if (grepl("table_ebase_set1|table_ebase_set2", df_name)) {
    category <- "global_Ebase"
    serovar <- "All"
  } else if (grepl("table_combined_set1|table_combined_set2", df_name)) {
    category <- "global_Combined"
    serovar <- "All"
  } else if (grepl("table_fn_set1", df_name)) {
    category <- "FoodNet"
    serovar <- "All"
  } else {
    if (grepl("table_ebase_", df_name)) {
      category <- "global_Ebase"
    } else if (grepl("table_combined_", df_name)) {
      category <- "global_Combined"
    } else if (grepl("table_fn_", df_name)) {
      category <- "FoodNet"
    }
    serovar <- ifelse(length(unique(df$Serovar)) == 1, unique(df$Serovar), "Multiple")
  }
  
  # Calculate sample size (number of rows)
  sample_size <- nrow(df)
  
  # Calculate num_pred for combinedsites
  combinedsites_counts <- table(df$CombinedSites)
  num_pred_combinedsites <- sum(combinedsites_counts > 10)
  
  # Calculate num_pred for combinedsite2
  combinedsite2_counts <- table(df$CombinedSite2)
  num_pred_combinedsite2 <- sum(combinedsite2_counts > 10)
  
  # Special cases for Kentucky and I:4,[5],12:i:-
  if (serovar == "Kentucky") {
    num_pred_combinedsites <- 5  # Set to 5 for label == "animal"
  } else if (serovar == "I:4,[5],12:i:-") {
    num_pred_combinedsite2 <- 5  # Set to 5 for label == "animalPlus"
  }
  
  # Extract the set value
  set_value <- ifelse(length(unique(df$set)) == 1, unique(df$set), "Multiple")
  
  # Prepare results for each label type
  results <- data.frame(
    category = category,
    serovar = serovar,
    set = set_value,
    label = c("animal", "animalPlus"),
    sample_size = sample_size,
    num_pred = c(num_pred_combinedsites, num_pred_combinedsite2),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

if (exists("df_names")) {
  rm(df_names)
}

# List of data frames to process
df_names <- ls(pattern = "^table_(fn|combined|ebase)_")

# Initialize an empty list to store results
results_list <- list()

# Loop through each data frame name
for (df_name in df_names) {
  # Get the data frame from the global environment
  df <- get(df_name, envir = .GlobalEnv)
  
  # Replace "Pig" with "Pork" in the "CombinedSites" column if it exists
  if ("CombinedSites" %in% colnames(df)) {
    df$CombinedSites <- gsub("Pig", "Pork", df$CombinedSites)
  }
  
  # Process the data frame and append the results to the list
  results_list[[df_name]] <- process_df(df, df_name)
}

# Combine the results into a single data frame
summary_df <- do.call(rbind, results_list)

# write.csv(summary_df, "../Desktop/Data/global_enterobase/Summary_global2.csv", row.names = F)
