library(caret)
library(dplyr)

canada_only <- read.csv("../Desktop/Data/locidex_out/ebase_host_locidex_LE_20240820.tsv", sep = "\t")

# Keeping only columns subject to nzv
canada_only_list <- canada_only %>%
  select(-sample_id, -CombinedSites, -CombinedSite2, -serovar_cgmlst)

# Generating a text file with columns to be removed based on nzv based on a list of names
nzv_canada_only <- nearZeroVar(canada_only_list, saveMetrics = T)
col_remove_canada_only <- names(canada_only_list)[nzv_canada_only$nzv]

# write.table(col_remove_canada_only, "../Desktop/Data/global_enterobase/canada_only_nzv_cols.txt",
#             row.names = F,
#             col.names = F,
#             quote = F)