############################################################
# 1. Install and Load Required Packages
############################################################
# Install packages (only if necessary)
if (!require("writexl")) install.packages("writexl")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("bibliometrix")) install.packages(
"bibliometrix",
dependencies = TRUE
)
# Load packages
library(writexl)
library(readr)
library(readxl)
library(dplyr)
library(bibliometrix)

############################################################
# 2. Initial Data Processing (Scopus Raw Dataset)
############################################################
# NOTE:
# The original Scopus export file (scopus_raw_1678.csv) is not
# redistributed due to database licensing restrictions.
# The steps below document the original preprocessing workflow.
# Import raw Scopus dataset (initial retrieval: 1,678 records)
scopus_raw_1678 <- read_csv("scopus_raw_1678.csv")
View(scopus_raw_1678)
# Convert Scopus file to bibliometrix format
M_initial <- convert2df(
file = "scopus_raw_1678.csv",
dbsource = "scopus",
format = "csv"
)
# Automatic duplicate removal
M_initial <- duplicatedMatching(
M_initial,
Field = "TI",
exact = FALSE,
tol = 0.95
)
# Remove records with empty affiliation field
M_initial <- M_initial[M_initial$C1 != "", ]
dim(M_initial)
# Export dataset used for manual screening
write_xlsx(
M_initial,
path = "scopus_1658.xlsx"
)







############################################################
# 3. Reimport After Manual Screening (Final Dataset – 724)
############################################################
# Screened dataset after title/abstract review
# Original retrieval:
# 2,725 → 1,678 → 724 articles retained
# Import screened dataset
df_724 <- read_xlsx("scopus_724.xlsx")
# Convert XLSX to CSV for bibliometrix compatibility
write.csv(
df_724,
"scopus_724_v1.csv",
row.names = FALSE
)
# Convert CSV to bibliometrix dataframe
M <- convert2df(
file = "scopus_724_v1.csv",
dbsource = "scopus",
format = "csv"
)
# Inspect structure
dim(M)
colnames(M)

############################################################
# 4. Data Cleaning (Object M)
############################################################
# Re-run duplicate verification to ensure no residual redundancy
M <- duplicatedMatching(
M,
Field = "TI",
exact = FALSE,
tol = 0.95
)
# Citation normalization (fuzzy matching to reduce reference fragmentation)
M_temp <- applyCitationMatching(
M,
threshold = 0.85
)
# Replace original CR field with normalized cited references
M <- M %>%
rename(CR_orig = CR) %>% # Preserve original cited references
left_join(
M_temp$CR_normalized,
by = "SR"
) # Join normalized citation field by source reference
# Inspect structure
dim(M)
colnames(M)

############################################################
# 5. Metadata Extraction and Normalization
############################################################
# Extract country metadata from affiliations
M <- metaTagExtraction(
M,
Field = "AU_CO",
sep = ";"
)
# Extract and disambiguate institutional affiliation metadata
M <- metaTagExtraction(
M,
Field = "AU_UN",
aff.disamb = TRUE
)

############################################################
# 6. Quality Assessment
############################################################
# Evaluate metadata completeness by identifying missing values
# in mandatory bibliographic fields
data_quality <- missingData(M)
print(data_quality$mandatoryTags)

############################################################
# 7. Export Datasets (M and M2)
############################################################
# Export Object M (Clean)
write_xlsx(
M,
path = "dataset_M_724_clean.xlsx"
)
write.csv(
M,
file = "dataset_M_724_clean.csv",
row.names = FALSE
)

############################################################

