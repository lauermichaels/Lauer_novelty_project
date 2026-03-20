# Data setup

setwd("~")

library(arrow) # Efficiently read and process parquet and other large files
library(tidyverse) # Data manipulation and ggplots
library(duckplyr) # leverage DuckDB

########################################################################################

## NIH Office of Portfolio Analysis (OPA) Data

########################################################################################

# Data from the NIH Office of Portfolio Analysis, downloaded on 2/27/26
# Data at https://doi.org/10.35092/yhjc.c.4586573. 

opa_total <- read_csv_duckdb("icite_metadata.csv",
                             options = list(ignore_errors = TRUE)) %>%
  
# Select needed variables
  
  select(pmid, doi, year, is_research_article, 
         journal, human, animal, molecular_cellular, apt,
         relative_citation_ratio, citation_count,
         is_clinical, authors) %>% distinct()

  opa_total %>% compute_parquet("opa_.parquet")
  
# Merge with NIH PMIDs -- I generated a list of NIH PMIDs from ExPORTER using R script NIH PMIDs 2 27 26.R
# NIH ExPORTER is at https://reporter.nih.gov/exporter.
  # I eliminated PMIDs that are linked to other sponsors like AHRQ, FDA, CDC, and VA
  # It includes NIH intramural and extramural
  # It includes all NIH PMIDs, including papers that OPA does not consider to be research articles
  
opa_ <- read_parquet_duckdb("opa_.parquet")
nih_pmids <- read_parquet_duckdb("NIH_pmids_2_27_26.parquet")
  
opa_ %>%
    left_join(nih_pmids, by = "pmid") %>%
    distinct(pmid, .keep_all = TRUE) %>%
    compute_parquet("opa_use.parquet")

# To enable merge with Sciscinet v2 (via doi), convert doi to lower case
# Count number of authors, then delete memory-using columns

opa_use2<-open_dataset("opa_use.parquet") %>%
  mutate(doi=str_to_lower(doi)) %>%
  mutate(author_count_opa=str_count(authors, pattern = ",")+1) %>%
  select(-authors) 

opa_use2%>%
  write_parquet("opa_use2.parquet")

########################################################################################

## Sciscinet V2 data and merge with OPA data 

########################################################################################

# Read Sciscinet V2 data, limit to articles with nonmissing Atyp_10pct_Z
# Change DOI to lower case to enable merger with Sciscinet V2
# Sciscinet V2 data is at https://huggingface.co/datasets/Northwestern-CSSI/sciscinet-v2/tree/main. 

sciscinet_v2_novelty_1980_2024 <- open_dataset('sciscinet_papers.parquet') %>%
  filter(!is.na(doi)) %>% filter(doctype=="article") %>% filter(!is.na(Atyp_10pct_Z)) %>%
  filter(year>=1980 & year<2025) %>%
  select(doi, Atyp_Median_Z, Atyp_10pct_Z, reference_count, institution_count) %>%
  mutate(doi = str_remove(doi, "https://doi.org/")) %>%
  mutate(doi = str_to_lower(doi))
sciscinet_v2_novelty_1980_2024<-collect(sciscinet_v2_novelty_1980_2024)

# Save as a parquet file

write_parquet(sciscinet_v2_novelty_1980_2024, "sciscinet_v2_novelty_1980_2024.parquet")

# Merge OPA / NIH data with Sciscinet data to create a new file opa_sciscinet.parquet

opa_total_use <- read_parquet_duckdb("opa_use2.parquet")
sciscinet <- read_parquet_duckdb("sciscinet_v2_novelty_1980_2024.parquet")

opa_total_use %>%
  filter(year>=1980 & year <= 2025) %>%
  left_join(sciscinet, by = "doi") %>%
  compute_parquet("opa_sciscinet_2_27_26.parquet")

# There are 3930 duplicates (out of 34 million rows) -- these are due to
# two entries of novelty and conventionality statistics

opa_sciscinet_use <- read_parquet("opa_sciscinet_2_27_26.parquet")

opa_sciscinet_use %>%
  distinct(pmid, .keep_all = TRUE) %>%
  write_parquet("opa_sciscinet_3_1_26.parquet")
  

###################################################################################

## Data specifically needed for Novelty and Conventionality Project

###################################################################################

# Read in original data, which is a merger of OPA, ExPORTER, and Sciscinet V2
# Only includes papers which are in the triangle of biomedicine

# Set up factors

novelty_order  <- c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower","Missing")
science_order  <- c("Fundamental", "Mixed", "Human-focused")

# Read in data

df_opa_sciscinet_2001_2022 <- read_parquet("opa_sciscinet_3_1_26.parquet") %>%
  
# Only research articles
  
  filter(is_research_article==TRUE) %>%
  
  # Limit to 2001 to 2022 and to those in triangle of biomedicine
  # Exclude 2011 due to known anomalies of novelty statistics in that year
  
  filter(year>=2001 & year <= 2022 & year !=2011) %>%
  filter(animal > 0 | human > 0 | molecular_cellular > 0) %>% 
  
  write_parquet("df_opa_sciscinet_2022.parquet")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2022.parquet") %>%

  # Define terms
  
  mutate(year = as.integer(year)) %>%
  mutate(Funding = ifelse(!is.na(funding_NIH), "NIH", "Not NIH")) %>%
  mutate(NIH_funding = ifelse(!is.na(funding_NIH),1,0)) %>%
  mutate(missing_atyp = ifelse(is.na(Atyp_10pct_Z),1,0)) %>%
  mutate(Science_Type = case_when(
    human==1 ~ "Human-focused",
    human==0 ~ "Fundamental",
    human>0 & human < 1 ~ "Mixed"
  )) %>%
  mutate(Novelty_Type = case_when(
    is.na(Atyp_10pct_Z) ~ "Missing",
    Atyp_10pct_Z >= 0 & Atyp_Median_Z <= median(Atyp_Median_Z, na.rm = TRUE) ~ "Platypus",
    Atyp_10pct_Z < 0 & Atyp_Median_Z <= median(Atyp_Median_Z, na.rm = TRUE) ~ "Avant-garde",
    Atyp_10pct_Z >= 0 & Atyp_Median_Z > median(Atyp_Median_Z, na.rm = TRUE) ~ "Accepted Wisdom",
    Atyp_10pct_Z < 0 & Atyp_Median_Z > median(Atyp_Median_Z, na.rm = TRUE) ~ "Darwin's Tower"
  )) %>%
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = novelty_order),
    Science_Type = factor(Science_Type, levels = science_order)
  ) %>%
  mutate(Highly_Cited = (!is.na(relative_citation_ratio) & relative_citation_ratio >= 3.45)) %>%
  
  # Missing authors to median (0.4% missinginess)
  
  mutate(
    author_count_opa = replace(author_count_opa, is.na(author_count_opa), median(author_count_opa, na.rm = TRUE))
  ) %>%
  
  mutate(
    quartile_author_count = cut(
      author_count_opa,
      breaks = quantile(author_count_opa, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    quartile_author_count = factor(
      quartile_author_count,
      levels = c("Q1", "Q2", "Q3", "Q4")
    )) %>%
  
  # Fields we will need
  
  select(pmid, year, journal, Science_Type, Novelty_Type, relative_citation_ratio,
         citation_count, is_clinical, author_count_opa, Atyp_10pct_Z,
         Atyp_Median_Z, reference_count, institution_count, Funding, NIH_funding,
         Highly_Cited, missing_atyp, quartile_author_count)

df_opa_sciscinet_2001_2022 %>%
  write_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

