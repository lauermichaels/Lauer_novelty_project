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
  
# Merge with NIH PMIDs -- I generated a list of NIH PMIDs from ExPORTER (see code below)
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

########################################################################################

## Appendix: Code to generate NIH-funded PMIDs

########################################################################################

# Identify NIH intramural and extramural projects

# Import ExPORTER files and keep needed fields

RePORTER_PRJ_C_FY1985 <- read_csv("RePORTER_PRJ_C_FY1985.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1986 <- read_csv("RePORTER_PRJ_C_FY1986.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1987 <- read_csv("RePORTER_PRJ_C_FY1987.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1988 <- read_csv("RePORTER_PRJ_C_FY1988.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1989 <- read_csv("RePORTER_PRJ_C_FY1989.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1990 <- read_csv("RePORTER_PRJ_C_FY1990.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1991 <- read_csv("RePORTER_PRJ_C_FY1991.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1992 <- read_csv("RePORTER_PRJ_C_FY1992.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1993 <- read_csv("RePORTER_PRJ_C_FY1993.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1994 <- read_csv("RePORTER_PRJ_C_FY1994.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1995 <- read_csv("RePORTER_PRJ_C_FY1995.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1996 <- read_csv("RePORTER_PRJ_C_FY1996.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1997 <- read_csv("RePORTER_PRJ_C_FY1997.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1998 <- read_csv("RePORTER_PRJ_C_FY1998.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY1999 <- read_csv("RePORTER_PRJ_C_FY1999.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2000 <- read_csv("RePORTER_PRJ_C_FY2000.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2001 <- read_csv("RePORTER_PRJ_C_FY2001.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2002 <- read_csv("RePORTER_PRJ_C_FY2002.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2003 <- read_csv("RePORTER_PRJ_C_FY2003.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2004 <- read_csv("RePORTER_PRJ_C_FY2004.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2005 <- read_csv("RePORTER_PRJ_C_FY2005.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2006 <- read_csv("RePORTER_PRJ_C_FY2006.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2007 <- read_csv("RePORTER_PRJ_C_FY2007.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2008 <- read_csv("RePORTER_PRJ_C_FY2008.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2009 <- read_csv("RePORTER_PRJ_C_FY2009.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2010 <- read_csv("RePORTER_PRJ_C_FY2010.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2011 <- read_csv("RePORTER_PRJ_C_FY2011.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2012 <- read_csv("RePORTER_PRJ_C_FY2012.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2013 <- read_csv("RePORTER_PRJ_C_FY2013.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2014 <- read_csv("RePORTER_PRJ_C_FY2014.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2015 <- read_csv("RePORTER_PRJ_C_FY2015.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2016 <- read_csv("RePORTER_PRJ_C_FY2016.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2017 <- read_csv("RePORTER_PRJ_C_FY2017.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2018 <- read_csv("RePORTER_PRJ_C_FY2018.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2019 <- read_csv("RePORTER_PRJ_C_FY2019.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2020 <- read_csv("RePORTER_PRJ_C_FY2020.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2021 <- read_csv("RePORTER_PRJ_C_FY2021.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2022 <- read_csv("RePORTER_PRJ_C_FY2022.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2023 <- read_csv("RePORTER_PRJ_C_FY2023.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2024 <- read_csv("RePORTER_PRJ_C_FY2024.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

RePORTER_PRJ_C_FY2025 <- read_csv("RePORTER_PRJ_C_FY2025.csv") %>% clean_names() %>%
  select(activity, administering_ic, core_project_num, ic_name)

# Bring together all projects and then identify those that are NIH

all_projects<-bind_rows(
  RePORTER_PRJ_C_FY1985, RePORTER_PRJ_C_FY1986, RePORTER_PRJ_C_FY1987, RePORTER_PRJ_C_FY1988, RePORTER_PRJ_C_FY1989,
  RePORTER_PRJ_C_FY1990, RePORTER_PRJ_C_FY1991, RePORTER_PRJ_C_FY1992, RePORTER_PRJ_C_FY1993, RePORTER_PRJ_C_FY1994,
  RePORTER_PRJ_C_FY1995, RePORTER_PRJ_C_FY1996, RePORTER_PRJ_C_FY1997, RePORTER_PRJ_C_FY1998, RePORTER_PRJ_C_FY1999,
  RePORTER_PRJ_C_FY2000, RePORTER_PRJ_C_FY2001, RePORTER_PRJ_C_FY2002, RePORTER_PRJ_C_FY2003, RePORTER_PRJ_C_FY2004,
  RePORTER_PRJ_C_FY2005, RePORTER_PRJ_C_FY2006, RePORTER_PRJ_C_FY2007, RePORTER_PRJ_C_FY2008, RePORTER_PRJ_C_FY2009,
  RePORTER_PRJ_C_FY2010, RePORTER_PRJ_C_FY2011, RePORTER_PRJ_C_FY2012, RePORTER_PRJ_C_FY2013, RePORTER_PRJ_C_FY2014,
  RePORTER_PRJ_C_FY2015, RePORTER_PRJ_C_FY2016, RePORTER_PRJ_C_FY2017, RePORTER_PRJ_C_FY2018, RePORTER_PRJ_C_FY2019,
  RePORTER_PRJ_C_FY2020, RePORTER_PRJ_C_FY2021, RePORTER_PRJ_C_FY2022, RePORTER_PRJ_C_FY2023, RePORTER_PRJ_C_FY2024,
  RePORTER_PRJ_C_FY2025
  ) %>% distinct()

# Make a table of ic names and abbreviations

ic_table <- all_projects %>% select(administering_ic, ic_name) %>%
  distinct() %>% arrange(administering_ic)

write.csv(ic_table, "ic_table.csv", row.names = FALSE)

nih_projects <- all_projects %>%
  filter(administering_ic %in% 
           c("RR","TW","AT","CA","EY","HG","HL","HG","AG","AA",
             "AI","AR","EB","HD","DA","DC","DE","DK","ES","GM",
             "MH","MD","NS","NR","LM","OD","TR","AO","BC","BU",
             "CL","CM","CN","CP","CT","HB","HC","HO","HR","HV",
             "NU","PC","RG","RS","SC","WH")) %>%
  select(core_project_num) %>% distinct() %>%
  rename(project_number = core_project_num)

# Now list of PMIDs along with project numbers

RePORTER_PUBLNK_C_FY2025 <- read_csv("RePORTER_PUBLNK_C_FY2025.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_FY2024 <- read_csv("RePORTER_PUBLNK_C_FY2024.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_FY2023 <- read_csv("RePORTER_PUBLNK_C_FY2023.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_FY2022 <- read_csv("RePORTER_PUBLNK_C_FY2022.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2021 <- read_csv("RePORTER_PUBLNK_C_2021.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2020 <- read_csv("RePORTER_PUBLNK_C_2020.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2019 <- read_csv("RePORTER_PUBLNK_C_2019.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2018 <- read_csv("RePORTER_PUBLNK_C_2018.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2017 <- read_csv("RePORTER_PUBLNK_C_2017.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2016 <- read_csv("RePORTER_PUBLNK_C_2016.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2015 <- read_csv("RePORTER_PUBLNK_C_2015.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2014 <- read_csv("RePORTER_PUBLNK_C_2014.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2013 <- read_csv("RePORTER_PUBLNK_C_2013.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2012 <- read_csv("RePORTER_PUBLNK_C_2012.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2011 <- read_csv("RePORTER_PUBLNK_C_2011.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_2010 <- read_csv("RePORTER_PUBLNK_C_2010.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2009 <- read_csv("RePORTER_PUBLNK_C_2009.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2008 <- read_csv("RePORTER_PUBLNK_C_2008.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2007 <- read_csv("RePORTER_PUBLNK_C_2007.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2006 <- read_csv("RePORTER_PUBLNK_C_2006.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_2005 <- read_csv("RePORTER_PUBLNK_C_2005.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2004 <- read_csv("RePORTER_PUBLNK_C_2004.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2003 <- read_csv("RePORTER_PUBLNK_C_2003.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2002 <- read_csv("RePORTER_PUBLNK_C_2002.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_2001 <- read_csv("RePORTER_PUBLNK_C_2001.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_2000 <- read_csv("RePORTER_PUBLNK_C_2000.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1999 <- read_csv("RePORTER_PUBLNK_C_1999.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1998 <- read_csv("RePORTER_PUBLNK_C_1998.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1997 <- read_csv("RePORTER_PUBLNK_C_1997.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1996 <- read_csv("RePORTER_PUBLNK_C_1996.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_1995 <- read_csv("RePORTER_PUBLNK_C_1995.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1994 <- read_csv("RePORTER_PUBLNK_C_1994.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1993 <- read_csv("RePORTER_PUBLNK_C_1993.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1992 <- read_csv("RePORTER_PUBLNK_C_1992.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1991 <- read_csv("RePORTER_PUBLNK_C_1991.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_1990 <- read_csv("RePORTER_PUBLNK_C_1990.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1989 <- read_csv("RePORTER_PUBLNK_C_1989.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1988 <- read_csv("RePORTER_PUBLNK_C_1988.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1987 <- read_csv("RePORTER_PUBLNK_C_1987.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1986 <- read_csv("RePORTER_PUBLNK_C_1986.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_1985 <- read_csv("RePORTER_PUBLNK_C_1985.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1984 <- read_csv("RePORTER_PUBLNK_C_1984.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1983 <- read_csv("RePORTER_PUBLNK_C_1983.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1982 <- read_csv("RePORTER_PUBLNK_C_1982.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

RePORTER_PUBLNK_C_1981 <- read_csv("RePORTER_PUBLNK_C_1981.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()


RePORTER_PUBLNK_C_1980 <- read_csv("RePORTER_PUBLNK_C_1980.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct()

all_pmids<-bind_rows(
  RePORTER_PUBLNK_C_1980, RePORTER_PUBLNK_C_1981, RePORTER_PUBLNK_C_1982, RePORTER_PUBLNK_C_1983, RePORTER_PUBLNK_C_1984,
  RePORTER_PUBLNK_C_1985, RePORTER_PUBLNK_C_1986, RePORTER_PUBLNK_C_1987, RePORTER_PUBLNK_C_1988, RePORTER_PUBLNK_C_1989,
  RePORTER_PUBLNK_C_1990, RePORTER_PUBLNK_C_1991, RePORTER_PUBLNK_C_1992, RePORTER_PUBLNK_C_1993, RePORTER_PUBLNK_C_1994,
  RePORTER_PUBLNK_C_1995, RePORTER_PUBLNK_C_1996, RePORTER_PUBLNK_C_1997, RePORTER_PUBLNK_C_1998, RePORTER_PUBLNK_C_1999,
  RePORTER_PUBLNK_C_2000, RePORTER_PUBLNK_C_2001, RePORTER_PUBLNK_C_2002, RePORTER_PUBLNK_C_2003, RePORTER_PUBLNK_C_2004,
  RePORTER_PUBLNK_C_2005, RePORTER_PUBLNK_C_2006, RePORTER_PUBLNK_C_2007, RePORTER_PUBLNK_C_2008, RePORTER_PUBLNK_C_2009,
  RePORTER_PUBLNK_C_2010, RePORTER_PUBLNK_C_2011, RePORTER_PUBLNK_C_2012, RePORTER_PUBLNK_C_2013, RePORTER_PUBLNK_C_2014,
  RePORTER_PUBLNK_C_2015, RePORTER_PUBLNK_C_2016, RePORTER_PUBLNK_C_2017, RePORTER_PUBLNK_C_2018, RePORTER_PUBLNK_C_2019,
  RePORTER_PUBLNK_C_2020, RePORTER_PUBLNK_C_2021, RePORTER_PUBLNK_C_FY2022, RePORTER_PUBLNK_C_FY2023, RePORTER_PUBLNK_C_FY2024,
  RePORTER_PUBLNK_C_FY2025
) %>% distinct()

# Now left_join NIH project numbers to the PMIDs

nih_project_pmids <- nih_projects %>% left_join(all_pmids) %>% distinct() %>%
  filter(!is.na(pmid)) %>%
  select(pmid) %>% distinct() %>%
  mutate(funding_NIH=1)

# Save result in .RData and .parquet formats

save(nih_project_pmids, file="NIH_pmids_2_27_26.RData")
write_parquet(nih_project_pmids, "NIH_pmids_2_27_26.parquet")






