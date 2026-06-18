# Data setup

setwd("~")

library(arrow) # Efficiently read and process parquet and other large files
library(tidyverse) # Data manipulation and ggplots
library(duckplyr) # leverage DuckDB
library(janitor) # clean names

########################################################################################

## NIH Office of Portfolio Analysis (OPA) Data

########################################################################################

# Data from the NIH Office of Portfolio Analysis, downloaded on 6/11/26,
# Snapshot 2026-04 at https://nih.figshare.com/articles/dataset/iCite_Database_Snapshot_2026-04/32209434?backTo=/collections/iCite_Database_Snapshots_NIH_Open_Citation_Collection_/4586573

opa_total <- read_csv_duckdb("icite_metadata.csv",
                             options = list(ignore_errors = TRUE)) %>%
  
# Select needed variables
  
  select(pmid, doi, year, is_research_article, 
         journal, human, animal, molecular_cellular, apt,
         relative_citation_ratio, citation_count, references,
         is_clinical, authors) %>% distinct()

  opa_total %>% compute_parquet("opa_.parquet")
  
# Merge with NIH PMIDs -- I generated a list of NIH PMIDs from ExPORTER (see code below)
  # This eliminates PMIDs that are linked to other sponsors like AHRQ, FDA, CDC, and VA
  # It includes NIH intramural and extramural
  # It includes all NIH PMIDs, including papers that OPA does not consider to be research articles
  
opa_ <- read_parquet_duckdb("opa_.parquet") %>%
  compute(prudence = "lavish")
nih_pmids <- read_parquet_duckdb("NIH_pmids_6_11_26.parquet") %>%
  compute(prudence = "lavish")
  
opa_ %>%
    left_join(nih_pmids, by = "pmid") %>%
    distinct(pmid, .keep_all = TRUE) %>%
    compute_parquet("opa_use.parquet")

# To enable merge with Sciscinet v2 (via doi), convert doi to lower case
# Count number of authors, then delete memory-using columns

opa_use3<-open_dataset("opa_use.parquet") %>%
  mutate(doi=str_to_lower(doi)) %>%
  mutate(author_count_opa=str_count(authors, pattern = ",")+1) %>%
  mutate(reference_count_opa=str_count(references, pattern = ",")+1) %>%
  select(-authors, -references) 

opa_use3%>%
  write_parquet("opa_use3.parquet")

########################################################################################

## Sciscinet V2 data and merge with OPA data 

########################################################################################

# Read Sciscinet V2 data, limit to articles with nonmissing Atyp_10pct_Z
# Change DOI to lower case to enable merger with Sciscinet V2
# Data obtained from https://huggingface.co/datasets/Northwestern-CSSI/sciscinet-v2/tree/main in November 2025

sciscinet_v2_novelty_1980_2024 <- open_dataset('sciscinet_papers.parquet') %>%
  filter(!is.na(doi)) %>% filter(doctype=="article") %>% filter(!is.na(Atyp_10pct_Z)) %>%
  filter(year>=1980 & year<2025) %>%
  select(doi, Atyp_Median_Z, Atyp_10pct_Z, reference_count, institution_count) %>%
  mutate(doi = str_remove(doi, "https://doi.org/")) %>%
  mutate(doi = str_to_lower(doi))
sciscinet_v2_novelty_1980_2024<-collect(sciscinet_v2_novelty_1980_2024)

# Save as a parquet file

write_parquet(sciscinet_v2_novelty_1980_2024, "sciscinet_v2_novelty_1980_2024.parquet")

# Merge OPA / NIH data with Sciscinet data to create a new file opa_sciscinet_6_18_26.parquet

library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

dbExecute(con, "
COPY (
    SELECT
        opa.*,
        sci.* EXCLUDE (doi)
    FROM read_parquet('opa_use3.parquet') AS opa
    LEFT JOIN read_parquet('sciscinet_v2_novelty_1980_2024.parquet') AS sci
        ON opa.doi = sci.doi
    WHERE opa.year >= 1980
      AND opa.year <= 2025
)
TO 'opa_sciscinet_6_18_26.parquet'
(FORMAT PARQUET)
")

dbDisconnect(con, shutdown = TRUE)

# Assess duplicates

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

duplicate_summary <- dbGetQuery(con, "
    SELECT
        COUNT(*) AS total_rows,
        COUNT(pmid) AS non_null_pmid_rows,
        COUNT(DISTINCT pmid) AS distinct_pmids,
        COUNT(pmid) - COUNT(DISTINCT pmid) AS duplicate_pmid_rows
    FROM read_parquet('opa_sciscinet_6_18_26.parquet')
")

duplicate_summary

# There are ~3930 duplicates (out of 34 million rows) -- these are due to
# two entries of novelty and conventionality statistics

library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

dbExecute(con, "
COPY (
    WITH numbered AS (
        SELECT
            ROW_NUMBER() OVER () AS __rownum,
            *
        FROM read_parquet('opa_sciscinet_6_18_26.parquet')
    )
    SELECT
        * EXCLUDE (__rownum)
    FROM numbered
    QUALIFY
        pmid IS NULL
        OR ROW_NUMBER() OVER (
            PARTITION BY pmid
            ORDER BY __rownum
        ) = 1
)
TO 'opa_sciscinet_6_18_26_pmid_unique.parquet'
(FORMAT PARQUET)
")

# Validation check after deduplication

dedup_check <- dbGetQuery(con, "
    SELECT
        COUNT(*) AS total_rows,
        COUNT(pmid) AS non_null_pmid_rows,
        COUNT(DISTINCT pmid) AS distinct_pmids,
        COUNT(pmid) - COUNT(DISTINCT pmid) AS duplicate_pmid_rows_remaining
    FROM read_parquet('opa_sciscinet_6_18_26_pmid_unique.parquet')
")

dedup_check

# Total rows 33994711
# No missing pmids
# Distinct pmids 33994711
# Duplicate pmids 0
  

###################################################################################

## Data specifically needed for Novelty and Conventionality Project

###################################################################################

# Read in original data, which is a merger of OPA, ExPORTER, and Sciscinet V2
# Only includes papers which are in the triangle of biomedicine

library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

dbExecute(con, "
COPY (
    WITH filtered AS (
        SELECT
            *
        FROM read_parquet('opa_sciscinet_6_18_26.parquet')
        WHERE is_research_article = TRUE
          AND year >= 2001
          AND year <= 2022
          AND year != 2011
          AND (
              animal > 0
              OR human > 0
              OR molecular_cellular > 0
          )
    ),

    medians AS (
        SELECT
            median(Atyp_Median_Z) AS median_atyp_median_z,
            median(author_count_opa) AS median_author_count_opa
        FROM filtered
    ),

    with_terms AS (
        SELECT
            CAST(f.year AS INTEGER) AS year,
            f.pmid,
            f.journal,
            f.relative_citation_ratio,
            f.citation_count,
            f.is_clinical,
            COALESCE(f.author_count_opa, m.median_author_count_opa) AS author_count_opa,
            f.Atyp_10pct_Z,
            f.Atyp_Median_Z,
            f.reference_count,
            f.reference_count_opa,
            f.institution_count,

            CASE
                WHEN f.funding_NIH IS NOT NULL THEN 'NIH'
                ELSE 'Not NIH'
            END AS Funding,

            CASE
                WHEN f.funding_NIH IS NOT NULL THEN 1
                ELSE 0
            END AS NIH_funding,

            CASE
                WHEN f.Atyp_10pct_Z IS NULL THEN 1
                ELSE 0
            END AS missing_atyp,

            CASE
                WHEN f.human = 1 THEN 'Human-focused'
                WHEN f.human = 0 THEN 'Fundamental'
                WHEN f.human > 0 AND f.human < 1 THEN 'Mixed'
                ELSE NULL
            END AS Science_Type,

            CASE
                WHEN f.Atyp_10pct_Z IS NULL THEN 'Missing'
                WHEN f.Atyp_10pct_Z >= 0
                     AND f.Atyp_Median_Z <= m.median_atyp_median_z THEN 'Platypus'
                WHEN f.Atyp_10pct_Z < 0
                     AND f.Atyp_Median_Z <= m.median_atyp_median_z THEN 'Avant-garde'
                WHEN f.Atyp_10pct_Z >= 0
                     AND f.Atyp_Median_Z > m.median_atyp_median_z THEN 'Accepted Wisdom'
                WHEN f.Atyp_10pct_Z < 0
                     AND f.Atyp_Median_Z > m.median_atyp_median_z THEN 'Darwin''s Tower'
                ELSE NULL
            END AS Novelty_Type,

            CASE
                WHEN f.relative_citation_ratio IS NOT NULL
                     AND f.relative_citation_ratio >= 3.45 THEN TRUE
                ELSE FALSE
            END AS Highly_Cited

        FROM filtered AS f
        CROSS JOIN medians AS m
    ),

    author_quantiles AS (
        SELECT
            quantile_cont(author_count_opa, 0.00) AS q0,
            quantile_cont(author_count_opa, 0.25) AS q25,
            quantile_cont(author_count_opa, 0.50) AS q50,
            quantile_cont(author_count_opa, 0.75) AS q75,
            quantile_cont(author_count_opa, 1.00) AS q100
        FROM with_terms
    ),

    final AS (
        SELECT
            w.pmid,
            w.year,
            w.journal,
            w.Science_Type,
            w.Novelty_Type,
            w.relative_citation_ratio,
            w.citation_count,
            w.is_clinical,
            w.author_count_opa,
            w.reference_count_opa,
            w.Atyp_10pct_Z,
            w.Atyp_Median_Z,
            w.reference_count,
            w.institution_count,
            w.Funding,
            w.NIH_funding,
            w.Highly_Cited,
            w.missing_atyp,

            CASE
                WHEN w.author_count_opa >= q.q0
                     AND w.author_count_opa <= q.q25 THEN 'Q1'
                WHEN w.author_count_opa > q.q25
                     AND w.author_count_opa <= q.q50 THEN 'Q2'
                WHEN w.author_count_opa > q.q50
                     AND w.author_count_opa <= q.q75 THEN 'Q3'
                WHEN w.author_count_opa > q.q75
                     AND w.author_count_opa <= q.q100 THEN 'Q4'
                ELSE NULL
            END AS quartile_author_count

        FROM with_terms AS w
        CROSS JOIN author_quantiles AS q
    )

    SELECT
        *
    FROM final
)
TO 'df_opa_sciscinet_2001_2022_6_18_26.parquet'
(FORMAT PARQUET)
")

# Validity check

df_check <- dbGetQuery(con, "
    SELECT
        COUNT(*) AS total_rows,
        MIN(year) AS min_year,
        MAX(year) AS max_year,
        SUM(CASE WHEN year = 2011 THEN 1 ELSE 0 END) AS rows_from_2011,
        SUM(NIH_funding) AS nih_funded_rows,
        SUM(missing_atyp) AS missing_atyp_rows,
        SUM(CASE WHEN quartile_author_count IS NULL THEN 1 ELSE 0 END) AS missing_author_quartile_rows
    FROM read_parquet('df_opa_sciscinet_2001_2022_6_18_26.parquet')
")

df_check

# NB -- code for factor levels, since duckDB/Parquet stores as character columns

library(arrow)
library(dplyr)

novelty_order <- c(
  "Platypus",
  "Avant-garde",
  "Accepted Wisdom",
  "Darwin's Tower",
  "Missing"
)

science_order <- c(
  "Fundamental",
  "Mixed",
  "Human-focused"
)

df_opa_sciscinet_2001_2022 <- read_parquet(
  "df_opa_sciscinet_2001_2022_6_18_26.parquet"
) %>%
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = novelty_order),
    Science_Type = factor(Science_Type, levels = science_order),
    quartile_author_count = factor(
      quartile_author_count,
      levels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

dbDisconnect(con, shutdown = TRUE)

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
# I went through these manually to make sure I was pulling out only those from NIH (not FDA, VA, AHRQ, etc)
# Besides the IC abbreviations (e.g. CA for NCI and HL for NHLBI) some of these are NIH intrmaural units

ic_table <- all_projects %>% select(administering_ic, ic_name) %>%
  distinct() %>% arrange(administering_ic)

write.csv(ic_table, "ic_table.csv", row.names = FALSE)

# Isolate the NIH projects (i.e., not projects from AHRQ, VA, CDC, FDA ...)

nih_projects <- all_projects %>%
  filter(administering_ic %in% 
           c("RR","TW","AT","CA","EY","HG","HL","HG","AG","AA",
             "AI","AR","EB","HD","DA","DC","DE","DK","ES","GM",
             "MH","MD","NS","NR","LM","OD","TR","AO","BC","BU",
             "CL","CM","CN","CP","CT","HB","HC","HO","HR","HV",
             "NU","PC","RG","RS","SC","WH")) %>%
  select(core_project_num) %>% distinct() %>%
  rename(project_number = core_project_num)

# Next pull all project numbers from publink files from 1980 to 1984 and assume that all
# these are NIH projects

RePORTER_PUBLNK_C_1984 <- read_csv("RePORTER_PUBLNK_C_1984.csv") %>% clean_names %>%
  select(project_number) %>% distinct()

RePORTER_PUBLNK_C_1983 <- read_csv("RePORTER_PUBLNK_C_1983.csv") %>% clean_names %>%
  select(project_number) %>% distinct()

RePORTER_PUBLNK_C_1982 <- read_csv("RePORTER_PUBLNK_C_1982.csv") %>% clean_names %>%
  select(project_number) %>% distinct()

RePORTER_PUBLNK_C_1981 <- read_csv("RePORTER_PUBLNK_C_1981.csv") %>% clean_names %>%
  select(project_number) %>% distinct()

RePORTER_PUBLNK_C_1980 <- read_csv("RePORTER_PUBLNK_C_1980.csv") %>% clean_names %>%
  select(project_number) %>% distinct()

nih_projects_1980_1984 <- bind_rows(RePORTER_PUBLNK_C_1980, RePORTER_PUBLNK_C_1981,
                                    RePORTER_PUBLNK_C_1982, RePORTER_PUBLNK_C_1983, RePORTER_PUBLNK_C_1984) %>%
  distinct()

nih_projects_use <- bind_rows(nih_projects, nih_projects_1980_1984) %>%
  distinct()

# Now list of PMIDs along with project numbers from the ExPORTER publink files

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

nih_project_pmids <- nih_projects_use %>% left_join(all_pmids) %>% distinct() %>%
  filter(!is.na(pmid)) %>%
  select(pmid) %>% distinct() %>%
  mutate(funding_NIH=1)

# Save result in .RData and .parquet formats

write_parquet(nih_project_pmids, "NIH_pmids_6_11_26.parquet")

