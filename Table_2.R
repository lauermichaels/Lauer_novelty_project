# Table 2 - characteristics by year

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(gtsummary) # summary tables
library(gt) # summary tables -- export

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

theme_gtsummary_compact()

df_tbl_2 <-
  df_opa_sciscinet_2001_2022 %>%
  
  select(year,
         NIH_funding,
         author_count_opa, institution_count, reference_count,
         Atyp_10pct_Z, Atyp_Median_Z,
         Novelty_Type, Science_Type
  ) %>%
  
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower","Missing")                       )
  ) %>%
  
  mutate(
    Science_Type = factor(Science_Type, levels = c("Fundamental","Mixed","Human-focused"))
  ) %>%
  
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2004 ~ "2001-2004",
      year >= 2005 & year <= 2009 ~ "2005-2009",
      year >= 2010 & year <= 2014 ~ "2010-2014",
      year >= 2015 & year <= 2019 ~ "2015-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

tbl_2 <-
  df_tbl_2 %>%
  select(year_group,
         NIH_funding,
         author_count_opa,
         institution_count,
         reference_count,
         Atyp_10pct_Z, Atyp_Median_Z,
         Novelty_Type, Science_Type
  ) %>%
  tbl_summary(
    by = year_group,
    type = list(
      NIH_funding ~ "dichotomous",
      author_count_opa ~ "continuous",
      institution_count ~ "continuous",
      reference_count ~ "continuous",
      Atyp_10pct_Z ~ "continuous",
      Atyp_Median_Z ~ "continuous",
      Novelty_Type ~ "categorical",
      Science_Type ~ "categorical"
    ),
    value = list(
      NIH_funding ~ TRUE
    ),
    label = list(
      NIH_funding ~ "NIH Funded",
      author_count_opa ~ "Number of Authors",
      institution_count ~ "Number of Institutions",
      reference_count ~ "Number of References",
      Atyp_10pct_Z ~ "10th Percentile Z-score",
      Atyp_Median_Z ~ "Median Z-score",
      Novelty_Type ~ "Article Type",
      Science_Type ~ "Science Type"
    ),
    statistic = list(
      statistic = all_continuous() ~ "{mean_trimmed} ({sd_trimmed})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1,1),
      all_dichotomous() ~ c(0, 0),
      all_categorical() ~ c(0, 0)
    ),
    
    missing = "no",
    sort=NULL
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        .var_rank = match(
          variable,
          c("NIH_funding",
            "author_count_opa","institution_count","reference_count",
            "Atyp_10pct_Z", "Atyp_Median_Z",
            "Novelty_Type", "Science_Type"
          )
        )
      ) %>%
      arrange(.var_rank, row_type) %>%
      select(-.var_rank)
  )%>%
  modify_caption("<div style='text-align: left;'>Table 2: Characteristics of Papers According to Year</div>") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Publication Year**") %>% 
  modify_footnote(
    all_stat_cols() ~ "Mean (SD); n (%)") %>%
  bold_labels()

# convert to gt for extra styling / export
tbl_gt_Table_2 <- as_gt(tbl_2) %>%
  gt::tab_options(table.font.names = "Times New Roman")
tbl_gt_Table_2

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

# Save table
gtsave(tbl_gt_Table_2, "Table 2 3 1 26.html")
