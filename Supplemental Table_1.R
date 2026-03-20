# Supplemental Table 1 -- characteristics by NIH funding -- nonmissing values for Atyp pairs

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(gtsummary) # summary tables
library(gt) # summary tables -- export

setwd("~")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

df_tbl <-
  df_opa_sciscinet_2001_2022 %>%
  
  filter(!is.na(Atyp_10pct_Z)) %>% # Exclude observations with missing values for Atypical Pairs
  
  select(
    Funding,
    author_count_opa, institution_count, reference_count,
    Atyp_10pct_Z, Atyp_Median_Z,
    Novelty_Type, Science_Type, 
    relative_citation_ratio, Highly_Cited
  ) %>%
  
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower")                       )
  ) %>%
  
  mutate(
    Science_Type = factor(Science_Type, levels = c("Fundamental","Mixed","Human-focused"))
  )

# Functions for extreme values

mean_trimmed <- function(x) mean(x, trim = 0.01, na.rm = TRUE)
sd_trimmed <- function(x, trim = 0.01) {
  # Remove NA values first to ensure trimming works correctly
  x_clean <- x[!is.na(x)]
  
  # Trim the data
  n <- length(x_clean)
  lo <- floor(n * trim) + 1
  hi <- n - lo + 1
  x_sorted <- sort(x_clean)
  x_trimmed <- x_sorted[lo:hi]
  
  # Calculate sd on trimmed data
  sd(x_trimmed)
}

tbl_by_funding <-
  df_tbl %>%
  select(
    Funding,  # keep the grouping var first
    author_count_opa,
    institution_count,
    reference_count,
    Atyp_10pct_Z, Atyp_Median_Z,
    Novelty_Type, Science_Type
  ) %>%
  tbl_summary(
    by = Funding,
    type = list(
      author_count_opa ~ "continuous",
      institution_count ~ "continuous",
      reference_count ~ "continuous",
      Atyp_10pct_Z ~ "continuous",
      Atyp_Median_Z ~ "continuous",
      Novelty_Type ~ "categorical",
      Science_Type ~ "categorical"
    ),
    label = list(
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
          c(
            "author_count_opa","institution_count","reference_count",
            "Atyp_10pct_Z", "Atyp_Median_Z",
            "Novelty_Type", "Science_Type"
          )
        )
      ) %>%
      arrange(.var_rank, row_type) %>%
      select(-.var_rank)
  )%>%
  modify_caption("<div style='text-align: left;'>Table 1: Characteristics of Papers According to Funding") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Funding**") %>% 
  modify_footnote(
    all_stat_cols() ~ "Mean (SD); n (%)") %>%
  bold_labels()

# convert to gt for extra styling / export
tbl_gt_supp_Table_1 <- as_gt(tbl_by_funding) %>%
  gt::tab_options(table.font.names = "Times New Roman")
tbl_gt_supp_Table_1

# Save table
gtsave(tbl_gt_supp_Table_1, "Supplemental Table 1 3 3 26.html")
