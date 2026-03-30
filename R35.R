# NIGMS R35 analyses

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots

# Identify NIGMS R35 papers -- read Publink files for papers published after 2017 and limit
# to those with project numbers indicating NIGMS R35

setwd("~/Dropbox/Book 2025/Publication link files")

R35_RePORTER_PUBLNK_C_FY2025 <- read_csv("RePORTER_PUBLNK_C_FY2025.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_FY2024 <- read_csv("RePORTER_PUBLNK_C_FY2024.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_FY2023 <- read_csv("RePORTER_PUBLNK_C_FY2023.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_FY2022 <- read_csv("RePORTER_PUBLNK_C_FY2022.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_2021 <- read_csv("RePORTER_PUBLNK_C_2021.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_2020 <- read_csv("RePORTER_PUBLNK_C_2020.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_2019 <- read_csv("RePORTER_PUBLNK_C_2019.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_2018 <- read_csv("RePORTER_PUBLNK_C_2018.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

R35_RePORTER_PUBLNK_C_2017 <- read_csv("RePORTER_PUBLNK_C_2017.csv") %>% clean_names %>%
  select(project_number, pmid) %>% distinct() %>%
  filter(substr(project_number,1,3)=="R35") %>%
  filter(substr(project_number,4,5)=="GM")

# Stack and pull out unique pmids

all_R35_pmids<-bind_rows(
  R35_RePORTER_PUBLNK_C_2017, R35_RePORTER_PUBLNK_C_2018, R35_RePORTER_PUBLNK_C_2019,
  R35_RePORTER_PUBLNK_C_2020, R35_RePORTER_PUBLNK_C_2021, R35_RePORTER_PUBLNK_C_FY2022, 
  R35_RePORTER_PUBLNK_C_FY2023, R35_RePORTER_PUBLNK_C_FY2024,R35_RePORTER_PUBLNK_C_FY2025
) %>% distinct()

R35_pmids <- all_R35_pmids %>% select(pmid) %>% distinct() %>%
  mutate(GM_R35=1)

# Merge with main data -- NIH publications 2017 and later, nonmissing novelty-conventionality data

main_df_opa_scisinet_for_R35 <- df_opa_sciscinet_2001_2022 %>%
  filter(year>=2017 & !is.na(Atyp_10pct_Z)) %>%
  left_join(R35_pmids) %>%
  mutate(GM_R35 = replace_na(GM_R35, 0)) %>%
  filter(NIH_funding==1) %>%
  mutate(R35_group = case_when(
    GM_R35==1 ~ "NIGMS R35",
    GM_R35==0 ~ "NIH, Not NIGMS R35"
  ))

# Summary table

# Set up table

df_tbl <-
  main_df_opa_scisinet_for_R35 %>%
  
  select(
    R35_group,
    author_count_opa, institution_count, reference_count,
    Atyp_10pct_Z, Atyp_Median_Z,
    Novelty_Type, Science_Type, 
    relative_citation_ratio, Highly_Cited, is_clinical
  ) %>%
  
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower")                       )
  ) %>%
  
  mutate(
    Science_Type = factor(Science_Type, levels = c("Fundamental","Mixed","Human-focused"))
  )

# ---- 3) gtsummary table (Funding as columns) ----

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
    R35_group,  # keep the grouping var first
    author_count_opa,
    institution_count,
    reference_count,
    Atyp_10pct_Z, Atyp_Median_Z,
    Novelty_Type, Science_Type, is_clinical, Highly_Cited
  ) %>%
  tbl_summary(
    by = R35_group,
    type = list(
      author_count_opa ~ "continuous",
      institution_count ~ "continuous",
      reference_count ~ "continuous",
      Atyp_10pct_Z ~ "continuous",
      Atyp_Median_Z ~ "continuous",
      Novelty_Type ~ "categorical",
      Science_Type ~ "categorical",
      is_clinical ~ "dichotomous",
      Highly_Cited ~ "dichotomous"
    ),
    label = list(
      author_count_opa ~ "Number of Authors",
      institution_count ~ "Number of Institutions",
      reference_count ~ "Number of References",
      Atyp_10pct_Z ~ "10th Percentile Z-score",
      Atyp_Median_Z ~ "Median Z-score",
      Novelty_Type ~ "Article Type",
      Science_Type ~ "Science Type",
      is_clinical ~ "Clinical Trial",
      Highly_Cited ~ "Highly Cited"
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
            "Atyp_10pct_Z", "Atyp_Median_Z","Science_Type","is_clinical",
            "Novelty_Type", "Highly_Cited"
          )
        )
      ) %>%
      arrange(.var_rank, row_type) %>%
      select(-.var_rank)
  )%>%
  modify_caption("<div style='text-align: left;'>Table: Features of NIH-supported Papers Published 2017-2022 According to Funding (R35 Analyses)") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Funding**") %>% 
  modify_footnote(
    all_stat_cols() ~ "Mean (SD); n (%)") %>%
  bold_labels()

# convert to gt for extra styling / export
R35_tbl_gt_Table_1 <- as_gt(tbl_by_funding) %>%
  gt::tab_options(table.font.names = "Times New Roman")
R35_tbl_gt_Table_1

# Save table
gtsave(R35_tbl_gt_Table_1, "Table R35.html")

# Darwin's Tower logistic regression 

df_analysis <- main_df_opa_scisinet_for_R35 %>%
  mutate(darwin=ifelse(Novelty_Type=="Darwin's Tower",1,0)) %>%
  mutate(year_c = year-2020) %>%
  mutate(R35_group = factor(R35_group, levels=c("NIH, Not NIGMS R35","NIGMS R35"))) %>%
  mutate(
    Highly_Cited_num = ifelse(Highly_Cited==TRUE, 1, 0)
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
    ))

fit_logit_R35_full_darwin <- glm(
  darwin ~
    R35_group +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical + R35_group:Science_Type,
  data    = df_analysis,
  family  = binomial()
)

summary(fit_logit_R35_full_darwin)  

# Highly cited logistic regression -- interaction terms for Science Type and Novelty Type were not significant

fit_logit_R35_simpler_cited <- glm(
  Highly_Cited_num ~
    R35_group +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical,
  data    = df_analysis,
  family  = binomial()
)

summary(fit_logit_R35_simpler_cited)  


fit_logit_R35_full_cited <- glm(
  Highly_Cited_num ~
    R35_group +
    Novelty_Type +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical,
  data    = df_analysis,
  family  = binomial()
)

summary(fit_logit_R35_full_cited)  

# Texreg display

coef_labels <- list(
  "(Intercept)"               = "Intercept",
  "R35_groupNIGMS R35"               = "Funded by NIGMS R35",
  "Science_TypeMixed"                 = "Mixed topic",
  "Science_TypeHuman-focused"         = "Human-focused topic",
  "ns(year_c, 3)1"       = "Year (spline 1)",
  "ns(year_c, 3)2"       = "Year (spline 2)",
  "ns(year_c, 3)3"       = "Year (spline 3)",
  "quartile_author_countQ2"                = "Author count (Quartile 2)",
  "quartile_author_countQ3"                = "Author count (Quartile 3)",
  "quartile_author_countQ4"                = "Author count (Quartile 4)",
  "institution_count"         = "Institution count",
  "ns(reference_count, 3)1" = "Reference count (spline 1)",
  "ns(reference_count, 3)2" = "Reference count (spline 2)",
  "ns(reference_count, 3)3" = "Reference count (spline 3)",
  "is_clinicalTRUE"           = "Clinical Trial",
  "R35_groupNIGMS R35:Science_TypeMixed" = "NIGMS R35 X Mixed",
  "R35_groupNIGMS R35:Science_TypeHuman-focused" = "NIGMS R35 X Human-focused",
  "Novelty_TypeAvant-garde"   = "Avant-garde",
  "Novelty_TypeAccepted Wisdom"  = "Accepted Wisdom",
  "Novelty_TypeDarwin's Tower"   = "Darwin's Tower"
)

screenreg(
  list(fit_logit_R35_full_darwin, fit_logit_R35_simpler_cited, fit_logit_R35_full_cited),
  custom.model.names = c("Darwin", "Highly Cited (1)","Highly Cited (2)"),
  custom.coef.map = coef_labels,
  digits = 3
)

htmlreg(
  list(fit_logit_R35_full_darwin, fit_logit_R35_simpler_cited, fit_logit_R35_full_cited),
  custom.model.names = c("Darwin", "Highly Cited (1)","Highly Cited (2)"),
  custom.coef.map = coef_labels,
  digits = 3,
  caption = "",
  file="Table R35 regression.html",
  caption.above = TRUE,
)








