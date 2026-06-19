# Table 2 -- characteristics by year
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)
library(gt)

# Reuse existing DuckDB connection if one is already open
if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")
  
  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"

# Create a lean temporary DuckDB table

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE table2_base AS
SELECT
    CASE
        WHEN year >= 2001 AND year <= 2004 THEN '2001-2004'
        WHEN year >= 2005 AND year <= 2009 THEN '2005-2009'
        WHEN year >= 2010 AND year <= 2014 THEN '2010-2014'
        WHEN year >= 2015 AND year <= 2019 THEN '2015-2019'
        WHEN year >= 2020 AND year <= 2022 THEN '2020-2022'
        ELSE 'Other'
    END AS year_group,
    NIH_funding,
    author_count_opa,
    institution_count,
    reference_count,
    Atyp_10pct_Z,
    Atyp_Median_Z,
    Novelty_Type,
    Science_Type,
    is_clinical
FROM read_parquet('%s')
", input_file))

# Compute continuous summaries in DuckDB

continuous_summary_t2 <- dbGetQuery(con, "
    WITH long_continuous AS (
        SELECT
            year_group,
            'author_count_opa' AS variable,
            'Number of Authors' AS label,
            CAST(author_count_opa AS DOUBLE) AS value,
            2 AS var_order
        FROM table2_base
        WHERE author_count_opa IS NOT NULL
          AND year_group IS NOT NULL

        UNION ALL

        SELECT
            year_group,
            'institution_count' AS variable,
            'Number of Institutions' AS label,
            CAST(institution_count AS DOUBLE) AS value,
            3 AS var_order
        FROM table2_base
        WHERE institution_count IS NOT NULL
          AND year_group IS NOT NULL

        UNION ALL

        SELECT
            year_group,
            'reference_count' AS variable,
            'Number of References' AS label,
            CAST(reference_count AS DOUBLE) AS value,
            4 AS var_order
        FROM table2_base
        WHERE reference_count IS NOT NULL
          AND year_group IS NOT NULL

        UNION ALL

        SELECT
            year_group,
            'Atyp_10pct_Z' AS variable,
            '10th Percentile Z-score' AS label,
            CAST(Atyp_10pct_Z AS DOUBLE) AS value,
            5 AS var_order
        FROM table2_base
        WHERE Atyp_10pct_Z IS NOT NULL
          AND year_group IS NOT NULL

        UNION ALL

        SELECT
            year_group,
            'Atyp_Median_Z' AS variable,
            'Median Z-score' AS label,
            CAST(Atyp_Median_Z AS DOUBLE) AS value,
            6 AS var_order
        FROM table2_base
        WHERE Atyp_Median_Z IS NOT NULL
          AND year_group IS NOT NULL
    ),

    ranked AS (
        SELECT
            *,
            ROW_NUMBER() OVER (
                PARTITION BY year_group, variable
                ORDER BY value
            ) AS rn,
            COUNT(*) OVER (
                PARTITION BY year_group, variable
            ) AS n_total
        FROM long_continuous
    ),

    trimmed AS (
        SELECT
            *
        FROM ranked
        WHERE rn >= FLOOR(n_total * 0.01) + 1
          AND rn <= n_total - FLOOR(n_total * 0.01)
    )

    SELECT
        year_group,
        variable,
        label,
        var_order,
        AVG(value) AS mean_trimmed,
        STDDEV_SAMP(value) AS sd_trimmed,
        COUNT(*) AS n_trimmed
    FROM trimmed
    GROUP BY
        year_group,
        variable,
        label,
        var_order
")

# Compute dichotomous and categorical summaries in DuckDB

categorical_summary_t2 <- dbGetQuery(con, "
    WITH nih_counts AS (
        SELECT
            year_group,
            'NIH_funding' AS variable,
            'NIH Funded' AS label,
            'TRUE' AS level,
            1 AS var_order,
            1 AS level_order,
            SUM(
                CASE
                    WHEN NIH_funding = 1 THEN 1
                    ELSE 0
                END
            ) AS n,
            COUNT(*) AS denom
        FROM table2_base
        WHERE NIH_funding IS NOT NULL
          AND year_group IS NOT NULL
        GROUP BY year_group
    ),

    categorical_long AS (
        SELECT
            year_group,
            'Novelty_Type' AS variable,
            'Article Type' AS label,
            Novelty_Type AS level,
            7 AS var_order,
            CASE Novelty_Type
                WHEN 'Platypus' THEN 1
                WHEN 'Avant-garde' THEN 2
                WHEN 'Accepted Wisdom' THEN 3
                WHEN 'Darwin''s Tower' THEN 4
                WHEN 'Missing' THEN 5
                ELSE 6
            END AS level_order
        FROM table2_base
        WHERE Novelty_Type IS NOT NULL
          AND year_group IS NOT NULL

        UNION ALL

        SELECT
            year_group,
            'Science_Type' AS variable,
            'Science Type' AS label,
            Science_Type AS level,
            8 AS var_order,
            CASE Science_Type
                WHEN 'Fundamental' THEN 1
                WHEN 'Mixed' THEN 2
                WHEN 'Human-focused' THEN 3
                ELSE 4
            END AS level_order
        FROM table2_base
        WHERE Science_Type IS NOT NULL
          AND year_group IS NOT NULL
    ),

    categorical_counts AS (
        SELECT
            year_group,
            variable,
            label,
            level,
            var_order,
            level_order,
            COUNT(*) AS n,
            SUM(COUNT(*)) OVER (
                PARTITION BY year_group, variable
            ) AS denom
        FROM categorical_long
        GROUP BY
            year_group,
            variable,
            label,
            level,
            var_order,
            level_order
    ),

    clinical_counts AS (
        SELECT
            year_group,
            'is_clinical' AS variable,
            'Clinical Trial' AS label,
            'TRUE' AS level,
            9 AS var_order,
            1 AS level_order,
            SUM(
                CASE
                    WHEN is_clinical = TRUE THEN 1
                    ELSE 0
                END
            ) AS n,
            COUNT(*) AS denom
        FROM table2_base
        WHERE is_clinical IS NOT NULL
          AND year_group IS NOT NULL
        GROUP BY year_group
    )

    SELECT
        year_group,
        variable,
        label,
        level,
        var_order,
        level_order,
        n,
        denom,
        n::DOUBLE / denom::DOUBLE AS prop
    FROM nih_counts

    UNION ALL

    SELECT
        year_group,
        variable,
        label,
        level,
        var_order,
        level_order,
        n,
        denom,
        n::DOUBLE / denom::DOUBLE AS prop
    FROM categorical_counts

    UNION ALL

    SELECT
        year_group,
        variable,
        label,
        level,
        var_order,
        level_order,
        n,
        denom,
        n::DOUBLE / denom::DOUBLE AS prop
    FROM clinical_counts
")

# Format summaries into a gt table body

continuous_table_t2 <- continuous_summary_t2 %>%
  mutate(
    stat = paste0(
      sprintf("%.1f", mean_trimmed),
      " (",
      sprintf("%.1f", sd_trimmed),
      ")"
    ),
    row_label = label,
    row_type = "continuous",
    level_order = 0
  ) %>%
  select(
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    year_group,
    stat
  )

categorical_table_t2 <- categorical_summary_t2 %>%
  mutate(
    stat = paste0(
      scales::comma(n, accuracy = 1),
      " (",
      sprintf("%.0f", prop * 100),
      "%)"
    ),
    row_label = ifelse(
      variable %in% c("NIH_funding", "is_clinical"),
      label,
      paste0("  ", level)
    ),
    row_type = "categorical"
  ) %>%
  select(
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    year_group,
    stat
  )

table2_body_long <- bind_rows(
  continuous_table_t2,
  categorical_table_t2
)

# Pivot year groups into columns

table2_body_wide <- table2_body_long %>%
  mutate(
    year_group = factor(
      year_group,
      levels = c(
        "2001-2004",
        "2005-2009",
        "2010-2014",
        "2015-2019",
        "2020-2022",
        "Other"
      )
    )
  ) %>%
  arrange(var_order, level_order, row_type) %>%
  select(row_label, year_group, stat, var_order, level_order) %>%
  pivot_wider(
    names_from = year_group,
    values_from = stat
  ) %>%
  arrange(var_order, level_order) %>%
  select(
    Variable = row_label,
    `2001-2004`,
    `2005-2009`,
    `2010-2014`,
    `2015-2019`,
    `2020-2022`
  )

table2_body_wide

# Create and save Table 2

tbl_gt_Table_2 <- table2_body_wide %>%
  gt() %>%
  tab_header(
    title = md("**Table 2: Characteristics of Papers According to Year**")
  ) %>%
  tab_spanner(
    label = md("**Publication Year**"),
    columns = c(
      `2001-2004`,
      `2005-2009`,
      `2010-2014`,
      `2015-2019`,
      `2020-2022`
    )
  ) %>%
  cols_label(
    Variable = md("**Variable**"),
    `2001-2004` = md("**2001-2004**"),
    `2005-2009` = md("**2005-2009**"),
    `2010-2014` = md("**2010-2014**"),
    `2015-2019` = md("**2015-2019**"),
    `2020-2022` = md("**2020-2022**")
  ) %>%
  tab_footnote(
    footnote = "Mean (SD); n (%)",
    locations = cells_column_labels(
      columns = c(
        `2001-2004`,
        `2005-2009`,
        `2010-2014`,
        `2015-2019`,
        `2020-2022`
      )
    )
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    data_row.padding = px(2)
  )

tbl_gt_Table_2

gtsave(
  tbl_gt_Table_2,
  "Table 2.html"
)

dbDisconnect(con, shutdown = TRUE)
