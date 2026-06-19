# Table 1 -- characteristics by NIH funding
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

# Compute continuous-variable summaries in DuckDB

continuous_summary <- dbGetQuery(con, sprintf("
    WITH long_continuous AS (
        SELECT
            Funding,
            'author_count_opa' AS variable,
            'Number of Authors' AS label,
            CAST(author_count_opa AS DOUBLE) AS value,
            1 AS var_order
        FROM read_parquet('%s')
        WHERE author_count_opa IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'institution_count' AS variable,
            'Number of Institutions' AS label,
            CAST(institution_count AS DOUBLE) AS value,
            2 AS var_order
        FROM read_parquet('%s')
        WHERE institution_count IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'reference_count' AS variable,
            'Number of References' AS label,
            CAST(reference_count AS DOUBLE) AS value,
            3 AS var_order
        FROM read_parquet('%s')
        WHERE reference_count IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'Atyp_10pct_Z' AS variable,
            '10th Percentile Z-score' AS label,
            CAST(Atyp_10pct_Z AS DOUBLE) AS value,
            4 AS var_order
        FROM read_parquet('%s')
        WHERE Atyp_10pct_Z IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'Atyp_Median_Z' AS variable,
            'Median Z-score' AS label,
            CAST(Atyp_Median_Z AS DOUBLE) AS value,
            5 AS var_order
        FROM read_parquet('%s')
        WHERE Atyp_Median_Z IS NOT NULL
          AND Funding IS NOT NULL
    ),

    ranked AS (
        SELECT
            *,
            ROW_NUMBER() OVER (
                PARTITION BY Funding, variable
                ORDER BY value
            ) AS rn,
            COUNT(*) OVER (
                PARTITION BY Funding, variable
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
        Funding,
        variable,
        label,
        var_order,
        AVG(value) AS mean_trimmed,
        STDDEV_SAMP(value) AS sd_trimmed,
        COUNT(*) AS n_trimmed
    FROM trimmed
    GROUP BY
        Funding,
        variable,
        label,
        var_order
    ORDER BY
        var_order,
        Funding
",
                                              input_file,
                                              input_file,
                                              input_file,
                                              input_file,
                                              input_file
))

# Compute categorical summaries in DuckDB

categorical_summary <- dbGetQuery(con, sprintf("
    WITH categorical_long AS (
        SELECT
            Funding,
            'Novelty_Type' AS variable,
            'Article Type' AS label,
            Novelty_Type AS level,
            6 AS var_order,
            CASE Novelty_Type
                WHEN 'Platypus' THEN 1
                WHEN 'Avant-garde' THEN 2
                WHEN 'Accepted Wisdom' THEN 3
                WHEN 'Darwin''s Tower' THEN 4
                WHEN 'Missing' THEN 5
                ELSE 6
            END AS level_order
        FROM read_parquet('%s')
        WHERE Novelty_Type IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'Science_Type' AS variable,
            'Science Type' AS label,
            Science_Type AS level,
            7 AS var_order,
            CASE Science_Type
                WHEN 'Fundamental' THEN 1
                WHEN 'Mixed' THEN 2
                WHEN 'Human-focused' THEN 3
                ELSE 4
            END AS level_order
        FROM read_parquet('%s')
        WHERE Science_Type IS NOT NULL
          AND Funding IS NOT NULL
    ),

    categorical_counts AS (
        SELECT
            Funding,
            variable,
            label,
            level,
            var_order,
            level_order,
            COUNT(*) AS n,
            SUM(COUNT(*)) OVER (
                PARTITION BY Funding, variable
            ) AS denom
        FROM categorical_long
        GROUP BY
            Funding,
            variable,
            label,
            level,
            var_order,
            level_order
    ),

    clinical_counts AS (
        SELECT
            Funding,
            'is_clinical' AS variable,
            'Clinical Trial' AS label,
            'TRUE' AS level,
            8 AS var_order,
            1 AS level_order,
            SUM(
                CASE
                    WHEN is_clinical = TRUE THEN 1
                    ELSE 0
                END
            ) AS n,
            COUNT(*) AS denom
        FROM read_parquet('%s')
        WHERE is_clinical IS NOT NULL
          AND Funding IS NOT NULL
        GROUP BY
            Funding
    )

    SELECT
        Funding,
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
        Funding,
        variable,
        label,
        level,
        var_order,
        level_order,
        n,
        denom,
        n::DOUBLE / denom::DOUBLE AS prop
    FROM clinical_counts

    ORDER BY
        var_order,
        level_order,
        Funding
",
                                               input_file,
                                               input_file,
                                               input_file
))

# Format the DuckDB summaries into a table body

continuous_table <- continuous_summary %>%
  mutate(
    stat = paste0(
      sprintf("%.1f", mean_trimmed),
      " (",
      sprintf("%.1f", sd_trimmed),
      ")"
    ),
    row_label = label,
    row_type = "continuous"
  ) %>%
  select(
    variable,
    label,
    row_label,
    row_type,
    var_order,
    Funding,
    stat
  )

categorical_table <- categorical_summary %>%
  mutate(
    stat = paste0(
      scales::comma(n, accuracy = 1),
      " (",
      sprintf("%.0f", prop * 100),
      "%)"
    ),
    row_label = ifelse(
      variable == "is_clinical",
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
    Funding,
    stat
  )

table_body_long <- bind_rows(
  continuous_table %>%
    mutate(level_order = 0),
  categorical_table
)

# Pivot to NIH vs Not NIH columns

table_body_wide <- table_body_long %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("Not NIH", "NIH")
    )
  ) %>%
  arrange(var_order, level_order, row_type) %>%
  select(row_label, Funding, stat, var_order, level_order) %>%
  pivot_wider(
    names_from = Funding,
    values_from = stat
  ) %>%
  arrange(var_order, level_order) %>%
  select(
    Variable = row_label,
    `Not NIH`,
    NIH
  )

table_body_wide

# Compute total N by funding group for column headers

funding_n <- dbGetQuery(con, sprintf("
    SELECT
        Funding,
        COUNT(*) AS n_total
    FROM read_parquet('%s')
    WHERE Funding IS NOT NULL
    GROUP BY Funding
", input_file))

not_nih_n <- funding_n %>%
  filter(Funding == "Not NIH") %>%
  pull(n_total) %>%
  scales::comma(accuracy = 1)

nih_n <- funding_n %>%
  filter(Funding == "NIH") %>%
  pull(n_total) %>%
  scales::comma(accuracy = 1)

# Create and save table

tbl_gt_Table_1 <- table_body_wide %>%
  gt() %>%
  tab_header(
    title = md("")
  ) %>%
  tab_spanner(
    label = md("**Funding**"),
    columns = c(`Not NIH`, NIH)
  ) %>%
  cols_label(
    Variable = md("**Variable**"),
    `Not NIH` = md(paste0("**Not NIH**<br>N = ", not_nih_n)),
    NIH = md(paste0("**NIH**<br>N = ", nih_n))
  ) %>%
  tab_footnote(
    footnote = "Mean (SD); n (%)",
    locations = cells_column_labels(columns = c(`Not NIH`, NIH))
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Not NIH`, NIH)
  ) %>%
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  tab_options(
    table.font.names = "Times New Roman"
  )

tbl_gt_Table_1

gtsave(
  tbl_gt_Table_1,
  "Table 1.html"
)
dbDisconnect(con, shutdown = TRUE)