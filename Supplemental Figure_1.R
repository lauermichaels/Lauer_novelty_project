# Supplemental Figure 1
# ECDF curves over time, separately for NIH and Non-NIH papers
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)
library(scales)
library(patchwork)

# Reuse existing DuckDB connection if one is already open

if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")
  
  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"

epsilon <- 1e-6
bin_width <- 0.01

# Build compact ECDF data in DuckDB
# This creates one compact table for all four panels:
  
# NIH Novelty
# Non-NIH Novelty
# NIH Conventionality
# Non-NIH Conventionality

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE supp_fig1_ecdf_data AS

WITH long_metrics AS (

    SELECT
        CASE
            WHEN NIH_funding = 1 THEN 'NIH'
            WHEN NIH_funding = 0 THEN 'Non-NIH'
            ELSE NULL
        END AS funding_group,

        CASE
            WHEN year >= 2001 AND year <= 2009 THEN '2001-2009'
            WHEN year >= 2010 AND year <= 2019 THEN '2010-2019'
            WHEN year >= 2020 AND year <= 2022 THEN '2020-2022'
            ELSE 'Other'
        END AS year_group,

        'Novelty' AS metric,

        SIGN(Atyp_10pct_Z) *
            LOG2(GREATEST(ABS(Atyp_10pct_Z), %f)) AS signed_log2

    FROM read_parquet('%s')
    WHERE NIH_funding IN (0, 1)
      AND Atyp_10pct_Z IS NOT NULL

    UNION ALL

    SELECT
        CASE
            WHEN NIH_funding = 1 THEN 'NIH'
            WHEN NIH_funding = 0 THEN 'Non-NIH'
            ELSE NULL
        END AS funding_group,

        CASE
            WHEN year >= 2001 AND year <= 2009 THEN '2001-2009'
            WHEN year >= 2010 AND year <= 2019 THEN '2010-2019'
            WHEN year >= 2020 AND year <= 2022 THEN '2020-2022'
            ELSE 'Other'
        END AS year_group,

        'Conventionality' AS metric,

        SIGN(Atyp_Median_Z) *
            LOG2(GREATEST(ABS(Atyp_Median_Z), %f)) AS signed_log2

    FROM read_parquet('%s')
    WHERE NIH_funding IN (0, 1)
      AND Atyp_Median_Z IS NOT NULL
),

binned AS (
    SELECT
        funding_group,
        year_group,
        metric,
        ROUND(signed_log2 / %f) * %f AS signed_log2,
        COUNT(*) AS n_bin
    FROM long_metrics
    WHERE funding_group IS NOT NULL
      AND year_group IN ('2001-2009', '2010-2019', '2020-2022')
      AND signed_log2 IS NOT NULL
    GROUP BY
        funding_group,
        year_group,
        metric,
        ROUND(signed_log2 / %f) * %f
),

with_ecdf AS (
    SELECT
        funding_group,
        year_group,
        metric,
        signed_log2,
        SUM(n_bin) OVER (
            PARTITION BY funding_group, year_group, metric
            ORDER BY signed_log2
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        )::DOUBLE
        /
        SUM(n_bin) OVER (
            PARTITION BY funding_group, year_group, metric
        )::DOUBLE AS ecdf_y
    FROM binned
)

SELECT
    funding_group,
    year_group,
    metric,
    signed_log2,
    ecdf_y
FROM with_ecdf
WHERE signed_log2 BETWEEN -8 AND 12
ORDER BY
    funding_group,
    metric,
    year_group,
    signed_log2
",
          epsilon, input_file,
          epsilon, input_file,
          bin_width, bin_width,
          bin_width, bin_width
))

# Pull only the compact plotting data into R


supp_fig1_ecdf <- dbGetQuery(con, "
    SELECT
        *
    FROM supp_fig1_ecdf_data
")

supp_fig1_ecdf <- supp_fig1_ecdf %>%
  mutate(
    funding_group = factor(
      funding_group,
      levels = c("NIH", "Non-NIH")
    ),
    year_group = factor(
      year_group,
      levels = c("2001-2009", "2010-2019", "2020-2022")
    ),
    metric = factor(
      metric,
      levels = c("Novelty", "Conventionality")
    )
  )

head(supp_fig1_ecdf)

# Shared axis settings


neg_pows <- 8:1
pos_pows <- 1:12

xticks_vals <- c(-neg_pows, 0, pos_pows)

xtick_lab_strings <- c(
  paste0("-2^", neg_pows),
  "0",
  paste0("2^", pos_pows)
)

xticks_labels <- parse(text = xtick_lab_strings)

# Plot helper

make_supp_fig1_ecdf_plot <- function(plot_data,
                                     plot_title,
                                     x_label,
                                     legend_position,
                                     right_annotation_x) {
  
  ggplot(
    plot_data,
    aes(
      x = signed_log2,
      y = ecdf_y,
      color = year_group
    )
  ) +
    geom_step(direction = "hv", linewidth = 0.7) +
    scale_x_continuous(
      breaks = xticks_vals,
      labels = xticks_labels,
      limits = c(-8, 12),
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(
      name = "Cumulative Distribution",
      limits = c(0, 1),
      expand = expansion(mult = 0),
      labels = label_percent(accuracy = 1)
    ) +
    labs(
      x = x_label,
      color = "",
      title = plot_title
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = legend_position
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = -5,
      y = 0.85,
      label = "Novel\nCombinations",
      size = 3
    ) +
    annotate(
      "text",
      x = right_annotation_x,
      y = 0.15,
      label = "Conventional\nCombinations",
      size = 3
    )
}

# Create the four panels

plot_ecdf_novelty_nih <- supp_fig1_ecdf %>%
  filter(
    funding_group == "NIH",
    metric == "Novelty"
  ) %>%
  make_supp_fig1_ecdf_plot(
    plot_title = "NIH Novelty",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.80, 0.55),
    right_annotation_x = 7.5
  )

plot_ecdf_novelty_non_nih <- supp_fig1_ecdf %>%
  filter(
    funding_group == "Non-NIH",
    metric == "Novelty"
  ) %>%
  make_supp_fig1_ecdf_plot(
    plot_title = "Non-NIH Novelty",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.80, 0.55),
    right_annotation_x = 7.5
  )

plot_ecdf_conventionality_nih <- supp_fig1_ecdf %>%
  filter(
    funding_group == "NIH",
    metric == "Conventionality"
  ) %>%
  make_supp_fig1_ecdf_plot(
    plot_title = "NIH Conventionality",
    x_label = "Median Z-score",
    legend_position = c(0.20, 0.55),
    right_annotation_x = 8.5
  )

plot_ecdf_conventionality_non_nih <- supp_fig1_ecdf %>%
  filter(
    funding_group == "Non-NIH",
    metric == "Conventionality"
  ) %>%
  make_supp_fig1_ecdf_plot(
    plot_title = "Non-NIH Conventionality",
    x_label = "Median Z-score",
    legend_position = c(0.20, 0.55),
    right_annotation_x = 8.5
  )

# Combine and save Supplementary Figure 1


supplementary_figure_1 <- plot_ecdf_novelty_nih +
  plot_ecdf_novelty_non_nih +
  plot_ecdf_conventionality_nih +
  plot_ecdf_conventionality_non_nih +
  plot_layout(ncol = 2)

supplementary_figure_1

ggsave(
  "Supplementary Figure 1.jpg",
  plot = supplementary_figure_1,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 600
)

#Optional validation
#This confirms each panel has data for each year group.


supp_fig1_check <- dbGetQuery(con, "
    SELECT
        funding_group,
        metric,
        year_group,
        COUNT(*) AS n_plot_points,
        MIN(signed_log2) AS min_signed_log2,
        MAX(signed_log2) AS max_signed_log2,
        MIN(ecdf_y) AS min_ecdf_y,
        MAX(ecdf_y) AS max_ecdf_y
    FROM supp_fig1_ecdf_data
    GROUP BY
        funding_group,
        metric,
        year_group
    ORDER BY
        funding_group,
        metric,
        year_group
")

supp_fig1_check

# Save of compact ECDF data


dbExecute(con, "
COPY (
    SELECT
        *
    FROM supp_fig1_ecdf_data
)
TO 'supplementary_figure_1_ecdf_plot_data.parquet'
(FORMAT PARQUET)
")

dbDisconnect(con, shutdown = TRUE)
