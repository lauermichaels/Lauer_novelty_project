# Figure 2 -- ECDF curves for NIH vs non-NIH broken down by Science Type using DuckDB

library(DBI)
library(duckdb)
library(tidyverse)
library(scales)
library(patchwork)

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"
epsilon <- 1e-6
bin_width <- 0.01

# Build the compact ECDF data in DuckDB

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE ecdf_plot_data_fig2 AS

WITH long_metrics AS (

    SELECT
        Science_Type,
        Funding,
        'Novelty' AS metric,
        SIGN(Atyp_10pct_Z) *
            LOG2(GREATEST(ABS(Atyp_10pct_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_10pct_Z IS NOT NULL
      AND Science_Type IN ('Fundamental', 'Mixed', 'Human-focused')
      AND Funding IN ('NIH', 'Not NIH')

    UNION ALL

    SELECT
        Science_Type,
        Funding,
        'Conventionality' AS metric,
        SIGN(Atyp_Median_Z) *
            LOG2(GREATEST(ABS(Atyp_Median_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_Median_Z IS NOT NULL
      AND Science_Type IN ('Fundamental', 'Mixed', 'Human-focused')
      AND Funding IN ('NIH', 'Not NIH')
),

binned AS (
    SELECT
        Science_Type,
        Funding,
        metric,
        ROUND(signed_log2 / %f) * %f AS signed_log2,
        COUNT(*) AS n_bin
    FROM long_metrics
    WHERE signed_log2 IS NOT NULL
    GROUP BY
        Science_Type,
        Funding,
        metric,
        ROUND(signed_log2 / %f) * %f
),

with_ecdf AS (
    SELECT
        Science_Type,
        Funding,
        metric,
        signed_log2,
        SUM(n_bin) OVER (
            PARTITION BY Science_Type, Funding, metric
            ORDER BY signed_log2
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        )::DOUBLE
        /
        SUM(n_bin) OVER (
            PARTITION BY Science_Type, Funding, metric
        )::DOUBLE AS ecdf_y
    FROM binned
)

SELECT
    Science_Type,
    Funding,
    metric,
    signed_log2,
    ecdf_y
FROM with_ecdf
WHERE signed_log2 BETWEEN -8 AND 12
ORDER BY
    Science_Type,
    metric,
    Funding,
    signed_log2
",
          epsilon, input_file,
          epsilon, input_file,
          bin_width, bin_width,
          bin_width, bin_width
))

# Pull only the compact plotting table into R

ecdf_fig2 <- dbGetQuery(con, "
    SELECT
        *
    FROM ecdf_plot_data_fig2
")

ecdf_fig2 <- ecdf_fig2 %>%
  mutate(
    Science_Type = factor(
      Science_Type,
      levels = c("Fundamental", "Mixed", "Human-focused")
    ),
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH")
    ),
    metric = factor(
      metric,
      levels = c("Novelty", "Conventionality")
    )
  )

head(ecdf_fig2)

# Shared axis setup

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

make_ecdf_plot_fig2 <- function(plot_data, plot_title, x_label, legend_position, right_annotation_x) {
  
  ggplot(
    plot_data,
    aes(x = signed_log2, y = ecdf_y, color = Funding)
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
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
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

# Create the six panels

plot_ecdf_novelty_funding_fundamental <- ecdf_fig2 %>%
  filter(
    Science_Type == "Fundamental",
    metric == "Novelty"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Novelty by Funding (Fundamental)",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.75, 0.55),
    right_annotation_x = 7
  )

plot_ecdf_conventionality_funding_fundamental <- ecdf_fig2 %>%
  filter(
    Science_Type == "Fundamental",
    metric == "Conventionality"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Conventionality by Funding (Fundamental)",
    x_label = "Median Z-score",
    legend_position = c(0.15, 0.55),
    right_annotation_x = 8
  )

plot_ecdf_novelty_funding_mixed <- ecdf_fig2 %>%
  filter(
    Science_Type == "Mixed",
    metric == "Novelty"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Novelty by Funding (Mixed)",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.75, 0.55),
    right_annotation_x = 7
  )

plot_ecdf_conventionality_funding_mixed <- ecdf_fig2 %>%
  filter(
    Science_Type == "Mixed",
    metric == "Conventionality"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Conventionality by Funding (Mixed)",
    x_label = "Median Z-score",
    legend_position = c(0.15, 0.55),
    right_annotation_x = 8
  )

plot_ecdf_novelty_funding_human <- ecdf_fig2 %>%
  filter(
    Science_Type == "Human-focused",
    metric == "Novelty"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Novelty by Funding (Human-focused)",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.75, 0.55),
    right_annotation_x = 7
  )

plot_ecdf_conventionality_funding_human <- ecdf_fig2 %>%
  filter(
    Science_Type == "Human-focused",
    metric == "Conventionality"
  ) %>%
  make_ecdf_plot_fig2(
    plot_title = "Conventionality by Funding (Human-focused)",
    x_label = "Median Z-score",
    legend_position = c(0.15, 0.55),
    right_annotation_x = 8
  )

# Combine and save figure 2

figure_2 <- plot_ecdf_novelty_funding_fundamental +
  plot_ecdf_conventionality_funding_fundamental +
  plot_ecdf_novelty_funding_mixed +
  plot_ecdf_conventionality_funding_mixed +
  plot_ecdf_novelty_funding_human +
  plot_ecdf_conventionality_funding_human +
  plot_layout(ncol = 2)

figure_2

ggsave(
  "Figure 2.jpg",
  plot = figure_2,
  width = 10,
  height = 11.25,
  units = "in",
  dpi = 600
)

# Validation

ecdf_fig2_check <- dbGetQuery(con, "
    SELECT
        Science_Type,
        Funding,
        metric,
        COUNT(*) AS n_plot_points,
        MIN(signed_log2) AS min_signed_log2,
        MAX(signed_log2) AS max_signed_log2,
        MIN(ecdf_y) AS min_ecdf_y,
        MAX(ecdf_y) AS max_ecdf_y
    FROM ecdf_plot_data_fig2
    GROUP BY
        Science_Type,
        Funding,
        metric
    ORDER BY
        Science_Type,
        metric,
        Funding
")

ecdf_fig2_check

# Save ECDF data

dbExecute(con, "
COPY (
    SELECT
        *
    FROM ecdf_plot_data_fig2
)
TO 'figure_2_ecdf_plot_data.parquet'
(FORMAT PARQUET)
")

dbDisconnect(con, shutdown = TRUE)