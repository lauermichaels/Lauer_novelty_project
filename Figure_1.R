# Figure 1 -- ECDF curves for Science Type and Funding using DuckDB

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

# Build compact table with ECDF data

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE ecdf_plot_data AS

WITH long_metrics AS (

    SELECT
        'Science Type' AS comparison,
        'Novelty' AS metric,
        Science_Type AS group_name,
        SIGN(Atyp_10pct_Z) *
            LOG(2, GREATEST(ABS(Atyp_10pct_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_10pct_Z IS NOT NULL
      AND Science_Type IS NOT NULL

    UNION ALL

    SELECT
        'Science Type' AS comparison,
        'Conventionality' AS metric,
        Science_Type AS group_name,
        SIGN(Atyp_Median_Z) *
            LOG(2, GREATEST(ABS(Atyp_Median_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_Median_Z IS NOT NULL
      AND Science_Type IS NOT NULL

    UNION ALL

    SELECT
        'Funding' AS comparison,
        'Novelty' AS metric,
        Funding AS group_name,
        SIGN(Atyp_10pct_Z) *
            LOG(2, GREATEST(ABS(Atyp_10pct_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_10pct_Z IS NOT NULL
      AND Funding IS NOT NULL

    UNION ALL

    SELECT
        'Funding' AS comparison,
        'Conventionality' AS metric,
        Funding AS group_name,
        SIGN(Atyp_Median_Z) *
            LOG(2, GREATEST(ABS(Atyp_Median_Z), %f)) AS signed_log2
    FROM read_parquet('%s')
    WHERE Atyp_Median_Z IS NOT NULL
      AND Funding IS NOT NULL
),

binned AS (
    SELECT
        comparison,
        metric,
        group_name,
        ROUND(signed_log2 / %f) * %f AS signed_log2,
        COUNT(*) AS n_bin
    FROM long_metrics
    WHERE signed_log2 BETWEEN -8 AND 12
    GROUP BY
        comparison,
        metric,
        group_name,
        ROUND(signed_log2 / %f) * %f
),

with_ecdf AS (
    SELECT
        comparison,
        metric,
        group_name,
        signed_log2,
        SUM(n_bin) OVER (
            PARTITION BY comparison, metric, group_name
            ORDER BY signed_log2
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        )::DOUBLE
        /
        SUM(n_bin) OVER (
            PARTITION BY comparison, metric, group_name
        )::DOUBLE AS ecdf_y
    FROM binned
)

SELECT
    comparison,
    metric,
    group_name,
    signed_log2,
    ecdf_y
FROM with_ecdf
ORDER BY
    comparison,
    metric,
    group_name,
    signed_log2
",
          epsilon, input_file,
          epsilon, input_file,
          epsilon, input_file,
          epsilon, input_file,
          bin_width, bin_width,
          bin_width, bin_width
))

# Pull compact plotting data into R

ecdf_all <- dbGetQuery(con, "
    SELECT
        *
    FROM ecdf_plot_data
")

ecdf_all <- ecdf_all %>%
  mutate(
    group_name = case_when(
      comparison == "Science Type" ~ factor(
        group_name,
        levels = c("Fundamental", "Mixed", "Human-focused")
      ),
      comparison == "Funding" ~ factor(
        group_name,
        levels = c("NIH", "Not NIH")
      ),
      TRUE ~ factor(group_name)
    )
  )

head(ecdf_all)

# Define shared axis ticks

neg_pows <- 8:1
pos_pows <- 1:12

xticks_vals <- c(-neg_pows, 0, pos_pows)

xticks_labels <- c(
  parse(text = paste0("-2^", neg_pows)),
  "0",
  parse(text = paste0("2^", pos_pows))
)

# Plot helper function

make_ecdf_plot <- function(plot_data, plot_title, x_label, legend_position) {
  
  ggplot(
    plot_data,
    aes(x = signed_log2, y = ecdf_y, color = group_name)
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
      x = 7.5,
      y = 0.15,
      label = "Conventional\nCombinations",
      size = 3
    )
}

# Create the four panels

plot_ecdf_novelty_science_type <- ecdf_all %>%
  filter(comparison == "Science Type", metric == "Novelty") %>%
  make_ecdf_plot(
    plot_title = "Novelty by Science Type",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.80, 0.55)
  )

plot_ecdf_conventionality_science_type <- ecdf_all %>%
  filter(comparison == "Science Type", metric == "Conventionality") %>%
  make_ecdf_plot(
    plot_title = "Conventionality by Science Type",
    x_label = "Median Z-score",
    legend_position = c(0.20, 0.55)
  )

plot_ecdf_novelty_funding <- ecdf_all %>%
  filter(comparison == "Funding", metric == "Novelty") %>%
  make_ecdf_plot(
    plot_title = "Novelty by Funding",
    x_label = expression(10^th ~ "Percentile Z-score"),
    legend_position = c(0.75, 0.55)
  )

plot_ecdf_conventionality_funding <- ecdf_all %>%
  filter(comparison == "Funding", metric == "Conventionality") %>%
  make_ecdf_plot(
    plot_title = "Conventionality by Funding",
    x_label = "Median Z-score",
    legend_position = c(0.15, 0.55)
  )

# Combine and save the figure

figure_1 <- plot_ecdf_novelty_science_type +
  plot_ecdf_conventionality_science_type +
  plot_ecdf_novelty_funding +
  plot_ecdf_conventionality_funding +
  plot_layout(ncol = 2)

figure_1

ggsave(
  "Figure 1.jpg",
  plot = figure_1,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 600
)

# Save ECDF data

dbExecute(con, "
COPY (
    SELECT
        *
    FROM ecdf_plot_data
)
TO 'figure_1_ecdf_plot_data.parquet'
(FORMAT PARQUET)
")

dbDisconnect(con, shutdown = TRUE)
