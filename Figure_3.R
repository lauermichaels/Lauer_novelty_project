# Figure 3 -- bar plots of novelty-conventionality profiles by funding and type of science
# DuckDB-efficient version

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

# Compute all Figure 3 proportions inside DuckDB

df_summary_fig3 <- dbGetQuery(con, sprintf("
    WITH analytic_sample AS (
        SELECT
            Science_Type,
            CASE
                WHEN NIH_funding = 1 THEN 'NIH-funded'
                ELSE 'Not NIH-funded'
            END AS NIH_funding_label,
            Novelty_Type
        FROM read_parquet('%s')
        WHERE Atyp_10pct_Z IS NOT NULL
          AND Science_Type IS NOT NULL
          AND NIH_funding IS NOT NULL
          AND Novelty_Type IN (
              'Platypus',
              'Avant-garde',
              'Accepted Wisdom',
              'Darwin''s Tower'
          )
    )

    SELECT
        Science_Type,
        NIH_funding_label,
        AVG(CASE WHEN Novelty_Type = 'Platypus' THEN 1.0 ELSE 0.0 END) AS prop_platypus,
        AVG(CASE WHEN Novelty_Type = 'Avant-garde' THEN 1.0 ELSE 0.0 END) AS prop_avant,
        AVG(CASE WHEN Novelty_Type = 'Accepted Wisdom' THEN 1.0 ELSE 0.0 END) AS prop_aw,
        AVG(CASE WHEN Novelty_Type = 'Darwin''s Tower' THEN 1.0 ELSE 0.0 END) AS prop_darwin,
        COUNT(*) AS n_articles
    FROM analytic_sample
    GROUP BY
        Science_Type,
        NIH_funding_label
    ORDER BY
        CASE Science_Type
            WHEN 'Fundamental' THEN 1
            WHEN 'Mixed' THEN 2
            WHEN 'Human-focused' THEN 3
            ELSE 4
        END,
        CASE NIH_funding_label
            WHEN 'NIH-funded' THEN 1
            WHEN 'Not NIH-funded' THEN 2
            ELSE 3
        END
", input_file))

df_summary_fig3 <- df_summary_fig3 %>%
  mutate(
    Science_Type = factor(
      Science_Type,
      levels = c("Fundamental", "Mixed", "Human-focused")
    ),
    NIH_funding_label = factor(
      NIH_funding_label,
      levels = c("NIH-funded", "Not NIH-funded")
    )
  )

df_summary_fig3

# Quick check

df_summary_fig3 %>%
  select(Science_Type, NIH_funding_label, n_articles)

# Plot helper

make_bar_plot_fig3 <- function(summary_data, y_var, plot_title, show_legend = FALSE) {
  
  ggplot(
    summary_data,
    aes(
      x = Science_Type,
      y = .data[[y_var]],
      fill = NIH_funding_label
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.7
    ) +
    scale_y_continuous(
      name = "",
      limits = c(0, 0.55),
      breaks = seq(0, 0.55, by = 0.1),
      labels = function(x) paste0(x * 100, "%")
    ) +
    scale_fill_discrete(name = "") +
    xlab("") +
    ggtitle(plot_title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = ifelse(show_legend, "top", "none"),
      legend.margin = margin(0, 0, 0, 0, "pt")
    )
}

# Create the four panels

plot_platypus <- make_bar_plot_fig3(
  summary_data = df_summary_fig3,
  y_var = "prop_platypus",
  plot_title = "Platypus",
  show_legend = TRUE
)

plot_avant <- make_bar_plot_fig3(
  summary_data = df_summary_fig3,
  y_var = "prop_avant",
  plot_title = "Avant-garde",
  show_legend = FALSE
)

plot_aw <- make_bar_plot_fig3(
  summary_data = df_summary_fig3,
  y_var = "prop_aw",
  plot_title = "Accepted Wisdom",
  show_legend = FALSE
)

plot_dt <- make_bar_plot_fig3(
  summary_data = df_summary_fig3,
  y_var = "prop_darwin",
  plot_title = "Darwin's Tower",
  show_legend = FALSE
)

# Combine and save Figure 3

figure_3 <- plot_platypus +
  plot_avant +
  plot_aw +
  plot_dt +
  plot_layout(ncol = 2)

figure_3

ggsave(
  "Figure 3.jpg",
  plot = figure_3,
  width = 6.665,
  height = 7.5,
  units = "in",
  dpi = 600
)

# Save compact summary table

dbExecute(con, sprintf("
COPY (
    WITH analytic_sample AS (
        SELECT
            Science_Type,
            CASE
                WHEN NIH_funding = 1 THEN 'NIH-funded'
                ELSE 'Not NIH-funded'
            END AS NIH_funding_label,
            Novelty_Type
        FROM read_parquet('%s')
        WHERE Atyp_10pct_Z IS NOT NULL
          AND Science_Type IS NOT NULL
          AND NIH_funding IS NOT NULL
          AND Novelty_Type IN (
              'Platypus',
              'Avant-garde',
              'Accepted Wisdom',
              'Darwin''s Tower'
          )
    )

    SELECT
        Science_Type,
        NIH_funding_label,
        AVG(CASE WHEN Novelty_Type = 'Platypus' THEN 1.0 ELSE 0.0 END) AS prop_platypus,
        AVG(CASE WHEN Novelty_Type = 'Avant-garde' THEN 1.0 ELSE 0.0 END) AS prop_avant,
        AVG(CASE WHEN Novelty_Type = 'Accepted Wisdom' THEN 1.0 ELSE 0.0 END) AS prop_aw,
        AVG(CASE WHEN Novelty_Type = 'Darwin''s Tower' THEN 1.0 ELSE 0.0 END) AS prop_darwin,
        COUNT(*) AS n_articles
    FROM analytic_sample
    GROUP BY
        Science_Type,
        NIH_funding_label
)
TO 'figure_3_summary.parquet'
(FORMAT PARQUET)
", input_file))

dbDisconnect(con, shutdown = TRUE)
