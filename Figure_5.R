# Figure 5 -- likelihood of being highly cited bar plot
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)

# Reuse the existing DuckDB connection if you already have one open
if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")
  
  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"

# Compute the plotting summary in DuckDB

df_summary_fig5 <- dbGetQuery(con, sprintf("
    WITH analytic_sample AS (
        SELECT
            CASE
                WHEN NIH_funding = 1 THEN 'NIH-funded'
                ELSE 'Not NIH-funded'
            END AS funding_group,
            Novelty_Type,
            Science_Type,
            CASE
                WHEN Highly_Cited THEN 1.0
                ELSE 0.0
            END AS highly_cited_num
        FROM read_parquet('%s')
        WHERE Atyp_10pct_Z IS NOT NULL
          AND Novelty_Type IS NOT NULL
          AND Science_Type IS NOT NULL
          AND Highly_Cited IS NOT NULL
          AND relative_citation_ratio IS NOT NULL
          AND NIH_funding IS NOT NULL
    )

    SELECT
        funding_group,
        Novelty_Type,
        Science_Type,
        AVG(highly_cited_num) AS prop_hc,
        COUNT(*) AS n_articles
    FROM analytic_sample
    GROUP BY
        funding_group,
        Novelty_Type,
        Science_Type
    ORDER BY
        CASE funding_group
            WHEN 'Not NIH-funded' THEN 1
            WHEN 'NIH-funded' THEN 2
            ELSE 3
        END,
        CASE Novelty_Type
            WHEN 'Platypus' THEN 1
            WHEN 'Avant-garde' THEN 2
            WHEN 'Accepted Wisdom' THEN 3
            WHEN 'Darwin''s Tower' THEN 4
            ELSE 5
        END,
        CASE Science_Type
            WHEN 'Fundamental' THEN 1
            WHEN 'Mixed' THEN 2
            WHEN 'Human-focused' THEN 3
            ELSE 4
        END
", input_file))

# Set factor ordering in R

df_summary_fig5 <- df_summary_fig5 %>%
  mutate(
    funding_group = factor(
      funding_group,
      levels = c("Not NIH-funded", "NIH-funded")
    ),
    Novelty_Type = factor(
      Novelty_Type,
      levels = c(
        "Platypus",
        "Avant-garde",
        "Accepted Wisdom",
        "Darwin's Tower"
      )
    ),
    Science_Type = factor(
      Science_Type,
      levels = c("Fundamental", "Mixed", "Human-focused")
    )
  )

df_summary_fig5

# Plot Figure 5

figure_5 <- ggplot(
  df_summary_fig5,
  aes(
    x = Novelty_Type,
    y = prop_hc,
    fill = Science_Type
  )
) +
  theme_bw() +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_wrap(~ funding_group, nrow = 1) +
  scale_y_continuous(
    name = "Percent Highly Cited",
    limits = c(0, 0.20),
    breaks = seq(0, 0.20, by = 0.05),
    labels = function(x) paste0(x * 100, "%")
  ) +
  xlab("") +
  scale_fill_discrete(name = "") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

figure_5

ggsave(
  "Figure 5.jpg",
  plot = figure_5,
  width = 6.665,
  height = 3.75,
  units = "in",
  dpi = 600
)

# Check cell sizes

df_summary_fig5 %>%
  arrange(funding_group, Novelty_Type, Science_Type) %>%
  select(funding_group, Novelty_Type, Science_Type, prop_hc, n_articles)

# Save compact summary table

arrow::write_parquet(
  df_summary_fig5,
  "figure_5_summary.parquet"
)

dbDisconnect(con, shutdown = TRUE)
