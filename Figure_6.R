# Figure 6 -- Proportions of novel, conventional, or both according to citation quantile
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)

# Reuse existing DuckDB connection if one is already open
if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")
  
  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"

# Compute RCR ventiles and proportions inside DuckDB

df_long_fig6 <- dbGetQuery(con, sprintf("
    WITH analytic_sample AS (
        SELECT
            pmid,
            Science_Type,
            relative_citation_ratio,
            Novelty_Type,
            NTILE(20) OVER (
                ORDER BY relative_citation_ratio, pmid
            ) AS rcr_ventile
        FROM read_parquet('%s')
        WHERE relative_citation_ratio IS NOT NULL
          AND Atyp_10pct_Z IS NOT NULL
    ),

    all_summary AS (
        SELECT
            'All' AS Science_Type,
            rcr_ventile,
            AVG(
                CASE
                    WHEN Novelty_Type IN ('Darwin''s Tower', 'Avant-garde') THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_novel,
            AVG(
                CASE
                    WHEN Novelty_Type IN ('Accepted Wisdom', 'Darwin''s Tower') THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_conventional,
            AVG(
                CASE
                    WHEN Novelty_Type = 'Darwin''s Tower' THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_darwin,
            COUNT(*) AS n_articles
        FROM analytic_sample
        GROUP BY rcr_ventile
    ),

    science_summary AS (
        SELECT
            Science_Type,
            rcr_ventile,
            AVG(
                CASE
                    WHEN Novelty_Type IN ('Darwin''s Tower', 'Avant-garde') THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_novel,
            AVG(
                CASE
                    WHEN Novelty_Type IN ('Accepted Wisdom', 'Darwin''s Tower') THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_conventional,
            AVG(
                CASE
                    WHEN Novelty_Type = 'Darwin''s Tower' THEN 1.0
                    ELSE 0.0
                END
            ) AS prop_darwin,
            COUNT(*) AS n_articles
        FROM analytic_sample
        WHERE Science_Type IN ('Fundamental', 'Mixed', 'Human-focused')
        GROUP BY
            Science_Type,
            rcr_ventile
    ),

    combined AS (
        SELECT * FROM all_summary
        UNION ALL
        SELECT * FROM science_summary
    ),

    long_form AS (
        SELECT
            Science_Type,
            rcr_ventile,
            'Novel' AS Measure,
            prop_novel AS Proportion,
            prop_novel * 100.0 AS Percent,
            n_articles
        FROM combined

        UNION ALL

        SELECT
            Science_Type,
            rcr_ventile,
            'Conventional' AS Measure,
            prop_conventional AS Proportion,
            prop_conventional * 100.0 AS Percent,
            n_articles
        FROM combined

        UNION ALL

        SELECT
            Science_Type,
            rcr_ventile,
            'Darwin''s Tower' AS Measure,
            prop_darwin AS Proportion,
            prop_darwin * 100.0 AS Percent,
            n_articles
        FROM combined
    )

    SELECT
        Science_Type,
        rcr_ventile,
        Measure,
        Proportion,
        Percent,
        n_articles
    FROM long_form
    ORDER BY
        CASE Science_Type
            WHEN 'All' THEN 1
            WHEN 'Fundamental' THEN 2
            WHEN 'Mixed' THEN 3
            WHEN 'Human-focused' THEN 4
            ELSE 5
        END,
        rcr_ventile,
        CASE Measure
            WHEN 'Novel' THEN 1
            WHEN 'Conventional' THEN 2
            WHEN 'Darwin''s Tower' THEN 3
            ELSE 4
        END
", input_file))

# Restore plotting order in R

df_long_fig6 <- df_long_fig6 %>%
  mutate(
    Science_Type = factor(
      Science_Type,
      levels = c(
        "All",
        "Fundamental",
        "Mixed",
        "Human-focused"
      )
    ),
    Measure = factor(
      Measure,
      levels = c(
        "Novel",
        "Conventional",
        "Darwin's Tower"
      )
    )
  )

head(df_long_fig6)

# Plot figure 6

figure_6 <- ggplot(
  df_long_fig6,
  aes(
    x = rcr_ventile,
    y = Percent,
    color = Measure
  )
) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:20,
    minor_breaks = NULL
  ) +
  labs(
    x = "Relative Citation Ratio quantile (1 = bottom 5%, 20 = top 5%)",
    y = "Percent of papers",
    color = "",
    title = ""
  ) +
  facet_wrap(~ Science_Type, nrow = 2, ncol = 2) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

figure_6

ggsave(
  "Figure 6.jpg",
  plot = figure_6,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 600
)

# Quick check

df_long_fig6 %>%
  distinct(Science_Type, rcr_ventile, n_articles) %>%
  group_by(Science_Type) %>%
  summarise(
    min_n = min(n_articles),
    max_n = max(n_articles),
    total_n = sum(n_articles),
    .groups = "drop"
  )

# Save plotting data

arrow::write_parquet(
  df_long_fig6,
  "figure_6_summary.parquet"
)

dbDisconnect(con, shutdown = TRUE)



























































































# Figure 6 -- Proportions of novel, conventional, or both according to citation quantile

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_6_18_26.parquet")

df <- df_opa_sciscinet_2001_2022 

df_plot <- df %>%
  filter(!is.na(relative_citation_ratio)) %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    Novel = Novelty_Type %in% c("Darwin's Tower", "Avant-garde"),
    Conventional = Novelty_Type %in% c("Accepted Wisdom", "Darwin's Tower"),
    Darwin = Novelty_Type == "Darwin's Tower",
  ) %>%
  # Create 20 RCR quantiles: 1 = bottom 5%, 20 = top 5%
  mutate(
    rcr_ventile = ntile(relative_citation_ratio, 20)
  )

# Compute mean (proportion) Novel and Conventional by ventile and Science_Type
df_summary <- df_plot %>%
  group_by(Science_Type, rcr_ventile) %>%
  summarise(
    prop_novel = mean(Novel, na.rm = TRUE),
    prop_conventional = mean(Conventional, na.rm = TRUE),
    prop_darwin = mean(Darwin, na.rm = TRUE),
    .groups = "drop"
  )

# Overall (all Science_Types combined)
df_all <- df_plot %>%
  group_by(rcr_ventile) %>%
  summarise(
    prop_novel = mean(Novel, na.rm = TRUE),
    prop_conventional = mean(Conventional, na.rm = TRUE),
    prop_darwin = mean(Darwin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Science_Type = "All")

# Combine All + Science-Type-specific
df_combined <- bind_rows(
  df_all,
  df_summary
)

# Reshape to long format: one row per (Science_Type, rcr_ventile, measure)
df_long <- df_combined %>%
  pivot_longer(
    cols = c(prop_novel, prop_conventional, prop_darwin),
    names_to = "Measure",
    values_to = "Proportion"
  ) %>%
  mutate(
    Measure = recode(
      Measure,
      prop_novel = "Novel",
      prop_conventional = "Conventional",
      prop_darwin = "Darwin's Tower",
    ),
    Percent = Proportion * 100
  ) %>%
  mutate(Measure = factor(Measure, levels = c("Novel","Conventional","Darwin's Tower")))

# Ensure ordering of panels
df_long$Science_Type <- factor(
  df_long$Science_Type,
  levels = c(
    "All",
    "Fundamental",
    "Mixed",
    "Human-focused"
  )
)

# 2x2 facet line plot
ggplot(df_long, aes(x = rcr_ventile, y = Percent, color = Measure)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:20,
    minor_breaks = NULL
  ) +
  labs(
    x = "Relative Citation Ratio quantile (1 = bottom 5%, 20 = top 5%)",
    y = "Percent of papers",
    color = "",
    title = ""
  ) +
  facet_wrap(~ Science_Type, nrow = 2, ncol = 2) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

ggsave("Figure 7.jpg", width = 10, height = 7.5, units = c("in"), dpi=600)


