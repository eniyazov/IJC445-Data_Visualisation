# ==========================================================
# IJC445 Coursework Appendix R Code (Final)
# Dataset: billboard_24years_lyrics_spotify.csv
# Outputs: Fig1, Fig2, FigA, FigB, FigC, Fig4
# ==========================================================

library(tidyverse)
library(tidytext)
library(stringr)
library(ggridges)
library(hexbin)
library(patchwork)

# --------------------------
# 1) Load data
# --------------------------
df <- read_csv("data/billboard_24years_lyrics_spotify.csv", show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    ranking = as.integer(ranking),
    lyrics = replace_na(lyrics, "")
  )

# --------------------------
# 2) Tokenise lyrics (stopwords removed)
# --------------------------
tokens <- df %>%
  select(year, ranking, song, band_singer, lyrics) %>%
  mutate(lyrics = str_to_lower(lyrics)) %>%
  unnest_tokens(word, lyrics) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  anti_join(stop_words, by = "word")

# --------------------------
# 3) Song-level lyric metrics
# --------------------------
song_metrics <- tokens %>%
  group_by(year, song, band_singer, ranking) %>%
  summarise(
    total_words = n(),
    unique_words = n_distinct(word),
    lexical_div = unique_words / total_words,
    .groups = "drop"
  )

# Output folder
dir.create("figures", showWarnings = FALSE)

# ==========================================================
# FIG 1: Median lyric word count by year
# ==========================================================
fig1_data <- song_metrics %>%
  group_by(year) %>%
  summarise(median_words = median(total_words, na.rm = TRUE), .groups = "drop")

fig1 <- ggplot(fig1_data, aes(x = year, y = median_words)) +
  geom_line() +
  labs(
    title = "Fig 1. Median lyric word count by year (Billboard Hot-100)",
    x = "Year", y = "Median word count"
  ) +
  theme_minimal()

ggsave("figures/fig1_median_wordcount.png", fig1, width = 9, height = 5, dpi = 300)

# ==========================================================
# FIG 2: Median lexical diversity by year
# ==========================================================
fig2_data <- song_metrics %>%
  group_by(year) %>%
  summarise(median_lexdiv = median(lexical_div, na.rm = TRUE), .groups = "drop")

fig2 <- ggplot(fig2_data, aes(x = year, y = median_lexdiv)) +
  geom_line() +
  labs(
    title = "Fig 2. Median lexical diversity by year (unique/total words)",
    x = "Year", y = "Median lexical diversity"
  ) +
  theme_minimal()

ggsave("figures/fig2_median_lexdiv.png", fig2, width = 9, height = 5, dpi = 300)

# ==========================================================
# FIG A: Ridgeline distribution of lyric word count over time (corrected)
# ==========================================================
ridge_data <- song_metrics %>%
  filter(total_words < 800) %>%      # readability threshold
  mutate(year = factor(year))

figA <- ggplot(ridge_data, aes(x = total_words, y = year)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  labs(
    title = "Fig A. Distribution of lyric word counts over time",
    x = "Total words per song",
    y = "Year"
  ) +
  theme_minimal()

ggsave("figures/figA_ridgeline_wordcount.png", figA, width = 10, height = 8, dpi = 300)

# ==========================================================
# FIG B: Hexbin density (lexical diversity vs lyric length)
# ==========================================================
figB <- ggplot(song_metrics, aes(x = total_words, y = lexical_div)) +
  geom_hex(bins = 30) +
  labs(
    title = "Fig B. Density of songs by lyric length and lexical diversity",
    subtitle = "Most songs cluster at low word counts; very long lyrics are rare",
    x = "Total words per song",
    y = "Lexical diversity",
    fill = "Song count"
  ) +
  theme_minimal()

ggsave("figures/figB_hex_lexdiv.png", figB, width = 9, height = 6, dpi = 300)

# ==========================================================
# FIG C: Slopegraph of selected lyrical themes (Early vs Late)
# ==========================================================
key_words <- c("love", "baby", "girl", "money", "party", "night")

word_trends <- tokens %>%
  filter(word %in% key_words) %>%
  mutate(period = case_when(
    year <= 2005 ~ "Early (2000–05)",
    year >= 2018 ~ "Late (2018–23)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  count(period, word) %>%
  group_by(period) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

figC <- ggplot(word_trends, aes(x = period, y = prop, group = word)) +
  geom_line() +
  geom_point() +
  geom_text(
    data = subset(word_trends, period == "Late (2018–23)"),
    aes(label = word),
    hjust = -0.1
  ) +
  labs(
    title = "Fig C. Shift in key lyrical themes from early to late Billboard eras",
    x = "Period",
    y = "Proportion of word usage"
  ) +
  theme_minimal()

ggsave("figures/figC_slopegraph_words.png", figC, width = 9, height = 6, dpi = 300)

# ==========================================================
# FIG 4: Lyric length vs chart ranking
# ==========================================================
fig4 <- ggplot(song_metrics, aes(x = total_words, y = ranking)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_reverse() +
  labs(
    title = "Fig 4. Relationship between lyric length and chart ranking",
    x = "Total words per song",
    y = "Ranking (1 is best)"
  ) +
  theme_minimal()

ggsave("figures/fig4_rank_vs_wordcount.png", fig4, width = 9, height = 5, dpi = 300)
