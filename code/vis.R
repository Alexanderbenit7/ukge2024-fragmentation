library(rio)
library(sf)
library(ggplot2)
library(patchwork)
library(viridis)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)

# Data
data <- import('/Users/alexander/Documents/benites-unsworth/ukResultsData.csv')
shp2024 <- st_read('/Users/alexander/Documents/MSc Data Science/erp-uom/shapefiles/2024/PCON_JULY_2024_UK_BFC.shp')

# Merge
shp2024 <- merge(shp2024, data, by.x = "PCON24CD", by.y = "constituency_geographic_code")

# Conservaties
dif_cons_map <- ggplot(shp2024) +
  geom_sf(aes(fill = DIF_Cons), colour = "NA", size = 0.05) +
  scale_fill_viridis_c(
    option = "A"
  ) +
  labs(
    title = "Change in Conservative Vote Share, 2019–2024",
    subtitle = "Negative values = loss of support"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  filename = "/Users/alexander/Documents/benites-unsworth/figs/DIF_Cons_map.jpeg", 
  plot = dif_cons_map,
  width = 8,
  height = 11,
  dpi = 300
)


# 1) Flag seats gained from Conservatives & extract the winner party
shp_map <- shp2024 %>%
  mutate(
    from_con  = grepl("gain from Con$", result_summary_2024),
    winner_from_con = ifelse(from_con,
                             sub("^(.*) gain from Con$", "\\1", result_summary_2024),
                             NA_character_),
    # Make a nice factor order for the legend
    winner_from_con = fct_relevel(winner_from_con,
                                  "Lab", "LD", "SNP", "PC", "Green", "RUK", "Ind")
  )


# 2) Party colours (adjust to your house style if you like)
party_cols <- c(
  "Lab"   = "#E4003B",  # Labour red
  "LD"    = "#FAA61A",  # Lib Dem orange
  "SNP"   = "#F6EF00",  # SNP yellow
  "PC"    = "#008142",  # Plaid green
  "Green" = "#33A532",  # Green green
  "RUK"   = "#12B6CF",  # Reform UK teal-ish
  "Ind"   = "#7F7F7F"   # Independent grey
)


f2 <- ggplot(shp_map) +
  geom_sf(aes(fill = winner_from_con), colour = "gray") +
  scale_fill_manual(
    values = party_cols,
    name = "Gained from Conservatives",
    na.value = "grey85",
    na.translate = FALSE,   # <-- this removes the NA tile from the legend
    drop = TRUE
  ) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  filename = "/Users/alexander/Documents/benites-unsworth/figs/vict.jpeg", 
  plot = f2,
  width = 8,
  height = 11,
  dpi = 600
)

# 1) Recompute indicators (explicit, NA-safe)
shp_map <- shp_map %>%
  mutate(
    gain_Lab    = replace_na(winner_from_con == "Lab",  FALSE),
    gain_LD     = replace_na(winner_from_con == "LD",   FALSE),
    gain_RUK    = replace_na(winner_from_con == "RUK",  FALSE),
    gain_Others = replace_na(winner_from_con %in% c("SNP","PC","Green"), FALSE)
  )

# Quick sanity check (expect 182, 60, 5, and 5 for Others)
print(table(shp_map$gain_Lab, useNA = "ifany"))
print(table(shp_map$gain_LD,  useNA = "ifany"))
print(table(shp_map$gain_RUK, useNA = "ifany"))
print(table(shp_map$gain_Others, useNA = "ifany"))  # should be 5 TRUE total

# Make sure everything is ready
stopifnot(inherits(shp_map, "sf"))

# Helper to build one map
make_map <- function(sf_obj, colnm, title) {
  ggplot(sf_obj) +
    geom_sf(aes(fill = .data[[colnm]]), colour = "grey75", size = 0.08) +
    scale_fill_manual(
      values = c(`FALSE` = "grey95", `TRUE` = "grey15"),
      limits = c(FALSE, TRUE),
      drop = FALSE,
      guide = "none"
    ) +
    coord_sf(datum = NA) +
    labs(title = title) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Build four maps
p_lab    <- make_map(shp_map, "gain_Lab",    "Labour")
p_ld     <- make_map(shp_map, "gain_LD",     "Liberal Democrats")
p_ruk    <- make_map(shp_map, "gain_RUK",    "RUK")
p_others <- make_map(shp_map, "gain_Others", "Others")

# Combine in a 2x2 layout
p_all <- (p_lab + p_ld) / (p_ruk + p_others)

# Save high-resolution composite
ggsave(
  filename = "/Users/alexander/Documents/benites-unsworth/figs/gains_all_2x2.jpeg",
  plot = p_all,
  width = 9,
  height = 12.5,
  dpi = 600
)




#####
breaks <- c(-Inf, 0.50, 0.55, 0.60, 0.65, 0.70, Inf)
labels <- c("<50%", "50–54%", "55–59%", "60–64%", "65–69%", "≥70%")

shp_map_turnout <- shp2024 %>%
  mutate(
    turnout_bin = cut(turnout_2024, breaks = breaks, labels = labels, include.lowest = TRUE)
  )


f_turnout <- ggplot(shp_map_turnout) +
  geom_sf(aes(fill = turnout_bin), colour = "gray80", size = 0.1) +
  scale_fill_brewer(
    palette = "YlGnBu",
    name = "Turnout (%)",
    na.value = "grey90",
    drop = TRUE
  ) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) 

ggsave(
  filename = "/Users/alexander/Documents/benites-unsworth/figs/turnout_breaks.jpeg",
  plot = f_turnout,
  width = 8,
  height = 11,
  dpi = 600
)


f_turnout_cont <- ggplot(shp2024) +
  geom_sf(aes(fill = turnout_2024), colour = "grey70", size = 0.1) +
  scale_fill_gradient(
    low = "grey95", high = "grey10",
    name = " ",
    labels = label_percent(accuracy = 1),
    limits = c(0.50, 0.70),          # clamp to your range of interest
    oob = squish,                    # values outside limits will be squished
    na.value = "grey95"
  ) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  filename = "/Users/alexander/Documents/benites-unsworth/figs/turnout_continuous.jpeg",
  plot = f_turnout_cont,
  width = 8, height = 11, dpi = 600
)

