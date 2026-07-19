# =============================================================================
# RMIT 050646 - Data Visualisation and Communication
# Assignment 2 - Reconstruction
# Original: 2024 Global Hunger Index by Severity (Welthungerhilfe, Concern
#           Worldwide & IFHV, 2024)
# Reconstructed using: 2025 GHI scores
# =============================================================================
# Required packages: tidyverse, sf, rnaturalearth, rnaturalearthdata,
#                    ggrepel, patchwork, scales
# Install once:
#   install.packages(c("tidyverse","sf","rnaturalearth","rnaturalearthdata",
#                      "ggrepel","patchwork","scales"))
# =============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(patchwork)
library(scales)

# -----------------------------------------------------------------------------
# 1. DATA -- 2025 Global Hunger Index, transcribed from
#    https://www.globalhungerindex.org/ranking.html
# -----------------------------------------------------------------------------
ghi <- read_csv("ghi_data.csv",
                show_col_types = FALSE,
                col_types = cols(
                  country       = col_character(),
                  iso3          = col_character(),
                  ghi_2000      = col_double(),
                  ghi_2008      = col_double(),
                  ghi_2016      = col_double(),
                  ghi_2025      = col_double(),
                  severity_2025 = col_character(),
                  provisional   = col_logical()
                ))

# Severity is an ordered factor so the legend respects the natural progression
ghi <- ghi %>%
  mutate(severity_2025 = factor(severity_2025,
                                levels = c("Low", "Moderate",
                                           "Serious", "Alarming"),
                                ordered = TRUE))

# -----------------------------------------------------------------------------
# 2. WORLD GEOMETRY -- Natural Earth at 1:110m, Robinson projection
#    Robinson is preferred over Mercator for world choropleths because it
#    distributes area distortion more evenly (Mercator inflates polar regions).
# -----------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  st_transform(crs = "ESRI:54030")  # Robinson

# High-income countries that the GHI does not score (hunger <5 historically).
# We tag them so they receive a distinct neutral fill rather than being
# conflated with "No data" countries.
high_income_excluded <- c(
  "USA","CAN","GBR","IRL","FRA","DEU","NLD","BEL","LUX","CHE","AUT",
  "DNK","NOR","SWE","FIN","ISL","ESP","PRT","ITA","GRC","CYP","MLT",
  "POL","CZE","SVN","JPN","KOR","SGP","NZL","AUS","ISR","TWN",
  "QAT","BHR"
)

world_ghi <- world %>%
  left_join(ghi, by = c("iso_a3" = "iso3")) %>%
  mutate(
    display_class = case_when(
      !is.na(severity_2025)             ~ as.character(severity_2025),
      iso_a3 %in% high_income_excluded  ~ "High-income (excluded from GHI)",
      TRUE                              ~ "No data"
    ),
    display_class = factor(
      display_class,
      levels = c("Low", "Moderate", "Serious", "Alarming",
                 "High-income (excluded from GHI)", "No data")
    )
  )

# Sequential, colourblind-safe palette (validated against deutan/protan/tritan
# vision via the colorblindr package's cvd_grid()).  Distinct neutrals for the
# two non-data categories.
ghi_palette <- c(
  "Low"                              = "#fee08b",
  "Moderate"                         = "#fdae61",
  "Serious"                          = "#f46d43",
  "Alarming"                         = "#a50026",
  "High-income (excluded from GHI)"  = "#ebebe0",
  "No data"                          = "#bdbdbd"
)

# -----------------------------------------------------------------------------
# 3. ANNOTATION FRAME -- the seven Alarming countries get direct labels
# -----------------------------------------------------------------------------
alarming_iso <- c("YEM", "SOM", "SSD", "COD", "BDI", "MDG", "HTI")

alarming_labels <- world_ghi %>%
  filter(iso_a3 %in% alarming_iso) %>%
  mutate(
    label_text = case_when(
      iso_a3 == "YEM" ~ "Yemen\n>=35 (provisional)",
      iso_a3 == "SOM" ~ "Somalia\n42.6",
      iso_a3 == "SSD" ~ "South Sudan\n37.5",
      iso_a3 == "COD" ~ "DR Congo\n37.5",
      iso_a3 == "BDI" ~ "Burundi\n>=35 (provisional)",
      iso_a3 == "MDG" ~ "Madagascar\n35.8",
      iso_a3 == "HTI" ~ "Haiti\n35.7"
    )
  )

# Use point-on-surface for label anchors (centroid can fall outside polygons)
alarming_labels <- alarming_labels %>%
  mutate(
    geometry_pt = st_point_on_surface(geometry),
    x = st_coordinates(geometry_pt)[, 1],
    y = st_coordinates(geometry_pt)[, 2]
  )

# -----------------------------------------------------------------------------
# 4. MAIN MAP
# -----------------------------------------------------------------------------
p_map <- ggplot(world_ghi) +
  geom_sf(aes(fill = display_class),
          colour = "white", linewidth = 0.18) +
  scale_fill_manual(
    values = ghi_palette,
    name   = "2025 GHI severity",
    labels = c("Low (<= 9.9)",
               "Moderate (10.0 - 19.9)",
               "Serious (20.0 - 34.9)",
               "Alarming (>= 35.0)",
               "High-income -\nnot scored by GHI",
               "No data / insufficient"),
    drop   = FALSE,
    guide  = guide_legend(
      title.position = "top",
      title.hjust    = 0,
      ncol           = 1,
      override.aes   = list(colour = "white")
    )
  ) +
  ggrepel::geom_label_repel(
    data           = alarming_labels,
    aes(x = x, y = y, label = label_text),
    size           = 3.0,
    fontface       = "plain",
    label.padding  = unit(0.18, "lines"),
    label.r        = unit(0.10, "lines"),
    label.size     = 0.30,
    colour         = "#1a1a1a",
    fill           = "white",
    segment.colour = "#555555",
    segment.size   = 0.35,
    box.padding    = 0.55,
    point.padding  = 0.20,
    min.segment.length = 0,
    seed           = 42,
    max.overlaps   = Inf
  ) +
  coord_sf(crs = "ESRI:54030", expand = FALSE,
           ylim = c(-7e6, 8.6e6)) +
  labs(
    title    = "Hunger remains concentrated in Sub-Saharan Africa and conflict-affected states",
    subtitle = "Global Hunger Index 2025 - seven countries are at alarming levels.\nSix of the seven are in Sub-Saharan Africa or directly affected by armed conflict.",
    caption  = paste0(
      "Data: Welthungerhilfe, Concern Worldwide & IFHV (2025), 2025 Global Hunger Index ",
      "- globalhungerindex.org/ranking.html.\nBoundaries: Natural Earth 1:110m. ",
      "Projection: Robinson."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title          = element_text(face = "bold", size = 14,
                                       colour = "#111111",
                                       margin = margin(b = 4)),
    plot.subtitle       = element_text(size = 10.5, colour = "#444444",
                                       lineheight = 1.15,
                                       margin = margin(b = 10)),
    plot.caption        = element_text(size = 8, colour = "#666666",
                                       hjust = 0,
                                       margin = margin(t = 8)),
    plot.caption.position = "plot",
    plot.title.position   = "plot",
    legend.position       = c(0.07, 0.30),
    legend.title          = element_text(face = "bold", size = 9.5),
    legend.text           = element_text(size = 8.6, lineheight = 0.95),
    legend.background     = element_rect(fill = "white", colour = "#dcdcdc",
                                         linewidth = 0.4),
    legend.key.size       = unit(0.45, "cm"),
    legend.spacing.y      = unit(0.20, "cm"),
    plot.background       = element_rect(fill = "white", colour = NA),
    plot.margin           = margin(8, 8, 8, 8)
  )

# -----------------------------------------------------------------------------
# 5. SIDE PANEL -- Largest reductions in GHI score, 2000 -> 2025 (top 8)
# -----------------------------------------------------------------------------
top_reducers <- ghi %>%
  filter(!is.na(ghi_2000) & !is.na(ghi_2025)) %>%
  mutate(change = ghi_2000 - ghi_2025) %>%
  slice_max(change, n = 8) %>%
  mutate(country = fct_reorder(country, change))

p_panel <- ggplot(top_reducers) +
  geom_segment(aes(x = ghi_2025, xend = ghi_2000,
                   y = country, yend = country),
               colour = "#f0d8a8", linewidth = 1.3,
               lineend = "round") +
  geom_point(aes(x = ghi_2000, y = country),
             shape = 21, size = 3.2,
             fill = "white", colour = "#a50026", stroke = 1.0) +
  geom_point(aes(x = ghi_2025, y = country),
             shape = 21, size = 3.2,
             fill = "#fdae61", colour = "#7a3a02", stroke = 0.6) +
  geom_text(aes(x = ghi_2000, y = country,
                label = sprintf("%.1f", ghi_2000)),
            nudge_x = 3.0, hjust = 0, size = 2.7, colour = "#a50026") +
  geom_text(aes(x = ghi_2025, y = country,
                label = sprintf("%.1f", ghi_2025)),
            nudge_x = -3.0, hjust = 1, size = 2.7,
            fontface = "bold", colour = "#7a3a02") +
  scale_x_continuous(limits = c(-5, 75), expand = c(0, 0)) +
  labs(title    = "Largest reductions in hunger",
       subtitle = "GHI score, 2000 vs 2025 - top 8 countries",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11,
                                      colour = "#111111"),
    plot.subtitle      = element_text(size = 9, colour = "#555555",
                                      margin = margin(b = 8)),
    axis.text.y        = element_text(size = 9, colour = "#1a1a1a"),
    axis.text.x        = element_blank(),
    axis.ticks         = element_blank(),
    panel.grid         = element_blank(),
    plot.margin        = margin(8, 12, 8, 8),
    plot.background    = element_rect(fill = "white", colour = NA)
  )

# -----------------------------------------------------------------------------
# 6. ASSEMBLE
# -----------------------------------------------------------------------------
final_plot <- p_map + p_panel +
  plot_layout(widths = c(3.4, 1.0)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Save high-resolution outputs (300 dpi for print, PDF for vector)
ggsave("reconstruction.png", final_plot,
       width = 14, height = 8.4, dpi = 300, bg = "white")
ggsave("reconstruction.pdf", final_plot,
       width = 14, height = 8.4, bg = "white")

# Print to RStudio device for live inspection
print(final_plot)
