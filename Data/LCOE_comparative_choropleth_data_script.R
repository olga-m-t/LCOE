## LCOE by County: As-Is vs. With Externalities

library(tidyverse)  # dplyr, ggplot2, readr, stringr, etc.
library(sf)          # spatial data (county polygons) + geom_sf()
library(tigris)       # downloads US Census county boundary shapefiles

options(tigris_use_cache = TRUE) 
#check working directory
getwd()
setwd("~/Documents/GitHub/LCOE/Data")

## in the source files, the "State" and "County" columns are mislabeled/swapped. "State" actually holds the county name, and "County" actually
## holds the state name
as_is <- read_csv("LCOE_data_as_is.csv") %>%
  mutate(FIPS = str_pad(FIPS, 5, pad = "0")) %>%
  rename(County = State, State = County) %>%
  rename(tech_asis = `Cheapest Technology`, cost_asis = Cost)

with_ext <- read_csv("LCOE_data_with_externalitiescsv.csv") %>%
  mutate(FIPS = str_pad(FIPS, 5, pad = "0")) %>%
  rename(County = State, State = County) %>%
  rename(tech_ext = `Cheapest Technology`, cost_ext = Cost)

lcoe <- as_is %>%
  select(FIPS, State, County, tech_asis, cost_asis) %>%
  left_join(with_ext %>% select(FIPS, tech_ext, cost_ext), by = "FIPS")

## county geometry
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022) %>%
  shift_geometry(position = "below") %>%   
  select(GEOID, geometry)

map_data <- counties_sf %>%
  left_join(lcoe, by = c("GEOID" = "FIPS")) %>%
  filter(!is.na(tech_asis))  # drop counties with no match (e.g. territories)

## Reshape into long format so both scenarios can be faceted
## Each county appears twice: once for "Without externalities" and once
## for "With externalities priced in..." -- facet_wrap() then draws one
## panel per scenario, using a single shared legend for both.
panel_levels <- c(
  "LCOE without externalities",
  "LCOE with externalities priced in at $62/tCO2*"
)

map_long <- bind_rows(
  map_data %>%
    mutate(technology = tech_asis, panel = panel_levels[1]),
  map_data %>%
    mutate(technology = tech_ext, panel = panel_levels[2])
) %>%
  mutate(panel = factor(panel, levels = panel_levels)) %>%
  select(GEOID, technology, panel, geometry)

## ---- 4. Color palette + footnote text ----
tech_colors <- c(
  "Natural Gas Combined Cycle" = "#7F7053",  # GRAY!
  "Wind"                       = "#1B9E77",  # teal/green
  "Solar PV (Utility)"         = "#E6AB02"   # gold
)

externality_caption <- "*Externalities priced at $62/tCO2, per Rhodes et al.,\n\u201cA geographically resolved method to estimate levelized power\nplant costs with environmental externalities.\u201d"

## ---- 5. Build the combined choropleth ----
choropleth <- ggplot(map_long) +
  geom_sf(aes(fill = technology), color = "white", linewidth = 0.02) +
  facet_wrap(~panel, ncol = 2) +
  scale_fill_manual(values = tech_colors, name = "Cheapest Technology") +
  labs(
    title = "Least-Cost Electricity Generation Technology by County",
    caption = externality_caption
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 8)),
    strip.text = element_text(size = 17, color = "grey20", face = "bold"),  # per-panel subtitle
    plot.caption = element_text(size = 7, color = "grey45", hjust = 0, face = "italic")
  )

choropleth
ggsave("lcoe_choropleth_side_by_side.png", choropleth,
       width = 13, height = 6.5, dpi = 300, bg = "white")

cat("\nDone. File saved:\n- lcoe_choropleth_side_by_side.png\n")