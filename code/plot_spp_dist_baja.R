pacman::p_load(tidyverse, sf, mapview, here)

anch.dist <- st_read(here::here("data/mexico_polygons/habitat_sardina_monterrey.gpkg")) %>%
  mutate(species = "anchovy")
sar.dist <- st_read(here::here("data/mexico_polygons/habitat_e.mordax.gpkg")) %>%
  mutate(species = "sardine")

all.dist <- bind_rows(anch.dist, sar.dist)

mapview(anch.dist)
mapview(sar.dist, col.regions = NA) + mapview(anch.dist)

mapview(all.dist, zcol = "species")
