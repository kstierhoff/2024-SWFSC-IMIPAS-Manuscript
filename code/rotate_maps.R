library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

# From https://stackoverflow.com/questions/31873151/how-rotate-map-in-r

# Rotate an sf geom around a center point. If no center is
# specified then it rotates around the center of the geom.
# This is technically an affine transformation: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
st_ellide_rotate = function(x, degrees, center_coords=NULL){
  if(degrees < -360 | degrees > 360) stop('Degrees must be in the range -360 to 360')
  x = sf::st_combine(x)
  if(is.null(center_coords)){
    center_coords = sf::st_centroid(x)
  }
  radians = degrees * pi/180
  transform_matrix = matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)

  return((x-center_coords) * transform_matrix + center_coords)
}

# Download country boundaries
countries = rnaturalearth::ne_countries(scale = 10,returnclass = 'sf') %>%
  filter(name %in% c("United States of America", "Mexico", "Canada"))

states <- ne_states(country = 'United States of America', returnclass = 'sf')

saved_crs = st_crs(countries)

rot.deg <-   30
rot.x   <- -120
rot.y   <-   22

countries_rotated  <-  countries %>%
  st_ellide_rotate(rot.deg, center_coords = c(rot.y, rot.x))
# st_crs(countries_rotated) <- saved_crs

ggplot() +
  geom_sf(data = countries_rotated)

# # Works with 30deg centered on 22/-120
base.map.rot <- ggplot() +
  geom_sf(data = countries_rotated) +
  coord_sf(xlim = c(-30,-18), ylim=c(70, 107)) +
  # labs(subtitle = paste('rotated:', rot.deg, ' deg\n', 'centered on:', rot.y, ', ',rot.x)) +
  # theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# # Works with 30deg centered on 22/-120
base.map.rot2 <- ggplot() +
  geom_sf(data = countries_rotated) +
  coord_sf(xlim = c(-30,-18), ylim=c(70, 107)) +
  # labs(subtitle = paste('rotated:', rot.deg, ' deg\n', 'centered on:', rot.y, ', ',rot.x)) +
  # theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

saveRDS(base.map.rot, here::here("data/base_map_rotated.rds"))
base.map.rot <- readRDS(here::here("data/base_map_rotated.rds"))

ggsave(base.map.rot,
       file = here::here("Figs/basemap_rotated_30.png"),
       width = 2)

# # Works with 40deg centered on 32/-120
# ggplot() +
#   geom_sf(data=countries_rotated) +
#   coord_sf(xlim = c(12,20), ylim=c(80, 115)) +
#   labs(subtitle = paste('rotated:', rot.deg, ' deg, centered on:', rot.y, ', ',rot.x))

ggsave(here::here("Figs/basemap_rotated.png"),
       width = 3)


# states_rotated = states %>%
#   st_ellide_rotate(40, center_coords = c(32,-120))
# # applying an affine transformation nulls the CRS for some reason, so reset it here
# st_crs(states_rotated)    <- saved_crs
#
#
#
# ggplot() +
#   geom_sf(data=states_rotated) +
#   geom_sf(data=points, size=1) +
#   geom_sf_label(data=points, aes(label=name), nudge_x=1) +
#   coord_sf(xlim = c(-1,7), ylim=c(34,42)) +
#   labs(subtitle = 'rotated 20 deg')
