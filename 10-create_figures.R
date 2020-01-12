

# Maps in R ---------------------------------------------------------------

# Read in the NEON plot boundary for NIWO,
# downloaded from: https://www.neonscience.org/data/about-data/spatial-data-maps
terrestrial_bounds <- sf::st_read("data/data_raw/neon_spatial_data_maps/fieldSamplingBoundaries/terrestrialSamplingBoundaries.shp")
niwo_bounds <- terrestrial_bounds[terrestrial_bounds$siteName == "Niwot Ridge Mountain Research Station",]

# NEON domain boundaries 
neon_domains <- sf::st_read("data/data_raw/neon_spatial_data_maps/NEONDomains_0/NEON_Domains.shp")

# create a SF point for the location of the NIWO site 
# NIWO latitude, longitude
# 40.05425, -105.58237



# Tutorial about maps in R: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# get country data for the entire world 
world <- ne_countries(scale = "medium", returnclass = "sf")

# get states data (admin level 1 in USA)
library(USAboundaries)
states <- USAboundaries::us_states()

# isolate a single state from the bunch
colorado <- states[states$name == "Colorado",]

# make a map with a scale bar and north arrow using the ggspatial package 
library(ggspatial)
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = states) + 
  geom_sf(data = colorado, color = "black", fill = "darkgrey") + 
  #geom_sf(data = niwo_bounds) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-125, -70), ylim = c(25, 50)) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() 

  # ggtitle("Main plot title")


