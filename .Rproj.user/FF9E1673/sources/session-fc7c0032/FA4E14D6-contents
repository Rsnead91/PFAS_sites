# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','readxl','units','sf')

# 2. load data ----

## 2.1 counties ----

pa_counties <- tigris::counties(state = "42", cb = TRUE)

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
                    filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County"))

# ggplot(cms_counties_filter) +
#   geom_sf(color = NA, aes(fill = std_med_payments)) +
#   coord_sf(crs = 5070) +
#   theme_void()


api_key <- "e0e0537e1640689fd0719a4f97581e336b10c8ce"
census_api_key(api_key, overwrite=TRUE, install=TRUE)

readRenviron("~/.Renviron")

options(tigris_use_cache = TRUE)

