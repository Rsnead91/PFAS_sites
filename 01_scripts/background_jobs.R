
if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','units','sf','ggmap','biscale','reshape2','janitor','ggpattern','tidygeocoder','lubridate', 'openxlsx','spdep','cowplot')

options(scipen = 999)

mun_00pop <- st_intersection(st_make_valid(sf::st_as_sf(mun_red)), st_make_valid(sf::st_as_sf(tract_00pop_20geo_studyarea_geo))) %>% 
  st_make_valid() %>% 
  mutate(
    area.int = st_area(.),
    int.pct = area.int/area.mun
  ) %>% 
  drop_units() %>% 
  group_by(COUNTY, MUNICIPAL_, MUNICIPAL1) %>%
  summarize(
    across(starts_with("P0"), ~ round(sum(.x * int.pct), 0), .names = "{col}")
  )

mun_fun2 <- function(df_in, df_out){
  df_geo <- left_join(df_in, tract20 %>% dplyr::select(GEOID, geometry), by = "GEOID")
  
  df_int <- st_intersection(st_make_valid(sf::st_as_sf(mun_red)), st_make_valid(sf::st_as_sf(df_geo)))
  
  df_int$area.int <- st_area(st_make_valid(df_int))
  
  df_int <- df_int %>% 
    mutate(
      int.pct = area.int/area.mun
    ) %>% 
    drop_units() %>% 
    group_by(COUNTY, MUNICIPAL_, MUNICIPAL1) %>%
    summarize(
      across(starts_with("B0"), ~ round(sum(.x * int.pct), 0), .names = "{col}")
    )
  
  assign(df_out, df_int, envir = .GlobalEnv)
}

mun_fun2(tract_1620pop_20geo_studyarea, "mun_1620pop")
mun_fun2(tract_1721pop_20geo_studyarea, "mun_1721pop")
mun_fun2(tract_1822pop_20geo_studyarea, "mun_1822pop")
