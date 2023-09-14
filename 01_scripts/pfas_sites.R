# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','readxl','units','sf','ggmap')

# 2. load data ----

## 2.1 counties ----

# pa_counties <- tigris::counties(state = "42", cb = TRUE)

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
                    filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")) %>% 
                    st_set_crs(st_crs(4269))

# ggplot(cms_counties_filter) +
#   geom_sf(color = NA, aes(fill = std_med_payments)) +
#   coord_sf(crs = 5070) +
#   theme_void()

## 2.2 superfund sites ----

superfund <- st_as_sf(
                get_spatial_layer("https://gispub.epa.gov/arcgis/rest/services/OEI/FRS_INTERESTS/MapServer/21", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
                dplyr::filter(STATE_CODE == "PA" & COUNTY_NAME %in% c("BUCKS","CHESTER","DELAWARE","LEBANON","LEHIGH","MONROE","MONROE COUNTY","MONTGOMERY","MONTGOMERY COUNTY","SCHUYLKILL") & PRIMARY_NAME %in% c("BOARHEAD FARMS","CHEM-FAB","NAVAL AIR DEVELOPMENT CENTER (8 WASTE AREAS)","WATSON JOHNSON LANDFILL","BISHOP TUBE CO","OLD WILMINGTON ROAD GW CONTAMINATION","STRASBURG LANDFILL","OLD WILMINGTON ROAD SITE - AKA - OLD WILM. RD. GRNDWTR. CONTAMTN","LOWER DARBY CREEK AREA","US ARMY NATIONAL GUARD FORT INDIANTOWN GAP","RODALE MANUFACTURING CO., INC.","HIGH QUALITY POLISHING & PLATING","US ARMY TOBYHANNA ARMY DEPOT","NORTH PENN - AREA 12","WILLOW GROVE NAVAL AIR AND AIR RESERVE STATION","NORTH PENN - AREA 5","NORTH PENN - AREA 2","BOYERTOWN BOVINE SITE","OLD FRACKVILLE DUMP")) %>% # only keeping 12 study area counties with a superfund site
                dplyr::select(PRIMARY_NAME, COUNTY_NAME, LATITUDE83, LONGITUDE83, FAC_URL, ACTIVE_STATUS) %>% 
                st_drop_geometry(),
              coords = c("LONGITUDE83","LATITUDE83")
            ) %>% 
            st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
            st_transform(crs = 4269) # set projection to match other geo files
  

## 2.3 hsca sites ----

non_superfund <- st_as_sf(
                  data.frame(
                  name = c("Easton Road","National Foam"),
                  county = c("Bucks","Chester"),
                  site = c("HSCA","PA DEP identified"),
                  lat = c("40.333356748990184","39.95923053157384"),
                  lon = c("-75.12744390670184","-75.59636003557287")
                ),
                coords = c("lon","lat")
              ) %>% 
              st_set_crs(st_crs(4269))


## 2.4 Neshaminy Creek Watershed ----

neshaminy_creek <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/BucksCounty/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(NAME == "NESHAMINY CREEK") # only keeping area of watershed


## 2.5 Ridge Run - East and West Rockhill Townships, Bucks County ----

ridge_run <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennDOT/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(FIPS_COUNT == "017" & MUNICIPAL1 %in% c("EAST ROCKHILL","WEST ROCKHILL")) # only keeping estimated area of landfill

## repairing simple geometry issues in municipality and pws shapefiles
ridge_run <- st_make_valid(ridge_run)



## 2.3 pasda (public water supply areas) ----

pws.shp <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP2/MapServer/8", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(CNTY_NAME %in% c("Berks", "Bucks", "Carbon", "Chester", "Delaware", "Lancaster", "Lebanon", "Lehigh", "Monroe", "Montgomery", "Northampton", "Schuylkill")) # only keeping 12 study area counties

## repairing simple geometry issues in pws shapefiles
pws.shp <- st_make_valid(pws.shp)

pws.pfastest <- read_excel("00_data/Public Water Supply x PFAS Test_onlysheet1.xlsx", .name_repair = "universal") %>% # .name_repair replaces spaces in variable names with '.' 
  mutate(tested = Tested.by.DEP.or.UCMR.3, # creating new variables for merging and easier variable naming
         PWS_ID = as.character(pwsid)) %>%
  dplyr::select(PWS_ID, tested) # only keeping PWS ID and testing column

pws.shp.pfas <- merge(x = pws.shp, y = pws.pfastest, by = "PWS_ID", all = TRUE) %>% # merge pws shapefiles and pfas testing data, retaining merge matches on x and y
  filter(tested == 1 & !is.na(area.pws) & !(OWNERSHIP %in% c("Mobile Home Park","Apartments","Institutional Recreational","Institutional Military","Institutional Health","Institutional Education","Institutional Correctional"))) %>% # retaining pws with testing data and non-missing and not an apartment, mobile home park, institutional corrections
  dplyr::select(PWS_ID, WUDS_ID, area.pws, tested) # subsetting to only necessary variables






## visually reviewing the shapefiles
ggplot() +
  geom_sf(data = study_counties) +
  geom_sf(data = neshaminy_creek, fill = "lightblue", color = "lightblue") +
  geom_sf(data = non_superfund, color = "red") +
  geom_sf(data = superfund, color = "red") +
  geom_sf(data = ridge_run, fill = "red", color = "red")

+
  geom_sf(data = pws.shp.pfas, fill = "transparent", color = "white", lwd = .5)


NESHAMINY WATERSHED SHOULD GO INTO MONTGOMERY COUNTY




