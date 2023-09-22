# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','readxl','units','sf','ggmap','reshape2','janitor','ggpattern','tidygeocoder')

# 2. load data ----

## 2.1 counties ----

# pa_counties <- tigris::counties(state = "42", cb = TRUE)

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
                    filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")) %>% 
                    st_set_crs(st_crs(4269))

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

PFAS_detected_superfund_others <- read_excel("00_data/PFAS_detected_superfund_others.xlsx") %>% 
                                    clean_names() %>% 
                                    filter(site == "Superfund" | name %in% c("BISHOP TUBE CO","US ARMY NATIONAL GUARD FORT INDIANTOWN GAP","HIGH QUALITY POLISHING & PLATING","OLD FRACKVILLE DUMP")) %>% 
                                    dplyr::select(-c(address,notes,links_to_more_info))

superfund_merge <- merge(x = superfund %>% 
                              dplyr::select(PRIMARY_NAME) %>% 
                              rename(name = PRIMARY_NAME)
                         , 
                         y = PFAS_detected_superfund_others, 
                         by = "name")


## 2.3 hsca sites ----

non_superfund <- st_as_sf(
                  data.frame(
                  name = c("Easton Road","National Foam"),
                  county = c("Bucks","Chester"),
                  site = c("State Advisory","State Advisory"),
                  type = c("Manufacturing","Manufacturing"),
                  groundwater = c("Yes","Unk"),
                  surface_water = c("Yes","Unk"),
                  electronic_manufacturing = c("No","No"),
                  active = c("Yes","Yes"),
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

neshaminy_creek2 <- data.frame(
                      county = "BUCKS + MONTGOMERY",
                      site = "State Advisory",
                      type = "Watershed",
                      groundwater = "Unk",
                      surface_water = "Yes",
                      electronic_manufacturing = "No",
                      active = "Yes"
                    )

neshaminy_creek <- cbind(neshaminy_creek,neshaminy_creek2) %>%
                    dplyr::select(NAME,county,site,type,groundwater,surface_water,electronic_manufacturing,active) %>%
                    rename(name = NAME)

## 2.5 Ridge Run - East and West Rockhill Townships, Bucks County ----

ridge_run <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennDOT/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(FIPS_COUNT == "017" & MUNICIPAL1 %in% c("EAST ROCKHILL","WEST ROCKHILL")) # only keeping estimated area of landfill

## repairing simple geometry issues in municipality and pws shapefiles
ridge_run <- st_make_valid(ridge_run)

## dissolving the two municipalities into a single shape and findinf the centroid
ridge_run_union <- ridge_run %>%  
                    group_by(FIPS_COUNT) %>% 
                    summarise() %>% 
                    st_centroid()

ridge_run2 <- data.frame(
                name = "Ridge Run",
                county = "BUCKS",
                site = "State Advisory",
                type = "Tire fire extinguished by FF foam",
                groundwater = "Yes",
                surface_water = "Yes",
                electronic_manufacturing = "No",
                active = "Yes"
              )

ridge_run <- cbind(ridge_run_union,ridge_run2) %>% 
              dplyr::select(-FIPS_COUNT)


## facilities with pfas discharge 

facilities_discharge_pfas <- st_read("00_data/Facilities_dishcarge_PFAS.shp") %>% 
  st_drop_geometry() %>% 
  filter(Facility_N %in% c("PARK CREEK STP","FOREST PARK WATER/CHALFONT","READING BERKS FIRE TRAINING CENTER")) %>% 
  dplyr::select(Facility_N,Watershed_,Facility_L,Facility_1) %>% 
  rename(name = Facility_N) %>% 
  st_as_sf(coords = c("Facility_1","Facility_L")) %>% 
  st_set_crs(st_crs(4269)) %>% 
  st_transform(crs = 4269)

facilities_discharge_pfas2 <- data.frame(
  site = c("Discharge","Discharge","Discharge"),
  type = c("Water Treatment Plant","Water Treatment Plant","Firefighter training"),
  groundwater = c("","",""),
  surface_water = c("","",""),
  electronic_manufacturing = c("No","No","No"),
  active = c("","","")
)

facilities_discharge_pfas <- cbind(facilities_discharge_pfas,facilities_discharge_pfas2)



## electronics manufacturing in pa registered with federal govt

electric <- read_csv("00_data/EntityPAElectronicsManufacturing.csv") %>% 
              clean_names() %>% 
              mutate(address = str_c(address_line_1,", ",city,", ",state_province," ",zip_code)) %>% 
              tidygeocoder::geocode(address = address, method = 'osm', lat = latitude , long = longitude)

electric_na <- electric %>% 
                filter(is.na(latitude)) %>% 
                dplyr::select(-c(latitude,longitude))

electric_na_retry <- electric_na %>% 
                      tidygeocoder::geocode(street = address_line_1, city = city, state = state_province, postalcode = zip_code, method = 'census', lat = latitude , long = longitude)

electric_nona <- rbind(
                  electric %>% 
                   filter(!is.na(latitude)),
                  electric_na_retry %>% 
                   filter(!is.na(latitude))
                )

electric_geo1 <- st_as_sf(
                  electric_nona,
                  coords = c("longitude","latitude")
                ) %>% 
                  st_set_crs(st_crs(4269))

electric_geo_study <- st_intersection(study_counties, electric_geo1)






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
                  rename(any_pfas = Overall.Detected..Any.PFAS.Value,
                         pfas_over_mrl = Overall.Detected.6.PFAS.Value.at.or.above.MRL) %>%
                  dplyr::select(PWS_ID, tested, any_pfas, pfas_over_mrl) # only keeping PWS ID and testing column

pws.shp.pfas <- merge(x = pws.shp, y = pws.pfastest, by = "PWS_ID", all = TRUE) %>% # merge pws shapefiles and pfas testing data, retaining merge matches on x and y
  filter(tested == 1 & !(OWNERSHIP %in% c("Mobile Home Park","Apartments","Institutional Recreational","Institutional Military","Institutional Health","Institutional Education","Institutional Correctional")))

pws.shp.pfas_inst <- merge(x = pws.shp, y = pws.pfastest, by = "PWS_ID", all = TRUE) %>% # merge pws shapefiles and pfas testing data, retaining merge matches on x and y
                      filter(tested == 1)






## ucmr 5 data ----

ucmr5 <- read_tsv("00_data/UCMR5_ALL_MA_WY.txt") %>% 
          filter(State == "PA" & Contaminant != "lithium") %>% 
          dplyr::select(PWSID, CollectionDate, Contaminant, MRL, Units, AnalyticalResultsSign, AnalyticalResultValue, FacilityWaterType, SamplePointName) %>% 
          mutate(
            PWSID = substr(PWSID,3,10)
          )

reduce_ucmr_to_study_pwsid <- pws.shp %>% 
                                dplyr::select(PWS_ID) %>% 
                                st_drop_geometry()

ucmr5_long <- left_join(x=reduce_ucmr_to_study_pwsid, y=ucmr5, by = c("PWS_ID" = "PWSID")) %>% 
                drop_na(CollectionDate)

#to do multiple columns, have to put setDT around data name

ucmr5_wide1 <- janitor::clean_names(
                dcast(
                data = ucmr5_long,
                formula = PWS_ID + CollectionDate ~ Contaminant,
                value.var = "MRL"
                )
              )




# maps ----

## pfas detected sites ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2",
      "Unknown" = "royalblue3"
    ),
    labels = c("Discharge","Non-superfund","Superfund","Unknown")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites.png", height = 6, width = 7, units = "in")


## pfas sites with pws ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = filter(pws.shp.pfas_inst, any_pfas == 1), fill = "grey90", color = "grey50") +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition and PFAS Positive Public Water Supply") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2"
    ),
    labels = c("Discharge","Non-superfund","Superfund")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites_pws.png", height = 6, width = 7, units = "in")

## pfas sites with pws pfas over mrl ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = filter(pws.shp.pfas_inst, pfas_over_mrl == 1), fill = "grey90", color = "grey50") +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition and PFAS Positive Public Water Supply") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2"
    ),
    labels = c("Discharge","Non-superfund","Superfund")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites_pws_pfas_over_mrl.png", height = 7, width = 8, units = "in")














# map electronic manufacturing facilities

## pfas detected sites ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = electric_geo_study, aes(color = "Unknown", shape = "Manufacturing"), size = 1.5) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2",
      "Unknown" = "royalblue3"
    ),
    labels = c("Discharge","Non-superfund","Superfund","Unknown")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites_electric.png", height = 6, width = 7, units = "in")


## pfas sites with pws ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = filter(pws.shp.pfas_inst, any_pfas == 1), fill = "grey90", color = "grey50") +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = electric_geo_study, aes(color = "Unknown", shape = "Manufacturing"), size = 1.5) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition and PFAS Positive Public Water Supply") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2",
      "Unknown" = "royalblue3"
    ),
    labels = c("Discharge","Non-superfund","Superfund","Unknown")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites_pws_electric.png", height = 7, width = 8, units = "in")

## pfas sites with pws pfas over mrl ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = filter(pws.shp.pfas_inst, pfas_over_mrl == 1), fill = "grey90", color = "grey50") +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = electric_geo_study, aes(color = "Unknown", shape = "Manufacturing"), size = 1.5) +
  geom_sf(data = non_superfund, aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Water Treatment Plant"), aes(color = "Discharge", shape = "Water Treatment Plant"), size = 3.5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Firefighter training"), aes(color = "Discharge", shape = "Firefighter training"), size = 5) +
  geom_sf(data = filter(facilities_discharge_pfas, type == "Manufacturing"), aes(color = "Discharge", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "Superfund"), aes(color = "Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Superfund"), aes(color = "Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "Superfund"), aes(color = "Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Landfill" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Landfill"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type %in% c("Manufacturing", "Manufacturing/Military") & site == "Non-Superfund"), aes(color = "Non-Superfund", shape = "Manufacturing"), size = 2.5) +
  geom_sf(data = filter(superfund_merge, type == "Military" & site == "State Advisory"), aes(color = "Non-Superfund", shape = "Military"), size = 2.5) +
  geom_sf(data = ridge_run, aes(color = "Non-Superfund", shape = "Tire fire extinguished by FF Foam"), size = 5) +
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition and PFAS Positive Public Water Supply") +
  scale_fill_manual(
    values = c("Non-Superfund" = "mediumpurple3")
  ) +
  scale_color_manual(
    values = c(
      "Discharge" = "forestgreen",
      "Non-Superfund" = "mediumpurple3",
      "Superfund" = "firebrick2",
      "Unknown" = "royalblue3"
    ),
    labels = c("Discharge","Non-superfund","Superfund","Unknown")
  ) +
  scale_shape_manual(
    values = c(
      "Landfill" = 15,
      "Manufacturing" = 16,
      "Military" = 17,
      "Water Treatment Plant" = 18,
      "Firefighter training" = 8,
      "Tire fire extinguished by FF Foam" = 4
    ),
    labels = c("Firefighter Training","Landfill","Manufacturing","Military","Tire Fire + FF Foam","Water Treatment Plant")
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Govt Recognition",override.aes=list(fill="white")),
    shape = guide_legend(title = "Site Type",override.aes=list(fill="white"))
  ) +
  theme(
    plot.background = element_rect(color = "black", linewidth = 3),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
    panel.background = element_rect(fill = "white"),
    title = element_text(family = "Arial", size = 14),  # Set font size here
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key = element_rect(colour = NA, fill = NA)
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )) 

ggsave("02_output/PFAS_sites_pws_pfasovermrl_electric.png", height = 7, width = 8, units = "in")








