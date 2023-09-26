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
          dplyr::select(PWSID, CollectionDate, Contaminant, SampleID, FacilityWaterType, MRL, Units, AnalyticalResultsSign, AnalyticalResultValue) %>% 
          mutate(
            PWSID = substr(PWSID,3,10),
            Contaminant = tolower(Contaminant)
          )

colnames(ucmr5) <- tolower(colnames(ucmr5))

reduce_ucmr_to_study_pwsid <- pws.shp %>% 
                                dplyr::select(PWS_ID) %>% 
                                st_drop_geometry()

colnames(reduce_ucmr_to_study_pwsid) <- tolower(colnames(reduce_ucmr_to_study_pwsid))

ucmr5_long <- left_join(x=reduce_ucmr_to_study_pwsid, y=ucmr5, by = c("pws_id" = "pwsid")) %>% 
                drop_na(collectiondate) %>% 
                clean_names()

# add number for collection date
ucmr5_long <- left_join(
                ucmr5_long,
                ucmr5_long %>% 
                  arrange(pws_id, collectiondate) %>%
                  distinct(pws_id, collectiondate) %>%
                  group_by(pws_id) %>% 
                  mutate(date = row_number())
                ,
                by = c("pws_id","collectiondate")
              )

#  tested for pfas and number of samples tested
ucmr5_wide_test <- ucmr5_long %>% 
                    group_by(pws_id) %>% 
                    mutate(num_samples = n()) %>% 
                    ungroup() %>% 
                    distinct(pws_id, num_samples) %>% 
                    mutate(tested = 1)

# number of collection dates at each pws
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      distinct(pws_id, collectiondate) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_dates = n())
                    ,
                    by = "pws_id"
                    )

# number of samples by date
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(date == 1) %>% 
                      group_by(pws_id) %>%
                      mutate(num_samples_date1 = n()) %>% 
                      distinct(pws_id, num_samples_date1)
                    ,
                    by = "pws_id"
                  )

ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(date == 2) %>% 
                      group_by(pws_id) %>%
                      mutate(num_samples_date2 = n()) %>% 
                      distinct(pws_id, num_samples_date2)
                    ,
                    by = "pws_id"
                  )

ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(date == 3) %>% 
                      group_by(pws_id) %>%
                      mutate(num_samples_date3 = n()) %>% 
                      distinct(pws_id, num_samples_date3)
                    ,
                    by = "pws_id"
                  )

# number of tests in groundwater and surface water and number of positive pfas in each
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      distinct(pws_id, collectiondate, facilitywatertype) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_gw = length(facilitywatertype == "GW"),
                                num_sw = length(facilitywatertype == "SW")
                                )
                    ,
                    by = "pws_id"
                  )

# how many pfas chemicals tested
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      distinct(pws_id, contaminant) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_contaminants = n())
                    ,
                    by = "pws_id"
                  )

# any pfas detected
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      group_by(pws_id) %>% 
                      summarise(any_pfas = sum(
                                            ifelse(
                                              sum(analyticalresultssign == "=") > 0,
                                              1, 0
                                            )
                                          ),
                      )
                    ,
                    by = "pws_id"
                  )

# how many chemicals detected above mrl
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(analyticalresultssign == "=") %>% 
                      distinct(pws_id, contaminant) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_detected_contaminants = n())
                    ,
                    by = "pws_id"
)

ucmr5_wide_test["num_detected_contaminants"][is.na(ucmr5_wide_test["num_detected_contaminants"])] <- 0

# how many chemicals detected above mrl from groundwater samples
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(facilitywatertype == "GW" & analyticalresultssign == "=") %>% 
                      distinct(pws_id, contaminant) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_gw_pfas_above_mrl = n())
                    ,
                    by = "pws_id"
)

ucmr5_wide_test["num_gw_pfas_above_mrl"][is.na(ucmr5_wide_test["num_gw_pfas_above_mrl"])] <- 0

# how many chemicals detected above mrl from surface water samples
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(facilitywatertype == "SW" & analyticalresultssign == "=") %>% 
                      distinct(pws_id, contaminant) %>% 
                      group_by(pws_id) %>% 
                      summarise(num_sw_pfas_above_mrl = n())
                    ,
                    by = "pws_id"
)

ucmr5_wide_test["num_sw_pfas_above_mrl"][is.na(ucmr5_wide_test["num_sw_pfas_above_mrl"])] <- 0

# which pfas chemicals over mrl?
ucmr5_wide_test <- left_join(
                      ucmr5_wide_test,
                      ucmr5_long %>% 
                        mutate(
                          x11cl_pf3ouds_above_mrl = ifelse(contaminant == "11cl-pf3ouds" & analyticalresultssign == "=", 1, 0),
                          x4_2fts_above_mrl = ifelse(contaminant == "4:2 fts" & analyticalresultssign == "=", 1, 0),
                          x6_2fts_above_mrl = ifelse(contaminant == "6:2 fts" & analyticalresultssign == "=", 1, 0),
                          x8_2fts_above_mrl = ifelse(contaminant == "8:2 fts" & analyticalresultssign == "=", 1, 0),
                          x9cl_pf3ons_above_mrl = ifelse(contaminant == "9cl-pf3ons" & analyticalresultssign == "=", 1, 0),
                          adona_above_mrl = ifelse(contaminant == "adona" & analyticalresultssign == "=", 1, 0),
                          hfpo_da_above_mrl = ifelse(contaminant == "hfpo-da" & analyticalresultssign == "=", 1, 0),
                          netfosaa_above_mrl = ifelse(contaminant == "netfosaa" & analyticalresultssign == "=", 1, 0),
                          nfdha_above_mrl = ifelse(contaminant == "nfdha" & analyticalresultssign == "=", 1, 0),
                          nmefosaa_above_mrl = ifelse(contaminant == "nmefosaa" & analyticalresultssign == "=", 1, 0),
                          pfba_above_mrl = ifelse(contaminant == "pfba" & analyticalresultssign == "=", 1, 0),
                          pfbs_above_mrl = ifelse(contaminant == "pfbs" & analyticalresultssign == "=", 1, 0),
                          pfda_above_mrl = ifelse(contaminant == "pfda" & analyticalresultssign == "=", 1, 0),
                          pfdoa_above_mrl = ifelse(contaminant == "pfdoa" & analyticalresultssign == "=", 1, 0),
                          pfeesa_above_mrl = ifelse(contaminant == "pfeesa" & analyticalresultssign == "=", 1, 0),
                          pfhpa_above_mrl = ifelse(contaminant == "pfhpa" & analyticalresultssign == "=", 1, 0),
                          pfhps_above_mrl = ifelse(contaminant == "pfhps" & analyticalresultssign == "=", 1, 0),
                          pfhxa_above_mrl = ifelse(contaminant == "pfhxa" & analyticalresultssign == "=", 1, 0),
                          pfhxs_above_mrl = ifelse(contaminant == "pfhxs" & analyticalresultssign == "=", 1, 0),
                          pfmba_above_mrl = ifelse(contaminant == "pfmba" & analyticalresultssign == "=", 1, 0),
                          pfmpa_above_mrl = ifelse(contaminant == "pfmpa" & analyticalresultssign == "=", 1, 0),
                          pfna_above_mrl = ifelse(contaminant == "pfna" & analyticalresultssign == "=", 1, 0),
                          pfoa_above_mrl = ifelse(contaminant == "pfoa" & analyticalresultssign == "=", 1, 0),
                          pfos_above_mrl = ifelse(contaminant == "pfos" & analyticalresultssign == "=", 1, 0),
                          pfpea_above_mrl = ifelse(contaminant == "pfpea" & analyticalresultssign == "=", 1, 0),
                          pfpes_above_mrl = ifelse(contaminant == "pfpes" & analyticalresultssign == "=", 1, 0),
                          pfta_above_mrl = ifelse(contaminant == "pfta" & analyticalresultssign == "=", 1, 0),
                          pftrda_above_mrl = ifelse(contaminant == "pftrda" & analyticalresultssign == "=", 1, 0),
                          pfuna_above_mrl = ifelse(contaminant == "pfuna" & analyticalresultssign == "=", 1, 0)
                        ) %>% 
                        group_by(pws_id) %>% 
                        summarise(
                          x11cl_pf3ouds_above_mrl = ifelse(sum(x11cl_pf3ouds_above_mrl)>0,1,0),
                          x4_2fts_above_mrl = ifelse(sum(x4_2fts_above_mrl)>0,1,0),
                          x6_2fts_above_mrl = ifelse(sum(x6_2fts_above_mrl)>0,1,0),
                          x8_2fts_above_mrl = ifelse(sum(x8_2fts_above_mrl)>0,1,0),
                          x9cl_pf3ons_above_mrl = ifelse(sum(x9cl_pf3ons_above_mrl)>0,1,0),
                          adona_above_mrl = ifelse(sum(adona_above_mrl)>0,1,0),
                          hfpo_da_above_mrl = ifelse(sum(hfpo_da_above_mrl)>0,1,0),
                          netfosaa_above_mrl = ifelse(sum(netfosaa_above_mrl)>0,1,0),
                          nfdha_above_mrl = ifelse(sum(nfdha_above_mrl)>0,1,0),
                          nmefosaa_above_mrl = ifelse(sum(nmefosaa_above_mrl)>0,1,0),
                          pfba_above_mrl = ifelse(sum(pfba_above_mrl)>0,1,0),
                          pfbs_above_mrl = ifelse(sum(pfbs_above_mrl)>0,1,0),
                          pfda_above_mrl = ifelse(sum(pfda_above_mrl)>0,1,0),
                          pfdoa_above_mrl = ifelse(sum(pfdoa_above_mrl)>0,1,0),
                          pfeesa_above_mrl = ifelse(sum(pfeesa_above_mrl)>0,1,0),
                          pfhpa_above_mrl = ifelse(sum(pfhpa_above_mrl)>0,1,0),
                          pfhps_above_mrl = ifelse(sum(pfhps_above_mrl)>0,1,0),
                          pfhxa_above_mrl = ifelse(sum(pfhxa_above_mrl)>0,1,0),
                          pfhxs_above_mrl = ifelse(sum(pfhxs_above_mrl)>0,1,0),
                          pfmba_above_mrl = ifelse(sum(pfmba_above_mrl)>0,1,0),
                          pfmpa_above_mrl = ifelse(sum(pfmpa_above_mrl)>0,1,0),
                          pfna_above_mrl = ifelse(sum(pfna_above_mrl)>0,1,0),
                          pfoa_above_mrl = ifelse(sum(pfoa_above_mrl)>0,1,0),
                          pfos_above_mrl = ifelse(sum(pfos_above_mrl)>0,1,0),
                          pfpea_above_mrl = ifelse(sum(pfpea_above_mrl)>0,1,0),
                          pfpes_above_mrl = ifelse(sum(pfpes_above_mrl)>0,1,0),
                          pfta_above_mrl = ifelse(sum(pfta_above_mrl)>0,1,0),
                          pftrda_above_mrl = ifelse(sum(pftrda_above_mrl)>0,1,0),
                          pfuna_above_mrl = ifelse(sum(pfuna_above_mrl)>0,1,0)
                        )
                      ,
                      by = "pws_id"
            )

# sum of pfas chemicals above mrl performed by collection date 
# to not add values from the same chemical more than once
ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(analyticalresultssign == "=" & date == 1) %>% 
                      distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
                      group_by(pws_id) %>% 
                      summarise(pfas_sum_values_above_mrl_1 = sum(analyticalresultvalue))
                    ,
                    by = "pws_id"
)

ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(analyticalresultssign == "=" & date == 2) %>% 
                      distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
                      group_by(pws_id) %>% 
                      summarise(pfas_sum_values_above_mrl_2 = sum(analyticalresultvalue))
                    ,
                    by = "pws_id"
                  )

ucmr5_wide_test <- left_join(
                    ucmr5_wide_test,
                    ucmr5_long %>% 
                      filter(analyticalresultssign == "=" & date == 3) %>% 
                      distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
                      group_by(pws_id) %>% 
                      summarise(pfas_sum_values_above_mrl_3 = sum(analyticalresultvalue))
                    ,
                    by = "pws_id"
                  )

ucmr5_wide_test <- ucmr5_wide_test %>% 
                    mutate(
                      pfas_sum_values_above_mrl_highest_detected = pmax(pfas_sum_values_above_mrl_1,pfas_sum_values_above_mrl_2,pfas_sum_values_above_mrl_3, na.rm = TRUE),
                      pfas_sum_values_above_mrl_lowest_detected = pmin(pfas_sum_values_above_mrl_1,pfas_sum_values_above_mrl_2,pfas_sum_values_above_mrl_3, na.rm = TRUE)
                    )

ucmr5_wide_test[is.na(ucmr5_wide_test)] <- 0

# pfas change went from over mrl to below mrl




x11cl_pf3ouds
x4_2fts
x6_2fts
x8_2fts
x9cl_pf3ons
adona
hfpo_da
nfdha
pfba
pfbs
pfda
pfdoa
pfeesa
pfhpa
pfhps
pfhxa
pfhxs
pfmba
pfmpa
pfna
pfoa
pfos
pfpea
pfpes
pfuna

ds <- ucmr5_long[ucmr5_long$pws_id == "1090063",]


# add samples to all ids to make a id file


# add date numbers
ucmr5_long_id_test <- left_join(
  ucmr5_long,
  ucmr5_long %>% 
    arrange(pws_id, collectiondate) %>%
    distinct(pws_id, collectiondate) %>%
    group_by(pws_id) %>% 
    mutate(date = row_number())
  ,
  by = c("pws_id","collectiondate")
)


ucmr5_long_id_test <- ucmr5_long %>% 
  arrange(pws_id, contaminant, collectiondate) %>%
  group_by(pws_id, contaminant) %>%
  mutate(date = row_number()) %>%
  ungroup()

view(ucmr5_long_id_test[ucmr5_long_id_test$pws_id == "1090026",])

view(ucmr5_long_id_test[ucmr5_long_id_test$pws_id == "3060029",])


ucmr5_long_id1 <- ucmr5_long %>% 
                    distinct(pws_id, contaminant, collectiondate) %>% 
                    arrange(pws_id, collectiondate) %>%
                    group_by(pws_id) %>%
                    mutate(date = row_number()) %>%
                    ungroup()

ucmr5_long_id2 <- ucmr5_long %>% 
                    distinct(pws_id, contaminant, collectiondate, sampleid) %>% 
                    arrange(pws_id, contaminant, collectiondate, sampleid) %>%
                    group_by(pws_id, contaminant, collectiondate) %>%
                    mutate(site = row_number()) %>%
                    ungroup()

ucmr5_long_test <- left_join(ucmr5_long, 
                             dplyr::select(
                               ucmr5_long_id1,
                               pws_id,
                               contaminant,
                               collectiondate,
                               date
                               ), by = c("pws_id","contaminant","collectiondate")
                    )

view(ucmr5_long_test[ucmr5_long_test$pws_id == "1090026",])

view(ucmr5_long_test[ucmr5_long_test$pws_id == "3060029",])


ucmr5_wide1 <- ucmr5_long %>%
  pivot_wider(
    id_cols = c(pws_id, collectiondate,sampleid),
    names_from = c(contaminant,sampleid,facilitywatertype),
    names_glue = "{contaminant}_{sampleid}_{facilitywatertype}_mrl",
    names_vary = "fastest",
    values_from = mrl
  ) %>% 
  clean_names()


view(ucmr5_long_id2[ucmr5_long_id2$pws_id == "3060029",])

view(ucmr5_wide1[ucmr5_wide1$pws_id == "1150108",])

facility_water_type
glimpse(ucmr5_long)
  
ucmr5_long %>%
  dplyr::group_by(PWS_ID, CollectionDate, Contaminant,  SampleID, FacilityWaterType) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)


                      c("PWS_ID","CollectionDate","Contaminant","SamplePointID","FacilityWaterType","MRL", "AnalyticalResultsSign", "AnalyticalResultValue")])





# permutations and hierarchy
# pwsid
## collection date
### contaminent
#### sample site + water type


#to do multiple columns, have to put setDT around data name

# ucmr5_wide1 <- dcast(
#                 data = ucmr5_long,
#                 formula = pws_id + collection_date + sample_id + facility_water_type ~ contaminant,
#                 value.var = "mrl",
#                 fun.aggregation = NULL
#                 )


# library(tidyr)




# calculate temporal descriptives from long format
# create agg data set from long format, any pfas, any pfas over, by GW/SW, by contaminant..








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








= ifelse(contaminant == "" & analyticalresultssign == "=", 1, 0),