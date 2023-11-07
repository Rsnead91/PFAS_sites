# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','units','sf','ggmap','reshape2','janitor','ggpattern','tidygeocoder','lubridate')

options(scipen = 999)

# 2. load data ----

## 2.1 study area counties ----

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
                    filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")) %>% 
                    st_set_crs(st_crs(4269))
# saving shp
st_write(study_counties, dsn = "00_data/study_counties.shp")

## 2.2 known pfas ----

### 2.2.1 superfund sites ----

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
                                    dplyr::select(-c(address,links_to_more_info,soil_sampling,soil_sampling_pfas,soil_sampling_notes,general_site_notes,possibly_responsible_parties))

superfund_merge <- merge(x = superfund %>% 
                              dplyr::select(PRIMARY_NAME) %>% 
                              rename(name = PRIMARY_NAME)
                         , 
                         y = PFAS_detected_superfund_others, 
                         by = "name")

# saving shp
st_write(superfund_merge, dsn = "00_data/known_pfas_superfund_sites.shp")


### 2.2.2 hsca sites ----

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

# saving shp
st_write(non_superfund, dsn = "00_data/known_pfas_hsca_sites.shp")


### 2.2.3 neshaminy creek watershed ----

# data pulled from here: https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/WBD/National/GDB/

# importing watershed geodatabase

## Define the path to the geodatabase (.gdb) file
gdb_path <- "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/00_data/WBD_National_GDB.gdb"

## List available layers in the geodatabase
layers <- st_layers(gdb_path)

## Read the selected layer
data <- st_read(dsn = gdb_path, layer = layers[["name"]][1]) #WBDHU12

# reducing data to only neshaminy creek watershed and projecting
neshaminy <- data %>%
  mutate(name = toupper(name)) %>%         
  filter(str_detect(name, "NESHAMINY")) %>% 
  dplyr::select(states, name) %>% 
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

rm(data)

# joining smaller sub-watersheds
neshaminy_union <- neshaminy %>%  
  group_by(states) %>% 
  summarise()

neshaminy_creek2 <- data.frame(
                      name = "Neshaminy Creek Watershed",
                      county = "BUCKS + MONTGOMERY",
                      site = "State Advisory",
                      type = "Watershed",
                      groundwater = "Unk",
                      surface_water = "Yes",
                      electronic_manufacturing = "No",
                      active = "Yes"
                    )

neshaminy_creek <- cbind(neshaminy_union,neshaminy_creek2) %>%
                    dplyr::select(name,county,site,type,groundwater,surface_water,electronic_manufacturing,active)

# saving shp
st_write(neshaminy_creek, dsn = "00_data/known_pfas_neshaminy_creek_watershed.shp")


### 2.2.4 ridge run - east and west rockhill townships, bucks county ----

ridge_run <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennDOT/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(FIPS_COUNT == "017" & MUNICIPAL1 %in% c("EAST ROCKHILL","WEST ROCKHILL","PERKASIE")) # only keeping estimated area of landfill

# repairing simple geometry issues in municipality and pws shapefiles
ridge_run <- st_make_valid(ridge_run)

# dissolving the two municipalities into a single shape and findinf the centroid
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

# saving shp
st_write(ridge_run, dsn = "00_data/known_pfas_ridge_run.shp")


### 2.2.5 facilities with pfas discharge ----

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

# saving shp
st_write(facilities_discharge_pfas, dsn = "00_data/known_pfas_discharge.shp")


## 2.3 suspected pfas ----

### 2.3.1 airports ----

airports_public_raw <- st_as_sf(
                        read_excel("00_data/pa_public_airports.xlsx"),
                        coords = c("X","Y")
                        ) %>% 
                          st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
                          st_transform(crs = 4269) # set projection to match other geo files

# spatial join

airports <- st_intersection(
  x = airports_public_raw,
  y = study_counties
)

# saving shp
st_write(airports, dsn = "00_data/susp_pfas_airports.shp")


### 2.3.2 firefighter training sites ----

Fire_Training_Facilities <- st_as_sf(
                              read_csv("00_data/Fire Training Facilities.csv"),
                              coords = c("Long","Lat")
                            ) %>% 
                              st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
                              st_transform(crs = 4269) # set projection to match other geo files

# saving shp
st_write(Fire_Training_Facilities, dsn = "00_data/susp_pfas_firefighter_training.shp")


### 2.3.3 landfill sites ----

# municipality

landfill_municipality <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP/MapServer/20", sf_type = "esriGeometryPoint")%>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

# repairing simple geometry issues in municipality and pws shapefiles
landfill_municipality <- st_make_valid(landfill_municipality)

# residual

landfill_residual <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP/MapServer/26", sf_type = "esriGeometryPoint") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

# repairing simple geometry issues in municipality and pws shapefiles
landfill_residual <- st_make_valid(landfill_residual)

# combining landfill data and reducing to study area

landfill <- rbind(landfill_municipality,landfill_residual)

# spatial join

landfill_studyarea <- st_intersection(
  x = landfill,
  y = study_counties
)

# saving shp
st_write(landfill_studyarea, dsn = "00_data/susp_pfas_landfills.shp")


### 2.3.4 EPA's envirofacts facilities registry: potential sources of PFAS ----

manufacturing <- st_as_sf(
                  filter(read_excel("00_data/envirofacts_studyarea.xlsx"), !is.na(LONGITUDE83)),
                  coords = c("LONGITUDE83","LATITUDE83")
                ) %>% 
                  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
                  st_transform(crs = 4269) # set projection to match other geo files

manufacturing <- st_intersection(
  x = manufacturing,
  y = study_counties
)

# saving shp
st_write(manufacturing, dsn = "00_data/susp_pfas_manufacturing.shp")


## 2.4 possible PWS wells/intakes ----

wells <- st_as_sf(
  read_csv("00_data/Wells.csv"),
  coords = c("Long","Lat")
) %>% 
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

wells <- st_intersection(
  x = wells,
  y = study_counties
)

# saving shp
st_write(wells, dsn = "00_data/wells.shp")


intakes <- st_as_sf(
  read_csv("00_data/Intakes.csv"),
  coords = c("Long","Lat")
) %>% 
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

intakes <- st_intersection(
  x = intakes,
  y = study_counties
)

# saving shp
st_write(intakes, dsn = "00_data/intakes.shp")


## 2.5 public water supply areas ----

pws <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP2/MapServer/8", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(CNTY_NAME %in% c("Berks", "Bucks", "Carbon", "Chester", "Delaware", "Lancaster", "Lebanon", "Lehigh", "Monroe", "Montgomery", "Northampton", "Schuylkill")) # only keeping 12 study area counties

#nrow(pws.shp): 581

## repairing simple geometry issues in pws shapefiles
pws <- st_make_valid(pws)

# saving shp
st_write(pws, dsn = "00_data/pws.shp")


## 2.6 pfas sampling data ----

### 2.6.1 pa dep ----

# this step was performed with the support of this blog post: https://medium.com/swlh/the-adventure-of-pdf-to-data-frame-in-r-f90609035600

### 2.6.1.1 2019 ----

## url to pa department of environmental protections sampling data of the PDF file
pdf_url <- "https://files.dep.state.pa.us/Water/DrinkingWater/Perfluorinated%20Chemicals/SamplingResults/PFASPhase1ResultsSummary.pdf"

## extract text from the PDF
pdf_text <- pdf_text(pdf_url) %>%
  str_split("\n") # splitting text into rows

## get rid of first 1 to 7 lines
for(i in 1:3) { 
  pdf_text[[i]] <- pdf_text[[i]][-1:-7]
}

## removing unnecessary lines at the end of the page
pdf_text[[1]]  <- pdf_text[[1]][-42]
pdf_text[[2]]  <- pdf_text[[2]][-42]
pdf_text[[3]]  <- pdf_text[[3]][-15:-27]

## removing excess spaces between substrings and splitting rows by common string patterns
pdf_text <- pdf_text %>%
  str_squish() %>%
  strsplit(split= "\\,\\s\\\"")

## removing common substring at the start of each page
for(i in 1:length(pdf_text)) {
  pdf_text[[i]][1] <- pdf_text[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}

## removing common substring at the end of each line
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text[[i]])) {
    pdf_text[[i]][j] <- pdf_text[[i]][j] %>%
      stringr::str_extract(".*(?=\")")
  }
}

## separating characters
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text)){
    pdf_text[[i]][j] %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% 
      print() #extracts the words
  }
}

## put into a single column of a data frame
names_ex = list()
for(i in 1:length(pdf_text)) {
  words <- pdf_text[[i]] #%>% str_extract(".*[:alpha:]+|\\&|\\-") 
  words_df <- data.frame(words) #turns into data frame for list
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex) %>% drop_na() # removing black row at the end of each page
}

for (i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][4]
}

left_df <- data.frame(left = str_sub(NH_names$words, end = unlist(NH_names$loc)))

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][nrow(str_locate_all(NH_names$words, " ")[[i]])-7]
}

right_df <- data.frame(right = str_sub(NH_names$words, start = unlist(NH_names$loc)))

ds <- cbind(left_df,right_df)

dep_sampling_2019_wide <- dplyr::select(cbind(tidyr::separate(ds, col = left, into = c("drop00","drop01","pwsid","drop02","drop03"), sep=" "), 
                                         tidyr::separate(ds, col = right, into = c("drop04","date","pfas01","pfas02","pfas03","pfas04","pfas05","pfas06","drop05"), sep=" ")),
                                   -left, -right, -starts_with("drop")
) %>%
  mutate_all(~ replace(., . == "ND", "0"))

colnames(dep_sampling_2019_wide) <- c("pwsid","collectiondate","pfbs","pfhpa","pfhxs","pfna","pfos","pfoa")

dep_sampling_2019 <- dep_sampling_2019_wide %>% 
                        pivot_longer(
                          cols = c("pfbs","pfhpa","pfhxs","pfna","pfos","pfoa"),
                          names_to = "contaminant",
                          values_to = "analyticalresultvalue"
                        )

rm(ds,left,right,NH_names,pdf_text,pdf_url,names_ex,words_df)

### 2.6.1.2 2020 ----

## url to pa department of environmental protections sampling data of the PDF file
pdf_url <- "https://files.dep.state.pa.us/Water/DrinkingWater/Perfluorinated%20Chemicals/SamplingResults/SUMMARY_OF_RESULTS_PFAS_PHASE_1_2020.pdf"

## extract text from the PDF
pdf_text <- pdf_text(pdf_url) %>%
  str_split("\n") # splitting text into rows

## get rid of first 1 to 7 lines
for(i in 1:2) { 
  pdf_text[[i]] <- pdf_text[[i]][-1:-7]
}

## removing unnecessary lines at the end of the page
pdf_text[[1]]  <- pdf_text[[1]][-86]
pdf_text[[2]]  <- pdf_text[[2]][-30:-41]

## removing excess spaces between substrings and splitting rows by common string patterns
pdf_text <- pdf_text %>%
  str_squish() %>%
  strsplit(split= "\\,\\s\\\"")

## removing common substring at the start of each page
for(i in 1:length(pdf_text)) {
  pdf_text[[i]][1] <- pdf_text[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}

## removing common substring at the end of each line
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text[[i]])) {
    pdf_text[[i]][j] <- pdf_text[[i]][j] %>%
      stringr::str_extract(".*(?=\")")
  }
}

## separating characters
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text)){
    pdf_text[[i]][j] %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% 
      print() #extracts the words
  }
}

## put into a single column of a data frame
names_ex = list()
for(i in 1:length(pdf_text)) {
  words <- pdf_text[[i]] %>% str_extract(".*[:alpha:]+|\\&|\\-") 
  words_df <- data.frame(words) #turns into data frame for list
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex) %>% drop_na() # removing black row at the end of each page
}
# print(NH_names)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][nrow(str_locate_all(NH_names$words, " ")[[i]])-20]
}

right <- str_sub(NH_names$words, start = unlist(NH_names$loc))
ds <- cbind(NH_names,right)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][5]
}

left <- str_sub(NH_names$words, end = unlist(NH_names$loc))
ds <- cbind(ds,left)

dep_sampling_2020_wide <- dplyr::select(cbind(tidyr::separate(ds, col = left, into = c("drop00","drop01","pwsid","drop02","drop03"), sep=" "), 
                                         tidyr::separate(ds, col = right, into = c("drop04","date","pfas01","pfas02","pfas03","pfas04","pfas05","pfas06","pfas07","pfas08","pfas09","pfas10","pfas11","pfas12","pfas13","pfas14","pfas15","pfas16","pfas17","pfas18","drop05","drop06"), sep=" ")),
                                   -words, -loc, -left, -right, -starts_with("drop")
                      ) %>%
                        mutate_all(~ replace(., . == "ND", "0"))

colnames(dep_sampling_2020_wide) <- c("pwsid","collectiondate","x11cl_pf3ouds","x9cl_pf3ons","adona","hfpo_da","netfosaa","nmefosaa","pfbs","pfda","pfdoa","pfhpa","pfhxs","pfhxa","pfna","pfos","pfoa","pfta","pftrda","pfuna")

dep_sampling_2020 <- dep_sampling_2020_wide %>% 
  pivot_longer(
    cols = c("x11cl_pf3ouds","x9cl_pf3ons","adona","hfpo_da","netfosaa","nmefosaa","pfbs","pfda","pfdoa","pfhpa","pfhxs","pfhxa","pfna","pfos","pfoa","pfta","pftrda","pfuna"),
    names_to = "contaminant",
    values_to = "analyticalresultvalue"
  ) %>% 
  mutate(
    contaminant = case_when(
      contaminant == "x11cl_pf3ouds" ~ "11cl-pf3ouds",
      contaminant == "x9cl_pf3ons" ~ "9cl-pf3ons",
      contaminant == "hfpo_da" ~ "hfpo-da",
      TRUE ~ contaminant
    )
  )

rm(ds,left,right,NH_names,pdf_text,pdf_url,names_ex,words_df)

### 2.6.1.3 2021 ----

## url to pa department of environmental protections sampling data of the PDF file
pdf_url <- "https://files.dep.state.pa.us/Water/DrinkingWater/Perfluorinated%20Chemicals/SamplingResults/PFAS_Sampling_Final_Results_May_2021.pdf"

## extract text from the PDF
pdf_text <- pdf_text(pdf_url) %>%
  str_split("\n") # splitting text into rows

## get rid of first 1 to 11 lines
for(i in 1:6) { 
  pdf_text[[i]] <- pdf_text[[i]][-1:-11]
}

## getting rid of definitions at the end of the last page
pdf_text[[6]]  <- pdf_text[[6]][-23:-34]

## removing excess spaces between substrings and splitting rows by common string patterns
pdf_text <- pdf_text %>%
  str_squish() %>%
  strsplit(split= "\\,\\s\\\"")

## removing common substring at the start of each page
for(i in 1:length(pdf_text)) {
  pdf_text[[i]][1] <- pdf_text[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}

## removing common substring at the end of each line
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text[[i]])) {
    pdf_text[[i]][j] <- pdf_text[[i]][j] %>%
      stringr::str_extract(".*(?=\")")
  }
}

## separating characters
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text)){
    pdf_text[[i]][j] %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% 
      print() #extracts the words
  }
}

## put into a single column of a data frame
names_ex = list()
for(i in 1:length(pdf_text)) {
  words <- pdf_text[[i]] %>% str_extract(".*[:alpha:]+|\\&|\\-") 
  words_df <- data.frame(words) #turns into data frame for list
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex) %>% drop_na() # removing black row at the end of each page
}
# print(NH_names)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][nrow(str_locate_all(NH_names$words, " ")[[i]])-20]
}

right <- str_sub(NH_names$words, start = unlist(NH_names$loc))
ds <- cbind(NH_names,right)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][5]
}

left <- str_sub(NH_names$words, end = unlist(NH_names$loc))
ds <- cbind(ds,left)

dep_sampling_2021_wide <- dplyr::select(cbind(tidyr::separate(ds, col = left, into = c("drop00","drop01","pwsid","drop02","drop03"), sep=" "), 
                               tidyr::separate(ds, col = right, into = c("drop04","date","pfas01","pfas02","pfas03","pfas04","pfas05","pfas06","pfas07","pfas08","pfas09","pfas10","pfas11","pfas12","pfas13","pfas14","pfas15","pfas16","pfas17","pfas18","drop05","drop06"), sep=" ")),
                         -words, -loc, -left, -right, -starts_with("drop")
                    ) %>%
                      mutate_all(~ replace(., . == "ND", "0"))

colnames(dep_sampling_2021_wide) <- c("pwsid","collectiondate","x11cl_pf3ouds","x9cl_pf3ons","adona","hfpo_da","netfosaa","nmefosaa","pfda","pfdoa","pfta","pftrda","pfhxa","pfuna","pfbs","pfhpa","pfhxs","pfna","pfos","pfoa")

dep_sampling_2021 <- dep_sampling_2021_wide %>% 
  pivot_longer(
    cols = c("x11cl_pf3ouds","x9cl_pf3ons","adona","hfpo_da","netfosaa","nmefosaa","pfbs","pfda","pfdoa","pfhpa","pfhxs","pfhxa","pfna","pfos","pfoa","pfta","pftrda","pfuna"),
    names_to = "contaminant",
    values_to = "analyticalresultvalue"
  ) %>% 
  mutate(
    contaminant = case_when(
      contaminant == "x11cl_pf3ouds" ~ "11cl-pf3ouds",
      contaminant == "x9cl_pf3ons" ~ "9cl-pf3ons",
      contaminant == "hfpo_da" ~ "hfpo-da",
      TRUE ~ contaminant
    )
  )

rm(ds,left,right,NH_names,pdf_text,pdf_url,names_ex,words_df)

# combine dep sampling years
dep_sampling_2019_2020_2021 <- rbind(dep_sampling_2019, dep_sampling_2020, dep_sampling_2021) %>% distinct()

# save dep long file
# save(dep_sampling_2019_2020_2021, file = "00_data/dep_sampling_2019_2020_2021.Rdata")


### 2.6.2 ucmr 3 ----

ucmr3 <- read_tsv("00_data/UCMR3_ALL_MA_WY.txt") %>% 
  filter(State == "PA" & Contaminant %in% c("PFHpA", "PFHxS", "PFNA", "PFOA", "PFOS", "PFBS")) %>% 
  dplyr::select(PWSID, CollectionDate, Contaminant, SampleID, SamplePointName, FacilityWaterType, MRL, AnalyticalResultsSign, AnalyticalResultValue) %>% 
  mutate(
    PWSID = substr(PWSID,3,10),
    Contaminant = tolower(Contaminant)
  )

colnames(ucmr3) <- tolower(colnames(ucmr3))


### 2.6.3 ucmr 5 ----

# the following chems are in dep sampling but not ucmr 5: netfosaa, nmefosaa, pfta, pftrda
# the following chems are in ucmr 5 but not dep sampling: pfpea, pfpes, pfmba, pfmpa, pfhps, pfeesa, pfba, nfdha, x4_2_fts, x6_2_fts, x8_2_fts

ucmr5 <- read_tsv("00_data/UCMR5_ALL_MA_WY.txt") %>% 
          filter(State == "PA" & Contaminant != "lithium") %>% 
          dplyr::select(PWSID, CollectionDate, Contaminant, SampleID, SamplePointName, FacilityWaterType, MRL, AnalyticalResultsSign, AnalyticalResultValue) %>% 
          mutate(
            PWSID = substr(PWSID,3,10),
            Contaminant = tolower(Contaminant)
          )

colnames(ucmr5) <- tolower(colnames(ucmr5))


### 2.6.4 merging all sampling data  ----

## dep
reduce_to_study_pwsid <- pws.shp %>% 
                                dplyr::select(PWS_ID) %>% 
                                st_drop_geometry()

colnames(reduce_to_study_pwsid) <- tolower(colnames(reduce_to_study_pwsid))

dep <- left_join(x=reduce_to_study_pwsid, y=dep_sampling_2019_2020_2021, by = c("pws_id" = "pwsid")) %>% 
  drop_na(collectiondate) %>% 
  mutate(data_source = "DEP")

## ucmr
ucmr <- left_join(x=reduce_to_study_pwsid, y=rbind(ucmr3,ucmr5), by = c("pws_id" = "pwsid")) %>% 
  drop_na(collectiondate) %>% 
  mutate(data_source = "UCMR") %>% 
  clean_names()

# pa_pfas_sampling_ucmr3dep <- rbind(
#   cbind(
#     dep,
#     data.frame(sampleid = NA, samplepointname = NA, facilitywatertype = NA, mrl = NA, analyticalresultssign = NA)
#   ),
#   ucmr
# )

## all pa pfas sampling
pa_pfas_sampling <- rbind(
                      cbind(
                        dep,
                        data.frame(sampleid = NA, samplepointname = NA, facilitywatertype = NA, mrl = NA, analyticalresultssign = NA)
                      ),
                      ucmr
                      )

pa_pfas_sampling$collectiondate <- mdy(pa_pfas_sampling$collectiondate)

# save long file
# save(pa_pfas_sampling, file = "00_data/pa_pfas_sampling.Rdata")


## 2.7 unused data ----

### electronics manufacturing in pa registered with federal govt NAICS: 334000

# electric <- read_csv("00_data/EntityPAElectronicsManufacturing.csv") %>% 
#               clean_names() %>% 
#               mutate(address = str_c(address_line_1,", ",city,", ",state_province," ",zip_code)) %>% 
#               tidygeocoder::geocode(address = address, method = 'osm', lat = latitude , long = longitude)
# 
# electric_na <- electric %>% 
#                 filter(is.na(latitude)) %>% 
#                 dplyr::select(-c(latitude,longitude))
# 
# electric_na_retry <- electric_na %>% 
#                       tidygeocoder::geocode(street = address_line_1, city = city, state = state_province, postalcode = zip_code, method = 'census', lat = latitude , long = longitude)
# 
# electric_nona <- rbind(
#                   electric %>% 
#                    filter(!is.na(latitude)),
#                   electric_na_retry %>% 
#                    filter(!is.na(latitude))
#                 )
# 
# electric_geo1 <- st_as_sf(
#                   electric_nona,
#                   coords = c("longitude","latitude")
#                 ) %>% 
#                   st_set_crs(st_crs(4269))
# 
# electric_geo_study <- st_intersection(study_counties, electric_geo1)

### plastics manufacturing in pa registered with federal govt NAICS: 326100

# plastics <- read_csv("00_data/EntityPAPlasticsManufacturing.csv") %>% 
#   clean_names() %>% 
#   mutate(address = str_c(address_line_1,", ",city,", ",state_province," ",zip_code)) %>% 
#   tidygeocoder::geocode(address = address, method = 'osm', lat = latitude , long = longitude)
# 
# plastics_na <- plastics %>% 
#   filter(is.na(latitude)) %>% 
#   dplyr::select(-c(latitude,longitude))
# 
# plastics_na_retry <- plastics_na %>% 
#   tidygeocoder::geocode(street = address_line_1, city = city, state = state_province, postalcode = zip_code, method = 'census', lat = latitude , long = longitude)
# 
# plastics_nona <- rbind(
#   plastics %>% 
#     filter(!is.na(latitude)),
#   plastics_na_retry %>% 
#     filter(!is.na(latitude))
# )
# 
# plastics_geo1 <- st_as_sf(
#   plastics_nona,
#   coords = c("longitude","latitude")
# ) %>% 
#   st_set_crs(st_crs(4269))
# 
# plastics_geo_study <- st_intersection(study_counties, plastics_geo1)
# 
# ggplot() +
#   geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
#   geom_sf(data = plastics_geo_study)


# 3. calculating aggregate variables and wide ds ----

# hierarchy:
## pwsid
### collection date
#### contaminent
##### sample site

# add number for collection date
pa_pfas_sampling <- left_join(
                      pa_pfas_sampling,
                      pa_pfas_sampling %>% 
                        arrange(pws_id, collectiondate) %>%
                        distinct(pws_id, collectiondate) %>%
                        group_by(pws_id) %>% 
                        mutate(date = row_number())
                      ,
                      by = c("pws_id","collectiondate")
                    )

# number of samples tested
pa_pfas_sampling_wide <- pa_pfas_sampling %>% 
                          group_by(pws_id) %>% 
                          mutate(num_samples = n()) %>% 
                          ungroup() %>% 
                          distinct(pws_id, num_samples)

# number of sample locations
pa_pfas_sampling_wide <- left_join(
                            pa_pfas_sampling_wide,
                            pa_pfas_sampling %>%
                            group_by(pws_id) %>%
                            summarize(num_sample_locations = n_distinct(unlist(strsplit(paste(samplepointname, collapse = " "), " "))))
                            ,
                            by = "pws_id"
                        )

# number of collection dates at each pws
pa_pfas_sampling_wide <- left_join(
                            pa_pfas_sampling_wide,
                            pa_pfas_sampling %>% 
                              distinct(pws_id, collectiondate) %>% 
                              group_by(pws_id) %>% 
                              summarise(num_dates = n())
                            ,
                            by = "pws_id"
                        )

# number of samples by date
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(date == 1) %>% 
#                             group_by(pws_id) %>%
#                             mutate(num_samples_date1 = n()) %>% 
#                             distinct(pws_id, num_samples_date1)
#                           ,
#                           by = "pws_id"
#                       )

# number of tests in groundwater and surface water and number of positive pfas in each
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             distinct(pws_id, collectiondate, facilitywatertype) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(num_gw = length(facilitywatertype == "GW"),
#                                       num_sw = length(facilitywatertype == "SW")
#                                       )
#                           ,
#                           by = "pws_id"
#                       )

# how many pfas chemicals tested
pa_pfas_sampling_wide <- left_join(
                            pa_pfas_sampling_wide,
                            pa_pfas_sampling %>% 
                            distinct(pws_id, contaminant) %>% 
                            group_by(pws_id) %>% 
                            summarise(num_contaminants_tested = n())
                          ,
                          by = "pws_id"
                        )

# how many chemicals detected above mrl
pa_pfas_sampling_wide <- left_join(
                            pa_pfas_sampling_wide,
                            pa_pfas_sampling %>% 
                            filter(analyticalresultssign == "=") %>% 
                            distinct(pws_id, contaminant) %>% 
                            group_by(pws_id) %>% 
                            summarise(num_contaminants_above_mrl = n())
                          ,
                          by = "pws_id"
      )

pa_pfas_sampling_wide["num_contaminants_above_mrl"][is.na(pa_pfas_sampling_wide["num_contaminants_above_mrl"])] <- 0

# any pfas detected
pa_pfas_sampling_wide$any_contaminants_above_mrl <- ifelse(pa_pfas_sampling_wide$num_contaminants_above_mrl > 0, 1, 0)

# how many chemicals detected above mrl from groundwater samples
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(facilitywatertype == "GW" & analyticalresultssign == "=") %>% 
#                             distinct(pws_id, contaminant) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(num_gw_pfas_above_mrl = n())
#                           ,
#                           by = "pws_id"
#       )
# 
# pa_pfas_sampling["num_gw_pfas_above_mrl"][is.na(pa_pfas_sampling["num_gw_pfas_above_mrl"])] <- 0

# how many chemicals detected above mrl from surface water samples
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(facilitywatertype == "SW" & analyticalresultssign == "=") %>% 
#                             distinct(pws_id, contaminant) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(num_sw_pfas_above_mrl = n())
#                           ,
#                           by = "pws_id"
#       )
# 
# pa_pfas_sampling["num_sw_pfas_above_mrl"][is.na(pa_pfas_sampling["num_sw_pfas_above_mrl"])] <- 0

# converting DEP units to match UCMR
#UCMR values and MRL are in micrograms/liter while PA DEP is in nanograms/liter

pa_pfas_sampling$analyticalresultvalue <- ifelse(
                                            pa_pfas_sampling$data_source == "DEP",
                                            as.numeric(pa_pfas_sampling$analyticalresultvalue)/1000,
                                            as.numeric(pa_pfas_sampling$analyticalresultvalue)
                                            )
  
# summary pfas detected variable

# !!!! BASED ON MRL FROM UCMR5!!!

pa_pfas_sampling <- pa_pfas_sampling %>% 
                      mutate(
                        mrl = case_when( #MRLs: https://www.epa.gov/dwucmr/fifth-unregulated-contaminant-monitoring-rule#scope
                          contaminant == 'pfoa' ~ 0.004,
                          contaminant == 'pfos' ~ 0.004,
                          contaminant == 'pfba' ~ 0.005,
                          contaminant == '6:2 fts' ~ 0.005,
                          contaminant == '9cl-pf3ons' ~ 0.002,
                          contaminant == '11cl-pf3ouds' ~ 0.005,
                          contaminant == 'pfna' ~ 0.004,
                          contaminant == 'pfbs' ~ 0.003,
                          contaminant == 'pfhxs' ~ 0.003,
                          contaminant == 'pfhxa' ~ 0.003,
                          contaminant == '8:2 fts' ~ 0.005,
                          contaminant == 'pfmpa' ~ 0.004,
                          contaminant == 'pfpea' ~ 0.003,
                          contaminant == 'hfpo-da' ~ 0.005,
                          contaminant == '4:2 fts' ~ 0.003,
                          contaminant == 'pfhps' ~ 0.003,
                          contaminant == 'pfda' ~ 0.003,
                          contaminant == 'pfuna' ~ 0.002,
                          contaminant == 'pfpes' ~ 0.004,
                          contaminant == 'adona' ~ 0.003,
                          contaminant == 'pfdoa' ~ 0.003,
                          contaminant == 'pfeesa' ~ 0.003,
                          contaminant == 'nfdha' ~ 0.020,
                          contaminant == 'pfhpa' ~ 0.003,
                          contaminant == 'pfmba' ~ 0.003,
                          contaminant == 'nmefosaa' ~ 0.006,
                          contaminant == 'pfta' ~ 0.008,
                          contaminant == 'pftrda' ~ 0.007,
                          contaminant == 'netfosaa' ~ 0.005
                        )
                      )

pa_pfas_sampling$analyticalresultssign <- ifelse(
                                              pa_pfas_sampling$data_source == "DEP",
                                              case_when(
                                                pa_pfas_sampling$analyticalresultvalue >= pa_pfas_sampling$mrl ~ "=",
                                                pa_pfas_sampling$analyticalresultvalue < pa_pfas_sampling$mrl ~ "<"
                                                ),
                                              pa_pfas_sampling$analyticalresultssign
                                          )

pa_pfas_sampling <- pa_pfas_sampling %>% 
                      mutate(
                        pfas_detected_source = case_when(
                          (data_source == "DEP" & (analyticalresultvalue == 0)) ~ "DEP: Not detected",
                          (data_source == "DEP" & (analyticalresultvalue > 0)) ~ "DEP: Detected",
                          (data_source == "UCMR" & (analyticalresultssign == "<")) ~ "UCMR: Detected below MRL",
                          (data_source == "UCMR" & (analyticalresultssign == "=")) ~ "UCMR: Detected at or above MRL"
                        ),
                        pfas_detected = case_when(
                          (data_source == "DEP" & (analyticalresultvalue == 0)) ~ "Not detected",
                          (data_source == "UCMR" & (analyticalresultssign == "<")) ~ "Detected at 0 or less than MRL",
                          (data_source == "DEP" & ((analyticalresultvalue > 0) & (analyticalresultvalue < mrl))) ~ "Detected above 0 but below MRL",
                          (data_source == "UCMR" & (analyticalresultssign == "=")) | (data_source == "DEP" & (analyticalresultvalue > mrl)) ~ "Detected at or above MRL"
                        )
                      )

pa_pfas_sampling_wide <- left_join(
  pa_pfas_sampling_wide,
  pa_pfas_sampling %>% 
    group_by(pws_id) %>% 
    mutate(pfas_detected_above_mrl = as.integer("Detected at or above MRL" %in% pfas_detected),
           pfas_detected_above_0 = as.integer("Detected above 0 but below MRL" %in% pfas_detected),
           pfas_detected_below_mrl = as.integer("Detected at 0 or less than MRL" %in% pfas_detected),
           pfas_not_detected = as.integer("Not detected" %in% pfas_detected),
           pfas_detected_4cat = 
             ifelse(
               pfas_detected_above_mrl == 1,
               "PFAS detected at or above MRL",
               ifelse(
                 pfas_detected_above_0 == 1,
                 "PFAS detected above 0 but below MRL",
                 ifelse(
                   pfas_detected_below_mrl == 1,
                   "PFAS detected at 0 or less than MRL",
                   "PFAS not detected"
                 )
               )
             )
    ) %>% 
    distinct(pws_id, pfas_detected_4cat),
  by = "pws_id"
)

# table(pa_pfas_sampling_wide$pfas_detected_4cat)

# which pfas chemicals were measured at all?
pa_pfas_sampling_wide <- left_join(
                          pa_pfas_sampling_wide,
                          pa_pfas_sampling %>% 
                            mutate(
                              x11cl_pf3ouds_any_value = ifelse(contaminant == "11cl-pf3ouds" & !is.na(analyticalresultvalue), 1, 0),
                              x4_2fts_any_value = ifelse(contaminant == "4:2 fts" & !is.na(analyticalresultvalue), 1, 0),
                              x6_2fts_any_value = ifelse(contaminant == "6:2 fts" & !is.na(analyticalresultvalue), 1, 0),
                              x8_2fts_any_value = ifelse(contaminant == "8:2 fts" & !is.na(analyticalresultvalue), 1, 0),
                              x9cl_pf3ons_any_value = ifelse(contaminant == "9cl-pf3ons" & !is.na(analyticalresultvalue), 1, 0),
                              adona_any_value = ifelse(contaminant == "adona" & !is.na(analyticalresultvalue), 1, 0),
                              hfpo_da_any_value = ifelse(contaminant == "hfpo-da" & !is.na(analyticalresultvalue), 1, 0),
                              netfosaa_any_value = ifelse(contaminant == "netfosaa" & !is.na(analyticalresultvalue), 1, 0),
                              nfdha_any_value = ifelse(contaminant == "nfdha" & !is.na(analyticalresultvalue), 1, 0),
                              nmefosaa_any_value = ifelse(contaminant == "nmefosaa" & !is.na(analyticalresultvalue), 1, 0),
                              pfba_any_value = ifelse(contaminant == "pfba" & !is.na(analyticalresultvalue), 1, 0),
                              pfbs_any_value = ifelse(contaminant == "pfbs" & !is.na(analyticalresultvalue), 1, 0),
                              pfda_any_value = ifelse(contaminant == "pfda" & !is.na(analyticalresultvalue), 1, 0),
                              pfdoa_any_value = ifelse(contaminant == "pfdoa" & !is.na(analyticalresultvalue), 1, 0),
                              pfeesa_any_value = ifelse(contaminant == "pfeesa" & !is.na(analyticalresultvalue), 1, 0),
                              pfhpa_any_value = ifelse(contaminant == "pfhpa" & !is.na(analyticalresultvalue), 1, 0),
                              pfhps_any_value = ifelse(contaminant == "pfhps" & !is.na(analyticalresultvalue), 1, 0),
                              pfhxa_any_value = ifelse(contaminant == "pfhxa" & !is.na(analyticalresultvalue), 1, 0),
                              pfhxs_any_value = ifelse(contaminant == "pfhxs" & !is.na(analyticalresultvalue), 1, 0),
                              pfmba_any_value = ifelse(contaminant == "pfmba" & !is.na(analyticalresultvalue), 1, 0),
                              pfmpa_any_value = ifelse(contaminant == "pfmpa" & !is.na(analyticalresultvalue), 1, 0),
                              pfna_any_value = ifelse(contaminant == "pfna" & !is.na(analyticalresultvalue), 1, 0),
                              pfoa_any_value = ifelse(contaminant == "pfoa" & !is.na(analyticalresultvalue), 1, 0),
                              pfos_any_value = ifelse(contaminant == "pfos" & !is.na(analyticalresultvalue), 1, 0),
                              pfpea_any_value = ifelse(contaminant == "pfpea" & !is.na(analyticalresultvalue), 1, 0),
                              pfpes_any_value = ifelse(contaminant == "pfpes" & !is.na(analyticalresultvalue), 1, 0),
                              pfta_any_value = ifelse(contaminant == "pfta" & !is.na(analyticalresultvalue), 1, 0),
                              pftrda_any_value = ifelse(contaminant == "pftrda" & !is.na(analyticalresultvalue), 1, 0),
                              pfuna_any_value = ifelse(contaminant == "pfuna" & !is.na(analyticalresultvalue), 1, 0)
                            ) %>% 
                            group_by(pws_id) %>% 
                            summarise(
                              x11cl_pf3ouds_any_value = ifelse(sum(x11cl_pf3ouds_any_value)>0,1,0),
                              x4_2fts_any_value = ifelse(sum(x4_2fts_any_value)>0,1,0),
                              x6_2fts_any_value = ifelse(sum(x6_2fts_any_value)>0,1,0),
                              x8_2fts_any_value = ifelse(sum(x8_2fts_any_value)>0,1,0),
                              x9cl_pf3ons_any_value = ifelse(sum(x9cl_pf3ons_any_value)>0,1,0),
                              adona_any_value = ifelse(sum(adona_any_value)>0,1,0),
                              hfpo_da_any_value = ifelse(sum(hfpo_da_any_value)>0,1,0),
                              netfosaa_any_value = ifelse(sum(netfosaa_any_value)>0,1,0),
                              nfdha_any_value = ifelse(sum(nfdha_any_value)>0,1,0),
                              nmefosaa_any_value = ifelse(sum(nmefosaa_any_value)>0,1,0),
                              pfba_any_value = ifelse(sum(pfba_any_value)>0,1,0),
                              pfbs_any_value = ifelse(sum(pfbs_any_value)>0,1,0),
                              pfda_any_value = ifelse(sum(pfda_any_value)>0,1,0),
                              pfdoa_any_value = ifelse(sum(pfdoa_any_value)>0,1,0),
                              pfeesa_any_value = ifelse(sum(pfeesa_any_value)>0,1,0),
                              pfhpa_any_value = ifelse(sum(pfhpa_any_value)>0,1,0),
                              pfhps_any_value = ifelse(sum(pfhps_any_value)>0,1,0),
                              pfhxa_any_value = ifelse(sum(pfhxa_any_value)>0,1,0),
                              pfhxs_any_value = ifelse(sum(pfhxs_any_value)>0,1,0),
                              pfmba_any_value = ifelse(sum(pfmba_any_value)>0,1,0),
                              pfmpa_any_value = ifelse(sum(pfmpa_any_value)>0,1,0),
                              pfna_any_value = ifelse(sum(pfna_any_value)>0,1,0),
                              pfoa_any_value = ifelse(sum(pfoa_any_value)>0,1,0),
                              pfos_any_value = ifelse(sum(pfos_any_value)>0,1,0),
                              pfpea_any_value = ifelse(sum(pfpea_any_value)>0,1,0),
                              pfpes_any_value = ifelse(sum(pfpes_any_value)>0,1,0),
                              pfta_any_value = ifelse(sum(pfta_any_value)>0,1,0),
                              pftrda_any_value = ifelse(sum(pftrda_any_value)>0,1,0),
                              pfuna_any_value = ifelse(sum(pfuna_any_value)>0,1,0)
                            )
                          ,
                          by = "pws_id"
                        )

# which pfas chemicals over mrl?
pa_pfas_sampling_wide <- left_join(
                            pa_pfas_sampling_wide,
                            pa_pfas_sampling %>% 
                            mutate(
                              x11cl_pf3ouds_above_mrl_0_005 = ifelse(contaminant == "11cl-pf3ouds" & analyticalresultssign == "=", 1, 0),
                              x4_2fts_above_mrl_0_003 = ifelse(contaminant == "4:2 fts" & analyticalresultssign == "=", 1, 0),
                              x6_2fts_above_mrl_0_005 = ifelse(contaminant == "6:2 fts" & analyticalresultssign == "=", 1, 0),
                              x8_2fts_above_mrl_0_005 = ifelse(contaminant == "8:2 fts" & analyticalresultssign == "=", 1, 0),
                              x9cl_pf3ons_above_mrl_0_002 = ifelse(contaminant == "9cl-pf3ons" & analyticalresultssign == "=", 1, 0),
                              adona_above_mrl_0_003 = ifelse(contaminant == "adona" & analyticalresultssign == "=", 1, 0),
                              hfpo_da_above_mrl_0_005 = ifelse(contaminant == "hfpo-da" & analyticalresultssign == "=", 1, 0),
                              netfosaa_above_mrl_0_005 = ifelse(contaminant == "netfosaa" & analyticalresultssign == "=", 1, 0),
                              nfdha_above_mrl_0_020 = ifelse(contaminant == "nfdha" & analyticalresultssign == "=", 1, 0),
                              nmefosaa_above_mrl_0_006 = ifelse(contaminant == "nmefosaa" & analyticalresultssign == "=", 1, 0),
                              pfba_above_mrl_0_005 = ifelse(contaminant == "pfba" & analyticalresultssign == "=", 1, 0),
                              pfbs_above_mrl_0_090 = ifelse(contaminant == "pfbs" & analyticalresultssign == "=", 1, 0),
                              pfda_above_mrl_0_003 = ifelse(contaminant == "pfda" & analyticalresultssign == "=", 1, 0),
                              pfdoa_above_mrl_0_003 = ifelse(contaminant == "pfdoa" & analyticalresultssign == "=", 1, 0),
                              pfeesa_above_mrl_0_003 = ifelse(contaminant == "pfeesa" & analyticalresultssign == "=", 1, 0),
                              pfhpa_above_mrl_0_003 = ifelse(contaminant == "pfhpa" & analyticalresultssign == "=", 1, 0),
                              pfhps_above_mrl_0_003 = ifelse(contaminant == "pfhps" & analyticalresultssign == "=", 1, 0),
                              pfhxa_above_mrl_0_003 = ifelse(contaminant == "pfhxa" & analyticalresultssign == "=", 1, 0),
                              pfhxs_above_mrl_0_003 = ifelse(contaminant == "pfhxs" & analyticalresultssign == "=", 1, 0),
                              pfmba_above_mrl_0_003 = ifelse(contaminant == "pfmba" & analyticalresultssign == "=", 1, 0),
                              pfmpa_above_mrl_0_004 = ifelse(contaminant == "pfmpa" & analyticalresultssign == "=", 1, 0),
                              pfna_above_mrl_0_004 = ifelse(contaminant == "pfna" & analyticalresultssign == "=", 1, 0),
                              pfoa_above_mrl_0_004 = ifelse(contaminant == "pfoa" & analyticalresultssign == "=", 1, 0),
                              pfos_above_mrl_0_004 = ifelse(contaminant == "pfos" & analyticalresultssign == "=", 1, 0),
                              pfpea_above_mrl_0_003 = ifelse(contaminant == "pfpea" & analyticalresultssign == "=", 1, 0),
                              pfpes_above_mrl_0_004 = ifelse(contaminant == "pfpes" & analyticalresultssign == "=", 1, 0),
                              pfta_above_mrl_0_008 = ifelse(contaminant == "pfta" & analyticalresultssign == "=", 1, 0),
                              pftrda_above_mrl_0_007 = ifelse(contaminant == "pftrda" & analyticalresultssign == "=", 1, 0),
                              pfuna_above_mrl_0_002 = ifelse(contaminant == "pfuna" & analyticalresultssign == "=", 1, 0)
                            ) %>% 
                            group_by(pws_id) %>% 
                            summarise(
                              x11cl_pf3ouds_above_mrl_0_005 = ifelse(sum(x11cl_pf3ouds_above_mrl_0_005)>0,1,0),
                              x4_2fts_above_mrl_0_003 = ifelse(sum(x4_2fts_above_mrl_0_003)>0,1,0),
                              x6_2fts_above_mrl_0_005 = ifelse(sum(x6_2fts_above_mrl_0_005)>0,1,0),
                              x8_2fts_above_mrl_0_005 = ifelse(sum(x8_2fts_above_mrl_0_005)>0,1,0),
                              x9cl_pf3ons_above_mrl_0_002 = ifelse(sum(x9cl_pf3ons_above_mrl_0_002)>0,1,0),
                              adona_above_mrl_0_003 = ifelse(sum(adona_above_mrl_0_003)>0,1,0),
                              hfpo_da_above_mrl_0_005 = ifelse(sum(hfpo_da_above_mrl_0_005)>0,1,0),
                              netfosaa_above_mrl_0_005 = ifelse(sum(netfosaa_above_mrl_0_005)>0,1,0),
                              nfdha_above_mrl_0_020 = ifelse(sum(nfdha_above_mrl_0_020)>0,1,0),
                              nmefosaa_above_mrl_0_006 = ifelse(sum(nmefosaa_above_mrl_0_006)>0,1,0),
                              pfba_above_mrl_0_005 = ifelse(sum(pfba_above_mrl_0_005)>0,1,0),
                              pfbs_above_mrl_0_090 = ifelse(sum(pfbs_above_mrl_0_090)>0,1,0),
                              pfda_above_mrl_0_003 = ifelse(sum(pfda_above_mrl_0_003)>0,1,0),
                              pfdoa_above_mrl_0_003 = ifelse(sum(pfdoa_above_mrl_0_003)>0,1,0),
                              pfeesa_above_mrl_0_003 = ifelse(sum(pfeesa_above_mrl_0_003)>0,1,0),
                              pfhpa_above_mrl_0_003 = ifelse(sum(pfhpa_above_mrl_0_003)>0,1,0),
                              pfhps_above_mrl_0_003 = ifelse(sum(pfhps_above_mrl_0_003)>0,1,0),
                              pfhxa_above_mrl_0_003 = ifelse(sum(pfhxa_above_mrl_0_003)>0,1,0),
                              pfhxs_above_mrl_0_003 = ifelse(sum(pfhxs_above_mrl_0_003)>0,1,0),
                              pfmba_above_mrl_0_003 = ifelse(sum(pfmba_above_mrl_0_003)>0,1,0),
                              pfmpa_above_mrl_0_004 = ifelse(sum(pfmpa_above_mrl_0_004)>0,1,0),
                              pfna_above_mrl_0_004 = ifelse(sum(pfna_above_mrl_0_004)>0,1,0),
                              pfoa_above_mrl_0_004 = ifelse(sum(pfoa_above_mrl_0_004)>0,1,0),
                              pfos_above_mrl_0_004 = ifelse(sum(pfos_above_mrl_0_004)>0,1,0),
                              pfpea_above_mrl_0_003 = ifelse(sum(pfpea_above_mrl_0_003)>0,1,0),
                              pfpes_above_mrl_0_004 = ifelse(sum(pfpes_above_mrl_0_004)>0,1,0),
                              pfta_above_mrl_0_008 = ifelse(sum(pfta_above_mrl_0_008)>0,1,0),
                              pftrda_above_mrl_0_007 = ifelse(sum(pftrda_above_mrl_0_007)>0,1,0),
                              pfuna_above_mrl_0_002 = ifelse(sum(pfuna_above_mrl_0_002)>0,1,0)
                            )
                          ,
                          by = "pws_id"
                )

# sum of pfas chemicals above mrl performed by collection date 
# to not add values from the same chemical more than once
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(analyticalresultssign == "=" & date == 1) %>% 
#                             distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(pfas_sum_values_above_mrl_1 = sum(analyticalresultvalue))
#                           ,
#                           by = "pws_id"
#       )
# 
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(analyticalresultssign == "=" & date == 2) %>% 
#                             distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(pfas_sum_values_above_mrl_2 = sum(analyticalresultvalue))
#                           ,
#                           by = "pws_id"
#                         )
# 
# pa_pfas_sampling_wide <- left_join(
#                             pa_pfas_sampling_wide,
#                             pa_pfas_sampling %>% 
#                             filter(analyticalresultssign == "=" & date == 3) %>% 
#                             distinct(pws_id, contaminant, sampleid, analyticalresultvalue) %>% 
#                             group_by(pws_id) %>% 
#                             summarise(pfas_sum_values_above_mrl_3 = sum(analyticalresultvalue))
#                           ,
#                           by = "pws_id"
#                         )

# pa_pfas_sampling_wide <- pa_pfas_sampling_wide %>% 
#                             mutate(
#                               pfas_sum_values_above_mrl_highest_detected = pmax(pfas_sum_values_above_mrl_1,pfas_sum_values_above_mrl_2,pfas_sum_values_above_mrl_3, na.rm = TRUE),
#                               pfas_sum_values_above_mrl_lowest_detected = pmin(pfas_sum_values_above_mrl_1,pfas_sum_values_above_mrl_2,pfas_sum_values_above_mrl_3, na.rm = TRUE)
#                             )

# of times detected over mrl by chemical?
num_samples_by_chem <- left_join(
                        left_join(
                          pa_pfas_sampling %>% 
                          count(pws_id, contaminant, name = "num_samples_by_chem")
                        ,
                          pa_pfas_sampling %>% 
                          filter(analyticalresultssign == "=") %>% 
                          count(pws_id, contaminant, name = "num_samples_by_chem_above_mrl")
                        ,
                        by = c("pws_id","contaminant")
                        ),
                        pa_pfas_sampling %>% 
                          filter(!is.na(analyticalresultvalue)) %>% 
                          count(pws_id, contaminant, name = "num_samples_by_chem_any_value")
                        ,
                        by = c("pws_id","contaminant")                        
                  )

pa_pfas_sampling_wide <- left_join(
                          pa_pfas_sampling_wide,
                          left_join(
                            left_join(
                              num_samples_by_chem %>%
                                pivot_wider(
                                  id_cols = c(pws_id),
                                  names_from = contaminant,
                                  names_glue = "{contaminant}_num_samples",
                                  values_from = num_samples_by_chem
                                ) %>% 
                                clean_names()
                              ,
                              num_samples_by_chem %>%
                                pivot_wider(
                                  id_cols = c(pws_id),
                                  names_from = contaminant,
                                  names_glue = "{contaminant}_num_samples_above_mrl",
                                  values_from = num_samples_by_chem_above_mrl
                                ) %>% 
                                clean_names()
                                ,
                                by = "pws_id"
                            ),
                            num_samples_by_chem %>%
                              pivot_wider(
                                id_cols = c(pws_id),
                                names_from = contaminant,
                                names_glue = "{contaminant}_num_samples_any_value",
                                values_from = num_samples_by_chem_above_mrl
                              ) %>% 
                              clean_names()
                            ,
                            by = "pws_id"
                          )
                          ,
                          by = "pws_id"
          )  %>% 
          rename(
            x4_2fts_num_samples = x4_2_fts_num_samples,
            x6_2fts_num_samples = x6_2_fts_num_samples,
            x8_2fts_num_samples = x8_2_fts_num_samples
          )

pa_pfas_sampling_wide[is.na(pa_pfas_sampling_wide)] <- 0

# change in detected pfas over time by chemical (e.g., went from over mrl to below mrl)

## first widen the data with all sample results for each date for each chemical
# temp_wide <- ucmr5_long %>%
#               pivot_wider(
#                 id_cols = c(pws_id),
#                 names_from = c(contaminant,date),
#                 names_glue = "{contaminant}_{date}",
#                 values_from = analyticalresultssign
#               ) %>% 
#               clean_names()
#           

## identify temporal pattern for each PWS and chemical

### if cell contains "=" at all then 1, else 0. for all 3 dates
### combine 3 date values into single column so.. 111, 000, etc..
### use to make new column for change.. positive, negative, no change, non-linear

### logic:
### 100 (negative) 110 (negative) 111 (no change) 101 (non-linear)
### 000 (no change) 010 (non-linear) 011 (positive) 001 (positive)
### 0NN (no longitudinal sample) 00N 

# mrl_time <- function(x){
#   temp_wide <- temp_wide %>% 
#     mutate(
#       !!paste0(x, "_time") := 
#         str_c(
#           ifelse(grepl("=", !!sym(paste0(x, "_1"))) == TRUE, 1, 0),
#           ifelse(grepl("=", !!sym(paste0(x, "_2"))) == TRUE, 1, 0),
#           ifelse(grepl("=", !!sym(paste0(x, "_3"))) == TRUE, 1, 0),
#           sep = ""
#         )
#       )
#   return(temp_wide)
# }
# 
# time_status <- function(x){
#   temp_wide <- temp_wide %>% 
#     mutate(
#       !!paste0(x, "_time_change") := 
#         case_when(
#           get(!!paste0(x, "_time")) %in% c("100", "110") ~ "Negative",
#           get(!!paste0(x, "_time")) %in% c("011", "001") ~ "Positive",      
#           get(!!paste0(x, "_time")) %in% c("111", "000") ~ "No change",      
#           get(!!paste0(x, "_time")) %in% c("101", "010") ~ "Non-linear"
#         )
#     )
#   return(temp_wide)
# }
# 
# pfas_chems <- c("x11cl_pf3ouds","x4_2_fts","x6_2_fts","x8_2_fts","x9cl_pf3ons","adona","hfpo_da","nfdha","pfba","pfbs","pfda","pfdoa","pfeesa","pfhpa","pfhps","pfhxa","pfhxs","pfmba","pfmpa","pfna","pfoa","pfos","pfpea","pfpes","pfuna")
# 
# for (i in pfas_chems) {
#   temp_wide <- mrl_time(i)
#   temp_wide <- time_status(i)
# }

### NOTE: The tests on different dates are not necessarily sampled from the same location.
### What the final variable shows is whether or not there was a change over time in the PWS in general.
### This could mean that on collection date 1, a sample in one well for the PWS could have tested positive but then
### on collection date 2 for the same PWS, no sample was taken at the original well but a new sample was
### taken at a a different well and that result was negative. This result would look like PFAS contamination
### in the PWS is decreasing, but it is possible the second well was never contaminated.
### Also, some samples were taken within days of each other at different sites.






# 4. output files for rti ----

pa_pfas_sampling_wide_rti <- pa_pfas_sampling_wide %>% 
                                dplyr::select(pws_id, starts_with("num"), any_contaminants_above_mrl, pfas_detected_4cat, starts_with("pfos"), starts_with("pfoa"), starts_with("pfna"), starts_with("pfhxs"), starts_with("pfhpa"), starts_with("pfbs"))

pa_pfas_sampling_rti <- pa_pfas_sampling %>% 
                          filter(contaminant %in% c("pfos","pfoa","pfna","pfhxs","pfhpa","pfbs"))

write_csv(pa_pfas_sampling_wide_rti, file = "00_data/pa_pfas_sampling_wide_rti.csv")
write_csv(pa_pfas_sampling_rti, file = "00_data/pa_pfas_sampling_rti.csv")

pws_pfastested <- merge(
                    x = pa_pfas_sampling_wide,
                    y = pws %>% 
                      dplyr::select(PWS_ID, NAME,CNTY_NAME, GW_SOURCE, SW_SOURCE, INTCONNECT, OWNERSHIP),
                    by.x = "pws_id",
                    by.y = "PWS_ID",
                    keep.x = TRUE
                  )
  
st_write(pws_pfastested, dsn = "00_data/pws_pfastested.shp")


# 5. calculate pws site density ----

# how many data points per PWS - adding half-mile buffer (804.672)

## known pfas
# superfund
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), superfund_merge)

superfund_count <- intersection %>%
                      filter(!is_empty(.)) %>%
                      group_by(pws_id) %>%
                      summarise(count_superfund = n()) %>% 
                      st_drop_geometry()

# hsca
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), non_superfund)

hsca_count <- intersection %>%
                filter(!is_empty(.)) %>%
                group_by(pws_id) %>%
                summarise(count_hsca = n()) %>% 
                st_drop_geometry()

# discharge sites 
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), facilities_discharge_pfas)

discharge_count <- intersection %>%
                    filter(!is_empty(.)) %>%
                    group_by(pws_id) %>%
                    summarise(count_discharge = n()) %>% 
                    st_drop_geometry()

# ridge run
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), ridge_run)

ridge_run_count <- intersection %>%
                    filter(!is_empty(.)) %>%
                    group_by(pws_id) %>%
                    summarise(count_ridge_run = n()) %>% 
                    st_drop_geometry()

# neshaminy creek
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), neshaminy_creek)

neshaminy_creek_count <- intersection %>%
                          filter(!is_empty(.)) %>%
                          group_by(pws_id) %>%
                          summarise(count_neshaminy_creek = n()) %>% 
                          st_drop_geometry()

# combine suspected pfas counts
known_pfas_count <- list(dplyr::select(pws_pfastested, pws_id), 
                        superfund_count, hsca_count, discharge_count, ridge_run_count, neshaminy_creek_count) %>% 
                    reduce(full_join, by='pws_id') %>% 
                    replace(is.na(.), 0) %>% 
                    mutate(count_known_pfas = (count_superfund + count_hsca + count_discharge + count_neshaminy_creek + count_ridge_run))



## suspected pfas
# airports
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), airports)

airport_count <- intersection %>%
                    filter(!is_empty(.)) %>%
                    group_by(pws_id) %>%
                    summarise(count_airports = n()) %>% 
                    st_drop_geometry()

# landfills
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), landfill_studyarea)

landfill_count <- intersection %>%
  filter(!is_empty(.)) %>%
  group_by(pws_id) %>%
  summarise(count_landfills = n()) %>% 
  st_drop_geometry()

# firefighter training
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), Fire_Training_Facilities)

firefighter_count <- intersection %>%
                      filter(!is_empty(.)) %>%
                      group_by(pws_id) %>%
                      summarise(count_firefighters = n()) %>% 
                      st_drop_geometry()

# manufacturing
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), manufacturing)

manufacturing_count <- intersection %>%
                        filter(!is_empty(.)) %>%
                        group_by(pws_id) %>%
                        summarise(count_manufacturing = n()) %>% 
                        st_drop_geometry()

# combine suspected pfas counts
susp_pfas_count <- list(dplyr::select(pws_pfastested, pws_id), 
                        airport_count, landfill_count, firefighter_count, manufacturing_count) %>% 
                   reduce(full_join, by='pws_id') %>% 
                   replace(is.na(.), 0) %>% 
                   mutate(count_susp_pfas = (count_airports + count_landfills + count_firefighters + count_manufacturing))


## how many wells/intakes per PWS

# wells 
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), wells)

wells_count <- intersection %>%
                filter(!is_empty(.)) %>%
                group_by(pws_id) %>%
                summarise(count_wells = n()) %>% 
                st_drop_geometry()

# intakes 
intersection <- st_intersection(st_buffer(st_as_sf(pws_pfastested), 804.672), intakes)

intakes_count <- intersection %>%
                  filter(!is_empty(.)) %>%
                  group_by(pws_id) %>%
                  summarise(count_intakes = n()) %>% 
                  st_drop_geometry()

# combine well/intake counts
wells_intakes_count <- list(dplyr::select(pws_pfastested, pws_id), 
                            wells_count, intakes_count) %>% 
                        reduce(full_join, by='pws_id') %>% 
                        replace(is.na(.), 0) %>% 
                        mutate(count_wells_intakes = (count_wells + count_intakes))


# combine all pws count data frames
pws_counts <- list(dplyr::select(pws_pfastested, pws_id), 
                   known_pfas_count, susp_pfas_count, wells_intakes_count) %>% 
                reduce(full_join, by='pws_id') %>% 
                replace(is.na(.), 0) %>% 
                mutate(count_known_susp_pfas = (count_known_pfas + count_susp_pfas))

write_csv(pws_counts, file = "00_data/pws_counts.csv")




# Create a .zip archive with one or more files
zip(zipfile = "00_data/pfas_data_geo.zip", 
    files = c(
      # study area
      "00_data/study_counties.shp",
      "00_data/study_counties.dbf",
      "00_data/study_counties.prj",
      "00_data/study_counties.shx",
      # excel of known pfas w notes
      "00_data/PFAS_detected_superfund_others.xlsx",
      # known pfas
      "00_data/known_pfas_superfund_sites.shp",
      "00_data/known_pfas_hsca_sites.shp",
      "00_data/known_pfas_neshaminy_creek_watershed.shp",
      "00_data/known_pfas_ridge_run.shp",
      "00_data/known_pfas_discharge.shp",
      "00_data/known_pfas_superfund_sites.dbf",
      "00_data/known_pfas_hsca_sites.dbf",
      "00_data/known_pfas_neshaminy_creek_watershed.dbf",
      "00_data/known_pfas_ridge_run.dbf",
      "00_data/known_pfas_discharge.dbf",
      "00_data/known_pfas_superfund_sites.prj",
      "00_data/known_pfas_hsca_sites.prj",
      "00_data/known_pfas_neshaminy_creek_watershed.prj",
      "00_data/known_pfas_ridge_run.prj",
      "00_data/known_pfas_discharge.prj",
      "00_data/known_pfas_superfund_sites.shx",
      "00_data/known_pfas_hsca_sites.shx",
      "00_data/known_pfas_neshaminy_creek_watershed.shx",
      "00_data/known_pfas_ridge_run.shx",
      "00_data/known_pfas_discharge.shx",
      # suspected pfas
      "00_data/susp_pfas_manufacturing.shp",
      "00_data/susp_pfas_landfills.shp",
      "00_data/susp_pfas_firefighter_training.shp",
      "00_data/susp_pfas_airports.shp",
      "00_data/susp_pfas_manufacturing.shx",
      "00_data/susp_pfas_landfills.shx",
      "00_data/susp_pfas_firefighter_training.shx",
      "00_data/susp_pfas_airports.shx",
      "00_data/susp_pfas_manufacturing.prj",
      "00_data/susp_pfas_landfills.prj",
      "00_data/susp_pfas_firefighter_training.prj",
      "00_data/susp_pfas_airports.prj",
      "00_data/susp_pfas_manufacturing.dbf",
      "00_data/susp_pfas_landfills.dbf",
      "00_data/susp_pfas_firefighter_training.dbf",
      "00_data/susp_pfas_airports.dbf",
      # wells targeted by state
      "00_data/wells.shp",
      "00_data/wells.prj",
      "00_data/wells.dbf",
      "00_data/wells.shx",
      # intakes targeted by state
      "00_data/intakes.shp",
      "00_data/intakes.shx",
      "00_data/intakes.dbf",
      "00_data/intakes.prj",
      # pws wide file w aggregate data 
      "00_data/pa_pfas_sampling_wide.csv",
      # pws long file observation for each sample
      "00_data/pa_pfas_sampling.csv",
      # pws shapefiles only
      "00_data/pws.shp",
      "00_data/pws.dbf",
      "00_data/pws.prj",
      "00_data/pws.shx",
      # pws shapefiles merged with aggregate wide data
      "00_data/pws_pfastested.shp",
      "00_data/pws_pfastested.prj",
      "00_data/pws_pfastested.dbf",
      "00_data/pws_pfastested.shx"
    )
)










# 6. descriptive statistics ----

# unique(pa_pfas_sampling_ucmr3dep$pws_id)
# +12 pws

pfas_sum <- pa_pfas_sampling_wide[,c("num_samples",
                                     "num_sample_locations",
                                     "num_dates",
                                     "num_gw",
                                     "num_contaminants_tested",
                                     "any_contaminants_above_mrl",
                                     "num_contaminants_above_mrl")]

pfas_sum_ds <- as.data.frame(
                t(
                  do.call(cbind, lapply(pfas_sum, summary))
                )
)

pfas_sum_ds <-rownames_to_column(pfas_sum_ds, "variable")

write_csv(pfas_sum_ds, file = "00_data/pfas_sum_ds.csv")

prop.table(table(pa_pfas_sampling_wide$pfas_detected_4cat))

sum(ifelse(pa_pfas_sampling_wide$x11cl_pf3ouds_num_samples>0,1,0))
sum(ifelse(pa_pfas_sampling_wide$x11cl_pf3ouds_above_mrl>0,1,0))

contaminants <- c("x11cl_pf3ouds","x4_2fts","x6_2fts","x8_2fts","x9cl_pf3ons","adona","hfpo_da","netfosaa","nfdha","nmefosaa","pfba","pfbs","pfda","pfdoa","pfeesa","pfhpa","pfhps","pfhxa","pfhxs","pfmba","pfmpa","pfna","pfoa","pfos","pfpea","pfpes","pfta","pftrda","pfuna")

contam_descrip <- data.frame()

for (i in 1:length(contaminants)) {
  temp_df <- cbind(contaminants[i],sum(ifelse(pa_pfas_sampling_wide[,paste0(contaminants[i],"_num_samples")]>0,1,0)),sum(ifelse(pa_pfas_sampling_wide[,paste0(contaminants[i],"_above_mrl")]>0,1,0)))
  contam_descrip <- rbind(contam_descrip,temp_df)
}

colnames(contam_descrip) <- c("contaminant","num_pws_sampledchem","num_pws_chem_above_mrl")

write_csv(contam_descrip, file = "00_data/contam_descrip.csv")


# 7. maps ----

# attaching geometry to wide file

pfas_pws_wide <- merge(x = pa_pfas_sampling_wide, y = pws.shp, by.x = "pws_id", by.y = "PWS_ID", keep.x = TRUE)

## 7.1 pws with and without sampling data ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = pws.shp, aes(fill = "nottested", color = "nottested")) +
  geom_sf(data = pfas_pws_wide, aes(geometry = geoms, fill = "tested", color = "tested")) +
  labs(caption = "Public water supply areas tested (134/581) for PFAS from 2013 to 2023") +
  scale_fill_manual(values = c("nottested" = "grey70","tested" = "royalblue3"), labels = c("Not Tested","Tested")) +
  scale_color_manual(values = c("nottested" = "grey70","tested" = "royalblue3")) +
  guides(
    fill = guide_legend(title = "PFAS Sampling"),
    color = "none"
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

ggsave("02_output/pws_w_samplingdata.png", height = 6, width = 7.2, units = "in")

## 7.2 pfas detected sites ----

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

## 7.3 pfas sites with pws ----

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

## 7.4 pfas sites with pws pfas over mrl ----

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

## 7.5 pws pfas detected 4cat ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = pfas_pws_wide, aes(geometry = geoms, fill = pfas_detected_4cat, color = pfas_detected_4cat)) +
  labs(caption = "PFAS public water supply area sampling results from 2013 to 2023") +
  scale_fill_manual(
    values = c("PFAS detected at or above MRL" = "firebrick2",
               "PFAS detected above 0 but below MRL" = "orange",
               "PFAS detected at 0 or less than MRL" = "yellow",
               "PFAS not detected" = "forestgreen"
               ),
    breaks = c("PFAS detected at or above MRL","PFAS detected above 0 but below MRL","PFAS detected at 0 or less than MRL","PFAS not detected"),    
    labels = c("PFAS detected at or above MRL","PFAS detected above 0 but below MRL","PFAS detected at 0 or less than MRL","PFAS not detected")
  ) +
  scale_color_manual(
    values = c("PFAS detected at or above MRL" = "firebrick2",
               "PFAS detected above 0 but below MRL" = "orange",
               "PFAS detected at 0 or less than MRL" = "yellow",
               "PFAS not detected" = "forestgreen"
    )
  ) +
  guides(
    fill = guide_legend(title = "PFAS Detection"),
    color = "none"
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

ggsave("02_output/PFAS_detected_pws.png", height = 4, width = 6.79, units = "in")

## 7.6 pws pfas detected 4cat w PFAS detected sites ----

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = pfas_pws_wide, aes(geometry = geoms, fill = pfas_detected_4cat, color = pfas_detected_4cat)) +
  labs(caption = "PFAS public water supply area sampling results from 2013 to 2023") +
  scale_fill_manual(
    values = c("PFAS detected at or above MRL" = "firebrick2",
               "PFAS detected above 0 but below MRL" = "orange",
               "PFAS detected at 0 or less than MRL" = "yellow",
               "PFAS not detected" = "forestgreen"
    ),
    breaks = c("PFAS detected at or above MRL","PFAS detected above 0 but below MRL","PFAS detected at 0 or less than MRL","PFAS not detected"),    
    labels = c("PFAS detected at or above MRL","PFAS detected above 0 but below MRL","PFAS detected at 0 or less than MRL","PFAS not detected")
  ) +
  scale_color_manual(
    values = c("PFAS detected at or above MRL" = "firebrick2",
               "PFAS detected above 0 but below MRL" = "orange",
               "PFAS detected at 0 or less than MRL" = "yellow",
               "PFAS not detected" = "forestgreen"
    )
  ) +
  guides(
    fill = guide_legend(title = "PFAS Detection"),
    color = "none"
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

ggsave("02_output/PFAS_detected_pws.png", height = 4, width = 6.79, units = "in")

## 7.7 pws x number of samples ----

pfas_pws_wide$num_samples.q <- cut(pfas_pws_wide$num_samples, quantile(pfas_pws_wide$num_samples, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)

cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = pfas_pws_wide, aes(geometry = geoms, fill = as.factor(num_samples.q), color = as.factor(num_samples.q))) +
  scale_fill_manual(values=cols.blues, labels = c('[0-25%: 12-18]','[25-50%: 18-24]', '[50-75%: 24-75]', '[75-100%: 75-435]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  labs(caption = "Quartiles of the number of PFAS samples in public water supply ares from 2013 to 2023") +
  guides(
    fill = guide_legend(title = "Number of Samples"),
    color = "none"
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

ggsave("02_output/pws_num_samples.png", height = 5.15, width = 6.68, units = "in")

## 7.8 pws x number of detected contaminants ----

pfas_pws_wide$num_contaminants_above_mrl.q <- cut(pfas_pws_wide$num_contaminants_above_mrl, c(0,1,2,3,+Inf), label = FALSE) #, include.lowest = TRUE)

view(pfas_pws_wide[,c("num_contaminants_above_mrl","num_contaminants_above_mrl.q")])

cols.reds <- c("#feedde","#fdbe85","#fd8d3c","#a63603")

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = pfas_pws_wide, aes(geometry = geoms, fill = as.factor(num_contaminants_above_mrl.q), color = as.factor(num_contaminants_above_mrl.q))) +
  scale_fill_manual(values=cols.reds, labels = c('1','2', '3', '4+'), limits= c("1","2","3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=cols.reds, limits= c("1","2","3","4"), na.value="grey90", drop=FALSE) +
  labs(caption = "Quartiles of the number of PFAS contaminats above the MRL in public water supply areas\nfrom 2013 to 2023") +
  guides(
    fill = guide_legend(title = "Number of PFAS\ncontaminants above MRL"),
    color = "none"
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

ggsave("02_output/pws_num_contaminants.png", height = 5.1, width = 6.85, units = "in")

## 7.9 unused ----

# electronic manufacturing facilities

## pfas detected sites

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
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition + Electronics Manufacturing") +
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


## pfas sites with pws

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

## pfas sites with pws pfas over mrl

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

ggsave("02_output/PFAS_sites_pws_pfasovermrl_plastics.png", height = 7, width = 8, units = "in")

# map plastics manufacturing facilities

## pfas detected sites

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = plastics_geo_study, aes(color = "Unknown", shape = "Manufacturing"), size = 1.5) +
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
  labs(caption = "PFAS Detected Sites by Site Type and Government Recognition + Plastics Manufacturing") +
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

ggsave("02_output/PFAS_sites_plastics.png", height = 6, width = 7, units = "in")


## pfas sites with pws

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = filter(pws.shp.pfas_inst, any_pfas == 1), fill = "grey90", color = "grey50") +
  geom_sf(data = neshaminy_creek, aes(fill = "Non-Superfund", color = "black")) +
  geom_sf(data = plastics_geo_study, aes(color = "Unknown", shape = "Manufacturing"), size = 1.5) +
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

ggsave("02_output/PFAS_sites_pws_plastics.png", height = 7, width = 8, units = "in")

## pfas sites with pws pfas over mrl

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

ggsave("02_output/PFAS_sites_pws_pfasovermrl_plastics.png", height = 7, width = 8, units = "in")

# end of r script --------------------------------------------------------