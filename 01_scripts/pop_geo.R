# beginning of r script ----

# 1. libraries ----

if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','units','sf','ggmap','biscale','reshape2','janitor','ggpattern','tidygeocoder','lubridate', 'openxlsx','spdep','cowplot')

options(scipen = 999)

# 2. load data ----

# census tracts

# c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")
# st_set_crs(st_crs(4269))

tract00 <- tigris::tracts(state = "42", cb = TRUE, year = 2000)
tract10 <- tigris::tracts(state = "42", cb = TRUE, year = 2010)
tract20 <- tigris::tracts(state = "42", cb = TRUE, year = 2020)

tract00_study <- tigris::tracts(state = "42", county = c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"), cb = TRUE, year = 2000)
tract10_study <- tigris::tracts(state = "42", county = c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"), cb = TRUE, year = 2010)
tract20_study <- tigris::tracts(state = "42", county = c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"), cb = TRUE, year = 2020)
  
# census population data

api_key <- "e0e0537e1640689fd0719a4f97581e336b10c8ce"
census_api_key(api_key, overwrite=TRUE, install=TRUE)

readRenviron("~/.Renviron")

options(tigris_use_cache = TRUE)

## use this link for reference to census tables: https://www.socialexplorer.com/data/C2020/metadata/?ds=SF1&table=P0120

# decennial_tables <- load_variables(2010, "sf1")
# view(decennial_tables)
# P012 = sex by age

# acs_tables <- load_variables(2010, "acs5")
# view(acs_tables)
# B01001 = sex by age

## downloading population counts for PA block groups in 12 study counties by 
## sex and age from the 2010 decennial census

# not available through tidycensus
# tract_pop_90 <- get_decennial(geography = "tract", 
#                               table = "P012",               
#                               year = 1990,
#                               state = "PA",
#                               geometry = TRUE,
#                               keep_geo_vars = TRUE,     
#                               sumfile = "sf1",              
#                               output = "wide",
#                               cache_table = TRUE
# )

tract_pop_00 <- get_decennial(geography = "tract", 
                            table = "P012",               
                            year = 2000,
                            state = c("34","36","39","42","54"),
                            geometry = TRUE,
                            keep_geo_vars = TRUE,     
                            sumfile = "sf1",              
                            output = "wide",
                            cache_table = TRUE
)

# neighboring states?
acs_pop <- function(ds,year){
  intermediate_df <- get_acs(geography = "tract", 
                             table = "B01001",               
                             year = year,
                             state = "PA",
                             geometry = TRUE,
                             keep_geo_vars = TRUE,     
                             survey = "acs5",              
                             output = "wide",
                             cache_table = TRUE
  )
  assign(ds, intermediate_df, envir = .GlobalEnv)
}

acs_pop("tract_pop_05_09",2009)
acs_pop("tract_pop_06_10",2010)
acs_pop("tract_pop_07_11",2011)
acs_pop("tract_pop_08_12",2012)
acs_pop("tract_pop_09_13",2013)
acs_pop("tract_pop_10_14",2014)
acs_pop("tract_pop_11_15",2015)
acs_pop("tract_pop_12_16",2016)
acs_pop("tract_pop_13_17",2017)
acs_pop("tract_pop_14_18",2018)
acs_pop("tract_pop_15_19",2019)
acs_pop("tract_pop_16_20",2020)
acs_pop("tract_pop_17_21",2021)
acs_pop("tract_pop_18_22",2022)
# ^^^^ last year of available data



# 1990 -> 2000 relationship file - DOES NOT INCLUDE PCT OVERLAP - DOES INCLUDE POP OF OVERLAPPING AREA
library(utils)

rel_1990_2000 <- read.fwf("00_data/tract_rel_files/pa_tract_1990_2000.txt",
                          widths = c(2,3,4,2,1,9,4,2,3,4,2,1,9,4,9,14,2,60),
                          col.names = c("statefips90","countyfips90","ctbase90","ctsuff90","ctpartflag90","2000popfor_1990ct","pct90ctpop",
                                        "statefips00","countyfips00","ctbase00","ctsuff00","ctpartflag00","2000popfor_2000ct","pct00ctpop",
                                        "2000pop_part","arealand_part","stabbrv00","cntyname00"))


# 2000 -> 2010 relationship file

rel_2000_2010 <- read_delim("00_data/tract_rel_files/pa_tract_2000_2010.txt", delim = ",", col_names = FALSE)
rel_2000_2010_colnames <- as.character(read_delim("00_data/tract_rel_files/pa_tract_2000_2010_colnames.txt", delim = ",", col_names = FALSE)[1,])
colnames(rel_2000_2010) <- rel_2000_2010_colnames

# rel_2000_2010 <- rel_2000_2010 %>% 
#   filter(COUNTY00 %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107") | 
#          COUNTY10 %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107")
#          )

# table(rel_2000_2010$STATE00,rel_2000_2010$STATE10)
# length(unique(rel_2000_2010$GEOID00))
# length(unique(rel_2000_2010$GEOID10))

# AREALANDPT = Calculated overlapping land area in square meters

# What % of 2010 is overlapped by 2000?

rel_2000_2010$overlap_10_00 <- (rel_2000_2010$AREALANDPT/rel_2000_2010$AREALAND10)*100

rel_2000_2010$overlap_flag_10_00 <- case_when(
  rel_2000_2010$overlap_10_00 == 100 ~ "100%",
  rel_2000_2010$overlap_10_00 < 100 & rel_2000_2010$overlap_10_00 >= 75 ~ "75% - 99%",
  rel_2000_2010$overlap_10_00 < 75 & rel_2000_2010$overlap_10_00 >= 50 ~ "50% - 74%",
  rel_2000_2010$overlap_10_00 < 50 & rel_2000_2010$overlap_10_00 >= 25 ~ "25% - 49%",
  rel_2000_2010$overlap_10_00 < 25 & rel_2000_2010$overlap_10_00 >= 0 ~ "0% - 24%"
)

rel_2000_2010$overlap_flag_10_00_bin <- ifelse(rel_2000_2010$overlap_flag_10_00 == "100%", 1, 0)

# What % of 2000 is overlapped by 2010?

rel_2000_2010$overlap_00_10 <- (rel_2000_2010$AREALANDPT/rel_2000_2010$AREALAND00)*100

rel_2000_2010$overlap_flag_00_10 <- case_when(
  rel_2000_2010$overlap_00_10 == 100 ~ "100%",
  rel_2000_2010$overlap_00_10 < 100 & rel_2000_2010$overlap_00_10 >= 75 ~ "75% - 99%",
  rel_2000_2010$overlap_00_10 < 75 & rel_2000_2010$overlap_00_10 >= 50 ~ "50% - 74%",
  rel_2000_2010$overlap_00_10 < 50 & rel_2000_2010$overlap_00_10 >= 25 ~ "25% - 49%",
  rel_2000_2010$overlap_00_10 < 25 & rel_2000_2010$overlap_00_10 >= 0 ~ "0% - 24%"
)

rel_2000_2010$overlap_flag_00_10_bin <- ifelse(rel_2000_2010$overlap_flag_00_10 == "100%", 1, 0)

table(rel_2000_2010$overlap_flag_00_10_bin)

# test by aggregating pct by geoid10

rel_2000_2010_agg <- rel_2000_2010 %>% 
#  filter(str_sub(GEOID00, 1, 2) == "42") %>% 
  group_by(GEOID00) %>% 
  summarise(area_sum = sum(overlap_00_10))

nrow(rel_2000_2010_agg$area_sum)
table(rel_2000_2010_agg$area_sum)

# 2010 -> 2020 relationship file

rel_2010_2020 <- read_delim("00_data/tract_rel_files/natl_tract_2010_2020.txt", delim = "|", col_names = TRUE) %>% 
  # filter(
  #   (str_sub(GEOID_TRACT_20, 1, 2) == "42" | str_sub(GEOID_TRACT_10, 1, 2) == "42") &
  #   (str_sub(GEOID_TRACT_20, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107") | str_sub(GEOID_TRACT_10, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"))
  #     ) %>% 
  dplyr::select(GEOID_TRACT_20, AREALAND_TRACT_20, GEOID_TRACT_10, AREALAND_TRACT_10, AREALAND_PART)

# table(rel_2010_2020$state20,rel_2010_2020$state10)
# length(unique(rel_2010_2020$GEOID_TRACT_20))
# length(unique(rel_2010_2020$GEOID_TRACT_10))

# AREALAND_PART = Calculated Land area of the overlapping part in square meters

# What % of 2020 is overlapped by 2010?

rel_2010_2020$overlap_20_10 <- (rel_2010_2020$AREALAND_PART/rel_2010_2020$AREALAND_TRACT_20)*100

rel_2010_2020$overlap_flag_20_10 <- case_when(
  rel_2010_2020$overlap_20_10 == 100 ~ "100%",
  rel_2010_2020$overlap_20_10 < 100 & rel_2010_2020$overlap_20_10 >= 75 ~ "75% - 99%",
  rel_2010_2020$overlap_20_10 < 75 & rel_2010_2020$overlap_20_10 >= 50 ~ "50% - 74%",
  rel_2010_2020$overlap_20_10 < 50 & rel_2010_2020$overlap_20_10 >= 25 ~ "25% - 49%",
  rel_2010_2020$overlap_20_10 < 25 & rel_2010_2020$overlap_20_10 >= 0 ~ "0% - 24%"
)

rel_2010_2020$overlap_flag_20_10_bin <- ifelse(rel_2010_2020$overlap_flag_20_10 == "100%", 1, 0)
  
# What % of 2010 is overlapped by 2020?

rel_2010_2020$overlap_10_20 <- (rel_2010_2020$AREALAND_PART/rel_2010_2020$AREALAND_TRACT_10)*100

rel_2010_2020$overlap_flag_10_20 <- case_when(
  rel_2010_2020$overlap_10_20 == 100 ~ "100%",
  rel_2010_2020$overlap_10_20 < 100 & rel_2010_2020$overlap_10_20 >= 75 ~ "75% - 99%",
  rel_2010_2020$overlap_10_20 < 75 & rel_2010_2020$overlap_10_20 >= 50 ~ "50% - 74%",
  rel_2010_2020$overlap_10_20 < 50 & rel_2010_2020$overlap_10_20 >= 25 ~ "25% - 49%",
  rel_2010_2020$overlap_10_20 < 25 & rel_2010_2020$overlap_10_20 >= 0 ~ "0% - 24%"
)

rel_2010_2020$overlap_flag_10_20_bin <- ifelse(rel_2010_2020$overlap_flag_10_20 == "100%", 1, 0)

table(rel_2010_2020$overlap_flag_10_20_bin)

# test by aggregating pct by geoid20

rel_2010_2020_agg <- rel_2010_2020 %>% 
  filter(str_sub(GEOID_TRACT_20, 1, 2) == "42") %>% 
  group_by(GEOID_TRACT_20) %>% 
  summarise(area_sum = sum(overlap_20_10))

nrow(rel_2010_2020_agg$area_sum)
table(rel_2010_2020_agg$area_sum)

#dont have 100% coverage by pa census tracts
view(rel_2010_2020[rel_2010_2020$GEOID_TRACT_20 %in% c("42097082300","42101980002","42101012203"),])


# 3. convert 2000 ct to 2020 ct ----

# DECENNIAL
# 2000 -> 2010
# merge pop with tracts

rel_2000_2010$GEOID00_c <- as.character(rel_2000_2010$GEOID00)

tract_pop_00_rel <- left_join(rel_2000_2010, tract_pop_00, by = c("GEOID00_c" = "GEOID")) %>% 
  filter(TRACT00 != "000000" & str_sub(GEOID00, 1, 2) == "42") %>%  # removing tracts with 0 population in 2000 (all water) and non-PA 2000 population
  dplyr::select(GEOID00,GEOID10,overlap_00_10, starts_with("P0")) %>% 
  st_drop_geometry() %>% 
  mutate(across(starts_with("P0"), ~ .x * (overlap_00_10 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
  dplyr::select(GEOID00,GEOID10,overlap_00_10, starts_with("P0"))

# aggregate 2000 census tract pop to 2010 geography

tract_00pop_10geo <- tract_pop_00_rel %>% 
  group_by(GEOID10) %>% 
  summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))

# quality check to see if the expected # of total pop is the same in the aggregated data
# 13 difference. Checked and not due to rounding. Likely due to census tract now overlapping with a neighboring state.
# sum(distinct(dplyr::select(tract_pop_00_rel, GEOID00, P012001))[,"P012001"], na.rm = TRUE)-sum(tract_00pop_10geo$P012001_wtd_sum, na.rm = TRUE)

# how many census tracts? 3217. This matches the num of census tracts in 2010.
# nrow(filter(tract_pop_00, STATE == "42"))


tract_00pop_10geo$GEOID10_c <- as.character(tract_00pop_10geo$GEOID10)

# 2010 -> 2020
# merge pop with tracts
tract_pop_10_rel <- left_join(tract_00pop_10geo, rel_2010_2020, by = c("GEOID10_c" = "GEOID_TRACT_10")) %>% 
#  filter(TRACT00 != "000000" & str_sub(GEOID00, 1, 2) == "42") %>%  # removing tracts with 0 population in 2000 (all water) and non-PA 2000 population
  dplyr::select(GEOID10, GEOID_TRACT_20, overlap_10_20, ends_with("_sum")) %>% 
  st_drop_geometry() %>% 
  mutate(across(ends_with("_sum"), ~ .x * (overlap_10_20 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
  dplyr::select(GEOID10, GEOID_TRACT_20 ,overlap_10_20, ends_with("_wtd"))

# aggregate 2000 census tract pop to 2010 geography

tract_00pop_20geo <- tract_pop_10_rel %>% 
  filter(str_sub(GEOID_TRACT_20, 1, 2) == "42") %>% 
  group_by(GEOID_TRACT_20) %>% 
  summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))

# quality check to see if the expected # of total pop is the same in the aggregated data
# 65 difference. Checked and not due to rounding. Likely due to census tract now overlapping with a neighboring state.
sum(distinct(dplyr::select(tract_pop_00_rel, GEOID00, P012001))[,"P012001"], na.rm = TRUE)-sum(tract_00pop_20geo$P012001_wtd_sum_wtd_sum, na.rm = TRUE)

# how many census tracts? 3445. This matches the num of census tracts in 2020.
# nrow(tract_00pop_20geo)

# reducing to census tracts in study counties
tract_00pop_20geo_studyarea <- tract_00pop_20geo %>% 
  filter(str_sub(GEOID_TRACT_20, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"))

save(tract_00pop_20geo_studyarea, file = "00_data/tract_00pop_20geo_studyarea.Rdata")

#ACS
# 2005 - 2009
# 2000 -> 2010

tract_pop_0509_rel <- left_join(rel_2000_2010, tract_pop_05_09, by = c("GEOID00_c" = "GEOID")) %>% 
  filter(TRACT00 != "000000" & str_sub(GEOID00, 1, 2) == "42") %>%  # removing tracts with 0 population in 2000 (all water) and non-PA 2000 population
  dplyr::select(GEOID00, GEOID10, overlap_00_10, starts_with("B01"), -c(ends_with("M"))) %>% 
  st_drop_geometry() %>% 
  mutate(across(starts_with("B01"), ~ .x * (overlap_00_10 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
  dplyr::select(GEOID00, GEOID10, overlap_00_10, starts_with("B01"))

tract_0509pop_10geo <- tract_pop_0509_rel %>% 
  group_by(GEOID10) %>% 
  summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))

# 2010 -> 2020

tract_0509pop_10geo$GEOID10_c <- as.character(tract_0509pop_10geo$GEOID10)

tract_pop_10_rel <- left_join(tract_0509pop_10geo, rel_2010_2020, by = c("GEOID10_c" = "GEOID_TRACT_10")) %>% 
  dplyr::select(GEOID10, GEOID_TRACT_20, overlap_10_20, ends_with("_sum")) %>% 
  st_drop_geometry() %>% 
  mutate(across(ends_with("_sum"), ~ .x * (overlap_10_20 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
  dplyr::select(GEOID10, GEOID_TRACT_20, overlap_10_20, ends_with("_wtd"))

tract_0509pop_20geo <- tract_pop_10_rel %>% 
  filter(str_sub(GEOID_TRACT_20, 1, 2) == "42") %>% 
  group_by(GEOID_TRACT_20) %>% 
  summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))

# quality check to see if the expected # of total pop is the same in the aggregated data
# 17 difference. Checked and not due to rounding. Likely due to census tract now overlapping with a neighboring state.
sum(distinct(dplyr::select(tract_pop_0509_rel, GEOID00, B01001_001E))[,"B01001_001E"], na.rm = TRUE)-sum(tract_0509pop_20geo$B01001_001E_wtd_sum_wtd_sum, na.rm = TRUE)

# how many census tracts? 3445. This matches the num of census tracts in 2020.
# nrow(tract_0509pop_20geo)

# reducing to census tracts in study counties
tract_0509pop_20geo_studyarea <- tract_0509pop_20geo %>% 
  filter(str_sub(GEOID_TRACT_20, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"))

save(tract_0509pop_20geo_studyarea, file = "00_data/tract_0509pop_20geo_studyarea.Rdata")



# 2006 - 2010

tract_pop_0610_rel <- left_join(rel_2010_2020, tract_pop_06_10, by = c("GEOID_TRACT_10" = "GEOID")) %>% 
  filter(str_sub(GEOID_TRACT_10, 1, 2) == "42") %>%  # removing non-PA population
  dplyr::select(GEOID_TRACT_10, GEOID_TRACT_20, overlap_10_20, starts_with("B01"), -c(ends_with("M"))) %>% 
  st_drop_geometry() %>% 
  mutate(across(starts_with("B01"), ~ .x * (overlap_10_20 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
  dplyr::select(GEOID_TRACT_10, GEOID_TRACT_20, overlap_10_20, starts_with("B01"))

tract_0610pop_20geo <- tract_pop_0610_rel %>% 
  filter(str_sub(GEOID_TRACT_20, 1, 2) == "42") %>%
  group_by(GEOID_TRACT_20) %>% 
  summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))

# quality check to see if the expected # of total pop is the same in the aggregated data
# 36 difference. Checked and not due to rounding. Likely due to census tract now overlapping with a neighboring state.
sum(distinct(dplyr::select(tract_pop_0610_rel, GEOID_TRACT_10, B01001_001E))[,"B01001_001E"], na.rm = TRUE)-sum(tract_0610pop_20geo$B01001_001E_wtd_sum, na.rm = TRUE)

# reducing to census tracts in study counties
tract_0610pop_20geo_studyarea <- tract_0610pop_20geo %>% 
  filter(str_sub(GEOID_TRACT_20, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"))

save(tract_0610pop_20geo_studyarea, file = "00_data/tract_0610pop_20geo_studyarea.Rdata")

# 2007-2011 through 2015-2019

pop_fun <- function(pop, ds){

  df1 <- left_join(rel_2010_2020, pop, by = c("GEOID_TRACT_10" = "GEOID")) %>% 
    filter(str_sub(GEOID_TRACT_10, 1, 2) == "42") %>%  # removing non-PA population
    dplyr::select(GEOID_TRACT_10, GEOID_TRACT_20, overlap_10_20, starts_with("B01"), -c(ends_with("M"))) %>% 
    st_drop_geometry() %>% 
    mutate(across(starts_with("B01"), ~ .x * (overlap_10_20 / 100), .names = "{.col}_wtd")) %>% # creating weighted population variable for each 00 x 10 combination
    dplyr::select(GEOID_TRACT_10, GEOID_TRACT_20, overlap_10_20, starts_with("B01"))
  
  df2 <- df1 %>% 
    filter(str_sub(GEOID_TRACT_20, 1, 2) == "42") %>%
    group_by(GEOID_TRACT_20) %>% 
    summarise(across(ends_with("_wtd"), ~ round(sum(.x),0), .names = "{.col}_sum"))
  
  df3 <- df2 %>% 
    filter(str_sub(GEOID_TRACT_20, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107"))
  
  assign(ds, df3, envir = .GlobalEnv)
}
  
pop_fun(tract_pop_07_11, "tract_0711pop_20geo_studyarea")
pop_fun(tract_pop_08_12, "tract_0812pop_20geo_studyarea")
pop_fun(tract_pop_09_13, "tract_0913pop_20geo_studyarea")
pop_fun(tract_pop_10_14, "tract_1014pop_20geo_studyarea")
pop_fun(tract_pop_11_15, "tract_1115pop_20geo_studyarea")
pop_fun(tract_pop_12_16, "tract_1216pop_20geo_studyarea")
pop_fun(tract_pop_13_17, "tract_1317pop_20geo_studyarea")
pop_fun(tract_pop_14_18, "tract_1418pop_20geo_studyarea")
pop_fun(tract_pop_15_19, "tract_1519pop_20geo_studyarea")

# 2016-2020
tract_1620pop_20geo_studyarea <- tract_pop_16_20 %>% 
  filter(str_sub(GEOID, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107")) %>% 
  dplyr::select(GEOID, starts_with("B01"), -c(ends_with("M"))) %>% 
  st_drop_geometry()

# 2017-2021  
tract_1721pop_20geo_studyarea <- tract_pop_17_21 %>% 
  filter(str_sub(GEOID, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107")) %>% 
  dplyr::select(GEOID, starts_with("B01"), -c(ends_with("M"))) %>% 
  st_drop_geometry()

# 2018-2022
tract_1822pop_20geo_studyarea <- tract_pop_18_22 %>% 
  filter(str_sub(GEOID, 3, 5) %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107")) %>% 
  dplyr::select(GEOID, starts_with("B01"), -c(ends_with("M"))) %>% 
  st_drop_geometry()


# saving data

save(tract_0610pop_20geo_studyarea, file = "00_data/tract_0610pop_20geo_studyarea.Rdata")
save(tract_0711pop_20geo_studyarea, file = "00_data/tract_0711pop_20geo_studyarea.Rdata")
save(tract_0812pop_20geo_studyarea, file = "00_data/tract_0812pop_20geo_studyarea.Rdata")
save(tract_0913pop_20geo_studyarea, file = "00_data/tract_0913pop_20geo_studyarea.Rdata")
save(tract_1014pop_20geo_studyarea, file = "00_data/tract_1014pop_20geo_studyarea.Rdata")
save(tract_1115pop_20geo_studyarea, file = "00_data/tract_1115pop_20geo_studyarea.Rdata")
save(tract_1216pop_20geo_studyarea, file = "00_data/tract_1216pop_20geo_studyarea.Rdata")
save(tract_1317pop_20geo_studyarea, file = "00_data/tract_1317pop_20geo_studyarea.Rdata")
save(tract_1418pop_20geo_studyarea, file = "00_data/tract_1418pop_20geo_studyarea.Rdata")
save(tract_1519pop_20geo_studyarea, file = "00_data/tract_1519pop_20geo_studyarea.Rdata")
save(tract_1620pop_20geo_studyarea, file = "00_data/tract_1620pop_20geo_studyarea.Rdata")
save(tract_1721pop_20geo_studyarea, file = "00_data/tract_1721pop_20geo_studyarea.Rdata")
save(tract_1822pop_20geo_studyarea, file = "00_data/tract_1822pop_20geo_studyarea.Rdata")





# attaching to municipality

mun_red$area.mun <- st_area(st_make_valid(sf::st_as_sf(mun_red)))

# 2000
tract_00pop_20geo_studyarea_geo <- left_join(tract_00pop_20geo_studyarea, tract20 %>% dplyr::select(GEOID, geometry), by = c("GEOID_TRACT_20" = "GEOID"))

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

mun_fun <- function(df_in, df_out){
  df_geo <- left_join(df_in, tract20 %>% dplyr::select(GEOID, geometry), by = c("GEOID_TRACT_20" = "GEOID"))
  
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

mun_fun(tract_0509pop_20geo_studyarea, "mun_0509pop")
mun_fun(tract_0610pop_20geo_studyarea, "mun_0610pop")
mun_fun(tract_0711pop_20geo_studyarea, "mun_0711pop")
mun_fun(tract_0812pop_20geo_studyarea, "mun_0812pop")
mun_fun(tract_0913pop_20geo_studyarea, "mun_0913pop")
mun_fun(tract_1014pop_20geo_studyarea, "mun_1014pop")
mun_fun(tract_1115pop_20geo_studyarea, "mun_1115pop")
mun_fun(tract_1216pop_20geo_studyarea, "mun_1216pop")
mun_fun(tract_1317pop_20geo_studyarea, "mun_1317pop")
mun_fun(tract_1418pop_20geo_studyarea, "mun_1418pop")
mun_fun(tract_1519pop_20geo_studyarea, "mun_1519pop")

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



mun_0509pop <- mun_0509pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum_wtd_sum"), .cols = starts_with("B0"))

mun_0610pop <- mun_0610pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_0711pop <- mun_0711pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_0812pop <- mun_0812pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_0913pop <- mun_0913pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1014pop <- mun_1014pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1115pop <- mun_1115pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1216pop <- mun_1216pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1317pop <- mun_1317pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1418pop <- mun_1418pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))
mun_1519pop <- mun_1519pop %>% ungroup() %>% rename_with(~str_remove(.x, "_wtd_sum"), .cols = starts_with("B0"))

# renaming 2000 decennial census column names to match ACS columns for age by sex calculations
colnames(mun_00pop) <- colnames(mun_0509pop)

# adding variables for survey and year
mun_00pop <- mun_00pop %>% mutate(survey = "decennial", year = "2000")
mun_0509pop <- mun_0509pop %>% mutate(survey = "acs", year = "2007")
mun_0610pop <- mun_0610pop %>% mutate(survey = "acs", year = "2008")
mun_0711pop <- mun_0711pop %>% mutate(survey = "acs", year = "2009")
mun_0812pop <- mun_0812pop %>% mutate(survey = "acs", year = "2010")
mun_0913pop <- mun_0913pop %>% mutate(survey = "acs", year = "2011")
mun_1014pop <- mun_1014pop %>% mutate(survey = "acs", year = "2012")
mun_1115pop <- mun_1115pop %>% mutate(survey = "acs", year = "2013")
mun_1216pop <- mun_1216pop %>% mutate(survey = "acs", year = "2014")
mun_1317pop <- mun_1317pop %>% mutate(survey = "acs", year = "2015")
mun_1418pop <- mun_1418pop %>% mutate(survey = "acs", year = "2016")
mun_1519pop <- mun_1519pop %>% mutate(survey = "acs", year = "2017")
mun_1620pop <- mun_1620pop %>% mutate(survey = "acs", year = "2018")
mun_1721pop <- mun_1620pop %>% mutate(survey = "acs", year = "2019")
mun_1822pop <- mun_1620pop %>% mutate(survey = "acs", year = "2020")

mun_pop_yr <- rbind(mun_00pop, mun_0509pop, mun_0610pop, mun_0711pop, mun_0812pop, mun_0913pop, mun_1014pop, mun_1115pop, mun_1216pop, mun_1317pop, mun_1418pop, mun_1519pop, mun_1620pop, mun_1721pop, mun_1822pop) %>% 
  mutate(

        # male
        m18_19 = B01001_007E, 
        m20_24 = B01001_008E+B01001_009E+B01001_010E, 
        m25_29 = B01001_011E,
        m30_34 = B01001_012E,
        m35_39 = B01001_013E,
        m40_44 = B01001_014E,
        m45_49 = B01001_015E,
        m50_54 = B01001_016E,
        m55_59 = B01001_017E,
        m60_64 = B01001_018E+B01001_019E,
        m65_69 = B01001_020E+B01001_021E,
        m70_74 = B01001_022E,
        m75_79 = B01001_023E,
        m80_84 = B01001_024E,
        m85 = B01001_025E,
        
        # female
        f18_19 = B01001_031E,
        f20_24 = B01001_032E+B01001_033E+B01001_034E,
        f25_29 = B01001_035E,
        f30_34 = B01001_036E,
        f35_39 = B01001_037E,
        f40_44 = B01001_038E,
        f45_49 = B01001_039E,
        f50_54 = B01001_040E,
        f55_59 = B01001_041E,
        f60_64 = B01001_042E+B01001_043E,
        f65_69 = B01001_044E+B01001_045E,
        f70_74 = B01001_046E,
        f75_79 = B01001_047E,
        f80_84 = B01001_048E,
        f85 = B01001_049E,

        # totals
        total_pop = B01001_001E

) %>% 
  dplyr::select(-c(starts_with("B0")))

mun_pop_yr$total_pop_adult_f <- rowSums(dplyr::select(mun_pop2, starts_with("f", ignore.case = FALSE)))
mun_pop_yr$total_pop_adult_m <- rowSums(dplyr::select(mun_pop2, starts_with("m", ignore.case = FALSE)))
mun_pop_yr$total_pop_adult <- mun_pop2$total_pop_adult_f + mun_pop2$total_pop_adult_m


mun_pop_agg <- mun_pop_yr %>% st_drop_geometry() %>% 
  group_by(COUNTY, MUNICIPAL_, MUNICIPAL1) %>% 
  summarise(across(.cols = c(starts_with("m", ignore.case = FALSE), starts_with("f", ignore.case = FALSE), starts_with("total")), ~ round(mean(.x), 0)))


save(mun_pop_yr, file = "00_data/mun_pop_yr.Rdata")
save(mun_pop_agg, file = "00_data/mun_pop_agg.Rdata")














