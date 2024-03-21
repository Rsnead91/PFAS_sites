# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','magick','units','sf','ggmap','biscale','reshape2','janitor','ggpattern','tidygeocoder','lubridate', 'openxlsx','spdep','cowplot')

options(scipen = 999)

# 2. data ----

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
#                              table = "P012",               
                              year = 2000,
                              state = "42",
                              geometry = TRUE,
                              keep_geo_vars = TRUE,     
                              sumfile = "sf1",              
                              output = "wide",
                              cache_table = TRUE
)

tract_pop_00 <- get_acs(geography = "tract", 
                           table = "B01001",               
                           year = 2010,
                           state = "PA",
                           geometry = TRUE,
                           keep_geo_vars = TRUE,     
                           survey = "acs5",              
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








vars <- load_variables(2016, "acs5", cache = TRUE)



var_names <- c("med_hhincome", "percap_income", "not_pubassist", "pop_pubassist", "below_pov", "pop_pov", "edu_bach_male", "edu_bach_female", "pop_edu", "housing_all_units", "housing_occ_units", "renter_housing", "owner_housing",  "vacant_housing", "med_rent", "units_mortgage", "mortgage_pop", "nhw", "pov_200", "pop_pov_200", "unemployed", "pop_labor", "hs_edu_male", "hs_edu_female", "owner_veh", "renter_veh", "veh_pop", "foreign_born", "pop_foreign", "nhb", "race_pop", paste0("male_bluecollar", 1:4), paste0("female_bluecollar", 1:4), "pop_bluecollar", "pop_own_rent", "gini", paste0("male_bfb", 1:2), paste0("female_bfb", 1:2), "public_assist", "us_born", paste0("us_bornblk_m", 1:2), paste0("us_bornblk_f", 1:2), "blk_nativity_pop", "all_black", "raceall_pop", paste0("edu_hsM", c(1:9)), paste0("edu_bachplusM", c(1:4)), paste0("edu_hsF", c(1:9)), paste0("edu_bachplusF", c(1:4)))



tables <-c(
  
"B01001",
"B02001", 
"B03002", 
"B05002", 
"B05003B", 
"B09010", 
"B14002", 
"B15002", 
"B17001", 
"B19013", 
"B19083", 
"B19301", 
"B25002", 
"B25003", 
"B23025", 
"B25045", 
"B25064", 
"B25081", 
"C24010", 
"C27016"






acs_raw_newvars <- ds.acs_raw %>%
  mutate(

    total_pop = B01001001,

    age_45_74 = sum(B01001015, B01001016, B01001017, B01001018, B01001019, B01001020, B01001021, B01001022, B01001039, B01001040, B01001041, B01001042, B01001043, B01001044, B01001045, B01001046),

    age_ge65 = sum(B01001020, B01001021, B01001022, B01001023, B01001024, B01001025, B01001044, B01001045, B01001046, B01001047, B01001048, B01001049) / B01001001,

    male = B01001002 / B01001001,
    female = B01001026 / B01001001,

    # Ethnicity
    hisp = B03002012 / B03002001,
    
    # School enrollment (3+)
    noenrolledinschool_3plus = (B14002025 + B14002049) / B14002001,
    
    # Educational Attainment (25+)
    nohsgrad_25plus = (B15002003 + B15002004 + B15002005 + B15002006 + B15002007 + B15002008 + B15002009 + B15002010 +
                         B15002020 + B15002021 + B15002022 + B15002023 + B15002024 + B15002025 + B15002026 + B15002027) / B15002001,
    
    # Employment status
    unemployed = (B23007008 + B23007010 + B23007013 + B23007018 + B23007024 + B23007029 + B23007037 + B23007042 + B23007047 + B23007053 + B23007058) /
      (B23007031 + B23007002),
    
    # Median Household income
    med_houseincome = B19013001,
    
    # Public assistance
    households_w_publicassist = B19057002 / B19057001,
    
    # Social security income
    socsecinsur = B19055002 / B19055001,
    
    # Poverty status
    belowpovertyline = B17017002 / B17017001,
    
    # Poverty status: Household 45+
    belowpovertyline45p = (B17017007 + B17017008 + B17017013 + B17017014 + B17017018 + B17017019 + B17017024 + B17017025 + B17017029 + B17017030) / B17017001,
    
    # Familes with kids under poverty line
    famkids_poverty = (B17010004 + B17010011 + B17010017) / B17010001,
    
    # Households with income below poverty line and householder 45+
    householder45plus_poverty = (B17017007 + B17017008 + B17017013 + B17017014 + B17017018 + B17017019 + B17017024 + B17017025 + B17017029 + B17017030) / B17017001,
    
    householder45plus_poverty2 = (B17017007 + B17017008 + B17017013 + B17017014 + B17017018 + B17017019 + B17017024 + B17017025 + B17017029 + B17017030) / B17017002,
    
    # Per capita income
    percapita_income = B19301001,
    
    # Gini index
    gini = B19083001,
    
    # Citizenship
    not_citizen = B26110011 / B26110001,
    foreign_born = B26110005 / B26110001,
    
    # Language spoken at home
    foreign_lang = (B16004004 + B16004009 + B16004014 + B16004019) / B16004001,
    
    # Travel time to work
    commute_60minplus = (B08303012 + B08303013) / B08303001,
    
    # Means of Transportation: Workers 16+
    publtrans_bike_walk = (B08301010 + B08301018 + B08301019) / B08301001,
    
    # Veteran status
    veteran = B21001002 / B21001001,
    
    # Households
    nonfam_households = B11001007 / B11001001,
    
    # Single parents with kids
    singledad = B11003010 / B11003001,
    singlemom = B11003016 / B11003001,
    
    # Housing Units
    house_units = B25001001,
    
    # Occupancy Status
    renter_occupied = B25003003 / B25003001,
    
    # Vacant Housing Units
    vacant = B25002003 / B25002001,
    
    # Mortgage Status
    no_mortgage = B25081008 / B25081001,
    
    # Occupants per room
    occupantsperroom = (B25014006 + B25014007 + B25014012 + B25014013) / B25014001,
    
    # Telephone Service Available
    no_telephone = (B25043007 + B25043016) / B25043001,
    
    # Vehicles Available
    no_vehicle = (B25044003 + B25044010) / B25044001,
    
    # House heating fuel
    no_heat = B25040010 / B25040001,
    
    # Plumbing facilities
    lack_plumbing = B25047003 / B25047001,
    
    # Kitchen facilities
    lack_kitchen = B25051003 / B25051001,
    
    # Median Year Structure Built
    med_yearbuilt = B25035001
  )
# Note: For simplification, row-wise operations and complex conditions were not included. 
# Adjustments might be necessary based on your specific dataset and needs.
