# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','units','sf','ggmap','biscale','reshape2','janitor','ggpattern','tidygeocoder','lubridate', 'openxlsx','spdep','cowplot')

options(scipen = 999)

# 2. load data ----

# study area counties

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
  filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")) %>% 
  st_set_crs(st_crs(4269))

# cancer data at the municipality-level

cancer <- read_csv("00_data/Cancer Incidence_00-20.csv")

# municipalities included in the study
load("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/mun.final.Rdata")

mun.shp <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennDOT/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) %>% # set projection to match other geo files
  dplyr::filter(FIPS_COUNT %in% c("011", "017", "025", "029", "045", "071", "075", "077", "089", "091", "095", "107")) # only keeping 12 study area counties

st_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/mun.final.shp")

mun_red <- left_join(st_drop_geometry(mun.final) %>% dplyr::select(COUNTY, MUNICIPAL_, MUNICIPAL1), mun.shp %>% dplyr::select(COUNTY, MUNICIPAL_, MUNICIPAL1, FIPS_MUN_C), by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1"))

mun_red <- mutate(mun_red,
                  county_fips = case_when(
                   COUNTY == "06" ~ "11",
                   COUNTY == "09" ~ "17",
                   COUNTY == "13" ~ "25",
                   COUNTY == "15" ~ "29",
                   COUNTY == "23" ~ "45",
                   COUNTY == "36" ~ "71",
                   COUNTY == "38" ~ "75",
                   COUNTY == "39" ~ "77",
                   COUNTY == "45" ~ "89",
                   COUNTY == "46" ~ "91",
                   COUNTY == "48" ~ "95",
                   COUNTY == "53" ~ "107"
                  )
)

mun_red$mun_fips <- as.double(str_c(mun_red$county_fips,mun_red$FIPS_MUN_C))

# population data by municipality

mun_pop <- read_csv("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/mun_pop.csv") %>% 
  dplyr::select(-geoms)

mun_pop$total_pop <- round(rowSums(mun_pop[,4:33]),0)
mun_pop$total_pop_m <- round(rowSums(mun_pop[,4:18]),0)
mun_pop$total_pop_f <- round(rowSums(mun_pop[,19:33]),0)

# quality check
# mun_pop$total_pop_diff <- mun_pop$total_pop-mun_pop$total_pop_m
# mun_pop$total_pop_sum <- mun_pop$total_pop_f+mun_pop$total_pop_m
# mun_pop <- dplyr::select(mun_pop, -c(total_pop_diff,total_pop_sum))

mun_pop$MUNICIPAL_ <- as.character(mun_pop$MUNICIPAL_)
  
mun_red <- left_join(mun_red, mun_pop %>% dplyr::select(COUNTY, MUNICIPAL_, MUNICIPAL1, total_pop, total_pop_m, total_pop_f), by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1"))

# pfas data

pfas <- read_csv("00_data/pa_pfas_sampling_wide.csv") %>% 
  mutate(
    pfas = case_when(
      pfas_detected_4cat == "PFAS not detected" ~ "0",
      pfas_detected_4cat == "PFAS detected at 0 or less than MRL" ~ "1",
      pfas_detected_4cat == "PFAS detected above 0 but below MRL" ~ "1",
      pfas_detected_4cat == "PFAS detected at or above MRL" ~ "2"
    )
  ) %>% 
  dplyr::select(pws_id, pfas)



# 3. review data ----

# str(cancer)
table(cancer$PrimarySite)
# table(cancer$County)
# table(cancer$Year)
# head(cancer)
# view(cancer)

# data includes:
# cancer site, county, municipality, year, sex (total, male, female), invasivecount, municipality fips code
# 274,000 observations, a row per site-municipality-sex-year combination
# 12 types of cancer

# 4. create new data ----

# need the aggregate data by cancer for the entire time period
# separate data by sex/total
# for now, only need kidney and thyroid
# reduce data to only municipalities in the study

cancer$len <- stringr::str_length(cancer$FIPSCode)  

cancer$FIPSCode2 <- ifelse(
  cancer$len == 8, str_sub(cancer$FIPSCode, 2),
  cancer$FIPSCode)

mun_red$mun_fips_c <- as.character(mun_red$mun_fips)

cancer_red <- left_join(mun_red %>% dplyr::select(mun_fips_c, total_pop, total_pop_m, total_pop_f, geoms), cancer, by = c("mun_fips_c" = "FIPSCode2")) %>% 
  mutate(
    primary_site = case_when(
      PrimarySite == "Brain and Other Nervous System" ~ "Brain",
      PrimarySite == "Breast" ~ "Breast",
      PrimarySite == "Cervix Uteri (F)" ~ "Cervical",
      PrimarySite == "Colon and Rectum" ~ "Colorectal",
      PrimarySite == "Corpus and Uterus, NOS (F)" ~ "Endometrial",
      PrimarySite == "Kidney and Renal Pelvis" ~ "Kidney",
      PrimarySite == "Liver and Intrahepatic Bile Duct" ~ "Liver",
      PrimarySite == "Non-Hodgkin Lymphoma" ~ "Non-hodgkin lymphoma",
      PrimarySite == "Ovary (F)" ~ "Ovarian",
      PrimarySite == "Prostate (M)" ~ "Prostate",
      PrimarySite == "Testis (M)" ~ "Testicular",
      PrimarySite == "Thyroid" ~ "Thyroid",
      PrimarySite == "Urinary Bladder" ~ "Bladder"
    )
  ) %>% 
  dplyr::select(-c(FIPSCode, len, MCD, PrimarySite)) %>% 
  dplyr::select(County, mun_fips_c, Year, primary_site, Sex, InvasiveCount, total_pop, total_pop_m, total_pop_f, geoms)
  
rm(cancer)

## 4.1 aggregate and cancer-specific data ---- 

ids <- distinct(cancer_red, mun_fips_c, primary_site, County, geoms)

cancer_agg <- cancer_red %>% 
  group_by(mun_fips_c, primary_site, Sex) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, InvasiveCount)) %>% 
  distinct()

# rm(cancer_agg_wide)

cancer_agg_t <- cancer_agg %>% 
  filter(Sex == "Total") %>% 
  dplyr::select(-c(total_pop_m, total_pop_f, Sex, geoms, County)) %>% 
  st_drop_geometry()

cancer_agg_m <- cancer_agg %>% 
  filter(Sex == "Male") %>% 
  dplyr::select(-c(Sex, County, total_pop, total_pop_f, geoms)) %>% 
  rename(incidence_m = incidence) %>% 
  st_drop_geometry()

cancer_agg_f <- cancer_agg %>% 
  filter(Sex == "Female") %>% 
  dplyr::select(-c(Sex, County, total_pop, total_pop_m, geoms)) %>% 
  rename(incidence_f = incidence) %>% 
  st_drop_geometry()

cancer_agg_wide <- left_join(
                    left_join(
                      left_join(
                        ids,
                        cancer_agg_t,
                        by = c("mun_fips_c", "primary_site")
                      ),
                      cancer_agg_m,
                      by = c("mun_fips_c", "primary_site")
                    ),
                    cancer_agg_f,
                    by = c("mun_fips_c", "primary_site")
                  )

cancer_agg_wide <- cancer_agg_wide[,c("County", "mun_fips_c", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms")]

# cancer_agg_wide$incd_rt100000_m <- (cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m)*100000
# cancer_agg_wide$incd_rt100000_f <- (cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f)*100000

# calculating the expected number of cases over a TWENTY YEAR period
# assuming the rate is constant by using the 2023 case rate for each cancer
# applied the adjustment to the 20-year aggregate. not year-by-year and then aggregating
cancer_agg_wide$exp_m <- case_when(
  cancer_agg_wide$primary_site == "Bladder" ~ cancer_agg_wide$total_pop_m*((31.7/100000)*20),
  cancer_agg_wide$primary_site == "Brain" ~ cancer_agg_wide$total_pop_m*((7.4/100000)*20),
  cancer_agg_wide$primary_site == "Breast" ~ 0,
  cancer_agg_wide$primary_site == "Cervical" ~ 0,
  cancer_agg_wide$primary_site == "Colorectal" ~ cancer_agg_wide$total_pop_m*((42.1/100000)*20),
  cancer_agg_wide$primary_site == "Endometrial" ~ 0,
  cancer_agg_wide$primary_site == "Kidney" ~ cancer_agg_wide$total_pop_m*((23.5/100000)*20),
  cancer_agg_wide$primary_site == "Liver" ~ cancer_agg_wide$total_pop_m*((14.1/100000)*20),
  cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ cancer_agg_wide$total_pop_m*((22.5/100000)*20),
  cancer_agg_wide$primary_site == "Ovarian" ~ 0,
  cancer_agg_wide$primary_site == "Prostate" ~ cancer_agg_wide$total_pop_m*((113.4/100000)*20),
  cancer_agg_wide$primary_site == "Testicular" ~ cancer_agg_wide$total_pop_m*((6.0/100000)*20),
  cancer_agg_wide$primary_site == "Thyroid" ~ cancer_agg_wide$total_pop_m*((7.4/100000)*20)
)

cancer_agg_wide$exp_f <- case_when(
  cancer_agg_wide$primary_site == "Bladder" ~ cancer_agg_wide$total_pop_f*((7.8/100000)*20),
  cancer_agg_wide$primary_site == "Brain" ~ cancer_agg_wide$total_pop_f*((5.2/100000)*20),
  cancer_agg_wide$primary_site == "Breast" ~ cancer_agg_wide$total_pop_f*((126.9/100000)*20),
  cancer_agg_wide$primary_site == "Cervical" ~ cancer_agg_wide$total_pop_f*((7.7/100000)*20),
  cancer_agg_wide$primary_site == "Colorectal" ~ cancer_agg_wide$total_pop_f*((32.0/100000)*20),
  cancer_agg_wide$primary_site == "Endometrial" ~ cancer_agg_wide$total_pop_f*((27.6/100000)*20),
  cancer_agg_wide$primary_site == "Kidney" ~ cancer_agg_wide$total_pop_f*((11.7/100000)*20),
  cancer_agg_wide$primary_site == "Liver" ~ cancer_agg_wide$total_pop_f*((5.2/100000)*20),
  cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ cancer_agg_wide$total_pop_f*((15.5/100000)*20),
  cancer_agg_wide$primary_site == "Ovarian" ~ cancer_agg_wide$total_pop_f*((10.3/100000)*20),
  cancer_agg_wide$primary_site == "Prostate" ~ 0,
  cancer_agg_wide$primary_site == "Testicular" ~ 0,
  cancer_agg_wide$primary_site == "Thyroid" ~ cancer_agg_wide$total_pop_f*((20.2/100000)*20)
)

# bladder cancer rates per 100,000 - Male: 31.7, Female: 7.8 (https://seer.cancer.gov/statfacts/html/urinb.html)
# brain cancer rates per 100,000 - Male: 7.4, Female: 5.2 (https://seer.cancer.gov/statfacts/html/brain.html)
# breast cancer rates per 100,000 - Female: 126.9 (https://seer.cancer.gov/statfacts/html/breast.html)
# cervical cancer rates per 100,000 - Female: 7.7 (https://seer.cancer.gov/statfacts/html/cervix.html)
# colorectal cancer rates per 100,000 - Male: 42.1, Female: 32.0 (https://seer.cancer.gov/statfacts/html/colorect.html)
# endometrial cancer rates per 100,000 - Female: 27.6 (https://seer.cancer.gov/statfacts/html/corp.html)
# kidney cancer rates per 100,000 - Male: 23.5, Female: 11.7 (https://seer.cancer.gov/statfacts/html/kidrp.html)
# liver cancer rates per 100,000 - Male: 14.1, Female: 5.2 (https://seer.cancer.gov/statfacts/html/livibd.html)
# non-hodgkin lymphoma cancer rates per 100,000 - Male: 22.5, Female: 15.5 (https://seer.cancer.gov/statfacts/html/nhl.html)
# ovarian cancer rates per 100,000 - Female: 10.3 (https://seer.cancer.gov/statfacts/html/ovary.html)
# prostate cancer rates per 100,000 - Male: 113.4 (https://seer.cancer.gov/statfacts/html/prost.html)
# testicular cancer rates per 100,000 - Male: 6.0 (https://seer.cancer.gov/statfacts/html/testis.html)
# thyroid cancer rates per 100,000 - Male: 7.4, Female: 20.2 (https://seer.cancer.gov/statfacts/html/thyro.html)

# smr_bladder <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Bladder", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Bladder", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Bladder", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Bladder", "exp_f"]))
# smr_brain <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Brain", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Brain", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Brain", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Brain", "exp_f"]))
# smr_breast <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Breast", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Breast", "exp_f"]))
# smr_cervical <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Cervical", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Cervical", "exp_f"]))
# smr_colorectal <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Colorectal", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Colorectal", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Colorectal", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Colorectal", "exp_f"]))
# smr_endometrial <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Endometrial", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Endometrial", "exp_f"]))
# smr_kidney <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Kidney", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Kidney", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Kidney", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Kidney", "exp_f"]))
# smr_liver <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Liver", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Liver", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Liver", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Liver", "exp_f"]))
# smr_nonhodgkin_lymphoma <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Non-hodgkin lymphoma", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Non-hodgkin lymphoma", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Non-hodgkin lymphoma", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Non-hodgkin lymphoma", "exp_f"]))
# smr_ovarian <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Ovarian", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Ovarian", "exp_f"]))
# smr_prostate <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Prostate", "incidence_m"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Prostate", "exp_m"]))
# smr_testicular <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Testicular", "incidence_m"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Testicular", "exp_m"]))
# smr_thyroid <- sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Thyroid", "incidence_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Thyroid", "incidence_f"]))/sum((cancer_agg_wide[cancer_agg_wide$primary_site == "Thyroid", "exp_m"] + cancer_agg_wide[cancer_agg_wide$primary_site == "Thyroid", "exp_f"]))


# cancer_agg_wide$smr <- case_when(
#   cancer_agg_wide$primary_site == "Bladder" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Brain" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Breast" ~ cancer_agg_wide$incidence_f/cancer_agg_wide$exp_f,
#   cancer_agg_wide$primary_site == "Cervical" ~ cancer_agg_wide$incidence_f/cancer_agg_wide$exp_f,
#   cancer_agg_wide$primary_site == "Colorectal" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Endometrial" ~ cancer_agg_wide$incidence_f/cancer_agg_wide$exp_f,
#   cancer_agg_wide$primary_site == "Kidney" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Liver" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f),
#   cancer_agg_wide$primary_site == "Ovarian" ~ cancer_agg_wide$incidence_f/cancer_agg_wide$exp_f,
#   cancer_agg_wide$primary_site == "Prostate" ~ cancer_agg_wide$incidence_m/cancer_agg_wide$exp_m,
#   cancer_agg_wide$primary_site == "Testicular" ~ cancer_agg_wide$incidence_m/cancer_agg_wide$exp_m,
#   cancer_agg_wide$primary_site == "Thyroid" ~ (cancer_agg_wide$incidence_m + cancer_agg_wide$incidence_f)/(cancer_agg_wide$exp_m +cancer_agg_wide$exp_f)
# )

# cancer_agg_wide$sex_adj_incdrt1000 <- case_when(
#   cancer_agg_wide$primary_site == "Bladder" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Brain" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Breast" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Cervical" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Colorectal" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Endometrial" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Kidney" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Liver" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Ovarian" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Prostate" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Testicular" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m)*cancer_agg_wide$smr)*1000,
#   cancer_agg_wide$primary_site == "Thyroid" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop)*cancer_agg_wide$smr)*1000
# )

cancer_agg_wide$crude_incdrt100000 <- case_when(
  cancer_agg_wide$primary_site == "Bladder" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Brain" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Breast" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Cervical" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Colorectal" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Endometrial" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Kidney" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Liver" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000,
  cancer_agg_wide$primary_site == "Ovarian" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Prostate" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Testicular" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Thyroid" ~ ((cancer_agg_wide$incidence/cancer_agg_wide$total_pop))*100000
)

# what is this population?

















## 4.2 yearly and cancer-specific data ---- 



# THYROID

# total
# thyroid_agg_tot <- cancer_red %>% 
#   filter(PrimarySite == "Thyroid" & Sex == "Total") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   unique() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(thyroid_agg_tot$incidence)/sum(thyroid_agg_tot$total_pop)
# 
# thyroid_agg_tot$exp <- round(thyroid_agg_tot$total_pop*overall_rate,0)

# male
# thyroid_agg_m <- cancer_red %>% 
#   filter(PrimarySite == "Thyroid" & Sex == "Male") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(thyroid_agg_m$incidence)/sum(thyroid_agg_m$total_pop)
# 
# thyroid_agg_m$exp <- round(thyroid_agg_m$total_pop*overall_rate,0)

# female
# thyroid_agg_f <- cancer_red %>% 
#   filter(PrimarySite == "Thyroid" & Sex == "Female") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(thyroid_agg_f$incidence)/sum(thyroid_agg_f$total_pop)
# 
# thyroid_agg_f$exp <- round(thyroid_agg_f$total_pop*overall_rate,0)

# KIDNEY

# total
# kidney_agg_tot <- cancer_red %>% 
#   filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Total") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(kidney_agg_tot$incidence)/sum(kidney_agg_tot$total_pop)
# 
# kidney_agg_tot$exp <- round(kidney_agg_tot$total_pop*overall_rate,0)

# male
# kidney_agg_m <- cancer_red %>% 
#   filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Male") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(kidney_agg_m$incidence)/sum(kidney_agg_m$total_pop)
# 
# kidney_agg_m$exp <- round(kidney_agg_m$total_pop*overall_rate,0)

# female
# kidney_agg_f <- cancer_red %>% 
#   filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Female") %>% 
#   group_by(mun_fips_c) %>% 
#   mutate(incidence = sum(InvasiveCount)) %>% 
#   ungroup() %>% 
#   dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
#   mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
#   distinct()
# 
# overall_rate <- sum(kidney_agg_f$incidence)/sum(kidney_agg_f$total_pop)
# 
# kidney_agg_f$exp <- round(kidney_agg_f$total_pop*overall_rate,0)
# 
# 5. local morans i ----

# THYROID

thyroid <- filter(cancer_agg_wide, primary_site == "Thyroid")
nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(thyroid)), queen = TRUE), zero.policy = TRUE)
thyroid_lmi <- localmoran(thyroid$incidence_f, nbr, zero.policy = TRUE)
thyroid_lmi_geo <- cbind(thyroid,thyroid_lmi)
thyroid_lmi_geo$cluster <- attributes(thyroid_lmi)$quadr$mean
colnames(thyroid_lmi_geo) <- c("county", "mun_fips", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms", "exp_m", "exp_f", "crude_incdrt100000", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")
thyroid_lmi_geo$cluster_sig <- ifelse(thyroid_lmi_geo$p_val < .05, thyroid_lmi_geo$cluster, NA)
# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.

# KIDNEY

kidney <- filter(cancer_agg_wide, primary_site == "Kidney")
nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(kidney)), queen = TRUE), zero.policy = TRUE)
kidney_lmi <- localmoran(kidney$incidence_f, nbr, zero.policy = TRUE)
kidney_lmi_geo <- cbind(kidney,kidney_lmi)
kidney_lmi_geo$cluster <- attributes(kidney_lmi)$quadr$mean
colnames(kidney_lmi_geo) <- c("county", "mun_fips", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms", "exp_m", "exp_f", "crude_incdrt100000", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")
kidney_lmi_geo$cluster_sig <- ifelse(kidney_lmi_geo$p_val < .05, kidney_lmi_geo$cluster, NA)
# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.

# OVARIAN

# ovarian <- filter(cancer_agg_wide, primary_site == "Ovarian")
# nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(ovarian)), queen = TRUE), zero.policy = TRUE)
# ovarian_lmi <- localmoran(ovarian$incidence_f, nbr, zero.policy = TRUE)
# ovarian_lmi_geo <- cbind(ovarian,ovarian_lmi)
# ovarian_lmi_geo$cluster <- attributes(ovarian_lmi)$quadr$mean
# colnames(ovarian_lmi_geo) <- c("county", "mun_fips", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms", "exp_m", "exp_f", "crude_incdrt100000", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")
# ovarian_lmi_geo$cluster_sig <- ifelse(ovarian_lmi_geo$p_val < .05, ovarian_lmi_geo$cluster, NA)
# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.

# ENDOMETRIAL

# endometrial <- filter(cancer_agg_wide, primary_site == "Endometrial")
# nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(endometrial)), queen = TRUE), zero.policy = TRUE)
# endometrial_lmi <- localmoran(endometrial$incidence_f, nbr, zero.policy = TRUE)
# endometrial_lmi_geo <- cbind(endometrial,endometrial_lmi)
# endometrial_lmi_geo$cluster <- attributes(endometrial_lmi)$quadr$mean
# colnames(endometrial_lmi_geo) <- c("county", "mun_fips", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms", "exp_m", "exp_f", "crude_incdrt100000", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")
# endometrial_lmi_geo$cluster_sig <- ifelse(endometrial_lmi_geo$p_val < .05, endometrial_lmi_geo$cluster, NA)
# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.

# FEMALE BREAST

# breast <- filter(cancer_agg_wide, primary_site == "Breast")
# nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(breast)), queen = TRUE), zero.policy = TRUE)
# breast_lmi <- localmoran(breast$incidence_f, nbr, zero.policy = TRUE)
# breast_lmi_geo <- cbind(breast,breast_lmi)
# breast_lmi_geo$cluster <- attributes(breast_lmi)$quadr$mean
# colnames(breast_lmi_geo) <- c("county", "mun_fips", "primary_site", "incidence", "incidence_m", "incidence_f", "total_pop", "total_pop_m", "total_pop_f", "geoms", "exp_m", "exp_f", "crude_incdrt100000", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")
# breast_lmi_geo$cluster_sig <- ifelse(breast_lmi_geo$p_val < .05, breast_lmi_geo$cluster, NA)
# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.






# 6. categorical variables for bivariate mapping ----

# incidence rate tertiles
thyroid_lmi_geo$incd_rt_tert <- cut(thyroid_lmi_geo$crude_incdrt100000, quantile(thyroid_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)
kidney_lmi_geo$incd_rt_tert <- cut(kidney_lmi_geo$crude_incdrt100000, quantile(kidney_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)
# ovarian_lmi_geo$incd_rt_tert <- cut(ovarian_lmi_geo$crude_incdrt100000, quantile(ovarian_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)
# endometrial_lmi_geo$incd_rt_tert <- cut(endometrial_lmi_geo$crude_incdrt100000, quantile(endometrial_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)
# breast_lmi_geo$incd_rt_tert <- cut(breast_lmi_geo$crude_incdrt100000, quantile(breast_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)


# 7. statistics ----

area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
  mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))

mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))

thyroid_lmi_geo2 <- left_join(thyroid_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )
thyroid_lmi_geo2$incd_rt_tert <- as.factor(thyroid_lmi_geo2$incd_rt_tert)
thyroid_lmi_geo2$pfas_tri <- as.factor(thyroid_lmi_geo2$pfas_tri)

levels(thyroid_lmi_geo2$incd_rt_tert) <- c("Lower Tertile", "Middle Tertile", "Upper Tertile")
levels(thyroid_lmi_geo2$pfas_tri) <- c("No PFAS Detected", "PFAS Detected Between 0 and MRL", "PFAS Detected Above MRL")


kidney_lmi_geo2 <- left_join(kidney_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )
kidney_lmi_geo2$incd_rt_tert <- as.factor(kidney_lmi_geo2$incd_rt_tert)
kidney_lmi_geo2$pfas_tri <- as.factor(kidney_lmi_geo2$pfas_tri)

levels(kidney_lmi_geo2$incd_rt_tert) <- c("Lower Tertile", "Middle Tertile", "Upper Tertile")
levels(kidney_lmi_geo2$pfas_tri) <- c("No PFAS Detected", "PFAS Detected Between 0 and MRL", "PFAS Detected Above MRL")


# descriptives
# summary stats of PFAS levels, cancer incidence overall and by sex

# total
quantile(thyroid_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1))
quantile(thyroid_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1))

# women
quantile((thyroid_lmi_geo$incidence_f/thyroid_lmi_geo$total_pop_f)*100000, probs = c(0,.33,.66,1))
quantile((thyroid_lmi_geo$incidence_f/thyroid_lmi_geo$total_pop_f)*100000, probs = c(0,.25,.5,.75,1))

# men
quantile((thyroid_lmi_geo$incidence_m/thyroid_lmi_geo$total_pop_m)*100000, probs = c(0,.33,.66,1))
quantile((thyroid_lmi_geo$incidence_m/thyroid_lmi_geo$total_pop_m)*100000, probs = c(0,.25,.5,.75,1))



quantile(kidney_lmi_geo$crude_incdrt100000, probs = c(0,.33,.66,1))
quantile(kidney_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1))

# women
quantile((kidney_lmi_geo$incidence_f/kidney_lmi_geo$total_pop_f)*100000, probs = c(0,.33,.66,1))
quantile((kidney_lmi_geo$incidence_f/kidney_lmi_geo$total_pop_f)*100000, probs = c(0,.25,.5,.75,1))

# men
quantile((kidney_lmi_geo$incidence_m/kidney_lmi_geo$total_pop_m)*100000, probs = c(0,.33,.66,1))
quantile((kidney_lmi_geo$incidence_m/kidney_lmi_geo$total_pop_m)*100000, probs = c(0,.25,.5,.75,1))




tabyl(thyroid_lmi_geo2$pfas_tri) %>% 
  adorn_pct_formatting()

tabyl(kidney_lmi_geo2$pfas_tri) %>% 
  adorn_pct_formatting()


# bivariate % PFAS x cancer incidence tertiles + X2 and p-trend








# THYROID














# # incidence
# summary(thyroid_lmi_geo$crude_incdrt100000)
# tapply(thyroid_lmi_geo$crude_incdrt100000, thyroid_lmi_geo$County, summary)
# thyroid_lmi_geo$crude_incdrt100000.q <- cut(thyroid_lmi_geo$crude_incdrt100000, quantile(thyroid_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# table(thyroid_lmi_geo$County, thyroid_lmi_geo$crude_incdrt100000.q)
# overall_rate <- (sum(thyroid_lmi_geo$incidence)/sum(thyroid_lmi_geo$total_pop))*100000
# overall_rate
# thyroid_lmi_geo2 %>% 
#   group_by(pfas_tri) %>% 
#   summarize(i_sum = sum(incidence))
# (7922/sum(thyroid_lmi_geo2$incidence))*100
# quantile(thyroid$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
# # lmi/global?
# gmi_thyroid <- moran.test(thyroid_lmi_geo$incidence, nbr, zero.policy = TRUE)
# gmi_thyroid
# (length(thyroid_lmi_geo$cluster_sig)-sum(is.na(thyroid_lmi_geo$cluster_sig)))/length(thyroid_lmi_geo$cluster_sig)
# sum(thyroid_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
# sum(thyroid_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
# sum(thyroid_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(thyroid_lmi_geo$cluster_sig == 4, na.rm=TRUE)
# (57/378)*100
# (33/378)*100
# (34/378)*100
# # incidence x pfas
# table(thyroid_lmi_geo2$incd_rt_tert, thyroid_lmi_geo2$pfas_tri)
# quantile(thyroid$crude_incdrt100000, probs = c(0,.33,.66,1))


mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
thyroid_lmi_geo2 <- left_join(thyroid_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )
thyroid_lmi_geo2$incd_rt_tert <- as.factor(thyroid_lmi_geo2$incd_rt_tert)
thyroid_lmi_geo2$pfas_tri <- as.factor(thyroid_lmi_geo2$pfas_tri)

# incidence
summary(thyroid$incidence_f)
summary(thyroid_lmi_geo$crude_incdrt100000)
tapply(thyroid_lmi_geo$crude_incdrt100000, thyroid_lmi_geo$county, summary)
thyroid_lmi_geo$crude_incdrt100000.q <- cut(thyroid_lmi_geo$crude_incdrt100000, quantile(thyroid_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
table(thyroid_lmi_geo$county, thyroid_lmi_geo$crude_incdrt100000.q)
overall_rate <- (sum(thyroid_lmi_geo$incidence_f)/sum(thyroid_lmi_geo$total_pop_f))*100000
overall_rate
thyroid_lmi_geo2 %>%
  group_by(pfas_tri) %>%
  summarize(i_sum = sum(incidence_f))
(37933/sum(thyroid_lmi_geo2$incidence_f))*100
quantile(thyroid$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
quantile(thyroid$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1))

# lmi/global?
gmi_thyroid <- moran.test(thyroid_lmi_geo$incidence_f, nbr, zero.policy = TRUE)
gmi_thyroid
(length(thyroid_lmi_geo$cluster_sig)-sum(is.na(thyroid_lmi_geo$cluster_sig)))/length(thyroid_lmi_geo$cluster_sig)
sum(thyroid_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
sum(thyroid_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
sum(thyroid_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(thyroid_lmi_geo$cluster_sig == 4, na.rm=TRUE)
(65/378)*100
(29/378)*100
(36/378)*100
# incidence x pfas
table(thyroid_lmi_geo2$incd_rt_tert, thyroid_lmi_geo2$pfas_tri)
quantile(thyroid$crude_incdrt100000, probs = c(0,.33,.66,1))

prop.table(table(thyroid_lmi_geo2$incd_rt_tert, thyroid_lmi_geo2$pfas_tri), 1)
chisq.test(thyroid_lmi_geo2$incd_rt_tert, thyroid_lmi_geo2$pfas_tri)
cor.test(x=as.numeric(thyroid_lmi_geo2$incd_rt_tert), y=as.numeric(thyroid_lmi_geo2$pfas_tri), method = 'kendall')

# KIDNEY

# # incidence
# summary(kidney_lmi_geo$crude_incdrt100000)
# tapply(kidney_lmi_geo$crude_incdrt100000, kidney_lmi_geo$County, summary)
# kidney_lmi_geo$crude_incdrt100000.q <- cut(kidney_lmi_geo$crude_incdrt100000, quantile(kidney_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# table(kidney_lmi_geo$County, kidney_lmi_geo$crude_incdrt100000.q)
# quantile(kidney$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
# overall_rate <- (sum(kidney_lmi_geo$incidence)/sum(kidney_lmi_geo$total_pop))*100000
# overall_rate
# kidney_lmi_geo2 %>% 
#   group_by(pfas_tri) %>% 
#   summarize(i_sum = sum(incidence))
# (8112/sum(kidney_lmi_geo2$incidence))*100
# # lmi/global?
# gmi_kidney <- moran.test(kidney_lmi_geo$incidence, nbr, zero.policy = TRUE)
# gmi_kidney
# (length(kidney_lmi_geo$cluster_sig)-sum(is.na(kidney_lmi_geo$cluster_sig)))/length(kidney_lmi_geo$cluster_sig)
# sum(kidney_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
# sum(kidney_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
# # incidence x pfas
# table(kidney_lmi_geo2$incd_rt_tert, kidney_lmi_geo2$pfas_tri)
# quantile(kidney$crude_incdrt100000, probs = c(0,.33,.66,1))

mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
kidney_lmi_geo2 <- left_join(kidney_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )
kidney_lmi_geo2$incd_rt_tert <- as.factor(kidney_lmi_geo2$incd_rt_tert)
kidney_lmi_geo2$pfas_tri <- as.factor(kidney_lmi_geo2$pfas_tri)

# incidence
summary(kidney$incidence_f)
summary(kidney_lmi_geo$crude_incdrt100000)
tapply(kidney_lmi_geo$crude_incdrt100000, kidney_lmi_geo$county, summary)
kidney_lmi_geo$crude_incdrt100000.q <- cut(kidney_lmi_geo$crude_incdrt100000, quantile(kidney_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
table(kidney_lmi_geo$county, kidney_lmi_geo$crude_incdrt100000.q)
overall_rate <- (sum(kidney_lmi_geo$incidence_f)/sum(kidney_lmi_geo$total_pop_f))*100000
overall_rate
kidney_lmi_geo2 %>%
  group_by(pfas_tri) %>%
  summarize(i_sum = sum(incidence_f))
(37933/sum(kidney_lmi_geo2$incidence_f))*100
quantile(kidney$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
quantile(kidney$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1))

# lmi/global?
gmi_kidney <- moran.test(kidney_lmi_geo$incidence_f, nbr, zero.policy = TRUE)
gmi_kidney
(length(kidney_lmi_geo$cluster_sig)-sum(is.na(kidney_lmi_geo$cluster_sig)))/length(kidney_lmi_geo$cluster_sig)
sum(kidney_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
sum(kidney_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
sum(kidney_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(kidney_lmi_geo$cluster_sig == 4, na.rm=TRUE)
(65/378)*100
(29/378)*100
(36/378)*100
# incidence x pfas
table(kidney_lmi_geo2$incd_rt_tert, kidney_lmi_geo2$pfas_tri)
quantile(kidney$crude_incdrt100000, probs = c(0,.33,.66,1))

prop.table(table(kidney_lmi_geo2$incd_rt_tert, kidney_lmi_geo2$pfas_tri), 1)
chisq.test(kidney_lmi_geo2$incd_rt_tert, kidney_lmi_geo2$pfas_tri)
cor.test(x=as.numeric(kidney_lmi_geo2$incd_rt_tert), y=as.numeric(kidney_lmi_geo2$pfas_tri), method = 'kendall')







# OVARIAN

# table(cancer_agg[cancer_agg$primary_site == "Breast", "Sex"])
# table(cancer_agg[cancer_agg$primary_site == "Endometrial", "Sex"])
# table(cancer_agg[cancer_agg$primary_site == "Ovarian", "Sex"])

# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
# ovarian_lmi_geo2 <- left_join(ovarian_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
# ovarian_lmi_geo2$incd_rt_tert <- as.factor(ovarian_lmi_geo2$incd_rt_tert)
# ovarian_lmi_geo2$pfas_tri <- as.factor(ovarian_lmi_geo2$pfas_tri)
# 
# # incidence
# 
# sum(ovarian$incidence_f)
# # sum(breast$incidence_f)
# # sum(endometrial$incidence_f)
# 
# summary(ovarian$incidence_f)
# 
# summary(ovarian_lmi_geo$crude_incdrt100000)
# tapply(ovarian_lmi_geo$crude_incdrt100000, ovarian_lmi_geo$county, summary)
# ovarian_lmi_geo$crude_incdrt100000.q <- cut(ovarian_lmi_geo$crude_incdrt100000, quantile(ovarian_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# table(ovarian_lmi_geo$county, ovarian_lmi_geo$crude_incdrt100000.q)
# overall_rate <- (sum(ovarian_lmi_geo$incidence_f)/sum(ovarian_lmi_geo$total_pop_f))*100000
# overall_rate
# ovarian_lmi_geo2 %>%
#   group_by(pfas_tri) %>%
#   summarize(i_sum = sum(incidence_f))
# (3782/sum(ovarian_lmi_geo2$incidence_f))*100
# quantile(ovarian$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
# quantile(ovarian$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1))
# 
# # lmi/global?
# gmi_ovarian <- moran.test(ovarian_lmi_geo$incidence_f, nbr, zero.policy = TRUE)
# gmi_ovarian
# (length(ovarian_lmi_geo$cluster_sig)-sum(is.na(ovarian_lmi_geo$cluster_sig)))/length(ovarian_lmi_geo$cluster_sig)
# sum(ovarian_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
# sum(ovarian_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
# sum(ovarian_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(ovarian_lmi_geo$cluster_sig == 4, na.rm=TRUE)
# (62/378)*100
# (27/378)*100
# (36/378)*100
# # incidence x pfas
# table(ovarian_lmi_geo2$incd_rt_tert, ovarian_lmi_geo2$pfas_tri)
# quantile(ovarian$crude_incdrt100000, probs = c(0,.33,.66,1))
# 
# prop.table(table(ovarian_lmi_geo2$incd_rt_tert, ovarian_lmi_geo2$pfas_tri), 1)
# chisq.test(ovarian_lmi_geo2$incd_rt_tert, ovarian_lmi_geo2$pfas_tri)
# cor.test(x=as.numeric(ovarian_lmi_geo2$incd_rt_tert), y=as.numeric(ovarian_lmi_geo2$pfas_tri), method = 'kendall')





# ENDOMETRIAL

# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
# endometrial_lmi_geo2 <- left_join(endometrial_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
# endometrial_lmi_geo2$incd_rt_tert <- as.factor(endometrial_lmi_geo2$incd_rt_tert)
# endometrial_lmi_geo2$pfas_tri <- as.factor(endometrial_lmi_geo2$pfas_tri)
# 
# # incidence
# summary(endometrial$incidence_f)
# 
# summary(cancer_agg[cancer_agg$primary_site == "Endometrial" & cancer_agg$Sex %in% c("Total","Female"), "incidence"])
# 
# summary(endometrial_lmi_geo$crude_incdrt100000)
# tapply(endometrial_lmi_geo$crude_incdrt100000, endometrial_lmi_geo$county, summary)
# endometrial_lmi_geo$crude_incdrt100000.q <- cut(endometrial_lmi_geo$crude_incdrt100000, quantile(endometrial_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# table(endometrial_lmi_geo$county, endometrial_lmi_geo$crude_incdrt100000.q)
# overall_rate <- (sum(endometrial_lmi_geo$incidence_f)/sum(endometrial_lmi_geo$total_pop_f))*100000
# overall_rate
# endometrial_lmi_geo2 %>%
#   group_by(pfas_tri) %>%
#   summarize(i_sum = sum(incidence_f))
# (8963/sum(endometrial_lmi_geo2$incidence_f))*100
# quantile(endometrial$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
# quantile(endometrial$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1))
# 
# # lmi/global?
# gmi_endometrial <- moran.test(endometrial_lmi_geo$incidence_f, nbr, zero.policy = TRUE)
# gmi_endometrial
# (length(endometrial_lmi_geo$cluster_sig)-sum(is.na(endometrial_lmi_geo$cluster_sig)))/length(endometrial_lmi_geo$cluster_sig)
# sum(endometrial_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
# sum(endometrial_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
# sum(endometrial_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(endometrial_lmi_geo$cluster_sig == 4, na.rm=TRUE)
# (60/378)*100
# (25/378)*100
# (35/378)*100
# # incidence x pfas
# table(endometrial_lmi_geo2$incd_rt_tert, endometrial_lmi_geo2$pfas_tri)
# quantile(endometrial$crude_incdrt100000, probs = c(0,.33,.66,1))
# 
# prop.table(table(endometrial_lmi_geo2$incd_rt_tert, endometrial_lmi_geo2$pfas_tri), 1)
# chisq.test(endometrial_lmi_geo2$incd_rt_tert, endometrial_lmi_geo2$pfas_tri)
# cor.test(x=as.numeric(endometrial_lmi_geo2$incd_rt_tert), y=as.numeric(endometrial_lmi_geo2$pfas_tri), method = 'kendall')





# BREAST

# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
# breast_lmi_geo2 <- left_join(breast_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
# breast_lmi_geo2$incd_rt_tert <- as.factor(breast_lmi_geo2$incd_rt_tert)
# breast_lmi_geo2$pfas_tri <- as.factor(breast_lmi_geo2$pfas_tri)
# 
# # incidence
# summary(breast$incidence_f)
# summary(breast_lmi_geo$crude_incdrt100000)
# tapply(breast_lmi_geo$crude_incdrt100000, breast_lmi_geo$county, summary)
# breast_lmi_geo$crude_incdrt100000.q <- cut(breast_lmi_geo$crude_incdrt100000, quantile(breast_lmi_geo$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# table(breast_lmi_geo$county, breast_lmi_geo$crude_incdrt100000.q)
# overall_rate <- (sum(breast_lmi_geo$incidence_f)/sum(breast_lmi_geo$total_pop_f))*100000
# overall_rate
# breast_lmi_geo2 %>%
#   group_by(pfas_tri) %>%
#   summarize(i_sum = sum(incidence_f))
# (37933/sum(breast_lmi_geo2$incidence_f))*100
# quantile(breast$crude_incdrt100000, probs = c(0,.25,.5,.75,1))
# quantile(breast$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1))
# 
# # lmi/global?
# gmi_breast <- moran.test(breast_lmi_geo$incidence_f, nbr, zero.policy = TRUE)
# gmi_breast
# (length(breast_lmi_geo$cluster_sig)-sum(is.na(breast_lmi_geo$cluster_sig)))/length(breast_lmi_geo$cluster_sig)
# sum(breast_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
# sum(breast_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high
# sum(breast_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(breast_lmi_geo$cluster_sig == 4, na.rm=TRUE)
# (65/378)*100
# (29/378)*100
# (36/378)*100
# # incidence x pfas
# table(breast_lmi_geo2$incd_rt_tert, breast_lmi_geo2$pfas_tri)
# quantile(breast$crude_incdrt100000, probs = c(0,.33,.66,1))
# 
# prop.table(table(breast_lmi_geo2$incd_rt_tert, breast_lmi_geo2$pfas_tri), 1)
# chisq.test(breast_lmi_geo2$incd_rt_tert, breast_lmi_geo2$pfas_tri)
# cor.test(x=as.numeric(breast_lmi_geo2$incd_rt_tert), y=as.numeric(breast_lmi_geo2$pfas_tri), method = 'kendall')







# 8. maps ----

l <- get_legend(ggplot() + geom_sf(data = study_counties, aes(fill = "leg"), color = "black", linewidth = 0.6) + scale_fill_manual(values = c("white"), labels = c("No data")) + guides(fill = guide_legend(title = NULL)))

# adding non-study area municipalities for NA values 
nonstudy_mun <- anti_join(mun.shp, mun_red, by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1")) %>% 
  mutate(county_fips = case_when(
          COUNTY == "06" ~ "11",
          COUNTY == "09" ~ "17",
          COUNTY == "13" ~ "25",
          COUNTY == "15" ~ "29",
          COUNTY == "23" ~ "45",
          COUNTY == "36" ~ "71",
          COUNTY == "38" ~ "75",
          COUNTY == "39" ~ "77",
          COUNTY == "45" ~ "89",
          COUNTY == "46" ~ "91",
          COUNTY == "48" ~ "95",
          COUNTY == "53" ~ "107"
        ),
        mun_fips_c = str_c(county_fips, FIPS_MUN_C),
        
      ) %>% 
  dplyr::select(mun_fips_c, geoms)

ob <- bind_rows(ovarian, nonstudy_mun)



# THYROID 

# # incidence
# thyroid$incd.q <- cut(thyroid$incidence, quantile(thyroid$incidence, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# thyroid$crude_incdrt100000.q <- cut(thyroid$crude_incdrt100000, quantile(thyroid$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")
# ggplot() +
#   geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
#   geom_sf(data = thyroid, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
#   geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
#   scale_fill_manual(values=cols.blues, labels = c('[0-25%]','[25-50%]', '[50-75%]', '[75-100%]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
#   scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
#   labs(caption = "Muncipality-level quartiles of thyroid cancer incidence per 1000 from 2000 to 2019") +
#   guides(
#     fill = guide_legend(title = "Quartiles"),
#     color = "none"
#   ) +
#   theme(
#     plot.background = element_rect(color = "black", linewidth = 3),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family = "Arial", size = 14),  # Set font size here
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     legend.key = element_rect(colour = NA, fill = NA)
#   )
# # +
# #   ggspatial::annotation_north_arrow(
# #     location = "br", which_north = "true",
# #     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
# #     height = unit(0.6, "in"), width = unit(0.6, "in"),
# #     style = ggspatial::north_arrow_fancy_orienteering(
# #       fill = c("black", "white"),
# #       line_col = "black"
# #     )) 
# # 
# ggsave("02_output/thyroid_incidper1000_municipality.png")
# 
# # significant clusters
# ggplot() +
#   geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
#   geom_sf(data = thyroid_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
#   geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
#   scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
#   scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
#   labs(caption = "Local positive and negative statistically significant clustering of thyroid cancer\nincidence from 2000 to 2019 at the muncipality-level") +
#   guides(
#     fill = guide_legend(title = "Direction"),
#     color = "none"
#   ) +
#   theme(
# #    plot.background = element_rect(color = "black", linewidth = 3),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family = "Arial", size = 14),  # Set font size here
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     legend.key = element_rect(colour = NA, fill = NA)
#   )
# ggsave("02_output/thyroid_lisa_municipality.png")
# 
# # bivariate
# # area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
# #   mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))
# # 
# # mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
# # 
# # thyroid_lmi_geo2 <- left_join(thyroid_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
# #   mutate(
# #     pfas_tri = case_when(
# #       pfas_final == 0 ~ "1",
# #       pfas_final %in% c(1, 2) ~ "2",
# #       pfas_final == 3 ~ "3"
# #     )
# #   )
# # 
# # thyroid_lmi_geo2$incd_rt_tert <- as.factor(thyroid_lmi_geo2$incd_rt_tert)
# # thyroid_lmi_geo2$pfas_tri <- as.factor(thyroid_lmi_geo2$pfas_tri)
# bivar_thyroid_pfas <- bi_class(thyroid_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)
# # create map
# pa <- ggplot() +
#   geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
#   geom_sf(data = bivar_thyroid_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
#   geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
#   bi_scale_fill(pal = "DkBlue2", dim = 3) +
#   bi_scale_color(pal = "DkBlue2", dim = 3) +
#   labs(caption = "Bivariate map of thyroid cancer incidence and pfas contamination") +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family="Times New Roman", face="bold"),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size=11,hjust=0),
#     axis.title.x=element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   )  
# l <- ggplot() +
#   geom_sf(data = study_counties, aes(fill = "not in study"), color = "grey25", size = 0.5)  +
#   scale_fill_manual(values = c("not in study" = "white")) +
#   guides(fill = guide_legend(title = ""))
# leg <- get_legend(l)
# # create legend
# legend <- bi_legend(pal = "DkBlue2",
#                     dim = 3,
#                     xlab = "Higher Incidence ",
#                     ylab = "Higher PFAS contamination ",
#                     size = 7)
# # combine map with legend
# ggdraw() +
#   draw_plot(pa, x = 0, y = 0, width = .6, height = 1) +
#   draw_plot(legend, .6, .4, 0.3, 0.3) + 
#   draw_plot(leg, .6, .2, 0.3, 0.3) +   
#   theme(plot.background = element_rect(fill="white", color = NA))
# ggsave("02_output/bivar_thyroid_pfas.png")












# KIDNEY 

# # incidence
# kidney$crude_incdrt100000.q <- cut(kidney$crude_incdrt100000, quantile(kidney$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
# cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")
# ggplot() +
#   geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
#   geom_sf(data = kidney, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
#   geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
#   scale_fill_manual(values=cols.blues, labels = c('[0-25%]','[25-50%]', '[50-75%]', '[75-100%]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
#   scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
#   labs(caption = "Muncipality-level quartiles of kidney cancer incidence per 1000 from 2000 to 2019") +
#   guides(
#     fill = guide_legend(title = "Quartiles"),
#     color = "none"
#   ) +
#   theme(
#     plot.background = element_rect(color = "black", linewidth = 3),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family = "Arial", size = 14),  # Set font size here
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     legend.key = element_rect(colour = NA, fill = NA)
#   )
# ggsave("02_output/kidney_incidper1000_municipality.png")
# # significant clusters
# ggplot() +
#   geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
#   geom_sf(data = kidney_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
#   geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
#   scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
#   scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
#   labs(caption = "Local positive and negative statistically significant clustering of kidney cancer\nincidence from 2000 to 2019 at the muncipality-level") +
#   guides(
#     fill = guide_legend(title = "Direction"),
#     color = "none"
#   ) +
#   theme(
#     #    plot.background = element_rect(color = "black", linewidth = 3),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size = 12, hjust = 0, vjust = 3),
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family = "Arial", size = 14),  # Set font size here
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     legend.key = element_rect(colour = NA, fill = NA)
#   )
# ggsave("02_output/kidney_lisa_municipality.png")
# 
# # bivariate
# # area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
# #   mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))
# # 
# # mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips), by = c("fips" = "FIPS_MUN_C"))
# # 
# # mun_pfas$mun_fips_c <- as.character(mun_pfas$mun_fips)
# # 
# # kidney_lmi_geo2 <- left_join(kidney_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
# #   mutate(
# #     pfas_tri = case_when(
# #       pfas_final == 0 ~ "1",
# #       pfas_final %in% c(1, 2) ~ "2",
# #       pfas_final == 3 ~ "3"
# #     )
# #   )
# # 
# # kidney_lmi_geo2$incd_rt_tert <- as.factor(kidney_lmi_geo2$incd_rt_tert)
# # kidney_lmi_geo2$pfas_tri <- as.factor(kidney_lmi_geo2$pfas_tri)
# bivar_kidney_pfas <- bi_class(kidney_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)
# # create map
# pa <- ggplot() +
#   geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
#   geom_sf(data = bivar_kidney_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
#   geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
#   bi_scale_fill(pal = "DkBlue2", dim = 3) +
#   bi_scale_color(pal = "DkBlue2", dim = 3) +
#   labs(caption = "Bivariate map of kidney cancer incidence and pfas contamination") +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     title = element_text(family="Times New Roman", face="bold"),
#     plot.caption.position = "panel",
#     plot.caption = element_text(size=11,hjust=0),
#     axis.title.x=element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   )  
# l <- ggplot() +
#   geom_sf(data = study_counties, aes(fill = "not in study"), color = "grey25", size = 0.5)  +
#   scale_fill_manual(values = c("not in study" = "white")) +
#   guides(fill = guide_legend(title = ""))
# leg <- get_legend(l)
# # create legend
# legend <- bi_legend(pal = "DkBlue2",
#                     dim = 3,
#                     xlab = "Higher Incidence ",
#                     ylab = "Higher PFAS contamination ",
#                     size = 7)
# # combine map with legend
# ggdraw() +
#   draw_plot(pa, x = 0, y = 0, width = .6, height = 1) +
#   draw_plot(legend, .6, .4, 0.3, 0.3) + 
#   draw_plot(leg, .6, .2, 0.3, 0.3) +   
#   theme(plot.background = element_rect(fill="white", color = NA))
# ggsave("02_output/bivar_kidney_pfas.png")













# Ovarian 

# incidence
ovarian$incd.q <- cut(ovarian$incidence, quantile(ovarian$incidence_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
ovarian$crude_incdrt100000.q <- cut(ovarian$crude_incdrt100000, quantile(ovarian$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
cols.blues <- c("white","#eff3ff","#bdd7e7","#6baed6","#08519c")

# 0%       25%       50%       75%      100% 
# 0.0000  202.9788  455.9400  847.0377 8840.8644 

ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues, labels = c('No Exposure Data','0.0 - 203.0','203.0 - 455.9', '455.9 - 847.0', '847.0 - 8840.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  labs(caption = "Ovarian cancer incidence rates per 100,000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    plot.background = element_rect(fill="white", color = "black", linewidth = 3),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 11.5, hjust = 0, vjust = 3),
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
  ) + ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(0.05, "in"), pad_y = unit(0.1, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.09, "in"), pad_y = unit(0.28, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

ggsave("02_output/ovarian_incidper1000_municipality4.png", height = 4, width = 5.35, units = "in")





ovarian$incd.q5 <- cut(ovarian$incidence, quantile(ovarian$incidence_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
ovarian$crude_incdrt100000.q5 <- cut(ovarian$crude_incdrt100000, quantile(ovarian$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
cols.blues5 <- c("white","#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d")

ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues5, labels = c('No Exposure Data','0.0 - 203.0','203.0 - 455.9', '455.9 - 847.0', '847.0 - 8840.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.blues5, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  labs(caption = "Ovarian cancer incidence rates per 100,000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Rate per 100,000"),
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
  )
# +
#   ggspatial::annotation_north_arrow(
#     location = "br", which_north = "true",
#     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#     height = unit(0.6, "in"), width = unit(0.6, "in"),
#     style = ggspatial::north_arrow_fancy_orienteering(
#       fill = c("black", "white"),
#       line_col = "black"
#     ))
#

ggdraw() +
  draw_plot(p5, 0, 0) +
  draw_plot(l, .764, .26, 0.3, 0.3) +   
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("02_output/ovarian_incidper1000_municipality5.png")























# significant clusters
ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = ovarian_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
  labs(caption = "Local positive and negative statistically significant clustering of ovarian cancer\nincidence from 2000 to 2020 at the muncipality-level") +
  guides(
    fill = guide_legend(title = "Direction"),
    color = "none"
  ) +
  theme(
#    plot.background = element_rect(color = "black", linewidth = 3),
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
  )
ggsave("02_output/ovarian_lisa_municipality.png")

# bivariate
# area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>%
#   mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))
#
# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
#
# ovarian_lmi_geo2 <- left_join(ovarian_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>%
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
#
# ovarian_lmi_geo2$incd_rt_tert <- as.factor(ovarian_lmi_geo2$incd_rt_tert)
# ovarian_lmi_geo2$pfas_tri <- as.factor(ovarian_lmi_geo2$pfas_tri)
bivar_ovarian_pfas <- bi_class(ovarian_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)
# create map
pa <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
  geom_sf(data = bivar_ovarian_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
  geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  bi_scale_color(pal = "DkBlue2", dim = 3) +
  labs(caption = "Bivariate map of ovarian cancer incidence and pfas contamination") +
  theme(
    panel.background = element_rect(fill = "white"),
    title = element_text(family="Times New Roman", face="bold"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=11,hjust=0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
l <- ggplot() +
  geom_sf(data = study_counties, aes(fill = "not in study"), color = "grey25", size = 0.5)  +
  scale_fill_manual(values = c("not in study" = "white")) +
  guides(fill = guide_legend(title = ""))
leg <- get_legend(l)
# create legend
legend <- bi_legend(pal = "DkBlue2",
                    dim = 3,
                    xlab = "Higher Incidence ",
                    ylab = "Higher PFAS contamination ",
                    size = 7)
# combine map with legend
ggdraw() +
  draw_plot(pa, x = 0, y = 0, width = .6, height = 1) +
  draw_plot(legend, .6, .4, 0.3, 0.3) +
  draw_plot(leg, .6, .2, 0.3, 0.3) +
  theme(plot.background = element_rect(fill="white", color = NA))
ggsave("02_output/bivar_ovarian_pfas.png")










# Endometrial 

# incidence
endometrial$incd.q <- cut(endometrial$incidence, quantile(endometrial$incidence_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
endometrial$crude_incdrt100000.q <- cut(endometrial$crude_incdrt100000, quantile(endometrial$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")
p4 <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = endometrial, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues, labels = c('[0.0-5.6]','[5.6-11.2]', '[11.2-20.5]', '[20.5-180.1]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quartiles of endometrial cancer incidence per 1000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Quartiles"),
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
  )
# +
#   ggspatial::annotation_north_arrow(
#     location = "br", which_north = "true",
#     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#     height = unit(0.6, "in"), width = unit(0.6, "in"),
#     style = ggspatial::north_arrow_fancy_orienteering(
#       fill = c("black", "white"),
#       line_col = "black"
#     ))
#

ggdraw() +
  draw_plot(p4, 0, 0) +
  draw_plot(l, .745, .273, 0.3, 0.3) +   
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("02_output/endometrial_incidper1000_municipality4.png")









endometrial$incd.q5 <- cut(endometrial$incidence, quantile(endometrial$incidence_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
endometrial$crude_incdrt100000.q5 <- cut(endometrial$crude_incdrt100000, quantile(endometrial$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
cols.blues5 <- c("#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d")
p5 <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = endometrial, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q5), color = as.factor(crude_incdrt100000.q5))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues5, labels = c('[0.0-4.6]','[4.6-8.8]', '[8.8-13.7]', '[13.7-24.3]', '[24.3-180.1]'), limits= c("1","2","3","4","5"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues5, limits= c("1","2","3","4","5"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quintiles of endometrial cancer incidence per 1000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Quintiles"),
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
  )
# +
#   ggspatial::annotation_north_arrow(
#     location = "br", which_north = "true",
#     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#     height = unit(0.6, "in"), width = unit(0.6, "in"),
#     style = ggspatial::north_arrow_fancy_orienteering(
#       fill = c("black", "white"),
#       line_col = "black"
#     ))
#

ggdraw() +
  draw_plot(p5, 0, 0) +
  draw_plot(l, .745, .26, 0.3, 0.3) +   
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("02_output/endometrial_incidper1000_municipality5.png")






# significant clusters
ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = endometrial_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
  labs(caption = "Local positive and negative statistically significant clustering of endometrial cancer\nincidence from 2000 to 2020 at the muncipality-level") +
  guides(
    fill = guide_legend(title = "Direction"),
    color = "none"
  ) +
  theme(
    #    plot.background = element_rect(color = "black", linewidth = 3),
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
  )
ggsave("02_output/endometrial_lisa_municipality.png")

# bivariate
# area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>%
#   mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))
#
# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
#
# endometrial_lmi_geo2 <- left_join(endometrial_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>%
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
#
# endometrial_lmi_geo2$incd_rt_tert <- as.factor(endometrial_lmi_geo2$incd_rt_tert)
# endometrial_lmi_geo2$pfas_tri <- as.factor(endometrial_lmi_geo2$pfas_tri)
bivar_endometrial_pfas <- bi_class(endometrial_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)
# create map
pa <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
  geom_sf(data = bivar_endometrial_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
  geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  bi_scale_color(pal = "DkBlue2", dim = 3) +
  labs(caption = "Bivariate map of endometrial cancer incidence and pfas contamination") +
  theme(
    panel.background = element_rect(fill = "white"),
    title = element_text(family="Times New Roman", face="bold"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=11,hjust=0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
l <- ggplot() +
  geom_sf(data = study_counties, aes(fill = "not in study"), color = "grey25", size = 0.5)  +
  scale_fill_manual(values = c("not in study" = "white")) +
  guides(fill = guide_legend(title = ""))
leg <- get_legend(l)
# create legend
legend <- bi_legend(pal = "DkBlue2",
                    dim = 3,
                    xlab = "Higher Incidence ",
                    ylab = "Higher PFAS contamination ",
                    size = 7)
# combine map with legend
ggdraw() +
  draw_plot(pa, x = 0, y = 0, width = .6, height = 1) +
  draw_plot(legend, .6, .4, 0.3, 0.3) +
  draw_plot(leg, .6, .2, 0.3, 0.3) +
  theme(plot.background = element_rect(fill="white", color = NA))
ggsave("02_output/bivar_endometrial_pfas.png")











# Breast 

# incidence
# quartiles
breast$incd.q <- cut(breast$incidence, quantile(breast$incidence_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
breast$crude_incdrt100000.q <- cut(breast$crude_incdrt100000, quantile(breast$crude_incdrt100000, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")
p4 <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = breast, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q), color = as.factor(crude_incdrt100000.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues, labels = c('[1.2-22.7]','[22.7-43.6]', '[43.6-85.9]', '[85.9-762.9]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quartiles of breast cancer incidence per 1000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Quartiles"),
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
  )

# +
#   ggspatial::annotation_north_arrow(
#     location = "br", which_north = "true",
#     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#     height = unit(0.6, "in"), width = unit(0.6, "in"),
#     style = ggspatial::north_arrow_fancy_orienteering(
#       fill = c("black", "white"),
#       line_col = "black"
#     ))
#

ggdraw() +
  draw_plot(p4, 0, 0) +
  draw_plot(l, .745, .273, 0.3, 0.3) +   
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("02_output/breast_incidper1000_municipality4.png")

# quintiles
breast$incd.q5 <- cut(breast$incidence, quantile(breast$incidence_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
breast$crude_incdrt100000.q5 <- cut(breast$crude_incdrt100000, quantile(breast$crude_incdrt100000, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE)
cols.blues5 <- c("#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d")
p5 <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = breast, aes(geometry = geoms, fill = as.factor(crude_incdrt100000.q5), color = as.factor(crude_incdrt100000.q5))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues5, labels = c('[1.2-19.1]','[19.1-38.0]', '[38.0-54.1]', '[54.1-99.4]', '[99.4-762.9]'), limits= c("1","2","3","4","5"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues5, limits= c("1","2","3","4","5"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quintiles of breast cancer incidence per 1000 from 2000 to 2020") +
  guides(
    fill = guide_legend(title = "Quintiles"),
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
  )
# +
#   ggspatial::annotation_north_arrow(
#     location = "br", which_north = "true",
#     pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#     height = unit(0.6, "in"), width = unit(0.6, "in"),
#     style = ggspatial::north_arrow_fancy_orienteering(
#       fill = c("black", "white"),
#       line_col = "black"
#     ))
#

ggdraw() +
  draw_plot(p5, 0, 0) +
  draw_plot(l, .745, .26, 0.3, 0.3) +   
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("02_output/breast_incidper1000_municipality5.png")










# significant clusters
ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = breast_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
  labs(caption = "Local positive and negative statistically significant clustering of breast cancer\nincidence from 2000 to 2020 at the muncipality-level") +
  guides(
    fill = guide_legend(title = "Direction"),
    color = "none"
  ) +
  theme(
    #    plot.background = element_rect(color = "black", linewidth = 3),
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
  )
ggsave("02_output/breast_lisa_municipality.png")

# bivariate
# area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>%
#   mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))
#
# mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))
#
# breast_lmi_geo2 <- left_join(breast_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>%
#   mutate(
#     pfas_tri = case_when(
#       pfas_final == 0 ~ "1",
#       pfas_final %in% c(1, 2) ~ "2",
#       pfas_final == 3 ~ "3"
#     )
#   )
#
# breast_lmi_geo2$incd_rt_tert <- as.factor(breast_lmi_geo2$incd_rt_tert)
# breast_lmi_geo2$pfas_tri <- as.factor(breast_lmi_geo2$pfas_tri)
bivar_breast_pfas <- bi_class(breast_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)
# create map
pa <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
  geom_sf(data = bivar_breast_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
  geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  bi_scale_color(pal = "DkBlue2", dim = 3) +
  labs(caption = "Bivariate map of breast cancer incidence and pfas contamination") +
  theme(
    panel.background = element_rect(fill = "white"),
    title = element_text(family="Times New Roman", face="bold"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=11,hjust=0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
l <- ggplot() +
  geom_sf(data = study_counties, aes(fill = "not in study"), color = "grey25", size = 0.5)  +
  scale_fill_manual(values = c("not in study" = "white")) +
  guides(fill = guide_legend(title = ""))
leg <- get_legend(l)
# create legend
legend <- bi_legend(pal = "DkBlue2",
                    dim = 3,
                    xlab = "Higher Incidence ",
                    ylab = "Higher PFAS contamination ",
                    size = 7)
# combine map with legend
ggdraw() +
  draw_plot(pa, x = 0, y = 0, width = .6, height = 1) +
  draw_plot(legend, .6, .4, 0.3, 0.3) +
  draw_plot(leg, .6, .2, 0.3, 0.3) +
  theme(plot.background = element_rect(fill="white", color = NA))
ggsave("02_output/bivar_breast_pfas.png")




