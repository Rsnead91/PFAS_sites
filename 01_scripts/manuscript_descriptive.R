# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load('utils','tidycensus','tidyverse','ggspatial','arcpullr','tigris','raster','pdftools','readxl','magick','units','sf','ggmap','biscale','reshape2','janitor','ggpattern','tidygeocoder','lubridate', 'openxlsx','spdep','cowplot')

options(scipen = 999)

# 2. data ----

# data for erin 20240226

# write_csv(
#   cancer_agg %>% 
#     filter(Sex == "Total") %>% 
#     dplyr::select(County, mun_fips_c, primary_site, incidence) %>% 
#     clean_names(),
#   file = "00_data/municipality_incidence.csv"
# )

## counties ----

study_counties <- tigris::counties(state = "42", cb = TRUE) %>% 
  filter(NAMELSAD %in% c("Berks County", "Bucks County", "Carbon County", "Chester County", "Delaware County", "Lancaster County", "Lebanon County", "Lehigh County", "Monroe County", "Montgomery County", "Northampton County", "Schuylkill County")) %>% 
  st_set_crs(st_crs(4269))

## municipalities ----

### shapefiles
mun_shp <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennDOT/MapServer/10", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269)

### average population over twenty-year study period (created in pop_geo.R)
load("00_data/mun_pop_agg.Rdata")

### reduce municipalities to study area and exposure data
mun_red <- left_join(mun_pop_agg, mun_shp %>% dplyr::select(COUNTY, MUNICIPAL_, MUNICIPAL1, FIPS_MUN_C, geoms), by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1")) %>% 
  mutate(
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
         ),
         mun_fips = as.double(str_c(county_fips, FIPS_MUN_C)),
         mun_fips_c = str_c(county_fips, FIPS_MUN_C)
  ) %>% 
  dplyr::select(county_fips, MUNICIPAL_, MUNICIPAL1, FIPS_MUN_C, mun_fips, mun_fips_c, total_pop_adult, total_pop_adult_f, total_pop_adult_m, geoms) %>% 
  rename(total_pop = total_pop_adult, total_pop_f = total_pop_adult_f, total_pop_m = total_pop_adult_m)

## pfas ----

area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
  mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))

mun_pfas <- left_join(area_mun_pfas %>% dplyr::select(fips, pfas_label), mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C")) %>% 
  mutate(
    pfas = case_when(
      pfas_label == "PFAS not detected" ~ "0",
      pfas_label == "PFAS detected at 0 or less than MRL" ~ "1",
      pfas_label == "PFAS detected above 0 but below MRL" ~ "1",
      pfas_label == "PFAS detected at or above MRL" ~ "2"
    )
  ) %>% 
  dplyr::select(mun_fips_c, pfas, pfas_label)

## cancer ----

cancer19 <- read_csv("00_data/Cancer Incidence_00-19.csv")

# table(cancer19$Year)
# table(cancer19$Sex)
# table(cancer19$PrimarySite)

cancer <- read_csv("00_data/Cancer Incidence_00-20.csv")

# t <- table(cancer$FIPSCode4)
# sum(ifelse(t == 480, 1, 0))
# table(cancer$Year)
# table(cancer$Sex)
# table(cancer[ cancer$Year != 2020, "PrimarySite"])
# view(filter(cancer, FIPSCode4 == "10160000"))
# view(filter(cancer, FIPSCode4 == "10703264"))

# excel file is missing 2020 and crc data

cancer$len <- stringr::str_length(cancer$FIPSCode)  

cancer$check <- str_sub(cancer$FIPSCode, 1, 1)

cancer$FIPSCode2 <- ifelse(
  cancer$check == 0, str_sub(cancer$FIPSCode, 2),
  cancer$FIPSCode)

cancer_red <- left_join(mun_red %>% dplyr::select(mun_fips_c, total_pop, total_pop_f, total_pop_m, geoms), cancer, by = c("mun_fips_c" = "FIPSCode2")) %>% 
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
  ungroup() %>% 
  dplyr::select(County, mun_fips_c, Year, primary_site, Sex, InvasiveCount, total_pop, total_pop_m, total_pop_f, geoms)

# save(cancer_red, file = "00_data/cancer_0020_studyarea.RData")

cancer_agg <- cancer_red %>% 
  group_by(mun_fips_c, primary_site, Sex) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, InvasiveCount)) %>% 
  distinct()

cancer_agg_t <- cancer_agg %>% 
  filter(Sex == "Total") %>% 
  dplyr::select(-c(total_pop_m, total_pop_f, Sex, geoms, County)) %>% 
  st_drop_geometry()

cancer_agg_f <- cancer_agg %>% 
  filter(Sex == "Female") %>% 
  dplyr::select(-c(Sex, County, total_pop, total_pop_m, geoms)) %>% 
  rename(incidence_f = incidence) %>% 
  st_drop_geometry()

cancer_agg_m <- cancer_agg %>% 
  filter(Sex == "Male") %>% 
  dplyr::select(-c(Sex, County, total_pop, total_pop_f, geoms)) %>% 
  rename(incidence_m = incidence) %>% 
  st_drop_geometry()

ids <- distinct(cancer_red, mun_fips_c, primary_site, County, geoms)

cancer_agg_wide <- left_join(
  left_join(
    left_join(
      ids,
      cancer_agg_t,
      by = c("mun_fips_c", "primary_site")
    ),
    cancer_agg_f,
    by = c("mun_fips_c", "primary_site")
  ),
  cancer_agg_m,
  by = c("mun_fips_c", "primary_site")
)

cancer_agg_wide <- cancer_agg_wide[,c("County", "mun_fips_c", "primary_site", "incidence", "incidence_f", "incidence_m", "total_pop", "total_pop_f", "total_pop_m", "geoms")]

cancer_agg_wide$crude_incdrt100k <- case_when(
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

cancer_agg_wide$crude_incdrt100k_f <- case_when(
  cancer_agg_wide$primary_site == "Bladder" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Brain" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Colorectal" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Kidney" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Liver" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Thyroid" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000
)

cancer_agg_wide$crude_incdrt100k_m <- case_when(
  cancer_agg_wide$primary_site == "Bladder" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Brain" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Colorectal" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Kidney" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Liver" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Non-hodgkin lymphoma" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000,
  cancer_agg_wide$primary_site == "Thyroid" ~ ((cancer_agg_wide$incidence_m/cancer_agg_wide$total_pop_m))*100000
)

# save(cancer_agg_wide, file = "00_data/cancer_0020_studyarea_wide.RData")

## final analysis data ----

### merging exposure and outcome data for analyses
kidney <- left_join(mun_pfas, cancer_agg_wide %>% filter(primary_site == "Kidney"), by = "mun_fips_c") %>% 
  mutate(
    incd_rt_tert = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_tert_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_tert_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),    
    incd_rt_quint = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
  )
  

thyroid <- left_join(mun_pfas, cancer_agg_wide %>% filter(primary_site == "Thyroid"), by = "mun_fips_c") %>% 
  mutate(
    incd_rt_tert = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_tert_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_tert_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),    
    incd_rt_quint = cut(crude_incdrt100k, quantile(crude_incdrt100k, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_m = cut(crude_incdrt100k_m, quantile(crude_incdrt100k_m, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
  )

## remove intermediate data ----

# rm(list = ls()[!ls() %in% c("kidney", "study_counties", "thyroid")])

# 3. statistical analysis ----

# summary stats of PFAS levels, cancer incidence overall and by sex

### pfas ----

kidney %>% 
  tabyl(pfas) %>%
  adorn_totals() %>%
  adorn_pct_formatting(rounding = "half up", digits = 1) %>%
  knitr::kable()

mun_pfas %>% 
  tabyl(pfas_label) %>%
  adorn_totals() %>%
  adorn_pct_formatting(rounding = "half up", digits = 1) %>%
  knitr::kable()


### cancer ----

##### counts

sum(kidney$incidence)
sum(kidney$incidence_f)
sum(kidney$incidence_m)

sum(thyroid$incidence)
sum(thyroid$incidence_f)
sum(thyroid$incidence_m)

##### summary stats

(sum(kidney$incidence)/sum(kidney$total_pop))*100000
(sum(kidney$incidence_f)/sum(kidney$total_pop_f))*100000
(sum(kidney$incidence_m)/sum(kidney$total_pop_m))*100000

(sum(thyroid$incidence)/sum(thyroid$total_pop))*100000
(sum(thyroid$incidence_f)/sum(thyroid$total_pop_f))*100000
(sum(thyroid$incidence_m)/sum(thyroid$total_pop_m))*100000


summary_stats <- data.frame(
  cancer = c("Kidney","Kidney", "Kidney", "Thyroid", "Thyroid", "Thyroid"),
  pop =    c("Total", "Female", "Male", "Total", "Female", "Male"),
  mean =   c(summary(kidney$crude_incdrt100k)[4], summary(kidney$crude_incdrt100k_f)[4], summary(kidney$crude_incdrt100k_m)[4], summary(thyroid$crude_incdrt100k)[4], summary(thyroid$crude_incdrt100k_f)[4], summary(thyroid$crude_incdrt100k_m)[4]),
  sd =     c(sd(kidney$crude_incdrt100k), sd(kidney$crude_incdrt100k_f), sd(kidney$crude_incdrt100k_m), sd(thyroid$crude_incdrt100k), sd(thyroid$crude_incdrt100k_f), sd(thyroid$crude_incdrt100k_m)),
  median = c(summary(kidney$crude_incdrt100k)[3], summary(kidney$crude_incdrt100k_f)[3], summary(kidney$crude_incdrt100k_m)[3], summary(thyroid$crude_incdrt100k)[3], summary(thyroid$crude_incdrt100k_f)[3], summary(thyroid$crude_incdrt100k_m)[3]),
  q1 =     c(summary(kidney$crude_incdrt100k)[2], summary(kidney$crude_incdrt100k_f)[2], summary(kidney$crude_incdrt100k_m)[2], summary(thyroid$crude_incdrt100k)[2], summary(thyroid$crude_incdrt100k_f)[2], summary(thyroid$crude_incdrt100k_m)[2]),
  q3 =     c(summary(kidney$crude_incdrt100k)[5], summary(kidney$crude_incdrt100k_f)[5], summary(kidney$crude_incdrt100k_m)[5], summary(thyroid$crude_incdrt100k)[5], summary(thyroid$crude_incdrt100k_f)[5], summary(thyroid$crude_incdrt100k_m)[5]),
  min =    c(summary(kidney$crude_incdrt100k)[1], summary(kidney$crude_incdrt100k_f)[1], summary(kidney$crude_incdrt100k_m)[1], summary(thyroid$crude_incdrt100k)[1], summary(thyroid$crude_incdrt100k_f)[1], summary(thyroid$crude_incdrt100k_m)[1]),
  max =    c(summary(kidney$crude_incdrt100k)[6], summary(kidney$crude_incdrt100k_f)[6], summary(kidney$crude_incdrt100k_m)[6], summary(thyroid$crude_incdrt100k)[6], summary(thyroid$crude_incdrt100k_f)[6], summary(thyroid$crude_incdrt100k_m)[6])
) %>% 
  mutate(
    iqr = q3 - q1
  ) %>% 
  dplyr::select(cancer, pop, mean, sd, median, q1, q3, iqr, min, max)

### bivariate ----

kid1 <- kidney %>% 
  tabyl(incd_rt_tert, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(kidney$crude_incdrt100k, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert))

kid2 <- kidney %>% 
  tabyl(incd_rt_tert_f, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(kidney$crude_incdrt100k_f, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_f))

kid3 <- kidney %>% 
  tabyl(incd_rt_tert_m, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(kidney$crude_incdrt100k_m, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_m))

thy1 <- thyroid %>% 
  tabyl(incd_rt_tert, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(thyroid$crude_incdrt100k, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert))

thy2 <- thyroid %>% 
  tabyl(incd_rt_tert_f, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(thyroid$crude_incdrt100k_f, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_f))

thy3 <- thyroid %>% 
  tabyl(incd_rt_tert_m, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(thyroid$crude_incdrt100k_m, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_m))

kidthy_bi <- rbind(kid1, kid2, kid3, thy1, thy2, thy3)

chi_ken_stats <- data.frame(
  cancer = c("Kidney","Kidney", "Kidney", "Thyroid", "Thyroid", "Thyroid"),
  pop =    c("Total", "Female", "Male", "Total", "Female", "Male"),
  chi2 = c(chisq.test(kidney$incd_rt_tert, kidney$pfas)[[1]], chisq.test(kidney$incd_rt_tert_f, kidney$pfas)[[1]], chisq.test(kidney$incd_rt_tert_m, kidney$pfas)[[1]], chisq.test(thyroid$incd_rt_tert, thyroid$pfas)[[1]], chisq.test(thyroid$incd_rt_tert_f, thyroid$pfas)[[1]], chisq.test(thyroid$incd_rt_tert_m, thyroid$pfas)[[1]]),
  chi_pval = c(chisq.test(kidney$incd_rt_tert, kidney$pfas)[[3]], chisq.test(kidney$incd_rt_tert_f, kidney$pfas)[[3]], chisq.test(kidney$incd_rt_tert_m, kidney$pfas)[[3]], chisq.test(thyroid$incd_rt_tert, thyroid$pfas)[[3]], chisq.test(thyroid$incd_rt_tert_f, thyroid$pfas)[[3]], chisq.test(thyroid$incd_rt_tert_m, thyroid$pfas)[[3]]),
  kendall_tau = c(cor.test(x=as.numeric(kidney$incd_rt_tert), y=as.numeric(kidney$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(kidney$incd_rt_tert_f), y=as.numeric(kidney$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(kidney$incd_rt_tert_m), y=as.numeric(kidney$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(thyroid$incd_rt_tert), y=as.numeric(thyroid$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(thyroid$incd_rt_tert_f), y=as.numeric(thyroid$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(thyroid$incd_rt_tert_m), y=as.numeric(thyroid$pfas), method = 'kendall')[[4]]),
  p_trend = c(cor.test(x=as.numeric(kidney$incd_rt_tert), y=as.numeric(kidney$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(kidney$incd_rt_tert_f), y=as.numeric(kidney$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(kidney$incd_rt_tert_m), y=as.numeric(kidney$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(thyroid$incd_rt_tert), y=as.numeric(thyroid$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(thyroid$incd_rt_tert_f), y=as.numeric(thyroid$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(thyroid$incd_rt_tert_m), y=as.numeric(thyroid$pfas), method = 'kendall')[[3]])
)

# 4. spatial analysis ----

### creating neighbors matrix
nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(kidney)), queen = TRUE), zero.policy = TRUE)


### kidney ----

#### global morans i
gmi_kidney <- moran.test(kidney$incidence, nbr, zero.policy = TRUE)
gmi_kidney

gmi_kidney_f <- moran.test(kidney$incidence_f, nbr, zero.policy = TRUE)
gmi_kidney_f

gmi_kidney_m <- moran.test(kidney$incidence_m, nbr, zero.policy = TRUE)
gmi_kidney_m

global_kid <- data.frame(
  cancer = c("Kidney","Kidney", "Kidney"),
  pop =    c("Total", "Female", "Male"),
  morani = c(gmi_kidney[[1]], gmi_kidney_f[[1]], gmi_kidney_m[[1]]),
  pval = c(gmi_kidney[[2]], gmi_kidney_f[[2]], gmi_kidney_m[[2]])
)

#### local morans i
lmi_kidney <- localmoran(kidney$incidence, nbr, zero.policy = TRUE)
kidney$cluster <- attributes(lmi_kidney)$quadr$mean
kidney$cluster_sig <- ifelse(lmi_kidney[,5] < .05, kidney$cluster, NA)

lmi_kidney_f <- localmoran(kidney$incidence_f, nbr, zero.policy = TRUE)
kidney$cluster_f <- attributes(lmi_kidney_f)$quadr$mean
kidney$cluster_sig_f <- ifelse(lmi_kidney_f[,5] < .05, kidney$cluster_f, NA)

lmi_kidney_m <- localmoran(kidney$incidence_m, nbr, zero.policy = TRUE)
kidney$cluster_m <- attributes(lmi_kidney_m)$quadr$mean
kidney$cluster_sig_m <- ifelse(lmi_kidney_m[,5] < .05, kidney$cluster_m, NA)

kid_tab <- bind_rows(
  table(kidney$cluster_sig) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq),
  table(kidney$cluster_sig_f) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq),
  table(kidney$cluster_sig_m) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq)
)


### thyroid ----

#### global morans i
gmi_thyroid <- moran.test(thyroid$incidence, nbr, zero.policy = TRUE)
gmi_thyroid

gmi_thyroid_f <- moran.test(thyroid$incidence_f, nbr, zero.policy = TRUE)
gmi_thyroid_f

gmi_thyroid_m <- moran.test(thyroid$incidence_m, nbr, zero.policy = TRUE)
gmi_thyroid_m

#### local morans i
lmi_thyroid <- localmoran(thyroid$incidence, nbr, zero.policy = TRUE)
thyroid$cluster <- attributes(lmi_thyroid)$quadr$mean
thyroid$cluster_sig <- ifelse(lmi_thyroid[,5] < .05, thyroid$cluster, NA)

lmi_thyroid_f <- localmoran(thyroid$incidence_f, nbr, zero.policy = TRUE)
thyroid$cluster_f <- attributes(lmi_thyroid_f)$quadr$mean
thyroid$cluster_sig_f <- ifelse(lmi_thyroid_f[,5] < .05, thyroid$cluster_f, NA)

lmi_thyroid_m <- localmoran(thyroid$incidence_m, nbr, zero.policy = TRUE)
thyroid$cluster_m <- attributes(lmi_thyroid_m)$quadr$mean
thyroid$cluster_sig_m <- ifelse(lmi_thyroid_m[,5] < .05, thyroid$cluster_m, NA)

thy_tab <- bind_rows(
  table(thyroid$cluster_sig) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq),
  table(thyroid$cluster_sig_f) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq),
  table(thyroid$cluster_sig_m) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq)
)

kidthy_tab <- bind_rows(kid_tab, thy_tab)

table(thyroid$cluster_m, thyroid$cluster_sig_m)

global_thy <- data.frame(
  cancer = c("Thyroid","Thyroid", "Thyroid"),
  pop =    c("Total", "Female", "Male"),
  morani = c(gmi_thyroid[[1]], gmi_thyroid_f[[1]], gmi_thyroid_m[[1]]),
  pval = c(gmi_thyroid[[2]], gmi_thyroid_f[[2]], gmi_thyroid_m[[2]])
)

global_df <- rbind(global_kid, global_thy)


# 5. visualizations ----

# setting color objects
cols.blues4 <- c("white","#eff3ff","#bdd7e7","#6baed6","#08519c")
cols.blues5 <- c("white","#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d")

colors4 <- c("grey98","#bae4b3","#74c476","#31a354","#006d2c")

cols.bw4 <- c("white","grey90","grey75","grey40","black")

# adding non-study area municipalities for NA values 
nonstudy_mun <- anti_join(mun_shp, mun_red, by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1")) %>% 
  mutate(
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
  ),
  mun_fips_c = str_c(county_fips, FIPS_MUN_C)
  ) %>% 
  filter(county_fips %in% c("11", "17", "25", "29", "45", "71", "75", "77", "89", "91", "95", "107")) %>% 
  dplyr::select(mun_fips_c, geoms)

state_boundary <- tigris::states(cb = TRUE) %>% filter(STUSPS %in% c("PA", "MD", "DE", "NJ", "NY"))

study_county_outline <- st_union(study_counties)

pa_counties <- tigris::counties(state = "PA", cb = TRUE)

## study area ----

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = kidney, aes(geometry = geoms), fill = "grey90", color = "grey90") +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values = "Municipalities") +
  guides(
    fill = "none",
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = kidney, aes(geometry = geoms, fill = "mun", color = "mun")) +
    scale_fill_manual(values = c("mun" = "grey90"), labels = "Municipalities") +
    scale_color_manual(values = c("mun" = "grey90")) +
    guides(
      fill = guide_legend(title = "Study Area"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  )

ggsave("02_output/study_area.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/study_area.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/study_area.png"
)


## kidney ----

### incidence - quartile - total - b&w ----
# quantile(kidney$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 403.6','403.6 - 727.4', '727.4 - 1408.4', '1408.4 - 13472.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
    ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 403.6','403.6 - 727.4', '727.4 - 1408.4', '1408.4 - 13472.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
    ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_total_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_total_bw.png")
      ),
    "black", "5x5"
    ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_total_bw.png"
)

### incidence - quartile - total - color ----
# quantile(kidney$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 403.6','403.6 - 727.4', '727.4 - 1408.4', '1408.4 - 13472.9'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 403.6','403.6 - 727.4', '727.4 - 1408.4', '1408.4 - 13472.9'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_total_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_total_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_total_color.png"
)


### incidence - quartile - female - b&w ----
# quantile(kidney$crude_incdrt100k_f, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 255.9','255.9 - 492.9', '492.9 - 968.2', '968.2 - 11290.3'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 255.9','255.9 - 492.9', '492.9 - 968.2', '968.2 - 11290.3'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_female_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_female_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_female_bw.png"
)


### incidence - quartile - female - color ----
# quantile(kidney$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 255.9','255.9 - 492.9', '492.9 - 968.2', '968.2 - 11290.3'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 255.9','255.9 - 492.9', '492.9 - 968.2', '968.2 - 11290.3'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_female_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_female_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_female_color.png"
)


### incidence - quartile - male - b&w ----
# quantile(kidney$crude_incdrt100k_m, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 523.9','523.9 - 951.4', '951.4 - 1849.9', '1849.9 - 15934.7'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", 
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 523.9','523.9 - 951.4', '951.4 - 1849.9', '1849.9 - 15934.7'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_male_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_male_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_male_bw.png"
)



### incidence - quartile - male - color ----
# quantile(kidney$crude_incdrt100k_m, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = kidney, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 523.9','523.9 - 951.4', '951.4 - 1849.9', '1849.9 - 15934.7'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(kidney, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 523.9','523.9 - 951.4', '951.4 - 1849.9', '1849.9 - 15934.7'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/kidney_incd_male_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_male_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/kidney_incd_male_color.png"
)

## thyroid ----

### incidence - quartile - total - b&w ----
# quantile(thyroid$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 389.6','389.6 - 622.3', '622.3 - 1468.3', '1468.3 - 13148.5'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 389.6','389.6 - 622.3', '622.3 - 1468.3', '1468.3 - 13148.5'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_total_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_total_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_total_bw.png"
)

### incidence - quartile - total - color ----
# quantile(thyroid$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 389.6','389.6 - 622.3', '622.3 - 1468.3', '1468.3 - 13148.5'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart), color = as.factor(incd_rt_quart))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 389.6','389.6 - 622.3', '622.3 - 1468.3', '1468.3 - 13148.5'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_total_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_total_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_total_color.png"
)


### incidence - quartile - female - b&w ----
# quantile(thyroid$crude_incdrt100k_f, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 554.8','554.8 - 977.6', '977.6 - 2052.3', '2052.3 - 19632.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 554.8','554.8 - 977.6', '977.6 - 2052.3', '2052.3 - 19632.9'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_female_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_female_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_female_bw.png"
)


### incidence - quartile - female - color ----
# quantile(thyroid$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 554.8','554.8 - 977.6', '977.6 - 2052.3', '2052.3 - 19632.9'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 554.8','554.8 - 977.6', '977.6 - 2052.3', '2052.3 - 19632.9'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_female_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_female_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_female_color.png"
)


### incidence - quartile - male - b&w ----
# quantile(thyroid$crude_incdrt100k_m, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "grey90", color = "white", linewidth = 2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 140.8','140.8 - 331.4', '331.4 - 744.3', '744.3 - 6494.4'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
  guides(
    fill = "none", 
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="white"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data','0.0 - 140.8','140.8 - 331.4', '331.4 - 744.3', '744.3 - 6494.4'), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    scale_color_manual(values=cols.bw4, limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_male_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_male_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_male_bw.png"
)



### incidence - quartile - male - color ----
# quantile(thyroid$crude_incdrt100k_m, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = thyroid, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
  geom_sf(data = pa_counties, fill = "transparent", color = "black", linewidth = 0.175) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(st_bbox(pa_counties)[1], st_bbox(study_counties)[3]),
    ylim = c(st_bbox(pa_counties)[2], st_bbox(study_counties)[4]+1),
    crs = 4269) +
  theme(
    text = element_text(family = "Arial"),
    plot.background = element_rect(fill="white", color = "black", linewidth = 0.75),
    panel.background = element_rect(fill="transparent"),
    plot.title = element_text(hjust = 0.5, vjust = -10, size = 12),
    plot.margin = margin(t = 15, r = 0, b = 0, l = 0),
    panel.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

main <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(data = state_boundary, fill = "#B4D79E", color = "black", linewidth = 0.2) +
  geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.2) +
  geom_sf(data = study_county_outline, fill = "transparent", color = "black", linewidth = 0.75) +
  geom_sf_text(data = state_boundary, 
               aes(label = NAME),
               nudge_x = c(0.25, 0, 1.4, 0, 0),
               nudge_y = c(0.79, 0, 0.15, 0, 0),
               size = 3) + 
  coord_sf(
    xlim = c(st_bbox(study_counties)[1], st_bbox(study_counties)[3]+0.75),
    ylim = c(st_bbox(study_counties)[2]-0.1, st_bbox(study_counties)[4]),
    crs = 4269
  ) +
  scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 140.8','140.8 - 331.4', '331.4 - 744.3', '744.3 - 6494.4'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
  guides(
    fill = "none", #guide_legend(title = "Rate per 100,000"),
    color = "none"
  ) +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill="#BEE8FF"), #, color = "black", linewidth = 2),
    plot.background = element_rect(fill="transparent", color = "transparent"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) + 
  ggspatial::annotation_scale(
    location = "br", unit_category = "imperial", style = "ticks",
    pad_x = unit(1, "in"), pad_y = unit(0.25, "in")) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(.03, "in"), pad_y = unit(0.2, "in"),
    height = unit(0.5, "in"), width = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

main_legend <- get_legend(
  ggplot() +
    geom_sf(data = bind_rows(thyroid, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_m), color = as.factor(incd_rt_quart_m))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data','0.0 - 140.8','140.8 - 331.4', '331.4 - 744.3', '744.3 - 6494.4'), limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    scale_color_manual(values=colors4, limits= c("NA","1","2","3","4"), na.value="grey98", drop=FALSE) +
    guides(
      fill = guide_legend(title = "Rate per 100,000"),
      color = "none"
    ) + theme(legend.background = element_rect(color = "black"),
              legend.key.size = unit(0.5, 'cm'), 
              legend.key.height = unit(0.5, 'cm'), 
              legend.key.width = unit(0.5, 'cm'),
              legend.title = element_text(size=8), 
              legend.text = element_text(size=6))
)

w <- 0.6

ggdraw(clip = "on") +
  draw_plot(main) +
  draw_plot(
    inset,
    width = w, height = w/2,
    x = 0.48, y = 0.68
  ) +
  draw_plot(
    main_legend,
    width = 0.01, height = 0.01,
    x = 0.78, y = 0.485
  ) +
  annotate(
    "text",
    label = "Delaware",
    x = 0.42, y = 0.09, angle = 50,
    family = "Arial", size = 3
  ) +
  annotate(
    "text",
    label = "Pennsylvania",
    x = 0.775, y = 0.95,
    family = "Arial", size = 3.755
  ) +
  annotate(
    "text",
    label = "New Jersey",
    x = 0.7, y = 0.2,
    family = "Arial", size = 3
  )

ggsave("02_output/thyroid_incd_male_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_male_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/thyroid_incd_male_color.png"
)







