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

cancer <- read_csv("00_data/Cancer Incidence_00-20.csv")

cancer$len <- stringr::str_length(cancer$FIPSCode)  

cancer$FIPSCode2 <- ifelse(
  cancer$len == 8, str_sub(cancer$FIPSCode, 2),
  cancer$FIPSCode)

cancer_red <- left_join(mun_red %>% dplyr::select(mun_fips_c, total_pop, total_pop_f, total_pop_m, geoms), cancer, by = c("mun_fips_c" = "FIPSCode")) %>% 
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
  dplyr::select(County, mun_fips_c, Year, primary_site, Sex, InvasiveCount, total_pop, total_pop_m, total_pop_f, geoms)

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

cancer_agg_wide$crude_incdrt100k_f <- case_when(
  cancer_agg_wide$primary_site == "Breast" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Endometrial" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000,
  cancer_agg_wide$primary_site == "Ovarian" ~ ((cancer_agg_wide$incidence_f/cancer_agg_wide$total_pop_f))*100000
)

## final analysis data ----

### merging exposure and outcome data for analyses
ovarian <- left_join(mun_pfas, cancer_agg_wide %>% filter(primary_site == "Ovarian"), by = "mun_fips_c") %>% 
  mutate(
    incd_rt_tert_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
  )
  
breast <- left_join(mun_pfas, cancer_agg_wide %>% filter(primary_site == "Breast"), by = "mun_fips_c") %>% 
  mutate(
    incd_rt_tert_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
  )

endometrial <- left_join(mun_pfas, cancer_agg_wide %>% filter(primary_site == "Endometrial"), by = "mun_fips_c") %>% 
  mutate(
    incd_rt_tert_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quart_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE),
    incd_rt_quint_f = cut(crude_incdrt100k_f, quantile(crude_incdrt100k_f, probs = c(0,.2,.4,.6,.8,1)), label = FALSE, include.lowest = TRUE),
  )

## remove intermediate data ----

# rm(list = ls()[!ls() %in% c("ovarian", "breast", "endometrial", "study_counties", "mun_pfas", "mun_shp", "mun_red")])

# 3. statistical analysis ----

# summary stats of PFAS levels, cancer incidence overall and by sex

### pfas ----

ovarian %>% 
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

sum(breast$incidence_f)
sum(endometrial$incidence_f)
sum(ovarian$incidence_f)

##### summary stats

(sum(breast$incidence_f)/sum(breast$total_pop_f))*100000
(sum(endometrial$incidence_f)/sum(endometrial$total_pop_f))*100000
(sum(ovarian$incidence_f)/sum(ovarian$total_pop_f))*100000

summary_stats <- data.frame(
  cancer = c("Breast","Endometrial", "Ovarian"),
  mean =   c(summary(breast$crude_incdrt100k_f)[4], summary(endometrial$crude_incdrt100k_f)[4], summary(ovarian$crude_incdrt100k_f)[4]),
  sd =     c(     sd(breast$crude_incdrt100k_f),         sd(endometrial$crude_incdrt100k_f),         sd(ovarian$crude_incdrt100k_f)),
  median = c(summary(breast$crude_incdrt100k_f)[3], summary(endometrial$crude_incdrt100k_f)[3], summary(ovarian$crude_incdrt100k_f)[3]),
  q1 =     c(summary(breast$crude_incdrt100k_f)[2], summary(endometrial$crude_incdrt100k_f)[2], summary(ovarian$crude_incdrt100k_f)[2]),
  q3 =     c(summary(breast$crude_incdrt100k_f)[5], summary(endometrial$crude_incdrt100k_f)[5], summary(ovarian$crude_incdrt100k_f)[5]),
  min =    c(summary(breast$crude_incdrt100k_f)[1], summary(endometrial$crude_incdrt100k_f)[1], summary(ovarian$crude_incdrt100k_f)[1]),
  max =    c(summary(breast$crude_incdrt100k_f)[6], summary(endometrial$crude_incdrt100k_f)[6], summary(ovarian$crude_incdrt100k_f)[6])
) %>% 
  mutate(
    iqr = q3 - q1
  ) %>% 
  dplyr::select(cancer, mean, sd, median, q1, q3, iqr, min, max)

### bivariate ----

breast_bi <- breast %>% 
  tabyl(incd_rt_tert_f, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(breast$crude_incdrt100k_f, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_f))

endometrial_bi <- endometrial %>% 
  tabyl(incd_rt_tert_f, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(endometrial$crude_incdrt100k_f, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_f))

ovarian_bi <- ovarian %>% 
  tabyl(incd_rt_tert_f, pfas) %>% 
  as.data.frame() %>% 
  mutate(rates = quantile(ovarian$crude_incdrt100k_f, probs = c(.33,.66,1))) %>% 
  dplyr::select(-c(incd_rt_tert_f))

bi <- rbind(breast_bi, endometrial_bi, ovarian_bi)

chi_ken_stats <- data.frame(
  cancer = c("Breast","Endometrial", "Ovarian"),
  chi2 =        c(chisq.test(breast$incd_rt_tert_f, breast$pfas)[[1]], chisq.test(endometrial$incd_rt_tert_f, endometrial$pfas)[[1]], chisq.test(ovarian$incd_rt_tert_f, ovarian$pfas)[[1]]),
  chi_pval =    c(chisq.test(breast$incd_rt_tert_f, breast$pfas)[[3]], chisq.test(endometrial$incd_rt_tert_f, endometrial$pfas)[[3]], chisq.test(ovarian$incd_rt_tert_f, ovarian$pfas)[[3]]),
  kendall_tau = c(cor.test(x=as.numeric(breast$incd_rt_tert_f), y=as.numeric(breast$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(endometrial$incd_rt_tert_f), y=as.numeric(endometrial$pfas), method = 'kendall')[[4]], cor.test(x=as.numeric(ovarian$incd_rt_tert_f), y=as.numeric(ovarian$pfas), method = 'kendall')[[4]]),
  p_trend =     c(cor.test(x=as.numeric(breast$incd_rt_tert_f), y=as.numeric(breast$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(endometrial$incd_rt_tert_f), y=as.numeric(endometrial$pfas), method = 'kendall')[[3]], cor.test(x=as.numeric(ovarian$incd_rt_tert_f), y=as.numeric(ovarian$pfas), method = 'kendall')[[3]])
)

# 4. spatial analysis ----

### creating neighbors matrix
nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(breast)), queen = TRUE), zero.policy = TRUE)


### breast ----

#### global morans i
gmi_breast_f <- moran.test(breast$incidence_f, nbr, zero.policy = TRUE)
gmi_breast_f

global_breast <- data.frame(
  cancer = "breast",
  morani = gmi_breast_f[[1]],
  pval = gmi_breast_f[[2]]
)

#### local morans i
lmi_breast_f <- localmoran(breast$incidence_f, nbr, zero.policy = TRUE)
breast$cluster_f <- attributes(lmi_breast_f)$quadr$mean
breast$cluster_sig_f <- ifelse(lmi_breast_f[,5] < .05, breast$cluster_f, NA)

breast_tab <- table(breast$cluster_sig_f) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq)


### endometrial ----

#### global morans i
gmi_endometrial_f <- moran.test(endometrial$incidence_f, nbr, zero.policy = TRUE)
gmi_endometrial_f

global_endometrial <- data.frame(
  cancer = "endometrial",
  morani = gmi_endometrial_f[[1]],
  pval = gmi_endometrial_f[[2]]
)

#### local morans i
lmi_endometrial_f <- localmoran(endometrial$incidence_f, nbr, zero.policy = TRUE)
endometrial$cluster_f <- attributes(lmi_endometrial_f)$quadr$mean
endometrial$cluster_sig_f <- ifelse(lmi_endometrial_f[,5] < .05, endometrial$cluster_f, NA)

endometrial_tab <- table(endometrial$cluster_sig_f) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq)


### ovarian ----

#### global morans i
gmi_ovarian_f <- moran.test(ovarian$incidence_f, nbr, zero.policy = TRUE)
gmi_ovarian_f

global_ovarian <- data.frame(
  cancer = "ovarian",
  morani = gmi_ovarian_f[[1]],
  pval = gmi_ovarian_f[[2]]
)

#### local morans i
lmi_ovarian_f <- localmoran(ovarian$incidence_f, nbr, zero.policy = TRUE)
ovarian$cluster_f <- attributes(lmi_ovarian_f)$quadr$mean
ovarian$cluster_sig_f <- ifelse(lmi_ovarian_f[,5] < .05, ovarian$cluster_f, NA)

ovarian_tab <- table(ovarian$cluster_sig_f) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq)


global_df <- rbind(global_breast, global_endometrial, global_ovarian)


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
  geom_sf(data = breast, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = breast, aes(geometry = geoms), fill = "grey90", color = "grey90") +
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
    geom_sf(data = breast, aes(geometry = geoms, fill = "mun", color = "mun")) +
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


## breast ----

### incidence - quartile - b&w ----
q <- quantile(breast$crude_incdrt100k_f, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = breast, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(breast, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(breast, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/breast_incd_female_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/breast_incd_female_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/breast_incd_female_bw.png"
)


### incidence - quartile - color ----
# quantile(breast$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = breast, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(breast, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(breast, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/breast_incd_female_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/breast_incd_female_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/breast_incd_female_color.png"
)




## endometrial ----

### incidence - quartile - b&w ----
q <- quantile(endometrial$crude_incdrt100k_f, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = endometrial, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(endometrial, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(endometrial, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/endometrial_incd_female_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/endometrial_incd_female_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/endometrial_incd_female_bw.png"
)


### incidence - quartile - color ----
# quantile(endometrial$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = endometrial, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(endometrial, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(endometrial, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/endometrial_incd_female_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/endometrial_incd_female_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/endometrial_incd_female_color.png"
)



## ovarian ----

### incidence - quartile - b&w ----
q <- quantile(ovarian$crude_incdrt100k_f, probs = c(0,.25,.5,.75,1))

inset <- ggplot() +
  geom_sf(data = ovarian, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=cols.bw4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/ovarian_incd_female_bw.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/ovarian_incd_female_bw.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/ovarian_incd_female_bw.png"
)


### incidence - quartile - color ----
# quantile(ovarian$crude_incdrt100k, probs = c(0,.25,.5,.75,1))

# Colors:
#   land: #B4D79E
#   water: #BEE8FF 

inset <- ggplot() +
  geom_sf(data = ovarian, fill = "grey90", color = "grey90", aes(geometry = geoms)) +
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
  geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
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
  scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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
    geom_sf(data = bind_rows(ovarian, nonstudy_mun), aes(geometry = geoms, fill = as.factor(incd_rt_quart_f), color = as.factor(incd_rt_quart_f))) +
    scale_fill_manual(values=colors4, labels = c('No Exposure Data',paste0("0.00 - ",round(q[[2]],2)),paste0(round(q[[2]],2)," - ",round(q[[3]],2)), paste0(round(q[[3]],2)," - ",round(q[[4]],2)), paste0(round(q[[4]],2)," - ",round(q[[5]],2))), limits= c("NA","1","2","3","4"), na.value="white", drop=FALSE) +
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

ggsave("02_output/ovarian_incd_female_color.png", height = 4, width = 6, units = "in")

image_write(
  image_border(
    image_trim(
      image_read("/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/ovarian_incd_female_color.png")
    ),
    "black", "5x5"
  ),
  path = "/Users/rsnead91/Documents/Personal/Work/Jobs/PFAS + Cancer/PFAS_sites/02_output/ovarian_incd_female_color.png"
)


