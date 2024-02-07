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
                   COUNTY == "53" ~ "105"
                  )
)

mun_red$mun_fips <- as.double(str_c(mun_red$county_fips,mun_red$FIPS_MUN_C))

# population data by municipality

mun_pop <- read_csv("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/mun_pop.csv") %>% 
  dplyr::select(-geoms)

mun_pop$total_pop <- round(rowSums(mun_pop[,4:33]),0)

mun_pop$MUNICIPAL_ <- as.character(mun_pop$MUNICIPAL_)
  
mun_red <- left_join(mun_red, mun_pop %>% dplyr::select(COUNTY, MUNICIPAL_, MUNICIPAL1, total_pop), by = c("COUNTY", "MUNICIPAL_", "MUNICIPAL1"))

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

str(cancer)

table(cancer$PrimarySite)
table(cancer$County)
table(cancer$Year)

head(cancer)

view(cancer)

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

cancer_red <- left_join(mun_red %>% dplyr::select(mun_fips_c, total_pop, geoms), cancer, by = c("mun_fips_c" = "FIPSCode2")) %>% 
  dplyr::select(-c(FIPSCode, len, wid))




# THYROID

# total
thyroid_agg_tot <- cancer_red %>% 
  filter(PrimarySite == "Thyroid" & Sex == "Total") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  unique() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(thyroid_agg_tot$incidence)/sum(thyroid_agg_tot$total_pop)

thyroid_agg_tot$exp <- round(thyroid_agg_tot$total_pop*overall_rate,0)

# male
thyroid_agg_m <- cancer_red %>% 
  filter(PrimarySite == "Thyroid" & Sex == "Male") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(thyroid_agg_m$incidence)/sum(thyroid_agg_m$total_pop)

thyroid_agg_m$exp <- round(thyroid_agg_m$total_pop*overall_rate,0)

# female
thyroid_agg_f <- cancer_red %>% 
  filter(PrimarySite == "Thyroid" & Sex == "Female") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(thyroid_agg_f$incidence)/sum(thyroid_agg_f$total_pop)

thyroid_agg_f$exp <- round(thyroid_agg_f$total_pop*overall_rate,0)

# KIDNEY

# total
kidney_agg_tot <- cancer_red %>% 
  filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Total") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(kidney_agg_tot$incidence)/sum(kidney_agg_tot$total_pop)

kidney_agg_tot$exp <- round(kidney_agg_tot$total_pop*overall_rate,0)

# male
kidney_agg_m <- cancer_red %>% 
  filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Male") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(kidney_agg_m$incidence)/sum(kidney_agg_m$total_pop)

kidney_agg_m$exp <- round(kidney_agg_m$total_pop*overall_rate,0)

# female
kidney_agg_f <- cancer_red %>% 
  filter(PrimarySite == "Kidney and Renal Pelvis" & Sex == "Female") %>% 
  group_by(mun_fips_c) %>% 
  mutate(incidence = sum(InvasiveCount)) %>% 
  ungroup() %>% 
  dplyr::select(-c(Year, Sex, InvasiveCount)) %>% 
  mutate(incd_rate_100 = round((incidence/total_pop)*1000,1)) %>% 
  distinct()

overall_rate <- sum(kidney_agg_f$incidence)/sum(kidney_agg_f$total_pop)

kidney_agg_f$exp <- round(kidney_agg_f$total_pop*overall_rate,0)

# 5. local morans i ----

# THYROID

nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(thyroid_agg_tot)), queen = TRUE), zero.policy = TRUE)

thyroid_agg_tot_lmi <- localmoran(thyroid_agg_tot$incidence, nbr, zero.policy = TRUE)
thyroid_agg_tot_lmi_geo <- cbind(thyroid_agg_tot,thyroid_agg_tot_lmi)
thyroid_agg_tot_lmi_geo$cluster <- attributes(thyroid_agg_tot_lmi)$quadr$mean

colnames(thyroid_agg_tot_lmi_geo) <- c("mun_fips", "total_pop", "geoms", "PrimarySite", "County", "MCD", "incidence", "incd_rate_100", "exp", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")

thyroid_agg_tot_lmi_geo$cluster_sig <- ifelse(thyroid_agg_tot_lmi_geo$p_val < .05, thyroid_agg_tot_lmi_geo$cluster, NA)

# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.

# KIDNEY

nbr <- nb2listw(poly2nb(st_make_valid(st_as_sf(kidney_agg_tot)), queen = TRUE), zero.policy = TRUE)

kidney_agg_tot_lmi <- localmoran(kidney_agg_tot$incidence, nbr, zero.policy = TRUE)
kidney_agg_tot_lmi_geo <- cbind(kidney_agg_tot,kidney_agg_tot_lmi)
kidney_agg_tot_lmi_geo$cluster <- attributes(kidney_agg_tot_lmi)$quadr$mean

colnames(kidney_agg_tot_lmi_geo) <- c("mun_fips", "total_pop", "geoms", "PrimarySite", "County", "MCD", "incidence", "incd_rate_100", "exp", "Ii", "E.Ii", "Var.Ii", "Z.Ii", "p_val", "cluster")

kidney_agg_tot_lmi_geo$cluster_sig <- ifelse(kidney_agg_tot_lmi_geo$p_val < .05, kidney_agg_tot_lmi_geo$cluster, NA)

# 3: Low-High (low), 4: High-High (high). No significant High-Low or Low-Low municipalities.








# 6. categorical variables for bivariate mapping ----

# incidence rate tertiles
thyroid_agg_tot_lmi_geo$incd_rt_tert <- cut(thyroid_agg_tot_lmi_geo$incd_rate_100, quantile(thyroid_agg_tot_lmi_geo$incd_rate_100, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)

kidney_agg_tot_lmi_geo$incd_rt_tert <- cut(kidney_agg_tot_lmi_geo$incd_rate_100, quantile(kidney_agg_tot_lmi_geo$incd_rate_100, probs = c(0,.33,.66,1)), label = FALSE, include.lowest = TRUE)





# 7. descriptive statistics ----

# THYROID

# incidence

summary(thyroid_agg_tot_lmi_geo$incd_rate_100)

tapply(thyroid_agg_tot_lmi_geo$incd_rate_100, thyroid_agg_tot_lmi_geo$County, summary)

thyroid_agg_tot_lmi_geo$incd_rate_100.q <- cut(thyroid_agg_tot_lmi_geo$incd_rate_100, quantile(thyroid_agg_tot_lmi_geo$incd_rate_100, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)

table(thyroid_agg_tot_lmi_geo$County, thyroid_agg_tot_lmi_geo$incd_rate_100.q)

overall_rate <- (sum(thyroid_agg_tot_lmi_geo$incidence)/sum(thyroid_agg_tot_lmi_geo$total_pop))*100000
overall_rate

thyroid_agg_tot_lmi_geo2 %>% 
  group_by(pfas_tri) %>% 
  summarize(i_sum = sum(incidence))

(7922/sum(thyroid_agg_tot_lmi_geo2$incidence))*100



quantile(thyroid_agg_tot$incd_rate_100, probs = c(0,.25,.5,.75,1))

# lmi/global?

gmi_thyroid <- moran.test(thyroid_agg_tot_lmi_geo$incidence, nbr, zero.policy = TRUE)
gmi_thyroid

(length(thyroid_agg_tot_lmi_geo$cluster_sig)-sum(is.na(thyroid_agg_tot_lmi_geo$cluster_sig)))/length(thyroid_agg_tot_lmi_geo$cluster_sig)

sum(thyroid_agg_tot_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
sum(thyroid_agg_tot_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high

sum(thyroid_agg_tot_lmi_geo$cluster_sig == 3, na.rm=TRUE)+sum(thyroid_agg_tot_lmi_geo$cluster_sig == 4, na.rm=TRUE)

(57/378)*100
(33/378)*100
(34/378)*100

# incidence x pfas

table(thyroid_agg_tot_lmi_geo2$incd_rt_tert, thyroid_agg_tot_lmi_geo2$pfas_tri)

quantile(thyroid_agg_tot$incd_rate_100, probs = c(0,.33,.66,1))

# KIDNEY

# incidence

summary(kidney_agg_tot_lmi_geo$incd_rate_100)

tapply(kidney_agg_tot_lmi_geo$incd_rate_100, kidney_agg_tot_lmi_geo$County, summary)

kidney_agg_tot_lmi_geo$incd_rate_100.q <- cut(kidney_agg_tot_lmi_geo$incd_rate_100, quantile(kidney_agg_tot_lmi_geo$incd_rate_100, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)

table(kidney_agg_tot_lmi_geo$County, kidney_agg_tot_lmi_geo$incd_rate_100.q)

quantile(kidney_agg_tot$incd_rate_100, probs = c(0,.25,.5,.75,1))

overall_rate <- (sum(kidney_agg_tot_lmi_geo$incidence)/sum(kidney_agg_tot_lmi_geo$total_pop))*100000
overall_rate

kidney_agg_tot_lmi_geo2 %>% 
  group_by(pfas_tri) %>% 
  summarize(i_sum = sum(incidence))

(8112/sum(kidney_agg_tot_lmi_geo2$incidence))*100




# lmi/global?

gmi_kidney <- moran.test(kidney_agg_tot_lmi_geo$incidence, nbr, zero.policy = TRUE)
gmi_kidney

(length(kidney_agg_tot_lmi_geo$cluster_sig)-sum(is.na(kidney_agg_tot_lmi_geo$cluster_sig)))/length(kidney_agg_tot_lmi_geo$cluster_sig)

sum(kidney_agg_tot_lmi_geo$cluster_sig == 3, na.rm=TRUE) # sig low-high
sum(kidney_agg_tot_lmi_geo$cluster_sig == 4, na.rm=TRUE) # sig high-high

# incidence x pfas

table(kidney_agg_tot_lmi_geo2$incd_rt_tert, kidney_agg_tot_lmi_geo2$pfas_tri)

quantile(kidney_agg_tot$incd_rate_100, probs = c(0,.33,.66,1))




# 8. maps ----

# THYROID 

# incidence

thyroid_agg_tot$incd.q <- cut(thyroid_agg_tot$incidence, quantile(thyroid_agg_tot$incidence, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)
thyroid_agg_tot$incd_rate_100.q <- cut(thyroid_agg_tot$incd_rate_100, quantile(thyroid_agg_tot$incd_rate_100, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)

cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = thyroid_agg_tot, aes(geometry = geoms, fill = as.factor(incd_rate_100.q), color = as.factor(incd_rate_100.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues, labels = c('[0-25%]','[25-50%]', '[50-75%]', '[75-100%]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quartiles of thyroid cancer incidence per 1000 from 2000 to 2019") +
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

ggsave("02_output/thyroid_incidper1000_municipality.png")

# significant clusters

ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = thyroid_agg_tot_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
  labs(caption = "Local positive and negative statistically significant clustering of thyroid cancer\nincidence from 2000 to 2019 at the muncipality-level") +
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

ggsave("02_output/thyroid_lisa_municipality.png")

# bivariate

area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
  mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))

mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips_c), by = c("fips" = "FIPS_MUN_C"))

thyroid_agg_tot_lmi_geo2 <- left_join(thyroid_agg_tot_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )

thyroid_agg_tot_lmi_geo2$incd_rt_tert <- as.factor(thyroid_agg_tot_lmi_geo2$incd_rt_tert)
thyroid_agg_tot_lmi_geo2$pfas_tri <- as.factor(thyroid_agg_tot_lmi_geo2$pfas_tri)

bivar_thyroid_pfas <- bi_class(thyroid_agg_tot_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)

# create map
pa <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
  geom_sf(data = bivar_thyroid_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
  geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  bi_scale_color(pal = "DkBlue2", dim = 3) +
  labs(caption = "Bivariate map of thyroid cancer incidence and pfas contamination") +
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

ggsave("02_output/bivar_thyroid_pfas.png")












# KIDNEY 

# incidence

kidney_agg_tot$incd_rate_100.q <- cut(kidney_agg_tot$incd_rate_100, quantile(kidney_agg_tot$incd_rate_100, probs = c(0,.25,.5,.75,1)), label = FALSE, include.lowest = TRUE)

cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#08519c")

ggplot() +
  geom_sf(data = study_counties, fill = "grey97", color = "black", linewidth = 0.6) +
  geom_sf(data = kidney_agg_tot, aes(geometry = geoms, fill = as.factor(incd_rate_100.q), color = as.factor(incd_rate_100.q))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=cols.blues, labels = c('[0-25%]','[25-50%]', '[50-75%]', '[75-100%]'), limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  scale_color_manual(values=cols.blues, limits= c("1","2","3","4"), na.value="grey", drop=FALSE) +
  labs(caption = "Muncipality-level quartiles of kidney cancer incidence per 1000 from 2000 to 2019") +
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

ggsave("02_output/kidney_incidper1000_municipality.png")

# significant clusters

ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "black", linewidth = 0.6) +
  geom_sf(data = kidney_agg_tot_lmi_geo, aes(geometry = geoms, fill = as.factor(cluster_sig), color = as.factor(cluster_sig))) +
  geom_sf(data = study_counties, fill = "transparent", color = "black", linewidth = 0.6) +
  scale_fill_manual(values=c("#0571b0", "#ca0020"), labels = c('Low-High (Negative)','High-High (Positive)'), limits= c("3","4"), na.value="grey90", drop=FALSE) +
  scale_color_manual(values=c("#0571b0", "#ca0020"), na.value="grey90", drop=FALSE) +
  labs(caption = "Local positive and negative statistically significant clustering of kidney cancer\nincidence from 2000 to 2019 at the muncipality-level") +
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

ggsave("02_output/kidney_lisa_municipality.png")

# bivariate

area_mun_pfas <- read_excel("~/Documents/Personal/Work/Jobs/PFAS + Cancer/Population Estimates/01_data/area_mun_pfas.xlsx") %>% 
  mutate(fips = str_pad(FIPS_MUN_C, 5, pad = "0"))

mun_pfas <- left_join(area_mun_pfas, mun_red %>% dplyr::select(FIPS_MUN_C, mun_fips), by = c("fips" = "FIPS_MUN_C"))

mun_pfas$mun_fips_c <- as.character(mun_pfas$mun_fips)

kidney_agg_tot_lmi_geo2 <- left_join(kidney_agg_tot_lmi_geo, mun_pfas %>% dplyr::select(mun_fips_c, pfas_final), by = c("mun_fips" = "mun_fips_c")) %>% 
  mutate(
    pfas_tri = case_when(
      pfas_final == 0 ~ "1",
      pfas_final %in% c(1, 2) ~ "2",
      pfas_final == 3 ~ "3"
    )
  )

kidney_agg_tot_lmi_geo2$incd_rt_tert <- as.factor(kidney_agg_tot_lmi_geo2$incd_rt_tert)
kidney_agg_tot_lmi_geo2$pfas_tri <- as.factor(kidney_agg_tot_lmi_geo2$pfas_tri)

bivar_kidney_pfas <- bi_class(kidney_agg_tot_lmi_geo2, x = incd_rt_tert, y = pfas_tri, dim = 3)

# create map
pa <- ggplot() +
  geom_sf(data = study_counties, fill = "white", color = "grey25", size = 0.5) +
  geom_sf(data = bivar_kidney_pfas, mapping = aes(fill = bi_class, color = bi_class, geometry = geoms), size = 0.1, show.legend = FALSE) +
  geom_sf(data = study_counties, fill = "transparent", color = "grey25", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  bi_scale_color(pal = "DkBlue2", dim = 3) +
  labs(caption = "Bivariate map of kidney cancer incidence and pfas contamination") +
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

ggsave("02_output/bivar_kidney_pfas.png")














