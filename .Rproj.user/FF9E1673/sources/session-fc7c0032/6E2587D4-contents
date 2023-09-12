
# libraries ----

if (!require("pacman")) install.packages("pacman")

pacman::p_load('tidytuesdayR','tidyverse','skimr','ggraph','igraph','viridis')

# data ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 37)

all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity

# explore data ----

skim(all_countries)
glimpse(all_countries)

tapply(all_countries$hoursPerDayCombined, all_countries$Subcategory, sd)

# skim(country_regions)

skim(global_economic_activity)
glimpse(global_economic_activity)
view(global_economic_activity)

skim(global_human_day)
glimpse(global_human_day)
view(global_human_day)
# sum(global_human_day$hoursPerDay)


# data manipulation ----

all_countries2 <- merge(x = all_countries, y = dplyr::select(country_regions, -region_code), by = "country_iso3", all.X = TRUE) %>% 
  mutate(
    continent = case_when(
      grepl("AF", region_code) ~ "Africa",
      grepl("AS", region_code) ~ "Asia",
      grepl("EU", region_code) ~ "Europe",
      grepl("AM", region_code) ~ "America",
      grepl("ANZ", region_code) ~ "Australia"                             
    ),
    world = "World",
    graph_names1 = paste0(world),
    graph_names2 = paste0(world,".",continent),
    graph_names3 = paste0(world,".",continent,".",region_code),
    graph_names4 = paste0(world,".",continent,".",region_code,".",country_iso3),
    name = country_name
  ) %>% 
  drop_na(alt_country_name)

sub_region <- dplyr::select(
                all_countries2,
                graph_names3, graph_names4 #alt_country_name
              ) %>%
              rename(
                from = graph_names3,
                to = graph_names4
              )

continent <- unique(
              dplyr::select(
                all_countries2,
                graph_names2, graph_names3
              ) %>%
              rename(
                from = graph_names2,
                to = graph_names3
              )
            )

world <- unique(
          dplyr::select(
            all_countries2,
            graph_names1, graph_names2
          ) %>%
          rename(
            from = graph_names1,
            to = graph_names2
          )
        )

edges <- rbind(world, continent, sub_region)


# Schooling & research (good non-centered)
# Sleep & bedrest (only good non-centered)
# Food growth & collection (non-centered is okay)
# Food preparation (non-centered good)
# Social

vert_prep <- all_countries2 %>% 
              filter(Subcategory == "Schooling & research") %>% 
              # mutate(hours_center = abs(hoursPerDayCombined - mean(hoursPerDayCombined))) %>% 
              dplyr::select(name, graph_names4, Subcategory, hoursPerDayCombined, uncertaintyCombined, population)

vertices <- merge(x = edges, 
                   y = vert_prep,
                   by.x = "to",
                   by.y = "graph_names4",
                   all.x = TRUE
            ) %>%
            unique() %>%
            replace(is.na(.), 0) %>% 
            dplyr::select(-from) %>% 
            rbind(
              world_df <- data.frame(
                name = "World",
                to = "World",
                Subcategory = "0",
                hoursPerDayCombined = 0,
                uncertaintyCombined = 0,
                population = 0
                )                
            ) %>% 
            mutate(
              name = ifelse(
                name == "0",
                case_when(
                  # to == "World.Africa" ~ "Africa",                  
                  to == "World.Africa.AF_E" ~ "East Africa",
                  to == "World.Africa.AF_M" ~ "Central Africa",
                  to == "World.Africa.AF_N" ~ "North Africa",
                  to == "World.Africa.AF_S" ~ "South Africa",
                  to == "World.Africa.AF_W" ~ "West Africa", 
                  # to == "World.America" ~ "America", 
                  to == "World.America.AM_C" ~ "Central America", 
                  to == "World.America.AM_N" ~ "North America", 
                  to == "World.America.AM_S" ~ "South America", 
                  # to == "World.Asia" ~ "Asia", 
                  to == "World.Asia.AS_C" ~ "Central Asia",                   
                  to == "World.Asia.AS_E" ~ "East Asia",
                  to == "World.Asia.AS_W" ~ "West Asia",                  
                  to == "World.Asia.AS_S" ~ "South Asia",                  
                  to == "World.Asia.AS_SE" ~ "Southeast Asia",                                    
                  to == "World.Asia.AS_S" ~ "South Asia",                                    
                  to == "World.Australia" ~ "Australia",                                    
                  # to == "World.Australia.ANZ" ~ "Australia + New Zealand",                  
                  # to == "World.Europe" ~ "Europe",
                  to == "World.Europe.EU_E" ~ "Eastern Europe",                  
                  to == "World.Europe.EU_N" ~ "Northern Europe",                                    
                  to == "World.Europe.EU_S" ~ "Southern Europe",                                    
                  to == "World.Europe.EU_W" ~ "Western Europe"                                 
                  ),
                NA
              )
            )

edges_final <- left_join(
                dplyr::select(vertices, to),
                edges,
                by = "to"
              ) %>% 
              unique() %>% 
              filter(to != "World") %>% 
              dplyr::select(from,to)

# visualization ----

# removing level 1
# edges_final2 <- edges_final %>% 
#                   filter(to %in% from) %>% 
#                   droplevels()
# vertices2 <- vertices %>% 
#               filter(to %in% c(edges_final2$from, edges_final2$to)) %>% 
#               droplevels()
# vertices2$hoursPerDayCombined <- runif(nrow(vertices2))

tt_cgraph <- graph_from_data_frame(edges_final, vertices = vertices)

# Control the size of each circle: (use the size column of the vertices data frame)
ggraph(tt_cgraph, layout = 'circlepack', weight = population) + 
      geom_node_circle(aes(fill=hoursPerDayCombined)) + 
      geom_node_label(aes(label=name), repel = TRUE) +
      scale_fill_distiller(palette = "RdPu") +
      theme_void()

+ 
      theme(legend.position="FALSE")






library(RColorBrewer)













