#map POE
library(leaflet)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(dplyr)

sbm_census_level <- read.csv("C:/Users/Ethan/Desktop/SBM_abstract/sbm_census_level.csv")

sbm_census_level <- sbm_census_level %>%
  mutate(
    drugs_wht_rate = (drugs_wht_count / white) * 1000,
    drugs_blk_rate = (drugs_blk_count / black) * 1000,
    arrest_wht_rate = (arrest_wht_count / white) * 1000,
    arrest_blk_rate = (arrest_blk_count / black) * 1000,
    surplus_wht_rate = (surplus_wht_count / white) * 1000,
    surplus_blk_rate = (surplus_blk_count / black) * 1000,
    citation_wht_rate = (citation_wht_count / white) * 1000,
    citation_blk_rate = (citation_blk_count / black) * 1000,
    patdown_wht_rate = (patdown_wht_count / white) * 1000,
    patdown_blk_rate = (patdown_blk_count / black) * 1000,
    ViolentCrime_rate = (ViolentCrime_conf_count / pop) * 1000,
    drugs_rate = (drugs_count / pop) *1000,
    surplus_rate = (surplus_count / pop) *1000
  )

subst <- sbm_census_level %>% dplyr::select(GEOID,
                                            gen_cat18, surplus_rate, drugs_rate, ViolentCrime_rate, pop, white, black, ice_race_inc,	ice_race, SDI_score, sdi, drugs_count,	arrest_count,
                                            surplus_count,	citation_count,	patdown_count,	drugs_wht_count,	drugs_blk_count,
                                            arrest_wht_count,	arrest_blk_count, surplus_wht_count,	surplus_blk_count, citation_wht_count,	
                                            citation_blk_count,	patdown_wht_count,	patdown_blk_count, Homicide_count, Robbery_count,
                                            Weapon_count,	Assault_count,	ViolentCrime_count,	ShotsFired_count,	drugs_911_count,
                                            drugs_wht_rate, drugs_blk_rate, arrest_wht_rate, arrest_blk_rate, surplus_wht_rate, surplus_blk_rate, citation_wht_rate, citation_blk_rate, patdown_wht_rate, patdown_blk_rate
)

subst <- subst %>% filter(pop!=0)




surplus_breaks <- quantile(subst$surplus_rate, probs = c(1/3, 2/3), na.rm = TRUE)
vc_breaks <- quantile(subst$ViolentCrime_rate, probs = c(1/3, 2/3), na.rm = TRUE)
ice_breaks <- quantile(subst$ice_race_inc, probs = c(1/3, 2/3), na.rm = TRUE)


subst <- subst %>%
  mutate(
    saf_class = case_when(
      surplus_rate <= surplus_breaks[1] ~ "Low",
      surplus_rate <= surplus_breaks[2] ~ "Medium",
      TRUE ~ "High"
    ),
    gen_class = case_when(
      gen_cat18 == 0 ~ "Non-Gentrifiable",
      gen_cat18 == 1 ~ "Gentrifiable",
      gen_cat18 == 2 ~ "Gentrifying"
    ),
    bivar_class = paste0(saf_class, "-", gen_class)
  )





#map_sf <- subst %>% filter(!(surplus_count == 0 & ViolentCrime_count == 0))
map_sf <- subst %>% filter(subst$pop != 0)


# Fetch Census Tract Geometry for New Orleans
bg_geo18 <- get_acs(
  geography = 'tract',
  state = 'LA',
  county = 'Orleans',
  geometry = TRUE,
  year = 2018,
  variables = "B01003_001" # Total population 
)

subst$GEOID <- as.character(subst$GEOID)
map_sf <- left_join(bg_geo18, subst, by = "GEOID")

map_sf$color <- bivar_palette[map_sf$bivar_class]

#####


map_sf <- map_sf %>%
  mutate(
    saf_class = case_when(
      surplus_rate <= surplus_breaks[1] ~ "Low",
      surplus_rate <= surplus_breaks[2] ~ "Medium",
      TRUE ~ "High"
    ),
    gen_class = case_when(
      gen_cat18 == 0 ~ "Non-Gentrifiable",
      gen_cat18 == 1 ~ "Gentrifiable",
      gen_cat18 == 2 ~ "Gentrifying"
    ),
    bivar_class = paste0(saf_class, "-", gen_class)
  )


bivar_palette <- c(
  "Low-Non-Gentrifiable"     = "#d6dbe9",  # light blue
  "Low-Gentrifiable"         = "#bfe3dc",  # light teal
  "Low-Gentrifying"          = "#fddbc7",  # light orange
  
  "Medium-Non-Gentrifiable"  = "#8da0cb",  # medium blue
  "Medium-Gentrifiable"      = "#66c2a5",  # medium teal
  "Medium-Gentrifying"       = "#fc8d62",  # medium orange
  
  "High-Non-Gentrifiable"    = "#4b5d8a",  # dark blue
  "High-Gentrifiable"        = "#2e7c6b",  # dark teal
  "High-Gentrifying"         = "#b3472d"   # dark orange
)


map_sf$color <- bivar_palette[map_sf$bivar_class]


legend_html <- "
<div style='background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 5px;'>
  <strong>Surplus-Stop Rate × Gentrification</strong><br>
  <table style='border-collapse: collapse; width: 100%; text-align: center; font-size: 12px;'>
    <tr>
      <td></td>
      <td><strong>Non-Gentrifiable</strong></td>
      <td><strong>Gentrifiable</strong></td>
      <td><strong>Gentrifying</strong></td>
    </tr>
    <tr>
      <td><strong>High Surplus</strong></td>
      <td style='background-color: #4b5d8a; width: 30px; height: 30px;'></td>
      <td style='background-color: #2e7c6b; width: 30px; height: 30px;'></td>
      <td style='background-color: #b3472d; width: 30px; height: 30px;'></td>
    </tr>
    <tr>
      <td><strong>Medium Surplus</strong></td>
      <td style='background-color: #8da0cb; width: 30px; height: 30px;'></td>
      <td style='background-color: #66c2a5; width: 30px; height: 30px;'></td>
      <td style='background-color: #fc8d62; width: 30px; height: 30px;'></td>
    </tr>
    <tr>
      <td><strong>Low Surplus</strong></td>
      <td style='background-color: #d6dbe9; width: 30px; height: 30px;'></td>
      <td style='background-color: #bfe3dc; width: 30px; height: 30px;'></td>
      <td style='background-color: #fddbc7; width: 30px; height: 30px;'></td>
    </tr>
  </table>
</div>
"


leaflet(map_sf) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    fillColor = ~color,
    color = "#444444",
    weight = 1,
    opacity = 0.8,
    fillOpacity = 0.8,
    label = ~paste0(
      "<strong>Gentrification Status: </strong>", gen_class, "<br>",
      "<strong>Surplus-Stop Rate: </strong>", surplus_rate, "<br>",
      "<strong>Bivariate Class: </strong>", bivar_class
    ) %>% lapply(htmltools::HTML)
  ) %>%
  addControl(html = legend_html, position = "bottomright")



############## Calc  Moran's I
neighbors <- poly2nb(map_sf2, queen = TRUE)  # Identifies neighbors
weights_list <- nb2listw(neighbors, style = "W", zero.policy = TRUE)  # Assigns row-standardized weights
map_sf2 <- map_sf %>%
  filter(!is.na(saf_rate) & !is.na(fav_rate))
map_sf2$saf_rate <- (map_sf2$saf_sum/map_sf2$youth_10_24)*1000
map_sf2$fav_rate <- (map_sf2$fav_sum/map_sf2$youth_10_24)*1000
summary(map_sf2$saf_rate)
summary(map_sf2$fav_rate)
moran_saf <- moran.test(map_sf2$saf_rate, weights_list, zero.policy = TRUE)
fav_saf <- moran.test(map_sf2$fav_rate, weights_list, zero.policy = TRUE)

print(moran_saf)
print(fav_saf)