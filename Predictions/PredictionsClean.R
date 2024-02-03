
setwd("~/Documents/Github/aireport/")
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(fasterize)
library(MetBrewer)

world <- ne_countries(scale = "medium", returnclass = "sf")

iucn <- read_sf("~/Dropbox/CurrentIUCN/MAMMALS.shp")
bats <- iucn %>% filter(`order_` == 'CHIROPTERA')
bats <- bats[,'binomial']

b <- read_csv("Predictions/Betacoronavirus.csv")
f <- read_csv("Predictions/Filovirus.csv")
n <- read_csv("Predictions/Nipah.csv")

b <- b %>% 
  rename(binomial = Sp) %>%
  mutate(Trait.1 = recode(Trait.1, !!!c("True +" = "Known", "False -" = "Known", "Unlikely" = " ", "Suspected" = "Predicted"))) %>%
  rename(Betacoronavirus = Trait.1) %>%
  dplyr::select(binomial, Betacoronavirus) 

f %>% filter(status == 1) %>% pull(probability) %>% quantile(0.10) -> f.t
f <- f %>% 
  rename(binomial = species) %>%
  mutate(Filovirus = (probability > f.t) + 2*status) %>%
  mutate(Filovirus = recode(Filovirus, !!!c("3" = "Known", "2" = "Known", "1" = "Predicted", "0" = " "))) %>%
  dplyr::select(binomial, Filovirus)

n %>% filter(`NiV+` == 1) %>% pull(`p(NiV+)`) %>% quantile(0.10) -> n.t
n <- n %>%
  rename(binomial = `...1`) %>%
  mutate(Nipah = (`p(NiV+)` > n.t) + 2*`NiV+`) %>%
  mutate(Nipah = recode(Nipah, !!!c("3" = "Known", "2" = "Known", "1" = "Predicted", "0" = " "))) %>%
  dplyr::select(binomial, Nipah)
  
bats <- bats %>%
  left_join(b) %>%
  left_join(n %>% mutate(binomial = str_replace(binomial, "_"," "))) %>%
  left_join(f %>% mutate(binomial = str_replace(binomial, "_"," "))) %>%
  mutate(Betacoronavirus = replace_na(Betacoronavirus, " ")) %>%
  mutate(Filovirus = replace_na(Filovirus, " ")) %>%
  mutate(Nipah = replace_na(Nipah, " "))

bats

# Country level 

drc <- world %>% filter(name == "Dem. Rep. Congo")
i <- st_intersects(bats, drc)
i <- sapply(i, length)
drcbats <- bats[i==1,]
raster <- raster(bats, res = 1/6)
map <- fasterize(drcbats, raster, fun="sum")
map <- mask(map, drc)
mapdf <- as.data.frame(map, xy = TRUE)
mapdf <- mapdf %>% filter(!is.na(layer)) %>% rename(Bats= layer)
ggplot() +
  geom_sf(data = world, fill = 'grey90', color = NA) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = mapdf, aes(x = x, y = y, fill = Bats)) +
  scale_fill_gradientn(colors = met.brewer("Hokusai3", 8)) +
  theme_bw() +
  coord_sf() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = drc, fill = NA, color = "black", lwd = 0.3) +
  theme(legend.position = c(0.1, 0.25)) +
  xlim(7, 33) + ylim(-15, 8) + 
  ggtitle("Democratic Republic of the Congo") + 
  xlab("") + ylab("") 

ggsave(width = 5, height = 5, dpi = 600, "DRC.pdf")

# # Junk
# 
# bats %>% 
#   filter(Betacoronavirus %in% c("Known", "Predicted")) -> covs
# 
# 
# 
# raster <- raster(bats, res = 1/6)
# map3 <- fasterize(covs, raster, fun="sum")
# map3 <- mask(map3, drc)
# map3df <- as.data.frame(map3, xy = TRUE) 
# map3df <- map3df %>% filter(!is.na(layer)) %>% rename(Species = layer)
# 
# ggplot() + 
#   geom_sf(data = world, fill = 'grey90', color = NA) + 
#   ggnewscale::new_scale_fill() +
#   geom_raster(data = map3df, aes(x = x, y = y, fill = Species)) + 
#   scale_fill_gradientn(colors = met.brewer("Hokusai3", 8)) +
#   theme_bw() + 
#   coord_sf() + 
#   ggnewscale::new_scale_fill() +
#   geom_sf(data = drc, fill = NA, color = "black", lwd = 0.3) + 
#   theme(legend.position = c(0.1, 0.2)) + 
#   xlim(-10, 45) + ylim(-15, 15)

drcbats %>% as.data.frame() %>% dplyr::select(-c("geometry")) %>% distinct() -> drclist

drclist %>% filter(Betacoronavirus == " ") %>% pull(binomial) -> list1
drclist %>% filter(Filovirus == " ") %>% pull(binomial) -> list2
drclist %>% filter(Nipah == " ") %>% pull(binomial) -> list3
drclist %>% filter(binomial %in% c(list1)) %>%
  filter(binomial %in% c(list2)) %>%
  filter(binomial %in% c(list3))
