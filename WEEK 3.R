setwd("E:/UEL SCHOOL MATERIALS/Spatial Data Analysis/Resources")
#install.packages()
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(ggspatial)


municipalities <- read_sf("E:/UEL SCHOOL MATERIALS/Spatial Data Analysis/Resources/municipalities.shp")


municipalities_sp <- municipalities %>% filter(UF == "SP")


municipalities_centroids <- municipalities_sp %>% st_centroid()


ggplot(data = municipalities_centroids) +
  geom_sf()

sp_border <- municipalities_sp %>% st_union()
sp_border %>%ggplot() + geom_sf()

sp_border %>% ggplot() + geom_sf() +
  geom_sf(data = municipalities_centroids) +
  theme_classic()

#Question2

view(municipalities)
br_states <- municipalities %>% group_by(UF) %>% summarise(IDHM_mean=mean(IDHM_10))
br_states %>% ggplot() +
  geom_sf(aes(fill=IDHM_mean))+
  theme_classic()+
  scale_fill_continuous(low="red", high="green")

#Question3
indigenous <- read_sf("E:/UEL SCHOOL MATERIALS/Spatial Data Analysis/Resources/BC250_Terra_Indigena_A.shp") %>% 
  st_transform(4326) 
indigenous_valid <- st_make_valid(indigenous)
st_write(indigenous_valid, "E:/UEL SCHOOL MATERIALS/Spatial Data Analysis/Assignments/BC250_Terra_Indigena_A.shp")
br_states %>% ggplot() +
  geom_sf()+
  geom_sf(data = indigenous_valid, fill="red")

Xingu <- indigenous_valid %>% filter(nome == "Parque do Xingu")%>%
  st_transform(4326)
Gaucha <- municipalities %>% filter(NOME == "GAUCHA DO NORTE") %>%
  st_transform(4326)
Gaucha %>% ggplot()+
  geom_sf(fill="red")+
  geom_sf(data = Xingu, fill="blue", alpha=0.5)

intersection <- Gaucha %>% st_intersection(Xingu)
intersection %>% ggplot()+
  geom_sf(fill ="dark green")
Gaucha %>% ggplot ()+
  geom_sf(fill="red", alpha=0.5) +
  geom_sf(data = Xingu,fill="blue", alpha=0.5)+
  geom_sf(data = intersection, fill = "black")

#Question4
Housing <-read_sf("E:/UEL SCHOOL MATERIALS/Spatial Data Analysis/Resources/MCMV_new.shp")
Housing_AC <- Housing %>% filter(UF == "AC")
br_states %>% filter(UF == "AC")%>%
  ggplot()+
  geom_sf()+
  geom_sf(data = Housing_AC)
distance <- Housing_AC%>%
  st_transform(29189)%>%
  st_distance()%>%
  as.data.frame()
Housing_AC %>% st_transform(29189)%>%
  st_buffer(20000)%>%
  ggplot()+
  geom_sf(fill="dark green")
br_states%>% filter(UF =="AC")%>%
  ggplot() +
  geom_sf() +
  geom_sf(data = Housing_AC %>% st_transform(29189)%>%st_buffer(20000),fill = "dark green")+
  geom_sf(data = Housing_AC, color = "red")


#Question5
mun_Housing_units <-municipalities%>% st_join(Housing)
mun_Housing_units <- mun_Housing_units%>%
  group_by(COD_MUN, NOME)%>%
  summarise(UH = sum(UH,na.rm=T))%>%
  ungroup()
mun_Housing_units %>% arrange(-UH)%>%
  slice(1)%>%
  pull(NOME)
mun_Housing_units %>% ggplot() +
  geom_sf(aes(fill=UH), col=NA) +
  scale_fill_gradient(low="#ccece6", high="dark green", trans="log") +
  theme_classic()
  
  