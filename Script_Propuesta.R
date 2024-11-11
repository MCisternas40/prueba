library(data.table)
library(tidyverse)
library(leaflet) # Mapas
library(sp)
library(broom)
library(janitor) #Paquete para limpiar datos
library(rgbif) #Datos para ejemplo de mapas
library(sf) #Geografia
library(chilemapas) #Geografía de Chile
library(RColorBrewer)
library(classInt)
library(plotly)
rm(list=ls())
####construcción con base de datos agentes culturales
base <- data.frame(longitud = -71.530144, latitud=-33.020034)
comunas <-data.table(mapa_comunas)
censo <- data.table(censo_2017_comunas)
library(readxl)
library(readxl)
BBDD<- read_excel("C:/Users/mcist/Downloads/bd.xlsx")
Cod_Comunas <- read_excel("C:/Users/mcist/Downloads/Cod_Comunas.xlsx")
names(Cod_Comunas)
tabla<-BBDD %>% group_by(pn_1_28Aux,pn_1_29Aux) %>% summarise(n_agent=n()) %>% ungroup()
tabla_f<-tabla %>% filter(pn_1_28Aux=="Valparaíso")
names(tabla_f)[2]<-"Nombre Comuna"
Cod_Comunas<-Cod_Comunas %>% left_join(., tabla_f, by="Nombre Comuna") %>% filter(pn_1_28Aux=="Valparaíso")
Cod_Comunasf<-Cod_Comunas %>% dplyr::select("Código Comuna desde 2018", "n_agent")
names(Cod_Comunasf)[1]<-"codigo_comuna"
censo<-censo %>% left_join(.,Cod_Comunasf,by="codigo_comuna") %>% filter(!duplicated("codigo_comuna"))
region<- merge(censo, comunas, by='codigo_comuna',all.x=T)
table(region$codigo_region)
region<-region %>% filter(codigo_region=="05") %>% filter(!duplicated(codigo_comuna)) %>% mutate(n_agent=ifelse(is.na(n_agent),0,n_agent))
ggplot() +
  geom_sf(region,mapping=aes(geometry=geometry, fill=n_agent))+
  coord_sf(xlim = c(-71.8,-70), ylim = c(-34,-32))

breaks_pretty <-  classIntervals(region$n_agent, style = "maximum",n=6)


region$pretty <-  cut(region$n_agent,breaks = breaks_pretty$brks, include.lowest = T)
codigos_territoriales <- data.table(codigos_territoriales) 
region <- merge(region, codigos_territoriales, by='codigo_comuna')
mapa <- ggplot() +
  geom_sf(region,mapping=aes(geometry=geometry, fill=pretty)) +
  scale_fill_viridis_d(name="Intervalos") +
  coord_sf(xlim = c(-71.8,-70), ylim = c(-34,-32)) +
  theme_void() + labs(title="Cantidad de Agentes Culturales" , subtitle = "En la región de Valparaíso", caption = "Fuente: BBDD Agentes Culturales 2021")  
mapa
# se recomienda hacer la instalación de esta manera
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
#####nube de letras
BBDD1<-BBDD %>% filter(pn_1_28Aux=="Valparaíso")
freq<- BBDD1 %>% group_by(pn_2_1_AuxPrio1) %>% summarise(actividades=n())
wordcloud2(data=freq, size = 0.7, color = 'random-light',backgroundColor = 'Black')
