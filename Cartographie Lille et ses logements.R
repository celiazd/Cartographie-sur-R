
#Importation des données 

Communes <-read.csv2("/Users/celiazaidi/Desktop/R M2/carto/Communes.csv")

library(ggplot2)
library(questionr)
library(tidyverse)

#importer un fichier shapefile :
library(sf)#tout ce qui est données géographiques et spatiales 

garesMEL_sf <- st_read("/Users/celiazaidi/Desktop/R M2/carto/liste-des-gares.shp")

#importer un fichier geoJson:
library(geojsonio)

communesMEL_sf <- geojson_read("/Users/celiazaidi/Desktop/R M2/carto/mel_communes.geojson",
                              what="sp")%>% #le pipe attend toujours quelque chose après
  st_as_sf()
class(gareMEL_sf)
class(communesMEL_sf)

#Représenter les données géographiques : 
ggplot()+
  geom_sf(data=garesMEL_sf)+
  geom_sf(data=communesMEL_sf, fill="transparent")

#vérifier les systèmes de coordonnée : st_crs
st_crs(garesMEL_sf)
st_crs(communesMEL_sf)

#transformer les systèmes de coordonnées : st transform
garesMEL_sf <- st_transform(garesMEL_sf, crs = 2154)
communesMEL_sf <- st_transform(communesMEL_sf, crs = 2154)

#Données attributaires :
names(garesMEL_sf)
freq(garesMEL_sf$fret)
freq(garesMEL_sf$voyageurs)
names(communesMEL_sf)

#faire apparaitre les données attributaires sur la carte :
# ex : ajouter les infons "gare de voyageurs" et "surface" :


ggplot()+
  geom_sf(data=communeMEL_sf, aes(fill=surface))+
  viridis::scale_fill_viridis()+
  geom_sf(data=gareMEL_sf,aes(col=voyageurs))+
  scale_color_manual(values = c("black","grey"))+
  theme_bw()+
  labs(title = "Surface des communes et gares de la MEL",  col="Voyageurs",
       fill="Surface (m²)", caption="Source : MEL")+
  coord_sf( datum = NA)

# Ajouter d'autres caractéristiques :

# Par exemple, la population des communes :
pop18 <- read.csv2("/Users/celiazaidi/Desktop/R M2/carto/Communes.csv")
names(pop18)
# jointure :
communeMEL_sf <- left_join(communeMEL_sf,
                           pop18, by = c("insee"="DEPCOM"))
names(communeMEL_sf)

communeMEL_sf$PTOT <- NULL # supprime une éventuelle colonne qui s'appellerait
#PTOT

communeMEL_sf <- left_join(communeMEL_sf, pop18, by =c("insee" = "DEPCOM"))
# Il faut retirer les 2 polygones de Lomme et Hellemmes
# communes associées, sans PTOT) :
communeMEL_sf <- communeMEL_sf %>% filter(is.na(PTOT)==FALSE)
# Carte :
ggplot()+
  geom_sf(data=communeMEL_sf, aes(fill=PTOT))


# Modifier la carte pour représenter le nombre d'habitants :

ggplot()+
  geom_sf(data=communeMEL_sf, 
          aes(fill=PTOT))+
  viridis::scale_fill_viridis()

# On essaie de représetner des tranches de population : Par ex les quintiles

#voir les déciles :

quantile(communeMEL_sf$PTOT, #1er arguement : Va numériques
         seq(0,1,.1))# les limites souhaitées : ici les déciles

#ajouter l'info dans le tableau de données :
#avec cut : pour créer des tranches à partir d'une Va numérique

communeMEL_sf$PTOTCl <- cut(communeMEL_sf$PTOT, #colonne numérique à découper
                            #en tranche
                            quantile(communeMEL_sf$PTOT,
                                     seq(0,1,.1)),#tranches, copie des déciles
                            include.lowest = T)# inclure la valeur minimal
freq(communeMEL_sf$PTOTCl)

#carte :
ggplot()+
  geom_sf(data=communeMEL_sf, aes(fill=PTOTCl))+
  viridis::scale_fill_viridis(discrete = T)


#transformation des données spatiales 

dvf_brut <- read.csv("/Users/celiazaidi/Desktop/R M2/carto/59.csv", stringsAsFactors = F)
dim(dvf_brut)
dvf <- readRDS("/Users/celiazaidi/Desktop/R M2/carto/dvf19_cours.rds")
names(dvf)  

#transformer un fichier de données tabulaires en fichier géographique en 
#désignant les colonnes longitude et lattitude : avec st_as_sf()
  
dvf_sf <- dvf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)
#puis changement de système de coordonnées :
dvf_sf <- dvf_sf %>% st_transform(crs=2154)

#carte des ventes et des limites des communes de la MEL : 

ggplot() +
  geom_sf(data=dvf_sf) +
  geom_sf(data=communesMEL_sf)

#importation des iris de la mel : 

irisMEL_sf <- st_read("/Users/celiazaidi/Desktop/R M2/carto/MEL_iris.shp")

ggplot() +
  geom_sf(data=dvf_sf) +
  geom_sf(data=irisMEL_sf)

#intersection : ne garder que les points dans la MEL et associer les infos des
#IRIS corrspondant : avec st_intersection : 

dvf_MEL_sf <- st_intersection( dvf_sf, irisMEL_sf)

#!! il faut bien que les 2 fichiers aient le même système de projection pour 
#que l'intersection se fasse : 

#carte : 

ggplot() +
  geom_sf(data=dvf_MEL_sf)+
  geom_sf(data=irisMEL_sf, fill="transparent", size=.1)

# Compter le nb de transaction dans chaque IRIS :

TransactionsIRIS <- dvf_MEL_sf %>% 
  group_by(CODE_IR) %>% 
  summarise(NbVente = n()) # compte le nb de lignes

# Supprimer les infos spatiales de TransactionsIRIS :
TransactionsIRIS$geometry <- NULL
# Ajouter le nb de transactions dans irisMEL_sf
irisMEL_sf <- left_join(irisMEL_sf,
                        TransactionsIRIS,
                        by="CODE_IR")

# Carte des IRIS selon le nb de transactions 
ggplot()+geom_sf(data=irisMEL_sf, aes(fill=NbVente), 
                 size=.2)+
  viridis::scale_fill_viridis()

# Taille des logements vendus :
taille <- dvf_MEL_sf %>% 
  filter(surface_reelle_bati<800) %>% 
  group_by(CODE_IR) %>% 
  summarise(taille = mean(surface_reelle_bati)) %>% 
  data.frame() %>%
  select(CODE_IR, taille)
irisMEL_sf$taille <- NULL
irisMEL_sf <- left_join(irisMEL_sf, taille, by="CODE_IR")
ggplot()+
  geom_sf(data=irisMEL_sf, size=.1)+
  aes(fill=taille)+
  viridis::scale_fill_viridis()+
  theme_bw()+
  labs(title = "Taille des logements vendus dans la métropole lilloise",
       fill="Surface réelle du bâti (m²)",
       caption="Source : DVF, Ministère des Finances, 2019")

# Focus sur Lille :
tailleLille <- dvf_MEL_sf %>% 
  filter(NOM_COM=="Lille") %>% 
  group_by(CODE_IR) %>% 
  summarise(tailleLille = mean(surface_reelle_bati)) %>% 
  data.frame() %>%
  select(CODE_IR, tailleLille)
irisMEL_sf <- left_join(irisMEL_sf, tailleLille, by="CODE_IR")
irisLille_sf <- irisMEL_sf %>% filter(NOM_COM=="Lille")
ggplot()+
  geom_sf(data=irisLille_sf, size=.1)+
  aes(fill=tailleLille)+
  viridis::scale_fill_viridis()+
  theme_bw()+
  labs(title = "Taille des logements vendus à Lille",
       fill="Surface réelle du bâti (m²)",
       caption="Source : DVF, Ministère des Finances, 2019")
# Points des ventes avec le prix au m² :

dvf_Lille_sf <-  st_intersection( dvf_MEL_sf, irisLille_sf)
dvf_Lille_sf$prix <- round(dvf_Lille_sf$valeur_fonciere/dvf_Lille_sf$surface_reelle_bati)
ggplot()+
  geom_sf(data=irisLille_sf, size=.1, fill="transparent")+ 
  geom_sf(data=dvf_Lille_sf, aes(col=prix), size=2)+
  viridis::scale_color_viridis()+
  coord_sf(crs = st_crs(irisLille_sf), datum = NA)+
  theme_bw()

# Ajout d'images raster de fond :
library(ggspatial)
ggplot()+
  annotation_map_tile(zoom=13,type="cartolight")+
  geom_sf(data=irisLille_sf, size=.3, fill="transparent", col="grey60")+
  geom_sf(data=dvf_Lille_sf, aes(col=prix), size=1)+
  viridis::scale_color_viridis()+
  theme_bw()+
  labs(title = "Prix au m² des ventes de 2019 à Lille",
       col="Prix (/m²)",
       caption="Sources : DVF, Ministère des Finances, 2019, fond : OSM")+
  coord_sf(crs = st_crs(irisLille_sf), datum = NA)

# rectangle spatial dans lequel se trouve les données irisLille_sf
st_bbox(irisLille_sf)

