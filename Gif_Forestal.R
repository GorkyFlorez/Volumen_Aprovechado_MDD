
#------------------------------------------------------------------------
library(flexdashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tm)
library(wordcloud)
library(sf)
library(formattable)
library(RColorBrewer)
library(DT)
library(crosstalk)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(htmlwidgets)
library(leafem)
library(plotly)
library(ggplot2)
library(writexl)
library(readxl)
library(DBI)
library(RPostgres)
library(sf)
library(leaflegend)
library(magick)
#------------------------------------------------------------------------
Con_Castaneras   <- st_read("SHP/Concesion_castañeras.geojson")
Con_Castanera<- st_transform(Con_Castaneras,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

library(raster)
library(viridis)
library(ggspatial)
library(gganimate)
Peru_DEPA       <- getData('GADM', country='Peru', level=3) %>% st_as_sf() 
MDD    <-subset(Peru_DEPA , NAME_1 == "Madre de Dios") 

# Cargamos los datos de covid-19
St_spp_madera <- read_excel("Excel/SPP_Madera.xlsx")

cortes <- c(10,40,60,80,115 )
new_size <- c( 5, 7, 9, 11, 13)
#------------------------------------------------------------------------


A <- ggplot() +
  geom_sf(data= MDD, fill="beige")+
  geom_sf(data= Con_Castanera , fill="khaki1")+
  geom_point(data=St_spp_madera, aes(x=X, y=Y, size=VOLCOM , color=VOLCOM )) +
  scale_size_continuous(range=c(1,7), name = "Vol (m3)",breaks = cortes,
                        labels = c("0 - 9", "10 - 39", "40 - 59", "60 - 79",
                                   "80 - 115"), guide="none") +
  scale_color_viridis(trans="log",name = "Vol (m3)",direction = -1,breaks = cortes,
                      labels = c("0 - 9", "10 - 39", "40 - 59", "60 - 79",
                                 "80 - 115"))+
  guides(colour=guide_legend(override.aes=list(size=new_size))) +
  theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.4, style ="bar") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  theme( panel.background = element_rect(fill = "aliceblue"),
         axis.text.y  = element_text(angle = 90,face="bold", color="black", size=9,family="serif"),
         axis.text.x  = element_text(face="bold", color="black", size=9,family="serif"))+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")")))+
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  coord_sf(xlim = c(-70.5,-68.7), ylim = c(-13.5,-11.5),expand = FALSE)


B = A +
  transition_time(FECHA) +
  labs(title = "MAPA DE FRECUENCIA DE VOL 2016- 2021", 
       subtitle="Años: {frame_time}",caption="GRFFS",x="Longitud",y="Latitud",tag="")+
  shadow_wake(wake_length = 0.1)
# Crear el git
gganimate::animate(B, height = 800, width = 800, fps = 30, duration = 20,end_pause = 60, res = 100)
# Para guardar git
anim_save("Desembarque2.gif")

