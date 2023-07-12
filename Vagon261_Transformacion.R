rm(list=ls())

library(readxl)
library(dplyr)
library(sqldf)
library(mice)
library(ggplot2)

setwd("C:/Users/RVQ1266/Desktop/4to ciclo/Tesis 3/Dataset/Ventas Vagon 261")



########### 1) DATA A UTILIZAR ################# 
data_vagon261 <- read_excel("Ventas_Vagon.xlsx")

data_vagon261 <- data_vagon261 %>% dplyr::select(Fecha_hora, Nombre, Precio_venta_unitario, Cantidad, Precio_de_venta_total,Descuento_de_venta_total,Descuento_global_de_venta_total, Precio_total_final, Utilidad)

##LIMPIEZA##
#encontrar registros que no son propiamente del bar y hacer una lista
sqldf::sqldf("SELECT distinct nombre from data_vagon261")

no_es_de_barra <- c(
'Pechuga ligth',
'Chicha morada 500ml',
'Lomo saltado de res',
'Piqueo tado',
'Arroz a la cubana', 
'Maracuyá 500ml',
'Sopa dieta',
'Ají de gallina',
'Chicharron de pollo mediano',
'Chicharron de pollo personal',
'+3 Chicha',
'12 Tequeños de Queso',
'Bisteck a lo pobre',
'Saltado de pollo',
'Tallarin verde con milanesa',
'Talla huanca con lomo saltado',
'Menú 12 soles',
'Tallarin a lo alfredo',
'Ensalada delicia',
'Arroz con pollo',
'06 Tequeños de Lomo',
'Saltado lomo fino',
'Tallarin saltado res',
'12 Tequeños de Lomo', 
'06 Croquetas de yuca',
'Papas fritas',
'Chicharron de pollo grande',
'Ensalada caesar',
'Arroz',
'Arroz chaufa pollo', 
'Suprema de pollo',
'06 Tequeños de Queso',
'Tallarin saltado pollo',
'Consumo cliente 6',
'+3 maracuyá',
'Consumo cliente 100',
'Pollo a la canasta',
'Huevo',
'Artículo 1',
'Adicional lomo',
'Delivery',
'Agrego mas para chicharron mediano',
'Consumo cliente 12',
'Plátano',
'Taper descartable',
'2x1 - 06 Tequeño queso',
'2x1 Lomo Res',
'Salchicha',
'Papa a la huancaina',
'Cierre mesa',
'A lo pobre',
'PROMO CUZQUEÑA  - HAMBURGUESA MI PRIMERA ESTACION',
'PROMO PILSEN  - HAMBURGUESA MI PRIMERA ESTACION')

#Eliminar lo que no es de la barra
data_vagon261 <- subset(data_vagon261, !(data_vagon261$Nombre %in% no_es_de_barra))

#Verificar
sqldf::sqldf("SELECT distinct nombre from data_vagon261")





########### 2) TRANSFORMACION ################# 


data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="BAD HAPPY NOOKIE 30ML","BAD HAPPY NOOKIE 330ML",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="Pilsen 5x50","Pilsen 5X50",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="CHILCANO CANELA  - LA BLANCO ","CHILCANO CANELA - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="CHILCANO CANELA  - LA BLANCO","CHILCANO CANELA - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="CHILCANO KION Y ROMERO  - LA BLANCO","CHILCANO KION Y ROMERO - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="PISCO SOUR FRUTOS ROJOS  -  LA BLANCO","PISCO SOUR FRUTOS ROJOS - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="RON FLOR DE CAÑA 5 AÑOS VASO ","RON FLOR DE CAÑA 5 AÑOS VASO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="SWEET  WINE ","SWEET WINE",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="SWEET  WINE","SWEET WINE",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="SWEET WINE ","SWEET WINE",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="PISCO SOUR LA BLANCO - FRUTOS ROJOS","PISCO SOUR FRUTOS ROJOS - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="PISCO SOUR LA BLANCO - CLASICO","PISCO SOUR CLASICO - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="PISCO SOUR LA BLANCO - MARACUYA","PISCO SOUR MARACUYA - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="PISCO SOUR FRUTOS ROJOS  -  LA BLANCO","PISCO SOUR FRUTOS ROJOS - LA BLANCO",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="LICOR JAGERMEISTER BOTELLA 700ML","JAGERMEISTER BOTELLA 700ML",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="HIGHBALL 2x1","HIGHBALL 2X1",data_vagon261$Nombre)
data_vagon261$Nombre <-ifelse(data_vagon261$Nombre=="GIN TONIC SEVILLA -  TROPICAL","GIN TONIC SEVILLA - TROPICAL",data_vagon261$Nombre)

data_vagon261$Categoria <- data_vagon261$Nombre

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AGUA CIELO S/G 625ML","AGUAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AGUA SAN MATEO C/G 600ML","AGUAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AGUA SAN MATEO S/G 600ML","AGUAS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA AGUAYMANTO 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA APU CHICON 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA EVA DEL VALLE 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA GRINGA IPA 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA PITUSIRAY 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA PUMAHUANCA 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA SANGRE DEL INKA 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALMA NATURAL PILSEN 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD ELECTRIC PUNANI 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD DARK LAGER 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD FANCY MARY ELLEN 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD GIRRLY BITS 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD GYPSY FLOWER 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD HAPPY NOOKIE 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="BAD MYSTIC PUNTANG 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CORONA EXTRA 355ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUZQUEÑA DORADA 310ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUZQUEÑA TRIGO 310ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="HEINEKEN 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PILSEN CALLAO 305ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="STELLA ARTOIS 330ML","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="Pilsen 5X50","CERVEZAS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="Cuzqueña Trigo 5x50","CERVEZAS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CASERO","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MERCADO PANDORA","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ALGARROBINA","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TE PITEADO","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="WICHOCO","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PANDORA PUNCH","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PINK PANDORA","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AMARETTO SOUR","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="APPLE FRESH","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="261","COCTELES BAR VAGON 261",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TUMBADITO","COCTELES BAR VAGON 261",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AGUA TONICA LATA BRITVIC 150ML","COMPLEMENTOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RED BULL ENERGY DRINK LATA 250ML","COMPLEMENTOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RED BULL TROPICAL LATA 250ML","COMPLEMENTOS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="COCA COLA ORIGINAL 600ML","GASEOSA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="COCA COLA ZERO 600ML","GASEOSA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GINGER ALE EVERVESS 500ML","GASEOSA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="INCA KOLA ORIGINAL 600ML","GASEOSA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="INCA KOLA ZERO 600ML","GASEOSA",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="NEGRONI","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TROPICAL","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TANQUERAY SEVILLA","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TANQUERAY LONDON DRY VASO","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC SEVILLA - TROPICAL","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC SEVILLA - FRUTOS ROJOS","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC SEVILLA - CLASICO","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC SEVILLA - PINK","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC CITRICO - OTRO","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC CITRICO - TANQUERAY","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - FRUTOS ROJOS","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC FRUTOS - OTRO","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC FRUTOS - TANQUERAY","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC LONDON DRY - CLASICO","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC LONDON DRY - FRUTOS ROJOS","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC LONDON DRY - BLACK","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - BLACK","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - PINK","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - TROPICAL","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - CAMPARI","GIN TONIC",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="GIN TONIC - CLASICO","GIN TONIC",data_vagon261$Categoria)


data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="ADIC. ONZAS","OTROS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CORCHO LIBRE","OTROS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PALOMA","OTROS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CAMPARI RESERVE","OTROS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="AMARETTO SHOT","OTROS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CLASICO - 4 GALLOS","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CLASICO - OTRO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO FRESA - 4 GALLOS","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO FRESA - OTRO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO MARACUYA - 4 GALLOS","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO MARACUYA - OTRO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CLASICO - LA BLANCO 2X1","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO PIÑA GOLDEN - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO KION Y ROMERO - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO AGUAYMANTO Y MARACUYA - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CANELA - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CLASICO - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO FRUTOS ROJOS - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CLASICO - OTRO 2X1","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO AGUAY. Y MARACUYA  - 2x35","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO AGUAY. Y MARACUYA  - 3x50","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO CANELA - 2x35","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CHILCANO 2X1 CLASICO - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO 4 GALLOS QUEB. BOTELLA 700ML","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SHOT","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR CLASICO - 4 GALLOS","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR CLASICO - OTRO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR MARACUYA - 4 GALLOS","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR MARACUYA - OTRO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TE PITEADO 32% DSCTO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TE PITEADO 2X25","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO 4 GALLOS SHOT","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR FRUTOS ROJOS - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR CLASICO - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR MARACUYA - LA BLANCO","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR CLASICO - 2x40","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR CLASICO - 2x40","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO SOUR MARACUYA - 2x40","PISCO",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PISCO LA BLANCO SHOT","PISCO",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="FROZEN FRESA","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="FROZEN LIMON","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="FROZEN MARACUYA","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="REFRESCO FRESA","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="REFRESCO LIMON","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="REFRESCO MARACUYA","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MERCADO PANDORA VIRGEN","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MOJITO VIRGEN","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PIÑA COLADA VIRGEN","REFRESCOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CASERO VIRGEN","REFRESCOS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="DAIKIRI","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUBA LIBRE - OTRO","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUBA LIBRE - ZACAPA","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUBA LIBRE - HAVANA CLUB","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUBA LIBRE - HAVANA CLUB 2X1","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CUBA LIBRE - OTRO 2X1","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MOJITO - OTRO","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MOJITO - HAVANA CLUB","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MOJITO - ZACAPA","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PIÑA COLADA - OTRO","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PIÑA COLADA - ZACAPA","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="PIÑA COLADA - HAVANA CLUB","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON BARCELO AÑEJO BOTELLA 750ML","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON ZACAPA 12A. VASO","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON ZACAPA A. 12A. BOTELLA 750ML","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON CAPTAIN MORGAN 700ML","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON HAVANA CLUB AÑEJO ESPECIAL 750 ML","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON FLOR DE CAÑA 5 AÑOS VASO","RON",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="RON FLOR DE CAÑA 5 AÑOS BOTELLA 750ML","RON",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TEQUILA RUBIO JOSÃ‰ C. BOT. 750ml","TEQUILA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TEQUILA JOSE CUERVO SHOT","TEQUILA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TEQUILA JIMADOR SHOT","TEQUILA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SWING SHOT - TEQUILA","TEQUILA",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="LICOR JAGERMEISTER SHOT","JAGERMEISTER",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SWING SHOT - JAGERMEISTER","JAGERMEISTER",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="JAGERMEISTER BOTELLA 700ML","JAGERMEISTER",data_vagon261$Categoria)


data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="APEROL SPRITZ","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="CINZANO PRO. ESP. BOTELLA 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="TINTO DE VERANO COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO BLANCO DE BLANCOS TACAMA BOTELLA 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO GRAN BLANCO TACAMA COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO GRAN TINTO TACAMA COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO F. L. MORAS BORANDA BOTELLA 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO FLM BORANDA COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO FLM MALBEC BOT. 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO FLM MALBEC COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO SELEC. ESP. TACAMA BOTELLA 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO BLANCO VITTORIA  BOTELLA 750ML","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO VITTORIA COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO BLANCO VITTORIA COPA","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SWEET WINE","VINOS",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VINO TINTO SELEC. ESP. TACAMA COPA","VINOS",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="MOSCOW MULE","VODKA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SCREWDRIVER","VODKA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="VODKA SMIRNOFF G. APPLE BOTELLA 355ML","VODKA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SPARKLING MARACUYA","VODKA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SPARKLING FRESA","VODKA",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="SPARKLING LIMON","VODKA",data_vagon261$Categoria)

data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="HIGHBALL","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="HIGHBALL 2X1","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="Highball 2x1","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="WHISKY JW BLACK LABEL BOTELLA 750ML","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="WHISKY JW BLACK LABEL VASO","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="WHISKY JW BLUE BOTELLA 750ML","WHISKY",data_vagon261$Categoria)
data_vagon261$Categoria <-ifelse(data_vagon261$Categoria=="WHISKY JW DOUBLE BLACK VASO","WHISKY",data_vagon261$Categoria)

#VERIFICACION
sqldf::sqldf("SELECT distinct categoria from data_vagon261 ORDER BY CATEGORIA")



##IDENTIFICACION DE PROMOCIONES
data_vagon261 <- sqldf::sqldf("SELECT*,
             CASE
             WHEN Nombre = 'CUBA LIBRE - OTRO 2X1' THEN 1
             WHEN Nombre = 'HIGHBALL 2X1' THEN 1
             WHEN Nombre = 'Highball 2x1' THEN 1
             WHEN Nombre = 'Pilsen 5X50' THEN 1
             WHEN Nombre = 'Cuzqueña Trigo 5x50' THEN 1
             WHEN Nombre = 'TE PITEADO 32% DSCTO' THEN 1
             WHEN Nombre = 'TE PITEADO 2X25' THEN 1
             WHEN Nombre = 'CHILCANO CANELA - 2x35' THEN 1
             WHEN Nombre = 'CHILCANO CLASICO - LA BLANCO 2X1' THEN 1
             WHEN Nombre = 'CHILCANO AGUAY. Y MARACUYA  - 3x50' THEN 1
             WHEN Nombre = 'PISCO SOUR MARACUYA - 2x40' THEN 1
             WHEN Nombre = 'CUBA LIBRE - HAVANA CLUB 2X1' THEN 1
             WHEN Nombre = 'CHILCANO AGUAY. Y MARACUYA  - 2x35' THEN 1
             WHEN Nombre = 'CHILCANO 2X1 CLASICO - LA BLANCO' THEN 1
             WHEN Nombre = 'CHILCANO CLASICO - OTRO 2X1' THEN 1
             WHEN Nombre = 'PISCO SOUR CLASICO - 2x40' THEN 1  ELSE 0
             END AS PROMOCIONES
             FROM data_vagon261 ")

##IDENTIFICACION DE DESCUENTOS
data_vagon261 <- sqldf::sqldf("SELECT *,
             CASE
             WHEN Descuento_de_venta_total > 0 THEN 1
             WHEN Descuento_global_de_venta_total > 0 THEN 1 ELSE 0
             END AS DESCUENTOS
             FROM data_vagon261")

##IDENTIFICACION DE CORTESIAS
data_vagon261 <- sqldf::sqldf("SELECT *,
             CASE
             WHEN Utilidad BETWEEN 0.1 AND 1 THEN 1 ELSE 0
             END AS CORTESIAS
             FROM data_vagon261")

##DATA LIMPIA (SIN PROMOCIONES,DESCUENTOS,CORTESIAS)
data_vagon261_clean <- sqldf::sqldf("SELECt * from data_vagon261 WHERE PROMOCIONES =0 AND DESCUENTOS =0 AND CORTESIAS=0")
data_vagon261_clean <- data_vagon261_clean %>% dplyr::select(Fecha_hora, Nombre,Categoria , Cantidad)

##VERIFICACION
sqldf::sqldf("SELECT distinct NOMBRE from data_vagon261_clean ORDER BY NOMBRE")


##TRANSFORMACIÓN PARA QUE HAYA SOLO 1 CANTIDAD DE PRODUCTO VENDIDO
data_vagon261_final_Cantidad1 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 1")
data_vagon261_final_Cantidad2 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 2")
data_vagon261_final_Cantidad3 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 3")
data_vagon261_final_Cantidad4 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 4")
data_vagon261_final_Cantidad5 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 5")
data_vagon261_final_Cantidad6 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 6")
data_vagon261_final_Cantidad7 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 7")
data_vagon261_final_Cantidad8 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 8")
data_vagon261_final_Cantidad9 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 9")
data_vagon261_final_Cantidad10 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 10")
data_vagon261_final_Cantidad11 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 11")
data_vagon261_final_Cantidad12 <-sqldf::sqldf("SELECT * FROM data_vagon261_clean WHERE CANTIDAD = 12")

data_vagon261_final <-sqldf::sqldf("SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad1
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad2 
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad2
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad3
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad3
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad3
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad4
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad4
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad4
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad4
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad5
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad5
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad5
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad5
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad5
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad6
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad7
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad8
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad9
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad10
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad11
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL 
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                              UNION ALL
                                              SELECT Fecha_Hora, Nombre, Categoria FROM data_vagon261_final_Cantidad12
                                             ")


##TRANSFORMACIÓN PARA AGREGAR FECHA Y RANGO DE HORARIO

data_vagon261_final$Fecha <- substr(data_vagon261_final$Fecha_hora, start = 1, stop = 10) #Sacamos la fecha
data_vagon261_final$Hora <- substr(data_vagon261_final$Fecha_hora, start = 12, stop = 19) #Sacamos la hora


Temporal <- sqldf::sqldf("SELECT *, SUBSTRING(fecha, 7,4) AÑO, SUBSTRING(fecha, 4,2) MES, SUBSTRING(fecha, 1,2) DIA from  data_vagon261_final") #Extraemos año,mes,dia
Temporal$Fecha2 <- paste(Temporal$MES,Temporal$DIA,Temporal$AÑO,sep = "/") #Juntamos en un nuevo formato m/d/y
Temporal$Dia_Nombre <- weekdays(Temporal$Fecha2) #sacamos nombre del dia de la semana


Temporal <- sqldf::sqldf("SELECT *, SUBSTRING(Hora,1,2) Hora_Sola from  Temporal") #Sacamos Solo la hora en unidad

Temporal <- sqldf::sqldf("SELECT *,
                         CASE 
                         WHEN Hora_Sola LIKE '12' THEN '1. 12pm a 4pm'
                         WHEN Hora_Sola LIKE '13' THEN '1. 12pm a 4pm'
                         WHEN Hora_Sola LIKE '14' THEN '1. 12pm a 4pm'
                         WHEN Hora_Sola LIKE '15' THEN '1. 12pm a 4pm'
                         WHEN Hora_Sola LIKE '16' THEN '2. 4pm a 6pm'
                         WHEN Hora_Sola LIKE '17' THEN '2. 4pm a 6pm'
                         WHEN Hora_Sola LIKE '18' THEN '3. 6pm a 8pm'
                         WHEN Hora_Sola LIKE '19' THEN '3. 6pm a 8pm'
                         WHEN Hora_Sola LIKE '20' THEN '4. 8pm a 10pm'
                         WHEN Hora_Sola LIKE '21' THEN '4. 8pm a 10pm'
                         WHEN Hora_Sola LIKE '22' THEN '5. 10pm a 12am'
                         WHEN Hora_Sola LIKE '23' THEN '5. 10pm a 12am'
                         WHEN Hora_Sola LIKE '00' THEN '6. 12am a 2am'
                         WHEN Hora_Sola LIKE '01' THEN '6. 12am a 2am'
                         WHEN Hora_Sola LIKE '02' THEN '7. 2am a +'
                         ELSE 'FUERA DE HORARIO'
                         END AS RANGO_HORARIO
                         from  Temporal")

data_vagon261_final <-sqldf::sqldf("SELECT Fecha, Dia_Nombre, Rango_Horario Rango_Horario_Venta, Nombre Nombre_Producto,Categoria Categoria_Producto FROM Temporal WHERE Rango_Horario != 'FUERA DE HORARIO'")


##TRANSFORMACIÓN PARA HALLAR EL NUMERO DE DIAS TRANSCURRIDOS

Temporal2 <- sqldf::sqldf("SELECT DISTINCT Fecha, Dia_Nombre from Temporal")
dias_transcurridos <- sqldf::sqldf("SELECT Dia_Nombre, count(*) Cantidad from Temporal2 GROUP BY Dia_Nombre")


##_------------------------------- FIN ----------------------------------------------









##_---------------------------ANALISIS DE INFORMACIÓN--------------------------------

#VALORES PERDIDOS
library(mice)
md.pattern(data_vagon261_final)


#PORCENTAJE DE VENTA POR DÍA 

#verificar los días (deben estar en igual de condiciones)
t <- sqldf::sqldf("SELECT DISTINCT Fecha, Dia_Nombre from data_vagon261_final")
sqldf::sqldf("SELECT Dia_Nombre, COUNT(fecha) from t GROUP BY Dia_Nombre")

# vamos a eliminar días para que la data este en las mismas condiciones de días
t <- sqldf::sqldf("SELECT * from data_vagon261_final WHERE (FECHA != '26/08/2022' AND FECHA != '22/07/2022' AND FECHA != '09/09/2022' AND FECHA != '24/03/2023') ") #viernes
t <- sqldf::sqldf("SELECT * from t WHERE (FECHA != '17/10/2022' AND FECHA != '21/11/2022' AND FECHA != '10/04/2023') ") #lunes
t <- sqldf::sqldf("SELECT * from t WHERE (FECHA != '20/08/2022' AND FECHA != '22/10/2022' AND FECHA != '25/02/2023') ") #sabado
t <- sqldf::sqldf("SELECT * from t WHERE (FECHA != '25/08/2022' AND FECHA != '08/12/2022' AND FECHA != '13/04/2023') ") #jueves
t <- sqldf::sqldf("SELECT * from t WHERE (FECHA != '11/10/2022' AND FECHA != '13/12/2022' AND FECHA != '17/01/2023') ") #martes
t <- sqldf::sqldf("SELECT * from t WHERE (FECHA != '30/11/2022' AND FECHA != '21/09/2022' AND FECHA != '08/03/2023' AND FECHA != '12/04/2023') ") #miercoles

#verificamos
sqldf::sqldf("SELECT DISTINCT Dia_Nombre , count(DISTINCT Fecha) from t group by Dia_Nombre ")



#Dia_Nombre <- c("Mon", "Tue", "Wed","Thu","Fri","Sat","Sun")
#Cant_Prod_Vendidos <- c(2022, 2452, 2690,3528,5991,6588,3927)
#t <- data.frame(cbind(Dia_Nombre, Cant_Prod_Vendidos))

Tabla <- data_vagon261_final %>% group_by(Dia_Nombre) %>% summarise(Total=n()) %>%   
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1))  

ggplot(Tabla, aes(x = Dia_Nombre, y=Porcentaje) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge(),fill="skyblue")+  ylim(c(0,100))+   labs(x="Día de la semana", y= "Porcentaje de Venta") + labs(fill = "") +                                          
  geom_text(aes(label=paste0( Porcentaje, "%")), 
            vjust=-0.9, 
            color="black", hjust=0.5,position = position_dodge(0.9),  
            angle=0, 
            size=7.0)+ theme_grey(base_size = 18)



#VENTAS POR FECHA
t <- sqldf::sqldf("SELECT DISTINCT Fecha, COUNT(*) cantidad from data_vagon261_final GROUP BY FECHA")
ListaFechas <- read.table(file = "ListaFechasCompletas.txt", header = TRUE)
t <- sqldf::sqldf("SELECT DISTINCT A.Fecha, B.cantidad from ListaFechas A join t B on A.FECHA = B.FECHA ")

t <- as.data.frame(t)

ggplot(data = t, mapping = aes(x = cantidad)) +
  geom_histogram(position = 'identity',alpha = 0.5,breaks = seq(0, 320, 24),fill="turquoise2")+
  ylab("Frecuencia")+
  xlab("Cantidad de venta por fecha")+
  geom_text(aes(label=..count..),stat="bin", size=5,vjust=-0.5,breaks = seq(0, 320, 24))+
  scale_x_continuous(breaks = seq(0, 320, 24)) + 
  theme(text = element_text(size = 15))


ggplot(t, aes(x = cantidad)) + 
  geom_histogram(fill = "turquoise2",alpha = 0.5 ,breaks = seq(0, 320, 24))+
  ylab("Porcentaje de fechas")+
  xlab("Cantidad de ventas")+
  #scale_y_continuous(breaks = seq(0,1000,10),labels = paste(seq(0, 1000, by = 10) / 100, "%", sep = ""))+
  scale_y_continuous(breaks = seq(0, 100, 5))+
  scale_x_continuous(breaks = seq(0, 320, 24)) +
  geom_text(aes(y = (..count..),label =  scales::percent((..count..)/sum(..count..))),breaks = seq(0, 320, 24), stat="bin",vjust=-0.5) +
  theme(text = element_text(size = 15))





#PORCENTAJE DE VENTA POR RANGO HORARIO 
Tabla <- data_vagon261_final %>% group_by(Rango_Horario_Venta) %>% summarise(Total=n()) %>%   
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1))  

ggplot(Tabla, aes(x = Rango_Horario_Venta, y=Porcentaje) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge(),fill="skyblue")+  ylim(c(0,100))+   labs(x="Rango Horario", y= "Porcentaje de Venta") + labs(fill = "") +                                          
  geom_text(aes(label=paste0( Porcentaje, "%")), 
            vjust= 0.5, hjust= -0.2,
            color="black", hjust=0.5,position = position_dodge(0.5),  
            angle=0, 
            size=7.0)+ theme_grey(base_size = 18) + coord_flip()


#PORCENTAJE DE VENTAS POR PRODUCTO 
Tabla <- data_vagon261_final %>% group_by(Nombre_Producto) %>% summarise(Total=n()) %>%   
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1))

Tabla <- dplyr::arrange(Tabla,desc(Total))
top_20 <- head(Tabla,20)

ggplot(top_20, aes(x = reorder(Nombre_Producto,Porcentaje), y=Porcentaje) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge(),fill="skyblue")+  ylim(c(0,100))+   labs(x="Nombre Producto", y= "Porcentaje de Venta") + labs(fill = "") +                                          
  geom_text(aes(label=paste0( Porcentaje, "%")), 
            vjust= 0.5, hjust= -0.2,
            color="black", hjust=0.5,position = position_dodge(0.5),  
            angle=0, 
            size=5.5)+ theme_grey(base_size = 18) + coord_flip()


#PORCENTAJE DE VENTAS POR CATEGORIA
Tabla <- data_vagon261_final %>% group_by(Categoria_Producto) %>% summarise(Total=n()) %>%   
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1))

Tabla <- dplyr::arrange(Tabla,desc(Total))

ggplot(Tabla, aes(x = reorder(Categoria_Producto,Porcentaje), y=Porcentaje) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge(),fill="skyblue")+  ylim(c(0,100))+   labs(x="Categoria Producto", y= "Porcentaje de Venta") + labs(fill = "") +                                          
  geom_text(aes(label=paste0( Porcentaje, "%")), 
            vjust= 0.5, hjust= -0.2,
            color="black", hjust=0.5,position = position_dodge(0.5),  
            angle=0, 
            size=5.5)+ theme_grey(base_size = 18) + coord_flip()





#GRAFICO DE CALOR POR DIA Y RANGO HORARIO

t <- sqldf::sqldf("SELECT dia_nombre, rango_horario_venta, count(Nombre_Producto) Cant_Prod_Vendidos from data_vagon261_final group by dia_nombre,rango_horario_venta")
ggplot(t, aes(x = Dia_Nombre, y = Rango_Horario_Venta, fill = Cant_Prod_Vendidos)) +
  geom_tile() +
  coord_fixed() + scale_fill_gradient(low = "white", high = "red") +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) + theme_grey(base_size = 18) +   labs(x="Día de la semana", y= "Rango Horario") + guides(fill = guide_colourbar(title = "Prod Vendidos"))







##_----------------------------------------------------------------------------------

#Lista de Productos
ListaP <- sqldf::sqldf("SELECT DISTINCT NOMBRE_PRODUCTO from data_vagon261_final ")

#Cantidad de Ventas de productos por día

Lunes <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Mon' GROUP BY NOMBRE_PRODUCTO")
Martes <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Tue' GROUP BY NOMBRE_PRODUCTO")
Miercoles <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Wed' GROUP BY NOMBRE_PRODUCTO")
Jueves <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Thu' GROUP BY NOMBRE_PRODUCTO")
Viernes <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Fri' GROUP BY NOMBRE_PRODUCTO")
Sabado <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Sat' GROUP BY NOMBRE_PRODUCTO")
Domingo <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Dia_Venta ='Sun' GROUP BY NOMBRE_PRODUCTO")


#Cantidad de Ventas de productos por rango de horario
de12a4 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='1. 12pm a 4pm' GROUP BY NOMBRE_PRODUCTO")
de4a6 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='2. 4pm a 6pm' GROUP BY NOMBRE_PRODUCTO")
de6a8 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='3. 6pm a 8pm' GROUP BY NOMBRE_PRODUCTO")
de8a10 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='4. 8pm a 10pm' GROUP BY NOMBRE_PRODUCTO")
de10a12 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='5. 10pm a 12am' GROUP BY NOMBRE_PRODUCTO")
de12a2 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='6. 12am a 2am' GROUP BY NOMBRE_PRODUCTO")
masde2 <- sqldf::sqldf("SELECT NOMBRE_PRODUCTO,COUNT(*) Cantidad FROM data_vagon261_final WHERE Rango_Horario_Venta ='7. 2am a +' GROUP BY NOMBRE_PRODUCTO")




#Juntar los dataframes de cantidades
ListaCantidad <- merge(x = ListaP, y = Lunes, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Martes, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Miercoles, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Jueves, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Viernes, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Sabado, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = Domingo, by = "Nombre_Producto",all.x = TRUE)

ListaCantidad <- merge(x = ListaCantidad, y = de12a4, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = de4a6, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = de6a8, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = de8a10, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = de10a12, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = de12a2, by = "Nombre_Producto",all.x = TRUE)
ListaCantidad <- merge(x = ListaCantidad, y = masde2, by = "Nombre_Producto",all.x = TRUE)


names (ListaCantidad) = c("Nombre_Producto","Venta_Lu", "Venta_Ma", "Venta_Mi", "Venta_Ju", "Venta_Vi", "Venta_Sa", "Venta_Do","Venta_12a4","Venta_4a6","Venta_6a8","Venta_8a10","Venta_10a12","Venta_12a2","Venta_2mas")
ListaCantidad[is.na(ListaCantidad)] <- 0



#probabilidades por producto por dia
Lunes$Lu_prob <- Lunes$Cantidad / colSums(Lunes[2])
Martes$Ma_prob <- Martes$Cantidad / colSums(Martes[2])
Miercoles$Mi_prob <- Miercoles$Cantidad / colSums(Miercoles[2])
Jueves$Ju_prob <- Jueves$Cantidad / colSums(Jueves[2])
Viernes$Vi_prob <- Viernes$Cantidad / colSums(Viernes[2])
Sabado$Sa_prob <- Sabado$Cantidad / colSums(Sabado[2])
Domingo$Do_prob <- Domingo$Cantidad / colSums(Domingo[2])


#probabilidades por producto por rango de horario
de12a4$de12a4_prob <- de12a4$Cantidad / colSums(de12a4[2])
de4a6$de4a6_prob <- de4a6$Cantidad / colSums(de4a6[2])
de6a8$de6a8_prob <- de6a8$Cantidad / colSums(de6a8[2])
de8a10$de8a10_prob <- de8a10$Cantidad / colSums(de8a10[2])
de10a12$de10a12_prob <- de10a12$Cantidad / colSums(de10a12[2])
de12a2$de12a2_prob <- de12a2$Cantidad / colSums(de12a2[2])
masde2$masde2_prob <- masde2$Cantidad / colSums(masde2[2])


#Juntar los dataframes
ListaProb <- merge(x = ListaP, y = Lunes, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Martes, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Miercoles, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Jueves, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Viernes, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Sabado, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = Domingo, by = "Nombre_Producto",all.x = TRUE)

ListaProb <- merge(x = ListaProb, y = de12a4, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = de4a6, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = de6a8, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = de8a10, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = de10a12, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = de12a2, by = "Nombre_Producto",all.x = TRUE)
ListaProb <- merge(x = ListaProb, y = masde2, by = "Nombre_Producto",all.x = TRUE)

ListaProb <- select(ListaProb,1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
ListaProb[is.na(ListaProb)] <- 0









