#Clear memory
rm(list = ls())

#Clear the console
cat("\014")

#devtools::install_github("hrbrmstr/firasans")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(designmatch)
library(hrbrthemes)
library(firasans)
library(augsynth)
library(magrittr)

#Turn off scientific notation (turn back on with 0)
options(scipen = 999)
set.seed(100)

### Load data (Linea 213 carga los datos que estan limpios que esta en el repositorio)

dir_data = "C:/Users/mc72574/Dropbox/covid/participacion/"

######## Data participacion (bajada del servel)

d2017 = read.csv(paste0(dir_data,"data_original/VW_VOTARON_2017_1V.csv"), sep = ";", header = TRUE)
d2020_padron = read.csv(paste0(dir_data,"data_original/PADRON2020.csv"), sep = ";", header = TRUE, encoding = "UTF-8")
d2020_comuna = read.csv(paste0(dir_data,"data_original/votacion2020.csv"), header = TRUE)

###########################

####### Data etapas COVID (https://docs.google.com/spreadsheets/d/1WieweYNSPdpmjUIyYcbKp1oaqwlnD61_/edit?usp=drive_web&ouid=117681751153105889471&dls=true)

etapas = read.csv(paste0(dir_data,"data_original/CUT_CUARENTENAS_COVID_24_25_Oct.csv"))

############################

####### Data CASEN 2017

casen = read.csv("https://raw.githubusercontent.com/maibennett/participacion/main/data/casen2017_clean.csv")

############################

# Crear variables para rangos etarios discretizados (joven <35, medio 35-64, mayor 65+)
rango_edad_joven = sort(unique(d2017$RANGO_EDAD))[1:4]
rango_edad_medio = sort(unique(d2017$RANGO_EDAD))[5:10]
rango_edad_mayor = sort(unique(d2017$RANGO_EDAD))[11:14]

# Elimino voto en el exterior:
d2017 = d2017[d2017$COMUNA!="",]

d2017 = d2017 %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))


d2017_comuna = d2017 %>% group_by(REGION,COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))

d2017_comuna$participacion = d2017_comuna$vote/d2017_comuna$n

d2017_comuna = d2017_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = c(n, vote, participacion))

d2017_comuna = d2017_comuna %>% mutate(n = n_1+n_2+n_3,
                                       vote = vote_1+vote_2+vote_3,
                                       participacion = vote/n)


d2017_comuna$year = 2017

# 2020

d2020_padron = d2020_padron %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))

d2020_padron = d2020_padron[d2020_padron$COMUNA!="",]

d2020_padron_comuna = d2020_padron %>% group_by(REGION,COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(rango_edad3)))


d2020_padron_comuna = d2020_padron_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                                          names_from = rango_edad3,
                                                          values_from = n)

names(d2020_padron_comuna) = c("REGION","COMUNA","n_1","n_2","n_3")

# Cambiar comunas que tienen distinto nombre:
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Aysen"] = "Aisen"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Cabo De Hornos(Ex-Navarino)"] = "Cabo De Hornos"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Paiguano"] = "Paihuano"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Trehuaco"] = "Treguaco"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Llaillay"] = "Llay-Llay"

# Votacion 2020

names(d2020_comuna) = c("REGION","COMUNA","n_mesas","n","vote","participacion")
d2020_comuna = d2020_comuna[,c("COMUNA","n","vote","participacion")]

d2020_comuna$year = 2020

d2020_comuna = left_join(d2020_comuna,d2020_padron_comuna, by="COMUNA")

# No tenemos datos de votacion por grupo etario por grupo etario, asi que incluyo 0s

d2020_comuna = d2020_comuna %>% mutate(vote_1 = 0, vote_2 = 0, vote_3 = 0,
                                       participacion_1 = 0, participacion_2 = 0,
                                       participacion_3 = 0) %>%
  relocate(REGION,COMUNA,n_1,n_2,n_3,vote_1,vote_2,vote_3,participacion_1,participacion_2,
           participacion_3, year)

# Merge 

d = data.frame(rbind(d2017_comuna,
                     d2020_comuna))

# Etapas COVID
etapas$group_etapa_plebiscito = NA
etapas$group_etapa_plebiscito[etapas$X24.Oct<=2] = 1
etapas$group_etapa_plebiscito[etapas$X24.Oct>2] = 0


d = left_join(d, etapas[,c("COMUNA","CUT","X24.Oct","group_etapa_plebiscito")], by = "COMUNA")

d = d %>% rename(cod_comuna = CUT)

d = left_join(d, casen, by="cod_comuna")

## Efecto pandemia y efecto plebiscito:

d$p1 = d$n_1/d$n
d$p2 = d$n_2/d$n
d$p3 = d$n_3/d$n

lm1 = lm(participacion ~ n + p1*factor(year) + p2*factor(year)
         + p3*factor(year) + factor(year) + yautcorh, data = d[d$REGION=="Del Maule",])