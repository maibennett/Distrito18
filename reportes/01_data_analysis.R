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

URLd18 = "https://raw.githubusercontent.com/maibennett/d18/main/data/otros/d_export.csv"
URLd18_listas_cores = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_core2017_candidatos.csv"
URLd18_derecha_cores = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_core2017_derecha.csv"
URLd18_listas_diputados = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_diputados2017_candidatos.csv"
URLd18_derecha_diputados = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_diputados2017_derecha.csv"
URLd18_listas_concejales = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_concejales2016_candidatos.csv"
URLd18_derecha_concejales = "https://raw.githubusercontent.com/maibennett/d18/main/data/servel/resultados_concejales2016_derecha.csv"

d = read.csv(URLd18)
d_listas_cores = read.csv(URLd18_listas_cores)
d_derecha_cores = read.csv(URLd18_derecha_cores)
d_listas_diputados = read.csv(URLd18_listas_diputados)
d_derecha_diputados = read.csv(URLd18_derecha_diputados)
d_listas_concejales = read.csv(URLd18_listas_concejales)
d_derecha_concejales = read.csv(URLd18_derecha_concejales)

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


## For exporting

# d2017_export = d2017 %>% group_by(REGION,COMUNA,RANGO_EDAD,rango_edad3) %>% 
#   summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))
# 
# d2017_export$participacion2017 = d2017_export$vote/d2017_export$n

######

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

### For exporting
# d2020_export = d2020_padron %>% group_by(REGION,COMUNA,RANGO_EDAD,rango_edad3) %>% 
#   summarise(n = sum(!is.na(rango_edad3)))
# 
# d2020_export = d2020_export[,c("COMUNA","RANGO_EDAD","n")]
# 
# names(d2020_export) = c("COMUNA","RANGO_EDAD","n2020")
# 
# d_export = left_join(d2017_export,d2020_export,by=c("COMUNA","RANGO_EDAD"))
# 
# d_export = d_export[d_export$REGION=="Del Maule",]

comunas_d18 = c("Cauquenes","Chanco","Colbun","Linares","Longavi","Parral",
                "Pelluhue","Retiro","San Javier","Villa Alegre","Yerbas Buenas")

d_export = d_export[d_export$COMUNA %in% comunas_d18,]

#######

d2020_padron_comuna = d2020_padron %>% group_by(REGION,COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(rango_edad3)))


d2020_padron_comuna = d2020_padron_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                                          names_from = rango_edad3,
                                                          values_from = n)

names(d2020_padron_comuna) = c("COMUNA","n_1","n_2","n_3")

# Cambiar comunas que tienen distinto nombre:
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Aysen"] = "Aisen"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Cabo De Hornos(Ex-Navarino)"] = "Cabo De Hornos"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Paiguano"] = "Paihuano"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Trehuaco"] = "Treguaco"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Llaillay"] = "Llay-Llay"

# Votacion 2020

names(d2020_comuna) = c("COMUNA","n_mesas","n","vote","participacion")
d2020_comuna = d2020_comuna[,c("COMUNA","n","vote","participacion")]

d2020_comuna$year = 2020

d2020_comuna = left_join(d2020_comuna,d2020_padron_comuna, by="COMUNA")

# No tenemos datos de votacion por grupo etario por grupo etario, asi que incluyo 0s

d2020_comuna = d2020_comuna %>% mutate(vote_1 = 0, vote_2 = 0, vote_3 = 0,
                                       participacion_1 = 0, participacion_2 = 0,
                                       participacion_3 = 0) %>%
  relocate(COMUNA,n_1,n_2,n_3,vote_1,vote_2,vote_3,participacion_1,participacion_2,
           participacion_3, year)

# Merge 

d_all = data.frame(rbind(d2017_comuna,
                         d2020_comuna))

# Etapas COVID
etapas$group_etapa_plebiscito = NA
etapas$group_etapa_plebiscito[etapas$X24.Oct<=2] = 1
etapas$group_etapa_plebiscito[etapas$X24.Oct>2] = 0


d_all = left_join(d_all, etapas[,c("COMUNA","CUT","X24.Oct","group_etapa_plebiscito")], by = "COMUNA")

d_all = d_all %>% rename(cod_comuna = CUT)

d_all = left_join(d_all, casen, by="cod_comuna")

## Efecto pandemia y efecto plebiscito:

d_all$p1 = d_all$n_1/d_all$n
d_all$p2 = d_all$n_2/d_all$n
d_all$p3 = d_all$n_3/d_all$n

d_all = left_join(d_all,d_derecha,by="COMUNA")

lm1 = lm(participacion ~ n + yautcorh + factor(year)*(
  p1 + p3 + p_derecha2017 + group_etapa_plebiscito), 
  data = d_all)

sum_lm = summary(lm1)
sum_lm

comunas_d18 = unique(d$COMUNA)

d_18 = d_all[(d_all$COMUNA %in% comunas_d18) & d_all$year==2020,]

efecto_covid = sum_lm$coefficients[4,1] +
  sum_lm$coefficients[10,1]*d_18$p3 + 
  sum_lm$coefficients[12,1]*d_18$group_etapa_plebiscito


efecto_plebiscito = sum_lm$coefficients[9,1]*d_18$p1 + 
  sum_lm$coefficients[11,1]*d_18$p_derecha2017


efectos = as.data.frame(cbind(d_18$COMUNA,d_18$participacion,efecto_covid,efecto_plebiscito))
names(efectos) = c("COMUNA","p2020","EfectoCOVID","EfectoPlebiscito")

write.csv(efectos,file="C:/Users/mc72574/Dropbox/Distrito18/data/otros/efectos.csv")