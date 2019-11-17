library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(DataExplorer)
library(reshape2)

rm(list = ls())

# Abrir todos los archivos que existen en la carpeta data
archivos <- list.files("Data")
for (nombre in archivos) {
  temp_df <- read_excel(path =  paste("Data", nombre, sep = "/"), skip = 9) %>% select(-X__1)
  temp_df <- temp_df %>% filter(complete.cases(temp_df))
  temp_df[1] <- as.integer(pull(temp_df, 1))
  temp_df[2] <- factor(pull(temp_df,2), ordered= TRUE)
  
  pre<- ifelse(str_detect(nombre, "municipal"), "m_", "d_") 
  nombre <- str_remove(nombre, "(_municipal|_departamental)\\.xlsx$")
  nombre <- paste(pre,nombre, sep = "")
  assign(nombre, temp_df)
  
}

rm(nombre, pre, archivos, temp_df, n)

## Formatear tablas de Caracteristicas

# Las tablas de caracteristicas se convirtieron en cuatro tablas: la de lugar de nacimiento, 
#    residencia en 2013, dificultades y por ultimo tambien la de hijos por mujeres fertiles

##### Departamental
d_lugar_nacimiento <- d_caracteristicas %>% select(1:7) %>% 
  gather(`Lugar de Nacimiento`, `Personas`, 4:7) %>% 
  mutate(`Porcentaje de la Poblacion`= round(`Personas`/`Total de personas`*100, 2))

d_recidencia_2013 <- d_caracteristicas %>% select(1:3, 8:12) %>% 
gather(key=`Lugar de Residencia en 2013`, value = `Personas`, 4:8) %>%  
  mutate(`Porcentaje de la Poblacion`= round(`Personas`/`Total de personas`*100, 2)) %>% 
  mutate(`Lugar de Residencia en 2013`= str_remove(`Lugar de Residencia en 2013`, '__1$'))

# Cambiar Nombres de Columnas
names(d_caracteristicas)[14] <- "Sin dificultad para Ver"
names(d_caracteristicas)[15] <- "Con dificultad para Ver"
names(d_caracteristicas)[16] <- "NA_Ver"
names(d_caracteristicas)[17] <- "Sin dificultad para Oir"
names(d_caracteristicas)[18] <- "Con dificultad para Oir"
names(d_caracteristicas)[19] <- "NA_Oir"
names(d_caracteristicas)[20] <- "Sin dificultad para Caminar"
names(d_caracteristicas)[21] <- "Con dificultad para Caminar"
names(d_caracteristicas)[22] <- "NA_Caminar"
names(d_caracteristicas)[23] <- "Sin dificultad para Recordar"
names(d_caracteristicas)[24] <- "Con dificultad para Recordar"
names(d_caracteristicas)[25] <- "NA_Recordar"
names(d_caracteristicas)[26] <- "Sin dificultad de Cuidado Personal"
names(d_caracteristicas)[27] <- "Con dificultad de Cuidado Personal"
names(d_caracteristicas)[28] <- "NA_Cuidado_Personal"
names(d_caracteristicas)[29] <- "Sin dificultad para Comunicarse"
names(d_caracteristicas)[30] <- "Con dificultad para Comunicarse"
names(d_caracteristicas)[31] <- "NA_Comunicarse"

names(d_caracteristicas)[33] <- "Con 0 hijos nacidos"
names(d_caracteristicas)[34] <- "Con  1 hijos nacidos"
names(d_caracteristicas)[35] <- "Con  2 hijos nacidos"
names(d_caracteristicas)[36] <- "Con  3 hijos nacidos"
names(d_caracteristicas)[37] <- "Con  4 hijos nacidos"
names(d_caracteristicas)[38] <- "Con  5 o más hijos nacidos"
names(d_caracteristicas)[39] <- "NA_Hijos_Nacidos"

names(d_caracteristicas)[40] <- "Con  0 hijos sobrevivientes"
names(d_caracteristicas)[41] <- "Con  1 hijos sobrevivientes"
names(d_caracteristicas)[42] <- "Con  2 hijos sobrevivientes"
names(d_caracteristicas)[43] <- "Con  3 hijos sobrevivientes"
names(d_caracteristicas)[44] <- "Con  4 hijos sobrevivientes"
names(d_caracteristicas)[45] <- "Con  5 o más hijos sobrevivientes"

d_dificultades <- d_caracteristicas %>% gather(key = "Dificultades", value = "Cantidad de Personas", 14:31) %>% 
  select(1:2,13, Dificultades, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje con Dificultad`= round(`Cantidad de Personas`/ `Población de 4 años o más` * 100 ,2))

d_hijos_x_mujeres <- d_caracteristicas %>% gather(key = 'Situación', value = 'Cantidad de Mujeres', 33:45) %>% 
  select(1:2, 32, `Situación`, `Cantidad de Mujeres`) %>% 
  mutate(`Porcentaje de Mujeres`= round(`Cantidad de Mujeres`/`Total de mujeres en edad fértil` ,2))

rm(d_caracteristicas)

#### Municipal




## Formatear tablas de Educacion
# La tabla de educacion se combirtio en cinco tablas, la de 1. educacion que contiene el nivel de educacion, 
# 2. causa de inasistencias, 3. alfabetizacion, 4. asistencia y por ultimo 5. lugar de estudio
### Departamental

d_causas_inasistencia <- d_educacion %>% 
  gather(key = 'Causa de Inasistencia 4-29 años', value = 'Cantidad de Personas', 12:20) %>% 
  select(1:2, `Causa de Inasistencia 4-29 años`, `Cantidad de Personas`) %>% arrange(Código)

d_alfabetizacion <- d_educacion %>% 
  gather(key = 'Situacion' ,value= 'Personas', 22, 23) %>% arrange(Código) %>% 
  select(1:2, 21, `Situacion`, `Personas`) %>% 
  mutate(`Porcentaje Alfabetizacion`= round(`Personas`/`Población de 7 años o más`*100,2))

d_asistencia <- d_educacion %>% 
  gather(key = 'Situacion' ,value= 'Personas', 24, 25) %>% arrange(Código) %>% 
  select(1:2, 21, `Situacion`, `Personas`) %>% 
  mutate(`Porcentaje Asistencia`= round(`Personas`/`Población de 7 años o más`*100,2))

d_lugar_estudio <- d_educacion %>% 
  gather(key = 'Lugar de Estudio' ,value= 'Cantidad de Personas x Lugar', 26:29) %>% arrange(Código) %>% 
  select(1:2, 21, `Lugar de Estudio`, `Cantidad de Personas x Lugar`) %>% 
  mutate(`Porcentaje por Lugar de Estudio`= round(`Cantidad de Personas x Lugar`/`Población de 7 años o más`*100,2)) %>% 
  mutate(`Lugar de Estudio` = recode(`Lugar de Estudio`, 'No especificado__1'= 'NA_Lugar_Estudio'))


d_educacion<- d_educacion %>% gather(key = 'Nivel Educativo', value = 'Cantidad de Personas', 3:11) %>% 
  select(1:2, `Nivel Educativo`,`Cantidad de Personas`) %>% arrange(Código)

### Municipal


## Formatear tablas de Empleo



## Formatear tablas de tecnologia
#### Departamental
d_tecnologia <- d_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
d_tecnologia_agrupado <- d_tecnologia %>% select(1:3, 13:16) 


d_tecnologia <-d_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))
#### Municipal




