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
# Se genera una sola tabla quye basicamente es la misma pero cambiada de forma.

# Depertamental
d_empleo <- d_empleo %>% gather(key = 'Situación Laboral', value = 'Personas', 5:7) %>% arrange(`Código`) %>% 
  select(1:2, 4, `Situación Laboral`, `Personas`) %>% 
  mutate(`Porcentaje de Economicamente Activa`=round(`Población Económicamente Activa`/`Personas`*100, 2))
  
# Municipal


## Formatear tablas de Hogares

# Departamental



# Municipal


## Formatear tablas de Poblacion
# De la tabla de poblacion se generaron siete tablas nuevas siendo estas: 1. genero 2.Edad grupos de 15 años 
# 3. Edad grupo de 5 años 4. Zona demografica 5. relación con el Jefe del Hogar 6.Personas en Situación de Calle y
# finalmente 7. Estado Civil

# Departamental

d_genero <- d_poblacion %>% gather(key = 'Genero' , value= 'Cantidad', 4:5) %>% arrange(`Código`) %>% 
  select(1:3, `Genero`, `Cantidad`)

d_edad_15 <- d_poblacion %>% gather(key = 'Grupo de Edades 15 años', value='Cantidad', 6:10) %>% arrange(`Código`) %>%
  select(1:3, `Grupo de Edades 15 años`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/ `Total de personas`*100, 2))

d_edad_5 <- d_poblacion %>% gather(key = 'Grupo de Edades 5 años', value='Cantidad', 11:31) %>% arrange(`Código`) %>%
  select(1:3, `Grupo de Edades 5 años`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/ `Total de personas`*100, 2))

d_zona <- d_poblacion %>%  gather(key = 'Zona', value='Cantidad de Personas', 32:33) %>% arrange(`Código`) %>%
  select(1:3, `Zona`, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Total de personas`*100, 2))

d_relacion_jefe <- d_poblacion %>%  gather(key = 'Relacion con el Jefe', value = 'Cantidad de Personas', 34:44) %>% arrange(`Código`) %>%
  select(1:3, `Relacion con el Jefe`, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Total de personas`*100, 2))

d_situacion_calle <- d_poblacion %>%  
  select(1:3, 45)

d_estado_civil <- d_poblacion %>% gather(key = 'Estado Civil', value = 'Cantidad', 47:52) %>% arrange(`Código`) %>%
  select(1:2,46, `Estado Civil`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/`Población de 10 años o más`*100, 2))

rm(d_poblacion)

# Municipal



## Formatear tablas de pueblo
# La tabla de pueblo se formatea y se generan dos nuevas tablas, una de comunidad linguistica y la otra con el 
# idioma materno, y la de pueblo queda unicamente con los pueblos de pertenencia

# Departamental
d_comunidad_linguistica <- d_pueblo %>% gather(key = 'Comunidad Linguistica', value = 'Cantidad', 10:31) %>% arrange(`Código`) %>%
  select(1:3, `Comunidad Linguistica`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/ `Total de personas`*100, 2))

d_idioma_materno <- d_pueblo %>% gather(key = 'Idioma Materno', value = 'Cantidad', 32:61) %>% arrange(`Código`) %>%
  select(1:2,62, `Idioma Materno`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/`Población de 4 años o más`*100, 2)) %>%
  mutate(`Idioma Materno` = str_remove(`Idioma Materno`, "__1"))

d_pueblo <- d_pueblo %>% gather(key = 'Pueblo de Pertenencia', value='Cantidad', 4:9) %>% arrange(`Código`) %>%
  select(1:3, `Pueblo de Pertenencia`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/ `Total de personas`*100, 2))

# Municipal



## Formatear tablas de tecnologia
# Esta tabla se formatea y se separa en dos tablas, una con información individual y la otra con conglomerado

#### Departamental
d_tecnologia <- d_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
d_tecnologia_agrupado <- d_tecnologia %>% select(1:3, 13:16) 


d_tecnologia <-d_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))
#### Municipal

## Formatear tablas de Vivienda

#### Departamental


#### Municipal




