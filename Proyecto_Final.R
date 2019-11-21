library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(corrplot)

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

rm(nombre, pre, archivos, temp_df)

#EDA



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
m_lugar_nacimiento <- m_caracteristicas %>% select(1:7) %>% 
  gather(`Lugar de Nacimiento`, `Personas`, 4:7) %>% 
  mutate(`Porcentaje de la Poblacion`= round(`Personas`/`Total de personas`*100, 2))

m_recidencia_2013 <- m_caracteristicas %>% select(1:3, 8:12) %>% 
  gather(key=`Lugar de Residencia en 2013`, value = `Personas`, 4:8) %>%  
  mutate(`Porcentaje de la Poblacion`= round(`Personas`/`Total de personas`*100, 2)) %>% 
  mutate(`Lugar de Residencia en 2013`= str_remove(`Lugar de Residencia en 2013`, '__1$'))

# Cambiar Nombres de Columnas
names(m_caracteristicas)[14] <- "Sin dificultad para Ver"
names(m_caracteristicas)[15] <- "Con dificultad para Ver"
names(m_caracteristicas)[16] <- "NA_Ver"
names(m_caracteristicas)[17] <- "Sin dificultad para Oir"
names(m_caracteristicas)[18] <- "Con dificultad para Oir"
names(m_caracteristicas)[19] <- "NA_Oir"
names(m_caracteristicas)[20] <- "Sin dificultad para Caminar"
names(m_caracteristicas)[21] <- "Con dificultad para Caminar"
names(m_caracteristicas)[22] <- "NA_Caminar"
names(m_caracteristicas)[23] <- "Sin dificultad para Recordar"
names(m_caracteristicas)[24] <- "Con dificultad para Recordar"
names(m_caracteristicas)[25] <- "NA_Recordar"
names(m_caracteristicas)[26] <- "Sin dificultad de Cuidado Personal"
names(m_caracteristicas)[27] <- "Con dificultad de Cuidado Personal"
names(m_caracteristicas)[28] <- "NA_Cuidado_Personal"
names(m_caracteristicas)[29] <- "Sin dificultad para Comunicarse"
names(m_caracteristicas)[30] <- "Con dificultad para Comunicarse"
names(m_caracteristicas)[31] <- "NA_Comunicarse"

names(m_caracteristicas)[33] <- "Con 0 hijos nacidos"
names(m_caracteristicas)[34] <- "Con  1 hijos nacidos"
names(m_caracteristicas)[35] <- "Con  2 hijos nacidos"
names(m_caracteristicas)[36] <- "Con  3 hijos nacidos"
names(m_caracteristicas)[37] <- "Con  4 hijos nacidos"
names(m_caracteristicas)[38] <- "Con  5 o más hijos nacidos"
names(m_caracteristicas)[39] <- "NA_Hijos_Nacidos"

names(m_caracteristicas)[40] <- "Con  0 hijos sobrevivientes"
names(m_caracteristicas)[41] <- "Con  1 hijos sobrevivientes"
names(m_caracteristicas)[42] <- "Con  2 hijos sobrevivientes"
names(m_caracteristicas)[43] <- "Con  3 hijos sobrevivientes"
names(m_caracteristicas)[44] <- "Con  4 hijos sobrevivientes"
names(m_caracteristicas)[45] <- "Con  5 o más hijos sobrevivientes"

m_dificultades <- m_caracteristicas %>% gather(key = "Dificultades", value = "Cantidad de Personas", 14:31) %>% 
  select(1:2,13, Dificultades, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje con Dificultad`= round(`Cantidad de Personas`/ `Población de 4 años o más` * 100 ,2))

m_hijos_x_mujeres <- m_caracteristicas %>% gather(key = 'Situación', value = 'Cantidad de Mujeres', 33:45) %>% 
  select(1:2, 32, `Situación`, `Cantidad de Mujeres`) %>% 
  mutate(`Porcentaje de Mujeres`= round(`Cantidad de Mujeres`/`Total de mujeres en edad fértil` ,2))

rm(m_caracteristicas)

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
m_causas_inasistencia <- m_educacion %>% 
  gather(key = 'Causa de Inasistencia 4-29 años', value = 'Cantidad de Personas', 12:20) %>% 
  select(1:2, `Causa de Inasistencia 4-29 años`, `Cantidad de Personas`) %>% arrange(Código)

m_alfabetizacion <- m_educacion %>% 
  gather(key = 'Situacion' ,value= 'Personas', 22, 23) %>% arrange(Código) %>% 
  select(1:2, 21, `Situacion`, `Personas`) %>% 
  mutate(`Porcentaje Alfabetizacion`= round(`Personas`/`Población de 7 años o más`*100,2))

m_asistencia <- m_educacion %>% 
  gather(key = 'Situacion' ,value= 'Personas', 24, 25) %>% arrange(Código) %>% 
  select(1:2, 21, `Situacion`, `Personas`) %>% 
  mutate(`Porcentaje Asistencia`= round(`Personas`/`Población de 7 años o más`*100,2))

m_lugar_estudio <- m_educacion %>% 
  gather(key = 'Lugar de Estudio' ,value= 'Cantidad de Personas x Lugar', 26:29) %>% arrange(Código) %>% 
  select(1:2, 21, `Lugar de Estudio`, `Cantidad de Personas x Lugar`) %>% 
  mutate(`Porcentaje por Lugar de Estudio`= round(`Cantidad de Personas x Lugar`/`Población de 7 años o más`*100,2)) %>% 
  mutate(`Lugar de Estudio` = recode(`Lugar de Estudio`, 'No especificado__1'= 'NA_Lugar_Estudio'))

m_educacion<- m_educacion %>% gather(key = 'Nivel Educativo', value = 'Cantidad de Personas', 3:11) %>% 
  select(1:2, `Nivel Educativo`,`Cantidad de Personas`) %>% arrange(Código)

## Formatear tablas de Empleo
# Se genera una sola tabla quye basicamente es la misma pero cambiada de forma.

# Depertamental
d_empleo <- d_empleo %>% gather(key = 'Situación Laboral', value = 'Personas', 5:7) %>% arrange(`Código`) %>% 
  select(1:2, 4, `Situación Laboral`, `Personas`) %>% 
  mutate(`Porcentaje de Economicamente Activa`=round(`Personas`/`Población Económicamente Activa`*100, 2))
  
# Municipal
m_empleo <- m_empleo %>% gather(key = 'Situación Laboral', value = 'Personas', 5:7) %>% arrange(`Código`) %>% 
  select(1:2, 4, `Situación Laboral`, `Personas`) %>% 
  mutate(`Porcentaje de Economicamente Activa`=round(`Personas`/`Población Económicamente Activa`*100, 2))

## Formatear tablas de Hogares

# Departamental
d_distribucion_hogares <- d_hogares %>% gather(key = 'Area', value = 'Distribución de Hogares', 3:4) %>% arrange(`Código`) %>% 
  select(1:2, `Area`, `Distribución de Hogares`) %>% 
  mutate(Area= str_remove(`Area`, "Distribución de hogares por área"))

d_hogares <- d_hogares %>% select(1,2,5,6)

# Municipal
m_distribucion_hogares <- m_hogares %>% gather(key = 'Area', value = 'Distribución de Hogares', 3:4) %>% arrange(`Código`) %>% 
  select(1:2, `Area`, `Distribución de Hogares`) %>% 
  mutate(Area= str_remove(`Area`, "Distribución de hogares por área"))

m_hogares <- m_hogares %>% select(1,2,5,6)

## Formatear tablas de Poblacion
# De la tabla de poblacion se generaron siete tablas nuevas siendo estas: 1. genero 2.Edad grupos de 15 años 
# 3. Edad grupo de 5 años 4. Zona demografica 5. relación con el Jefe del Hogar 6.Personas en Situación de Calle y
# finalmente 7. Estado Civil

# Departamental
d_genero <- d_poblacion %>% gather(key = 'Genero' , value= 'Cantidad', 4:5) %>% arrange(`Código`) %>% 
  select(1:3, `Genero`, `Cantidad`)

d_edad_15 <- d_poblacion %>% gather(key = 'Grupo de Edades 15 años', value='Cantidad', 6:10, factor_key = TRUE) %>% arrange(`Código`) %>%
  select(1:3, `Grupo de Edades 15 años`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/ `Total de personas`*100, 2))

d_edad_5 <- d_poblacion %>% gather(key = 'Grupo de Edades 5 años', value='Cantidad', 11:31, factor_key = TRUE) %>% arrange(`Código`) %>%
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
m_genero <- m_poblacion %>% gather(key = 'Genero' , value= 'Cantidad', 4:5) %>% arrange(`Código`) %>% 
  select(1:3, `Genero`, `Cantidad`)

m_edad_15 <- m_poblacion %>% gather(key = 'Grupo de Edades 15 años', value='Cantidad', 6:10, factor_key = TRUE) %>% arrange(`Código`) %>%
  select(1:3, `Grupo de Edades 15 años`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/ `Total de personas`*100, 2))

m_edad_5 <- m_poblacion %>% gather(key = 'Grupo de Edades 5 años', value='Cantidad', 11:31, factor_key = TRUE) %>% arrange(`Código`) %>%
  select(1:3, `Grupo de Edades 5 años`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/ `Total de personas`*100, 2))

m_zona <- m_poblacion %>%  gather(key = 'Zona', value='Cantidad de Personas', 32:33) %>% arrange(`Código`) %>%
  select(1:3, `Zona`, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Total de personas`*100, 2))

m_relacion_jefe <- m_poblacion %>%  gather(key = 'Relacion con el Jefe', value = 'Cantidad de Personas', 34:44) %>% arrange(`Código`) %>%
  select(1:3, `Relacion con el Jefe`, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Total de personas`*100, 2))

m_situacion_calle <- m_poblacion %>%  
  select(1:3, 45)

m_estado_civil <- m_poblacion %>% gather(key = 'Estado Civil', value = 'Cantidad', 47:52) %>% arrange(`Código`) %>%
  select(1:2,46, `Estado Civil`, `Cantidad`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad`/`Población de 10 años o más`*100, 2))

rm(m_poblacion)

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

## Formatear tablas de tecnologia
# Esta tabla se formatea y se separa en dos tablas, una con información individual y la otra con conglomerado

#### Departamental
d_tecnologia <- d_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
d_tecnologia_agrupado <- d_tecnologia %>% select(1:3, 13:16) 


d_tecnologia <-d_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))
#### Municipal
m_tecnologia <- m_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
m_tecnologia_agrupado <- m_tecnologia %>% select(1:3, 13:16) 


m_tecnologia <-m_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))

## Formatear tablas de Vivienda

#### Departamental
d_tipo_ocupacion <- d_vivienda %>% gather(key = 'Tipo de Ocupacion', value = 'Cantidad de Viviendas', 13:16) %>% arrange(`Código`) %>% 
  select(1:2,4, `Tipo de Ocupacion`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2))

d_material_pared <- d_vivienda %>% gather(key = 'Material Predominante de la Pared', value = 'Cantidad de Viviendas', 17:27) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante de la Pared`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante de la Pared` = str_remove(`Material Predominante de la Pared`, "__\\d"))

d_material_techo <- d_vivienda %>% gather(key = 'Material Predominante del Techo', value = 'Cantidad de Viviendas', 28:35) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante del Techo`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante del Techo` = str_remove(`Material Predominante del Techo`, "__\\d"))

d_material_piso <- d_vivienda %>% gather(key = 'Material Predominante del Piso', value = 'Cantidad de Viviendas', 36:43) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante del Piso`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante del Piso` = str_remove(`Material Predominante del Piso`, "__\\d"))


d_vivienda <- d_vivienda %>% gather(key = 'Tipo de Vivienda', value = 'Cantidad de Viviendas', 5:12) %>% arrange(`Código`) %>% 
  select(1:3, `Tipo de Vivienda`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas`= round(`Cantidad de Viviendas`/ `Total de viviendas`*100, 2))

#### Municipal
m_tipo_ocupacion <- m_vivienda %>% gather(key = 'Tipo de Ocupacion', value = 'Cantidad de Viviendas', 13:16) %>% arrange(`Código`) %>% 
  select(1:2,4, `Tipo de Ocupacion`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2))

m_material_pared <- m_vivienda %>% gather(key = 'Material Predominante de la Pared', value = 'Cantidad de Viviendas', 17:27) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante de la Pared`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante de la Pared` = str_remove(`Material Predominante de la Pared`, "__\\d"))

m_material_techo <- m_vivienda %>% gather(key = 'Material Predominante del Techo', value = 'Cantidad de Viviendas', 28:35) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante del Techo`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante del Techo` = str_remove(`Material Predominante del Techo`, "__\\d"))

m_material_piso <- m_vivienda %>% gather(key = 'Material Predominante del Piso', value = 'Cantidad de Viviendas', 36:43) %>% arrange(`Código`) %>% 
  select(1:2,4, `Material Predominante del Piso`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas Particulares`= round(`Cantidad de Viviendas`/ `Total de viviendas particulares`*100, 2)) %>% 
  mutate(`Material Predominante del Piso` = str_remove(`Material Predominante del Piso`, "__\\d"))

m_vivienda <- m_vivienda %>% gather(key = 'Tipo de Vivienda', value = 'Cantidad de Viviendas', 5:12) %>% arrange(`Código`) %>% 
  select(1:3, `Tipo de Vivienda`, `Cantidad de Viviendas`) %>% 
  mutate(`Porcentaje de Viviendas`= round(`Cantidad de Viviendas`/ `Total de viviendas`*100, 2))



write.csv(d_alfabetizacion, file = 'Base de Datos/d_alfabetizacion.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_causas_inasistencia, file = 'Base de Datos/d_causas_inasistencia.csv', row.names = FALSE)
write.csv(d_comunidad_linguistica, file = 'Base de Datos/d_comunidad_linguistica.csv', row.names = FALSE)
write.csv(d_dificultades, file = 'Base de Datos/d_dificultades.csv', row.names = FALSE)
write.csv(d_edad_15, file = 'Base de Datos/d_edad_15.csv', row.names = FALSE)
write.csv(d_edad_5, file = 'Base de Datos/d_edad_5.csv', row.names = FALSE)
write.csv(d_empleo, file = 'Base de Datos/d_empleo.csv', row.names = FALSE)
write.csv(d_estado_civil, file = 'Base de Datos/d_estado_civil.csv', row.names = FALSE)
write.csv(d_genero, file = 'Base de Datos/d_genero.csv', row.names = FALSE)
write.csv(d_hijos_x_mujeres, file = 'Base de Datos/d_hijos_x_mujeres.csv', row.names = FALSE)
write.csv(d_hogares, file = 'Base de Datos/d_hogares.csv', row.names = FALSE)
write.csv(d_lugar_estudio, file = 'Base de Datos/d_lugar_estudio.csv', row.names = FALSE)
write.csv(d_lugar, file = 'Base de Datos/d_lugar.csv', row.names = FALSE)

write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)



d_edad_5 %>% plot_ly(x =~`Grupo de Edades 5 años`, y = ~`Porcentaje de Poblacion`, color= ~Departamento,colors = "Accent", barmode = 'group', type = 'bar')%>%
  layout(title = 'Grupos de Edades') 

d_estado_civil %>% plot_ly(x =~`Estado Civil`, y = ~`Porcentaje de Poblacion`, color= ~Departamento,colors = "Accent", barmode = 'group', type = 'bar')%>%
  layout(title = 'Estado Civil') 

d_estado_civil %>% plot_ly(labels =~`Estado Civil`, values = ~`Porcentaje de Poblacion` ,colors = "Accent", type = 'pie')%>%
  layout(title = 'Estado Civil Pie') 

d_educacion %>% plot_ly(labels = ~`Nivel Educativo`, values = ~`Cantidad de Personas`, type = 'pie') %>% 
  layout(title = 'Personas por Nivel Educativo')

d_empleo %>% plot_ly(labels = ~`Situación Laboral`, values = ~`Personas`, type = 'pie') %>% 
  layout(title = 'Situación Laboral')
m_empleo %>% plot_ly(x = ~`Situación Laboral`, y = ~`Porcentaje de Poblacion`, color= ~Departamento,colors = "Accent", barmode = 'group', type = 'bar')%>%
  layout(title = 'Situación Laboral por Municipio') 



d_lugar_estudio %>% filter(`Lugar de Estudio`== 'En otro país') %>% full_join(d_empleo, by = c('Departamento', 'Código')) %>% 
  plot_correlation()

m_lugar_nacimiento



m_lugar_nacimiento %>% arrange(`Porcentaje de la Poblacion`) %>% head(3)

d_lugar_nacimiento %>% filter(`Lugar de Nacimiento`=="En otro paÍs")%>% arrange(desc(`Porcentaje de la Poblacion`)) %>% head(3)


d_educacion %>% filter(`Lugar de Nacimiento`=="Nive")%>% arrange(desc(`Porcentaje de la Poblacion`)) %>% head(3)







