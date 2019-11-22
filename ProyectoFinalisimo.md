Proyecto Final Censo
================
Rodolfo Rojas y Boris Juarez
11/21/2019

Cargar Librerias

``` r
library(readxl)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(stringr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ readr   1.3.1
    ## ✔ tibble  2.1.3     ✔ purrr   0.2.5
    ## ✔ tidyr   0.8.1     ✔ forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(DataExplorer)
```

    ## Warning: package 'DataExplorer' was built under R version 3.5.2

``` r
library(ggplot2)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
# Se borra el enviorement completo para que no haya problemas
rm(list = ls())
```

Para hacer este proyecto nos dimos cuenta con el EDA que la data era la
misma entre las tablas de departamento y las municipales. En este
markdown se mostrara unicamente como se formatearon las tablas
departamentales y se asume que las municipales se hicieron igual.

Abrir todos los archivos que existen en la carpeta data

``` r
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

##Se eliminan para que no interrumpan
rm(nombre, pre, archivos, temp_df)
```

# EDA

Como EDA, desde el inicio se coloca el departamento o el municipio como
factores pues es mas facil despues trabajarlos asi y se cambia el tipo
de datos de los codigos.

Con este codigo podemos ver como la estructura general de las tablas son
la misma excepto por la cantidad de filas entre municipales y
departamentales.

``` r
introduce(d_poblacion)
```

    ## # A tibble: 1 x 9
    ##    rows columns discrete_columns continuous_colu… all_missing_col…
    ##   <int>   <int>            <int>            <int>            <int>
    ## 1    22      52                1               51                0
    ## # … with 4 more variables: total_missing_values <int>, complete_rows <int>,
    ## #   total_observations <int>, memory_usage <dbl>

``` r
introduce(m_poblacion)
```

    ## # A tibble: 1 x 9
    ##    rows columns discrete_columns continuous_colu… all_missing_col…
    ##   <int>   <int>            <int>            <int>            <int>
    ## 1   340      52                1               51                0
    ## # … with 4 more variables: total_missing_values <int>, complete_rows <int>,
    ## #   total_observations <int>, memory_usage <dbl>

## Formatear tablas de Caracteristicas

Las tablas de caracteristicas se convirtieron en cuatro tablas: la de
lugar de nacimiento, residencia en 2013, dificultades y por ultimo
tambien la de hijos por mujeres fertiles.

``` r
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
```

hay mas pero no se muestran para no
saturar.

``` r
d_dificultades <- d_caracteristicas %>% gather(key = "Dificultades", value = "Cantidad de Personas", 14:31) %>% 
  select(1:2,13, Dificultades, `Cantidad de Personas`) %>% 
  mutate(`Porcentaje con Dificultad`= round(`Cantidad de Personas`/ `Población de 4 años o más` * 100 ,2))

d_hijos_x_mujeres <- d_caracteristicas %>% gather(key = 'Situación', value = 'Cantidad de Mujeres', 33:45) %>% 
  select(1:2, 32, `Situación`, `Cantidad de Mujeres`) %>% 
  mutate(`Porcentaje de Mujeres`= round(`Cantidad de Mujeres`/`Total de mujeres en edad fértil` ,2))

rm(d_caracteristicas)
```

## Formatear tablas de Educacion

La tabla de educacion se combirtio en cinco tablas, la de 1. educacion
que contiene el nivel de educacion, 2. causa de inasistencias, 3.
alfabetizacion, 4. asistencia y por ultimo 5. lugar de estudio

``` r
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
```

\#\#Formatear tablas de Empleo Se genera una sola tabla quye basicamente
es la misma pero cambiada de forma

``` r
# Departamental
d_empleo <- d_empleo %>% gather(key = 'Situación Laboral', value = 'Personas', 5:7) %>% arrange(`Código`) %>% 
  select(1:2, 4, `Situación Laboral`, `Personas`) %>% 
  mutate(`Porcentaje de Economicamente Activa`=round(`Personas`/`Población Económicamente Activa`*100, 2))
```

## Formatear tablas de Hogares

``` r
# Departamental
d_distribucion_hogares <- d_hogares %>% gather(key = 'Area', value = 'Distribución de Hogares', 3:4) %>% arrange(`Código`) %>% 
  select(1:2, `Area`, `Distribución de Hogares`) %>% 
  mutate(Area= str_remove(`Area`, "Distribución de hogares por área"))

d_hogares <- d_hogares %>% select(1,2,5,6)
```

## Formatear tablas de Poblacion

De la tabla de poblacion se generaron siete tablas nuevas siendo estas:
1. genero 2.Edad grupos de 15 años 3. Edad grupo de 5 años 4. Zona
demografica 5. relación con el Jefe del Hogar 6.Personas en Situación de
Calle y finalmente 7. Estado Civil

``` r
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
```

## Formatear tablas de pueblo

La tabla de pueblo se formatea y se generan dos nuevas tablas, una de
comunidad linguistica y la otra con el idioma materno, y la de pueblo
queda unicamente con los pueblos de pertenencia

``` r
# Departamental
d_comunidad_linguistica <- d_pueblo %>% 
  gather(key = 'Comunidad Linguistica', value = 'Cantidad', 10:31) %>% 
  arrange(`Código`) %>%
  select(1:3, `Comunidad Linguistica`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/ `Total de personas`*100, 2))

d_idioma_materno <- d_pueblo %>% 
  gather(key = 'Idioma Materno', value = 'Cantidad', 32:61) %>% arrange(`Código`) %>%
  select(1:3, `Idioma Materno`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/`Total de personas`*100, 2)) %>%
  mutate(`Idioma Materno` = str_remove(`Idioma Materno`, "__1"))

d_pueblo <- d_pueblo %>% gather(key = 'Pueblo de Pertenencia', value='Cantidad', 4:9) %>% arrange(`Código`) %>%
  select(1:3, `Pueblo de Pertenencia`, `Cantidad`) %>% 
  mutate(`Porcentaje de Total`= round(`Cantidad`/ `Total de personas`*100, 2))
```

## Formatear tablas de tecnologia

Esta tabla se formatea y se separa en dos tablas, una con información
individual y la otra con conglomerado

``` r
#### Departamental
d_tecnologia <- d_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
d_tecnologia_agrupado <- d_tecnologia %>% select(1:3, 13:16) 

d_tecnologia <-d_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))
```

``` r
#### Municipal
m_tecnologia <- m_tecnologia %>% rename('NA_Celular'=`No Declarado`, 'NA_Computadora'=`No Declarado__1`, 'NA_Internet'=`No Declarado__2`)
m_tecnologia_agrupado <- m_tecnologia %>% select(1:3, 13:16) 

m_tecnologia <-m_tecnologia %>% select(-(13:16)) %>%  
  gather(key= 'Uso de Tecnologias', value= 'Cantidad de Personas', `Usa Celular`:`NA_Internet`) %>% arrange(`Código`) %>% 
  mutate(`Porcentaje de Poblacion`= round(`Cantidad de Personas`/ `Población de 7 años o más`*100, 2))
```

## Formatear tablas de Vivienda

``` r
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
```

``` r
write.csv(d_alfabetizacion, file = 'Base de Datos/d_alfabetizacion.csv', row.names = FALSE)
write.csv(d_asistencia, file = 'Base de Datos/d_asistencia.csv', row.names = FALSE)
write.csv(d_causas_inasistencia, file = 'Base de Datos/d_causas_inasistencia.csv', row.names = FALSE)
write.csv(d_dificultades, file = 'Base de Datos/d_dificultades.csv', row.names = FALSE)
write.csv(d_distribucion_hogares, file = 'Base de Datos/d_distribucion_hogares.csv', row.names = FALSE)
write.csv(d_edad_15, file = 'Base de Datos/d_edad_15.csv', row.names = FALSE)
write.csv(d_edad_5, file = 'Base de Datos/d_edad_5.csv', row.names = FALSE)
write.csv(d_educacion, file = 'Base de Datos/d_educacion.csv', row.names = FALSE)
write.csv(d_empleo, file = 'Base de Datos/d_empleo.csv', row.names = FALSE)
write.csv(d_estado_civil, file = 'Base de Datos/d_estado_civil.csv', row.names = FALSE)
write.csv(d_genero, file = 'Base de Datos/d_genero.csv', row.names = FALSE)
write.csv(d_hijos_x_mujeres, file = 'Base de Datos/d_hijos_x_mujeres.csv', row.names = FALSE)
write.csv(d_hogares, file = 'Base de Datos/d_hogares.csv', row.names = FALSE)
write.csv(d_lugar_estudio, file = 'Base de Datos/d_lugar_estudio.csv', row.names = FALSE)
write.csv(d_lugar_nacimiento, file = 'Base de Datos/d_lugar_nacimiento.csv', row.names = FALSE)
write.csv(d_material_pared, file = 'Base de Datos/d_material_pared.csv', row.names = FALSE)
write.csv(d_material_piso, file = 'Base de Datos/d_material_piso.csv', row.names = FALSE)
write.csv(d_material_techo, file = 'Base de Datos/d_material_techo.csv', row.names = FALSE)
write.csv(d_recidencia_2013, file = 'Base de Datos/d_recidencia_2013.csv', row.names = FALSE)
write.csv(d_relacion_jefe, file = 'Base de Datos/d_relacion_jefe.csv', row.names = FALSE)
write.csv(d_situacion_calle, file = 'Base de Datos/d_situacion_calle.csv', row.names = FALSE)
write.csv(d_tecnologia, file = 'Base de Datos/d_tecnologia.csv', row.names = FALSE)
write.csv(d_tecnologia_agrupado, file = 'Base de Datos/d_tecnologia_agrupado', row.names = FALSE)
write.csv(d_tipo_ocupacion, file = 'Base de Datos/d_tipo_ocupacion.csv', row.names = FALSE)
write.csv(d_vivienda, file = 'Base de Datos/d_vivienda.csv', row.names = FALSE)
write.csv(d_zona, file = 'Base de Datos/d_zona.csv', row.names = FALSE)

write.csv(d_comunidad_linguistica, file = 'Base de Datos/d_comunidad_linguistica.csv', row.names = FALSE)
write.csv(d_pueblo, file = 'Base de Datos/d_pueblo.csv', row.names = FALSE)

write.csv(m_alfabetizacion, file = 'Base de Datos/m_alfabetizacion.csv', row.names = FALSE)
write.csv(m_asistencia, file = 'Base de Datos/m_asistencia.csv', row.names = FALSE)
write.csv(m_causas_inasistencia, file = 'Base de Datos/m_causas_inasistencia.csv', row.names = FALSE)
write.csv(m_dificultades, file = 'Base de Datos/m_dificultades.csv', row.names = FALSE)
write.csv(m_distribucion_hogares, file = 'Base de Datos/m_distribucion_hogares.csv', row.names = FALSE)
write.csv(m_edad_15, file = 'Base de Datos/m_edad_15.csv', row.names = FALSE)
write.csv(m_edad_5, file = 'Base de Datos/m_edad_5.csv', row.names = FALSE)
write.csv(m_educacion, file = 'Base de Datos/m_educacion.csv', row.names = FALSE)
write.csv(m_empleo, file = 'Base de Datos/m_empleo.csv', row.names = FALSE)
write.csv(m_estado_civil, file = 'Base de Datos/m_estado_civil.csv', row.names = FALSE)
write.csv(m_genero, file = 'Base de Datos/m_genero.csv', row.names = FALSE)
write.csv(m_hijos_x_mujeres, file = 'Base de Datos/m_hijos_x_mujeres.csv', row.names = FALSE)
write.csv(m_hogares, file = 'Base de Datos/m_hogares.csv', row.names = FALSE)
write.csv(m_lugar_estudio, file = 'Base de Datos/m_lugar_estudio.csv', row.names = FALSE)
write.csv(m_lugar_nacimiento, file = 'Base de Datos/m_lugar_nacimiento.csv', row.names = FALSE)
write.csv(m_material_pared, file = 'Base de Datos/m_material_pared.csv', row.names = FALSE)
write.csv(m_material_piso, file = 'Base de Datos/m_material_piso.csv', row.names = FALSE)
write.csv(m_material_techo, file = 'Base de Datos/m_material_techo.csv', row.names = FALSE)
write.csv(m_recidencia_2013, file = 'Base de Datos/m_recidencia_2013.csv', row.names = FALSE)
write.csv(m_relacion_jefe, file = 'Base de Datos/m_relacion_jefe.csv', row.names = FALSE)
write.csv(m_situacion_calle, file = 'Base de Datos/m_situacion_calle.csv', row.names = FALSE)
write.csv(m_tecnologia, file = 'Base de Datos/m_tecnologia.csv', row.names = FALSE)
write.csv(m_tecnologia_agrupado, file = 'Base de Datos/m_tecnologia_agrupado', row.names = FALSE)
write.csv(m_tipo_ocupacion, file = 'Base de Datos/m_tipo_ocupacion.csv', row.names = FALSE)
write.csv(m_vivienda, file = 'Base de Datos/m_vivienda.csv', row.names = FALSE)
write.csv(m_zona, file = 'Base de Datos/m_zona.csv', row.names = FALSE)
```

# Codigo Generador de Graficas

``` r
# d_edad_5 %>% plot_ly(x =~`Grupo de Edades 5 años`, y = ~`Porcentaje de Poblacion`, color= ~Departamento,colors = "Accent", barmode = 'group', type = 'bar')%>%
#   layout(title = 'Grupos de Edades') 
# 
# d_estado_civil %>% plot_ly(x =~`Estado Civil`, y = ~`Porcentaje de Poblacion`, color= ~Departamento,colors = "Accent", barmode = 'group', type = 'bar')%>%
#   layout(title = 'Estado Civil') 
# 
# d_estado_civil %>% plot_ly(labels =~`Estado Civil`, values = ~`Porcentaje de Poblacion` ,colors = "Accent", type = 'pie')%>%
#   layout(title = 'Estado Civil Pie') 
# 
# d_educacion %>% plot_ly(labels = ~`Nivel Educativo`, values = ~`Cantidad de Personas`, type = 'pie') %>% 
#   layout(title = 'Personas por Nivel Educativo')
# 
# d_empleo %>% plot_ly(labels = ~`Situación Laboral`, values = ~`Personas`, type = 'pie') %>% 
#   layout(title = 'Situación Laboral')
# m_empleo %>% plot_ly(x = ~`Situación Laboral`, y = ~`Porcentaje de Economicamente Activa`, color= ~Municipio,colors = "Accent", barmode = 'group', type = 'bar')%>%
#   layout(title = 'Situación Laboral por Municipio') 

# m_lugar_nacimiento %>%
# plot_ly(x =~`Lugar de Nacimiento`, y = ~`Porcentaje de la Poblacion`, color= ~Municipio,colors = "Accent", barmode = 'group', type = 'bar')%>%
  # layout(title = 'Lugar de Nac') 
```

# Rankings utiles

``` r
m_lugar_nacimiento %>% arrange(`Porcentaje de la Poblacion`) %>% head(3)
```

    ## # A tibble: 3 x 6
    ##   Código Municipio  `Total de person… `Lugar de Nacim… Personas `Porcentaje de …
    ##    <int> <ord>                  <dbl> <chr>               <dbl>            <dbl>
    ## 1   1405 Chajul                 46658 En otro paÍs            1             0   
    ## 2    705 Nahualá                75430 En otro paÍs           11             0.01
    ## 3    807 Santa Luc…             22378 En otro paÍs            2             0.01

``` r
d_lugar_nacimiento %>% filter(`Lugar de Nacimiento`=="En otro paÍs")%>% arrange(desc(`Porcentaje de la Poblacion`)) %>% head(3)
```

    ## # A tibble: 3 x 6
    ##   Código Departamento `Total de perso… `Lugar de Nacim… Personas
    ##    <int> <ord>                   <dbl> <chr>               <dbl>
    ## 1      1 Guatemala             3015081 En otro paÍs        34216
    ## 2     20 Chiquimula             415063 En otro paÍs         3798
    ## 3     22 Jutiapa                488395 En otro paÍs         4386
    ## # … with 1 more variable: `Porcentaje de la Poblacion` <dbl>

``` r
m_lugar_nacimiento %>% filter(`Lugar de Nacimiento`=="En otro paÍs")%>% 
  arrange(desc(`Porcentaje de la Poblacion`)) %>% head(5)
```

    ## # A tibble: 5 x 6
    ##   Código Municipio  `Total de person… `Lugar de Nacim… Personas `Porcentaje de …
    ##    <int> <ord>                  <dbl> <chr>               <dbl>            <dbl>
    ## 1   2208 Jerez                   6309 En otro paÍs          301             4.77
    ## 2   2007 Esquipulas             53556 En otro paÍs         2189             4.09
    ## 3   1331 Santa Ana…              9413 En otro paÍs          339             3.6 
    ## 4    113 Fraijanes              58922 En otro paÍs         1623             2.75
    ## 5   2207 Atescatem…             18402 En otro paÍs          457             2.48

Intento de crrelacion

``` r
# rm(list = ls())

# archivos <- list.files("Data")
# for (nombre in archivos) {
#   temp_df <- read_excel(path =  paste("Data", nombre, sep = "/"), skip = 9) %>% select(-X__1)
#   temp_df <- temp_df %>% filter(complete.cases(temp_df))
#   temp_df[1] <- as.integer(pull(temp_df, 1))
#   temp_df[2] <- factor(pull(temp_df,2), ordered= TRUE)
#   
#   pre<- ifelse(str_detect(nombre, "municipal"), "m_", "d_") 
#   nombre <- str_remove(nombre, "(_municipal|_departamental)\\.xlsx$")
#   nombre <- paste(pre,nombre, sep = "")
#   assign(nombre, temp_df)
# }


# d_empleo %>% full_join(d_tecnologia) %>%
#   select(`Población ocupada`,Cesante, Aspirante, `No declarado`, `Usa Celular`, `Usa Computadora`, `Usa Internet`) %>% 
#   cor %>% corrplot()
```

Estas correlaciones no son muy utiles pues ademas de ser una falacia en
la interpretacion resulta que en datos demograficos Guatemala es muy
parecida en forma general
