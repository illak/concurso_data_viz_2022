library(tidyverse)
library(janitor)
library(fs)
library(sf)
library(geofacet)


data_personas <- dir_ls("data/personas/") %>% 
  map_dfr(read_csv2)


data_personas_sexo <- data_personas %>% 
  left_join(data_sexo)

data_sexo <- read_csv2("data/ref_sexo.csv")
data_disciplina <- read_csv2("data/ref_disciplina.csv")
data_conicet <- read_csv2("data/ref_categoria_conicet.csv")
data_clase_cargo <- read_csv2("data/ref_clase_cargo.csv")


data_org_map <- read_csv2("data/organizaciones_localizacion.csv")
data_provincias_map <- read_sf("https://infra.datos.gob.ar/catalog/modernizacion/dataset/7/distribution/7.12/download/provincias.geojson")


# chequeamos números globales

test <- data_personas_sexo %>% 
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
  group_by(anio, sexo_descripcion, gran_area_descripcion) %>% 
  count() %>% 
  pivot_wider(names_from = sexo_descripcion, values_from = n)



data_personas_sexo %>% 
  filter(seniority_level=="A") %>% 
  mutate(anio = factor(anio, levels = 2011:2019)) %>% 
  group_by(anio, sexo_descripcion) %>% 
  count() %>% 
  ggplot(aes(x = anio, y = n, color = sexo_descripcion, group = sexo_descripcion)) +
  geom_line() +
  geom_point()


data_personas_sexo %>% 
  mutate(anio = factor(anio, levels = 2011:2019)) %>% 
  left_join(data_clase_cargo, by = c("clase_cargo_docente_id"="clase_cargo_id")) %>% 
  filter(grupo_cargo_descripcion == "Cargos docentes de nivel superior universitario y/o posgrado") %>% 
  group_by(anio, sexo_descripcion) %>% 
  count() %>% 
  ggplot(aes(x = anio, y = n, color = sexo_descripcion, group = sexo_descripcion)) +
  geom_line() +
  geom_point()


test <- data_org_map %>% 
  filter(pais_id == 1) %>% 
  group_by(dpt_descripcion) %>% 
  count()

ggplot(test, aes(x = dpt_descripcion, y = n)) +
  geom_col() +
  facet_geo(~ dpt_descripcion, grid = "argentina_grid1")
