library(tidyverse)
library(janitor)
library(fs)
library(sf)
library(geofacet)
library(stringr)


data_personas <- dir_ls("data/personas/") %>% 
  map_dfr(read_csv2)


data_personas_sexo <- data_personas %>% 
  left_join(data_sexo)

data_sexo <- read_csv2("data/ref_sexo.csv")
data_disciplina <- read_csv2("data/ref_disciplina.csv")
data_conicet <- read_csv2("data/ref_categoria_conicet.csv")
data_clase_cargo <- read_csv2("data/ref_clase_cargo.csv")
data_tipo_personal <- read_csv2("data/ref_tipo_personal.csv")


data_org_map <- read_csv2("data/organizaciones_localizacion.csv")
data_org <- read_csv2("data/organizaciones.csv")
data_org_act <- read_csv2("data/organizaciones_actividad_economica.csv")
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



# Mapa distribución por genero/sexo

personas_2019 <- read_csv2("data/personas/personas_2019.csv")

provincias_data <- data_org_map %>% 
  filter(pais_id == 1) %>% 
  mutate(name_es =  str_replace_all(dpt_descripcion," ",".")) %>% 
  mutate(name_es = if_else(name_es=="Capital.Federal","C.A.B.A.",name_es)) %>% 
  mutate(dpt_descripcion = if_else(dpt_descripcion=="Capital Federal",
                                   "C.A.B.A.",dpt_descripcion)) %>% 
  select(name_dpt_descripcion = dpt_descripcion, name_es) %>% 
  unique()

# modificamos datos de la grilla
argentina_grid3 <- argentina_grid2 %>% 
  left_join(provincias_data) %>% 
  mutate(name_dpt_descripcion = str_wrap(name_dpt_descripcion, width = 10))


personas_2019 %>% 
  left_join(data_sexo) %>% 
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
  left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>% 
  filter(pais_id == 1) %>% 
  mutate(label =  str_replace_all(dpt_descripcion," ",".")) %>% 
  mutate(label = if_else(label=="Capital.Federal","C.A.B.A.",label)) %>% 
  mutate(sexo_descripcion = if_else(sexo_descripcion=="MASCULINO","M","F")) %>% 
  group_by(label, dpt_descripcion, sexo_descripcion) %>% 
  summarise(n = n()) %>%  
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(x = sexo_descripcion, y = pct)) +
  geom_col(aes(fill = sexo_descripcion)) +
  guides(y = "none", fill = "none") +
  labs(x = NULL, y = NULL) +
  facet_geo(~ label, grid = argentina_grid3, label = "name_dpt_descripcion") +
  theme(
    axis.text.x = element_blank()
  )
  

# Disciplinas tecnologias, ingenierias

(personas_tecno_infor <- personas_2019 %>% 
  left_join(data_sexo) %>% 
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
  left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>% 
  filter(pais_id == 1) %>% 
  filter(gran_area_descripcion == "INGENIERÍAS Y TECNOLOGÍAS" | area_descripcion == "Ciencias de la Computación e Información") %>% 
  mutate(label =  str_replace_all(dpt_descripcion," ",".")) %>% 
  mutate(label = if_else(label=="Capital.Federal","C.A.B.A.",label)) %>% 
  mutate(sexo_descripcion = if_else(sexo_descripcion=="MASCULINO","M","F")) %>% 
  group_by(label, dpt_descripcion, sexo_descripcion) %>% 
  summarise(n = n()) %>%  
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(y = pct, x = "A")) +
  geom_col(aes(fill = sexo_descripcion)) +
  coord_polar("y", start=0) +
  guides(y = "none", fill = "none") +
  labs(x = NULL, y = NULL) +
  facet_geo(~ label, grid = argentina_grid3, label = "name_dpt_descripcion") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_rect(fill = "grey50"),
    strip.text = element_text(color = "white", margin = margin(1.5,0,1.5,0), size = 8),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ))


(crecimiento_tecnos <- data_personas %>% 
  left_join(data_sexo) %>% 
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
  left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>% 
  filter(pais_id == 1) %>%  
  filter(gran_area_descripcion == "INGENIERÍAS Y TECNOLOGÍAS" | area_descripcion == "Ciencias de la Computación e Información") %>% 
  mutate(anio = factor(anio)) %>% 
  mutate(label =  str_replace_all(dpt_descripcion," ",".")) %>% 
  mutate(label = if_else(label=="Capital.Federal","C.A.B.A.",label)) %>% 
  mutate(sexo_descripcion = if_else(sexo_descripcion=="MASCULINO","M","F")) %>% 
  group_by(label, dpt_descripcion, sexo_descripcion, anio) %>% 
  summarise(n = sum(producciones_ult_anio, na.rm = T)) %>% 
  # GGPLOT
  ggplot(aes(y = n, x = anio)) +
  geom_line(aes(color = sexo_descripcion, group = sexo_descripcion)) +
  guides(y = "none", fill = "none") +
  labs(x = NULL, y = NULL) +
  facet_geo(~ label, grid = argentina_grid3, label = "name_dpt_descripcion") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_rect(fill = "grey50"),
    strip.text = element_text(color = "white", margin = margin(1.5,0,1.5,0), size = 8),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ))



(crecimiento_cargos_docente_sup <- data_personas %>% 
    left_join(data_sexo) %>% 
    left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
    left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>% 
    left_join(data_clase_cargo, by = c("clase_cargo_docente_id"="clase_cargo_id")) %>% 
    filter(tipo_personal_id==4) %>% 
    filter(pais_id == 1) %>%  
    #filter(gran_area_descripcion == "INGENIERÍAS Y TECNOLOGÍAS" | area_descripcion == "Ciencias de la Computación e Información") %>% 
    mutate(anio = factor(anio)) %>% 
    mutate(label =  str_replace_all(dpt_descripcion," ",".")) %>% 
    mutate(label = if_else(label=="Capital.Federal","C.A.B.A.",label)) %>% 
    mutate(sexo_descripcion = if_else(sexo_descripcion=="MASCULINO","M","F")) %>% 
    group_by(label, dpt_descripcion, sexo_descripcion, anio) %>% 
    summarise(n = n()) %>% 
    # GGPLOT
    ggplot(aes(y = n, x = anio)) +
    geom_line(aes(color = sexo_descripcion, group = sexo_descripcion)) +
    guides(y = "none", fill = "none") +
    labs(x = NULL, y = NULL) +
    facet_geo(~ label, grid = argentina_grid3, label = "name_dpt_descripcion") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_rect(fill = "grey50"),
      strip.text = element_text(color = "white", margin = margin(1.5,0,1.5,0), size = 8),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)
    ))

ggsave(filename = "output.png", plot = crecimiento_cargos_docente_sup, width = 5, height = 10, dpi = 320)



persona_disciplina <- personas_2019 %>% 
  left_join(data_sexo) %>% 
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>% 
  left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>% 
  left_join(data_tipo_personal) %>% 
  filter(pais_id == 1) %>% 
  filter(gran_area_descripcion == "INGENIERÍAS Y TECNOLOGÍAS" | area_descripcion == "Ciencias de la Computación e Información") %>% 
  group_by(area_descripcion, sexo_descripcion) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n / sum(n),
         pct = if_else(pct > .6, pct, NULL))
              
persona_disciplina_totales <- personas_2019 %>%
  left_join(data_sexo) %>%
  left_join(data_disciplina, by = c("disciplina_experticia_id"="disciplina_id")) %>%
  left_join(data_org_map, by = c("institucion_trabajo_id"="organizacion_id")) %>%
  left_join(data_tipo_personal) %>%
  filter(pais_id == 1) %>%
  filter(gran_area_descripcion == "INGENIERÍAS Y TECNOLOGÍAS" | area_descripcion == "Ciencias de la Computación e Información") %>%
  group_by(area_descripcion) %>%
  summarise(n = n())

(personas_disciplina <- ggplot(persona_disciplina, aes(y = fct_reorder(area_descripcion, n), x = n)) +
  geom_col(aes(fill = sexo_descripcion)) +
  geom_text(data = persona_disciplina_totales, mapping = aes(label = n),
            hjust = -.1) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_x_continuous(limits = c(0, 2300)) +
  guides(x = "none", fill = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    #axis.text.y = element_text(hjust = .5)
  ))


personas_tecno_infor
personas_disciplina
