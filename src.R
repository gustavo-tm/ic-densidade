library(tidyverse)
library(sf)

setwd("C:/git/densidade")

distrito <- read_sf("dados/distrito/SIRGAS_SHP_distrito.shp") %>% st_set_crs("epsg:31983")

IPTU.censo <- local({
  
  if (!"zip" %in% list.files(path = "dados/lotes")){
    for (file in list.files(path="dados/lotes/zip", full.names = FALSE) %>% 
         str_remove("\\.zip")){
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shp", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".dbf", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shx", sep = ""), exdir = "dados/lotes/unzip")
    }
  }
  
  print("Extração dos dados de lotes raw completa")
  
  lotes <- list.files(path="dados/lotes/unzip", full.names = FALSE) %>% 
    paste("dados/lotes/unzip/", ., "/", ., ".shp", sep = "") %>% 
    lapply(read_sf) %>% 
    bind_rows %>% 
    st_set_crs("epsg:31983") %>% 
    filter(lo_tp_lote == "F") %>% # Seleção apenas de lotes fiscais
    mutate(lo_lote = ifelse(lo_lote == "0000", paste("CD", lo_condomi, sep = ""), lo_lote)) %>% 
    select(setor = lo_setor, quadra = lo_quadra, lote = lo_lote)
  
  print("Dados lotes importados e tratados")
  
  IPTU <- read.csv("dados/IPTU/IPTU_2024.csv", sep=";", encoding = "latin1") %>% 
    as_tibble() %>% 
    select(sql = "NUMERO.DO.CONTRIBUINTE", 
           bairro = "BAIRRO.DO.IMOVEL",
           condominio = "NUMERO.DO.CONDOMINIO",
           area_terreno = "AREA.DO.TERRENO",
           area_construida = "AREA.CONSTRUIDA",
           area_ocupada = "AREA.OCUPADA",
           pavimentos = "QUANTIDADE.DE.PAVIMENTOS",
           ano_construcao = "ANO.DA.CONSTRUCAO.CORRIGIDO",
           tipo = "TIPO.DE.PADRAO.DA.CONSTRUCAO") %>% 
    mutate(setor =  str_sub(sql, 1, 3),
           quadra = str_sub(sql, 4, 6),
           lote =   str_sub(sql, 7, 10) %>% ifelse(condominio == "00-0", ., paste("CD", str_sub(condominio, 1, 2), sep = ""))) %>% 
    group_by(setor, quadra, lote) %>% 
    mutate(residencial = str_detect(tipo, "Residencial") %>% as.numeric(),
           percent_area = (area_construida / sum(area_construida))) %>% 
    group_by(setor, quadra, lote) %>% 
    summarize(unidades = n(),
              area_terreno = mean(area_terreno), 
              area_construida = sum(area_construida), 
              area_ocupada = mean(area_ocupada),
              pavimentos = mean(pavimentos),
              ano_construcao = mean(ano_construcao),
              percent_residencial_unidades = mean(residencial),
              percent_residencial_ponderado = sum(residencial * percent_area))
  
  print("Dados IPTU importados e tratados")
  
  IPTU.lotes <- IPTU %>% 
    left_join(lotes)
  
  print("Dados IPTU cruzados com lotes")
  
  censo <- read_sf("dados/censo/SP_Malha_Preliminar_2022.shp") %>% 
    filter(CD_MUN == "3550308") %>% 
    st_transform("epsg:31983") %>% 
    select(id_setor_censitario = CD_SETOR, v0001:v0007) %>% 
    mutate(area_setor = st_area(geometry))
  
  print("Dados censo importados e tratados")
  
  IPTU.censo <- censo %>% 
    st_intersection(IPTU.lotes %>% st_as_sf()) %>% 
    as_tibble() %>% 
    rename(geometria_intersec = geometry) %>% 
    left_join(censo %>% as_tibble() %>% select(id_setor_censitario, geometria_setor_censitario = geometry)) %>% 
    left_join(IPTU.lotes %>% as_tibble() %>% select(setor, quadra, lote, geometria_lote = geometry)) %>% 
    mutate(percent_intersec = as.numeric(st_area(geometria_intersec) / st_area(geometria_lote)))
  
  print("Dados IPTU cruzados com dados do censo")
  print("Processo concluído!")
  
})

# save.image("C:/git/densidade/.RData")

IPTU.censo %>% 
  group_by(setor, quadra, lote) %>% 
  summarize(soma_percent_intersec = sum(percent_intersec)) %>% 
  View()

view_geomtria <- function(geometria, zoom_out){
  
  bbox <- geometria %>% 
    st_transform("epsg:31983") %>% 
    st_bbox()
  
  x_min <- (bbox$xmin - zoom_out) %>% as.numeric()
  y_min <- (bbox$ymin - zoom_out) %>% as.numeric()
  x_max <- (bbox$xmax + zoom_out) %>% as.numeric()
  y_max <- (bbox$ymax + zoom_out) %>% as.numeric()
  
  corte <- list(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max)

  IPTU.censo %>%
    st_as_sf() %>%
    st_transform("epsg:31983") %>% 
    st_crop(corte %>% unlist()) %>% 
    mutate(densidade = v0001/area_setor,
           decil = ntile(densidade, 10),
           percent_intersec = round(percent_intersec * 100)) %>% 
    ggplot() +
    geom_sf(aes(geometry = geometria_setor_censitario, fill = factor(decil)), color = "white", lwd = 1.5) +
    geom_sf(aes(geometry = geometria_intersec), fill = "red", color = "red", alpha = .25) +
    geom_sf_label(aes(geometry = geometria_intersec, label = percent_intersec), label.size = .01) +
    scale_fill_viridis_d() +
    labs(fill = "Decil de densidade") +
    theme(legend.position = "none") +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_void()
}

view_geomtria(IPTU.censo %>% 
                filter(id_setor_censitario == "355030810000143P") %>% 
                select(geometria_setor_censitario) %>% 
                st_as_sf(),
              zoom_out = 50)

IPTU.censo %>% filter(id_setor_censitario == "355030810000143P") %>% select(geometria_setor_censitario) %>% st_as_sf() %>% st_bbox()





IPTU.censo %>% 
  head(10000) %>% 
  mutate(unidades_original = unidades, 
         unidades_residenciais = unidades * percent_residencial_unidades,
         unidades_setor = unidades * percent_intersec,
         unidades_residenciais_setor = unidades * percent_residencial_unidades * percent_intersec,
         erro = unidades + 1< unidades_residenciais_setor) %>% 
  group_by(id_setor_censitario) %>% 
  summarize(unidades = sum(unidades),
            unidades_residenciais = sum(unidades_residenciais),
            unidades_setor = sum(unidades_setor),
            unidades_residenciais_setor = sum(unidades_residenciais_setor)) %>% 
  mutate(erro = unidades + 1 < unidades_residenciais_setor) %>% 
  View()



IPTU.censo %>% 
  arrange(id_setor_censitario) %>% 
  head(10000) %>% 
  group_by(id_setor_censitario) %>% 
  summarize(n = n(),
            unidades = sum(unidades * percent_intersec)) %>% View()



IPTU.censo %>% 
  filter(id_setor_censitario == "355030810000143P") %>% View()
summarize(percent_intersec = sum(percent_intersec))
View()


IPTU.censo %>% 
  glimpse()

df <- IPTU.censo %>% 
  group_by(id_setor_censitario) %>% 
  summarize(populacao = nth(v0007, 1),
            area = mean(area_setor) %>% as.numeric(),
            unidades_IPTU = sum(unidades * percent_intersec),
            domicilios = sum(v0002),
            domicilios_ocupados = sum(v0007),
            area_terreno = sum(area_terreno * percent_intersec),
            area_construida = sum(area_construida * percent_intersec),
            area_ocupada = sum(area_ocupada * percent_intersec),
            pavimentos = mean(pavimentos * percent_intersec)) %>% 
  mutate(densidade = populacao / area_terreno,
         cota_parte = area_construida / unidades_IPTU,
         CA = area_construida / area_terreno)

lm(densidade ~ cota_parte + CA + pavimentos, data = df) %>% summary()


df %>% 
  ggplot(aes(y = densidade, x = cota_parte)) +
  geom_point(alpha = .2) +
  scale_y_log10() +
  scale_x_log10()

df %>% 
  ggplot(aes(y = densidade, x = CA)) +
  geom_point(alpha = .2) +
  scale_y_log10()

df %>% 
  ggplot(aes(y = densidade, x = pavimentos)) +
  geom_point(alpha = .2) +
  scale_y_log10() +
  scale_x_log10()

a <- 100
limites <- list(xmin = 334654.5 - a, ymin = 7395000 - a, xmax = 334763.9 + a, ymax = 7395071 + a) 

a <- 50
limites_plot <- list(xmin = 334654.5 - a, ymin = 7395000 - a, xmax = 334763.9 + a, ymax = 7395071 + a) 

gg <- IPTU.censo %>%
  st_as_sf() %>% 
  # st_crop(distrito %>% filter(ds_nome == "SE")) %>% 
  #IPTU.censo %>% filter(id_setor_censitario == "355030810000143P") %>% select(geometria_setor_censitario) %>% st_as_sf()
  st_crop(limites %>% unlist()) %>% 
  mutate(densidade = v0001/area_setor,
         decil = ntile(densidade, 10),
         percent_intersec = round(percent_intersec * 100)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometria_setor_censitario, fill = factor(decil)), color = "white", lwd = 1.5) +
  geom_sf(aes(geometry = geometria_intersec), fill = "red", color = "red", alpha = .25) +
  geom_sf_label(aes(geometry = geometria_intersec, label = percent_intersec), label.size = .01) +
  scale_fill_viridis_d() +
  labs(fill = "Decil de densidade") +
  theme(legend.position = "none") +
  coord_sf(xlim = c(limites_plot$xmin, limites_plot$xmax), ylim = c(limites_plot$ymin, limites_plot$ymax)) +
  theme_void()

a <- 100
limites <- list(xmin = 334654.5 - a, ymin = 7395000 - a, xmax = 334763.9 + a, ymax = 7395071 + a) 

a <- 50
limites_plot <- list(xmin = 334654.5 - a, ymin = 7395000 - a, xmax = 334763.9 + a, ymax = 7395071 + a) 
gg <- IPTU.censo %>%
  st_as_sf() %>% 
  # st_crop(distrito %>% filter(ds_nome == "SE")) %>% 
  #IPTU.censo %>% filter(id_setor_censitario == "355030810000143P") %>% select(geometria_setor_censitario) %>% st_as_sf()
  st_crop(limites %>% unlist()) %>% 
  mutate(densidade = v0001/area_setor,
         decil = ntile(densidade, 10),
         percent_intersec = round(percent_intersec * 100)) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometria_setor_censitario, fill = factor(decil)), color = "white", lwd = 1.5) +
  geom_sf(aes(geometry = geometria_intersec), fill = "red", color = "red", alpha = .25) +
  geom_sf_label(aes(geometry = geometria_intersec, label = percent_intersec), label.size = .01) +
  scale_fill_viridis_d() +
  labs(fill = "Decil de densidade") +
  theme(legend.position = "none") +
  coord_sf(xlim = c(limites_plot$xmin, limites_plot$xmax), ylim = c(limites_plot$ymin, limites_plot$ymax)) +
  theme_void()

# ggsave("tex/imagens/mapa-lotes.png", gg, width = 70, height = 70, dpi = 150, limitsize = FALSE)
ggsave("teste.png", gg, width = 20, height = 20, dpi = 150, limitsize = FALSE)




