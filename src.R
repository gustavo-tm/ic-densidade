library(tidyverse)
library(sf)

setwd("C:/git/densidade")
load("C:/git/densidade/.RData")

# Import dados geosampa de distritos
distrito <- read_sf("dados/distrito/SIRGAS_SHP_distrito.shp") %>% st_set_crs("epsg:31983")
onibus <- read_sf("dados/onibus/SIRGAS_SHP_pontoonibus.shp") %>% st_set_crs("epsg:31983")
metro <- read_sf("dados/metro/SIRGAS_SHP_estacaometro_point.shp") %>% st_set_crs("epsg:31983")

# Geração da tabela IPTU.censo sem deixar arquivos temporários desnecessários
IPTU.censo <- local({
  
  # Tracking do tempo de rodar esse código, pode demorar cerca de 5 minutos
  hora_inicio <- Sys.time()
  cat("Processo iniciado")
  cat(str(Sys.time()))
  
  
  # Extração do zip file com lotes de cada bairro
  if (!"zip" %in% list.files(path = "dados/lotes")){
    for (file in list.files(path="dados/lotes/zip", full.names = FALSE) %>% 
         str_remove("\\.zip")){
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shp", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".dbf", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shx", sep = ""), exdir = "dados/lotes/unzip")
    }
  }
  
  print("Extração dos dados de lotes raw completa")
  cat(str(Sys.time()-hora_inicio))
  
  # Concatenação dos dados dos lotes em uma tabela
  lotes <- list.files(path="dados/lotes/unzip", full.names = FALSE) %>% 
    paste("dados/lotes/unzip/", ., "/", ., ".shp", sep = "") %>% 
    lapply(read_sf) %>% 
    bind_rows %>% 
    st_set_crs("epsg:31983") %>% 
    filter(lo_tp_lote == "F") %>% # Seleção apenas de lotes fiscais
    mutate(lo_lote = ifelse(lo_lote == "0000", paste("CD", lo_condomi, sep = ""), lo_lote)) %>% 
    select(setor = lo_setor, quadra = lo_quadra, lote = lo_lote)
  
  print("Dados lotes importados e tratados")
  cat(str(Sys.time()-hora_inicio))
  
  # Read dados do IPTU
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
    
    # Separação do número de contribuinte (SQL) em setor quadra e lote
    mutate(setor =  str_sub(sql, 1, 3),
           quadra = str_sub(sql, 4, 6),
           
           # Quando o lote é um condomínio, haverá vários SQLs no mesmo lote. CD = Condomínio
           lote =   str_sub(sql, 7, 10) %>% ifelse(condominio == "00-0", ., paste("CD", str_sub(condominio, 1, 2), sep = ""))) %>% 
    
    group_by(setor, quadra, lote) %>% 
    mutate(residencial = str_detect(tipo, "Residencial") %>% as.numeric(),
           
           # No caso de condomínios, calcula quando aquele IPTU representa o lote, com base na área construída
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
  cat(str(Sys.time()-hora_inicio))
  
  # Join dos lotes com IPTU com base no SQL
  IPTU.lotes <- IPTU %>% 
    left_join(lotes, by = join_by(setor, quadra, lote))
  
  print("Dados IPTU cruzados com lotes")
  cat(str(Sys.time()-hora_inicio))
  
  # Read dados do censo 2022
  censo <- read_sf("dados/censo/SP_Malha_Preliminar_2022.shp") %>% 
    filter(CD_MUN == "3550308") %>% 
    st_transform("epsg:31983") %>% # Sistema de coordenadas do geosampa
    select(id_setor_censitario = CD_SETOR, v0001:v0007) %>% 
    mutate(area_setor = st_area(geometry))
  
  print("Dados censo importados e tratados")
  cat(str(Sys.time()-hora_inicio))
  
  # Join dados do IPTU com do Censo através da intersecção das geometrias
  IPTU.censo <- censo %>% 
    st_intersection(IPTU.lotes %>% st_as_sf()) %>% 
    as_tibble() %>% 
    rename(geometria_intersec = geometry) %>% 
    
    # Retomada das geometrias do setor e lotes
    left_join(censo %>% as_tibble() %>% select(id_setor_censitario, geometria_setor_censitario = geometry),
              by = join_by(id_setor_censitario)) %>% 
    left_join(IPTU.lotes %>% as_tibble() %>% select(setor, quadra, lote, geometria_lote = geometry),
              by = join_by(setor, quadra, lote)) %>% 
    
    # Cálculo de quanto % do lote da dentro do setor
    mutate(percent_intersec = as.numeric(st_area(geometria_intersec) / st_area(geometria_lote)))
  
  print("Dados IPTU cruzados com dados do censo")
  cat(str(Sys.time()-hora_inicio))
  print("Processo concluído!")
  
  return(IPTU.censo)
})

gc()

# save.image("C:/git/densidade/.RData")


# Função para plotar um setor censitário ou SQL específico
view_geomtria <- function(geometria, zoom_out){
  
  bbox <- geometria %>% 
    st_transform("epsg:31983") %>% 
    st_bbox()
  
  x_min <- (bbox$xmin - zoom_out) %>% as.numeric()
  y_min <- (bbox$ymin - zoom_out) %>% as.numeric()
  x_max <- (bbox$xmax + zoom_out) %>% as.numeric()
  y_max <- (bbox$ymax + zoom_out) %>% as.numeric()
  
  corte <- list(xmin = x_min - zoom_out, ymin = y_min - zoom_out, xmax = x_max + zoom_out, ymax = y_max + zoom_out)
  
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

IPTU.censo %>% 
  glimpse()

# Exemplo de lote que é cortado por setor censitário
view_geomtria(IPTU.censo %>% 
                filter(id_setor_censitario == "355030810000143P") %>% 
                select(geometria_setor_censitario) %>% 
                st_as_sf(),
              zoom_out = 50)


# Casos em que a soma do percent_intersec não é 1 
IPTU.censo %>%
  group_by(setor, quadra, lote) %>% 
  mutate(soma_percent_intersec = sum(percent_intersec) %>% round(2)) %>% 
  filter(soma_percent_intersec < 1)

# Mapa de um dos casos (está na borda do mapa de SP)
view_geomtria(IPTU.censo %>% 
                filter(setor == "160", quadra == "128", lote == "0008") %>% 
                select(geometria_lote) %>% 
                st_as_sf(),
              zoom_out = 50)

# !!!!!!
# Área e estatísticas não estão sendo calculadas apenas para residenciais
# Em eixos, existe cotas para comércio e residencial ou mercado escolhe?

df <- IPTU.censo %>% 
  group_by(id_setor_censitario) %>% 
  summarize(populacao = mean(v0007),
            area_setor = mean(area_setor) %>% as.numeric(),
            unidades_total = sum(unidades * percent_intersec),
            unidades_residenciais = sum(unidades * percent_intersec * percent_residencial_unidades),
            domicilios = sum(v0002),
            domicilios_ocupados = sum(v0007),
            area_terreno_total = sum(area_terreno * percent_intersec),
            area_construida_total = sum(area_construida * percent_intersec),
            area_ocupada_total = sum(area_ocupada * percent_intersec),
            pavimentos = weighted.mean(pavimentos, area_terreno * percent_intersec),
            taxa_residencial = weighted.mean(percent_residencial_unidades)) %>% 
  mutate(densidade = populacao / area_setor,
         cota_parte = area_construida_total / unidades_total,
         CA = area_construida_total / area_terreno_total)

lm(densidade ~ (cota_parte + CA + pavimentos), data = df %>% filter(taxa_residencial > 0.1)) %>% summary()


df %>% 
  ggplot(aes(y = densidade, x = cota_parte, color = taxa_residencial)) +
  geom_point(alpha = .2) +
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = .5) + 
  scale_y_log10() +
  scale_x_log10() 

df %>% 
  ggplot(aes(y = densidade, x = CA, color = taxa_residencial)) +
  geom_point(alpha = .2) +
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = .5) + 
  scale_y_log10()

df %>% 
  ggplot(aes(y = densidade, x = pavimentos, color = taxa_residencial)) +
  geom_point(alpha = .2) +
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = .5) + 
  scale_y_log10() +
  scale_x_log10()


lote_metro <- IPTU.censo %>% 
  st_set_geometry("geometria_lote") %>% 
  mutate(centroide_lote = st_centroid(geometria_lote),
         metro = st_nearest_feature(centroide_lote, metro)) %>% 
  left_join(metro %>% 
              as_tibble() %>% 
              mutate(rn = row_number()) %>% 
              rename(geomeria_metro = geometry), by = join_by(metro== rn)) %>% 
  mutate(distancia_metro = st_distance(centroide_lote, geomeria_metro,  by_element=TRUE)) %>% 
  as_tibble()

IPTU.censo <- IPTU.censo %>% 
  left_join(lote_metro %>% 
              st_drop_geometry() %>% 
              select(setor, quadra, lote, distancia_metro),
            by = join_by(setor, quadra, lote))


ggplot() +
  geom_sf(data = IPTU.censo %>% st_set_geometry("geometria_lote") %>% st_crop(distrito %>% filter(ds_nome == "SE")),
          aes(geometry = geometria_lote)) +
  geom_sf(data = lote_metro %>% st_crop(distrito %>% filter(ds_nome == "SE")), alpha = .1, color = "darkblue") +
  geom_sf(data = metro %>% st_crop(distrito %>% filter(ds_nome == "SE")), color = "red") +
  theme_void()


