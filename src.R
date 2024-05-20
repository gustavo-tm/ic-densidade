library(tidyverse)
library(sf)


distrito <- read_sf("dados/distrito/SIRGAS_SHP_distrito.shp") %>% st_set_crs("epsg:31983")

IPTU.lotes <- local({
  
  if (!"zip" %in% list.files(path = "dados/lotes")){
    for (file in list.files(path="dados/lotes/zip", full.names = FALSE) %>% 
         str_remove("\\.zip")){
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shp", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".dbf", sep = ""), exdir = "dados/lotes/unzip")
      unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shx", sep = ""), exdir = "dados/lotes/unzip")
    }
  }
  
  
  lotes <- list.files(path="dados/lotes/unzip", full.names = FALSE) %>% 
    paste("dados/lotes/unzip/", ., "/", ., ".shp", sep = "") %>% 
    lapply(read_sf) %>% 
    bind_rows %>% 
    st_set_crs("epsg:31983") %>% 
    filter(lo_tp_lote == "F") %>% # Seleção apenas de lotes fiscais
    mutate(lo_lote = ifelse(lo_lote == "0000", paste("CD", lo_condomi, sep = ""), lo_lote)) %>% 
    select(setor = lo_setor, quadra = lo_quadra, lote = lo_lote)
  
  IPTU <- read.csv("dados/IPTU/IPTU_2024.csv", sep=";", encoding = "latin1") %>% 
    as_tibble() %>% 
    select(sql = "NUMERO.DO.CONTRIBUINTE", 
           bairro = "BAIRRO.DO.IMOVEL",
           condominio = "NUMERO.DO.CONDOMINIO",
           area_terreno = "AREA.DO.TERRENO",
           area_construida = "AREA.CONSTRUIDA",
           area_ocupada = "AREA.OCUPADA",
           tipo = "TIPO.DE.PADRAO.DA.CONSTRUCAO") %>% 
    mutate(setor =  str_sub(sql, 1, 3),
           quadra = str_sub(sql, 4, 6),
           lote =   str_sub(sql, 7, 10) %>% ifelse(condominio == "00-0", ., paste("CD", str_sub(condominio, 1, 2), sep = ""))) %>% 
    group_by(setor, quadra, lote) %>% 
    summarize(unidades = n(), 
              area_terreno = max(area_terreno), 
              area_construida = sum(area_construida), 
              area_ocupada = max(area_ocupada))
  
  
  # # VERIFICAR PROBLEMA !!!!
  # read.csv("dados/IPTU/IPTU_2024.csv", sep=";", encoding = "latin1") %>% 
  #   as_tibble() %>% 
  #   select(sql = "NUMERO.DO.CONTRIBUINTE", 
  #          bairro = "BAIRRO.DO.IMOVEL",
  #          condominio = "NUMERO.DO.CONDOMINIO",
  #          area_terreno = "AREA.DO.TERRENO",
  #          area_construida = "AREA.CONSTRUIDA",
  #          area_ocupada = "AREA.OCUPADA",
  #          tipo = "TIPO.DE.PADRAO.DA.CONSTRUCAO") %>% 
  #   mutate(setor =  str_sub(sql, 1, 3),
  #          quadra = str_sub(sql, 4, 6),
  #          lote =   str_sub(sql, 7, 10) %>% ifelse(condominio == "00-0", ., paste("CD", str_sub(condominio, 1, 2), sep = ""))) %>% 
  #   group_by(setor, quadra, lote) %>% 
  #   summarize(unidades = n(), 
  #             area_terreno = sd(area_terreno), 
  #             area_construida = sum(area_construida), 
  #             area_ocupada = sd(area_ocupada))
  
  IPTU.lotes <- IPTU %>% 
    left_join(lotes)

  return(IPTU.lotes)
  
})


censo <- read_sf("dados/censo/SP_Malha_Preliminar_2022.shp") %>% 
  filter(CD_MUN == "3550308") %>% 
  st_transform("epsg:31983") %>% 
  select(id_setor_censitario = CD_SETOR, v0001:v0007) %>% 
  mutate(area_setor = st_area(geometry))


IPTU.censo <- censo %>% 
  st_intersection(IPTU.lotes %>% st_as_sf()) %>% 
  as_tibble() %>% 
  rename(geometria_intersec = geometry) %>% 
  left_join(censo %>% as_tibble() %>% select(id_setor_censitario, geometria_setor_censitario = geometry)) %>% 
  left_join(IPTU.lotes %>% as_tibble() %>% select(setor, quadra, lote, geometria_lote = geometry)) %>% 
  mutate(percent_intersec = as.numeric(st_area(geometria_intersec) / st_area(geometria_lote)))




