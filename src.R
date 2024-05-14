library(tidyverse)
library(sf)

IPTU <- read.csv("dados/IPTU/IPTU_2024.csv", sep=";", encoding = "latin1") %>% 
  as_tibble() %>% 
  select(sql = "NUMERO.DO.CONTRIBUINTE", 
         bairro = "BAIRRO.DO.IMOVEL",
         condominio = "NUMERO.DO.CONDOMINIO",
         area_terreno = "AREA.DO.TERRENO",
         area_construida = "AREA.CONSTRUIDA",
         area_ocupada = "AREA.OCUPADA",
         tipo = "TIPO.DE.PADRAO.DA.CONSTRUCAO") %>% 
  mutate(lo_setor =  str_sub(sql, 1, 3),
         lo_quadra = str_sub(sql, 4, 6),
         lo_lote =   str_sub(sql, 7, 10) %>% ifelse(condominio == "00-0", ., paste("CD", str_sub(condominio, 1, 2), sep = "")))

for (file in list.files(path="dados/lotes/zip", full.names = FALSE) %>% 
     str_remove("\\.zip")){
  
  unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shp", sep = ""), exdir = "dados/lotes/unzip")
  unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".dbf", sep = ""), exdir = "dados/lotes/unzip")
  unzip(paste("dados/lotes/zip/", file, ".zip", sep = ""), paste(file, "/", file, ".shx", sep = ""), exdir = "dados/lotes/unzip")
  
}

lotes <- list.files(path="dados/lotes/unzip", full.names = FALSE) %>% 
  paste("dados/lotes/unzip/", ., "/", ., ".shp", sep = "") %>% 
  lapply(read_sf) %>% 
  bind_rows %>% 
  st_set_crs("epsg:31983") %>% 
  mutate(lo_lote = ifelse(lo_lote == "0000", paste("CD", lo_condomi, sep = ""), lo_lote))

IPTU.lotes <- IPTU %>% 
  left_join(lotes)

censo <- read_sf("dados/censo/SP_Malha_Preliminar_2022.shp")

gg <- censo %>% 
  filter(CD_MUN == "3550308") %>% 
  mutate(densidade = v0001/AREA_KM2,
         decil = ntile(densidade, 10)) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(decil)), color = NA) +
  scale_fill_viridis_d() +
  labs(fill = "Decil de densidade") +
  theme_void()

ggsave("mapa.png", gg, width = 20, height = 20, dpi = 400)


