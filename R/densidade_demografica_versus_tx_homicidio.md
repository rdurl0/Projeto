Densidade demográfica
================
Raul de Sá Durlo

## Densidade demográfica

``` r
# pacotes --------------------

library(tidyverse)
library(gganimate)
library(spcrimr)
library(sf)

# carrega dados --------------

options(gganimate.dev_args = list(width = 1000, height = 50000))

  # prepara os dados
seade %>%
  select(chave:nome_localidade_pai, populacao) %>%
  inner_join(sf_sampa, .,
             by = c("chave", "municipio")) %>%
  inner_join(., select(ssp_ocorrencias_crimes, chave:ano, homicidio_doloso),
             by = c("municipio", "chave", "ano")) %>%
  
  # transforma variáveis
  mutate(area_km2 = round(unclass(st_area(.))[1:nrow(seade)] / 1000000),
         dens_demogr = populacao / area_km2,
         ano = as.integer(ano)) %>%
  select(nm_municip,
         ano,
         area_km2,
         dens_demogr,
         everything()) %>%
  st_set_geometry(NULL) %>%
  mutate(taxa_hom = homicidio_doloso / populacao * 100000) %>%
  
  # começa o ggplot ----------------------------------------#
  ggplot(aes(y = taxa_hom,
             x = dens_demogr,
             size = populacao,
             colour = nome_localidade_pai)) +
  # monta o ponto ------------------------------------------#
  geom_point(alpha = 0.7,
             show.legend = FALSE) +
  # separa por regiões -------------------------------------#
  facet_wrap(~ nome_localidade_pai, dir = "v", nrow = NULL, ncol = 2L) +
  # escala -------------------------------------------------#
  scale_x_log10() +
  # texto --------------------------------------------------#
  labs(title = "Taxa de homicídio e densidade demográfica - Ano: {frame_time}",
       x = "Densidade demográfica (hab/Km2)",
       y = "Taxa de Homicídio") +
  # gganimate ----------------------------------------------#
  transition_time(ano) +
  ease_aes("linear")
```

![](densidade_demografica_versus_tx_homicidio_files/figure-gfm/unnamed-chunk-1-1.gif)<!-- -->

``` r
  # fim ggplot ---------------------------------------------#


#TODO:
##Controlar o tamanho das cidades (tamanho dos círculos)
```
