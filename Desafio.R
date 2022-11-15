# 1. Cargar paquetes

pacman::p_load(tidyverse,
               sjmisc,
               srvyr,
               survey,
               forcats,
               car,
               haven,
               dplyr, 
               car,
               sjmisc,
               sjlabelled,
               sjPlot)

# 2. Cargar Base 

Datos <- read_dta("input/data/Base de datos Full VI EME (extracto).dta")

# 3. Visualizar datos 

sjPlot::view_df(Datos)

# 4. Procesamiento de datos 

region_macrozona <- Datos %>%
  mutate(macrozona = case_when(region %in% c(15, 1, 2, 3) ~ "Macrozona Norte (cód 1)",
                                      region %in% c(4, 5, 6, 7, 8, 16) ~ "Macrozona Centro (cód 2)",
                                      region %in% c(9, 10, 14) ~ "Macrozona Sur (cód 3)",
                                      region %in% c(11, 12) ~ "Macrozona Austral (cód 4)",
                                      TRUE ~ NA_character_)) %>%


# 5. Objeto encuesta 
  
bbdd_ENE <- region_macrozona %>%
  as_survey_design(ids = Enc_rph, 
                   weights = Factor_EME)

# 6. Generar tabla 

bbdd_ENE2 <- bbdd_ENE %>%
  group_by(macrozona) %>%
  summarise(ganancia_prom = srvyr::survey_mean(ganancia_final_mensual, na.rm = T)) %>%
  ungroup()






         
           
           