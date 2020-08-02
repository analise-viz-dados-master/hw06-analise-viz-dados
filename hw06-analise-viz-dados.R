library(tidyverse); source("utils.R")

df_obitos <- read_csv2("data-raw/obitos-confirmados-covid19-mg.csv")
df_pop <- read_csv2("data-raw/populacao-mg-2020.csv")

df_pop_agg <- df_pop %>% 
  mutate(FAIXA_ETARIA = case_when(
    FAIXA_ETARIA %in% c("0a4", "5a9") ~ "0 a 9 anos",
    FAIXA_ETARIA %in% c("10a14", "15a19") ~ "10 a 19 anos",
    FAIXA_ETARIA %in% c("20a24", "25a29") ~ "20 a 29 anos",
    FAIXA_ETARIA %in% c("30a34", "35a39") ~ "30 a 39 anos",
    FAIXA_ETARIA %in% c("40a44", "45a49") ~ "40 a 49 anos",
    FAIXA_ETARIA %in% c("50a54", "55a59") ~ "50 a 59 anos",
    FAIXA_ETARIA %in% c("60a64", "65a69") ~ "60 a 69 anos",
    FAIXA_ETARIA %in% c("70a74", "75a79") ~ "70 a 79 anos",
    FAIXA_ETARIA %in% c("80+") ~ "80+",
    TRUE ~ NA_character_
  )) %>% 
  mutate(FAIXA_ETARIA = as.factor(FAIXA_ETARIA)) %>% 
  mutate(SEXO = case_when(
    SEXO == "HOMENS" ~ "H",
    SEXO == "MULHERES" ~ "M",
    TRUE ~ NA_character_
  )) %>% 
  group_by(FAIXA_ETARIA) %>% 
  summarize(POPULACAO = sum(POPULACAO))


df_obitos_agg <- df_obitos %>% 
  mutate(FAIXA_ETARIA = calculate_age_range(IDADE)) %>% 
  count(FAIXA_ETARIA, name = "OBITOS_CONF")


left_join(df_pop_agg, df_obitos_agg, by = c("FAIXA_ETARIA")) %>% 
  mutate(TAXA_MORTALIDADE = (OBITOS_CONF / POPULACAO)*100000) %>% 
  ggplot(aes(x = FAIXA_ETARIA, y = TAXA_MORTALIDADE)) +
  geom_col() +
  coord_flip()
