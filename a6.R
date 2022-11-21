# Pacotes necessários

library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(readxl) # Leitura de arquivos do Excel
library(tidyr) # Pivotagem dos dados
library(janitor) # Padronização dos nomes das variáveis
library(stringr) # Manipular strings

# Entrando com os Anexos, manipulando e juntado os dados.

# Nomes dos arquivos Anexo
data_names <- list.files(pattern = "Anexo A")

# Outros indicadores de interna,cão ---------------------------------------

ind_inter_outros <- data.frame()
ind_parto <- data.frame()
ind_nascimento <- data.frame()

for (i in 1:length(data_names)) {
  
  data_all <- read_excel(data_names[i], sheet = 6, skip = 3, col_names = FALSE)
  
  # Outros indicadores de internação
  oii <- data_all %>% select(1,7) %>% slice(1:4) %>% rename(Tipo = `...1`, Valor = `...7`) %>% 
    mutate(Tipo = substring(Tipo, regexpr("N", Tipo)))
  oii2 <- data_all %>% select(10,17) %>% slice(1:3) %>% rename(Tipo = `...10`, Valor = `...17`) %>% 
    mutate(Tipo = substring(Tipo, regexpr("N", Tipo)))
  
  oii_all <- rbind(oii,oii2)
  
  oii_all$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  oii_all$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_inter_outros <- rbind(ind_inter_outros, oii_all)
  
  # Partos
  parto <- data_all %>% select(1,6) %>% slice(18:19) %>% rename(Partos = `...1`, Quantidade = `...6`) %>% 
    mutate(Partos = substring(Partos, regexpr("N", Partos)))
  
  parto$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  parto$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_parto <- rbind(ind_parto, parto)
  
  # Nascimentos
  nascimento <- data_all %>% select(9,17) %>% slice(18:19) %>% rename(Nascimentos = `...9`, Quantidade = `...17`) %>% 
    mutate(Nascimentos = substring(Nascimentos, regexpr("N", Nascimentos)))
  
  nascimento$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  nascimento$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  ind_nascimento <- rbind(ind_nascimento, nascimento)
  
}

# Fazendo ajustes nos dados
ind_inter_outros <- ind_inter_outros %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

ind_parto <- ind_parto %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

ind_nascimento <- ind_nascimento %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)
