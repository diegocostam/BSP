# Pacotes necessários

library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(readxl) # Leitura de arquivos do Excel
library(stringr) # Manipilar strings

# Entrando com os Anexos, manipulando e juntado os dados.

data_names <- list.files(pattern = "Anexo A")

# Função técnica ----------------------------------------------------------

data_rh_tec <- data.frame()


for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 1, range = "A12:I30")
  
  data <- data %>% pivot_longer(cols = `CSM / CAM(QT) / CAP / CFN`:OUTROS, names_to = "Funcao_Tecnica", values_to = "Quantitativo")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  data_rh_tec <- rbind(data_rh_tec, data)
  
}

data_rh_tec <- data_rh_tec %>% rename(profissao = `...1`) %>% 
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)


# Observação do número de biólogos

obs <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 1, range = "A33", col_names = FALSE)
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  obs <- rbind(obs, data)
  
}

obs <- obs %>% rename(Obs = `...1`) %>% 
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

# Função administrativa ---------------------------------------------------


data_rh_adm <- data.frame()


for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 1, range = "A12:A30")
  data2 <- read_excel(data_names[i], sheet = 1, range = "K12:O30")
  data3 <- cbind(data, data2)
  data3 <- data3 %>% pivot_longer(cols = `CSM / CAM(QT) / CAP / CFN`:OUTROS, names_to = "Funcao_Administrativa", values_to = "Quantitativo")
  
  data3$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data3$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  data_rh_adm <- rbind(data_rh_adm, data3)
  
}

data_rh_adm <- data_rh_adm %>% rename(profissao = `...1`) %>% 
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)