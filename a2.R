# Pacotes necessários

library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(readxl) # Leitura de arquivos do Excel

# Entrando com os Anexos, manipulando e juntado os dados.

data_names <- list.files(pattern = "Anexo A")

# Leitos hospitalares de internação -------------------------------------------

infra_inter <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 2, range = "A3:C8")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  infra_inter <- rbind(infra_inter, data)
  
}

infra_inter <- infra_inter %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)


# Salas/Consultórios/Leitos sem internação --------------------------------


infra_salas <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 2, range = "A10:C23")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  infra_salas <- rbind(infra_salas, data)
  
}

infra_salas <- infra_salas %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)


# Leitos por hospital dia ------------------------------------------------

infra_hospdia <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 2, range = "A25:C28")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  infra_hospdia <- rbind(infra_hospdia, data)
  
}

infra_hospdia <- infra_hospdia %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)
