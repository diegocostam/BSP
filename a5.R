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


# Clientela da internação -------------------------------------------------

int_clientela <- data.frame()

# Criando um laço para compilar os Anexos mensais em um único projeto
for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 5, range = "A4:I47")
  
  data <- data %>% pivot_longer(cols = MA:TOTAL, names_to = "Clientela", values_to = "Internacao")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  int_clientela <- rbind(int_clientela, data)
  
}

# Fazendo ajustes nos dados
int_clientela <- int_clientela %>% rename(Clinica = `...1`) %>% 
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

# Tipos de Internação



# Indicadores Hospitalares -----------------------------------------------

ind_inter <- data.frame()

for (i in 1:length(data_names)) {
  
  data <- read_excel(data_names[i], sheet = 5, range = "J47:Q47", col_names = FALSE)
  data2 <- read_excel(data_names[i], sheet = 5, range = "J4:Q4")

  colnames(data) <- colnames(data2)
  
  data <- data %>% pivot_longer(cols = 1:8, names_to = "Indicadores", values_to = "Valores")

  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)

  ind_inter <- rbind(ind_inter, data)

}

# Fazendo ajustes nos dados
ind_inter <- ind_inter %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

