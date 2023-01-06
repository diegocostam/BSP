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

nosologia <- data.frame()

add_grupo_nosologia <- function(vetor){
  for (i in 1:length(vetor)) {
    if(is.na(vetor[i])) {
      vetor[i] <- vetor[i-1]
    }
  }
  return(vetor)
}

# Criando um laço para compilar os Anexos mensais em um único projeto
for (i in 1:length(data_names)) {
  sheet9_1 <- read_excel(data_names[i], sheet = 9, range = "A4:G45", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  sheet9_2 <- read_excel(data_names[i], sheet = 9, range = "J4:P38", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  sheet10_1 <- read_excel(data_names[i], sheet = 10, range = "A4:G40", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  sheet10_2 <- read_excel(data_names[i], sheet = 10, range = "J4:P39", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  sheet11_1 <- read_excel(data_names[i], sheet = 11, range = "A4:G39", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  sheet11_2 <- read_excel(data_names[i], sheet = 11, range = "J4:P42", col_names = FALSE) %>% 
    select(!c(4, 6)) %>% rename(`GRUPOS DE DOENÇAS` = `...1`, CID = `...2`, AMBULATORIAL = `...3`, `EMERGÊNCIA` = `...5`, `INTERNAÇÃO` = `...7`) %>% filter(!is.na(CID))
  
  data <- bind_rows(sheet9_1, sheet9_2, sheet10_1, sheet10_2, sheet11_1, sheet11_2) %>% mutate(`GRUPOS DE DOENÇAS` = add_grupo_nosologia(`GRUPOS DE DOENÇAS`))
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  nosologia <- rbind(nosologia, data)
  
}

# Fazendo ajustes nos dados
nosologia <- nosologia %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)
