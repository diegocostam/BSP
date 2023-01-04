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


# Procedimentos cirurgícos ------------------------------------------------

proc_cirurg <- data.frame()
anestesia <- data.frame()

# Criando um laço para compilar os Anexos mensais em um único projeto
for (i in 1:length(data_names)) {
  # cirúrgias
  data_all <- read_excel(data_names[i], sheet = 7, skip = 4)
  
  cir <- data_all %>% select(c(1,5:8)) %>% slice(1:15) %>% rename(`Clínicas cirúrgicas` = `...1`, `Procedimentos cirúrgicos ambulatoriais` = `...8`)
  
  cir$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  cir$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  # Anestesias
  anes <- data_all %>% select(1,10:13) %>% slice(1:15) %>% rename(`Clínicas cirúrgicas` = `...1`)
  
  anes$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  anes$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  
  proc_cirurg <- rbind(proc_cirurg, cir)
  anestesia <- rbind(anestesia, anes)
  
}

# Fazendo ajustes nos dados
proc_cirurg <- proc_cirurg %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

anestesia <- anestesia %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(Mes)

