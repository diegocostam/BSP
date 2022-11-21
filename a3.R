# Pacotes necessários

library(dplyr) # Manipulação de dados
library(magrittr) # Utilização do pipe (%>%)
library(readxl) # Leitura de arquivos do Excel
library(tidyr) # Pivotagem dos dados
library(janitor) # Padronização dos nomes das variáveis
library(stringr)

# Entrando com os Anexos, manipulando e juntado os dados.

# Nomes dos arquivos Anexo
data_names <- list.files(pattern = "Anexo A")


# Assistência Ambulatorial ------------------------------------------------


## Atendimento ambulatorial ------------------------------------------------

# Criando um laço para compilar os Anexos mensais em um único projeto
aten_amb <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 3, range = "A3:I62")
  
  data <- data %>% pivot_longer(cols = MA:TOTAL, names_to = "Clientela", values_to = "Atendimentos")
  
  data$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  aten_amb <- rbind(aten_amb, data)
  
}

# Fazendo ajustes nos dados
aten_amb <- aten_amb %>% rename(Clinica = `...1`) %>% 
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>% 
  arrange(Mes)

## Atendimento ambulatorial por clientela ---------------------------------------------------------------

# Compilando os dados da clientela e criando a variável TOTAL
clientela_amb <- aten_amb %>% select(Clientela, Atendimentos, Mes) %>% group_by(Clientela, Mes) %>% summarise(TOTAL=sum(Atendimentos, na.rm = TRUE))
# Pivotando os dados
clientela_amb <- clientela_amb %>% pivot_wider(names_from = Mes, values_from = TOTAL)


# Indices Ambulatoriais -----------------------------------------------

## Número de pareceres ambulatoriais -----------------------------------------------

# Criando um laço para compilar os Anexos mensais em um único projeto
par_amb <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 3, range = "J3:J59")
  data2 <- read_excel(data_names[i], sheet = 3, range = "A3:A59")
  data3 <- cbind(data2, data)
  
  data3$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data3$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  par_amb <- rbind(par_amb, data3)
  
}

# Fazendo ajustes nos dados
# par_amb <- par_amb %>% arrange(Mes) %>% group_by(Mes) %>%
#   mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
#                                       "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>% 
#   clean_names() %>% 
#   summarise(TOTAL=sum(no_pareceres_ambulatoriais, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = mes, values_from = TOTAL) %>%
#   mutate(TOTAL = select(., JAN:DEZ) %>% apply(1, sum, na.rm=TRUE))

par_amb <- par_amb %>%
  mutate(Mes = factor(Mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  rename(clinica = `...1`) %>% 
  clean_names() %>% arrange(mes) %>% 
  pivot_wider(names_from = mes, values_from = no_pareceres_ambulatoriais) %>% 
  mutate(TOTAL = select(.,JAN:DEZ) %>% apply(1, sum, na.rm=TRUE))


## Prazos de marcação de consultas -----------------------------------------

# Criando um laço para compilar os Anexos mensais em um único projeto
pmc <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 3, range = "K3:K59")
  data2 <- read_excel(data_names[i], sheet = 3, range = "A3:A59")
  data3 <- cbind(data, data2)
  
  data3$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data3$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  pmc <- rbind(pmc, data3)
  
}

# Fazendo ajustes nos dados
pmc <- pmc %>% rename(clinicas = `...1`) %>% 
  clean_names() %>%
  mutate(mes = factor(mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(mes) %>% 
  pivot_wider(names_from = mes, values_from = prazo_marcacao_consulta)



## Absenteísmo -------------------------------------------------------------

# Criando um laço para compilar os Anexos mensais em um único projeto
abs <- data.frame()

for (i in 1:length(data_names)) {
  data <- read_excel(data_names[i], sheet = 3, range = "L3:M59")
  data2 <- read_excel(data_names[i], sheet = 3, range = "A3:A59")
  data3 <- cbind(data, data2)
  
  data3$Mes <- substr(sub(".*V - ", "", data_names[i]), 1 , 3)
  data3$Ano <- substr(sub(".*V - ", "", data_names[i]), 5 , 8)
  
  abs <- rbind(abs, data3)
  
}

# Criando uma função para arredondar .5

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

# Fazendo ajustes nos dados
abs <- abs %>% rename(clinicas = `...1`) %>% 
  clean_names() %>%
  mutate(mes = factor(mes, levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                                      "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"))) %>%
  arrange(mes) %>%
  mutate(abs = paste(round2(faltas_a_consultas/consultas_agendadas*100, digits = 0), "%", sep = "") %>% 
           str_replace_all("NA%", "NA")) %>%
  select(!c(faltas_a_consultas, consultas_agendadas)) %>% 
  pivot_wider(names_from = mes, values_from = abs)
